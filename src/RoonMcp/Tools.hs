-- |
-- Module      : RoonMcp.Tools
-- Description : The tools of the server, each over a connection of its own
-- License     : BSD-3-Clause
--
-- Every call opens a connection to the Roon Server, registers with the
-- stored token, does its work and closes the connection: the server keeps
-- no state. Connections are taken one at a time ('connectionLock'). Browsing sessions outlive connections on the Roon side, which
-- is what lets a search and the queueing of its candidates be two calls.
-- Images are the exception: they come over HTTP, without a connection.
module RoonMcp.Tools
  ( Context (..)
  , extension
  , tools
  , instructions
  , describeError
  ) where

import Data.Aeson (FromJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.List (sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (MVar, withMVar)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Data.Text.Encoding (encodeUtf8)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (stderr)
import Effectful.FileSystem.IO.ByteString (hPutStrLn)
import Effectful.MCP.Tool
import Roon.Browse
  ( Candidate (..)
  , Item (..)
  , SearchOptions (..)
  , defaultSearchOptions
  , performTrackAction
  , searchTracksConcurrently
  , trackActionTitle
  )
import Roon.Connection (runRoonWithin)
import Roon.Effect.Moo (Moo)
import Roon.Effect.TokenStore (TokenStore, runTokenStoreFile)
import Roon.Effect.WebSocket (ServerAddress (..), WebSocket, runWebSocket, traceFrames)
import Roon.Error (RoonError (..))
import Roon.Image (Image (..), fetchImage, fitWithin)
import Roon.Registry (ExtensionInfo (..))
import Roon.Transport
  ( Control (..)
  , DisplayLines (..)
  , NowPlaying (..)
  , Output (..)
  , PlaybackState (..)
  , QueueEvent (..)
  , QueueItem (..)
  , Zone (..)
  , ZoneId (..)
  , control
  , getZones
  , subscribeQueue
  )
import RoonMcp.Plan
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream

-- | What every call needs.
data Context = Context
  { address :: ServerAddress
  , tokenFile :: FilePath
  , registrationTimeout :: Int
  -- ^ Microseconds to wait for the registration; the extension must have
  -- been enabled beforehand.
  , traceLimit :: Maybe Int
  -- ^ When set, every frame is described on stderr, its body cut to this
  -- many characters.
  , connectionLock :: MVar ()
  -- ^ Taken for each connection. Roon keeps one connection per extension:
  -- when a second one registers, it sends an empty frame to the first,
  -- which then fails. Calls served at the same time wait for each other.
  }

-- | How the server introduces itself to Roon.
extension :: ExtensionInfo
extension =
  ExtensionInfo
    { extensionId = "io.github.guillaumedevolpiano.roon-mcp"
    , displayName = "roon-mcp"
    , displayVersion = "0.1.0.0"
    , publisher = "roon-mcp"
    , email = "nobody@example.invalid"
    , website = Just "https://github.com/GuillaumedeVolpiano/roon_mcp"
    , requiredServices = ["com.roonlabs.transport:2", "com.roonlabs.browse:1"]
    , optionalServices = []
    , providedServices = []
    }

instructions :: Text
instructions =
  "Plays music on the Roon system of this machine. To play tracks: call list_zones to pick a zone; \
  \call search_tracks with one query per track, giving the title and the artist; choose among the \
  \candidates by their title and artists (search results give no album); then call queue_tracks \
  \with the chosen candidate ids, in order, and a mode. Candidate ids come from search_tracks: use \
  \them soon after the search. Zones, queue items and candidates may carry an image_key: get_image \
  \returns that image, such as an album cover."

type Effects es = (IOE :> es, Concurrent :> es, FileSystem :> es)

-- | Run Roon work over a connection of its own.
withConnection
  :: Effects es
  => Context
  -> Eff (Moo : WebSocket : TokenStore : Error RoonError : es) a
  -> Eff es (Either RoonError a)
withConnection context work =
  withMVar context.connectionLock . const . runErrorNoCallStack . runTokenStoreFile context.tokenFile $
    runWebSocket context.address . trace $
      runRoonWithin (Just context.registrationTimeout) extension (const work)
  where
    trace = maybe id (\limit -> traceFrames limit (hPutStrLn stderr . encodeUtf8)) context.traceLimit

-- | Run Roon work and turn a failure into a result the model can read.
roon :: Effects es => Context -> (a -> ToolResult) -> Eff (Moo : WebSocket : TokenStore : Error RoonError : es) a -> Eff es ToolResult
roon context toResult work = either (errorResult . describeError context) toResult <$> withConnection context work

describeError :: Context -> RoonError -> Text
describeError context = \case
  TimedOut "com.roonlabs.registry:1/register" ->
    "The extension \"roon-mcp\" is not enabled in Roon. Ask the user to run `roon-mcp register` in a terminal \
    \and to enable \"roon-mcp\" in Roon, under Settings, Extensions; then call again."
  ConnectFailed cause ->
    "Cannot reach the Roon Server at " <> Text.pack context.address.host <> ":" <> Text.pack (show context.address.port) <> ": " <> cause
  ConnectionLost -> "The connection to the Roon Server was lost; call again."
  TimedOut request -> "The Roon Server did not answer " <> request <> " in time."
  BrowseFailed message -> "Roon refused: " <> message
  BrowseItemMissing what -> "Roon did not offer " <> what <> "."
  ImageFailed reason -> "Roon gave no image: " <> reason
  UnexpectedReply request status -> "Roon answered " <> request <> " with " <> status <> "."
  other -> Text.pack (show other)

tools :: Effects es => Context -> [Tool es]
tools context =
  [ (tool "list_zones" "List the zones of Roon: where music plays, what plays there, and what it allows." noArguments (\_ -> listZones context))
      {annotations = readOnly}
  , (tool "get_queue" "List the next items of the queue of a zone." queueSchema (withArguments (getQueue context)))
      {annotations = readOnly}
  , ( tool
        "search_tracks"
        "Search the Roon library for tracks, one query per track, all at once. Each query gives its candidates \
        \(id, title, artists), best first, or an error. Put the title and the artist in the query."
        searchSchema
        (withArguments (searchTracks context))
    )
      {annotations = readOnly}
  , ( tool
        "queue_tracks"
        "Put tracks found by search_tracks in the queue of a zone, in the order given. Modes: \"queue\" adds them \
        \at the end of the queue; \"add_next\" right after the current track; \"play_now\" replaces the queue with \
        \these tracks and plays the first at once. Prefer add_next or queue unless the user wants the queue \
        \replaced. Reports each track; a failure does not stop the others."
        queueTracksSchema
        (withArguments (queueTracks context))
    )
      -- Destructive: the play_now mode replaces the queue.
      {annotations = noAnnotations {readOnlyHint = Just False, destructiveHint = Just True, idempotentHint = Just False, openWorldHint = Just False}}
  , ( tool
        "control"
        "Operate the playback of a zone: play, pause, playpause, stop, next or previous."
        controlSchema
        (withArguments (controlZone context))
    )
      {annotations = noAnnotations {readOnlyHint = Just False, destructiveHint = Just False, openWorldHint = Just False}}
  , ( tool
        "get_image"
        "Fetch an image of Roon, such as the cover of what plays, from an image_key given by list_zones, \
        \get_queue or search_tracks. The image fits in a square of the size asked, in pixels."
        imageSchema
        (withArguments (getImage context))
    )
      {annotations = readOnly}
  ]
  where
    readOnly = noAnnotations {readOnlyHint = Just True, openWorldHint = Just False}

-- list_zones

listZones :: Effects es => Context -> Eff es ToolResult
listZones context = roon context (structuredResult . object . pure . ("zones" .=) . map zoneSummary) getZones

zoneSummary :: Zone -> Value
zoneSummary zone =
  object $
    [ "zone_id" .= unZoneId zone.zoneId
    , "name" .= zone.displayName
    , "state" .= stateName zone.state
    , "outputs" .= map (\output -> output.displayName) zone.outputs
    , "allowed" .= [name | (name, True) <- [("play", zone.isPlayAllowed), ("pause", zone.isPauseAllowed), ("previous", zone.isPreviousAllowed), ("next", zone.isNextAllowed), ("seek", zone.isSeekAllowed)] :: [(Text, Bool)]]
    ]
      <> catMaybes
        [ ("now_playing" .=) . nowPlaying <$> zone.nowPlaying
        , ("queue_items_remaining" .=) <$> zone.queueItemsRemaining
        , ("queue_seconds_remaining" .=) <$> zone.queueTimeRemaining
        ]
  where
    nowPlaying playing =
      object $
        ["title" .= playing.threeLine.line1]
          <> catMaybes
            [ ("artists" .=) <$> playing.threeLine.line2
            , ("album" .=) <$> playing.threeLine.line3
            , ("seconds" .=) <$> playing.length
            , ("position" .=) <$> playing.seekPosition
            , ("image_key" .=) <$> playing.imageKey
            ]

stateName :: PlaybackState -> Text
stateName = \case
  Playing -> "playing"
  Paused -> "paused"
  Loading -> "loading"
  Stopped -> "stopped"
  OtherState name -> name

unZoneId :: ZoneId -> Text
unZoneId (ZoneId zoneId) = zoneId

-- get_queue

data QueueArguments = QueueArguments Text Int

instance FromJSON QueueArguments where
  parseJSON = withObject "arguments" $ \fields ->
    QueueArguments <$> fields .: "zone" <*> fields .:? "max_items" .!= 20

getQueue :: Effects es => Context -> QueueArguments -> Eff es ToolResult
getQueue context (QueueArguments wanted maxItems) =
  withZone context wanted $ \zone ->
    subscribeQueue zone.zoneId (clamp 1 100 maxItems) (Stream.fold Fold.one) >>= \case
      Just (QueueSubscribed items) -> pure (structuredResult (object ["items" .= map queueEntry items]))
      _ -> pure (errorResult "Roon gave no queue.")
  where
    queueEntry item =
      object $
        ["queue_item_id" .= item.queueItemId, "title" .= item.threeLine.line1]
          <> catMaybes [("artists" .=) <$> item.threeLine.line2, ("album" .=) <$> item.threeLine.line3, ("seconds" .=) <$> item.length, ("image_key" .=) <$> item.imageKey]

-- | Find the zone, then do the work; a zone that is not there gives an
-- error result that lists the zones.
withZone
  :: Effects es
  => Context
  -> Text
  -> (Zone -> Eff (Moo : WebSocket : TokenStore : Error RoonError : es) ToolResult)
  -> Eff es ToolResult
withZone context wanted work =
  roon context id $
    getZones >>= \zones -> either (pure . errorResult) work (findZone wanted zones)

-- search_tracks

data SearchArguments = SearchArguments [Text] Int

instance FromJSON SearchArguments where
  parseJSON = withObject "arguments" $ \fields ->
    SearchArguments <$> fields .: "queries" <*> fields .:? "max_candidates" .!= 5

searchTracks :: Effects es => Context -> SearchArguments -> Eff es ToolResult
searchTracks context (SearchArguments queries maxCandidates)
  | null queries = pure (errorResult "Give at least one query.")
  | length queries > 20 = pure (errorResult "Give at most 20 queries at once.")
  | otherwise = do
      -- Sessions of their own, so that this search does not move those of
      -- earlier ones, whose candidates may still be queued.
      stamp <- liftIO getPOSIXTime
      let prefix = "roon-mcp-" <> Text.pack (show (floor (stamp * 1_000_000) :: Integer)) <> "-"
          options = defaultSearchOptions {maxCandidates = clamp 1 20 maxCandidates}
      roon context (structuredResult . object . pure . ("results" .=) . map result) $
        searchTracksConcurrently options prefix queries
  where
    result (query, outcome) = case outcome of
      Left err -> object ["query" .= query, "error" .= describeError context err]
      Right candidates -> object ["query" .= query, "candidates" .= mapMaybe candidate candidates]
    candidate found =
      (\identifier -> object (["candidate_id" .= identifier, "title" .= found.item.title] <> catMaybes [("artists" .=) <$> found.item.subtitle, ("image_key" .=) <$> found.item.imageKey]))
        <$> candidateId found

-- queue_tracks

data QueueTracksArguments = QueueTracksArguments Text [Text] Text

instance FromJSON QueueTracksArguments where
  parseJSON = withObject "arguments" $ \fields ->
    QueueTracksArguments <$> fields .: "zone" <*> fields .: "candidate_ids" <*> fields .:? "mode" .!= "queue"

queueTracks :: Effects es => Context -> QueueTracksArguments -> Eff es ToolResult
queueTracks context (QueueTracksArguments wanted identifiers modeText) = case parseMode modeText of
  Nothing -> pure (errorResult ("Unknown mode \"" <> modeText <> "\": use queue, add_next or play_now."))
  Just mode
    | null identifiers -> pure (errorResult "Give at least one candidate id.")
    | otherwise -> withZone context wanted $ \zone -> report mode <$> traverse (perform zone) (plan mode identifiers)
  where
    perform zone (index, action, identifier) = case decodeCandidateId identifier of
      Nothing -> pure (index, identifier, action, Left "Not a candidate id from search_tracks.")
      Just candidate ->
        (index,identifier,action,) . either (Left . describeError context) Right
          <$> runErrorNoCallStack (performTrackAction zone.zoneId (trackActionTitle action) candidate)
    report mode outcomes =
      let entries = sortOn (\(index, _, _, _) -> index) outcomes
          failures = length [() | (_, _, _, Left _) <- entries]
          entry (_, identifier, action, outcome) =
            object $
              ["candidate_id" .= identifier, "action" .= trackActionTitle action, "ok" .= either (const False) (const True) outcome]
                <> either (\err -> ["error" .= err]) (const []) outcome
          value = object ["mode" .= modeName mode, "tracks" .= map entry entries, "failures" .= failures]
       in (structuredResult value) {isError = failures == length entries}

-- control

data ControlArguments = ControlArguments Text Text

instance FromJSON ControlArguments where
  parseJSON = withObject "arguments" $ \fields -> ControlArguments <$> fields .: "zone" <*> fields .: "action"

controlZone :: Effects es => Context -> ControlArguments -> Eff es ToolResult
controlZone context (ControlArguments wanted actionName) = case lookup actionName controls of
  Nothing -> pure (errorResult ("Unknown action \"" <> actionName <> "\": use " <> Text.intercalate ", " (map fst controls) <> "."))
  Just which -> withZone context wanted $ \zone -> textResult ("Done: " <> actionName <> " on " <> zone.displayName <> ".") <$ control zone.zoneId which
  where
    controls = [("play", Play), ("pause", Pause), ("playpause", PlayPause), ("stop", Stop), ("next", Next), ("previous", Previous)]

-- get_image

data ImageArguments = ImageArguments Text Int

instance FromJSON ImageArguments where
  parseJSON = withObject "arguments" $ \fields ->
    ImageArguments <$> fields .: "image_key" <*> fields .:? "size" .!= 300

-- | Images come over HTTP, without a connection of the extension: no
-- registration, and no need to wait for the calls that hold one.
getImage :: Effects es => Context -> ImageArguments -> Eff es ToolResult
getImage context (ImageArguments key size) =
  runErrorNoCallStack (fetchImage context.address key (fitWithin (clamp 32 1024 size))) >>= \case
    Left err -> pure (errorResult (describeError context err))
    Right image -> pure $ case imageMediaType image.contentType of
      Just mediaType -> imageResult mediaType image.bytes
      Nothing -> errorResult ("Roon answered with \"" <> image.contentType <> "\", which is not an image.")

-- Schemas

noArguments :: Value
noArguments = object ["type" .= ("object" :: Text), "additionalProperties" .= False]

zoneProperty :: Value
zoneProperty = object ["type" .= ("string" :: Text), "description" .= ("A zone id or name, from list_zones." :: Text)]

imageSchema :: Value
imageSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "image_key" .= object ["type" .= ("string" :: Text), "description" .= ("An image_key from list_zones, get_queue or search_tracks." :: Text)]
          , "size" .= object ["type" .= ("integer" :: Text), "minimum" .= (32 :: Int), "maximum" .= (1024 :: Int), "default" .= (300 :: Int)]
          ]
    , "required" .= ["image_key" :: Text]
    , "additionalProperties" .= False
    ]

queueSchema :: Value
queueSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "zone" .= zoneProperty
          , "max_items" .= object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "maximum" .= (100 :: Int), "default" .= (20 :: Int)]
          ]
    , "required" .= ["zone" :: Text]
    , "additionalProperties" .= False
    ]

searchSchema :: Value
searchSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "queries"
              .= object
                [ "type" .= ("array" :: Text)
                , "items" .= object ["type" .= ("string" :: Text)]
                , "minItems" .= (1 :: Int)
                , "maxItems" .= (20 :: Int)
                , "description" .= ("One query per track: its title and its artist." :: Text)
                ]
          , "max_candidates" .= object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "maximum" .= (20 :: Int), "default" .= (5 :: Int)]
          ]
    , "required" .= ["queries" :: Text]
    , "additionalProperties" .= False
    ]

queueTracksSchema :: Value
queueTracksSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "zone" .= zoneProperty
          , "candidate_ids"
              .= object
                [ "type" .= ("array" :: Text)
                , "items" .= object ["type" .= ("string" :: Text)]
                , "minItems" .= (1 :: Int)
                , "description" .= ("Candidate ids from search_tracks, in the order to play them." :: Text)
                ]
          , "mode" .= object ["type" .= ("string" :: Text), "enum" .= map modeName [minBound .. maxBound], "default" .= ("queue" :: Text)]
          ]
    , "required" .= ["zone" :: Text, "candidate_ids"]
    , "additionalProperties" .= False
    ]

controlSchema :: Value
controlSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "zone" .= zoneProperty
          , "action" .= object ["type" .= ("string" :: Text), "enum" .= ["play", "pause", "playpause", "stop", "next", "previous" :: Text]]
          ]
    , "required" .= ["zone" :: Text, "action"]
    , "additionalProperties" .= False
    ]

clamp :: Int -> Int -> Int -> Int
clamp low high = max low . min high
