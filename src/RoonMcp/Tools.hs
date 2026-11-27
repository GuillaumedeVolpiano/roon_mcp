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
  , serverVersion
  , Effects
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
import Data.Version (showVersion)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (MVar, withMVar)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Data.Text.Encoding (encodeUtf8)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (stderr)
import Effectful.FileSystem.IO.ByteString (hPutStrLn)
import Effectful.MCP.Tool
import Roon.Browse
  ( BrowseResult (..)
  , Candidate (..)
  , Hierarchy (..)
  , Item (..)
  , List (..)
  , ListHint (..)
  , LoadResult (..)
  , Navigation (..)
  , SearchOptions (..)
  , Session (..)
  , browse
  , defaultSearchOptions
  , load
  , performAction
  , performTrackAction
  , searchTracksConcurrently
  , trackActionTitle
  )
import Roon.Connection (runRoonWithin)
import Roon.Effect.Moo (Moo)
import Roon.Effect.TokenStore (TokenStore, runTokenStoreFile)
import Roon.Effect.WebSocket (ServerAddress (..), WebSocket, runWebSocket, traceFrames)
import Roon.Error (RoonError (..))
import Roon.Image (Image (..), defaultImageLimits, fetchImage, fitWithin, runImageHttp)
import Roon.Registry (ExtensionInfo (..))
import Roon.Transport
  ( Control (..)
  , DisplayLines (..)
  , Loop (..)
  , LoopChange (..)
  , NowPlaying (..)
  , Output (..)
  , OutputId (..)
  , PlaybackState (..)
  , QueueEvent (..)
  , QueueItem (..)
  , Seek (..)
  , SettingsChange (..)
  , Volume (..)
  , VolumeChange (..)
  , VolumeType (..)
  , Zone (..)
  , ZoneId (..)
  , ZoneSettings (..)
  , changeSettings
  , changeVolume
  , control
  , getZones
  , mute
  , seek
  , subscribeQueue
  , transferZone
  )
import Paths_roon_mcp (version)
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

-- | The version of this package, as the server gives it to Roon and to
-- MCP clients.
serverVersion :: Text
serverVersion = Text.pack (showVersion version)

-- | How the server introduces itself to Roon.
extension :: ExtensionInfo
extension =
  ExtensionInfo
    { extensionId = "io.github.guillaumedevolpiano.roon-mcp"
    , displayName = "roon-mcp"
    , displayVersion = serverVersion
    , publisher = "roon-mcp"
    , email = "nobody@example.invalid"
    , website = Just "https://github.com/GuillaumedeVolpiano/roon_mcp"
    , requiredServices = ["com.roonlabs.transport:2", "com.roonlabs.browse:1"]
    , optionalServices = []
    , providedServices = []
    }

-- | What the server tells the model about how to use its tools.
instructions :: Text
instructions =
  "Plays music on the Roon system of this machine. To play tracks: call list_zones to pick a zone; \
  \call search_tracks with one query per track, giving the title and the artist; choose among the \
  \candidates by their title and artists (search results give no album); then call queue_tracks \
  \with the chosen candidate ids, in order, and a mode. Candidate ids come from search_tracks: use \
  \them soon after the search. Zones, queue items and candidates may carry an image_key: get_image \
  \returns that image, such as an album cover. For albums, artists, playlists and radios, \
  \browse_library lists or searches the library and play_item plays what it found. set_volume, seek, \
  \set_playback_options and transfer_zone act on a zone."

-- | What the tools need: input and output, threads, and the files where
-- the token is kept.
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

-- | An error of Roon as a text the model can act on.
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
  UnexpectedReply _ "InvalidItemKey" -> "This id is no longer valid: call browse_library or search_tracks again for fresh ids."
  UnexpectedReply request status -> "Roon answered " <> request <> " with " <> status <> "."
  other -> Text.pack (show other)

-- | The tools of the server, in the order they are listed.
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
        "browse_library"
        "Browse the Roon library: albums, artists, playlists, genres, composers and internet radios, or search \
        \it for anything. Give one of: a hierarchy to list it from its top, a search text, or the item_id of an \
        \item of kind \"list\" from an earlier call to open it. Items of kind \"playable\" (a track, or \
        \\"Play Album\", \"Play Playlist\") go to play_item, and so does a whole album or playlist by its own \
        \item_id. Long listings come in pages: offset and max_items."
        browseSchema
        (withArguments (browseLibrary context))
    )
      {annotations = readOnly}
  , ( tool
        "play_item"
        "Play something found by browse_library in a zone: a track, an album, a playlist, an artist or a radio, \
        \by its item_id. Actions: \"queue\" adds it at the end of the queue; \"add_next\" right after the \
        \current track; \"play_now\" replaces the queue and plays it at once; \"start_radio\" plays similar \
        \music; \"shuffle\" (playlists) plays it in random order, replacing the queue. Prefer queue or add_next \
        \unless the user wants the queue replaced."
        playItemSchema
        (withArguments (playItem context))
    )
      -- Destructive: play_now, start_radio and shuffle replace the queue.
      {annotations = noAnnotations {readOnlyHint = Just False, destructiveHint = Just True, idempotentHint = Just False, openWorldHint = Just False}}
  , ( tool
        "set_volume"
        "Set or move the volume of a zone, or mute it. The volume is in the units of each output, between the \
        \minimum and the maximum that list_zones gives (some are in decibels, hence negative). Give \"level\" \
        \for a volume, or \"change\" to move it by that much, and \"mute\" to mute or unmute. All the outputs \
        \of the zone are changed unless \"output\" names one. Raise the volume in small moves."
        volumeSchema
        (withArguments (setVolume context))
    )
      {annotations = changes}
  , ( tool
        "seek"
        "Move inside the track that plays in a zone: to a position in seconds from its start, or by a number of \
        \seconds (negative to go back) when \"relative\" is true."
        seekSchema
        (withArguments (seekZone context))
    )
      {annotations = changes}
  , ( tool
        "set_playback_options"
        "Change how a zone plays: shuffle, the Roon Radio that follows the queue (auto_radio), and the loop \
        \(\"disabled\", \"loop\" for the whole queue, \"loop_one\" for the current track). Options left out \
        \stay as they are."
        optionsSchema
        (withArguments (setOptions context))
    )
      {annotations = changes {idempotentHint = Just True}}
  , ( tool
        "transfer_zone"
        "Move what plays in a zone, with its queue, to another zone."
        transferSchema
        (withArguments (transfer context))
    )
      {annotations = changes}
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
    changes = noAnnotations {readOnlyHint = Just False, destructiveHint = Just False, openWorldHint = Just False}

-- list_zones

listZones :: Effects es => Context -> Eff es ToolResult
listZones context = roon context (structuredResult . object . pure . ("zones" .=) . map zoneSummary) getZones

zoneSummary :: Zone -> Value
zoneSummary zone =
  object $
    [ "zone_id" .= unZoneId zone.zoneId
    , "name" .= zone.displayName
    , "state" .= stateName zone.state
    , "outputs" .= map outputSummary zone.outputs
    , "allowed" .= [name | (name, True) <- [("play", zone.isPlayAllowed), ("pause", zone.isPauseAllowed), ("previous", zone.isPreviousAllowed), ("next", zone.isNextAllowed), ("seek", zone.isSeekAllowed)] :: [(Text, Bool)]]
    ]
      <> catMaybes
        [ ("options" .=) . options <$> zone.settings
        , ("now_playing" .=) . nowPlaying <$> zone.nowPlaying
        , ("queue_items_remaining" .=) <$> zone.queueItemsRemaining
        , ("queue_seconds_remaining" .=) <$> zone.queueTimeRemaining
        ]
  where
    options settings = object ["shuffle" .= settings.shuffle, "auto_radio" .= settings.autoRadio, "loop" .= loopName settings.loop]
    outputSummary output =
      object $
        ["output_id" .= unOutputId output.outputId, "name" .= output.displayName]
          <> catMaybes [("volume" .=) . volumeSummary <$> output.volume]
    volumeSummary volume =
      object $
        ["unit" .= volumeUnit volume.volumeType]
          <> catMaybes
            [ ("level" .=) <$> volume.value
            , ("minimum" .=) <$> volume.minimum
            , ("maximum" .=) <$> volume.maximum
            , ("step" .=) <$> volume.step
            , ("muted" .=) <$> volume.isMuted
            ]
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

unOutputId :: OutputId -> Text
unOutputId (OutputId outputId) = outputId

loopName :: Loop -> Text
loopName = \case
  LoopDisabled -> "disabled"
  LoopQueue -> "loop"
  LoopOne -> "loop_one"
  OtherLoop name -> name

volumeUnit :: VolumeType -> Text
volumeUnit = \case
  VolumeDecibel -> "db"
  VolumeNumber -> "number"
  VolumeIncremental -> "incremental"
  OtherVolumeType name -> name

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

-- browse_library

data BrowseArguments = BrowseArguments (Maybe Text) (Maybe Text) (Maybe Text) Int Int

instance FromJSON BrowseArguments where
  parseJSON = withObject "arguments" $ \fields ->
    BrowseArguments
      <$> fields .:? "hierarchy"
      <*> fields .:? "search"
      <*> fields .:? "item_id"
      <*> fields .:? "offset" .!= 0
      <*> fields .:? "max_items" .!= 30

browseLibrary :: Effects es => Context -> BrowseArguments -> Eff es ToolResult
browseLibrary context (BrowseArguments hierarchyName searchText itemText offset maxItems) = do
  stamp <- liftIO getPOSIXTime
  -- A session of its own for each listing started here, so that it does
  -- not move those of earlier ones, whose ids may still be used.
  let fresh hierarchy = Session {hierarchy, multiSessionKey = Just ("roon-mcp-b-" <> Text.pack (show (floor (stamp * 1_000_000) :: Integer)))}
  case (hierarchyName, searchText, itemText) of
    (Just name, Nothing, Nothing) -> case lookup name [(found, hierarchy) | (found, hierarchy) <- libraryHierarchies, hierarchy /= Search] of
      Just hierarchy -> open (fresh hierarchy) (Top Nothing)
      Nothing -> pure (errorResult ("Unknown hierarchy \"" <> name <> "\": use " <> Text.intercalate ", " browsable <> "."))
    (Nothing, Just text, Nothing)
      | Text.null (Text.strip text) -> pure (errorResult "Give a text to search for.")
      | otherwise -> open (fresh Search) (Top (Just text))
    (Nothing, Nothing, Just identifier) -> case decodeItemId identifier of
      Nothing -> pure (errorResult "Not an item_id from browse_library.")
      Just ItemRef {kind = KindAction} -> pure (errorResult "This item is an action: it is not opened. Give play_item the item that offers it.")
      Just ref -> open ref.session (Select ref.key)
    _ -> pure (errorResult "Give exactly one of hierarchy, search and item_id.")
  where
    browsable = [name | (name, hierarchy) <- libraryHierarchies, hierarchy /= Search]
    open current navigation =
      roon context (listing current) $ do
        browse current (Nothing :: Maybe ZoneId) navigation >>= \case
          BrowseMessage message True -> throwError (BrowseFailed message)
          _ -> pure ()
        level <- load current (max 0 offset) (clamp 1 100 maxItems)
        -- Observed with Roon Server 2.75: a key selected in a session that
        -- Roon has forgotten is not refused; the top of the hierarchy
        -- comes back instead. An item opened is never the top.
        case navigation of
          Select _ | level.list.level == 0 -> throwError (UnexpectedReply "com.roonlabs.browse:1/browse" "InvalidItemKey")
          _ -> pure level
    listing current level =
      structuredResult . object $
        [ "title" .= plainText level.list.title
        , "total" .= level.list.count
        , "offset" .= level.offset
        , "items" .= map (entry current) level.items
        ]
          <> catMaybes
            [ ("subtitle" .=) . plainText <$> level.list.subtitle
            , ("image_key" .=) <$> level.list.imageKey
            , if level.list.hint == Just ActionListHint
                then Just ("note" .= ("These are the actions of the item opened: give that item to play_item with the action wanted." :: Text))
                else Nothing
            ]
    entry current item =
      object $
        ["title" .= plainText item.title, "kind" .= kindName (itemKind item)]
          <> catMaybes
            [ ("item_id" .=) <$> itemId current item
            , ("subtitle" .=) . plainText <$> item.subtitle
            , ("image_key" .=) <$> item.imageKey
            ]

-- play_item

data PlayItemArguments = PlayItemArguments Text Text Text

instance FromJSON PlayItemArguments where
  parseJSON = withObject "arguments" $ \fields ->
    PlayItemArguments <$> fields .: "zone" <*> fields .: "item_id" <*> fields .:? "action" .!= "queue"

playItem :: Effects es => Context -> PlayItemArguments -> Eff es ToolResult
playItem context (PlayItemArguments wanted identifier actionName) = case (parsePlayAction actionName, target) of
  (Nothing, _) -> pure (errorResult ("Unknown action \"" <> actionName <> "\": use " <> Text.intercalate ", " (map playActionName [minBound .. maxBound]) <> "."))
  (_, Left problem) -> pure (errorResult problem)
  (Just action, Right candidate) -> withZone context wanted $ \zone ->
    textResult ("Done: " <> playActionTitle action <> " in " <> zone.displayName <> ".")
      <$ performAction zone.zoneId (playActionTitle action) candidate
  where
    -- An item of browse_library, or a candidate of search_tracks.
    target = case (decodeItemId identifier, decodeCandidateId identifier) of
      (Just ItemRef {kind = KindAction}, _) -> Left "This item is an action. Give the item that offers it, with the action wanted."
      (Just ItemRef {kind = KindOther}, _) -> Left "This item cannot be played."
      (Just ref, _) -> Right Candidate {session = ref.session, item = Item {title = "this item", subtitle = Nothing, imageKey = Nothing, itemKey = Just ref.key, hint = Nothing, inputPrompt = Nothing}}
      (Nothing, Just candidate) -> Right candidate
      (Nothing, Nothing) -> Left "Not an item_id from browse_library nor a candidate id from search_tracks."

-- set_volume

data VolumeArguments = VolumeArguments Text (Maybe Text) (Maybe Double) (Maybe Double) (Maybe Bool)

instance FromJSON VolumeArguments where
  parseJSON = withObject "arguments" $ \fields ->
    VolumeArguments <$> fields .: "zone" <*> fields .:? "output" <*> fields .:? "level" <*> fields .:? "change" <*> fields .:? "mute"

setVolume :: Effects es => Context -> VolumeArguments -> Eff es ToolResult
setVolume context (VolumeArguments wanted outputName level change muting)
  | Just _ <- level, Just _ <- change = pure (errorResult "Give level or change, not both.")
  | Nothing <- level, Nothing <- change, Nothing <- muting = pure (errorResult "Give level, change or mute.")
  | otherwise = withZone context wanted $ \zone -> case outputsFor outputName zone of
      Left problem -> pure (errorResult problem)
      Right outputs -> report zone <$> traverse apply outputs
  where
    apply output = (output,) <$> runErrorNoCallStack (adjust output)
    adjust output = case output.volume of
      Nothing -> pure (Just ("Roon does not control the volume of this output." :: Text))
      Just volume -> do
        mapM_ (changeVolume output.outputId . VolumeTo . volumeWithin volume) level
        mapM_ (changeVolume output.outputId . VolumeBy) change
        mapM_ (mute output.outputId) muting
        pure Nothing
    report zone outcomes =
      let entry (output, outcome) =
            object $
              ["output" .= output.displayName, "ok" .= (outcome == Right Nothing)]
                <> either (\err -> ["error" .= describeError context err]) (maybe [] (\why -> ["error" .= why])) outcome
          failures = length [() | (_, outcome) <- outcomes, outcome /= Right Nothing]
          value = object ["zone" .= zone.displayName, "outputs" .= map entry outcomes, "failures" .= failures]
       in (structuredResult value) {isError = failures == length outcomes}

-- seek

data SeekArguments = SeekArguments Text Int Bool

instance FromJSON SeekArguments where
  parseJSON = withObject "arguments" $ \fields ->
    SeekArguments <$> fields .: "zone" <*> fields .: "seconds" <*> fields .:? "relative" .!= False

seekZone :: Effects es => Context -> SeekArguments -> Eff es ToolResult
seekZone context (SeekArguments wanted seconds relative) = withZone context wanted $ \zone ->
  if not zone.isSeekAllowed
    then pure (errorResult ("Seeking is not allowed in " <> zone.displayName <> " at the moment."))
    else
      textResult ("Done: " <> (if relative then "moved by " else "moved to ") <> Text.pack (show seconds) <> " seconds in " <> zone.displayName <> ".")
        <$ seek zone.zoneId (if relative then SeekBy seconds else SeekTo (max 0 seconds))

-- set_playback_options

data OptionsArguments = OptionsArguments Text (Maybe Bool) (Maybe Bool) (Maybe Text)

instance FromJSON OptionsArguments where
  parseJSON = withObject "arguments" $ \fields ->
    OptionsArguments <$> fields .: "zone" <*> fields .:? "shuffle" <*> fields .:? "auto_radio" <*> fields .:? "loop"

setOptions :: Effects es => Context -> OptionsArguments -> Eff es ToolResult
setOptions context (OptionsArguments wanted shuffle autoRadio loopText) = case traverse parseLoop loopText of
  Nothing -> pure (errorResult ("Unknown loop \"" <> maybe "" id loopText <> "\": use disabled, loop or loop_one."))
  Just loop
    | shuffle == Nothing && autoRadio == Nothing && loop == Nothing -> pure (errorResult "Give shuffle, auto_radio or loop.")
    | otherwise -> withZone context wanted $ \zone ->
        textResult ("Done: options changed in " <> zone.displayName <> ".")
          <$ changeSettings zone.zoneId SettingsChange {setShuffle = shuffle, setAutoRadio = autoRadio, setLoop = SetLoop <$> loop}
  where
    parseLoop name = lookup name [("disabled", LoopDisabled), ("loop", LoopQueue), ("loop_one", LoopOne)]

-- transfer_zone

data TransferArguments = TransferArguments Text Text

instance FromJSON TransferArguments where
  parseJSON = withObject "arguments" $ \fields -> TransferArguments <$> fields .: "from_zone" <*> fields .: "to_zone"

transfer :: Effects es => Context -> TransferArguments -> Eff es ToolResult
transfer context (TransferArguments fromName toName) =
  roon context id $
    getZones >>= \zones -> case (findZone fromName zones, findZone toName zones) of
      (Left problem, _) -> pure (errorResult problem)
      (_, Left problem) -> pure (errorResult problem)
      (Right from, Right to)
        | from.zoneId == to.zoneId -> pure (errorResult "The two zones are the same.")
        | otherwise -> textResult ("Done: moved from " <> from.displayName <> " to " <> to.displayName <> ".") <$ transferZone from.zoneId to.zoneId

-- get_image

data ImageArguments = ImageArguments Text Int

instance FromJSON ImageArguments where
  parseJSON = withObject "arguments" $ \fields ->
    ImageArguments <$> fields .: "image_key" <*> fields .:? "size" .!= 300

-- | Images come over HTTP, without a connection of the extension: no
-- registration, and no need to wait for the calls that hold one.
getImage :: Effects es => Context -> ImageArguments -> Eff es ToolResult
getImage context (ImageArguments key size) =
  runErrorNoCallStack (runImageHttp defaultImageLimits (fetchImage context.address key (fitWithin (clamp 32 1024 size)))) >>= \case
    Left err -> pure (errorResult (describeError context err))
    Right image -> pure $ case imageMediaType image.contentType of
      Just mediaType -> imageResult mediaType image.bytes
      Nothing -> errorResult ("Roon answered with \"" <> image.contentType <> "\", which is not an image.")

-- Schemas

noArguments :: Value
noArguments = object ["type" .= ("object" :: Text), "additionalProperties" .= False]

zoneProperty :: Value
zoneProperty = object ["type" .= ("string" :: Text), "description" .= ("A zone id or name, from list_zones." :: Text)]

browseSchema :: Value
browseSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "hierarchy" .= object ["type" .= ("string" :: Text), "enum" .= [name | (name, hierarchy) <- libraryHierarchies, hierarchy /= Search], "description" .= ("List this part of the library from its top." :: Text)]
          , "search" .= object ["type" .= ("string" :: Text), "description" .= ("Search the library for this text: artists, albums, tracks, playlists and more." :: Text)]
          , "item_id" .= object ["type" .= ("string" :: Text), "description" .= ("Open this item of kind \"list\" from an earlier browse_library." :: Text)]
          , "offset" .= object ["type" .= ("integer" :: Text), "minimum" .= (0 :: Int), "default" .= (0 :: Int)]
          , "max_items" .= object ["type" .= ("integer" :: Text), "minimum" .= (1 :: Int), "maximum" .= (100 :: Int), "default" .= (30 :: Int)]
          ]
    , "additionalProperties" .= False
    ]

playItemSchema :: Value
playItemSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "zone" .= zoneProperty
          , "item_id" .= object ["type" .= ("string" :: Text), "description" .= ("An item_id from browse_library, or a candidate id from search_tracks." :: Text)]
          , "action" .= object ["type" .= ("string" :: Text), "enum" .= map playActionName [minBound .. maxBound], "default" .= ("queue" :: Text)]
          ]
    , "required" .= ["zone" :: Text, "item_id"]
    , "additionalProperties" .= False
    ]

volumeSchema :: Value
volumeSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "zone" .= zoneProperty
          , "output" .= object ["type" .= ("string" :: Text), "description" .= ("An output id or name of the zone, from list_zones; all its outputs when left out." :: Text)]
          , "level" .= object ["type" .= ("number" :: Text), "description" .= ("The volume to set, in the units of the output." :: Text)]
          , "change" .= object ["type" .= ("number" :: Text), "description" .= ("How much to move the volume, in the units of the output; negative to lower it." :: Text)]
          , "mute" .= object ["type" .= ("boolean" :: Text)]
          ]
    , "required" .= ["zone" :: Text]
    , "additionalProperties" .= False
    ]

seekSchema :: Value
seekSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "zone" .= zoneProperty
          , "seconds" .= object ["type" .= ("integer" :: Text)]
          , "relative" .= object ["type" .= ("boolean" :: Text), "default" .= False]
          ]
    , "required" .= ["zone" :: Text, "seconds"]
    , "additionalProperties" .= False
    ]

optionsSchema :: Value
optionsSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "zone" .= zoneProperty
          , "shuffle" .= object ["type" .= ("boolean" :: Text)]
          , "auto_radio" .= object ["type" .= ("boolean" :: Text)]
          , "loop" .= object ["type" .= ("string" :: Text), "enum" .= ["disabled" :: Text, "loop", "loop_one"]]
          ]
    , "required" .= ["zone" :: Text]
    , "additionalProperties" .= False
    ]

transferSchema :: Value
transferSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties" .= object ["from_zone" .= zoneProperty, "to_zone" .= zoneProperty]
    , "required" .= ["from_zone" :: Text, "to_zone"]
    , "additionalProperties" .= False
    ]

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
