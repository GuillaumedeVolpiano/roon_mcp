-- |
-- Module      : RoonMcp.Plan
-- Description : The pure decisions of the server: candidate ids, zones,
--               and the order of the actions that queue tracks
-- License     : BSD-3-Clause
module RoonMcp.Plan
  ( -- * Candidate ids
    candidateId
  , decodeCandidateId

    -- * Zones
  , findZone

    -- * Queueing
  , Mode (..)
  , modeName
  , parseMode
  , plan

    -- * Library items
  , ItemKind (..)
  , ItemRef (..)
  , itemKind
  , kindName
  , itemId
  , decodeItemId
  , libraryHierarchies
  , plainText
  , PlayAction (..)
  , playActionName
  , playActionTitle
  , parsePlayAction

    -- * Volume
  , outputsFor
  , volumeWithin

    -- * Images
  , imageMediaType
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Roon.Browse (Candidate (..), Hierarchy (..), Item (..), ItemHint (..), Session (..), TrackAction (..))
import Roon.Transport (Output (..), OutputId (..), Volume (..), Zone (..), ZoneId (..))

-- | An id that carries all a later call needs: the browsing session and
-- the key of the item in it. The server keeps nothing between calls.
candidateId :: Candidate -> Maybe Text
candidateId Candidate {session = Session {multiSessionKey = Just key}, item = Item {itemKey = Just itemKey}} =
  Just (key <> "|" <> itemKey)
candidateId _ = Nothing

-- | The candidate an id stands for. Its title is the id itself, since the
-- id does not carry the title.
decodeCandidateId :: Text -> Maybe Candidate
decodeCandidateId text = case Text.breakOn "|" text of
  (key, rest)
    | not (Text.null key)
    , Just itemKey <- Text.stripPrefix "|" rest
    , not (Text.null itemKey) ->
        Just
          Candidate
            { session = Session {hierarchy = Search, multiSessionKey = Just key}
            , item = Item {title = text, subtitle = Nothing, imageKey = Nothing, itemKey = Just itemKey, hint = Just ActionList, inputPrompt = Nothing}
            }
  _ -> Nothing

-- | The zone with this id or, failing that, this name, ignoring case.
-- 'Left' says why none fits, with the zones there are.
findZone :: Text -> [Zone] -> Either Text Zone
findZone wanted zones = case [zone | zone <- zones, zone.zoneId == ZoneId wanted] of
  zone : _ -> Right zone
  [] -> case [zone | zone <- zones, Text.toCaseFold zone.displayName == Text.toCaseFold wanted] of
    [zone] -> Right zone
    [] -> Left ("No zone named or identified as \"" <> wanted <> "\". Zones: " <> known)
    _ -> Left ("Several zones are named \"" <> wanted <> "\"; use a zone id. Zones: " <> known)
  where
    known = Text.intercalate ", " [zone.displayName <> " (" <> unZoneId zone.zoneId <> ")" | zone <- zones]
    unZoneId (ZoneId zoneId) = zoneId

-- | How to put tracks in a queue.
data Mode
  = -- | At the end of the queue, in order.
    ModeQueue
  | -- | Right after the current track, in order.
    ModeAddNext
  | -- | In place of the queue: the first track at once, the others right
    -- after it, in order.
    ModePlayNow
  deriving stock (Eq, Show, Enum, Bounded)

-- | The name of a mode in the arguments of a tool.
modeName :: Mode -> Text
modeName = \case
  ModeQueue -> "queue"
  ModeAddNext -> "add_next"
  ModePlayNow -> "play_now"

-- | The mode a name stands for.
parseMode :: Text -> Maybe Mode
parseMode name = lookup name [(modeName mode, mode) | mode <- [minBound .. maxBound]]

-- | The actions to perform, in the order to perform them, each with the
-- position of its track in the request. Roon's Add Next puts a track right
-- after the current one, so a list added next goes in reverse order.
plan :: Mode -> [a] -> [(Int, TrackAction, a)]
plan mode tracks = case mode of
  ModeQueue -> [(index, Queue, track) | (index, track) <- indexed]
  ModeAddNext -> [(index, AddNext, track) | (index, track) <- reverse indexed]
  ModePlayNow -> case indexed of
    [] -> []
    (index, first) : rest -> (index, PlayNow, first) : [(i, AddNext, track) | (i, track) <- reverse rest]
  where
    indexed = zip [0 ..] tracks

-- | The media type of an image from a @Content-Type@: its parameters
-- dropped, in lower case. 'Nothing' when it does not name an image.
imageMediaType :: Text -> Maybe Text
imageMediaType contentType
  | "image/" `Text.isPrefixOf` mediaType && Text.length mediaType > 6 = Just mediaType
  | otherwise = Nothing
  where
    mediaType = Text.toLower (Text.strip (Text.takeWhile (/= ';') contentType))

-- | What an item of the library is to the caller.
data ItemKind
  = -- | Opens another listing.
    KindList
  | -- | Something to play: a track, or what plays a whole album.
    KindPlayable
  | -- | An action of Roon. Never opened from an id: it would be performed.
    KindAction
  | -- | Only displayed.
    KindOther
  deriving stock (Eq, Show, Enum, Bounded)

-- | The kind of an item, from its hint.
itemKind :: Item -> ItemKind
itemKind item = case item.hint of
  Just ListItem -> KindList
  Just ActionList -> KindPlayable
  Just Action -> KindAction
  _ -> KindOther

-- | The name of a kind in the results of a tool.
kindName :: ItemKind -> Text
kindName = \case
  KindList -> "list"
  KindPlayable -> "playable"
  KindAction -> "action"
  KindOther -> "text"

kindLetter :: ItemKind -> Text
kindLetter = Text.take 1 . kindName

-- | An item of a listing, as its id names it.
data ItemRef = ItemRef
  { session :: Session
  , kind :: ItemKind
  , key :: Text
  }
  deriving stock (Eq, Show)

-- | The hierarchies of Roon that the library tools open, by name. The
-- settings of Roon are left out.
libraryHierarchies :: [(Text, Hierarchy)]
libraryHierarchies =
  [ ("albums", Albums)
  , ("artists", Artists)
  , ("playlists", Playlists)
  , ("genres", Genres)
  , ("composers", Composers)
  , ("internet_radio", InternetRadio)
  , ("search", Search)
  ]

-- | An id that carries all a later call needs: the hierarchy, the key of
-- the browsing session, what the item is, and its key in the session. The
-- server keeps nothing between calls.
itemId :: Session -> Item -> Maybe Text
itemId Session {hierarchy, multiSessionKey = Just sessionKey} item@Item {itemKey = Just key}
  | Just name <- lookup hierarchy [(found, name) | (name, found) <- libraryHierarchies] =
      Just (Text.intercalate "|" [name, sessionKey, kindLetter (itemKind item), key])
itemId _ _ = Nothing

-- | The item an id names; 'Nothing' when the text is not an id.
decodeItemId :: Text -> Maybe ItemRef
decodeItemId text = case Text.splitOn "|" text of
  [name, sessionKey, letter, key]
    | Just hierarchy <- lookup name libraryHierarchies
    , kind : _ <- [found | found <- [minBound .. maxBound], kindLetter found == letter]
    , not (Text.null sessionKey)
    , not (Text.null key) ->
        Just ItemRef {session = Session {hierarchy, multiSessionKey = Just sessionKey}, kind, key}
  _ -> Nothing

-- | A text of Roon without its links: @[[487290|Miles Davis]]@ is given
-- as @Miles Davis@.
plainText :: Text -> Text
plainText text = case Text.breakOn "[[" text of
  (before, "") -> before
  (before, rest) -> case Text.breakOn "]]" (Text.drop 2 rest) of
    (_, "") -> text
    (link, after) -> before <> snd (Text.breakOnEnd "|" link) <> plainText (Text.drop 2 after)

-- | What to do with something to play.
data PlayAction = ActPlayNow | ActAddNext | ActQueue | ActStartRadio | ActShuffle
  deriving stock (Eq, Show, Enum, Bounded)

-- | The name of an action in the arguments of a tool.
playActionName :: PlayAction -> Text
playActionName = \case
  ActPlayNow -> "play_now"
  ActAddNext -> "add_next"
  ActQueue -> "queue"
  ActStartRadio -> "start_radio"
  ActShuffle -> "shuffle"

-- | The English title of the action in Roon.
playActionTitle :: PlayAction -> Text
playActionTitle = \case
  ActPlayNow -> "Play Now"
  ActAddNext -> "Add Next"
  ActQueue -> "Queue"
  ActStartRadio -> "Start Radio"
  ActShuffle -> "Shuffle"

-- | The action a name stands for.
parsePlayAction :: Text -> Maybe PlayAction
parsePlayAction name = lookup name [(playActionName action, action) | action <- [minBound .. maxBound]]

-- | The outputs of a zone a volume change is for: the one with this id or
-- this name, or all of them when none is asked for.
outputsFor :: Maybe Text -> Zone -> Either Text [Output]
outputsFor Nothing zone = Right zone.outputs
outputsFor (Just wanted) zone = case [output | output <- zone.outputs, output.outputId == OutputId wanted || Text.toCaseFold output.displayName == Text.toCaseFold wanted] of
  [] -> Left ("No output named or identified as \"" <> wanted <> "\" in " <> zone.displayName <> ". Outputs: " <> Text.intercalate ", " (map (.displayName) zone.outputs))
  found -> Right found

-- | A volume kept within what the output accepts, when it says.
volumeWithin :: Volume -> Double -> Double
volumeWithin volume = maybe id min volume.maximum . maybe id max volume.minimum
