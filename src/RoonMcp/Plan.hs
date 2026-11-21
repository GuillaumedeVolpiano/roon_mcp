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

    -- * Images
  , imageMediaType
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Roon.Browse (Candidate (..), Hierarchy (..), Item (..), ItemHint (..), Session (..), TrackAction (..))
import Roon.Transport (Zone (..), ZoneId (..))

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

modeName :: Mode -> Text
modeName = \case
  ModeQueue -> "queue"
  ModeAddNext -> "add_next"
  ModePlayNow -> "play_now"

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
