module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.KeyMap qualified as JK (lookup)
import Data.Foldable (toList)
import Data.Text (Text)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.MCP.Server (handleRequest, implementation, serverConfig)
import Roon.Browse (Candidate (..), Hierarchy (..), Item (..), ItemHint (..),
                    Session (..), TrackAction (..))
import Roon.Effect.WebSocket (localServer)
import Roon.Transport (Output (..), OutputId (..), PlaybackState (..),
                       Volume (..), VolumeType (..), Zone (..), ZoneId (..))
import RoonMcp.Plan (ItemKind (KindList, KindPlayable),
                     ItemRef (ItemRef, key, kind, session),
                     Mode (ModeAddNext, ModePlayNow, ModeQueue),
                     PlayAction (ActAddNext, ActPlayNow, ActQueue, ActShuffle, ActStartRadio),
                     candidateId, decodeCandidateId, decodeItemId, findZone,
                     imageMediaType, itemId, outputsFor, parseMode,
                     parsePlayAction, plainText, plan, playActionTitle,
                     volumeWithin)
import RoonMcp.Tools (Context (..), tools)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "roon-mcp"
    [ testGroup
        "plan"
        [ testCase "queue: in order, at the end" $
            plan ModeQueue "abc" @?= [(0, Queue, 'a'), (1, Queue, 'b'), (2, Queue, 'c')]
        , testCase "add_next: in reverse, so that they play in order" $
            plan ModeAddNext "abc" @?= [(2, AddNext, 'c'), (1, AddNext, 'b'), (0, AddNext, 'a')]
        , testCase "play_now: the first at once, the others after it in reverse" $
            plan ModePlayNow "abc" @?= [(0, PlayNow, 'a'), (2, AddNext, 'c'), (1, AddNext, 'b')]
        , testCase "play_now with one track" $ plan ModePlayNow "a" @?= [(0, PlayNow, 'a')]
        , testCase "modes by name" $ map parseMode ["queue", "add_next", "play_now", "shuffle"] @?= [Just ModeQueue, Just ModeAddNext, Just ModePlayNow, Nothing]
        ]
    , testGroup
        "candidate ids"
        [ testCase "carry the session and the item key" $ do
            let found = Candidate {session = Session Search (Just "roon-mcp-1-0"), item = trackItem "First Song" "2:0"}
            candidateId found @?= Just "roon-mcp-1-0|2:0"
            fmap (\candidate -> (candidate.session, candidate.item.itemKey)) (decodeCandidateId "roon-mcp-1-0|2:0")
              @?= Just (Session Search (Just "roon-mcp-1-0"), Just "2:0")
        , testCase "reject what is not one" $ map decodeCandidateId ["", "nokey", "|2:0", "session|"] @?= [Nothing, Nothing, Nothing, Nothing]
        ]
    , testGroup
        "zones"
        [ testCase "by id" $ fmap (.displayName) (findZone "zone-0002" zones) @?= Right "Kitchen"
        , testCase "by name, ignoring case" $ fmap (.zoneId) (findZone "living room" zones) @?= Right (ZoneId "zone-0001")
        , testCase "an unknown zone lists the zones" $
            findZone "Garage" zones @?= Left "No zone named or identified as \"Garage\". Zones: Living Room (zone-0001), Kitchen (zone-0002)"
        ]
    , testGroup
        "item ids"
        [ testCase "carry the hierarchy, the session, the kind and the key" $ do
            let current = Session Albums (Just "roon-mcp-b-1")
            itemId current (trackItem "Play Album" "85:0") @?= Just "albums|roon-mcp-b-1|p|85:0"
            itemId current (trackItem "An Album" "83:4") {hint = Just ListItem} @?= Just "albums|roon-mcp-b-1|l|83:4"
            itemId current (trackItem "Play Now" "88:0") {hint = Just Action} @?= Just "albums|roon-mcp-b-1|a|88:0"
            decodeItemId "albums|roon-mcp-b-1|l|83:4" @?= Just ItemRef {session = current, kind = KindList, key = "83:4"}
            decodeItemId "search|s|p|2:0" @?= Just ItemRef {session = Session Search (Just "s"), kind = KindPlayable, key = "2:0"}
        , testCase "items without a key, sessions without one and the settings have no id" $ do
            itemId (Session Albums (Just "s")) (trackItem "No Results" "x") {itemKey = Nothing} @?= Nothing
            itemId (Session Albums Nothing) (trackItem "An Album" "1:0") @?= Nothing
            itemId (Session Settings (Just "s")) (trackItem "Setup" "1:0") @?= Nothing
        , testCase "reject what is not one" $
            map decodeItemId ["", "albums|s|l", "settings|s|l|1:0", "albums||l|1:0", "albums|s|z|1:0", "albums|s|l|", "roon-mcp-1-0|2:0"]
              @?= replicate 7 Nothing
        ]
    , testGroup
        "texts of Roon"
        [ testCase "links are given as their text" $
            map plainText ["Miles Davis", "[[487290|Miles Davis]]", "[[1|A]] / [[2|B]] and C", "open [[ only", "[[3|unclosed"]
              @?= ["Miles Davis", "Miles Davis", "A / B and C", "open [[ only", "[[3|unclosed"]
        ]
    , testGroup
        "play actions"
        [ testCase "by name" $
            map parsePlayAction ["play_now", "add_next", "queue", "start_radio", "shuffle", "Play Now", ""]
              @?= [Just ActPlayNow, Just ActAddNext, Just ActQueue, Just ActStartRadio, Just ActShuffle, Nothing, Nothing]
        , testCase "as Roon titles them" $
            map playActionTitle [minBound .. maxBound] @?= ["Play Now", "Add Next", "Queue", "Start Radio", "Shuffle"]
        ]
    , testGroup
        "volume"
        [ testCase "all the outputs of a zone, or the one named or identified" $ do
            let zone = (zoneNamed "zone-0001" "Living Room") {outputs = [output "out-1" "Amplifier", output "out-2" "Kitchen Speaker"]}
            fmap (map (.displayName)) (outputsFor Nothing zone) @?= Right ["Amplifier", "Kitchen Speaker"]
            fmap (map (.displayName)) (outputsFor (Just "kitchen speaker") zone) @?= Right ["Kitchen Speaker"]
            fmap (map (.displayName)) (outputsFor (Just "out-1") zone) @?= Right ["Amplifier"]
            fmap (map (.displayName)) (outputsFor (Just "Garage") zone)
              @?= Left "No output named or identified as \"Garage\" in Living Room. Outputs: Amplifier, Kitchen Speaker"
        , testCase "a level is kept within what the output accepts" $ do
            let bounded = Volume {volumeType = VolumeDecibel, minimum = Just (-80), maximum = Just 0, value = Just (-30), step = Just 1, isMuted = Just False}
            map (volumeWithin bounded) [-100, -30, 12] @?= [-80, -30, 0]
            volumeWithin bounded {minimum = Nothing, maximum = Nothing} 500 @?= 500
        ]
    , testGroup
        "image media types"
        [ testCase "parameters and case are dropped" $
            map imageMediaType ["image/jpeg", "Image/PNG; q=1", " image/webp "] @?= [Just "image/jpeg", Just "image/png", Just "image/webp"]
        , testCase "what is not an image is refused" $
            map imageMediaType ["", "text/plain", "image/", "application/json"] @?= [Nothing, Nothing, Nothing, Nothing]
        ]
    , testCase "tools/list gives the twelve tools, each with an object schema" $ do
        lock <- newMVar ()
        let context = Context {address = localServer, tokenFile = "/nonexistent/tokens.json", registrationTimeout = 1, traceLimit = Nothing, connectionLock = lock}
        result <- runEff . runConcurrent . runFileSystem $
          handleRequest (serverConfig (implementation "roon-mcp" "0") (tools context)) "tools/list" (fromList [("_meta", meta)])
        case result of
          Right (Object fields) | Just (Array listed) <- JK.lookup "tools" fields -> do
            map (field "name") (toList listed) @?= map (Just . String) ["list_zones", "get_queue", "search_tracks", "queue_tracks", "control", "browse_library", "play_item", "set_volume", "seek", "set_playback_options", "transfer_zone", "get_image"]
            map (\definition -> field "inputSchema" definition >>= field "type") (toList listed) @?= replicate 12 (Just (String "object"))
          other -> assertFailure ("unexpected: " <> show other)
    ]
  where
    meta = object ["io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text), "io.modelcontextprotocol/clientCapabilities" .= object []]
    field key = \case
      Object fields -> JK.lookup key fields
      _ -> Nothing

trackItem :: Text -> Text -> Item
trackItem title key = Item {title, subtitle = Nothing, imageKey = Nothing, itemKey = Just key, hint = Just ActionList, inputPrompt = Nothing}

output :: Text -> Text -> Output
output identifier name =
  Output {outputId = OutputId identifier, zoneId = ZoneId "zone-0001", displayName = name, volume = Nothing, sourceControls = [], canGroupWithOutputIds = []}

zones :: [Zone]
zones = [zoneNamed "zone-0001" "Living Room", zoneNamed "zone-0002" "Kitchen"]

zoneNamed :: Text -> Text -> Zone
zoneNamed identifier name =
  Zone
    { zoneId = ZoneId identifier
    , displayName = name
    , outputs = []
    , state = Stopped
    , isPreviousAllowed = False
    , isNextAllowed = False
    , isPauseAllowed = False
    , isPlayAllowed = True
    , isSeekAllowed = False
    , queueItemsRemaining = Nothing
    , queueTimeRemaining = Nothing
    , settings = Nothing
    , nowPlaying = Nothing
    }
