module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.Text (Text)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.MCP.Server (handleRequest, implementation, serverConfig)
import Roon.Browse (Candidate (..), Hierarchy (..), Item (..), ItemHint (..), Session (..), TrackAction (..))
import Roon.Effect.WebSocket (localServer)
import Roon.Transport (PlaybackState (..), Zone (..), ZoneId (..))
import RoonMcp.Plan
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
        "image media types"
        [ testCase "parameters and case are dropped" $
            map imageMediaType ["image/jpeg", "Image/PNG; q=1", " image/webp "] @?= [Just "image/jpeg", Just "image/png", Just "image/webp"]
        , testCase "what is not an image is refused" $
            map imageMediaType ["", "text/plain", "image/", "application/json"] @?= [Nothing, Nothing, Nothing, Nothing]
        ]
    , testCase "tools/list gives the six tools, each with an object schema" $ do
        lock <- newMVar ()
        let context = Context {address = localServer, tokenFile = "/nonexistent/tokens.json", registrationTimeout = 1, traceLimit = Nothing, connectionLock = lock}
        result <- runEff . runConcurrent . runFileSystem $
          handleRequest (serverConfig (implementation "roon-mcp" "0") (tools context)) "tools/list" (KeyMap.fromList [("_meta", meta)])
        case result of
          Right (Object fields) | Just (Array listed) <- KeyMap.lookup "tools" fields -> do
            map (field "name") (toList listed) @?= map (Just . String) ["list_zones", "get_queue", "search_tracks", "queue_tracks", "control", "get_image"]
            map (\definition -> field "inputSchema" definition >>= field "type") (toList listed) @?= replicate 6 (Just (String "object"))
          other -> assertFailure ("unexpected: " <> show other)
    ]
  where
    meta = object ["io.modelcontextprotocol/protocolVersion" .= ("2026-07-28" :: Text), "io.modelcontextprotocol/clientCapabilities" .= object []]
    field key = \case
      Object fields -> KeyMap.lookup key fields
      _ -> Nothing

trackItem :: Text -> Text -> Item
trackItem title key = Item {title, subtitle = Nothing, imageKey = Nothing, itemKey = Just key, hint = Just ActionList, inputPrompt = Nothing}

zones :: [Zone]
zones = [zone "zone-0001" "Living Room", zone "zone-0002" "Kitchen"]
  where
    zone identifier name =
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
