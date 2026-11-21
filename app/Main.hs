-- | @roon-mcp@: an MCP server, on stdio, that plays music on Roon.
--
-- @roon-mcp register@ registers the extension once, waiting for the user
-- to enable it in Roon.
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.MVar (newMVar)
import Effectful.Environment (Environment, getArgs, lookupEnv, runEnvironment)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO (stderr)
import Effectful.FileSystem.IO.ByteString (hPutStrLn)
import Effectful.MCP.Server (ServerConfig (..), implementation, serveStdio, serverConfig)
import Roon.Connection (runRoon)
import Roon.Discovery (Core (..), defaultDiscovery, discover)
import Roon.Effect.TokenStore (defaultTokenFile, runTokenStoreFile)
import Roon.Effect.WebSocket (ServerAddress (..), localServer, runWebSocket)
import Roon.Error (RoonError)
import Roon.Registry (ExtensionInfo (..))
import RoonMcp.Tools (Context (..), describeError, extension, instructions, tools, serverVersion)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = do
  succeeded <- runEff . runConcurrent . runFileSystem . runEnvironment $ program
  if succeeded then pure () else exitFailure

type Effects = '[Environment, FileSystem, Concurrent, IOE]

-- | 'False' when the program failed.
program :: Eff Effects Bool
program = do
  context <- configure
  getArgs >>= \case
    [] -> do
      serveStdio
        (serverConfig (implementation "roon-mcp" serverVersion) (tools context))
          { instructions = Just instructions
          }
      pure True
    ["register"] -> register context
    _ -> do
      complain "usage: roon-mcp [register]   (ROON_HOST and ROON_PORT choose the server, otherwise it is discovered, ROON_CORE choosing among several; ROON_TRACE=1 describes every frame on stderr)"
      pure False

-- | The server address, the token file and the options.
configure :: Eff Effects Context
configure = do
  address <- serverAddress
  tokenFile <- defaultTokenFile extension.extensionId
  connectionLock <- newMVar ()
  -- ROON_TRACE, when set, describes every frame on stderr.
  tracing <- maybe False (not . null) <$> lookupEnv "ROON_TRACE"
  pure
    Context
      { address
      , tokenFile
      , registrationTimeout = 5_000_000
      , traceLimit = if tracing then Just 300 else Nothing
      , connectionLock
      }

-- | ROON_HOST and ROON_PORT when ROON_HOST is set. Otherwise the server
-- that answers a discovery on the network, the one named or identified by
-- ROON_CORE if there are several; failing that, 127.0.0.1:9330.
serverAddress :: Eff Effects ServerAddress
serverAddress =
  lookupEnv "ROON_HOST" >>= \case
    Just host -> do
      port <- (>>= readMaybe) <$> lookupEnv "ROON_PORT"
      pure ServerAddress {host, port = maybe localServer.port id port}
    Nothing -> do
      wanted <- fmap Text.pack <$> lookupEnv "ROON_CORE"
      cores <- discover defaultDiscovery
      let chosen = case wanted of
            Nothing -> cores
            Just name -> [core | core <- cores, core.uniqueId == name || core.name == Just name]
      case chosen of
        core : others -> do
          complain ("Using the Roon Server " <> maybe core.uniqueId id core.name <> " at " <> describe core.address <> if null others then "." else ", the first of several: ROON_CORE chooses one.")
          pure core.address
        [] -> do
          complain ("No Roon Server " <> maybe "" (\name -> "named " <> name <> " ") wanted <> "answered the discovery; using " <> describe localServer <> ".")
          pure localServer
  where
    describe address = Text.pack address.host <> ":" <> Text.pack (show address.port)

-- | Register, waiting as long as the user takes to enable the extension.
register :: Context -> Eff Effects Bool
register context = do
  complain ("Connecting to " <> Text.pack context.address.host <> ":" <> Text.pack (show context.address.port) <> ". Enable \"roon-mcp\" in Roon, under Settings, Extensions.")
  outcome <-
    runErrorNoCallStack @RoonError . runTokenStoreFile context.tokenFile . runWebSocket context.address $
      runRoon extension (const (pure ()))
  case outcome of
    Right () -> True <$ complain ("Registered. The token is in " <> Text.pack context.tokenFile <> ".")
    Left err -> False <$ complain ("roon-mcp: " <> describeError context err)

-- | Write to stderr: stdout carries the protocol.
complain :: FileSystem :> es => Text -> Eff es ()
complain = hPutStrLn stderr . encodeUtf8

