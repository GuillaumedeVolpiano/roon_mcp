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
import Roon.Effect.TokenStore (defaultTokenFile, runTokenStoreFile)
import Roon.Effect.WebSocket (ServerAddress (..), localServer, runWebSocket)
import Roon.Error (RoonError)
import Roon.Registry (ExtensionInfo (..))
import RoonMcp.Tools (Context (..), describeError, extension, instructions, tools)
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
        (serverConfig (implementation "roon-mcp" "0.1.0.0") (tools context))
          { instructions = Just instructions
          }
      pure True
    ["register"] -> register context
    _ -> do
      complain "usage: roon-mcp [register]   (ROON_HOST and ROON_PORT choose the server; default 127.0.0.1:9330)"
      pure False

-- | The server address from ROON_HOST and ROON_PORT, and the token file.
configure :: Eff Effects Context
configure = do
  host <- maybe localServer.host id <$> lookupEnv "ROON_HOST"
  port <- (>>= readMaybe) <$> lookupEnv "ROON_PORT"
  tokenFile <- defaultTokenFile extension.extensionId
  connectionLock <- newMVar ()
  pure
    Context
      { address = ServerAddress {host, port = maybe localServer.port id port}
      , tokenFile
      , registrationTimeout = 5_000_000
      , connectionLock
      }

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

