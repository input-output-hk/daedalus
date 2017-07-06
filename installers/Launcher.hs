module Launcher where

import qualified Data.List       as L
import           Data.Monoid     ((<>))
import           System.FilePath (pathSeparator)

-- OS dependent configuration
data Launcher = Launcher
    { nodePath      :: String
    , nodeLogPath   :: String
    , walletPath    :: String
    , installerPath :: String
    , runtimePath   :: String
    }

launcherArgs :: Launcher -> String
launcherArgs launcher = unwords $
  [ "--node", quote (nodePath launcher)
  , "--node-log-path", quote (nodeLogPath launcher)
  , "--wallet", quote (walletPath launcher)
  , "--updater ", quote (installerPath launcher)
  , "--node-timeout 5"
  , (" -n " ++ (L.intercalate " -n " nodeArgs))
  ]
    where
      nodeArgs = [
        "--report-server", "http://report-server.aws.iohk.io:8080",
        "--log-config", "log-config-prod.yaml",
        "--update-latest-path", quote (installerPath launcher),
        "--keyfile", quote (runtimePath launcher <> "Secrets-0.5" <> (pathSeparator : "secret.key")),
        "--logs-prefix", quote (runtimePath launcher <> "Logs"),
        "--db-path", quote (runtimePath launcher <> "DB-0.5"),
        "--wallet-db-path", quote (runtimePath launcher <> "Wallet-0.5"),
        "--kademlia-peers-file", "ip-dht-mappings",
        "--system-start", "1499360281",
        "--wallet",
        "--static-peers"
        ]

quote :: String -> String
quote p = "\"" <> p <> "\""
