module Launcher where

import qualified Data.List       as L
import           Data.Monoid     ((<>))
import           System.FilePath (pathSeparator)

-- OS dependent configuration
data Launcher = Launcher
    { nodePath             :: String
    , nodeLogPath          :: String
    , walletPath           :: String
    , installerPath        :: String
    , windowsInstallerPath :: Maybe String
    , runtimePath          :: String
    }

launcherArgs :: Launcher -> String
launcherArgs launcher = unwords $
    maybe [] (("--updater-windows-runner":) . (:[]) . quote) (windowsInstallerPath launcher) ++
  [ "--node", quote (nodePath launcher)
  , "--node-log-path", quote (nodeLogPath launcher)
  , "--wallet", quote (walletPath launcher)
  , "--updater ", quote (installerPath launcher)
  , "--node-timeout 5"
  , (" -n " ++ (L.intercalate " -n " nodeArgs))
  ]
    where
      nodeArgs = [
        "--listen", "127.0.0.1:12100",
        "--report-server", "http://52.59.7.118:8080",
        "--log-config", "log-config-prod.yaml",
        "--update-latest-path", quote (installerPath launcher),
        "--keyfile", quote (runtimePath launcher <> "Secrets" <> (pathSeparator : "secret.key")),
        "--logs-prefix", quote (runtimePath launcher <> "Logs"),
        "--db-path", quote (runtimePath launcher <> "DB-0.4"),
        "--wallet-db-path", quote (runtimePath launcher <> "Wallet-0.4"),
        "--peers-file", "ip-dht-mappings",
        "--system-start", "1492428658",
        "--wallet",
        "--explicit-initial"
        ]

quote :: String -> String
quote p = "\"" <> p <> "\""
