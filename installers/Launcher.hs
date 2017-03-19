module Launcher where

import qualified Data.List          as L
import           Data.Monoid        ((<>))
import           System.FilePath    (pathSeparator)


data Launcher = Launcher
    { nodePath :: String
    , nodeLogPath :: String
    , walletPath :: String
    , installerPath :: String
    , runtimePath :: String
    , reportServerUrl :: String
    , systemStart :: Maybe Int
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
        "--listen", "0.0.0.0:12100",
        "--report-server", reportServerUrl launcher,
        "--log-config", "log-config-prod.yaml",
        "--update-latest-path", quote (installerPath launcher),
        "--keyfile", quote (runtimePath launcher <> "Secrets" <> (pathSeparator : "secret.key")),
        "--logs-prefix", quote (runtimePath launcher <> "Logs"),
        "--db-path", quote (runtimePath launcher <> "DB-0.2"),
        "--wallet-db-path", quote (runtimePath launcher <> "Wallet-0.2"),
        "--peers-file", "ip-dht-mappings",
        "--wallet"
        ] ++ sysStartArgs
      sysStartArgs = maybe [] impl $ systemStart launcher
        where
          impl time = ["--system-start", show time]


quote :: String -> String
quote p = "\"" <> p <> "\""
