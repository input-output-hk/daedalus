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
    , installerArgs        :: [String]
    , installerArchivePath :: Maybe String
    , runtimePath          :: String
    }

launcherArgs :: Launcher -> String
launcherArgs launcher = unwords $
    maybe [] (("--updater-windows-runner":) . (:[]) . quote) (windowsInstallerPath launcher) ++
  [ "--node", quote (nodePath launcher)
  , "--node-log-path", quote (nodeLogPath launcher)
  , "--wallet", quote (walletPath launcher)
  , "--updater", quote (installerPath launcher)
  , unwords $ map ("-u " ++) (installerArgs launcher)
  , maybe "" (("--update-archive " ++) . quote) (installerArchivePath launcher)
  , "--node-timeout 5"
  , unwords $ map ("-n " ++) nodeArgs
  ]
    where
      nodeArgs = [
        "--report-server", "http://report-server.aws.iohkdev.io:8080",
        "--log-config", "log-config-prod.yaml",
        "--update-latest-path", quote (installerPath launcher),
        "--keyfile", quote (runtimePath launcher <> "Secrets-0.5.1" <> (pathSeparator : "secret.key")),
        "--logs-prefix", quote (runtimePath launcher <> "Logs"),
        "--db-path", quote (runtimePath launcher <> "DB-0.5.1"),
        "--wallet-db-path", quote (runtimePath launcher <> "Wallet-0.5.1"),
        "--kademlia-peers-file", "ip-dht-mappings",
        "--system-start", "1499433433",
        "--wallet",
        "--static-peers"
        ]

quote :: String -> String
quote p = "\"" <> p <> "\""
