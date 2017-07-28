module Launcher where

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
  , "--node-timeout 5 ^\r\n"
  , unwords $ map (\x -> "-n " ++ x ++ " ^\r\n") nodeArgs
  ]
    where
      nodeArgs = [
        "--report-server", "http://report-server.aws.iohk.io:8080",
        "--listen", "127.0.0.1:12100",
        "--log-config", "log-config-prod.yaml",
        "--update-latest-path", quote (installerPath launcher),
        "--keyfile", quote (runtimePath launcher <> "Secrets" <> (pathSeparator : "secret.key")),
        "--logs-prefix", quote (runtimePath launcher <> "Logs"),
        "--db-path", quote (runtimePath launcher <> "DB-0.5"),
        "--wallet-db-path", quote (runtimePath launcher <> "Wallet-0.5"),
        "--kademlia-peers-file", "ip-dht-mappings",
        "--kademlia-explicit-initial",
        "--update-server", "http://localhost:8080/",
        "--system-start", "1499360281",
        "--wallet",
        "--update-with-package",
        "--tlscert", quote (runtimePath launcher <> tlsPath <> "server" <> (pathSeparator : "server.crt")),
        "--tlskey", quote (runtimePath launcher <> tlsPath <> "server" <> (pathSeparator : "server.key")),
        "--tlsca", quote (runtimePath launcher <> tlsPath <> "ca" <> (pathSeparator : "ca.crt")),
        "--static-peers"
        ]
      tlsPath = "tls" <> (pathSeparator : [])

quote :: String -> String
quote p = "\"" <> p <> "\""
