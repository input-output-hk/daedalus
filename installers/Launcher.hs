module Launcher where

import           Data.Monoid     ((<>))
import           System.FilePath (pathSeparator)
import           System.Info     (os)

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
  , "--node-timeout 5 " ++ batchCmdNewline
  , unwords $ map (\x ->  batchCmdNewline ++ "-n " ++ x) nodeArgs
  ]
    where
      nodeArgs = [
        "--report-server", "http://report-server.aws.iohkdev.io:8080",
        "--log-config", "log-config-prod.yaml",
        "--update-latest-path", quote (installerPath launcher),
        "--keyfile", quote (runtimePath launcher <> "Secrets-0.5" <> (pathSeparator : "secret.key")),
        "--logs-prefix", quote (runtimePath launcher <> "Logs"),
        "--db-path", quote (runtimePath launcher <> "DB-0.5"),
        "--wallet-db-path", quote (runtimePath launcher <> "Wallet-0.5"),
        "--update-server", "http://localhost:8080/",
        "--system-start", "1499360281",
        "--update-with-package",
        "--tlscert", quote (runtimePath launcher <> tlsPath <> "server" <> (pathSeparator : "server.crt")),
        "--tlskey", quote (runtimePath launcher <> tlsPath <> "server" <> (pathSeparator : "server.key")),
        "--tlsca", quote (runtimePath launcher <> tlsPath <> "ca" <> (pathSeparator : "ca.crt"))
        ] <> walletTopology
      tlsPath = "tls" <> (pathSeparator : [])
      -- NOTE: looks like windows *.bat file is cut of on 1024 characters per line. This is a workaround
      walletTopology  | os == "mingw32" = ["--topology", quote "%DAEDALUS_DIR%\\wallet-topology.yaml"]
                      | otherwise = mempty
      batchCmdNewline | os == "mingw32" = "^\r\n"
                      | otherwise = mempty

quote :: String -> String
quote p = "\"" <> p <> "\""
