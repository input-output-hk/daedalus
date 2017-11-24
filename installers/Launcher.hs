{-# LANGUAGE RecordWildCards #-}
module Launcher where

import           Data.Monoid     ((<>))
import           System.FilePath (pathSeparator)
import           System.Info     (os)

data Updater =
    SelfUnpacking
      { updArchivePath :: FilePath
      , updArgs        :: [String]
      }
  | WithUpdater
      { updArchivePath :: FilePath
      , updExec        :: FilePath
      , updArgs        :: [String]
      }

-- OS dependent configuration
data Launcher = Launcher
    { nodePath             :: FilePath
    , nodeLogPath          :: FilePath
    , launcherLogPath      :: FilePath
    , walletPath           :: FilePath
    , runtimePath          :: FilePath
    , updater              :: Updater
    , windowsInstallerPath :: Maybe FilePath
    }

launcherArgs :: Launcher -> String
launcherArgs Launcher{..} = unwords $
    maybe [] (\wi -> ["--updater-windows-runner", quote wi]) windowsInstallerPath ++
  [ "--node", quote nodePath
  , "--node-log-path", quote nodeLogPath
  , "--db-path", quote nodeDbPath
  , "--wallet", quote walletPath
  , "--launcher-logs-prefix", quote launcherLogPath
  ] ++ updaterLArgs ++ configurationArgs ++
  [ "--node-timeout 30 " ++ batchCmdNewline
  , unwords $ map (\x ->  batchCmdNewline ++ "-n " ++ x) nodeArgs
  ]
    where
      version = "1.0"
      updaterLArgs =
          case updater of
            SelfUnpacking {..} ->
              [ "--updater", quote updArchivePath
              , unwords $ map ("-u " ++) updArgs
              ]
            WithUpdater {..} ->
              [ "--updater", quote updExec
              , unwords $ map ("-u " ++) updArgs
              , "--update-archive ", quote updArchivePath
              ]
      configurationArgs | os == "mingw32" =
                          [ "--configuration-file", quote "%DAEDALUS_DIR%\\configuration.yaml"
                          , "--configuration-key",  quote "mainnet_wallet_win64"
                          ]
                        | otherwise =
                          [ "--configuration-file", quote "./configuration.yaml"
                          , "--configuration-key",  quote "mainnet_wallet_macos64"
                         ]
      nodeDbPath = runtimePath <> "DB-" <> version
      nodeArgs = [
        "--report-server", "http://report-server.cardano-mainnet.iohk.io:8080",
        "--log-config", "log-config-prod.yaml",
        "--update-latest-path", quote (updArchivePath updater),
        "--keyfile", quote (runtimePath <> "Secrets-" <> version <> (pathSeparator : "secret.key")),
        "--logs-prefix", quote (runtimePath <> "Logs"),
        "--db-path", quote nodeDbPath,
        "--wallet-db-path", quote (runtimePath <> "Wallet-" <> version),
        "--update-server", "http://update.cardano-mainnet.iohk.io",
        "--update-with-package",
        "--no-ntp",
        "--tlscert", quote (tlsBase <> "server" <> (pathSeparator : "server.crt")),
        "--tlskey",  quote (tlsBase <> "server" <> (pathSeparator : "server.key")),
        "--tlsca",   quote (tlsBase <> "ca"     <> (pathSeparator : "ca.crt"))
        ] <> configFiles
      -- NOTE: looks like windows *.bat file is cut of on 1024 characters per line. This is a workaround
      batchCmdNewline | os == "mingw32" = "^\r\n"
                      | otherwise = mempty
      configFiles     | os == "mingw32" =
                        [ "--topology",           quote "%DAEDALUS_DIR%\\wallet-topology.yaml"
                        ]
                      | otherwise =
                        [ "--topology",           quote "./wallet-topology.yaml"
                        ]
      tlsBase         | os == "mingw32" = "%DAEDALUS_DIR%\\"   <> "tls" <> (pathSeparator : [])
                      | otherwise       = "./"                 <> "tls" <> (pathSeparator : [])

quote :: String -> String
quote p = "\"" <> p <> "\""
