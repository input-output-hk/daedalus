{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Launcher
    ( Updater (..)
    , Launcher (..)
    , getLauncherConfig
    , writeLauncherConfig
    ) where

import           Universum

import           Data.Aeson (ToJSON (toJSON), object, (.=))
import           Data.Yaml (encode, encodeFile)
import           System.FilePath (pathSeparator)
import           System.Info (os)

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

instance ToJSON Launcher where
    toJSON Launcher {..} = object
        [ "nodePath" .= nodePath
        , "nodeArgs" .= nodeArgs
        , "nodeDbPath" .= nodeDbPath
        , "nodeLogConfig" .= ("log-config-prod.yaml" :: Text)
        , "nodeLogPath" .= nodeLogPath
        , "walletPath" .= walletPath
        , "walletArgs" .= ([] :: [Text])
        , "updaterPath" .= updaterPath
        , "updaterArgs" .= updaterArgs
        , "updateArchive" .= updateArchive
        , "updateWindowsRunner" .= windowsInstallerPath
        , "nodeTimeoutSec" .= (30 :: Integer)
        , "reportServer" .= ("http://report-server.cardano-mainnet.iohk.io:8080" :: Text)
        , "launcherLogsPrefix" .= launcherLogPath
        , "configuration" .= object
            [ "filePath" .= configurationPath
            , "key" .= configurationKey
            -- Actually @systemStart@ is @Maybe Timestamp@ from @Pos.Core.Slotting@,
            -- but we don't want to add a dependency on CSL just for that.
            , "systemStart" .= (Nothing :: Maybe Integer)
            , "seed" .= (Nothing :: Maybe Integer)
            ]
        ]
      where
        version = "1.0"
        nodeArgs :: [String]
        nodeArgs =
            [ "--update-latest-path", updArchivePath updater
            , "--keyfile", runtimePath <> "Secrets-" <> version <> (pathSeparator : "secret.key")
            , "--logs-prefix", runtimePath <> "Logs"
            , "--wallet-db-path", runtimePath <> "Wallet-" <> version
            , "--update-server", "http://update.cardano-mainnet.iohk.io"
            , "--update-with-package"
            , "--no-ntp"
            , "--tlscert", tlsBase <> "server" <> (pathSeparator : "server.crt")
            , "--tlskey",  tlsBase <> "server" <> (pathSeparator : "server.key")
            , "--tlsca",   tlsBase <> "ca"     <> (pathSeparator : "ca.crt")
            ] <> configFiles
        (updaterPath, updaterArgs, updateArchive) =
            case updater of
                SelfUnpacking {..} ->
                  ( updArchivePath
                  , updArgs
                  , Nothing
                  )
                WithUpdater {..} ->
                  ( updExec
                  , updArgs
                  , Just updArchivePath
                  )
        configurationPath :: Text
        configurationKey :: Text
        (configurationPath, configurationKey)
            | os == "mingw32" =
                ( "%DAEDALUS_DIR%\\configuration.yaml"
                , "mainnet_wallet_win64"
                )
            | otherwise =
                ( "./configuration.yaml"
                , "mainnet_wallet_macos64"
                )
        nodeDbPath = runtimePath <> "DB-" <> version
        configFiles
            | os == "mingw32" =
                ["--topology", "%DAEDALUS_DIR%\\wallet-topology.yaml"]
            | otherwise =
                ["--topology", "./wallet-topology.yaml"]
        tlsBase
            | os == "mingw32" =
                "%DAEDALUS_DIR%\\tls" <> [pathSeparator]
            | otherwise =
                "./tls" <> [pathSeparator]

getLauncherConfig :: Launcher -> ByteString
getLauncherConfig = encode

writeLauncherConfig :: FilePath -> Launcher -> IO ()
writeLauncherConfig = encodeFile
