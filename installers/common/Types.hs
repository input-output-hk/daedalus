{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  (  -- * Atomic types
    OS(..)
  , Cluster(..)
  , Backend(..)
  , Config(..), configFilename
  , ConfigRequest(..)

  , AppName(..)
  , BuildJob(..)
  , Version(..)

  -- * Flags
  , TestInstaller(..), testInstaller

  -- * Misc
  , lshowText
  , tt
  , packageFileName
  , getDaedalusVersion
  , packageVersion
  , clusterNetwork
  , withDir
  , InstallerConfig(..)
  )
where

import           Universum                    hiding (FilePath)
import qualified Data.Text                        as T
import           Data.String                         (IsString)
import           Filesystem.Path
import           Filesystem.Path.CurrentOS           (fromText, encodeString)
import           Turtle                              (pwd, cd)
import           Turtle.Format                       (format, fp)
import           Data.Aeson                          (FromJSON(..), withObject, eitherDecode, (.:))
import qualified Data.ByteString.Lazy.Char8       as L8
import qualified Dhall as Dhall



data OS
  = Linux64
  | Macos64
  | Win64
  deriving (Bounded, Enum, Eq, Read, Show)

data Cluster
  = Mainnet
  | Staging
  | Testnet
  deriving (Bounded, Enum, Eq, Read, Show)

-- | The wallet backend to include in the installer.
data Backend
  = Cardano FilePath -- ^ Cardano SL with the given daedalus-bridge.
  | Mantis           -- ^ Mantis, to be implemented in DEVOPS-533.
  deriving (Eq, Show)

data Config
  = Launcher
  | Topology
  deriving (Bounded, Enum, Eq, Show)

configFilename :: Config -> FilePath
configFilename Launcher = "launcher-config.yaml"
configFilename Topology = "wallet-topology.yaml"

-- | What runtime config file to generate.
data ConfigRequest = ConfigRequest
  { rOS      :: OS
  , rCluster :: Cluster
  , rConfig  :: Config
  } deriving (Eq, Show)

newtype AppName      = AppName      { fromAppName      :: Text } deriving (Eq, IsString, Show)
newtype BuildJob     = BuildJob     { fromBuildJob     :: Text } deriving (Eq, IsString, Show)
newtype Version      = Version      { fromVer          :: Text } deriving (Eq, IsString, Show)



data TestInstaller      = DontTestInstaller         | TestInstaller          deriving (Eq, Show)
testInstaller    True   =                             TestInstaller
testInstaller    False  = DontTestInstaller



lshowText :: Show a => a -> Text
lshowText = T.toLower . Universum.show

tt :: FilePath -> Text
tt = format fp



-- | Value of the NETWORK variable used by the npm build.
-- See also: the networkMap variable in yarn2nix.nix.
clusterNetwork :: Cluster -> Text
clusterNetwork Mainnet = "mainnet"
clusterNetwork Staging = "testnet"
clusterNetwork Testnet = "testnet"

packageFileName :: OS -> Cluster -> Version -> Backend -> Text -> Maybe BuildJob -> FilePath
packageFileName os cluster ver backend backendVer build = fromText name <.> ext
  where
    name = T.intercalate "-" parts
    parts = ["daedalus", fromVer ver, backend', backendVer, lshowText cluster, os'] ++ build'
    backend' = case backend of
                 Cardano _ -> "cardano-sl"
                 Mantis    -> "mantis"
    ext = case os of
            Win64   -> "exe"
            Macos64 -> "pkg"
            Linux64 -> "bin"
    os' = case os of
            Win64   -> "windows"
            Macos64 -> "macos"
            Linux64 -> "linux"
    build' = maybe [] (\b -> [fromBuildJob b]) build

instance FromJSON Version where
  parseJSON = withObject "Package" $ \o -> Version <$> o .: "version"

getDaedalusVersion :: FilePath -> IO Version
getDaedalusVersion packageJSON = L8.readFile (encodeString packageJSON) >>= packageVersion

packageVersion :: L8.ByteString -> IO Version
packageVersion json = case eitherDecode json of
  Right ver -> pure ver
  Left err -> fail err

-- | More or less the same as pushd function in newer Turtle
withDir :: FilePath -> IO a -> IO a
withDir path = bracket (pwd >>= \old -> (cd path >> pure old)) cd . const

data InstallerConfig = InstallerConfig {
      installDirectory :: Text
    , macPackageName :: Text
    } deriving (Generic, Show)

instance Dhall.Interpret InstallerConfig
