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
  , SigningResult(..)

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
import           Filesystem.Path
import           Filesystem.Path.CurrentOS           (fromText, encodeString)
import           Turtle                              (pwd, cd)
import           Turtle.Format                       (format, fp)
import           Data.Aeson                          (FromJSON(..), withObject, eitherDecode, (.:), genericParseJSON, defaultOptions)
import qualified Data.ByteString.Lazy.Char8       as L8
import qualified System.Info

data OS
  = Linux64
  | Macos64
  | Win64
  deriving (Bounded, Enum, Eq, Read, Show)

data Cluster
  = Selfnode
  | Mainnet
  | Mainnet_Flight
  | Staging
  | Shelley_QA
  | Testnet
  | Alonzo_Purple
  | Vasil_Dev
  | Preprod
  | Preview
  deriving (Bounded, Enum, Eq, Read, Show)

-- | The wallet backend to include in the installer.
--
data Backend
  = Cardano FilePath -- ^ Cardano SL with the given daedalus-bridge.
  deriving (Eq, Show)

data SigningResult
  = SignedOK
  | NotSigned
  deriving (Bounded, Enum, Eq, Read, Show)

data Config
  = Launcher
  deriving (Bounded, Enum, Eq, Show)

configFilename :: Config -> FilePath
configFilename Launcher = "launcher-config.yaml"

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
-- See also: the cluster argument in default.nix.
clusterNetwork :: Cluster -> Text
clusterNetwork Selfnode = "selfnode"
clusterNetwork Mainnet = "mainnet"
clusterNetwork Mainnet_Flight = "mainnet_flight"
clusterNetwork Staging = "staging"
clusterNetwork Shelley_QA = "shelley_qa"
clusterNetwork Testnet = "testnet"
clusterNetwork Alonzo_Purple = "alonzo_purple"
clusterNetwork Vasil_Dev = "vasil_dev"
clusterNetwork Preprod = "preprod"
clusterNetwork Preview = "preview"

packageFileName :: OS -> Cluster -> Version -> Backend -> Text -> Maybe BuildJob -> FilePath
packageFileName _os cluster ver backend _backendVer build = fromText name <.> ext
  where
    name = T.intercalate "-" parts
    parts = ["daedalus", fromVer ver, lshowText cluster] ++ build' ++ [archOS]
    _backend' = case backend of
                 Cardano _ -> "cardano-wallet"
    ext = case _os of
            Win64   -> "exe"
            Macos64 -> "pkg"
            Linux64 -> "bin"
    _os' = case _os of
            Win64   -> "windows"
            Macos64 -> "macos"
            Linux64 -> "linux"
    archOS = case _os of
            Win64   -> "x86_64-windows"
            Macos64 ->
              if System.Info.arch == "aarch64"
              then "aarch64-darwin"
              else "x86_64-darwin"
            Linux64 -> "x86_64-linux"
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
    , spacedName :: Text
    , macPackageName :: Text
    , dataDir :: Text
    , configPath :: Maybe Text
    } deriving (Generic, Show)

instance FromJSON InstallerConfig where
  parseJSON = genericParseJSON defaultOptions
