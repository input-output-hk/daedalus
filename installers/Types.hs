{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Types
  (  -- * Atomic types
    OS(..)
  , Cluster(..)
  , Config(..), configFilename
  , CI(..)

  , AppName(..)
  , BuildJob(..)
  , PullReq(..)
  , Version(..)

  -- * Flags
  , TestInstaller(..), testInstaller

  -- * Misc
  , lshowText
  , errorT
  )
where

import           Data.Text                           (Text, toLower, unpack)
import           Data.String                         (IsString)
import qualified Universum

import           Filesystem.Path.CurrentOS           (FilePath)
import           Prelude                      hiding (FilePath)



data OS
  = Linux
  | Macos64
  | Win64
  deriving (Bounded, Enum, Eq, Read, Show)

data Cluster
  = Mainnet
  | Staging
  deriving (Bounded, Enum, Eq, Read, Show)

data Config
  = Launcher
  | Topology
  deriving (Bounded, Enum, Eq, Show)

configFilename :: Config -> FilePath
configFilename Launcher = "launcher-config.yaml"
configFilename Topology = "wallet-topology.yaml"

data CI
  = Appveyor
  | Travis
  | Buildkite
  | Manual
  deriving (Bounded, Enum, Eq, Read, Show)

newtype AppName      = AppName      { fromAppName      :: Text } deriving (Eq, IsString, Show)
newtype BuildJob     = BuildJob     { fromBuildJob     :: Text } deriving (Eq, IsString, Show)
newtype PullReq      = PullReq      { fromPullReq      :: Text } deriving (Eq, IsString, Show)
newtype Version      = Version      { fromVer          :: Text } deriving (Eq, IsString, Show)



data TestInstaller      = DontTestInstaller         | TestInstaller          deriving (Eq, Show)
testInstaller    True   =                             TestInstaller
testInstaller    False  = DontTestInstaller



lshowText :: Show a => a -> Text
lshowText = toLower . Universum.show

errorT :: Text -> a
errorT = error . unpack
