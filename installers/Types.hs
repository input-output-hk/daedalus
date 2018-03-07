{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
where

import           Data.Text                           (Text, pack, toLower)
import           Data.String                         (IsString)
import           Prelude



data API
  = Cardano
  | ETC
  deriving (Bounded, Enum, Eq, Read, Show)

data OS
  = Linux
  | Macos64
  | Win64
  deriving (Eq, Show)

data Cluster
  = Mainnet
  | Staging
  deriving (Bounded, Enum, Eq, Read, Show)

data Config
  = Launcher
  | Topology
  deriving (Eq, Show)

data CI
  = Appveyor
  | Travis
  | Buildkite
  | Manual
  deriving (Bounded, Enum, Eq, Read, Show)

data Request
  = Request
  { rOS      :: OS
  , rCluster :: Cluster
  , rConfig  :: Config
  } deriving (Eq, Show)

class (Bounded a, Eq a) => Flag a where
  toBool :: a -> Bool
  toBool = (== enabled)
  fromBool :: Bool -> a
  fromBool x = if x then minBound else maxBound
  enabled, disabled :: a
  enabled  = minBound
  disabled = maxBound
  opposite :: a -> a
  opposite = fromBool . not . toBool

lowerShowT :: Show a => a -> Text
lowerShowT = toLower . pack . show



data TestInstaller   = TestInstaller   | NoInstallerTest   deriving (Bounded, Eq, Ord, Show); instance Flag TestInstaller

newtype BuildJob     = BuildJob     { fromBuildJob     :: Text } deriving (Eq, IsString, Show)
newtype PullReq      = PullReq      { fromPullReq      :: Text } deriving (Eq, IsString, Show)
newtype Version      = Version      { fromVer          :: Text } deriving (Eq, IsString, Show)
