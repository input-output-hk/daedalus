{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Types
  (  -- * Atomic types
    API(..)
  , OS(..)
  , Cluster(..)
  , Config(..)
  , CI(..)
  , Request(..)

  , AppName(..)
  , BuildJob(..)
  , PullReq(..)
  , Version(..)

  -- * Flags
  , TestInstaller(..), testInstaller

  -- * Misc
  , lshowText
  )
where

import           Data.Text                           (Text, toLower)
import           Data.String                         (IsString)
import qualified Universum
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

newtype AppName      = AppName      { fromAppName      :: Text } deriving (Eq, IsString, Show)
newtype BuildJob     = BuildJob     { fromBuildJob     :: Text } deriving (Eq, IsString, Show)
newtype PullReq      = PullReq      { fromPullReq      :: Text } deriving (Eq, IsString, Show)
newtype Version      = Version      { fromVer          :: Text } deriving (Eq, IsString, Show)



data TestInstaller   = DontTestInstaller       | TestInstaller       deriving (Eq, Show)
testInstaller :: Bool -> TestInstaller
testInstaller True  = TestInstaller
testInstaller False = DontTestInstaller



lshowText :: Show a => a -> Text
lshowText = toLower . Universum.show
