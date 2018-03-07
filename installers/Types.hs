module Types
where

import           Data.Text                           (Text)
import           Prelude



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



data TestInstaller   = TestInstaller   | NoInstallerTest   deriving (Bounded, Eq, Ord, Show); instance Flag TestInstaller

newtype Version      = Version { fromVersion     :: Text } deriving (Eq, Show)
