import           Universum
import           Turtle

import           Config

main :: IO ()
main = do
  gcl <- options "Daedalus launcher config generator" generateCardanoLauncherParser
  generateOSClusterConfigs gcl
