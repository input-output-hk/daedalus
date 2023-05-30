{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
import           Data.Text                           (pack)
import           Universum
import qualified System.Info                      as Sys
import           Turtle                              (export)
import qualified System.IO as IO

import qualified MacInstaller
import qualified WindowsInstaller
import           Data.Yaml                 (decodeFileThrow)

import           Types
import           Config

main :: IO ()
main = do
  IO.hSetEncoding IO.stderr IO.utf8
  IO.hSetEncoding IO.stdout IO.utf8
  let os = case Sys.os of
             "linux"   -> Linux64
             "darwin"  -> Macos64
             "mingw32" -> Win64
             _         -> error ("Unsupported OS: " <> pack Sys.os)

  (options', command) <- Config.options "Daedalus installer generator" $
    (,) <$> optionsParser os <*> commandParser

  case command of
    GenInstaller -> do
        genSignedInstaller (oOS options') options'
    BuildkiteCrossWin -> do
      fullVersion <- getAppVersion "../package.json"
      installerConfig <- decodeFileThrow "installer-config.json"
      let fullName = packageFileName (uglyName installerConfig) Win64 (oCluster options') fullVersion (oBackend options') (oBuildJob options') (oBuildCounter options')
      WindowsInstaller.writeInstallerNSIS fullName fullVersion installerConfig options' (oCluster options')
      WindowsInstaller.writeUninstallerNSIS fullVersion installerConfig
-- | The contract of `genSignedInstaller` is not to produce unsigned installer binaries.
genSignedInstaller :: OS -> Options -> IO ()
genSignedInstaller os options'= do
    putStrLn $ "Generating installer for " <>  Sys.os <> "-" <> Sys.arch
    export "NETWORK" (clusterNetwork $ oCluster options')
    case os of
        Linux64 -> putStrLn ("Use default.nix, please." :: String)
        Macos64 -> MacInstaller.main options'
        Win64   -> WindowsInstaller.main options'
