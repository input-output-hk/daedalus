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
import           System.Environment (getEnv)
import           Data.List.Split (splitOn)
import           Data.Maybe                          (fromJust)
import           System.Directory
import qualified Data.Text.IO as T
import qualified Data.Text as T

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
    GenConfig{..}    ->
      generateOSClusterConfigs cfDhallRoot cfOutdir options'
    CheckConfigs{..} ->
      checkAllConfigs          cfDhallRoot
    GenInstaller -> do
        genSignedInstaller (oOS options') options'
    BuildkiteCrossWin -> do
      fullVersion <- getDaedalusVersion "../package.json"
      ver <- T.strip <$> T.readFile "version"
      let fullName = packageFileName Win64 (oCluster options') fullVersion (oBackend options') ver (oBuildJob options')
      installerConfig <- getInstallerConfig "./dhall" Win64 (oCluster options')
      WindowsInstaller.writeInstallerNSIS fullName fullVersion installerConfig (oCluster options')
      WindowsInstaller.writeUninstallerNSIS fullVersion installerConfig
      generateOSClusterConfigs "./dhall" "." options'
    Appveyor -> do
        buildNumber <- getEnv "APPVEYOR_BUILD_NUMBER"
        let
            opts' = options' {
                  oBuildJob = Just $ BuildJob $ pack $ buildNumber
                }
            go :: String -> IO ()
            go cluster' = do
                let
                    getAppName Mainnet = "Daedalus"
                    getAppName Staging = "DaedalusStaging"
                    getAppName Testnet = "DaedalusTestnet"
                    getAppName JormungandrTestnet = "DaedalusJormungandrTestnet"
                    cluster = fromJust $ diagReadCaseInsensitive cluster'
                    opts'' = opts' {
                          oCluster = cluster
                        , oAppName = getAppName cluster
                    }
                    banner :: Text
                    banner = "##############################################################################\n" <>
                             "###\n" <>
                             "### Building for cluster " <> pack cluster' <> "\n" <>
                             "###\n" <>
                             "##############################################################################\n"
                putStr banner
                genSignedInstaller os opts''
                copyFile "launcher-config.yaml" ("launcher-config-" <> cluster' <> ".win64.yaml")
                copyFile "wallet-topology.yaml" ("wallet-topology-" <> cluster' <> ".win64.yaml")
        clusters' <- getEnv "CLUSTERS"
        let clusters = splitOn " " clusters'
        print clusters
        mapM_ go clusters

-- | The contract of `genSignedInstaller` is not to produce unsigned installer binaries.
genSignedInstaller :: OS -> Options -> IO ()
genSignedInstaller os options'= do
    putStrLn $ "Generating installer for " <>  Sys.os <> "-" <> Sys.arch
    export "NETWORK" (clusterNetwork $ oCluster options')
    case os of
        Linux64 -> putStrLn ("Use default.nix, please." :: String)
        Macos64 ->     MacInstaller.main options'
        Win64   -> WindowsInstaller.main options'
