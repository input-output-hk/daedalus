{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
import           Data.Text                           (pack)
import           Universum
import qualified System.Info                      as Sys
import           Turtle                              (export)

import qualified MacInstaller                        (main)
import qualified WindowsInstaller                    (main)
import           System.Environment (getEnv)
import           Data.List.Split (splitOn)
import           Data.Maybe                          (fromJust)
import           System.Directory

import           Types
import           Config

main :: IO ()
main = do
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
        genInstaller os options'
    Appveyor -> do
        buildNumber <- getEnv "APPVEYOR_BUILD_NUMBER"
        let
            opts' = options' {
                  oBuildJob = Just $ BuildJob $ pack $ buildNumber
                }
            go :: String -> IO ()
            go cluster' = do
                let
                    opts'' = opts' {
                        oCluster = fromJust $ diagReadCaseInsensitive cluster'
                    }
                    banner :: Text
                    banner = "##############################################################################\n" <>
                             "###\n" <>
                             "### Building for cluster " <> pack cluster' <> "\n" <>
                             "###\n" <>
                             "##############################################################################\n"
                putStr banner
                genInstaller os opts''
                copyFile "launcher-config.yaml" ("launcher-config-" <> cluster' <> ".win64.yaml")
                copyFile "wallet-topology.yaml" ("wallet-topology-" <> cluster' <> ".win64.yaml")
        clusters' <- getEnv "CLUSTERS"
        let clusters = splitOn " " clusters'
        print clusters
        mapM_ go clusters

genInstaller :: OS -> Options -> IO ()
genInstaller os options'= do
    putStrLn $ "Generating installer for " <>  Sys.os <> "-" <> Sys.arch
    export "NETWORK" (clusterNetwork $ oCluster options')
    case os of
        Linux64 -> putStrLn ("Use default.nix, please." :: String)
        Macos64 ->     MacInstaller.main options'
        Win64   -> WindowsInstaller.main options'
