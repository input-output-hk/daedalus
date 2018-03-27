{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
import           Data.Text
import           Universum
import           System.Environment                  (lookupEnv)
import qualified System.Info                      as Sys

import qualified MacInstaller                        (main)
import qualified WindowsInstaller                    (main)

import           Types
import           Config



detectCI :: IO CI
detectCI = do
  mappveryor <- lookupEnv "APPVEYOR_BUILD_VERSION"
  mbuildkite <- lookupEnv "BUILDKITE_BUILD_NUMBER"
  mtravis    <- lookupEnv "TRAVIS_BUILD_JOB"
  pure $ case (mappveryor, mbuildkite, mtravis) of
           (Nothing, Nothing, Nothing) -> Manual
           (Just _,  Nothing, Nothing) -> Appveyor
           (Nothing, Just _,  Nothing) -> Buildkite
           (Nothing, Nothing, Just _)  -> Travis
           _              -> error "Conflicting CI environments: more than one of APPVEYOR_BUILD_VERSION, BUILDKITE_BUILD_NUMBER and TRAVIS_BUILD_JOB set!"

main :: IO ()
main = do
  let os = case Sys.os of
             "linux"   -> Linux
             "darwin"  -> Macos64
             "mingw32" -> Win64
             _         -> error ("Unsupported OS: " <> pack Sys.os)

  (options', command) <- options "Daedalus installer generator" $
    (,) <$> optionsParser os <*> commandParser
  options <- (\ci -> options' { oCI = ci }) <$> detectCI

  case command of
    GenConfig{..}    ->
      generateOSClusterConfigs cfDhallRoot cfOutdir options
    CheckConfigs{..} ->
      checkAllConfigs          cfDhallRoot
    GenInstaller -> do
      putStrLn $ "Generating installer for " <>  Sys.os <> "-" <> Sys.arch
      case os of
        Linux   -> putStrLn ("Use default.nix, please." :: String)
        Macos64 ->     MacInstaller.main options
        Win64   -> WindowsInstaller.main options
