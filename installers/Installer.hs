{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
import           Universum
import           System.Environment                  (lookupEnv)
import           System.Info                         (arch, os)

import qualified MacInstaller                        (main)
import qualified WindowsInstaller                    (main)

import           Types
import qualified Config



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
  options' <- Config.options "Daedalus installer generator" Config.optionsParser
  options  <- (\ci -> options' { Config.oCI = ci }) <$> detectCI

  putStrLn $ "Generating installer for " <>  os <> "-" <> arch
  case os of
    "linux"   -> putStrLn ("No installer yet" :: String)
    "darwin"  ->     MacInstaller.main options
    "mingw32" -> WindowsInstaller.main options
    _         -> fail "No installer available for this platform."
