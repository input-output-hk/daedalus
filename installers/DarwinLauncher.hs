{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getExecutablePath)
import Turtle (FilePath, Text, parent, decodeString, encodeString, (</>), procs, format, fp)
import System.Posix.Process (executeFile)

main :: IO ()
main = do
  self <- getExecutablePath
  let
    installDir = parent (decodeString self)
    launcherConfig :: String
    launcherConfig = encodeString $ installDir </> "../Resources/launcher-config.yaml"
    launcher = installDir </> "cardano-launcher"
  procs (tt $ installDir </> "../Resources/helper") [] mempty
  executeFile (encodeString launcher) False [ "--config", launcherConfig ] Nothing

tt :: Turtle.FilePath -> Text
tt = format fp
