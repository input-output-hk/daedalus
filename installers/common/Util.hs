{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Util where

import Control.Monad (mapM_)
import Data.Text (Text)
import System.Directory (listDirectory, withCurrentDirectory, removeDirectory, removeFile, doesDirectoryExist)
import Turtle (export, format, d)

import Config (Options(..), Backend(..))
import Types (InstallerConfig(walletPort), fromBuildJob, clusterNetwork)

windowsRemoveDirectoryRecursive :: FilePath -> IO ()
windowsRemoveDirectoryRecursive path = do
    isDir <- doesDirectoryExist path
    if isDir then do
        cont <- listDirectory path
        withCurrentDirectory path $ do
            mapM_ windowsRemoveDirectoryRecursive cont
        removeDirectory path
    else do
        removeFile path

-- | Sets the many environment variables required for the
-- "npm package" build.
-- When updating this, check that all variables are baked in with both
-- webpack.config.js files.
exportBuildVars :: Options -> InstallerConfig -> Text -> IO ()
exportBuildVars Options{oBackend, oBuildJob, oCluster} cfg backendVersion = do
    mapM_ (uncurry export)
        [ ("API", apiName oBackend)
        , ("API_VERSION", backendVersion)
        , ("BUILD_NUMBER", maybe "" fromBuildJob oBuildJob)
        , ("NETWORK", clusterNetwork oCluster)
        , ("WALLET_PORT", format d (walletPort cfg))
        ]
    where
        apiName (Cardano _) = "ada"
        apiName Mantis      = "etc"
