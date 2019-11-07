{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Util where

import Control.Monad (mapM_)
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Directory (listDirectory, withCurrentDirectory, removeDirectory, removeFile, doesDirectoryExist)
import Turtle (export)
import Data.Aeson (Value, decodeStrict', FromJSON, Value(Object, String), ToJSON, encode)
import qualified Data.HashMap.Strict as HM

import Config (Options(..), Backend(..))
import Types (fromBuildJob, clusterNetwork)

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
-- "yarn package" build.
-- When updating this, check that all variables are baked in with both
-- webpack.config.js files.
exportBuildVars :: Options -> Text -> IO ()
exportBuildVars Options{oBackend, oBuildJob, oCluster} backendVersion = do
    mapM_ (uncurry export)
        [ ("API", apiName oBackend)
        , ("API_VERSION", backendVersion)
        , ("BUILD_NUMBER", maybe "" fromBuildJob oBuildJob)
        , ("NETWORK", clusterNetwork oCluster)
        ]
    where
        apiName (Cardano _) = "ada"
        apiName Mantis      = "etc"

decodeFileStrict' :: FromJSON a => FilePath -> IO (Maybe a)
decodeFileStrict' = fmap decodeStrict' . BS.readFile

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile fp = BSL.writeFile fp . encode

rewritePackageJson :: FilePath -> Text -> IO ()
rewritePackageJson path name = do
  rawObject <- (decodeFileStrict' path :: IO (Maybe Value))
  newObject <- case rawObject of
    Just (Object hashmap) -> do
      pure $ Object $ HM.insert "productName" (String name) hashmap
    _ -> error "invalid package.json detected"
  encodeFile path newObject
