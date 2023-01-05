{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Util where

import Data.Text (Text)
import System.Directory (listDirectory, withCurrentDirectory, removeDirectory, removeFile, doesDirectoryExist)
import Data.Aeson (Value, Value(Object, String), encodeFile, decodeFileStrict')
import qualified Data.Aeson.KeyMap as HM

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

rewritePackageJson :: FilePath -> Text -> IO ()
rewritePackageJson path name = do
  rawObject <- (decodeFileStrict' path :: IO (Maybe Value))
  newObject <- case rawObject of
    Just (Object hashmap) -> do
      pure $ Object $ HM.insert "productName" (String name) hashmap
    _ -> error "invalid package.json detected"
  encodeFile path newObject
