module Util where

import System.Directory (listDirectory, withCurrentDirectory, removeDirectory, removeFile, doesDirectoryExist)

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
