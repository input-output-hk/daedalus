#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle megaparsec text directory universum ])'

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RewriteLibs
    ( chain
    ) where

import           Universum hiding (isPrefixOf)

import           Data.List (last)
import           Data.Text (isPrefixOf, isSuffixOf, splitOn)
import           System.Directory (copyFile, getPermissions, setOwnerWritable, setPermissions)
import           Text.Megaparsec (anyChar, eof, eol, manyTill, parse, someTill, spaceChar)
import           Text.Megaparsec.Text (Parser)
import           Turtle (procStrict, procs)


{- Given a binary file on MacOS, rewrite some of the dynamic libraries to relative paths.

1. Crawl through the tree of libraries
2. filter only for libraries not core to MacOS
3. copy the library to global relative folder
4. set the new library path for all references
5. test that --help executes on the binary
-}

systemLibs :: [Text]
systemLibs = ["libSystem.B.dylib"]

-- dir: final path of the files
-- args: libraries to process
-- returns processed libraries
chain :: FilePath -> [Text] -> IO [Text]
chain dir args@(x:xs) = do
    (_, output) <- procStrict "otool" ["-L", x] mempty
    case (parse parseOTool (toString x) output) of
        Left err -> do
            print err
            return []
        Right files -> do
            -- parse again all libraries pointing to nix store that we haven't processed yet
            let libs = filter (isPrefixOf "/nix/store/") files
            filtered <- traverse (patchLib x dir) libs
            chained <- chain dir (xs ++ (filter (\f -> not $ elem f args) $ catMaybes filtered))
            return $ x : chained
chain _ [] = return []


patchLib :: Text -> FilePath -> Text -> IO (Maybe Text)
patchLib source dir lib
    | (filter (\pattern -> isSuffixOf pattern lib) systemLibs) /= mempty = do
        -- if it's a system lib, just point to correct folder and be done
        print $ "Patching " <> lib <> " as system in " <> source
        procs "install_name_tool" ["-change", lib, "/usr/lib/" <> (filename lib), (toText dir) <> "/" <> (filename source)] mempty
        return Nothing
    | otherwise = do
        -- otherwise, copy it to dist and change where it points
        print $ "Bundling " <> lib <> " in " <> source
        -- substitute store path if they are missing
        procs "nix-store" ["-r", lib] mempty
        procs "install_name_tool" ["-change", lib, "@executable_path/" <> (filename lib), (toText dir) <> "/" <> (filename source)] mempty
        let dest = dir <> "/" <> (toString $ filename lib)
        copyFile (toString lib) dest
        permissions <- getPermissions dest
        setPermissions dest $ setOwnerWritable True permissions
        return $ Just lib

filename :: Text -> Text
filename path = last $ splitOn "/" path

-- otool parser

parseLibLine :: Parser Text
parseLibLine = do
    _ <- many spaceChar
    path <- someTill anyChar spaceChar
    _ <- someTill anyChar eol
    return (toText path)


parseOTool :: Parser [Text]
parseOTool = do
    _ <- manyTill anyChar eol
    manyTill parseLibLine eof
