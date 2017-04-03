#!/usr/bin/env nix-shell
#! nix-shell -j 4 -i runhaskell -p 'pkgs.haskellPackages.ghcWithPackages (hp: with hp; [ turtle megaparsec text directory ])'

{-# LANGUAGE OverloadedStrings   #-}

module RewriteLibs where

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text
import System.Directory (copyFile, setPermissions, getPermissions, setOwnerWritable)
import Text.Megaparsec
import Text.Megaparsec.Text
import Turtle (procStrict, procs)


{- Given a binary file on MacOS, rewrite some of the dynamic libraries to relative paths.

1. Crawl through the tree of libraries
2. filter only for libraries not core to MacOS
3. copy the library to global relative folder
4. set the new library path for all references
5. test that --help executes on the binary
-}

systemLibs :: [Text]
systemLibs = ["libSystem.B.dylib"]

main :: IO ()
main = do
  --print "------- cardano-launcher"
  --files <- chain "./temp" ["./temp/cardano-launcher"]
  --print "------- cardano-node"
  --files <- chain "./temp" ["./temp/cardano-node"]
  return ()

-- dir: final path of the files
-- args: libraries to process
-- returns processed libraries
chain :: FilePath -> [Text] -> IO [Text]
chain dir args@(x:xs) = do
  (_, output) <- procStrict "otool" ["-L", x] mempty
  case (parse parseOTool (unpack x) output) of
    Left err -> do
      print err
      return []
    Right files -> do
      -- parse again all libraries pointing to nix store that we haven't processed yet
      let libs = Prelude.filter (isPrefixOf "/nix/store/") files
      filtered <- traverse (patchLib x dir) libs
      chained <- chain dir (xs ++ (Prelude.filter (\f -> notElem f args) $ catMaybes filtered))
      return $ x : chained
chain _ [] = return []


patchLib :: Text -> FilePath -> Text -> IO (Maybe Text)
patchLib source dir lib
    | (Prelude.filter (\pattern -> isSuffixOf pattern lib) systemLibs) /= mempty = do
        -- if it's a system lib, just point to correct folder and be done
        print $ "Patching " <> lib <> " as system in " <> source
        procs "install_name_tool" ["-change", lib, "/usr/lib/" <> (filename lib), (pack dir) <> "/" <> (filename source)] mempty
        return Nothing
    | otherwise = do
        -- otherwise, copy it to dist and change where it points
        print $ "Bundling " <> lib <> " in " <> source
        -- substitute store path if they are missing
        procs "nix-store" ["-r", lib] mempty
        procs "install_name_tool" ["-change", lib, "@executable_path/" <> (filename lib), (pack dir) <> "/" <> (filename source)] mempty
        let dest = dir <> "/" <> (unpack $ filename lib)
        copyFile (unpack lib) dest
        permissions <- getPermissions dest
        setPermissions dest $ setOwnerWritable True permissions
        return $ Just lib

filename :: Text -> Text
filename path = Prelude.last $ splitOn "/" path

-- otool parser

parseLibLine :: Parser Text
parseLibLine = do
  _ <- many spaceChar
  path <- someTill anyChar spaceChar
  _ <- someTill anyChar eol
  return (pack path)


parseOTool :: Parser [Text]
parseOTool = do
  _ <- manyTill anyChar eol
  manyTill parseLibLine eof
