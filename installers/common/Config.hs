{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Config
  ( OS(..), Cluster(..), Config(..), Backend(..)
  , optReadLower, argReadLower
  , Options(..), optionsParser
  , Command(..), commandParser
  , dfp
  -- Re-export Turtle:
  , options
  , diagReadCaseInsensitive
  ) where

import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Optional                       (Optional)
import qualified Data.Text                        as T

import           Filesystem.Path                     (FilePath)
import qualified Filesystem.Path.Rules            as FP

import           Turtle                              (format, (%), s, Format, makeFormat)
import           Turtle.Options

import           Universum                    hiding (FilePath, unlines, writeFile)
import           Types

-- | Enum-instanced sum types as case-insensitive option values.
--
-- λ> data Foo = Bar | Baz deriving (Show, Enum, Bounded)
-- λ> let x = enumFromTo minBound maxBound :: [Foo]
-- λ> x
-- [Bar,Baz]
-- λ> fmap ((fmap toLower) . show) x
-- ["bar","baz"]
diagReadCaseInsensitive :: (Bounded a, Enum a, Read a, Show a) => String -> Maybe a
diagReadCaseInsensitive str = diagRead $ T.toLower $ T.pack str
  where mapping    = Map.fromList [ (lshowText x, x) | x <- enumFromTo minBound maxBound ]
        diagRead x = Just $ flip fromMaybe (Map.lookup x mapping)
                     (error $ format ("Couldn't parse '"%s%"' as one of: "%s)
                              (T.pack str) (T.intercalate ", " $ Map.keys mapping))

optReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (diagReadCaseInsensitive . T.unpack)
argReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> Optional HelpMessage -> Parser a
argReadLower = arg (diagReadCaseInsensitive . T.unpack)

data Command
  =  GenInstaller
  | BuildkiteCrossWin
  deriving (Eq, Show)

data Options = Options
  { oBackend               :: Backend
  , oBuildJob              :: Maybe BuildJob
  , oBuildCounter         :: Maybe BuildJob
  , oOS                    :: OS
  , oCluster               :: Cluster
  , oAppName               :: AppName
  , oAppRootOverride       :: Maybe FilePath
  , oDontPkgbuild          :: Bool
  , oOutputDir             :: FilePath
  , oTestInstaller         :: TestInstaller
  , oCodeSigningConfigPath :: Maybe FilePath
  , oSigningConfigPath     :: Maybe FilePath
  } deriving Show

commandParser :: Parser Command
commandParser = (fromMaybe GenInstaller <$>) . optional $
  subcommandGroup "Subcommands:"
  [ ("installer",  "Build an installer",
      pure GenInstaller)
  , ("buildkite-cross", "cross-compile windows from linux", pure BuildkiteCrossWin)
  ]

optionsParser :: OS -> Parser Options
optionsParser detectedOS = Options
  <$> backendOptionParser
  <*> (optional      $
      (BuildJob     <$> optText "build-rev-short"     'b' "CI Build Job/ID"))
  <*> (optional      $
      (BuildJob     <$> optText "build-counter"     'v' "‘inputs.self.sourceInfo.revCount’"))
  <*> (fromMaybe detectedOS <$> (optional $
                   optReadLower "os"                  's' "OS, defaults to host OS.  One of:  linux64 macos64 win64"))
  <*> (fromMaybe Selfnode   <$> (optional $
                   optReadLower "cluster"             'c' "Cluster the resulting installer will target:  mainnet, staging, or testnet"))
  <*> (fromMaybe "daedalus" <$> (optional $
      (AppName      <$> optText "appname"             'n' "Application name:  daedalus or..")))
  <*> (optional (optPath        "app-root-override"   'r' "If you built the Electron app outside of MacInstaller.hs"))
  <*> (switch                   "dont-pkgbuild"       'd' "Stop after preparing the package (content root), don’t create the final *.pkg file")
  <*>                   optPath "out-dir"             'o' "Installer output directory"
  <*> (testInstaller
                    <$> switch  "test-installer"      't' "Test installers after building")
  <*> (optional $ optPath       "code-signing-config" 's' "the path to the json file describing the code signing config")
  <*> (optional $ optPath       "signing-config"      'k' "the path to the json file describing the product signing config")

backendOptionParser :: Parser Backend
backendOptionParser = cardano
  where
    cardano = Cardano <$> optPath "cardano" 'S' "Use Cardano backend with given Daedalus bridge path"

-- | Render a FilePath with POSIX-style forward slashes, which is the
dfp :: Format r (FilePath -> r)
dfp = makeFormat (\fpath -> either id id (FP.toText FP.posix fpath))
