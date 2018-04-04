{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Config
  ( checkAllConfigs
  , generateOSClusterConfigs
  , OS(..), Cluster(..), Config(..), Backend(..)
  , optReadLower, argReadLower
  , Options(..), optionsParser
  , Command(..), commandParser
  -- Re-export Turtle:
  , options
  ) where

import qualified Control.Exception                as Ex

import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Optional                       (Optional)
import           Data.Semigroup                      ((<>))
import           Data.Text                           (Text, pack, unpack, intercalate, toLower)
import qualified Data.Yaml                        as YAML

import qualified Dhall                            as Dhall
import qualified Dhall.JSON                       as Dhall

import           Filesystem.Path.CurrentOS           (FilePath, fromText, encodeString, encodeString)
import qualified GHC.IO.Encoding                  as GHC

import qualified System.IO                        as Sys
import qualified System.Exit                      as Sys

import           Turtle                              (optional, (<|>), (</>), format, (%), s)
import           Turtle.Options

import           Prelude                      hiding (FilePath, unlines, writeFile)
import           Types
import           Debug.Trace



-- | Enum-instanced sum types as case-insensitive option values.
--
-- λ> data Foo = Bar | Baz deriving (Show, Enum, Bounded)
-- λ> let x = enumFromTo minBound maxBound :: [Foo]
-- λ> x
-- [Bar,Baz]
-- λ> fmap ((fmap toLower) . show) x
-- ["bar","baz"]
diagReadCaseInsensitive :: (Bounded a, Enum a, Read a, Show a) => String -> Maybe a
diagReadCaseInsensitive str = diagRead $ toLower $ pack str
  where mapping    = Map.fromList [ (lshowText x, x) | x <- enumFromTo minBound maxBound ]
        diagRead x = Just $ flip fromMaybe (Map.lookup x mapping)
                     (errorT $ format ("Couldn't parse '"%s%"' as one of: "%s)
                               (pack str) (intercalate ", " $ Map.keys mapping))

optReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (diagReadCaseInsensitive . unpack)
argReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> Optional HelpMessage -> Parser a
argReadLower = arg (diagReadCaseInsensitive . unpack)

data Backend
  = Cardano { cardanoDaedalusBridge :: FilePath }
  | Mantis
  deriving (Eq, Show)

data Command
  = GenConfig
    { cfDhallRoot   :: Text
    , cfOutdir      :: FilePath
    }
  | CheckConfigs
    { cfDhallRoot   :: Text
    }
  | GenInstaller
  deriving (Eq, Show)

data Options = Options
  { oBackend        :: Backend
  , oBuildJob       :: Maybe BuildJob
  , oOS             :: OS
  , oCluster        :: Cluster
  , oAppName        :: AppName
  , oDaedalusVer    :: Version
  , oOutput         :: FilePath
  , oPullReq        :: Maybe PullReq
  , oTestInstaller  :: TestInstaller
  , oCI             :: CI
  } deriving Show

commandParser :: Parser Command
commandParser = (fromMaybe GenInstaller <$>) . optional $
  subcommandGroup "Subcommands:"
  [ ("config",        "Build configs for an OS / cluster  (see: --os, --cluster top-level options)",
      GenConfig
      <$> argText "INDIR"  "Directory containing Dhall config files"
      <*> (fromText <$> argText "OUTDIR" "Target directory for generated YAML config files"))
  , ("check-configs", "Verify all Dhall-defined config components",
      CheckConfigs
      <$> argText "DIR" "Directory containing Dhall config files")
  , ("installer",  "Build an installer",
      pure GenInstaller)
  ]

optionsParser :: OS -> Parser Options
optionsParser detectedOS = Options
  <$> backendOptionParser
  <*> (optional      $
      (BuildJob     <$> optText "build-job"           'b' "CI Build Job/ID"))
  <*> (fromMaybe detectedOS <$> (optional $
                   optReadLower "os"                  's' "OS, defaults to host OS.  One of:  linux macos64 win64"))
  <*> (fromMaybe Mainnet    <$> (optional $
                   optReadLower "cluster"             'c' "Cluster the resulting installer will target:  mainnet or staging"))
  <*> (fromMaybe "daedalus" <$> (optional $
      (AppName      <$> optText "appname"             'n' "Application name:  daedalus or..")))
  <*> (fromMaybe "dev"   <$> (optional $
      (Version      <$> optText "daedalus-version"    'v' "Daedalus version string")))
  <*> (fromMaybe (error "--output not specified for 'installer' subcommand") . (fromText <$>)
       <$> (optional $  optText "output"              'o' "Installer output file"))
  <*> (optional   $
      (PullReq      <$> optText "pull-request"        'r' "Pull request #"))
  <*> (testInstaller
                    <$> switch  "test-installer"      't' "Test installers after building")
  <*> pure Buildkite -- NOTE: this is filled in by auto-detection

backendOptionParser :: Parser Backend
backendOptionParser = cardano <|> mantis <|> pure (Cardano "")
  where
    cardano = Cardano <$> optPath "cardano" 'C'
      "Use Cardano backend with given Daedalus bridge path"
    mantis = switch "mantis" 'M' "Use Mantis (ETC) backend" *> pure Mantis



dhallTopExpr :: Text -> Config -> OS -> Cluster -> Text
dhallTopExpr dhallRoot cfg os cluster
  | Launcher <- cfg = format (s%" "%s%" ("%s%" "%s%" )") (comp Launcher) (comp cluster) (comp os) (comp cluster)
  | Topology <- cfg = format (s%" "%s)                   (comp Topology) (comp cluster)
  where comp x = dhallRoot <>"/"<> lshowText x <>".dhall"

forConfigValues :: Text -> OS -> Cluster -> (Config -> YAML.Value -> IO a) -> IO ()
forConfigValues dhallRoot os cluster action = do
  sequence_ [ let topExpr = dhallTopExpr dhallRoot cfg os cluster
              in action cfg =<<
                 (handle $ Dhall.detailed $ Dhall.codeToValue "(stdin)" $
                  (trace (unpack $ "Dhall top-level expression: " <> topExpr) topExpr))
            | cfg     <- enumFromTo minBound maxBound ]

checkAllConfigs :: Text -> IO ()
checkAllConfigs dhallRoot =
  sequence_ [ forConfigValues dhallRoot os cluster (\_ _ -> pure ())
            | os      <- enumFromTo minBound maxBound
            , cluster <- enumFromTo minBound maxBound ]

generateOSClusterConfigs :: Text -> FilePath -> Options -> IO ()
generateOSClusterConfigs dhallRoot outDir Options{..} = forConfigValues dhallRoot oOS oCluster $
  \config val -> do
    GHC.setLocaleEncoding GHC.utf8
    BS.writeFile (encodeString $ outDir </> configFilename config) $ YAML.encode val

-- | Generic error handler: be it encoding/decoding, file IO, parsing or type-checking.
handle :: IO a -> IO a
handle = Ex.handle handler
  where
    handler :: Ex.SomeException -> IO a
    handler e = do
        Sys.hPutStrLn Sys.stderr ""
        Sys.hPrint    Sys.stderr e
        Sys.exitFailure
