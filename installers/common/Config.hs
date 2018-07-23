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
  , forConfigValues
  , OS(..), Cluster(..), Config(..), Backend(..)
  , optReadLower, argReadLower
  , Options(..), optionsParser
  , Command(..), commandParser
  , dfp
  -- Re-export Turtle:
  , options
  , getInstallerConfig
  , dhallTopExpr
  , diagReadCaseInsensitive
  ) where

import qualified Control.Exception                as Ex

import           Data.Bool                           (bool)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BS8
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Optional                       (Optional)
import           Data.Semigroup                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as LT
import qualified Data.Yaml                        as YAML

import qualified Dhall.JSON                       as Dhall
import qualified Dhall                            as Dhall

import           Filesystem.Path                     (FilePath, (</>))
import           Filesystem.Path.CurrentOS           (fromText, encodeString)
import qualified Filesystem.Path.Rules            as FP
import qualified GHC.IO.Encoding                  as GHC

import qualified System.IO                        as Sys
import qualified System.Exit                      as Sys
import           Turtle                              (optional, (<|>), format, (%), s, Format, makeFormat)
import           Turtle.Options

import           Universum                    hiding (FilePath, unlines, writeFile)
import           GHC.Base                            (id)
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
  = GenConfig
    { cfDhallRoot   :: Text
    , cfOutdir      :: FilePath
    }
  | CheckConfigs
    { cfDhallRoot   :: Text
    }
  | GenInstaller
  | Appveyor
  deriving (Eq, Show)

data Options = Options
  { oBackend        :: Backend
  , oBuildJob       :: Maybe BuildJob
  , oOS             :: OS
  , oCluster        :: Cluster
  , oAppName        :: AppName
  , oOutputDir      :: FilePath
  , oTestInstaller  :: TestInstaller
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
  , ("appveyor",   "do an appveroy build", pure Appveyor)
  ]

optionsParser :: OS -> Parser Options
optionsParser detectedOS = Options
  <$> backendOptionParser
  <*> (optional      $
      (BuildJob     <$> optText "build-job"           'b' "CI Build Job/ID"))
  <*> (fromMaybe detectedOS <$> (optional $
                   optReadLower "os"                  's' "OS, defaults to host OS.  One of:  linux64 macos64 win64"))
  <*> (fromMaybe Mainnet    <$> (optional $
                   optReadLower "cluster"             'c' "Cluster the resulting installer will target:  mainnet, staging, or testnet"))
  <*> (fromMaybe "daedalus" <$> (optional $
      (AppName      <$> optText "appname"             'n' "Application name:  daedalus or..")))
  <*>                   optPath "out-dir"             'o' "Installer output directory"
  <*> (testInstaller
                    <$> switch  "test-installer"      't' "Test installers after building")

backendOptionParser :: Parser Backend
backendOptionParser = cardano <|> bool (Cardano "") Mantis <$> enableMantis
  where
    cardano = Cardano <$> optPath "cardano" 'C'
      "Use Cardano backend with given Daedalus bridge path"
    enableMantis = switch "mantis" 'M' "Use Mantis (ETC) backend"



-- | Render a FilePath with POSIX-style forward slashes, which is the
-- Dhall syntax.
dfp :: Format r (FilePath -> r)
dfp = makeFormat (\fpath -> either id id (FP.toText FP.posix fpath))

dhallTopExpr :: Text -> Config -> OS -> Cluster -> Text
dhallTopExpr dhallRoot cfg os cluster
  | Launcher <- cfg = format (s%" "%s%" ("%s%" "%s%" )") (comp Launcher) (comp cluster) (comp os) (comp cluster)
  | Topology <- cfg = format (s%" "%s)                   (comp Topology) (comp cluster)
  where comp x = dhallRoot <>"/"<> lshowText x <>".dhall"

getInstallerConfig :: Text -> OS -> Cluster -> IO InstallerConfig
getInstallerConfig dhallRoot os cluster = do
    let
        topexpr :: Dhall.Text
        topexpr = LT.fromStrict $ format (s%" ("%s%" "%s%")") (dhallRoot <> "/installer.dhall") (comp os) (comp cluster)
        comp x = dhallRoot <>"/"<> lshowText x <>".dhall"
    Dhall.input Dhall.auto topexpr

forConfigValues :: Text -> OS -> Cluster -> (Config -> YAML.Value -> IO a) -> IO ()
forConfigValues dhallRoot os cluster action = do
  sequence_ [ let topExpr = dhallTopExpr dhallRoot cfg os cluster
              in action cfg =<<
                 (handle $ Dhall.codeToValue (BS8.pack $ T.unpack topExpr) topExpr)
            | cfg     <- enumFromTo minBound maxBound ]

checkAllConfigs :: Text -> IO ()
checkAllConfigs dhallRoot =
  sequence_ [ forConfigValues dhallRoot os cluster (\_ _ -> pure ())
            | os      <- enumFromTo minBound maxBound
            , cluster <- enumFromTo minBound maxBound ]

generateOSClusterConfigs :: Text -> FilePath -> Options -> IO ()
generateOSClusterConfigs dhallRoot outDir Options{..} = do
  GHC.setLocaleEncoding GHC.utf8
  forConfigValues dhallRoot oOS oCluster $
    \config val ->
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
