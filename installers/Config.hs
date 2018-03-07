{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Config
  ( generateConfig
  , Request(..)
  , OS(..), Cluster(..), Config(..)
  , readClusterName
  , Flag(..), flag
  , optReadLower, argReadLower
  , Options(..), optionsParser
  ) where

import qualified Control.Exception

import qualified Data.Char
import qualified Data.ByteString
import qualified Data.List                        as L
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Optional                       (Optional)
import           Data.Semigroup
import           Data.Text                           (Text, pack, unpack)
import qualified Data.Text.Lazy                   as LT
import qualified Data.Yaml

import qualified Dhall
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

import qualified GHC.IO.Encoding

import           Options.Applicative.Builder         (strOption, long, short, metavar)

import qualified System.IO
import qualified System.Exit

import           Text.Printf                         (printf)
import           Text.Read                           (readMaybe)
import           Text.Trifecta.Delta                 (Delta(..))
import           Turtle                              (optional)
import           Turtle.Options

import           Prelude
import           Types



flag :: Flag a => a -> ArgName -> Char -> Optional HelpMessage -> Parser a
flag effect long ch help = (\case
                               True  -> effect
                               False -> opposite effect) <$> switch long ch help

diagReadCaseInsensitive :: (Bounded a, Enum a, Read a, Show a) => String -> Maybe a
diagReadCaseInsensitive str = diagRead $ Data.Char.toLower <$> str
  where mapping    = Map.fromList [ (Data.Char.toLower <$> show x, x) | x <- enumFromTo minBound maxBound ]
        diagRead x = Just $ flip fromMaybe (Map.lookup x mapping)
                     (error $ printf ("Couldn't parse '%s' as one of: %s")
                                     str (L.intercalate ", " $ Map.keys mapping))

optReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (diagReadCaseInsensitive . unpack)
argReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> Optional HelpMessage -> Parser a
argReadLower = arg (diagReadCaseInsensitive . unpack)

data Options = Options
  { oAPI           :: Text
  , oAppveyorBuild :: Text
  , oCertPass      :: Text
  , oCluster       :: Cluster
  , oDaedalusVer   :: Version
  , oTestInstaller :: TestInstaller
  }

optionsParser :: Parser Options
optionsParser = Options
  <$>              optText "api"                 'a' "Backend API:  cardano or ..."
  <*>              optText "appveyor-build"      'b' "AppVeyor build ID"
  <*>              optText "cert-pass"           'p' "Certificate password"
  <*> (fromMaybe Mainnet <$> (optional $
              optReadLower "cluster"             'c' "Cluster the resulting installer will target:  mainnet or staging"))
  <*> (Version <$> optText "daedalus-version"    'v' "Daedalus version string")
  <*> flag TestInstaller   "test-installer"      't' "Test installers after building"



readClusterName :: String -> Cluster
readClusterName name = fromMaybe (error $ "Unrecognised cluster name "<>name<>": should be one of:  mainnet staging") $
                       readMaybe $ capitalize name
  where capitalize :: String -> String
        capitalize [] = []
        capitalize (x:xs) = [Data.Char.toUpper x] <> xs

lshow :: Show a => a -> Text
lshow = pack . fmap Data.Char.toLower . show

dhallTopExpr :: Text -> Config -> OS -> Cluster -> Text
dhallTopExpr path Launcher os cluster = path <> "/launcher.dhall ( "<>path<>"/" <> lshow cluster <> ".dhall "<>path<>"/" <> lshow os <> ".dhall ) "<>path<>"/" <> lshow os <> ".dhall"
dhallTopExpr path Topology os cluster = path <> "/topology.dhall ( "<>path<>"/" <> lshow cluster <> ".dhall "<>path<>"/" <> lshow os <> ".dhall )"

generateConfig :: Request -> FilePath -> FilePath -> IO ()
generateConfig Request{..} configRoot outFile = handle $ do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

  Dhall.detailed $ do
    let inText = LT.fromStrict $ dhallTopExpr (pack configRoot) rConfig rOS rCluster
    print inText
    expr <- case Dhall.Parser.exprFromText (Directed "(stdin)" 0 0 0 0) inText of
              Left  err  -> Control.Exception.throwIO err
              Right expr -> return expr

    expr' <- Dhall.Import.load expr
    case Dhall.TypeCheck.typeOf expr' of
      Left  err -> Control.Exception.throwIO err
      Right _   -> return ()

    json <- case Dhall.JSON.dhallToJSON expr' of
              Left err  -> Control.Exception.throwIO err
              Right json -> return json
    
    Data.ByteString.writeFile outFile (Data.Yaml.encode json) 

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: Control.Exception.SomeException -> IO a
    handler e = do
        System.IO.hPutStrLn System.IO.stderr ""
        System.IO.hPrint    System.IO.stderr e
        System.Exit.exitFailure
