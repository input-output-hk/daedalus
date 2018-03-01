{-# LANGUAGE RecordWildCards   #-}
module Config
  ( generateConfig
  , Request(..)
  , OS(..), Cluster(..), Config(..)
  ) where

import qualified Control.Exception

import qualified Data.Char
import qualified Data.ByteString
import           Data.Semigroup
import           Data.Text.Lazy
import qualified Data.Yaml

import qualified Dhall
import qualified Dhall.Import
import qualified Dhall.JSON
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

import qualified GHC.IO.Encoding

import qualified System.IO
import qualified System.Exit

import           Text.Trifecta.Delta (Delta(..))

import Prelude

data OS
  = Linux
  | Macos64
  | Win64
  deriving (Eq, Show)

data Cluster
  = Mainnet
  | Staging
  deriving (Eq, Read, Show)

data Config
  = Launcher
  | Topology
  deriving (Eq, Show)

data Request
  = Request
  { rOS      :: OS
  , rCluster :: Cluster
  , rConfig  :: Config
  } deriving (Eq, Show)

lshow :: Show a => a -> Text
lshow = Data.Text.Lazy.pack . fmap Data.Char.toLower . show

dhallTopExpr :: Config -> OS -> Cluster -> Text
dhallTopExpr Launcher os cluster = "./launcher.dhall ( ./" <> lshow cluster <> ".dhall ./" <> lshow os <> ".dhall ) ./" <> lshow os <> ".dhall"
dhallTopExpr Topology os cluster = "./topology.dhall ( ./" <> lshow cluster <> ".dhall ./" <> lshow os <> ".dhall )"

generateConfig :: Request -> FilePath -> IO ()
generateConfig Request{..} outFile = handle $ do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

  Dhall.detailed $ do
    let inText = dhallTopExpr rConfig rOS rCluster

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
