{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Pos.Launcher.Configuration (Configuration, ccUpdate_L)
import System.Environment (getArgs)
import qualified Data.Yaml as Y
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Control.Lens
import Pos.Chain.Update (ccApplicationVersion_L)
import Pos.Util.Config (parseYamlConfig)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Exit (exitWith, ExitCode(ExitFailure))
import qualified Data.Aeson
import Cardano.X509.Configuration
import Data.Monoid ((<>))

parseToValue :: String -> IO (Either Y.ParseException Data.Aeson.Value)
parseToValue = Y.decodeFileEither

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ filepath, key, appVer ] -> do
      tlscfg <- decodeConfigFile (ConfigurationKey key) filepath
      cfg <- parseYamlConfig filepath (T.pack key)
      let
        newcfg :: Configuration
        newcfg = cfg & ccUpdate_L . ccApplicationVersion_L .~ (read appVer)
        newmap = Map.singleton key newcfg
        partialcfg = Data.Aeson.toJSON newcfg
        yaml = Y.encode newmap
      -- this mess exists because `Configuration` lacks the `tls` field
      let
        tlsvalue = Data.Aeson.toJSON tlscfg
        deobj (Data.Aeson.Object hm) = hm
        wrappedtls :: HM.HashMap T.Text Data.Aeson.Value
        wrappedtls = HM.singleton "tls" tlsvalue
        both = Data.Aeson.Object (wrappedtls <> (deobj partialcfg))
        finalCfg = Data.Aeson.Object (HM.singleton (T.pack key) both)
      BS.putStr $ Y.encode finalCfg
    _ -> do
      putStrLn "usage: config-mutator lib/configuration.yaml mainnet_dryrun_wallet_win64 42"
      exitWith $ ExitFailure 1
