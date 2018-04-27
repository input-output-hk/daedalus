{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
import           Data.Text
import           Universum
import qualified System.Info                      as Sys

import qualified MacInstaller                        (main)
import qualified WindowsInstaller                    (main)

import           Types
import           Config

main :: IO ()
main = do
  let os = case Sys.os of
             "linux"   -> Linux64
             "darwin"  -> Macos64
             "mingw32" -> Win64
             _         -> error ("Unsupported OS: " <> pack Sys.os)

  (options', command) <- Config.options "Daedalus installer generator" $
    (,) <$> optionsParser os <*> commandParser

  case command of
    GenConfig{..}    ->
      generateOSClusterConfigs cfDhallRoot cfOutdir options'
    CheckConfigs{..} ->
      checkAllConfigs          cfDhallRoot
    GenInstaller -> do
      putStrLn $ "Generating installer for " <>  Sys.os <> "-" <> Sys.arch
      case os of
        Linux64 -> putStrLn ("Use default.nix, please." :: String)
        Macos64 ->     MacInstaller.main options'
        Win64   -> WindowsInstaller.main options'
