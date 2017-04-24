module MacInstaller where

import           Control.Monad        (unless)
import qualified Data.Text            as T
import           Data.Foldable        (for_)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           System.Environment   (lookupEnv)
import           System.FilePath      (replaceExtension)
import           System.FilePath.Glob (globDir1, compile)
import           System.Directory
import           Turtle               (procs, echo, shells)
import           Turtle.Line          (unsafeTextToLine)

import RewriteLibs                    (chain)
import Launcher


main :: IO ()
main = do
  version <- fromMaybe "dev" <$> lookupEnv "DAEDALUS_VERSION"

  let dir = "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app/Contents/MacOS"
      pkg = "dist/Daedalus-installer-" <> version <> ".pkg"
  createDirectoryIfMissing False "dist"

  echo "Creating icons ..."
  icons <- globDir1 (compile "*.png") "icons"
  for_ icons $ \icon -> do
    let file = replaceExtension icon "icns"
    procs "sips" ["-s", "format", "icns", T.pack icon, "--out", T.pack file] mempty

  echo "Preparing files ..."
  copyFile "cardano-launcher" (dir <> "/cardano-launcher")
  copyFile "cardano-node" (dir <> "/cardano-node")
  copyFile "cardano-addr-convert" (dir <> "/cardano-addr-convert")
  copyFile "log-config-prod.yaml" (dir <> "/log-config-prod.yaml")
  copyFile "data/ip-dht-mappings" (dir <> "/ip-dht-mappings")

  -- Rewrite libs paths and bundle them
  _ <- chain dir $ fmap T.pack [dir <> "/cardano-launcher", dir <> "/cardano-node", dir <> "/cardano-addr-convert"]

  -- Prepare launcher
  de <- doesFileExist (dir <> "/Frontend")
  unless de $ renameFile (dir <> "/Daedalus") (dir <> "/Frontend")
  run "chmod" ["+x", T.pack (dir <> "/Frontend")]
  writeFile (dir <> "/Daedalus") $ unlines
    [ "#!/usr/bin/env bash"
    , "cd \"$(dirname $0)\""
    , "mkdir -p \"$HOME/Library/Application Support/Daedalus/\"{Wallet-0.2,DB-0.2,Logs,Secrets}"
    , doLauncher
    ]
  run "chmod" ["+x", T.pack (dir <> "/Daedalus")]

  let pkgargs =
       [ "--identifier"
       , "org.daedalus.pkg"
       , "--scripts", "data/scripts"
       , "--component"
       , "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app"
       , "--install-location"
       , "/Applications"
       , "dist/temp.pkg"
       ]
  run "pkgbuild" pkgargs

  let productargs =
       [ "--product"
       , "data/plist"
       , "--package"
       , "dist/temp.pkg"
       , "dist/temp2.pkg"
       ]
  run "productbuild" productargs

  -- Sign the installer with a special macOS dance
  run "security" ["create-keychain", "-p", "travis", "macos-build.keychain"]
  run "security" ["default-keychain", "-s", "macos-build.keychain"]
  shells "security import macos.p12 -P $CERT_PASS -k macos-build.keychain -T `which productsign`" mempty
  run "security" ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", "travis", "macos-build.keychain"]
  run "security" ["unlock-keychain", "-p", "travis", "macos-build.keychain"]
  shells ("productsign --sign \"Developer ID Installer: Nikolaos Bentenitis\" --keychain macos-build.keychain dist/temp2.pkg " <> T.pack pkg) mempty

  run "rm" ["dist/temp.pkg"]
  run "rm" ["dist/temp2.pkg"]

  echo $ "Generated " <> unsafeTextToLine (T.pack pkg)

doLauncher :: String
doLauncher = "./cardano-launcher " <> (launcherArgs $ Launcher
  { nodePath = "./cardano-node"
  , walletPath = "./Frontend"
  , nodeLogPath = appdata <> "Logs/cardano-node.log"
  , installerPath = appdata <> "installer.pkg"
  , runtimePath = appdata
  })
    where
      appdata = "$HOME/Library/Application Support/Daedalus/"

run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
  echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
  procs cmd args mempty
