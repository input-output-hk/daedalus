module MacInstaller where

---
--- An overview of Mac .pkg internals:  http://www.peachpit.com/articles/article.aspx?p=605381&seqNum=2
---

import           Control.Monad        (unless)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import qualified Data.Text            as T
import           System.Directory
import           System.Environment   (lookupEnv)
import           System.FilePath.Glob (glob)
import           Turtle               (ExitCode (..), echo, procs, shell, shells, Managed, with, mktempdir, liftIO, fromString, (</>))
import           Turtle.Line          (unsafeTextToLine)

import           Launcher
import           RewriteLibs          (chain)
import           System.IO
import           Filesystem.Path.CurrentOS (encodeString)

data InstallerConfig = InstallerConfig {
    icApi :: String
  , appNameLowercase :: T.Text
  , appName :: String
  , pkg :: T.Text
  , predownloadChain :: Bool
  , appRoot :: String
}

setupKeychain :: IO ()
setupKeychain = do
  -- Sign the installer with a special macOS dance
  run "security" ["create-keychain", "-p", "travis", "macos-build.keychain"]
  run "security" ["default-keychain", "-s", "macos-build.keychain"]
  exitcode <- shell "security import macos.p12 -P \"$CERT_PASS\" -k macos-build.keychain -T `which productsign`" mempty
  unless (exitcode == ExitSuccess) $ error "Signing failed"
  run "security" ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", "travis", "macos-build.keychain"]
  run "security" ["unlock-keychain", "-p", "travis", "macos-build.keychain"]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  api <- fromMaybe "cardano" <$> lookupEnv "API"
  version <- fromMaybe "dev" <$> lookupEnv "DAEDALUS_VERSION"
  isPullRequest <- fromMaybe "true" <$> lookupEnv "TRAVIS_PULL_REQUEST"
  if isPullRequest == "false" then setupKeychain
  else pure ()
  let
    cfg :: InstallerConfig
    cfg = InstallerConfig {
          icApi = api
        , predownloadChain = False
      }
  case api of
    "cardano" -> do
      let
        cfg' = cfg {
            appNameLowercase = "daedalus"
          , appName = "Daedalus"
          , pkg = "dist/Daedalus-installer-" <> T.pack version <> ".pkg"
          , appRoot = "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app"
        }
      makeInstaller cfg'
    "etc" -> do
      let
        cfg' = cfg {
            appNameLowercase = "daedalusmantis"
          , appName = "DaedalusMantis"
          , appRoot = "../release/darwin-x64/DaedalusMantis-darwin-x64/DaedalusMantis.app"
        }
        cfgtrue = cfg' {
            pkg = "dist/Daedalus-mantis-bootstrap-installer-" <> T.pack version <> ".pkg"
          , predownloadChain = True
        }
        cfgfalse = cfg' {
            pkg = "dist/Daedalus-mantis-installer-" <> T.pack version <> ".pkg"
          , predownloadChain = False
        }
      echo "pass one"
      makeInstaller cfgtrue
      echo "pass two"
      makeInstaller cfgfalse

makeScriptsDir :: InstallerConfig -> Managed T.Text
makeScriptsDir cfg = do
  case icApi cfg of
    "cardano" -> pure "data/scripts"
    "etc" -> do
      tempdir <- mktempdir "/tmp" "scripts"
      liftIO $ do
        copyFile "data/scripts/dockutil" (encodeString $ tempdir </> "dockutil")
        if predownloadChain cfg then
          copyFile "data/scripts-mantis/postinstall-bootstrap" (encodeString $ tempdir </> "postinstall")
        else
          copyFile "data/scripts-mantis/postinstall" (encodeString $ tempdir </> "postinstall")
      pure $ T.pack $ encodeString tempdir

makeInstaller :: InstallerConfig -> IO ()
makeInstaller cfg = do
  -- TODO, pass this in
  let
    bootstrap_url :: String
    bootstrap_url = "https://s3-eu-west-1.amazonaws.com/iohk.mantis.bootstrap/mantis-boot-classic-18DEC.zip"
    bootstrap_hash :: String
    bootstrap_hash = "7e210991f10f9ddebd5d9fe42e98c59e"
    -- how much space the chain will take in gig
    bootstrap_size :: Int
    bootstrap_size = 33

  let dir     = appRoot cfg <> "/Contents/MacOS"
      -- resDir  = appRoot <> "/Contents/Resources"
  createDirectoryIfMissing False "dist"

  echo "Creating icons ..."
  procs "iconutil" ["--convert", "icns", "--output", T.pack dir <> "/../Resources/electron.icns", "icons/electron.iconset"] mempty

  echo "Preparing files ..."
  case icApi cfg of
    "cardano" -> do
      copyFile "cardano-launcher" (dir <> "/cardano-launcher")
      copyFile "cardano-node" (dir <> "/cardano-node")
      copyFile "wallet-topology.yaml" (dir <> "/wallet-topology.yaml")
      copyFile "configuration.yaml" (dir <> "/configuration.yaml")
      genesisFiles <- glob "*genesis*.json"
      procs "cp" (fmap T.pack (genesisFiles <> [dir])) mempty
      copyFile "log-config-prod.yaml" (dir <> "/log-config-prod.yaml")
      copyFile "build-certificates-unix.sh" (dir <> "/build-certificates-unix.sh")
      copyFile "ca.conf"     (dir <> "/ca.conf")
      copyFile "server.conf" (dir <> "/server.conf")
      copyFile "client.conf" (dir <> "/client.conf")

      -- Rewrite libs paths and bundle them
      _ <- chain dir $ fmap T.pack [dir <> "/cardano-launcher", dir <> "/cardano-node"]
      pure ()
    "etc" -> do
      copyFile "build-certificates-unix-mantis.sh" (dir <> "/build-certificates-unix.sh")
      pure ()

  -- Prepare launcher
  de <- doesFileExist (dir <> "/Frontend")
  unless de $ renameFile (dir <> "/" <> appName cfg) (dir <> "/Frontend")
  run "chmod" ["+x", T.pack (dir <> "/Frontend")]

  case icApi cfg of
    "cardano" -> do
      writeFile (dir <> "/" <> appName cfg) $ unlines
        [ "#!/usr/bin/env bash"
        , "cd \"$(dirname $0)\""
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Secrets-1.0\""
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/pub\""
        , doLauncher
        ]
    "etc" -> do
      writeFile (dir <> "/" <> appName cfg) $ unlines
        [ "#!/usr/bin/env bash"
        , "cd \"$(dirname $0)\""
        , "export API=etc"
        , "export MANTIS_PATH=../Resources/app/mantis.app/Contents/"
        , "export MANTIS_CMD=MacOS/mantis"
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/\""
        , "./Frontend"
        ]
      writeFile (dir <> "/bootstrap.sh") $ unlines
        [ "#!/bin/sh"
        , "/Applications/DaedalusMantis.app/Contents/Resources/app/mantis.app/Contents/MacOS/mantis bootstrap " <> bootstrap_url <> " " <> bootstrap_hash <> " " <> show bootstrap_size
        ]
      run "chmod" [ "+x", T.pack (dir <> "/bootstrap.sh") ]
  run "chmod" ["+x", T.pack (dir <> "/" <> appName cfg)]

  with (makeScriptsDir cfg) $ \scriptsDir -> do
    let
      pkgargs :: [ T.Text ]
      pkgargs =
         [ "--identifier"
         , "org." <> appNameLowercase cfg <> ".pkg"
         -- data/scripts/postinstall is responsible for running build-certificates
         , "--scripts", scriptsDir
         , "--component"
         , T.pack $ appRoot cfg
         , "--install-location"
         , "/Applications"
         , "dist/temp.pkg"
         ]
    run "ls" [ "-ltrh", scriptsDir ]
    run "pkgbuild" pkgargs

  let productargs =
       [ "--product"
       , "data/plist"
       , "--package"
       , "dist/temp.pkg"
       , "dist/temp2.pkg"
       ]
  run "productbuild" productargs

  isPullRequest <- fromMaybe "true" <$> lookupEnv "TRAVIS_PULL_REQUEST"
  if isPullRequest == "false" then
    shells ("productsign --sign \"Developer ID Installer: Input Output HK Limited (89TW38X994)\" --keychain macos-build.keychain dist/temp2.pkg " <> pkg cfg) mempty
  else do
    echo "Pull request, not signing the installer."
    run "cp" ["dist/temp2.pkg", pkg cfg ]

  run "rm" ["dist/temp.pkg"]
  run "rm" ["dist/temp2.pkg"]

  echo $ "Generated " <> unsafeTextToLine (pkg cfg)

doLauncher :: String
doLauncher = "./cardano-launcher " <> launcherArgs Launcher
  { nodePath = "./cardano-node"
  , walletPath = "./Frontend"
  , nodeLogPath = appdata <> "Logs/cardano-node.log"
  , launcherLogPath = appdata <> "Logs/pub/"
  , windowsInstallerPath = Nothing
  , runtimePath = appdata
  , updater =
      WithUpdater
        { updArchivePath = appdata <> "installer.pkg"
        , updExec = "/usr/bin/open"
        , updArgs = ["-FW"]
        }
  }
    where
      appdata = "$HOME/Library/Application Support/Daedalus/"

run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
  echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
  procs cmd args mempty
