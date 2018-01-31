module MacInstaller
    ( main
    ) where

---
--- An overview of Mac .pkg internals:    http://www.peachpit.com/articles/article.aspx?p=605381&seqNum=2
---

import           Universum

import           Control.Monad (unless)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, renameFile)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Glob (glob)
import           Turtle (ExitCode (..), echo, procs, shell, shells, Managed, with)
import           Turtle.Line (unsafeTextToLine)

import           Launcher
import           RewriteLibs (chain)

import           System.IO (hSetBuffering, BufferMode(NoBuffering))

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

isPullRequestFromEnv :: IO Bool
isPullRequestFromEnv = interpret <$> lookupEnv "TRAVIS_PULL_REQUEST"
  where
    interpret (Just "false") = False
    interpret _              = True

installerConfigFromEnv :: IO InstallerConfig
installerConfigFromEnv = mkEnv <$> envAPI <*> envVersion
  where
    envAPI = fromMaybe "cardano" <$> lookupEnv "API"
    envVersion = fromMaybe "dev" <$> lookupEnv "DAEDALUS_VERSION"
    mkEnv "cardano" ver = InstallerConfig
      { icApi = "cardano"
      , predownloadChain = False
      , appNameLowercase = "daedalus"
      , appName = "Daedalus"
      , pkg = "dist/Daedalus-installer-" <> T.pack ver <> ".pkg"
      , appRoot = "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app"
      }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  pr <- isPullRequestFromEnv

  unless pr setupKeychain

  cfg <- installerConfigFromEnv

  tempInstaller <- makeInstaller cfg

  if pr
    then do
      echo "Pull request, not signing the installer."
      run "cp" [toText tempInstaller, pkg cfg]
    else do
      signInstaller (toText tempInstaller) (pkg cfg)

  run "rm" [toText tempInstaller]
  echo $ "Generated " <> unsafeTextToLine (pkg cfg)


makeScriptsDir :: InstallerConfig -> Managed T.Text
makeScriptsDir cfg = case icApi cfg of
  "cardano" -> pure "data/scripts"
  "etc" -> pure "[DEVOPS-533]"

makeInstaller :: InstallerConfig -> IO FilePath
makeInstaller cfg = do
  let dir     = appRoot cfg </> "Contents/MacOS"
      resDir  = appRoot cfg </> "Contents/Resources"
  createDirectoryIfMissing False "dist"

  echo "Creating icons ..."
  procs "iconutil" ["--convert", "icns", "--output", toText (resDir </> "electron.icns"), "icons/electron.iconset"] mempty

  echo "Preparing files ..."
  case icApi cfg of
    "cardano" -> do
      copyFile "cardano-launcher" (dir </> "cardano-launcher")
      copyFile "cardano-node" (dir </> "cardano-node")
      copyFile "wallet-topology.yaml" (dir </> "wallet-topology.yaml")
      copyFile "configuration.yaml" (dir </> "configuration.yaml")
      genesisFiles <- glob "*genesis*.json"
      procs "cp" (fmap toText (genesisFiles <> [dir])) mempty
      copyFile "log-config-prod.yaml" (dir </> "log-config-prod.yaml")
      copyFile "build-certificates-unix.sh" (dir </> "build-certificates-unix.sh")
      copyFile "ca.conf"     (dir </> "ca.conf")
      copyFile "server.conf" (dir </> "server.conf")
      copyFile "client.conf" (dir </> "client.conf")

      let launcherConfigFileName = "launcher-config.yaml"
      copyFile "launcher-config-mac.yaml" (dir </> launcherConfigFileName)

      -- Rewrite libs paths and bundle them
      _ <- chain dir $ fmap toText [dir </> "cardano-launcher", dir </> "cardano-node"]
      pure ()
    _ -> pure () -- DEVOPS-533

  -- Prepare launcher
  de <- doesFileExist (dir </> "Frontend")
  unless de $ renameFile (dir </> "Daedalus") (dir </> "Frontend")
  run "chmod" ["+x", toText (dir </> "Frontend")]

  launcherFile <- writeLauncherFile dir cfg
  run "chmod" ["+x", toText launcherFile]

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

  run "productbuild" [ "--product", "data/plist"
                     , "--package", "dist/temp.pkg"
                     , "dist/temp2.pkg"
                     ]

  run "rm" ["dist/temp.pkg"]
  pure "dist/temp2.pkg"

writeLauncherFile :: FilePath -> InstallerConfig -> IO FilePath
writeLauncherFile dir cfg = writeFile path contents >> pure path
  where
    path = dir </> appName cfg
    contents = unlines $ launcher (icApi cfg)
    launcher "cardano" =
      [ "#!/usr/bin/env bash"
      , "cd \"$(dirname $0)\""
      , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Secrets-1.0\""
      , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/pub\""
      , toText doLauncher
      ]
    launcher _ = [] -- DEVOPS-533

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

signInstaller :: T.Text -> T.Text -> IO ()
signInstaller src dst = do
  -- Sign the installer with a special macOS dance
  run "security" ["create-keychain", "-p", "ci", "macos-build.keychain"]
  run "security" ["default-keychain", "-s", "macos-build.keychain"]
  exitcode <- shell "security import macos.p12 -P \"$CERT_PASS\" -k macos-build.keychain -T `which productsign`" mempty
  unless (exitcode == ExitSuccess) $ error "Signing failed"
  run "security" ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", "ci", "macos-build.keychain"]
  run "security" ["unlock-keychain", "-p", "ci", "macos-build.keychain"]
  shells ("productsign --sign \"Developer ID Installer: Input Output HK Limited (89TW38X994)\" --keychain macos-build.keychain " <> toText src <> " " <> toText dst) mempty

run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
    echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
    procs cmd args mempty
