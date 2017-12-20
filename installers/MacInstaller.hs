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
import           Turtle               (ExitCode (..), echo, procs, shell, shells)
import           Turtle.Line          (unsafeTextToLine)

import           Launcher
import           RewriteLibs          (chain)
import           System.Posix.Files   (rename)

main :: IO ()
main = do
  version <- fromMaybe "dev" <$> lookupEnv "DAEDALUS_VERSION"
  api <- fromMaybe "cardano" <$> lookupEnv "API"
  let
    appNameLowercase = if api == "etc" then "daedalusmantis" else "daedalus"
    appName = if api == "etc" then "DaedalusMantis" else "Daedalus"

  let appRoot = "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app"
      dir     = appRoot <> "/Contents/MacOS"
      -- resDir  = appRoot <> "/Contents/Resources"
  let
    pkg = case api of
      "cardano" -> "dist/Daedalus-installer-" <> version <> ".pkg"
      "etc" -> "dist/Daedalus-mantis-installer-" <> version <> ".pkg"
  createDirectoryIfMissing False "dist"

  echo "Creating icons ..."
  procs "iconutil" ["--convert", "icns", "--output", T.pack dir <> "/../Resources/electron.icns", "icons/electron.iconset"] mempty

  echo "Preparing files ..."
  case api of
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
  unless de $ renameFile (dir <> "/Daedalus") (dir <> "/Frontend")
  run "chmod" ["+x", T.pack (dir <> "/Frontend")]

  case api of
    "cardano" -> do
      writeFile (dir <> "/Daedalus") $ unlines
        [ "#!/usr/bin/env bash"
        , "cd \"$(dirname $0)\""
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Secrets-1.0\""
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/pub\""
        , doLauncher
        ]
    "etc" -> do
      writeFile (dir <> "/Daedalus") $ unlines
        [ "#!/usr/bin/env bash"
        , "cd \"$(dirname $0)\""
        , "export API=etc"
        , "export MANTIS_PATH=../Resources/app/mantis.app/Contents/"
        , "export MANTIS_CMD=MacOS/mantis"
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/\""
        , "./Frontend"
        ]
  run "chmod" ["+x", T.pack (dir <> "/Daedalus")]

  if api == "etc" then
    rename "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app" "../release/darwin-x64/Daedalus-darwin-x64/DaedalusMantis.app"
  else
    pure ()

  let scriptsDir = if api == "etc" then "data/scripts-mantis" else "data/scripts"

  let pkgargs =
       [ "--identifier"
       , "org." <> appNameLowercase <> ".pkg"
       -- data/scripts/postinstall is responsible for running build-certificates
       , "--scripts", scriptsDir
       , "--component"
       , "../release/darwin-x64/Daedalus-darwin-x64/" <> appName <> ".app"
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

  isPullRequest <- fromMaybe "true" <$> lookupEnv "TRAVIS_PULL_REQUEST"
  if isPullRequest == "false" then do
    -- Sign the installer with a special macOS dance
    run "security" ["create-keychain", "-p", "travis", "macos-build.keychain"]
    run "security" ["default-keychain", "-s", "macos-build.keychain"]
    exitcode <- shell "security import macos.p12 -P \"$CERT_PASS\" -k macos-build.keychain -T `which productsign`" mempty
    unless (exitcode == ExitSuccess) $ error "Signing failed"
    run "security" ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", "travis", "macos-build.keychain"]
    run "security" ["unlock-keychain", "-p", "travis", "macos-build.keychain"]
    shells ("productsign --sign \"Developer ID Installer: Input Output HK Limited (89TW38X994)\" --keychain macos-build.keychain dist/temp2.pkg " <> T.pack pkg) mempty
  else do
    echo "Pull request, not signing the installer."
    run "cp" ["dist/temp2.pkg", T.pack pkg]

  run "rm" ["dist/temp.pkg"]
  run "rm" ["dist/temp2.pkg"]

  echo $ "Generated " <> unsafeTextToLine (T.pack pkg)

doLauncher :: String
doLauncher = "./cardano-launcher " <> (launcherArgs $ Launcher
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
  })
    where
      appdata = "$HOME/Library/Application Support/Daedalus/"

run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
  echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
  procs cmd args mempty
