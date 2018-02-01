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
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import           Turtle (ExitCode (..), echo, procs, shell, shells)
import           Turtle.Line (unsafeTextToLine)

import           RewriteLibs (chain)


main :: IO ()
main = do
    version <- fromMaybe "dev" <$> lookupEnv "DAEDALUS_VERSION"

    let appRoot = "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app"
        dir     = appRoot </> "Contents/MacOS"
        -- resDir = appRoot </> "Contents/Resources"
        pkg     = "dist/Daedalus-installer-" <> version <> ".pkg"
    createDirectoryIfMissing False "dist"

    echo "Creating icons ..."
    procs "iconutil" ["--convert", "icns", "--output", toText (dir </> "../Resources/electron.icns"), "icons/electron.iconset"] mempty

    echo "Preparing files ..."
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

    -- Prepare launcher
    de <- doesFileExist (dir </> "Frontend")
    unless de $ renameFile (dir </> "Daedalus") (dir </> "Frontend")
    run "chmod" ["+x", toText (dir </> "Frontend")]
    writeFile (dir </> "Daedalus") $ unlines
        [ "#!/usr/bin/env bash"
        , "cd \"$(dirname $0)\""
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Secrets-1.0\""
        , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/pub\""
        , "./cardano-launcher"
        ]
    run "chmod" ["+x", toText (dir </> "Daedalus")]

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

    isPullRequest <- fromMaybe "true" <$> lookupEnv "TRAVIS_PULL_REQUEST"
    if isPullRequest == "false" then do
        -- Sign the installer with a special macOS dance
        run "security" ["create-keychain", "-p", "travis", "macos-build.keychain"]
        run "security" ["default-keychain", "-s", "macos-build.keychain"]
        exitcode <- shell "security import macos.p12 -P \"$CERT_PASS\" -k macos-build.keychain -T `which productsign`" mempty
        unless (exitcode == ExitSuccess) $ error "Signing failed"
        run "security" ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", "travis", "macos-build.keychain"]
        run "security" ["unlock-keychain", "-p", "travis", "macos-build.keychain"]
        shells ("productsign --sign \"Developer ID Installer: Input Output HK Limited (89TW38X994)\" --keychain macos-build.keychain dist/temp2.pkg " <> toText pkg) mempty
    else do
        echo "Pull request, not signing the installer."
        run "cp" ["dist/temp2.pkg", toText pkg]

    run "rm" ["dist/temp.pkg"]
    run "rm" ["dist/temp2.pkg"]

    echo $ "Generated " <> unsafeTextToLine (toText pkg)

run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
    echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
    procs cmd args mempty
