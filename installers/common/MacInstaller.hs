{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MacInstaller
    ( main
    , SigningConfig(..)
    , signingConfig
    , signInstaller
    , importCertificate
    , deleteCertificate
    , run
    , run'
    , readCardanoVersionFile
    , withDir
    ) where

---
--- An overview of Mac .pkg internals:    http://www.peachpit.com/articles/article.aspx?p=605381&seqNum=2
---

import           Universum hiding (FilePath, toText, (<>))

import           Control.Monad (unless)
import           Control.Exception (handle)
import           Data.Text (Text)
import qualified Data.Text as T

import           System.FilePath.Glob (glob)
import           Filesystem.Path.CurrentOS (encodeString)
import           Filesystem.Path (FilePath, dropExtension, (</>), (<.>))
import           Turtle hiding (stdout, prefix, e)
import           Turtle.Line (unsafeTextToLine)

import           RewriteLibs (chain)

import           System.IO (hSetBuffering, BufferMode(NoBuffering))
import           System.IO.Error (IOError, isDoesNotExistError)

import           Config
import           Types

main :: Options -> IO ()
main opts@Options{..} = do
  hSetBuffering stdout NoBuffering

  generateOSClusterConfigs "./dhall" "." opts
  cp "launcher-config.yaml" "../launcher-config.yaml"

  installerConfig <- getInstallerConfig "./dhall" Macos64 oCluster

  appRoot <- buildElectronApp (macPackageName installerConfig) oCluster
  ver <- makeComponentRoot opts appRoot installerConfig
  daedalusVer <- getDaedalusVersion "../package.json"

  let pkg = packageFileName Macos64 oCluster daedalusVer ver oBuildJob
      opkg = oOutputDir </> pkg

  tempInstaller <- makeInstaller opts installerConfig appRoot pkg

  signInstaller signingConfig tempInstaller opkg
  checkSignature opkg

  run "rm" [tt tempInstaller]
  printf ("Generated "%fp%"\n") opkg

  when (oTestInstaller == TestInstaller) $ do
    echo $ "--test-installer passed, will test the installer for installability"
    procs "sudo" ["installer", "-dumplog", "-verbose", "-target", "/", "-pkg", tt opkg] empty

makePostInstall :: Format a (Text -> Text -> a)
makePostInstall = "#!/usr/bin/env bash\n" %
                  "#\n" %
                  "# See /var/log/install.log to debug this\n" %
                  "\n" %
                  "src_pkg=\"$1\"\ndst_root=\"$2\"\ndst_mount=\"$3\"\nsys_root=\"$4\"\n" %
                  "./dockutil --add \"${dst_root}/" % s % ".app\" --allhomes\n" %
                  "cd \"${dst_root}/" % s % ".app/Contents/MacOS/\"\n" %
                  "bash ./build-certificates-unix.sh"

makeScriptsDir :: Options -> Text -> Managed T.Text
makeScriptsDir Options{..} appname = case oBackend of
  Cardano _ -> do
    tempdir <- mktempdir "/tmp" "scripts"
    liftIO $ do
      cp "data/scripts/dockutil" (tempdir </> "dockutil")
      writeTextFile (tempdir </> "postinstall") (format makePostInstall appname appname)
      run "chmod" ["+x", tt (tempdir </> "postinstall")]
    pure $ tt tempdir
  Mantis    -> pure "[DEVOPS-533]"

-- | Builds the electron app with "npm package" and returns its
-- component root path.
-- NB: If webpack scripts are changed then this function may need to
-- be updated.
buildElectronApp :: Text -> Cluster -> IO FilePath
buildElectronApp appName cluster = do
  echo "Creating icons ..."
  procs "iconutil" ["--convert", "icns", "--output", "icons/electron.icns"
                   , "icons/electron.iconset"] mempty

  withDir ".." . sh $ npmPackage appName cluster

  let
    formatter :: Format r (Text -> Text -> r)
    formatter = "../release/darwin-x64/" % s % "-darwin-x64/" % s % ".app"
  pure $ fromString $ T.unpack $ format formatter appName appName

npmPackage :: Text -> Cluster -> Shell ()
npmPackage appName cluster = do
  let
    clusterToNetwork Mainnet = "mainnet"
    clusterToNetwork Staging = "testnet"
    clusterToNetwork Testnet = "testnet"
  mktree "release"
  echo "~~~ Installing nodejs dependencies..."
  procs "npm" ["install"] empty
  export "NODE_ENV" "production"
  export "NETWORK" $ clusterToNetwork cluster
  echo "~~~ Running electron packager script..."
  procs "npm" ["run", "package", "--", "--name", appName ] empty
  size <- inproc "du" ["-sh", "release"] empty
  printf ("Size of Electron app is " % l % "\n") size

makeComponentRoot :: Options -> FilePath -> InstallerConfig -> IO Text
makeComponentRoot Options{..} appRoot installerConfig = do
  let dir     = appRoot </> "Contents/MacOS"

  echo "~~~ Preparing files ..."
  ver <- case oBackend of
    Cardano bridge -> do
      -- Executables
      forM ["cardano-launcher", "cardano-node"] $ \f -> do
        cp (bridge </> "bin" </> f) (dir </> f)

      -- Config files (from daedalus-bridge)
      cp (bridge </> "config/configuration.yaml") (dir </> "configuration.yaml")
      cp (bridge </> "config/log-config-prod.yaml") (dir </> "log-config-prod.yaml")

      -- Genesis (from daedalus-bridge)
      genesisFiles <- glob . encodeString $ bridge </> "config" </> "*genesis*.json"
      when (null genesisFiles) $
        error "Cardano package carries no genesis files."
      procs "cp" (map T.pack genesisFiles ++ [tt dir]) mempty

      -- Config yaml (generated from dhall files)
      cp "launcher-config.yaml" (dir </> "launcher-config.yaml")
      cp "wallet-topology.yaml" (dir </> "wallet-topology.yaml")

      -- SSL
      cp "build-certificates-unix.sh" (dir </> "build-certificates-unix.sh")
      cp "ca.conf"     (dir </> "ca.conf")
      cp "server.conf" (dir </> "server.conf")
      cp "client.conf" (dir </> "client.conf")

      procs "chmod" ["-R", "+w", tt dir] empty

      -- Rewrite libs paths and bundle them
      void $ chain (encodeString dir) $ fmap tt [dir </> "cardano-launcher", dir </> "cardano-node"]

      readCardanoVersionFile bridge

    Mantis -> pure "mantis" -- DEVOPS-533

  -- Prepare launcher
  de <- testdir (dir </> "Frontend")
  unless de $ mv (dir </> (fromString $ T.unpack $ macPackageName installerConfig)) (dir </> "Frontend")
  run "chmod" ["+x", tt (dir </> "Frontend")]
  writeLauncherFile dir installerConfig

  pure ver

makeInstaller :: Options -> InstallerConfig -> FilePath -> FilePath -> IO FilePath
makeInstaller opts@Options{..} InstallerConfig{..} componentRoot pkg = do
  let tempPkg1 = format fp (oOutputDir </> pkg)
      tempPkg2 = oOutputDir </> (dropExtension pkg <.> "unsigned" <.> "pkg")

  mktree oOutputDir
  with (makeScriptsDir opts macPackageName) $ \scriptsDir -> do
    let
      pkgargs :: [ T.Text ]
      pkgargs =
           [ "--identifier"
           , "org."<> macPackageName <>".pkg"
           -- data/scripts/postinstall is responsible for running build-certificates
           , "--scripts", scriptsDir
           , "--component"
           , tt componentRoot
           , "--install-location"
           , "/Applications"
           , tempPkg1
           ]
    run "ls" [ "-ltrh", scriptsDir ]
    run "pkgbuild" pkgargs

  run "productbuild" [ "--product", "data/plist"
                     , "--package", tempPkg1
                     , format fp tempPkg2
                     ]

  run "rm" [tempPkg1]
  pure tempPkg2

-- | cardano-sl.daedalus-bridge should have a file containing its version.
readCardanoVersionFile :: FilePath -> IO Text
readCardanoVersionFile bridge = prefix <$> handle handler (readTextFile verFile)
  where
    verFile = bridge </> "version"
    prefix = maybe "UNKNOWN" ("cardano-sl-" <>) . safeHead . T.lines
    handler :: IOError -> IO Text
    handler e | isDoesNotExistError e = pure ""
              | otherwise = throwM e

writeLauncherFile :: FilePath -> InstallerConfig -> IO FilePath
writeLauncherFile dir installerConfig = do
  writeTextFile path $ T.unlines contents
  run "chmod" ["+x", tt path]
  pure path
  where
    appname = T.unpack $ macPackageName installerConfig
    path = dir </> (fromString appname)
    dataDir = "$HOME/Library/Application Support/" <> (installDirectory installerConfig)
    contents =
      [ "#!/usr/bin/env bash"
      , "cd \"$(dirname $0)\""
      , "mkdir -p \"" <> dataDir <> "/Secrets-1.0\""
      , "mkdir -p \"" <> dataDir <> "/Logs/pub\""
      , "./cardano-launcher"
      ]

data SigningConfig = SigningConfig
  { signingIdentity         :: T.Text
  , signingKeyChain         :: Maybe T.Text
  , signingKeyChainPassword :: Maybe T.Text
  } deriving (Show, Eq)

signingConfig :: SigningConfig
signingConfig = SigningConfig
  { signingIdentity = "Developer ID Installer: Input Output HK Limited (89TW38X994)"
  , signingKeyChain = Nothing
  , signingKeyChainPassword = Nothing
  }

-- | Runs "security import -x"
importCertificate :: SigningConfig -> FilePath -> Maybe Text -> IO ExitCode
importCertificate SigningConfig{..} cert password = do
  let optArg s = maybe [] (\p -> [s, p])
      certPass = optArg "-P" password
      keyChain = optArg "-k" signingKeyChain
  productSign <- optArg "-T" . fmap tt <$> which "productsign"
  let args = ["import", tt cert, "-x"] ++ keyChain ++ certPass ++ productSign
  proc "security" args mempty

--- | Remove our certificate from the keychain
deleteCertificate :: SigningConfig -> IO ExitCode
deleteCertificate SigningConfig{..} = run' "security" args
  where
    args = ["delete-certificate", "-c", signingIdentity] ++ keychain
    keychain = maybe [] pure signingKeyChain

-- | Creates a new installer package with signature added.
signInstaller :: SigningConfig -> FilePath -> FilePath -> IO ()
signInstaller SigningConfig{..} src dst =
  run "productsign" $ sign ++ keychain ++ map tt [src, dst]
  where
    sign = [ "--sign", signingIdentity ]
    keychain = maybe [] (\k -> [ "--keychain", k]) signingKeyChain

-- | Use pkgutil to verify that signing worked.
checkSignature :: FilePath -> IO ()
checkSignature pkg = run "pkgutil" ["--check-signature", tt pkg]

-- | Print the command then run it. Raises an exception on exit
-- failure.
run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
    echoCmd cmd args
    procs cmd args mempty

-- | Print the command then run it.
run' :: T.Text -> [T.Text] -> IO ExitCode
run' cmd args = do
    echoCmd cmd args
    proc cmd args mempty

echoCmd :: T.Text -> [T.Text] -> IO ()
echoCmd cmd args = echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
