{-# LANGUAGE RecordWildCards, LambdaCase #-}
module MacInstaller
    ( main
    , SigningConfig(..)
    , signingConfig
    , setupKeyChain
    , deleteKeyChain
    , signInstaller
    ) where

---
--- An overview of Mac .pkg internals:    http://www.peachpit.com/articles/article.aspx?p=605381&seqNum=2
---

import           Universum

import           Control.Monad (unless, liftM2)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import           System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, renameFile)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Glob (glob)
import           Filesystem.Path.CurrentOS (encodeString)
import           Turtle (ExitCode (..), echo, proc, procs, which, Managed, with)
import           Turtle.Line (unsafeTextToLine)

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

-- In both Travis and Buildkite, the environment variable is set to
-- the pull request number if the current job is a pull request build,
-- or "false" if it’s not.
pullRequestFromEnv :: IO (Maybe String)
pullRequestFromEnv = liftM2 (<|>) (getPR "BUILDKITE_PULL_REQUEST") (getPR "TRAVIS_PULL_REQUEST")
  where
    getPR = fmap interpret . lookupEnv
    interpret Nothing        = Nothing
    interpret (Just "false") = Nothing
    interpret (Just num)     = Just num

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

  pr <- isJust <$> pullRequestFromEnv
  cfg <- installerConfigFromEnv

  tempInstaller <- makeInstaller cfg

  if pr
    then do
      echo "Pull request, not signing the installer."
      run "cp" [toText tempInstaller, pkg cfg]
    else do
      signInstaller signingConfig (toText tempInstaller) (pkg cfg)
      checkSignature (pkg cfg)

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
  writeLauncherFile dir cfg

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
writeLauncherFile dir _ = do
  writeFile path $ unlines contents
  run "chmod" ["+x", toText path]
  pure path
  where
    path = dir </> "Daedalus"
    contents =
      [ "#!/usr/bin/env bash"
      , "cd \"$(dirname $0)\""
      , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Secrets-1.0\""
      , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/pub\""
      , "./cardano-launcher"
      ]

data SigningConfig = SigningConfig
  { signingIdentity         :: T.Text
  , signingCertificate      :: FilePath
  , signingKeyChain         :: T.Text
  , signingKeyChainPassword :: T.Text
  } deriving (Show, Eq)

signingConfig :: SigningConfig
signingConfig = SigningConfig
  { signingIdentity = "Input Output HK Limited (89TW38X994)"
  , signingCertificate = "macos.p12"
  , signingKeyChain = "macos-build.keychain"
  , signingKeyChainPassword = "ci"
  }

-- | Add our certificate to a new keychain.
-- Uses the CERT_PASS environment variable to decrypt certificate.
setupKeyChain :: SigningConfig -> IO ()
setupKeyChain cfg@SigningConfig{..} = do
  password <- lookupEnv "CERT_PASS"
  run "security" ["create-keychain", "-p", signingKeyChainPassword, signingKeyChain]
  run "security" ["default-keychain", "-s", signingKeyChain]
  importCertificate cfg password >>= \case
    ExitSuccess -> do
      -- avoids modal dialogue popping up on sierra
      let sierraFix = ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", signingKeyChainPassword, signingKeyChain]
      echoCmd "security" sierraFix
      void $ proc "security" sierraFix mempty
      -- disables unlock timeout
      run "security" ["set-keychain-settings", signingKeyChain]
      -- for informational purposes
      run "security" ["show-keychain-info", signingKeyChain]
    ExitFailure c -> do
      deleteKeyChain cfg
      die $ "Signing failed with status " ++ show c

-- | Runs "security import"
importCertificate :: SigningConfig -> Maybe String -> IO ExitCode
importCertificate SigningConfig{..} password = do
  let optArg s = map toText . maybe [] (\p -> [s, p])
      certPass = optArg "-P" password
  productSign <- optArg "-T" . fmap encodeString <$> which "productsign"
  let args = ["import", toText signingCertificate, "-k", signingKeyChain] ++ certPass ++ productSign
  -- echoCmd "security" args
  proc "security" args mempty

-- | Remove our certificate's keychain from store.
deleteKeyChain :: SigningConfig -> IO ()
deleteKeyChain SigningConfig{..} = void $ proc "security" ["delete-keychain", signingKeyChain] mempty

signInstaller :: SigningConfig -> T.Text -> T.Text -> IO ()
signInstaller cfg@SigningConfig{..} src dst = withUnlockedKeyChain cfg $ sign
  where
    sign = procs "productsign" [ "--sign", "Developer ID Installer: " <> signingIdentity
                               , "--keychain", signingKeyChain
                               , toText src, toText dst ] mempty

-- | Unlock the keychain, perform an action, lock keychain.
withUnlockedKeyChain :: SigningConfig -> IO a -> IO a
withUnlockedKeyChain SigningConfig{..} = bracket_ unlock lock
  where
    unlock = run "security" ["unlock-keychain", "-p", signingKeyChainPassword, signingKeyChain]
    lock = run "security" ["lock-keychain", signingKeyChain]

-- | Use pkgutil to verify that signing worked.
checkSignature :: T.Text -> IO ()
checkSignature pkg = run "pkgutil" ["--check-signature", pkg]

-- | Print the command then run it.
run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
    echoCmd cmd args
    procs cmd args mempty

echoCmd :: T.Text -> [T.Text] -> IO ()
echoCmd cmd args = echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
