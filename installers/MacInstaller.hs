{-# LANGUAGE RecordWildCards, LambdaCase #-}
module MacInstaller
    ( main
    , SigningConfig(..)
    , signingConfig
    , signInstaller
    , importCertificate
    , deleteCertificate
    ) where

---
--- An overview of Mac .pkg internals:    http://www.peachpit.com/articles/article.aspx?p=605381&seqNum=2
---

import           Universum

import           Control.Monad (unless, liftM2)
import           Data.Maybe (fromMaybe)
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

travisJobIdFromEnv :: IO (Maybe String)
travisJobIdFromEnv = lookupEnv "TRAVIS_JOB_ID"

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

  cfg <- installerConfigFromEnv
  tempInstaller <- makeInstaller cfg
  shouldSign <- shouldSignDecision

  if shouldSign
    then do
      signInstaller signingConfig (toText tempInstaller) (pkg cfg)
      checkSignature (pkg cfg)
    else do
      echo "Pull request, not signing the installer."
      run "cp" [toText tempInstaller, pkg cfg]

  run "rm" [toText tempInstaller]
  echo $ "Generated " <> unsafeTextToLine (pkg cfg)

-- | Only sign installer if this is not a normal PR build.
shouldSignDecision :: IO Bool
shouldSignDecision = do
  pr <- pullRequestFromEnv
  pure (pr == Nothing || pr == Just "629")

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
  let optArg s = map toText . maybe [] (\p -> [s, p])
      certPass = optArg "-P" password
      keyChain = optArg "-k" signingKeyChain
  productSign <- optArg "-T" . fmap (toText . encodeString) <$> which "productsign"
  let args = ["import", toText cert, "-x"] ++ keyChain ++ certPass ++ productSign
  -- echoCmd "security" args
  proc "security" args mempty

--- | Remove our certificate from the keychain
deleteCertificate :: SigningConfig -> IO ExitCode
deleteCertificate SigningConfig{..} = proc "security" args mempty
  where
    args = ["delete-certificate", "-c", signingIdentity] ++ keychain
    keychain = maybe [] pure signingKeyChain

-- | Creates a new installer package with signature added.
signInstaller :: SigningConfig -> T.Text -> T.Text -> IO ()
signInstaller SigningConfig{..} src dst =
  procs "productsign" (sign ++ keychain ++ [ toText src, toText dst ]) mempty
  where
    sign = [ "--sign", signingIdentity ]
    keychain = maybe [] (\k -> [ "--keychain", k]) signingKeyChain

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
