module MacOSPackageSigning
    ( SigningConfig(..)
    , signingConfig
    , signMacOSInstaller
    , importCertificate
    , deleteCertificate
    , checkSignature
    , run
    , run'
    ) where

import           Universum                 hiding (FilePath, toText, (<>))

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Turtle                    hiding (e, prefix, stdout)

import           Types

data SigningConfig = SigningConfig
  { signingIdentity         :: Text
  , signingKeyChain         :: Maybe Text
  , signingKeyChainPassword :: Maybe Text
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
signMacOSInstaller :: FilePath -> FilePath -> IO ()
signMacOSInstaller = signInstaller signingConfig

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
run :: Text -> [Text] -> IO ()
run cmd args = do
    echoCmd cmd args
    procs cmd args mempty

-- | Print the command then run it.
run' :: Text -> [Text] -> IO ExitCode
run' cmd args = do
    echoCmd cmd args
    proc cmd args mempty

echoCmd :: Text -> [Text] -> IO ()
echoCmd cmd args = echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
