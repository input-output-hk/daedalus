{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module SigningSpec where

import           Universum hiding (FilePath)
import           Filesystem.Path.CurrentOS (encodeString)
import           System.Info
import           Turtle (MonadIO(..), FilePath, proc, procs, mktree, touch, (</>), mktempfile, mktempdir, sh, export, ExitCode(..))
import           Control.Monad.Managed (MonadManaged(..))
import           MacInstaller hiding (main)

import           Test.Hspec

-- | Very basic test case.
-- Attempts to do all the commands for setting up keychain,
-- importing certificate, signing.
-- Generates a dummy package with pkgbuild.
-- Creates a self-signed certifate for signing.
--
-- TODO: unfortunately it fails, something is
-- productsign: error: Could not find appropriate signing identity for “Developer ID Installer: Input Output HK Limited (89TW38X994)” in keychain at “installer-tests.keychain”.
spec :: Spec
spec = do
  describe "Mac Installer Signing" $ do
    xit "signs stuff" $ macTest $ do
      res <- sh $ do
        pkg <- createDummyPkg
        outPkg <- mktempfile "/tmp" "installer.pkg"
        cfg <- setupConfig
        liftIO $ signInstaller cfg pkg (format outPkg)
        res <- checkSignature outPkg
        liftIO $ res `shouldBe` ExitSuccess
      res `shouldBe` ()

createDummyPkg :: MonadManaged m => m Text
createDummyPkg = do
  pkg <- mktempfile "/tmp" "dummy.pkg"
  root <- createDummyPkgRoot
  procs "pkgbuild" ["--identifier", "org.daedalus.pkg", "--root", format root, "--install-location", "/Applications", toText (encodeString pkg)] mempty
  return $ toText (encodeString pkg)

createDummyPkgRoot :: MonadManaged m => m FilePath
createDummyPkgRoot = do
  dir <- mktempdir "/tmp" "dummy-pkg-root"
  let app = dir </> "Applications/Daedalus"
  mktree app
  touch (app </> "hello")
  return dir

setupConfig :: MonadManaged m => m SigningConfig
setupConfig = do
  tmp <- mktempfile "/tmp" "signing-tests.p12"
  let cfg = signingConfig { signingCertificate = encodeString tmp
                          , signingKeyChain = "installer-tests.keychain" }
  liftIO $ createDummyCertificate cfg
  pure cfg

-- | Create a self-signed PKCS#12 certificate and import it to our
-- keychain, for testing purposes.
createDummyCertificate :: SigningConfig -> IO ()
createDummyCertificate cfg@SigningConfig{..} = do
  let password = "dummy"
  export "CERT_PASS" password
  sh $ do
    tmp <- mktempdir "/tmp" "dummy-cert"
    let pem = tmp </> "key.pem"
        cert = tmp </> "macos.pem"
        conf = tmp </> "apple.conf"
    writeFile (encodeString conf) selfSignedConf
    procs "openssl" [ "req", "-newkey", "rsa:2048", "-nodes"
                    , "-keyout", format pem, "-x509", "-days", "365"
                    , "-config", format conf
                    , "-subj", "/C=UK/ST=Oxfordshire/L=Oxford/O=IOHK/OU=DevOps/CN=" <> signingIdentity
                    , "-out", format cert ] mempty
    procs "openssl" ["pkcs12", "-inkey", format pem, "-in", format cert, "-export"
                    , "-out", toText signingCertificate
                    , "-passout", "pass:" <> toText password] mempty
  deleteKeyChain cfg
  setupKeyChain cfg

-- https://security.stackexchange.com/a/47980
selfSignedConf :: Text
selfSignedConf = unlines
  [ "[ req ]"
  , "distinguished_name = req_name"
  , "prompt = no"
  , "[ req_name ]"
  , "CN = my-test-installer"
  , "[ extensions ]"
  , "basicConstraints=critical,CA:false"
  , "keyUsage=critical,digitalSignature"
  , "extendedKeyUsage=critical,1.2.840.113635.100.4.13"
  , "1.2.840.113635.100.6.1.14=critical,DER:0500"
  ]

checkSignature :: MonadIO io => FilePath -> io ExitCode
checkSignature pkg = proc "pkgutil" ["--check-signature", format pkg] mempty

-- | Only execute this spec on Mac.
macTest :: Applicative f => f () -> f ()
macTest s | os == "darwin" = s
          | otherwise = pure ()

format :: FilePath -> Text
format = toText . encodeString
