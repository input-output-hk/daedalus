{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, LambdaCase #-}
module Main where

import Universum hiding (FilePath, fold)
import Test.Hspec
import qualified Data.Text as T
import Filesystem.Path (FilePath, (</>))
import Filesystem.Path.CurrentOS (fromText, decodeString)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Turtle (mktempdir, inproc, strict, ls, fold, writeTextFile, mktree, mkdir, cptree)
import Control.Monad.Managed (MonadManaged, runManaged)
import Data.Aeson.Types (Value)
import qualified Control.Foldl as Fold
import           System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified GitHub as GH
import Data.Aeson (decode)

import Config
import Types
import qualified MacInstaller as Mac
import Util
import AppVeyor

main :: IO ()
main = hspec $ do
  describe "Utility functions" utilSpec
  describe "MacInstaller build" macBuildSpec
  describe "recursive directory deletion" deleteSpec
  describe "Hydra downloads for AppVeyor" hydraSpec

macBuildSpec :: Spec
macBuildSpec = do
  describe "The whole thing" $ do
    it "Runs through the whole installer build" $ runManaged $ do
      out <- getTempDir "test-build"
      installersDir <- makeTestInstallersDir
      daedalusBridge <- liftIO getDaedalusBridge

      let opts = Options
                 { oOS = Win64
                 , oBackend = Cardano daedalusBridge
                 , oBuildJob = Just (BuildJob "test")
                 , oCluster = Nightly
                 , oAppName = "Daedalus"
                 , oOutputDir = out
                 , oTestInstaller = testInstaller False
                 , oSigningConfigPath = Nothing
                 }

      liftIO $ do
        withDir installersDir $ do
          mktree "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app/Contents/Resources/app"
          writeFile "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app/Contents/Resources/app/package.json" "{}"
          Mac.main opts

        -- there should be an installer file at the end
        fold (ls out) Fold.length `shouldReturn` 1

  describe "Cardano version file" $ do
    it "Reads it" $ runManaged $ do
      tmp <- getTempDir "test-bridge"
      liftIO $ writeTextFile (tmp </> "version") "1.2.3"
      liftIO $ Mac.readCardanoVersionFile tmp `shouldReturn` "1.2.3"
    it "Handles missing version file" $ runManaged $ do
      tmp <- getTempDir "test-bridge"
      liftIO $ Mac.readCardanoVersionFile tmp `shouldReturn` "UNKNOWN"

-- | Set up a temporary source/installers directory with everything
-- required for the installer builder. This is so that the installer
-- builder can be tested in a pure environment without any
-- dependencies.
makeTestInstallersDir :: MonadManaged m => m FilePath
makeTestInstallersDir = do
  src <- getTempDir "test-source"
  liftIO $ writeTextFile (src </> "package.json") "{ \"version\": \"0.4.2\" }"
  let installersDir = src </> "installers"
  mkdir installersDir
  cptree "dhall" (installersDir </> "dhall")
  mktree (installersDir </> "data/scripts")
  liftIO $ writeTextFile (installersDir </> "data/scripts/dockutil") "fake dock util"
  pure installersDir

-- | Run a special command to get the cardano-sl.daedalus-bridge path.
getDaedalusBridge :: IO FilePath
getDaedalusBridge = fromText . T.stripEnd <$> strict (inproc "daedalus-bridge" [] empty)

deleteSpec :: Spec
deleteSpec = do
  describe "deleting a path over 256 chars long" $ do
    it "it" $ do
      let
        name = replicate 16 'a'
        goMake :: Int -> IO ()
        goMake 0 = pure ()
        goMake n = do
          createDirectory name
          withCurrentDirectory name (goMake $ n-1)
      whenM (doesDirectoryExist name) $
        windowsRemoveDirectoryRecursive name
      goMake 32
      let
        file = (intercalate "/" (replicate 32 name)) <> "/filename"
      writeFile file "body"
      windowsRemoveDirectoryRecursive name

type Yuck = Value -> IO ()

getTempDir :: MonadManaged io => Text -> io FilePath
getTempDir template = do
  tmp <- liftIO . fmap decodeString $ getCanonicalTemporaryDirectory
  mktempdir tmp template

utilSpec :: Spec
utilSpec = do
  describe "Daedalus version loading" $ do
    xit "loads the actual version file" $ do
      -- referring to parent directory won't work in nix-build
      Version ver <- getDaedalusVersion "../package.json"
      ver `shouldSatisfy` (not . T.null)

    it "loads a version file" $ do
      packageVersion "{ \"version\": \"1.1.1\" }" `shouldReturn` (Version "1.1.1")

  describe "Package filename generation" $ do
    it "generates a good filename for windows" $ do
      let f = packageFileName Win64 Nightly (Version "0.4.2") (Cardano "") "9.9" (Just "job.id")
      f `shouldBe` (fromText "daedalus-0.4.2-cardano-sl-9.9-nightly-windows-job.id.exe")

----------------------------------------------------------------------------
-- Tests for Hydra downloading (yes it's in the AppVeyor module)

hydraBuildJSON :: BL.ByteString
hydraBuildJSON = "{\"stoptime\":1548312357,\"starttime\":1548312357,\"timestamp\":1548312190,\"buildstatus\":0,\"nixname\":\"daedalus-mingw32-pkg\",\"id\":545198,\"finished\":1,\"buildmetrics\":{},\"releasename\":null,\"jobset\":\"cardano-sl-pr-4001\",\"priority\":100,\"buildproducts\":{\"1\":{\"type\":\"file\",\"path\":\"/nix/store/ifswlpnvvw4z5xj7wwhygc4alvrs7whj-daedalus-mingw32-pkg/CardanoSL.zip\",\"sha1hash\":\"b844f4a8ce40782f0764151d1e0b3c6e4432254a\",\"subtype\":\"binary-dist\",\"defaultpath\":\"\",\"filesize\":168,\"name\":\"CardanoSL.zip\",\"sha256hash\":\"9fe1bf5e7b8b07628fd25f0ffd07ea297c12dd820d4c433a9f20efcbc0104a82\"}},\"buildoutputs\":{\"out\":{\"path\":\"/nix/store/ifswlpnvvw4z5xj7wwhygc4alvrs7whj-daedalus-mingw32-pkg\"}},\"drvpath\":\"/nix/store/yy9cs24y0gcacpl0646n083vnvbfj85g-daedalus-mingw32-pkg.drv\",\"jobsetevals\":[484201],\"job\":\"daedalus-mingw32-pkg\",\"system\":\"x86_64-linux\",\"project\":\"serokell\"}"

hydraBuildURL :: Text
hydraBuildURL = "https://hydra.iohk.io/build/545273"

githubStatusJSON :: BL.ByteString
githubStatusJSON = "[{\"url\":\"https://api.github.com/repos/input-output-hk/cardano-sl/statuses/b57ede2df60658750bb7629d3946e984187a3224\",\"avatar_url\":\"https://avatars1.githubusercontent.com/u/29731078?v=4\",\"id\":6137719519,\"node_id\":\"MDEzOlN0YXR1c0NvbnRleHQ2MTM3NzE5NTE5\",\"state\":\"success\",\"description\":\"Hydra build #550173 of serokell:cardano-sl-pr-4001:required\",\"target_url\":\"https://hydra.iohk.io/build/550173\",\"context\":\"ci/hydra:serokell:cardano-sl:required\",\"created_at\":\"2019-01-25T00:50:50Z\",\"updated_at\":\"2019-01-25T00:50:50Z\",\"creator\":{\"login\":\"iohk-devops\",\"id\":29731078,\"node_id\":\"MDQ6VXNlcjI5NzMxMDc4\",\"avatar_url\":\"https://avatars1.githubusercontent.com/u/29731078?v=4\",\"gravatar_id\":\"\",\"url\":\"https://api.github.com/users/iohk-devops\",\"html_url\":\"https://github.com/iohk-devops\",\"followers_url\":\"https://api.github.com/users/iohk-devops/followers\",\"following_url\":\"https://api.github.com/users/iohk-devops/following{/other_user}\",\"gists_url\":\"https://api.github.com/users/iohk-devops/gists{/gist_id}\",\"starred_url\":\"https://api.github.com/users/iohk-devops/starred{/owner}{/repo}\",\"subscriptions_url\":\"https://api.github.com/users/iohk-devops/subscriptions\",\"organizations_url\":\"https://api.github.com/users/iohk-devops/orgs\",\"repos_url\":\"https://api.github.com/users/iohk-devops/repos\",\"events_url\":\"https://api.github.com/users/iohk-devops/events{/privacy}\",\"received_events_url\":\"https://api.github.com/users/iohk-devops/received_events\",\"type\":\"User\",\"site_admin\":false}},{\"url\":\"https://api.github.com/repos/input-output-hk/cardano-sl/statuses/b57ede2df60658750bb7629d3946e984187a3224\",\"avatar_url\":\"https://avatars1.githubusercontent.com/u/29731078?v=4\",\"id\":6137711125,\"node_id\":\"MDEzOlN0YXR1c0NvbnRleHQ2MTM3NzExMTI1\",\"state\":\"pending\",\"description\":\"Hydra build #550173 of serokell:cardano-sl-pr-4001:required\",\"target_url\":\"https://hydra.iohk.io/build/550173\",\"context\":\"ci/hydra:serokell:cardano-sl:required\",\"created_at\":\"2019-01-25T00:48:58Z\",\"updated_at\":\"2019-01-25T00:48:58Z\",\"creator\":{\"login\":\"iohk-devops\",\"id\":29731078,\"node_id\":\"MDQ6VXNlcjI5NzMxMDc4\",\"avatar_url\":\"https://avatars1.githubusercontent.com/u/29731078?v=4\",\"gravatar_id\":\"\",\"url\":\"https://api.github.com/users/iohk-devops\",\"html_url\":\"https://github.com/iohk-devops\",\"followers_url\":\"https://api.github.com/users/iohk-devops/followers\",\"following_url\":\"https://api.github.com/users/iohk-devops/following{/other_user}\",\"gists_url\":\"https://api.github.com/users/iohk-devops/gists{/gist_id}\",\"starred_url\":\"https://api.github.com/users/iohk-devops/starred{/owner}{/repo}\",\"subscriptions_url\":\"https://api.github.com/users/iohk-devops/subscriptions\",\"organizations_url\":\"https://api.github.com/users/iohk-devops/orgs\",\"repos_url\":\"https://api.github.com/users/iohk-devops/repos\",\"events_url\":\"https://api.github.com/users/iohk-devops/events{/privacy}\",\"received_events_url\":\"https://api.github.com/users/iohk-devops/received_events\",\"type\":\"User\",\"site_admin\":false}}]"

hydraSpec :: Spec
hydraSpec = do
  describe "Hydra API parsing" $ do
    let build = decode hydraBuildJSON

    it "parses build status" $
      hydraBuildStatus <$> build `shouldBe` Just 0

    it "parses the job filename" $
      map (fmap bpName) . M.toList . hydraBuildProducts <$> build `shouldBe` Just [("1", "CardanoSL.zip")]

    it "makes the correct download url" $ do
      let findUrl = findHydraBuildProductURL "CardanoSL.zip" (GH.URL hydraBuildURL) . hydraBuildProducts
      let Just mUrl = findUrl <$> build
      mUrl `shouldBe` Just (hydraBuildURL <> "/download/1/CardanoSL.zip")

  describe "GitHub status API parsing" $ do
    let statuses = decode githubStatusJSON

    it "finds the build URL from a successful github status blob" $
      hydraURL' "required" <$> statuses `shouldBe` Just (Just (GH.URL "https://hydra.iohk.io/build/550173"))
