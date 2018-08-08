{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase, RecordWildCards, DeriveDataTypeable, DeriveGeneric #-}

module AppVeyor (downloadCardanoSL, AppVeyorError(..)) where

import Universum hiding (get)
import Data.Aeson
import GitHub hiding (URL)
import GitHub.Endpoints.Repos.Statuses
import qualified GitHub as GH
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import Network.URI
import Data.Maybe (mapMaybe)
import Network.Wreq
import Data.Aeson.Lens
import Lens.Micro
import Turtle.Format (printf, (%), d, s, w, Format, makeFormat, format)
import System.Environment (lookupEnv)

downloadCardanoSL :: FilePath -> IO L8.ByteString
downloadCardanoSL srcJson = do
  src <- getCardanoRev srcJson
  maybeZip <- downloadCardanoSLS3 src
  case maybeZip of
    Just zip -> return zip
    Nothing -> downloadCardanoSLArtifact src

-- | Gets CardanoSL.zip corresponding to the src json revision from AppVeyor CI
downloadCardanoSLArtifact :: CardanoSource -> IO L8.ByteString
downloadCardanoSLArtifact src@CardanoSource{..} = do
  printf ("Fetching GitHub CI status for commit "%n%" of "%n%"/"%n%"\n") srcRev srcOwner srcRepo
  buildURL <- appVeyorURL src
  printf ("Build URL is "%s%"\n") (getUrl buildURL)
  jobs <- appVeyorJobs buildURL
  printf ("Build has "%d%" job(s), status "%ss%".\n") (length jobs)
    (map (show . jobStatus) jobs)
  arts <- appVeyorArtifacts $ map jobId jobs
  printf ("Artifacts are: "%ss%"\n") (map fst arts)
  case findAppVeyorArtifact "CardanoSL.zip" arts of
    Just bs -> printf "Done\n" >> pure bs
    Nothing -> throwM (MissingArtifactsError $ getUrl buildURL)

-- | Gets CardanoSL.zip corresponding to the src json revision from S3
downloadCardanoSLS3 :: CardanoSource -> IO (Maybe L8.ByteString)
downloadCardanoSLS3 CardanoSource{..} = do
  let
    url = T.unpack $ format ("https://s3-ap-northeast-1.amazonaws.com/appveyor-ci-deploy/" % n % "/" % n % ".zip") srcRepo srcRev
    opts = set Network.Wreq.checkResponse (Just $ \_ _ -> return ()) defaults
  printf ("Downloading "%w%" ... ") url
  r <- getWith opts $ toString url
  case r ^. responseStatus . statusCode of
    200 -> do
      printf "Successfully downloaded CardanoSL.zip from S3\n"
      pure ( Just $ r ^. responseBody)
    403 -> throwM (S3Error 403 )
    500 -> throwM (S3Error 500 )
    404 -> pure Nothing
    status -> do
      printf ( "Got Invalid return code" % d ) status
      pure Nothing


n :: Format r (Name a -> r)
n = makeFormat untagName

ss :: Format r ([Text] -> r)
ss = makeFormat (T.intercalate ",")

----------------------------------------------------------------------------

-- | Given a build status URL, query AppVeyor for job IDs.
appVeyorJobs :: GH.URL -> IO [Job]
appVeyorJobs buildURL = do
  let Just req = buildAPI buildURL
  r <- asValue =<< get (show req)
  pure (r ^.. responseBody . buildJobs)

-- | Download all artifacts for all job ids.
appVeyorArtifacts :: [Text] -> IO [(Text, L8.ByteString)]
appVeyorArtifacts = fmap concat . mapM artifactForJobId

-- | Filter the artifact we are interested in.
findAppVeyorArtifact :: Text -> [(Text, L8.ByteString)] -> Maybe L8.ByteString
findAppVeyorArtifact name arts = safeHead [bs | (name', bs) <- arts, name' == name]

-- | Downloads all artifacts for a job.
-- Returns the filename and contents.
artifactForJobId :: Text -> IO [(Text, L8.ByteString)]
artifactForJobId jobId = do
  let req = jobArtifactsURI jobId
  r <- asValue =<< get req
  forM (r ^.. responseBody . artifactFilenames) $ \filename -> do
    let req' = req ++ ('/':T.unpack filename)
    printf ("Downloading "%w%" ... ") req'
    r' <- get req'
    printf "done\n"
    pure (filename, r' ^. responseBody)

-- | Looks into AppVeyor API build info json to get job IDs
buildJobs :: Monoid r => Getting r Value Job
buildJobs = key "build" . key "jobs" . _Array . traverse . _JSON

-- | Looks into AppVeyor API job artifacts json to get the filenames
artifactFilenames :: Monoid r => Getting r Value Text
artifactFilenames = _Array . traverse . key "fileName" . _String

-- | The AppVeyor API URL of an build, given its web URL.
buildAPI :: GH.URL -> Maybe URI
buildAPI = parseAbsoluteURI . T.unpack . T.replace "/project/" "/api/projects/" . getUrl

-- | The AppVeyor API URL to list artifacts belonging to a job.
jobArtifactsURI :: Text -> String
jobArtifactsURI jobId = "https://ci.appveyor.com/api/buildJobs/" <> T.unpack jobId <> "/artifacts"

-- | Use GitHub API to find AppVeyor build status
appVeyorURL :: CardanoSource -> IO GH.URL
appVeyorURL src = fmap collect <$> statusFor' src >>= \case
  Right (Just u) -> pure u
  Right Nothing  -> throwM $ StatusMissingError src
  Left err       -> throwM $ GitHubStatusError src err
  where
    collect = safeHead . mapMaybe statusTargetUrl . filter isAppVeyor . toList
    isAppVeyor st = statusContext st == Just "continuous-integration/appveyor/branch"

statusFor' :: CardanoSource -> IO (Either GH.Error (Vector GH.Status))
statusFor' CardanoSource{..} = do
  auth <- authFromEnv
  statusesFor auth srcOwner srcRepo srcRev

authFromEnv :: IO GH.Auth
authFromEnv = maybe noAuth (OAuth . S8.pack) <$> lookupEnv "GITHUB_OAUTH_TOKEN"
  where noAuth = BasicAuth "" ""

data JobStatus = JobSuccess | JobFailed | JobRunning
               | JobUnknown Text deriving (Show, Eq, Generic)

data Job = Job { jobId :: Text
               , jobStatus :: JobStatus
               } deriving (Show, Eq, Generic)

instance FromJSON JobStatus where
  parseJSON = withText "status" $ \case
    "success" -> pure JobSuccess
    "failed" -> pure JobFailed
    "running" -> pure JobRunning
    other -> pure $ JobUnknown other

instance FromJSON Job where
  parseJSON = withObject "Job" $ \o -> Job <$> o .: "jobId" <*> o .: "status"

instance ToJSON JobStatus
instance ToJSON Job -- Only required for _JSON prism

----------------------------------------------------------------------------

-- | Load commit hash and GitHub repo name from cardano-sl-src.json.
getCardanoRev :: FilePath -> IO CardanoSource
getCardanoRev src = eitherDecode <$> L8.readFile src >>= \case
  Right rev -> pure rev
  Left err -> throwM (SourceJSONDecodeError err)

data CardanoSource = CardanoSource
  { srcOwner :: Name Owner
  , srcRepo  :: Name Repo
  , srcRev   :: Name Commit
  } deriving (Show, Eq)

instance FromJSON CardanoSource where
  parseJSON = withObject "fetchGit source" $ \o -> do
    (owner, repo) <- parseGitHubURL <$> o .: "url"
    CardanoSource owner repo <$> o .: "rev"

instance FromJSON URI where
  parseJSON = withText "Absolute URI" $ \u -> case parseAbsoluteURI (T.unpack u) of
    Just uri -> pure uri
    Nothing -> fail "Could not parse absolute URI"

-- | Gets owner/repo from gitcom.com URL
parseGitHubURL :: URI -> (Name Owner, Name Repo)
parseGitHubURL uri = (fromString owner, fromString (drop 1 repo))
  where (owner, repo) = break (== '/') (drop 1 $ uriPath uri)

----------------------------------------------------------------------------

data AppVeyorError = SourceJSONDecodeError String
                   | GitHubStatusError CardanoSource GH.Error
                   | StatusMissingError CardanoSource
                   | MissingArtifactsError Text
                   | S3Error Int
                   deriving (Show, Typeable)

instance Exception AppVeyorError
