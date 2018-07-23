import Universum hiding (FilePath)
import Options.Applicative
import qualified System.IO as IO
import Turtle (readline, need, lineToText, Line, ExitCode(..), FilePath)
import qualified Data.Text as T

import MacOSPackageSigning

data Command = Command
  { cmdKeychain :: Maybe Text
  , cmdAction :: Action
  } deriving (Show, Eq)

data Action = LoadCertificate FilePath
            | DeleteCertificate
            deriving (Show, Eq)

parseCommand :: Parser Command
parseCommand = Command <$> optional keychain <*> parseAction
  where
    keychain = fmap T.pack . strOption $
               long "keychain"
               <> short 'k'
               <> metavar "FILE"
               <> help "Use this keychain instead of system"

parseAction :: Parser Action
parseAction = parseLoad <|> parseDelete
  where
    parseLoad = LoadCertificate <$> strOption
      ( long "cert-file"
        <> short 'f'
        <> metavar "NAME"
        <> help "Certificate in PKCS#12 format" )
    parseDelete = flag' DeleteCertificate
      ( long "delete"
        <> short 'd'
        <> help "Remove certificate from store, and keychain if specified." )

main :: IO ()
main = exitWith =<< go =<< execParser opts
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
        <> progDesc "Imports a signing certificate into the system keychain."
        <> header "load-certificate - installer signing setup script" )

systemKeyChain :: Text
systemKeyChain = "/Library/Keychains/System.keychain"

go :: Command -> IO ExitCode
go (Command kc (LoadCertificate f)) = do
  doMaybe (setupKeychain "ci") kc
  let cfg' = signingConfig { signingKeyChain = Just (fromMaybe systemKeyChain kc) }
  res <- getPassword >>= importCertificate cfg' f
  doMaybe (preventPasswordPrompts "ci") kc
  pure res
go (Command kc DeleteCertificate) = do
  res <- deleteCertificate signingConfig
  doMaybe deleteKeychain kc
  pure res

doMaybe :: (a -> IO ()) -> Maybe a -> IO ()
doMaybe = maybe (pure ())

getPassword :: IO (Maybe Text)
getPassword = need "CERT_PASS" >>= \case
  Just p -> pure . Just . toText $ p
  Nothing -> do
    putStr ("Password: " :: Text)
    fmap (>>= lineMaybe) getSecret

lineMaybe :: Line -> Maybe Text
lineMaybe = nonempty . lineToText
  where
    nonempty "" = Nothing
    nonempty l = Just l

getSecret :: IO (Maybe Line)
getSecret = bracket open close (\_ -> readline)
  where
    open = do
        IO.hFlush IO.stdout
        b <- IO.hGetBuffering IO.stdin
        e <- IO.hGetEcho IO.stdin
        IO.hSetBuffering IO.stdin IO.NoBuffering
        IO.hSetEcho      IO.stdin False
        IO.putChar '\n'
        return (b, e)
    close (b, e) = do
        IO.hSetBuffering IO.stdin b
        IO.hSetEcho      IO.stdin e

-- | Create a new keychain for non-interactive signing.
setupKeychain :: Text -> Text -> IO ()
setupKeychain password keychain = do
  run "security" ["create-keychain", "-p", password, keychain]
  run "security" ["default-keychain", "-s", keychain]
  -- disables unlock timeout
  run "security" ["set-keychain-settings", keychain]
  run "security" ["unlock-keychain", "-p", password, keychain]
  -- for informational purposes
  run "security" ["show-keychain-info", keychain]

-- | Remove our certificate's keychain from store.
deleteKeychain :: Text -> IO ()
deleteKeychain keychain = void $ run' "security" ["delete-keychain", keychain]

-- | Sets the "partition list" of all keys which can sign. Not sure
-- what this means exactly but it prevents a modal password dialogue
-- popping up on sierra.
preventPasswordPrompts :: Text -- ^ Keychain password
                       -> Text -- ^ Specific keychain
                       -> IO ()
preventPasswordPrompts password keychain = void $ run' "security" args
  where args = [ "set-key-partition-list", "-S", "apple-tool:,apple:", "-s"
               , "-k", password, keychain ]
