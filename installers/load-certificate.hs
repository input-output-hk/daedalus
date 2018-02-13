{-# LANGUAGE LambdaCase #-}

module Main where

import Universum
import Options.Applicative
import qualified System.IO as IO
import Turtle (readline, need, lineToText, Line, ExitCode(..), proc, procs)
import qualified Data.Text as T

import MacInstaller hiding (main)

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
main = exitWith =<< run =<< execParser opts
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
        <> progDesc "Imports a signing certificate into the system keychain."
        <> header "load-certificate - installer signing setup script" )

systemKeyChain :: Text
systemKeyChain = "/Library/Keychains/System.keychain"

run :: Command -> IO ExitCode
run (Command kc (LoadCertificate f)) = do
  maybe (pure ()) (setupKeychain "travis") kc
  getPassword >>= importCertificate cfg' f
    where cfg' = signingConfig { signingKeyChain = Just (fromMaybe systemKeyChain kc) }
run (Command kc DeleteCertificate) = do
  res <- deleteCertificate signingConfig
  maybe (pure ()) deleteKeychain kc
  pure res

getPassword :: IO (Maybe Text)
getPassword = need "CERT_PASS" >>= \case
  Just p -> pure . Just . toText $ p
  Nothing -> do
    putStr ("Password: " :: Text)
    fmap (>>= lineMaybe) getSecret

lineMaybe :: Line -> Maybe Text
lineMaybe = nonEmpty . lineToText
  where
    nonEmpty "" = Nothing
    nonEmpty l = Just l

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
  procs "security" ["create-keychain", "-p", password, keychain] mempty
  procs "security" ["default-keychain", "-s", keychain] mempty
  -- avoids modal dialogue popping up on sierra
  let sierraFix = ["set-key-partition-list", "-S", "apple-tool:,apple:", "-s", "-k", password, keychain]
  void $ proc "security" sierraFix mempty
  -- disables unlock timeout
  procs "security" ["set-keychain-settings", keychain] mempty
  procs "security" ["unlock-keychain", "-p", password, keychain] mempty
  -- for informational purposes
  procs "security" ["show-keychain-info", keychain] mempty

-- | Remove our certificate's keychain from store.
deleteKeychain :: Text -> IO ()
deleteKeychain keychain = void $ proc "security" ["delete-keychain", keychain] mempty
