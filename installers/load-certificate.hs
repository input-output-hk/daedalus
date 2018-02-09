{-# LANGUAGE LambdaCase #-}

module Main where

import Universum
import Options.Applicative
import qualified System.IO as IO
import Turtle (readline, need, lineToText, Line, ExitCode(..))

import MacInstaller hiding (main)

data Command = LoadCertificate FilePath
             | DeleteCertificate
             deriving (Show, Eq)

parseCommand :: Parser Command
parseCommand = parseLoad <|> parseDelete
  where
    parseLoad = LoadCertificate <$> strOption
      ( long "cert-file"
        <> short 'f'
        <> metavar "NAME"
        <> help "Certificate in PKCS#12 format" )
    parseDelete = flag' DeleteCertificate
      ( long "delete"
        <> short 'd'
        <> help "Remove certificate from store" )

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
run (LoadCertificate f) = getPassword >>= importCertificate cfg' f
  where cfg' = signingConfig { signingKeyChain = Just systemKeyChain }
run DeleteCertificate = deleteCertificate signingConfig

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
        return (b, e)
    close (b, e) = do
        IO.hSetBuffering IO.stdin b
        IO.hSetEcho      IO.stdin e
