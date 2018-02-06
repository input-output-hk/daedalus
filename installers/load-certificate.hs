{-# LANGUAGE LambdaCase #-}

module Main where

import Universum
import Options.Applicative
import MacInstaller hiding (main)
import qualified System.IO as IO
import Turtle (readline, need, lineToText, Line)

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
main = run =<< execParser opts
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
        <> progDesc "Creates a keychain and imports a signing certificate."
        <> header "load-certificate - installer signing setup script" )

run :: Command -> IO ()
run (LoadCertificate f) = getPassword >>= setupKeyChain cfg'
  where cfg' = signingConfig { signingCertificate = f }
run DeleteCertificate = deleteKeyChain signingConfig

getPassword :: IO (Maybe Text)
getPassword = need "CERT_PASS" >>= \case
  Just p -> pure . Just . toText $ p
  Nothing -> do
    putStr ("Password: " :: Text)
    fmap lineToText <$> getSecret

getSecret :: IO (Maybe Line)
getSecret = bracket open close (\_ -> readline)
  where
    open = do
        b <- IO.hGetBuffering IO.stdin
        e <- IO.hGetEcho IO.stdin
        IO.hSetBuffering IO.stdin IO.NoBuffering
        IO.hSetEcho      IO.stdin False
        return (b, e)
    close (b, e) = do
        IO.hSetBuffering IO.stdin b
        IO.hSetEcho      IO.stdin e
