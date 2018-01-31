module Main where

import Universum
import Options.Applicative
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
main = run =<< execParser opts
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
        <> progDesc "Creates a keychain and imports a signing certificate."
        <> header "load-certificate - installer signing setup script" )

run :: Command -> IO ()
run (LoadCertificate f) = setupKeyChain cfg'
  where cfg' = signingConfig { signingCertificate = f }
run DeleteCertificate = deleteKeyChain signingConfig
