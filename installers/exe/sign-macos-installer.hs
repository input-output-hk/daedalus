import           Universum hiding (FilePath)
import           Turtle

import           MacOSPackageSigning

main :: IO ()
main = do
  (inPkg, outPkg) <- options "Daedalus macOS installer signer" signingOptionsParser
  signMacOSInstaller inPkg outPkg

signingOptionsParser :: Parser (FilePath, FilePath)
signingOptionsParser = (,)
  <$> argPath "INPUT.pkg" "Unsigned package"
  <*> argPath "OUTPUT.pkg" "Destination filename for signed package"
