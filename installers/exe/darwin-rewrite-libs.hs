import           Universum hiding (FilePath)
import           Turtle
import qualified Filesystem.Path.CurrentOS as FP

import           RewriteLibs

main :: IO ()
main = do
  (outDir, libs) <- options "macOS dynamic libary path rewriter" rewriteOptionsParser
  void $ chain (FP.encodeString outDir) (map (format fp) libs)

rewriteOptionsParser :: Parser (FilePath, [FilePath])
rewriteOptionsParser = (,)
  <$> optPath "output-dir" 'o' "Output directory"
  <*> some (argPath "LIB" "Libraries to process")
