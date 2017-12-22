import           Universum

import           System.Info (arch, os)
import           Turtle (echo)
import           Turtle.Line (unsafeTextToLine)

import qualified MacInstaller (main)
import qualified WindowsInstaller (main)


main :: IO ()
main = do
  echo $ unsafeTextToLine . toText $ "Generating installer for " <>  os <> "-" <> arch
  case os of
    "linux"   -> echo "No installer yet"
    "darwin"  -> MacInstaller.main
    "mingw32" -> WindowsInstaller.main
    _         -> fail "No installer available for this platform."
