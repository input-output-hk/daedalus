module WindowsInstaller where

import qualified Data.List          as L
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))
import           Development.NSIS
import           System.Directory   (renameFile)
import           System.Environment (lookupEnv)
import           Turtle             (echo, procs)

shortcutParameters :: [String] -> String
shortcutParameters ipdht = L.intercalate " " $
    [ "--node \"%PROGRAMFILES%\\Daedalus\\cardano-node.exe\""
    , "--node-log-path", "\"%APPDATA%\\Daedalus\\Logs\\cardano-node.log\""
    , "--node-log-config", "log-config-prod.yaml" 
    , "--wallet \"%PROGRAMFILES%\\Daedalus\\Daedalus.exe\""
    , "--updater \"" <> installerPath <> "\""
    , "--node-timeout 5"
    , (" -n " ++ (L.intercalate " -n " nodeArgs))
    ]
  where
    installerPath = "%APPDATA%\\Daedalus\\Installer.exe"
    nodeArgs = [
      "--listen", "0.0.0.0:12100",
      "--keyfile", "\"%APPDATA%\\Daedalus\\Secrets\\secret.key\"",
      "--update-latest-path", "\"" <> installerPath <> "\"",
      "--logs-prefix", "\"%APPDATA%\\Daedalus\\Logs\"",
      "--db-path", "\"%APPDATA%\\Daedalus\\DB-0.2\"",
      "--wallet-db-path", "\"%APPDATA%\\Daedalus\\Wallet-0.2\"",
      "--wallet"
      ] <> ("--peer" : (L.intersperse "--peer" ipdht))

daedalusShortcut :: [String] -> [Attrib]
daedalusShortcut ipdht =
    [ Target "$INSTDIR\\cardano-launcher.exe"
    , Parameters (str $ shortcutParameters ipdht)
    , IconFile "$INSTDIR\\Daedalus.exe"
    , StartOptions "SW_SHOWMINIMIZED"
    , IconIndex 0
    ]

writeNSIS :: IO ()
writeNSIS = do
  version <- fmap (fromMaybe "dev") $ lookupEnv "APPVEYOR_BUILD_VERSION"
  ipdhtRaw <- readFile "data\\ip-dht-mappings"
  let ds = daedalusShortcut $ lines ipdhtRaw
  writeFile "version.txt" version
  writeFile "daedalus.nsi" $ nsis $ do
    _ <- constantStr "Version" (str version)
    name "Daedalus $Version"                  -- The name of the installer
    outFile "daedalus-win64-$Version-installer.exe"           -- Where to produce the installer
    -- See enableUnicodeNSIS hack; should eventually be replaced by line below.
    {- unicode True -}
    installDir "$PROGRAMFILES64\\Daedalus"   -- The default installation directory
    installDirRegKey HKLM "Software/Daedalus" "Install_Dir"
    requestExecutionLevel Highest

    page Directory                   -- Pick where to install
    page InstFiles                   -- Give a progress bar while installing

    _ <- section "" [Required] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeRegStr HKLM "Software/Daedalus" "Install_Dir" "$INSTDIR"
        createDirectory "$APPDATA\\Daedalus\\DB-0.2"
        createDirectory "$APPDATA\\Daedalus\\Wallet-0.2"
        createDirectory "$APPDATA\\Daedalus\\Logs"
        createDirectory "$APPDATA\\Daedalus\\Secrets"
        createShortcut "$DESKTOP\\Daedalus.lnk" ds
        file [] "cardano-node.exe"
        file [] "cardano-launcher.exe"
        file [] "log-config-prod.yaml"
        file [] "version.txt"
        file [Recursive] "dlls\\"
        file [Recursive] "..\\release\\win32-x64\\Daedalus-win32-x64\\"

        -- Uninstaller
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "Publisher" "Eureka Solutions LLC"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "ProductVersion" (str version)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayName" "Daedalus"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoRepair" 1
        writeUninstaller "uninstall.exe"

    _ <- section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/Daedalus"
        createShortcut "$SMPROGRAMS/Daedalus/Uninstall Daedalus.lnk"
          [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/Daedalus/Daedalus.lnk" ds

    uninstall $ do
      -- Remove registry keys
      deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus"
      deleteRegKey HKLM "Software/Daedalus"
      rmdir [Recursive,RebootOK] "$INSTDIR"
      delete [] "$SMPROGRAMS/Daedalus/*.*"
      delete [] "$DESKTOP\\Daedalus.lnk"
      -- Note: we leave user data alone

-- TODO (jmitchell): Monitor https://github.com/ndmitchell/nsis/pull/10 and
-- clean this up once it's merged and deployed. See commented out line above in
-- writeNSIS.
enableUnicodeNSIS :: IO ()
enableUnicodeNSIS = do
  nsiScript <- readFile "daedalus.nsi"
  appendFile "new-daedalus.nsi" $ "Unicode true\n" ++ nsiScript
  renameFile "new-daedalus.nsi" "daedalus.nsi"

main :: IO ()
main = do
  echo "Writing daedalus.nsi"
  writeNSIS
  enableUnicodeNSIS
  echo "Generating NSIS installer daedalus-win64-installer.exe"
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
