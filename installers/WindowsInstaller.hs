module WindowsInstaller where

import qualified Data.List          as L
import           Data.Maybe         (fromJust, fromMaybe)
import           Data.Monoid        ((<>))
import           Data.Text          (pack)
import           Development.NSIS
import           System.Directory   (renameFile)
import           System.Environment (lookupEnv)
import           Turtle             (echo, proc, procs)

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

-- See INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
writeUninstallerNSIS :: IO ()
writeUninstallerNSIS = do
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  writeFile "uninstaller.nsi" $ nsis $ do
    injectGlobalLiteral $ "OutFile \"" <> tempDir <> "\\tempinstaller.exe\""
    injectGlobalLiteral "SetCompress off"
    _ <- section "" [Required] $ do
      injectLiteral $ "WriteUninstaller \"" <> tempDir <> "\\uninstall.exe\""

    uninstall $ do
      -- Remove registry keys
      deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus"
      deleteRegKey HKLM "Software/Daedalus"
      rmdir [Recursive,RebootOK] "$INSTDIR"
      delete [] "$SMPROGRAMS/Daedalus/*.*"
      delete [] "$DESKTOP\\Daedalus.lnk"
      -- Note: we leave user data alone

-- See non-INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
signUninstaller :: IO ()
signUninstaller = do
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["uninstaller.nsi"] mempty
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  writeFile "runtempinstaller.bat" $ tempDir <> "\\tempinstaller.exe /S"
  _ <- proc "runtempinstaller.bat" [] mempty
  echo . pack $ "TODO: Sign " <> tempDir <> "\\uninstall.exe"

writeInstallerNSIS :: IO ()
writeInstallerNSIS = do
  version <- fmap (fromMaybe "dev") $ lookupEnv "APPVEYOR_BUILD_VERSION"
  ipdhtRaw <- readFile "data\\ip-dht-mappings"
  let ds = daedalusShortcut $ lines ipdhtRaw
  writeFile "version.txt" version
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  writeFile "daedalus.nsi" $ nsis $ do
    _ <- constantStr "Version" (str version)
    name "Daedalus $Version"                  -- The name of the installer
    outFile "daedalus-win64-$Version-installer.exe"           -- Where to produce the installer
    injectGlobalLiteral $ "VIProductVersion " <> version
    -- see ndmitchell/nsis#10 and https://github.com/jmitchell/nsis/tree/feature/escape-hatch
    {- unicode True -}
    injectGlobalLiteral "Unicode true"
    installDir "$PROGRAMFILES64\\Daedalus"   -- The default installation directory
    -- installDirRegKey HKLM "Software/Daedalus" "Install_Dir"
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
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "InstallLocation" "$PROGRAMFILES64\\Daedalus"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "Publisher" "Eureka Solutions LLC"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "ProductVersion" (str version)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayName" "Daedalus"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoRepair" 1
        file [] $ (str $ tempDir <> "\\uninstall.exe")

    _ <- section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/Daedalus"
        createShortcut "$SMPROGRAMS/Daedalus/Uninstall Daedalus.lnk"
          [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/Daedalus/Daedalus.lnk" ds

    return ()

main :: IO ()
main = do
  echo "Writing uninstaller.nsi"
  writeUninstallerNSIS
  signUninstaller

  echo "Writing daedalus.nsi"
  writeInstallerNSIS

  echo "Generating NSIS installer daedalus-win64-installer.exe"
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
