module WindowsInstaller where

import           Data.Maybe (fromMaybe)
import qualified Data.List as L
import           Development.NSIS
import           System.Environment (lookupEnv)
import           Turtle (echo, procs)

shortcutParameters :: String
shortcutParameters = L.intercalate " " $
  [ "--node $INSTDIR\\cardano-node.exe"
  , "--wallet $INSTDIR\\Daedalus.exe"
  , "--updater C:/TODO" -- TODO
  , "--node-timeout 5"
  , (" -n " ++ (L.intercalate " -n " nodeArgs))
  ] 
    where
      appdata = "$APPDATA"
      nodeArgs = [
        "--listen", "0.0.0.0:12100",
        "--peer", "35.156.182.24:3000/MHdrsP-oPf7UWl0007QuXnLK5RD=",
        "--peer", "54.183.103.204:3000/MHdrsP-oPf7UWl0077QuXnLK5RD=",
        "--peer", "52.53.231.169:3000/MHdrsP-oPf7UWl0127QuXnLK5RD=",
        "--peer", "35.157.41.94:3000/MHdrsP-oPf7UWl0057QuXnLK5RD=",
        "--log-config", "log-config-prod.yaml",
        "--keyfile", appdata ++ "\\Daedalus\\Secrets\\secret.key",
        "--logs-prefix", appdata ++ "\\Daedalus\\Logs",
        "--db-path", appdata ++ "\\Daedalus\\DB",
        "--wallet-db-path", appdata ++ "\\Daedalus\\Wallet",
        "--wallet"
        ]

daedalusShortcut :: [Attrib]
daedalusShortcut =
    [ Target "$INSTDIR\\cardano-launcher.exe"
    , Parameters (str shortcutParameters)
    , IconFile "$INSTDIR\\Daedalus.exe"
    , IconIndex 0
    ]

writeNSIS :: IO ()
writeNSIS = do
  version <- fmap (fromMaybe "dev") $ lookupEnv "APPVEYOR_BUILD_VERSION"
  writeFile "version.txt" version
  writeFile "daedalus.nsi" $ nsis $ do
    _ <- constantStr "Version" (str version)
    name "Daedalus $Version"                  -- The name of the installer
    outFile "daedalus-win64-$Version-installer.exe"           -- Where to produce the installer
    installDir "$PROGRAMFILES64\\Daedalus"   -- The default installation directory
    installDirRegKey HKLM "Software/Daedalus" "Install_Dir"
    requestExecutionLevel Highest     

    page Directory                   -- Pick where to install
    page InstFiles                   -- Give a progress bar while installing

    section "" [Required] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeRegStr HKLM "Software/Daedalus" "Install_Dir" "$INSTDIR"
        createDirectory "$APPDATA\\Daedalus\\DB"
        createDirectory "$APPDATA\\Daedalus\\Wallet" 
        createDirectory "$APPDATA\\Daedalus\\Logs"
        createDirectory "$APPDATA\\Daedalus\\Secrets"
        createShortcut "$DESKTOP\\Daedalus.lnk" daedalusShortcut
        file [] "cardano-node.exe"
        file [] "cardano-launcher.exe"
        file [] "log-config-prod.yaml"
        file [] "version.txt"
        file [Recursive] "dlls\\"
        file [Recursive] "..\\release\\win32-x64\\Daedalus-win32-x64\\"

        -- Uninstaller
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayName" "Daedalus"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoRepair" 1
        writeUninstaller "uninstall.exe"

    section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/Daedalus"
        createShortcut "$SMPROGRAMS/Daedalus/Uninstall Daedalus.lnk" 
          [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/Daedalus/Daedalus.lnk" daedalusShortcut

    uninstall $ do
      -- Remove registry keys
      deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus"
      deleteRegKey HKLM "Software/Daedalus"
      rmdir [Recursive,RebootOK] "$INSTDIR"
      delete [] "$SMPROGRAMS/Daedalus/*.*"
      delete [] "$DESKTOP\\Daedalus.lnk"
      -- Note: we leave user data alone
   
main :: IO ()
main = do
  echo "Writing daedalus.nsi"
  writeNSIS
  echo "Generating NSIS installer daedalus-win64-installer.exe"
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
