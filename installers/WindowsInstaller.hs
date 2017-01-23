module WindowsInstaller where

import           Development.NSIS
import           Turtle (echo, procs)

writeNSIS :: IO ()
writeNSIS = writeFile "daedalus.nsi" $ nsis $ do
    name "Daedalus"                  -- The name of the installer
    outFile "daedalus-win64-installer.exe"           -- Where to produce the installer
    installDir "$PROGRAMFILES\\Daedalus"   -- The default installation directory
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
        createShortcut "$DESKTOP\\Daedalus.lnk"
          [Target "$INSTDIR\\Daedalus.exe"
          , IconFile "$INSTDIR\\Daedalus.exe"
          , IconIndex 0
          ]
        file [] "cardano-node.exe"
        file [Recursive] "dlls\\"
        file [] "..\\log-config-prod.yaml"
        file [Recursive] "C:\\daedalus\\release\\win32-x64\\Daedalus-win32-x64\\"

        -- Uninstaller
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayName" "Daedalus"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoRepair" 1
        writeUninstaller "uninstall.exe"

    section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/Daedalus"
        createShortcut "$SMPROGRAMS/Daedalus/Uninstall.lnk" 
          [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/Daedalus/Daedalus.lnk"
          [Target "$INSTDIR/Daedalus.exe", IconFile "$INSTDIR/Daedalus.exe", IconIndex 0]

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
