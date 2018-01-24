module WindowsInstaller
    ( main
    ) where

import           Universum hiding (pass, writeFile)

import           Control.Monad (unless)
import qualified Data.List as L
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Development.NSIS (Attrib (IconFile, IconIndex, OName, RebootOK, Recursive, Required, StartOptions, Target),
                                   HKEY (HKLM), Level (Highest), Page (Directory, InstFiles), abort,
                                   constant, constantStr, createDirectory, createShortcut, delete,
                                   deleteRegKey, execWait, file, iff_, installDir, installDirRegKey,
                                   name, nsis, onPagePre, outFile, page, readRegStr,
                                   requestExecutionLevel, rmdir, section, setOutPath, str,
                                   strLength, uninstall, unsafeInject, unsafeInjectGlobal,
                                   writeRegDWORD, writeRegStr, (%/=))
import           Prelude ((!!))
import           System.Directory (doesFileExist)
import           System.Environment (lookupEnv)
import           System.IO (writeFile)
import           Turtle (ExitCode (..), echo, proc, procs)
import           Turtle.Line (unsafeTextToLine)


daedalusShortcut :: [Attrib]
daedalusShortcut =
        [ Target "$INSTDIR\\cardano-launcher.exe"
        , IconFile "$INSTDIR\\Daedalus.exe"
        , StartOptions "SW_SHOWMINIMIZED"
        , IconIndex 0
        ]

-- See INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
writeUninstallerNSIS :: String -> IO ()
writeUninstallerNSIS fullVersion = do
    tempDir <- fmap fromJust $ lookupEnv "TEMP"
    writeFile "uninstaller.nsi" $ nsis $ do
        _ <- constantStr "Version" (str fullVersion)
        name "Daedalus Uninstaller $Version"
        outFile . str $ tempDir <> "\\tempinstaller.exe"
        unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""
        unsafeInjectGlobal "SetCompress off"
        _ <- section "" [Required] $ do
            unsafeInject $ "WriteUninstaller \"" <> tempDir <> "\\uninstall.exe\""

        uninstall $ do
            -- Remove registry keys
            deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus"
            deleteRegKey HKLM "Software/Daedalus"
            rmdir [Recursive,RebootOK] "$INSTDIR"
            delete [] "$SMPROGRAMS/Daedalus/*.*"
            delete [] "$DESKTOP\\Daedalus.lnk"
            mapM_ unsafeInject
                [ "liteFirewall::RemoveRule \"$INSTDIR\\cardano-node.exe\" \"Cardano Node\""
                , "Pop $0"
                , "DetailPrint \"liteFirewall::RemoveRule: $0\""
                ]
            -- Note: we leave user data alone

-- See non-INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
signUninstaller :: IO ()
signUninstaller = do
    procs "C:\\Program Files (x86)\\NSIS\\makensis" ["uninstaller.nsi"] mempty
    tempDir <- fmap fromJust $ lookupEnv "TEMP"
    writeFile "runtempinstaller.bat" $ tempDir <> "\\tempinstaller.exe /S"
    _ <- proc "runtempinstaller.bat" [] mempty
    signFile (tempDir <> "\\uninstall.exe")

signFile :: FilePath -> IO ()
signFile filename = do
    exists <- doesFileExist filename
    if exists then do
        maybePass <- lookupEnv "CERT_PASS"
        case maybePass of
            Nothing -> echo . unsafeTextToLine . toText $ "Skipping signing " <> filename <> " due to lack of password"
            Just pass -> do
                echo . unsafeTextToLine . toText $ "Signing " <> filename
                -- TODO: Double sign a file, SHA1 for vista/xp and SHA2 for windows 8 and on
                --procs "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", toText pass, "/t", "http://timestamp.comodoca.com", "/v", toText filename] mempty
                exitcode <- proc "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", toText pass, "/fd", "sha256", "/tr", "http://timestamp.comodoca.com/?td=sha256", "/td", "sha256", "/v", toText filename] mempty
                unless (exitcode == ExitSuccess) $ error "Signing failed"
    else
        error $ "Unable to sign missing file '" <> (toText filename) <> "''"

parseVersion :: String -> [String]
parseVersion ver =
    case T.split (== '.') (toText ver) of
        v@[_, _, _, _] -> map toString v
        _              -> ["0", "0", "0", "0"]

fileSubstString :: Text -> Text -> FilePath -> FilePath -> IO ()
fileSubstString from to src dst =
    TIO.writeFile dst =<< T.replace from to <$> TIO.readFile src

writeInstallerNSIS :: String -> IO ()
writeInstallerNSIS fullVersion = do
    tempDir <- fmap fromJust $ lookupEnv "TEMP"
    let viProductVersion = L.intercalate "." $ parseVersion fullVersion
    echo $ unsafeTextToLine $ toText $ "VIProductVersion: " <> viProductVersion

    forM_ ["ca.conf", "server.conf", "client.conf"] $
        \f-> fileSubstString "OPENSSL_MD" "sha256" f (f <> ".windows")

    writeFile "daedalus.nsi" $ nsis $ do
        _ <- constantStr "Version" (str fullVersion)
        name "Daedalus ($Version)"                  -- The name of the installer
        outFile "daedalus-win64-$Version-installer.exe"           -- Where to produce the installer
        unsafeInjectGlobal $ "!define MUI_ICON \"icons\\64x64.ico\""
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE"
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_BITMAP \"icons\\installBanner.bmp\""
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_RIGHT"
        unsafeInjectGlobal $ "VIProductVersion " <> viProductVersion
        unsafeInjectGlobal $ "VIAddVersionKey \"ProductVersion\" " <> fullVersion
        unsafeInjectGlobal "Unicode true"
        requestExecutionLevel Highest
        unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""

        installDir "$PROGRAMFILES64\\Daedalus"                   -- Default installation directory...
        installDirRegKey HKLM "Software/Daedalus" "Install_Dir"  -- ...except when already installed.

        page Directory                   -- Pick where to install
        _ <- constant "INSTALLEDAT" $ readRegStr HKLM "Software/Daedalus" "Install_Dir"
        onPagePre Directory (iff_ (strLength "$INSTALLEDAT" %/= 0) $ abort "")

        page InstFiles                   -- Give a progress bar while installing

        _ <- section "" [Required] $ do
                setOutPath "$INSTDIR"        -- Where to install files in this section
                writeRegStr HKLM "Software/Daedalus" "Install_Dir" "$INSTDIR" -- Used by launcher batch script
                createDirectory "$APPDATA\\Daedalus\\Secrets-1.0"
                createDirectory "$APPDATA\\Daedalus\\Logs"
                createDirectory "$APPDATA\\Daedalus\\Logs\\pub"
                createShortcut "$DESKTOP\\Daedalus.lnk" daedalusShortcut
                file [] "cardano-node.exe"
                file [] "cardano-launcher.exe"
                file [] "log-config-prod.yaml"
                file [] "version.txt"
                file [] "build-certificates-win64.bat"
                file [] "ca.conf.windows"
                file [] "server.conf.windows"
                file [] "client.conf.windows"
                file [] "wallet-topology.yaml"
                file [] "configuration.yaml"
                file [] "*genesis*.json"
                file [OName (str "launcher-config.yaml")] "launcher-config-windows.yaml"
                file [Recursive] "dlls\\"
                file [Recursive] "libressl\\"
                file [Recursive] "..\\release\\win32-x64\\Daedalus-win32-x64\\"

                mapM_ unsafeInject
                    [ "liteFirewall::AddRule \"$INSTDIR\\cardano-node.exe\" \"Cardano Node\""
                    , "Pop $0"
                    , "DetailPrint \"liteFirewall::AddRule: $0\""
                    ]

                execWait "build-certificates-win64.bat \"$INSTDIR\" >\"%APPDATA%\\Daedalus\\Logs\\build-certificates.log\" 2>&1"

                -- Uninstaller
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "InstallLocation" "$INSTDIR\\Daedalus"
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "Publisher" "IOHK"
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "ProductVersion" (str fullVersion)
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "VersionMajor" (str . (!! 0). parseVersion $ fullVersion)
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "VersionMinor" (str . (!! 1). parseVersion $ fullVersion)
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayName" "Daedalus"
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "DisplayVersion" (str fullVersion)
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "UninstallString" "\"$INSTDIR/uninstall.exe\""
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "QuietUninstallString" "\"$INSTDIR/uninstall.exe\" /S"
                writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoModify" 1
                writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "NoRepair" 1
                file [] $ (str $ tempDir <> "\\uninstall.exe")

        _ <- section "Start Menu Shortcuts" [] $ do
                createDirectory "$SMPROGRAMS/Daedalus"
                createShortcut "$SMPROGRAMS/Daedalus/Uninstall Daedalus.lnk"
                    [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
                createShortcut "$SMPROGRAMS/Daedalus/Daedalus.lnk" daedalusShortcut
        return ()

main :: IO ()
main = do
    echo "Writing version.txt"
    version <- fmap (fromMaybe "dev") $ lookupEnv "APPVEYOR_BUILD_VERSION"
    let fullVersion = version <> ".0"
    writeFile "version.txt" fullVersion

    echo "Adding permissions manifest to cardano-launcher.exe"
    procs "C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\mt.exe" ["-manifest", "cardano-launcher.exe.manifest", "-outputresource:cardano-launcher.exe;#1"] mempty

    signFile "cardano-launcher.exe"
    signFile "cardano-node.exe"

    echo "Writing uninstaller.nsi"
    writeUninstallerNSIS fullVersion
    signUninstaller

    echo "Writing daedalus.nsi"
    writeInstallerNSIS fullVersion

    echo "Generating NSIS installer daedalus-win64-installer.exe"
    procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
    signFile ("daedalus-win64-" <> fullVersion <> "-installer.exe")
