{-# LANGUAGE RecordWildCards #-}
module WindowsInstaller
    ( main
    ) where

import           Universum hiding (pass, writeFile)

import           Control.Monad (unless)
import qualified Data.List as L
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Development.NSIS (Attrib (IconFile, IconIndex, RebootOK, Recursive, Required, StartOptions, Target),
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
import           Turtle (ExitCode (..), echo, proc, procs, shells)
import           Turtle.Line (unsafeTextToLine)

import           Config
import           Types



daedalusShortcut :: [Attrib]
daedalusShortcut =
        [ Target "$INSTDIR\\cardano-launcher.exe"
        , IconFile "$INSTDIR\\Daedalus.exe"
        , StartOptions "SW_SHOWMINIMIZED"
        , IconIndex 0
        ]

-- See INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
writeUninstallerNSIS :: Version -> IO ()
writeUninstallerNSIS (Version fullVersion) = do
    tempDir <- fmap fromJust $ lookupEnv "TEMP"
    writeFile "uninstaller.nsi" $ nsis $ do
        _ <- constantStr "Version" (str $ unpack fullVersion)
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
signUninstaller :: Options -> IO ()
signUninstaller opts = do
    procs "C:\\Program Files (x86)\\NSIS\\makensis" ["uninstaller.nsi"] mempty
    tempDir <- fmap fromJust $ lookupEnv "TEMP"
    writeFile "runtempinstaller.bat" $ tempDir <> "\\tempinstaller.exe /S"
    _ <- proc "runtempinstaller.bat" [] mempty
    signFile opts (tempDir <> "\\uninstall.exe")

signFile :: Options -> FilePath -> IO ()
signFile Options{..} filename = do
    exists <- doesFileExist filename
    if exists then do
        case oCertPass of
            Nothing -> echo . unsafeTextToLine . toText $ "Skipping signing " <> filename <> " due to lack of password"
            Just pass -> do
                echo . unsafeTextToLine . toText $ "Signing " <> filename
                -- TODO: Double sign a file, SHA1 for vista/xp and SHA2 for windows 8 and on
                --procs "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", toText pass, "/t", "http://timestamp.comodoca.com", "/v", toText filename] mempty
                exitcode <- proc "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", toText pass, "/fd", "sha256", "/tr", "http://timestamp.comodoca.com/?td=sha256", "/td", "sha256", "/v", toText filename] mempty
                unless (exitcode == ExitSuccess) $ error "Signing failed"
    else
        error $ "Unable to sign missing file '" <> (toText filename) <> "''"

parseVersion :: Text -> [String]
parseVersion ver =
    case T.split (== '.') (toText ver) of
        v@[_, _, _, _] -> map toString v
        _              -> ["0", "0", "0", "0"]

fileSubstString :: Text -> Text -> FilePath -> FilePath -> IO ()
fileSubstString from to src dst =
    TIO.writeFile dst =<< T.replace from to <$> TIO.readFile src

writeInstallerNSIS :: Version -> Cluster -> IO ()
writeInstallerNSIS (Version fullVersion') clusterName = do
    tempDir <- fmap fromJust $ lookupEnv "TEMP"
    let fullVersion = unpack fullVersion'
        viProductVersion = L.intercalate "." $ parseVersion fullVersion'
    echo $ unsafeTextToLine $ toText $ "VIProductVersion: " <> viProductVersion

    forM_ ["ca.conf", "server.conf", "client.conf"] $
        \f-> fileSubstString "OPENSSL_MD" "sha256" f (f <> ".windows")

    writeFile "daedalus.nsi" $ nsis $ do
        _ <- constantStr "Version" (str fullVersion)
        _ <- constantStr "Cluster" (str $ unpack $ lshowText clusterName)
        name "Daedalus ($Version)"                  -- The name of the installer
        outFile "daedalus-win64-$Version-$Cluster-installer.exe"           -- Where to produce the installer
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
                file [] "launcher-config.yaml"
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
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "VersionMajor" (str . (!! 0). parseVersion $ fullVersion')
                writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/Daedalus" "VersionMinor" (str . (!! 1). parseVersion $ fullVersion')
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

main :: Options -> IO ()
main opts@Options{..}  = do
    echo "Writing version.txt"
    let fullVersion = Version $ fromVer oDaedalusVer <> ".0"
        fullName    = "daedalus-win64-" <> fromVer fullVersion <> "-" <> lshowText oCluster <> "-installer.exe"
    TIO.writeFile "version.txt" $ fromVer fullVersion

    echo "Generating configuration file:  launcher-config.yaml"
    generateConfig (Request Win64 oCluster Launcher) "./dhall" "launcher-config.yaml"
    echo "Generating configuration file:  wallet-topology.yaml"
    generateConfig (Request Win64 oCluster Topology) "./dhall" "wallet-topology.yaml"

    echo "Packaging frontend"
    shells "npm run package -- --icon installers/icons/64x64" mempty

    echo "Adding permissions manifest to cardano-launcher.exe"
    procs "C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\mt.exe" ["-manifest", "cardano-launcher.exe.manifest", "-outputresource:cardano-launcher.exe;#1"] mempty

    signFile opts "cardano-launcher.exe"
    signFile opts "cardano-node.exe"

    echo "Writing uninstaller.nsi"
    writeUninstallerNSIS fullVersion
    signUninstaller opts

    echo "Writing daedalus.nsi"
    writeInstallerNSIS fullVersion oCluster

    echo "Generating NSIS installer"
    procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
    signFile opts $ unpack fullName
