{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module WindowsInstaller
    ( main
    , writeInstallerNSIS
    , writeUninstallerNSIS
    ) where

import           Universum hiding (pass, writeFile, stdout, FilePath, die, view)

import qualified Data.List as L
import qualified Data.Text as T
import           Data.Yaml                 (decodeFileThrow)
import           Development.NSIS (Attrib (IconFile, IconIndex, RebootOK, Recursive, Required, StartOptions, Target),
                                   HKEY (HKLM), Level (Highest), Page (Directory, InstFiles), abort,
                                   constant, constantStr, createDirectory, createShortcut, delete,
                                   deleteRegKey, file, iff_, installDir, installDirRegKey,
                                   name, nsis, onPagePre, onError, outFile, page, readRegStr,
                                   requestExecutionLevel, rmdir, section, setOutPath, str,
                                   strLength, uninstall, unsafeInject, unsafeInjectGlobal,
                                   loadLanguage, sleep, (@=), detailPrint, (%<), (%&&),
                                   not_, mutableInt_, mutable_, while, false, true, strShow, (&),
                                   writeRegDWORD, writeRegStr, (%/=), fileExists)
import           Prelude ((!!))
import qualified System.IO as IO
import           Filesystem.Path (FilePath, (</>))
import           Filesystem.Path.CurrentOS (encodeString, fromText)
import           Turtle (ExitCode (..), echo, proc, procs, shells, testfile, export, format, printf, fp, w, s, (%), need, writeTextFile, die, cp, rm)

import           Config
import           Types
import           Util



desktopShortcut :: Text -> [Attrib]
desktopShortcut installDir =
        [ Target "$INSTDIR\\cardano-launcher.exe"
        , IconFile $ fromString $ T.unpack $ "$INSTDIR\\" <> installDir <> ".exe"
        , StartOptions "SW_SHOWMINIMIZED"
        , IconIndex 0
        ]

-- See INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
writeUninstallerNSIS :: Version -> InstallerConfig -> IO ()
writeUninstallerNSIS (Version fullVersion) installerConfig =
    IO.writeFile "uninstaller.nsi" $ nsis $ do
        _ <- constantStr "Version" (str $ T.unpack fullVersion)
        _ <- constantStr "InstallDir" (str $ T.unpack $ installDirectory installerConfig)
        _ <- constantStr "SpacedName" (str $ T.unpack $ spacedName installerConfig)
        unsafeInjectGlobal "Unicode true"

        loadLanguage "English"
        loadLanguage "Japanese"

        --mapM_ unsafeInjectGlobal
        --  [ "LangString UninstallName ${LANG_ENGLISH} \"Uninstaller\""
        --  , "LangString UninstallName ${LANG_JAPANESE} \"アンインストーラー\""
        --  ]

        name "$SpacedName Uninstaller $Version"
        -- TODO, the nsis library doesn't support translation vars
        -- name "$InstallDir $(UninstallName) $Version"
        --unsafeInjectGlobal $ T.unpack ( "Name \"" <> (installDirectory installerConfig) <> " $(UninstallName) " <> (fullVersion) <> "\"")
        outFile "tempinstaller.exe"
        unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""
        unsafeInjectGlobal "SetCompress off"

        _ <- section "" [Required] $ do
            unsafeInject . T.unpack $ format ("WriteUninstaller \""%fp%"\"") ("c:\\uninstall.exe")

        uninstall $ do
            -- Remove registry keys
            deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/$SpacedName"
            deleteRegKey HKLM "Software/$SpacedName"
            rmdir [Recursive,RebootOK] "$INSTDIR"
            delete [] "$SMPROGRAMS/$SpacedName/*.*"
            delete [] "$DESKTOP\\$SpacedName.lnk"
            mapM_ unsafeInject
                [ "liteFirewall::RemoveRule \"$INSTDIR\\cardano-node.exe\" \"Cardano Node\""
                , "Pop $0"
                , "DetailPrint \"liteFirewall::RemoveRule: $0\""
                ]
            -- Note: we leave user data alone

-- See non-INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
signUninstaller :: Options -> IO SigningResult
signUninstaller opts = do
    rawnsi <- readFile "uninstaller.nsi"
    putStr rawnsi
    IO.hFlush IO.stdout

    procs "C:\\Program Files (x86)\\NSIS\\makensis" ["uninstaller.nsi"] mempty
    tempDir <- getTempDir
    writeTextFile "runtempinstaller.bat" $ format (fp%" /S") (tempDir </> "tempinstaller.exe")
    -- in order to sign the uninstaller, we must first create a dummy nsis script that generates a stand-alone uninstaller at "install time"
    -- then "install" that dummy on the CI system, to create the uninstaller
    void $ proc "runtempinstaller.bat" [] mempty
    result <- signFile opts ("c:/uninstall.exe")
    tempDir <- getTempDir
    cp "c:/uninstall.exe" (tempDir </> "uninstall.exe")
    pure result

signFile :: Options -> FilePath -> IO SigningResult
signFile Options{..} filename = do
    exists   <- testfile filename
    mCertPass <- need "CERT_PASS"
    case (exists, mCertPass) of
      (True, Just certPass) -> do
        printf ("Signing "%fp%"\n") filename
        -- TODO: Double sign a file, SHA1 for vista/xp and SHA2 for windows 8 and on
        -- procs "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", toText pass, "/t", "http://timestamp.comodoca.com", "/v", toText filename] mempty
        exitcode <- proc "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", toText certPass, "/fd", "sha256", "/tr", "http://timestamp.comodoca.com/?td=sha256", "/td", "sha256", "/v", tt filename] mempty
        unless (exitcode == ExitSuccess) $ die "Signing failed"
        pure SignedOK
      (False, _) ->
        die $ format ("Unable to sign missing file '"%fp%"'\n") filename
      (_, Nothing) -> do
        echo "Not signing: CERT_PASS not specified."
        pure NotSigned

parseVersion :: Text -> [String]
parseVersion ver =
    case T.split (== '.') (toText ver) of
        v@[_, _, _, _] -> map toString v
        _              -> ["0", "0", "0", "0"]

writeInstallerNSIS :: FilePath -> Version -> InstallerConfig -> Options -> Cluster -> IO ()
writeInstallerNSIS outName (Version fullVersion') InstallerConfig{installDirectory,uglyName,spacedName} Options{oBackend} clusterName = do
    let fullVersion = T.unpack fullVersion'
        viProductVersion = L.intercalate "." $ parseVersion fullVersion'
    printf ("VIProductVersion: "%w%"\n") viProductVersion

    IO.writeFile (T.unpack uglyName ++ ".nsi") $ nsis $ do
        _ <- constantStr "Version" (str fullVersion)
        _ <- constantStr "Cluster" (str $ lshow clusterName)
        _ <- constantStr "InstallDir" (str $ T.unpack installDirectory)
        _ <- constantStr "SpacedName" (str $ T.unpack spacedName)
        name "$SpacedName ($Version)"                  -- The name of the installer
        outFile $ str $ encodeString outName        -- Where to produce the installer
        unsafeInjectGlobal $ "!define MUI_ICON \"icons\\" ++ lshow clusterName ++ "\\" ++ lshow clusterName ++ ".ico\""
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE"
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_BITMAP \"icons\\installBanner.bmp\""
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_RIGHT"
        unsafeInjectGlobal $ "!include WinVer.nsh"
        unsafeInjectGlobal $ "VIProductVersion " <> viProductVersion
        unsafeInjectGlobal $ "VIAddVersionKey \"ProductVersion\" " <> fullVersion
        unsafeInjectGlobal "Unicode true"
        requestExecutionLevel Highest
        unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""

        installDir "$PROGRAMFILES64\\$SpacedName"                   -- Default installation directory...
        installDirRegKey HKLM "Software/$SpacedName" "Install_Dir"  -- ...except when already installed.

        loadLanguage "English"
        loadLanguage "Japanese"
        mapM_ unsafeInjectGlobal
          [ "LangString AlreadyRunning ${LANG_ENGLISH} \"is running. It needs to be fully shut down before running the installer!\""
          , "LangString AlreadyRunning ${LANG_JAPANESE} \"が起動中です。 インストーラーを実行する前に完全にシャットダウンする必要があります！\""
          , "LangString TooOld ${LANG_ENGLISH} \"This version of Windows is not supported. Windows 8.1 or above required.\""
          , "LangString TooOld ${LANG_JAPANESE} \"このWindowsバージョンはサポートされていません。Windows 8.1以降が必要です。\""
          ]

        mapM_ unsafeInject [
            "${IfNot} ${AtLeastWin8.1}"
          , "  MessageBox MB_OK \"$(TooOld)\""
          , "  Quit"
          , "${EndIf}"
          ]

        page Directory                   -- Pick where to install
        _ <- constant "INSTALLEDAT" $ readRegStr HKLM "Software/$SpacedName" "Install_Dir"
        onPagePre Directory (iff_ (strLength "$INSTALLEDAT" %/= 0) $ abort "")

        page InstFiles                   -- Give a progress bar while installing

        _ <- section "" [Required] $ do
                setOutPath "$INSTDIR"        -- Where to install files in this section
                unsafeInject "AllowSkipFiles off"
                writeRegStr HKLM "Software/$SpacedName" "Install_Dir" "$INSTDIR" -- Used by launcher batch script
                createDirectory "$APPDATA\\$InstallDir\\Secrets-1.0"
                createDirectory "$APPDATA\\$InstallDir\\Logs"
                createDirectory "$APPDATA\\$InstallDir\\Logs\\pub"

                -- XXX: sometimes during auto-update, it takes longer for the app to exit,
                -- and cardano-launcher.exe’s lockfile to be unlocked (deletable), so
                -- let’s loop waiting for this to happen:
                let waitSeconds = 30
                lockfileCounter <- mutableInt_ 0
                lockfileDeleted <- mutable_ false
                while ((lockfileCounter %< waitSeconds) %&& (not_ lockfileDeleted)) $ do
                    detailPrint (
                        "Checking if "
                        Development.NSIS.& str (T.unpack spacedName)
                        Development.NSIS.& " is not running ("
                        Development.NSIS.& strShow (lockfileCounter + 1)
                        Development.NSIS.& "/"
                        Development.NSIS.& strShow waitSeconds
                        Development.NSIS.& ")..."
                        )
                    lockfileDeleted @= true
                    onError (delete [] (str $ "$APPDATA\\$InstallDir\\" ++ T.unpack uglyName ++ "_lockfile")) $ do
                        lockfileDeleted @= false
                    iff_ (not_ lockfileDeleted) $ do
                        sleep 1000 -- milliseconds
                    lockfileCounter @= lockfileCounter + 1
                iff_ (not_ (lockfileDeleted)) $ do
                    unsafeInject $ T.unpack $ "Abort \"" <> installDirectory <> " $(AlreadyRunning)\""

                iff_ (fileExists "$INSTDIR") $ do
                  detailPrint "Removing previously installed version"
                  rmdir [Recursive] "$INSTDIR"

                iff_ (fileExists "$APPDATA\\$InstallDir\\Wallet-1.0\\open\\*.*") $
                    rmdir [] "$APPDATA\\$InstallDir\\Wallet-1.0\\open"
                case oBackend of
                  Cardano _ -> do
                    file [] "cardano-node.exe"
                    file [] "cardano-wallet.exe"
                    file [] "cardano-address.exe"
                    file [] "cardano-cli.exe"
                    file [] "config.yaml"
                    file [] "topology.yaml"
                    file [] "genesis.json"
                    when (clusterName /= Selfnode) $ do
                      file [] "genesis-byron.json"
                      file [] "genesis-shelley.json"
                      file [] "genesis-alonzo.json"
                    file [] "libsodium-23.dll"
                    file [] "libsecp256k1-1.dll"
                    file [] "libsecp256k1-0.dll"
                    file [] "libssl-3-x64.dll"
                    file [] "libcrypto-3-x64.dll"
                    when (clusterName == Selfnode) $ do
                      file [] "signing.key"
                      file [] "delegation.cert"
                      file [] "local-cluster.exe"
                      file [] "libgmpxx-4.dll"
                      file [] "libwinpthread-1.dll"
                      file [] "mock-token-metadata-server.exe"
                      file [Recursive] "test\\"
                      file [] "token-metadata.json"
                file [] "cardano-launcher.exe"
                file [] "libffi-8.dll"
                file [] "libgmp-10.dll"
                file [] "libstdc++-6.dll"
                file [] "mcfgthread-12.dll"
                file [] "libgcc_s_seh-1.dll"
                --file [] "cardano-x509-certificates.exe"
                --file [] "log-config-prod.yaml"
                --file [] "wallet-topology.yaml"
                --file [] "configuration.yaml"
                --file [] "*genesis*.json"
                file [] "launcher-config.yaml"
                file [Recursive] "..\\release\\win32-x64\\$SpacedName-win32-x64\\"

                mapM_ unsafeInject
                    [ "liteFirewall::AddRule \"$INSTDIR\\cardano-node.exe\" \"Cardano Node\""
                    , "Pop $0"
                    , "DetailPrint \"liteFirewall::AddRule: $0\""
                    ]

                createShortcut "$DESKTOP\\$SpacedName.lnk" (desktopShortcut spacedName)

                -- Uninstaller
                let
                    uninstallKey = "Software/Microsoft/Windows/CurrentVersion/Uninstall/$SpacedName"
                do
                    writeRegStr HKLM uninstallKey "InstallLocation" "$INSTDIR"
                    writeRegStr HKLM uninstallKey "Publisher" "IOHK"
                    writeRegStr HKLM uninstallKey "ProductVersion" (str fullVersion)
                    writeRegStr HKLM uninstallKey "VersionMajor" (str . (!! 0). parseVersion $ fullVersion')
                    writeRegStr HKLM uninstallKey "VersionMinor" (str . (!! 1). parseVersion $ fullVersion')
                    writeRegStr HKLM uninstallKey "DisplayName" "$SpacedName"
                    writeRegStr HKLM uninstallKey "DisplayVersion" (str fullVersion)
                    writeRegStr HKLM uninstallKey "UninstallString" "\"$INSTDIR/uninstall.exe\""
                    writeRegStr HKLM uninstallKey "QuietUninstallString" "\"$INSTDIR/uninstall.exe\" /S"
                    writeRegDWORD HKLM uninstallKey "NoModify" 1
                    writeRegDWORD HKLM uninstallKey "NoRepair" 1
                file [] "uninstall.exe"

        -- this string never appears in the UI
        _ <- section "Start Menu Shortcuts" [] $ do
                createDirectory "$SMPROGRAMS/$SpacedName"
                createShortcut "$SMPROGRAMS/$SpacedName/Uninstall $SpacedName.lnk"
                    [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
                createShortcut "$SMPROGRAMS/$SpacedName/$SpacedName.lnk" (desktopShortcut installDirectory)
        return ()

lshow :: Show a => a -> String
lshow = T.unpack . lshowText

packageFrontend :: Cluster -> InstallerConfig -> IO ()
packageFrontend cluster installerConfig = do
    let
      icon = format ("installers/icons/"%s%"/"%s) (lshowText cluster) (lshowText cluster)
      installDir :: Text
      installDir = installDirectory installerConfig
      releaseDir :: Text
      releaseDir = "../release/win32-x64/" <> (installDirectory installerConfig) <> "-win32-x64"
    export "NODE_ENV" "production"
    rewritePackageJson "../package.json" installDir
    echo "running yarn"
    shells ("yarn run package --icon " <> icon) empty
    cp "../node_modules/ps-list/fastlist.exe" $ fromString $ T.unpack $ releaseDir <> "/resources/app/dist/main/fastlist.exe"

-- | The contract of `main` is not to produce unsigned installer binaries.
main :: Options -> IO ()
main opts@Options{..}  = do
    cp (fromText "launcher-config.yaml") (fromText "../launcher-config.yaml")

    installerConfig <- decodeFileThrow "installer-config.json"

    fullVersion <- getAppVersion "../package.json"

    echo "Packaging frontend"
    packageFrontend oCluster installerConfig

    let fullName = packageFileName (uglyName installerConfig) Win64 oCluster fullVersion oBackend oBuildJob oBuildCounter

    printf ("Building: "%fp%"\n") fullName

    echo "Adding permissions manifest to cardano-launcher.exe"
    procs "C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\mt.exe" ["-manifest", "cardano-launcher.exe.manifest", "-outputresource:cardano-launcher.exe;#1"] mempty

    signFile opts "cardano-launcher.exe"
    signFile opts "cardano-node.exe"

    echo "Writing uninstaller.nsi"
    writeUninstallerNSIS fullVersion installerConfig
    signUninstaller opts

    let
      nsiFileName :: String
      nsiFileName = T.unpack (uglyName installerConfig) ++ ".nsi"

    echo . fromString $ "Writing " ++ nsiFileName
    writeInstallerNSIS fullName fullVersion installerConfig opts oCluster

    rawnsi <- readFile nsiFileName
    putStr rawnsi
    IO.hFlush IO.stdout

    windowsRemoveDirectoryRecursive $ T.unpack $ "../release/win32-x64/" <> (installDirectory installerConfig) <> "-win32-x64/resources/app/installers/.stack-work"

    echo "Generating NSIS installer"
    procs "C:\\Program Files (x86)\\NSIS\\makensis" [T.pack nsiFileName, "-V4"] mempty

    signed <- signFile opts fullName
    case signed of
      SignedOK  -> pure ()
      NotSigned -> rm fullName

getTempDir :: MonadIO io => io FilePath
getTempDir = need "TEMP" >>= \case
  Just temp -> pure (fromText temp)
  Nothing -> die "Environment variable TEMP is not set."
