{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module WindowsInstaller
    ( main
    , writeInstallerNSIS
    , writeUninstallerNSIS
    ) where

import           Universum hiding (pass, writeFile, stdout, FilePath, die, view)

import           Control.Monad (unless)
import qualified Data.List as L
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Yaml                 (decodeFileThrow)
import           Development.NSIS (Attrib (IconFile, IconIndex, RebootOK, Recursive, Required, StartOptions, Target),
                                   HKEY (HKLM), Level (Highest), Page (Directory, InstFiles), abort,
                                   constant, constantStr, createDirectory, createShortcut, delete,
                                   deleteRegKey, file, iff_, installDir, installDirRegKey,
                                   name, nsis, onPagePre, onError, outFile, page, readRegStr,
                                   requestExecutionLevel, rmdir, section, setOutPath, str,
                                   strLength, uninstall, unsafeInject, unsafeInjectGlobal,
                                   loadLanguage,
                                   writeRegDWORD, writeRegStr, (%/=), fileExists)
import           Prelude ((!!))
import qualified System.IO as IO
import           Filesystem.Path (FilePath, (</>))
import           Filesystem.Path.CurrentOS (encodeString, fromText)
import           Turtle (Shell, Line, ExitCode (..), echo, proc, procs, inproc, shells, testfile, stdout, input, export, sed, strict, format, printf, fp, w, s, (%), need, writeTextFile, die, cp, rm)
import           Turtle.Pattern (text, plus, noneOf, star, dot)
import           AppVeyor
import qualified Codec.Archive.Zip    as Zip

import           Config
import           Types
import           Util



daedalusShortcut :: Text -> [Attrib]
daedalusShortcut installDir =
        [ Target "$INSTDIR\\cardano-launcher.exe"
        , IconFile $ fromString $ unpack $ "$INSTDIR\\" <> installDir <> ".exe"
        , StartOptions "SW_SHOWMINIMIZED"
        , IconIndex 0
        ]

-- See INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
writeUninstallerNSIS :: Version -> InstallerConfig -> IO ()
writeUninstallerNSIS (Version fullVersion) installerConfig = do
    tempDir <- getTempDir
    IO.writeFile "uninstaller.nsi" $ nsis $ do
        _ <- constantStr "Version" (str $ unpack fullVersion)
        _ <- constantStr "InstallDir" (str $ unpack $ installDirectory installerConfig)
        _ <- constantStr "SpacedName" (str $ unpack $ spacedName installerConfig)
        unsafeInjectGlobal "Unicode true"

        loadLanguage "English"
        loadLanguage "Japanese"

        --mapM_ unsafeInjectGlobal
        --  [ "LangString UninstallName ${LANG_ENGLISH} \"Uninstaller\""
        --  , "LangString UninstallName ${LANG_JAPANESE} \"アンインストーラー\""
        --  ]

        name "$SpacedName Uninstaller $Version"
        -- TODO, the nsis library doesnt support translation vars
        -- name "$InstallDir $(UninstallName) $Version"
        --unsafeInjectGlobal $ unpack ( "Name \"" <> (installDirectory installerConfig) <> " $(UninstallName) " <> (fullVersion) <> "\"")
        outFile . str . encodeString $ tempDir </> "tempinstaller.exe"
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
writeInstallerNSIS outName (Version fullVersion') InstallerConfig{hasBlock0,installDirectory,spacedName} Options{oBackend} clusterName = do
    tempDir <- getTempDir
    let fullVersion = unpack fullVersion'
        viProductVersion = L.intercalate "." $ parseVersion fullVersion'
    printf ("VIProductVersion: "%w%"\n") viProductVersion

    IO.writeFile "daedalus.nsi" $ nsis $ do
        _ <- constantStr "Version" (str fullVersion)
        _ <- constantStr "Cluster" (str $ lshow clusterName)
        _ <- constantStr "InstallDir" (str $ unpack installDirectory)
        _ <- constantStr "SpacedName" (str $ unpack spacedName)
        name "$SpacedName ($Version)"                  -- The name of the installer
        outFile $ str $ encodeString outName        -- Where to produce the installer
        unsafeInjectGlobal $ "!define MUI_ICON \"icons\\" ++ lshow clusterName ++ "\\" ++ lshow clusterName ++ ".ico\""
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE"
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_BITMAP \"icons\\installBanner.bmp\""
        unsafeInjectGlobal $ "!define MUI_HEADERIMAGE_RIGHT"
        unsafeInjectGlobal $ "VIProductVersion " <> viProductVersion
        unsafeInjectGlobal $ "VIAddVersionKey \"ProductVersion\" " <> fullVersion
        unsafeInjectGlobal "Unicode true"
        requestExecutionLevel Highest
        unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""

        installDir "$PROGRAMFILES64\\$SpacedName"                   -- Default installation directory...
        installDirRegKey HKLM "Software/$SpacedName" "Install_Dir"  -- ...except when already installed.

        page Directory                   -- Pick where to install
        _ <- constant "INSTALLEDAT" $ readRegStr HKLM "Software/$SpacedName" "Install_Dir"
        onPagePre Directory (iff_ (strLength "$INSTALLEDAT" %/= 0) $ abort "")

        page InstFiles                   -- Give a progress bar while installing

        loadLanguage "English"
        loadLanguage "Japanese"
        mapM_ unsafeInjectGlobal
          [ "LangString AlreadyRunning ${LANG_ENGLISH} \"is running. It needs to be fully shut down before running the installer!\""
          , "LangString AlreadyRunning ${LANG_JAPANESE} \"が起動中です。 インストーラーを実行する前に完全にシャットダウンする必要があります！\""
          ]

        _ <- section "" [Required] $ do
                setOutPath "$INSTDIR"        -- Where to install files in this section
                unsafeInject "AllowSkipFiles off"
                writeRegStr HKLM "Software/$SpacedName" "Install_Dir" "$INSTDIR" -- Used by launcher batch script
                createDirectory "$APPDATA\\$InstallDir\\Secrets-1.0"
                createDirectory "$APPDATA\\$InstallDir\\Logs"
                createDirectory "$APPDATA\\$InstallDir\\Logs\\pub"
                onError (delete [] "$APPDATA\\$InstallDir\\launcher.lock") $
                    --abort "$SpacedName $(AlreadyRunning)"
                    unsafeInject $ unpack $ "Abort \" " <> installDirectory <> "$(AlreadyRunning)\""
                iff_ (fileExists "$APPDATA\\$InstallDir\\Wallet-1.0\\open\\*.*") $
                    rmdir [] "$APPDATA\\$InstallDir\\Wallet-1.0\\open"
                case oBackend of
                  Jormungandr _ -> do
                    file [] "jormungandr.exe"
                    file [] "cardano-wallet-jormungandr.exe"
                    file [] "libffi-6.dll"
                    when (clusterName /= Selfnode) $
                      file [] "jormungandr-config.yaml"
                  Cardano _ -> do
                    file [] "cardano-node.exe"
                    file [] "cardano-wallet-byron.exe"
                  _ -> pure ()
                file [] "cardano-launcher.exe"
                --file [] "cardano-x509-certificates.exe"
                --file [] "log-config-prod.yaml"
                --file [] "wallet-topology.yaml"
                --file [] "configuration.yaml"
                --file [] "*genesis*.json"
                file [] "launcher-config.yaml"
                when hasBlock0 $
                  file [] "block-0.bin"
                when (clusterName == Selfnode) $ do
                  file [] "config.yaml"
                  file [] "genesis.yaml"
                  file [] "secret.yaml"
                file [Recursive] "dlls\\"
                file [Recursive] "..\\release\\win32-x64\\$SpacedName-win32-x64\\"

                mapM_ unsafeInject
                    [ "liteFirewall::AddRule \"$INSTDIR\\cardano-node.exe\" \"Cardano Node\""
                    , "Pop $0"
                    , "DetailPrint \"liteFirewall::AddRule: $0\""
                    ]

                createShortcut "$DESKTOP\\$SpacedName.lnk" (daedalusShortcut spacedName)

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
                file [] $ (str . encodeString $ tempDir </> "uninstall.exe")

        -- this string never appears in the UI
        _ <- section "Start Menu Shortcuts" [] $ do
                createDirectory "$SMPROGRAMS/$SpacedName"
                createShortcut "$SMPROGRAMS/$SpacedName/Uninstall $SpacedName.lnk"
                    [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
                createShortcut "$SMPROGRAMS/$SpacedName/$SpacedName.lnk" (daedalusShortcut installDirectory)
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
    cp "../node_modules/ps-list/fastlist.exe" $ fromString $ unpack $ releaseDir <> "/resources/app/dist/main/fastlist.exe"

-- | The contract of `main` is not to produce unsigned installer binaries.
main :: Options -> IO ()
main opts@Options{..}  = do
    cp (fromText "launcher-config.yaml") (fromText "../launcher-config.yaml")

    installerConfig <- decodeFileThrow "installer-config.json"

    fetchCardanoSL "."
    printCardanoBuildInfo "."

    fullVersion <- getDaedalusVersion "../package.json"
    ver <- getCardanoVersion

    echo "Packaging frontend"
    exportBuildVars opts ver
    packageFrontend oCluster installerConfig

    let fullName = packageFileName Win64 oCluster fullVersion oBackend ver oBuildJob

    printf ("Building: "%fp%"\n") fullName

    echo "Adding permissions manifest to cardano-launcher.exe"
    procs "C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\mt.exe" ["-manifest", "cardano-launcher.exe.manifest", "-outputresource:cardano-launcher.exe;#1"] mempty

    signFile opts "cardano-launcher.exe"
    signFile opts "cardano-node.exe"

    echo "Writing uninstaller.nsi"
    writeUninstallerNSIS fullVersion installerConfig
    signUninstaller opts

    echo "Writing daedalus.nsi"
    writeInstallerNSIS fullName fullVersion installerConfig opts oCluster

    rawnsi <- readFile "daedalus.nsi"
    putStr rawnsi
    IO.hFlush IO.stdout

    windowsRemoveDirectoryRecursive $ unpack $ "../release/win32-x64/" <> (installDirectory installerConfig) <> "-win32-x64/resources/app/installers/.stack-work"

    echo "Generating NSIS installer"
    procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi", "-V4"] mempty

    signed <- signFile opts fullName
    case signed of
      SignedOK  -> pure ()
      NotSigned -> rm fullName

-- | Download and extract the cardano-sl windows build.
fetchCardanoSL :: FilePath -> IO ()
fetchCardanoSL dst = do
  bs <- downloadCardanoSL "../cardano-sl-src.json"
  let opts = [Zip.OptDestination (encodeString dst), Zip.OptVerbose]
  Zip.extractFilesFromArchive opts (Zip.toArchive bs)

printCardanoBuildInfo :: MonadIO io => FilePath -> io ()
printCardanoBuildInfo dst = do
  let buildInfo what f = do
        let f' = dst </> f
        e <- testfile f'
        when e $ do
          echo what
          stdout (input f')
  buildInfo "cardano-sl build-id:" "build-id"
  buildInfo "cardano-sl commit-id:" "commit-id"
  buildInfo "cardano-sl ci-url:" "ci-url"

-- | Run cardano-node --version to get a version string.
-- Because this is Windows, all necessary DLLs for cardano-node.exe
-- need to be in the PATH.
getCardanoVersion :: IO Text
getCardanoVersion = withDir "DLLs" (grepCardanoVersion run)
  where
    run = inproc (tt prog) ["--version"] empty
    prog = ".." </> "cardano-node.exe"

grepCardanoVersion :: Shell Line -> IO Text
grepCardanoVersion = fmap T.stripEnd . strict . sed versionPattern
  where
    versionPattern = text "cardano-node-" *> plus (noneOf ", ") <* star dot

getTempDir :: MonadIO io => io FilePath
getTempDir = need "TEMP" >>= \case
  Just temp -> pure (fromText temp)
  Nothing -> die "Environment variable TEMP is not set."
