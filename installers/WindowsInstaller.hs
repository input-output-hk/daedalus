module WindowsInstaller where

import           Control.Monad      (unless)
import qualified Data.List          as L
import           Data.Maybe         (fromJust, fromMaybe)
import           Data.Monoid        ((<>))
import           Data.String        (fromString)
import           Data.Text          (pack, split, unpack)
import           Development.NSIS
import           System.Directory   (doesFileExist)
import           System.Environment (lookupEnv)
import           Turtle             (ExitCode (..), echo, proc, procs)
import           Turtle.Line        (unsafeTextToLine)

import           Launcher

import qualified Data.ByteString.Lazy as LBS

data InstallerConfig = InstallerConfig {
    icApi :: String
  , appName :: String
  , predownloadChain :: Bool
  , camelCasedName :: String
  , spacedName :: String
}

launcherScript :: InstallerConfig -> [ String ]
launcherScript cfg = if icApi cfg == "etc" then mantisLauncherScript else cardanoLauncherScript

mantisLauncherScript :: [String]
mantisLauncherScript =
  [ "@echo off"
  , "set DAEDALUS_DIR=%~dp0"
  , "set API=etc"
  , "set MANTIS_PATH=%DAEDALUS_DIR%\\resources\\app\\mantis"
  , "set MANTIS_CMD=mantis.exe"
  , "start Daedalus.exe"
  ]

cardanoLauncherScript :: [ String ]
cardanoLauncherScript =
  [ "@echo off"
  , "SET DAEDALUS_DIR=%~dp0"
  , "start /D \"%DAEDALUS_DIR%\" cardano-launcher.exe " <> args
  ]
  where
    args = launcherArgs Launcher
      { nodePath = "%DAEDALUS_DIR%\\cardano-node.exe"
      , nodeLogPath = "%APPDATA%\\Daedalus\\Logs\\cardano-node.log"
      , walletPath = "%DAEDALUS_DIR%\\Daedalus.exe"
      , launcherLogPath = "%APPDATA%\\Daedalus\\Logs\\pub"
      , windowsInstallerPath = Just "%APPDATA%\\Daedalus\\Installer.bat"
      , updater =
          SelfUnpacking
            { updArchivePath = "%APPDATA%\\Daedalus\\Installer.exe"
            , updArgs = []
            }
      , runtimePath = "%APPDATA%\\Daedalus\\"
      }

daedalusShortcut :: [Attrib]
daedalusShortcut =
    [ Target "$INSTDIR\\daedalus.bat"
    , IconFile "$INSTDIR\\Daedalus.exe"
    , StartOptions "SW_SHOWMINIMIZED"
    , IconIndex 0
    ]

bootstrapScript :: String -> String -> Int -> [ String ]
bootstrapScript bootstrap_url bootstrap_hash bootstrap_size =
  [ "resources\\app\\mantis\\mantis.exe bootstrap \"" <> bootstrap_url <> "\" " <> bootstrap_hash <> " " <> show bootstrap_size <> "\""
  ]

-- See INNER blocks at http://nsis.sourceforge.net/Signing_an_Uninstaller
writeUninstallerNSIS :: InstallerConfig -> String -> IO ()
writeUninstallerNSIS cfg fullVersion = do
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  writeFile "uninstaller.nsi" $ nsis $ do
    _ <- constantStr "Version" (str fullVersion)
    name (fromString $ appName cfg <> " Uninstaller $Version")
    outFile . str $ tempDir <> "\\tempinstaller.exe"
    unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""
    unsafeInjectGlobal "SetCompress off"
    constantStr "camelCasedName" (fromString $ camelCasedName cfg)
    constantStr "spacedName" (fromString $ spacedName cfg)
    _ <- section "" [Required] $ do
      unsafeInject $ "WriteUninstaller \"" <> tempDir <> "\\uninstall.exe\""

    uninstall $ do
      -- Remove registry keys
      deleteRegKey HKLM (fromString $ "Software/Microsoft/Windows/CurrentVersion/Uninstall/" <> camelCasedName cfg)
      deleteRegKey HKLM (fromString $ "Software/" <> camelCasedName cfg)
      rmdir [Recursive,RebootOK] "$INSTDIR"
      delete [] "$SMPROGRAMS/$spacedName/*.*"
      delete [] "$DESKTOP\\$spacedName.lnk"
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
      Nothing -> echo . unsafeTextToLine . pack $ "Skipping signing " <> filename <> " due to lack of password"
      Just pass -> do
        echo . unsafeTextToLine . pack $ "Signing " <> filename
        -- TODO: Double sign a file, SHA1 for vista/xp and SHA2 for windows 8 and on
        --procs "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", pack pass, "/t", "http://timestamp.comodoca.com", "/v", pack filename] mempty
        exitcode <- proc "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A\\Bin\\signtool.exe" ["sign", "/f", "C:\\iohk-windows-certificate.p12", "/p", pack pass, "/fd", "sha256", "/tr", "http://timestamp.comodoca.com/?td=sha256", "/td", "sha256", "/v", pack filename] mempty
        unless (exitcode == ExitSuccess) $ error "Signing failed"
  else
    error $ "Unable to sign missing file '" <> filename <> "''"

parseVersion :: String -> [String]
parseVersion ver =
  case split (== '.') (pack ver) of
    v@[_, _, _, _] -> map unpack v
    _              -> ["0", "0", "0", "0"]

writeInstallerNSIS :: InstallerConfig -> String -> IO String
writeInstallerNSIS cfg fullVersion = do
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  let viProductVersion = L.intercalate "." $ parseVersion fullVersion
  let
    bootstrap_url :: String
    bootstrap_url = "https://s3-eu-west-1.amazonaws.com/iohk.mantis.bootstrap/mantis-boot-classic-18DEC.zip"
    bootstrap_hash :: String
    bootstrap_hash = "7e210991f10f9ddebd5d9fe42e98c59e"
    -- how much space the chain will take in gig
    bootstrap_size :: Int
    bootstrap_size = 33
  let -- Where to produce the installer
    outputFile :: String
    outputFile = if predownloadChain cfg then
        "daedalus-win64-" <> fullVersion <> "-installer-with-chain.exe"
      else
        "daedalus-win64-" <> fullVersion <> "-installer.exe"
    backendPath :: String
    backendPath = if icApi cfg == "cardano" then "$INSTDIR\\cardano-node.exe" else "$INSTDIR\\resources\\app\\mantis\\mantis.exe"
    backendName = if icApi cfg == "cardano" then "Cardano Node" else "Mantis Node"
  echo $ unsafeTextToLine $ pack $ "VIProductVersion: " <> viProductVersion
  writeFile "daedalus.nsi" $ nsis $ do
    _ <- constantStr "Version" (str fullVersion)
    if icApi cfg == "etc" then do
      constantStr "bootstrapUrl" $ fromString bootstrap_url
      constantStr "backendPath" $ fromString backendPath
      constantStr "bootstrapHash" $ fromString bootstrap_hash
      constantInt "bootstrapSize" $ fromIntegral bootstrap_size
      pure ()
    else
      pure ()
    constantStr "camelCasedName" (fromString $ camelCasedName cfg)
    constantStr "spacedName" (fromString $ spacedName cfg)
    if icApi cfg == "etc" then
      name "Daedalus (with Mantis $Version)"                  -- The name of the installer
    else
      name "Daedalus ($Version)"
    outFile $ fromString outputFile
    unsafeInjectGlobal "!define MUI_ICON \"icons\\64x64.ico\""
    unsafeInjectGlobal "!define MUI_HEADERIMAGE"
    unsafeInjectGlobal "!define MUI_HEADERIMAGE_BITMAP \"icons\\installBanner.bmp\""
    unsafeInjectGlobal "!define MUI_HEADERIMAGE_RIGHT"
    unsafeInjectGlobal $ "VIProductVersion " <> viProductVersion
    unsafeInjectGlobal $ "VIAddVersionKey \"ProductVersion\" " <> fullVersion
    unsafeInjectGlobal "Unicode true"
    requestExecutionLevel Highest
    unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""

    installDir "$PROGRAMFILES64\\$spacedName"                   -- Default installation directory...
    installDirRegKey HKLM (fromString $ "Software/" <> camelCasedName cfg) "Install_Dir"  -- ...except when already installed.

    page Directory                   -- Pick where to install
    _ <- constant "INSTALLEDAT" $ readRegStr HKLM (fromString $ "Software/" <> camelCasedName cfg) "Install_Dir"
    onPagePre Directory (iff_ (strLength "$INSTALLEDAT" %/= 0) $ abort "")

    page InstFiles                   -- Give a progress bar while installing

    _ <- section "" [Required] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeRegStr HKLM ( fromString $ "Software/" <> camelCasedName cfg) "Install_Dir" "$INSTDIR" -- Used by launcher batch script
        createDirectory "$APPDATA\\$camelCasedName\\Secrets-1.0"
        createDirectory "$APPDATA\\$camelCasedName\\Logs"
        createDirectory "$APPDATA\\$camelCasedName\\Logs\\pub"
        createShortcut "$DESKTOP\\$spacedName.lnk" daedalusShortcut
        file [] "version.txt"
        writeFileLines "$INSTDIR\\daedalus.bat" (map str $ launcherScript cfg)
        file [Recursive] "dlls\\"
        file [Recursive] "libressl\\"
        file [Recursive] "..\\release\\win32-x64\\Daedalus-win32-x64\\"

        mapM_ unsafeInject
          [ "liteFirewall::AddRule \"" <> backendPath <> "\" \"" <> backendName <> "\""
          , "Pop $0"
          , "DetailPrint \"liteFirewall::AddRule: $0\""
          ]

        case icApi cfg of
          "etc" -> do
            file [] "build-certificates-win64-mantis.bat"
            execWait "build-certificates-win64-mantis.bat \"$INSTDIR\" >\"%APPDATA%\\$camelCasedName\\Logs\\build-certificates.log\" 2>&1"
            writeFileLines "$INSTDIR\\bootstrap.bat" (map str $ bootstrapScript bootstrap_url bootstrap_hash bootstrap_size)
            if predownloadChain cfg then
              execWait "$INSTDIR\\bootstrap.bat"
            else
              pure ()
          "cardano" -> do
            file [] "build-certificates-win64.bat"
            file [] "ca.conf"
            file [] "server.conf"
            file [] "client.conf"
            file [] "wallet-topology.yaml"
            file [] "configuration.yaml"
            file [] "*genesis*.json"
            file [] "cardano-node.exe"
            file [] "cardano-launcher.exe"
            file [] "log-config-prod.yaml"
            execWait "build-certificates-win64.bat \"$INSTDIR\" >\"%APPDATA%\\Daedalus\\Logs\\build-certificates.log\" 2>&1"

        -- Uninstaller
        do
          let
            commonDirectory = fromString $ "Software/Microsoft/Windows/CurrentVersion/Uninstall/" <> camelCasedName cfg
          writeRegStr HKLM commonDirectory "InstallLocation" "$INSTDIR"
          writeRegStr HKLM commonDirectory "Publisher" "Input Output HK"
          writeRegStr HKLM commonDirectory "ProductVersion" (str fullVersion)
          writeRegStr HKLM commonDirectory "VersionMajor" (str . (!! 0). parseVersion $ fullVersion)
          writeRegStr HKLM commonDirectory "VersionMinor" (str . (!! 1). parseVersion $ fullVersion)
          writeRegStr HKLM commonDirectory "DisplayName" "$spacedName"
          writeRegStr HKLM commonDirectory "DisplayVersion" (str fullVersion)
          writeRegStr HKLM commonDirectory "UninstallString" "\"$INSTDIR/uninstall.exe\""
          writeRegStr HKLM commonDirectory "QuietUninstallString" "\"$INSTDIR/uninstall.exe\" /S"
          writeRegDWORD HKLM commonDirectory "NoModify" 1
          writeRegDWORD HKLM commonDirectory "NoRepair" 1
        file [] (str $ tempDir <> "\\uninstall.exe")

    _ <- section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/$spacedName"
        createShortcut "$SMPROGRAMS/$spacedName/Uninstall $spacedName.lnk"
          [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/$spacedName/Daedalus.lnk" daedalusShortcut
    return ()
  temp <- LBS.readFile "daedalus.nsi"
  LBS.putStrLn temp
  return outputFile

main :: IO ()
main = do
  api <- fromMaybe "cardano" <$> lookupEnv "API"
  case api of
    "cardano" -> do
      let
        cfg = InstallerConfig {
            icApi = "cardano"
          , appName = "Daedalus"
          , camelCasedName = "Daedalus"
          , spacedName = "Daedalus"
          , predownloadChain = False
        }
      makeInstaller cfg
    "etc" -> do
      let
        cfg = InstallerConfig {
            icApi = "etc"
          , appName = "Daedalus(Mantis)"
          , camelCasedName = "DaedalusMantis"
          , spacedName = "Daedalus Mantis"
          , predownloadChain = False
        }
      makeInstaller (cfg { predownloadChain = True})
      makeInstaller (cfg { predownloadChain = False})

makeInstaller :: InstallerConfig -> IO ()
makeInstaller cfg = do
  echo "Writing version.txt"
  version <- fmap (fromMaybe "dev") $ lookupEnv "APPVEYOR_BUILD_VERSION"
  let fullVersion = version <> ".0"
  writeFile "version.txt" fullVersion

  case icApi cfg of
    "cardano" -> do
      echo "Adding permissions manifest to cardano-launcher.exe"
      procs "C:\\Program Files (x86)\\Windows Kits\\8.1\\bin\\x64\\mt.exe" ["-manifest", "cardano-launcher.exe.manifest", "-outputresource:cardano-launcher.exe;#1"] mempty
      signFile "cardano-node.exe"
      signFile "cardano-launcher.exe"
    "etc" -> do
      -- TODO, sign mantis binaries?
      pure ()

  echo "Writing uninstaller.nsi"
  writeUninstallerNSIS cfg fullVersion
  signUninstaller

  echo "Writing daedalus.nsi"
  outputFile <- writeInstallerNSIS cfg fullVersion

  echo $ "Generating NSIS installer " <> fromString outputFile
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
  signFile outputFile
