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

launcherScript :: [String]
launcherScript =
  [ "@echo off"
  , "set DAEDALUS_DIR=%~dp0"
  , "set API=etc"
  , "set MANTIS_PATH=%DAEDALUS_DIR%\\resources\\app\\mantis"
  , "set MANTIS_CMD=mantis.exe"
  , "start Daedalus.exe"
  ]

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
writeUninstallerNSIS :: String -> IO ()
writeUninstallerNSIS fullVersion = do
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  writeFile "uninstaller.nsi" $ nsis $ do
    _ <- constantStr "Version" (str fullVersion)
    name "Daedalus(Mantis) Uninstaller $Version"
    outFile . str $ tempDir <> "\\tempinstaller.exe"
    unsafeInjectGlobal "!addplugindir \"nsis_plugins\\liteFirewall\\bin\""
    unsafeInjectGlobal "SetCompress off"
    _ <- section "" [Required] $ do
      unsafeInject $ "WriteUninstaller \"" <> tempDir <> "\\uninstall.exe\""

    uninstall $ do
      -- Remove registry keys
      deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis"
      deleteRegKey HKLM "Software/DaedalusMantis"
      rmdir [Recursive,RebootOK] "$INSTDIR"
      delete [] "$SMPROGRAMS/Daedalus Mantis/*.*"
      delete [] "$DESKTOP\\Daedalus Mantis.lnk"
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

writeInstallerNSIS :: String -> Bool -> IO String
writeInstallerNSIS fullVersion predownloadChain = do
  tempDir <- fmap fromJust $ lookupEnv "TEMP"
  let viProductVersion = L.intercalate "." $ parseVersion fullVersion
  let
    bootstrap_url :: String
    bootstrap_url = "https://s3-eu-west-1.amazonaws.com/iohk.mantis.bootstrap/mantis-boot-classic-18DEC.zip"
    bootstrap_hash :: String
    bootstrap_hash = "7e210991f10f9ddebd5d9fe42e98c59e"
    -- how much space the chain will take in gig
    bootstrap_size :: Int
    bootstrap_size = 45
  let -- Where to produce the installer
    outputFile :: String
    outputFile = if predownloadChain then
        "daedalus-win64-" <> fullVersion <> "-installer-with-chain.exe"
      else
        "daedalus-win64-" <> fullVersion <> "-installer.exe"
    backendPath :: String
    backendPath = if False then "$INSTDIR\\cardano-node.exe" else "$INSTDIR\\resources\\app\\mantis\\mantis.exe"
    backendName = if False then "Cardano Node" else "Mantis Node"
  echo $ unsafeTextToLine $ pack $ "VIProductVersion: " <> viProductVersion
  writeFile "daedalus.nsi" $ nsis $ do
    _ <- constantStr "Version" (str fullVersion)
    if True then do
      constantStr "bootstrapUrl" $ fromString bootstrap_url
      constantStr "backendPath" $ fromString backendPath
      constantStr "bootstrapHash" $ fromString bootstrap_hash
      constantInt "bootstrapSize" $ fromIntegral bootstrap_size
      pure ()
    else
      pure ()
    name "Daedalus (with Mantis $Version)"                  -- The name of the installer
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

    installDir "$PROGRAMFILES64\\Daedalus Mantis"                   -- Default installation directory...
    installDirRegKey HKLM "Software/DaedalusMantis" "Install_Dir"  -- ...except when already installed.

    page Directory                   -- Pick where to install
    _ <- constant "INSTALLEDAT" $ readRegStr HKLM "Software/DaedalusMantis" "Install_Dir"
    onPagePre Directory (iff_ (strLength "$INSTALLEDAT" %/= 0) $ abort "")

    page InstFiles                   -- Give a progress bar while installing

    _ <- section "" [Required] $ do
        setOutPath "$INSTDIR"        -- Where to install files in this section
        writeRegStr HKLM "Software/DaedalusMantis" "Install_Dir" "$INSTDIR" -- Used by launcher batch script
        createDirectory "$APPDATA\\DaedalusMantis\\Secrets-1.0"
        createDirectory "$APPDATA\\DaedalusMantis\\Logs"
        createDirectory "$APPDATA\\DaedalusMantis\\Logs\\pub"
        createShortcut "$DESKTOP\\Daedalus Mantis.lnk" daedalusShortcut
        file [] "version.txt"
        file [] "build-certificates-win64-mantis.bat"
        writeFileLines "$INSTDIR\\daedalus.bat" (map str launcherScript)
        file [Recursive] "dlls\\"
        file [Recursive] "libressl\\"
        file [Recursive] "..\\release\\win32-x64\\Daedalus-win32-x64\\"

        mapM_ unsafeInject
          [ "liteFirewall::AddRule \"" <> backendPath <> "\" \"" <> backendName <> "\""
          , "Pop $0"
          , "DetailPrint \"liteFirewall::AddRule: $0\""
          ]

        execWait "build-certificates-win64-mantis.bat \"$INSTDIR\" >\"%APPDATA%\\DaedalusMantis\\Logs\\build-certificates.log\" 2>&1"
        writeFileLines "$INSTDIR\\bootstrap.bat" (map str $ bootstrapScript bootstrap_url bootstrap_hash bootstrap_size)
        if predownloadChain then
          execWait "$INSTDIR\\bootstrap.bat"
        else
          pure ()

        -- Uninstaller
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "InstallLocation" "$INSTDIR"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "Publisher" "Input Output HK"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "ProductVersion" (str fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "VersionMajor" (str . (!! 0). parseVersion $ fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "VersionMinor" (str . (!! 1). parseVersion $ fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "DisplayName" "Daedalus Mantis"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "DisplayVersion" (str fullVersion)
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "QuietUninstallString" "\"$INSTDIR/uninstall.exe\" /S"
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/DaedalusMantis" "NoRepair" 1
        file [] (str $ tempDir <> "\\uninstall.exe")

    _ <- section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/Daedalus Mantis"
        createShortcut "$SMPROGRAMS/Daedalus Mantis/Uninstall Daedalus.lnk"
          [Target "$INSTDIR/uninstall.exe", IconFile "$INSTDIR/uninstall.exe", IconIndex 0]
        createShortcut "$SMPROGRAMS/Daedalus Mantis/Daedalus.lnk" daedalusShortcut
    return ()
  temp <- LBS.readFile "daedalus.nsi"
  LBS.putStrLn temp
  return outputFile

main :: IO ()
main = do
  makeInstaller True
  makeInstaller False

makeInstaller :: Bool -> IO ()
makeInstaller predownloadChain = do
  echo "Writing version.txt"
  version <- fmap (fromMaybe "dev") $ lookupEnv "APPVEYOR_BUILD_VERSION"
  let fullVersion = version <> ".0"
  writeFile "version.txt" fullVersion

  --signFile "cardano-node.exe"

  echo "Writing uninstaller.nsi"
  writeUninstallerNSIS fullVersion
  signUninstaller

  echo "Writing daedalus.nsi"
  outputFile <- writeInstallerNSIS fullVersion predownloadChain

  echo $ "Generating NSIS installer " <> fromString outputFile
  procs "C:\\Program Files (x86)\\NSIS\\makensis" ["daedalus.nsi"] mempty
  signFile outputFile
