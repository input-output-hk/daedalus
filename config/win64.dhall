let dataDir = "%APPDATA%\\Daedalus\\"
in
{ name             = "win64"
, nodeArgsOS =
  { keyfile          = dataDir ++ "Secrets-1.0\\secret.key"
  , logsPrefix       = dataDir ++ "Logs"
  , updateLatestPath = dataDir ++ "Installer.exe"
  , walletDBPath     = dataDir ++ "Wallet-1.0"
  }
, passOS =
  { nodePath            = "%DAEDALUS_DIR%\\cardano-node.exe"
  , nodeDbPath          = dataDir ++ "DB-1.0"
  , nodeLogPath         = dataDir ++ "Logs\\cardano-node.log"

  , walletPath          = "%DAEDALUS_DIR%\\Daedalus.exe"

  , updaterPath         = dataDir ++ "Installer.exe"
  , updaterArgs         = [] : List Text
  , updateArchive       = [] : Optional Text
  , updateWindowsRunner = [dataDir ++ "Installer.bat"] : Optional Text

  , launcherLogsPrefix  = dataDir ++ "Logs\\pub"
  }
}