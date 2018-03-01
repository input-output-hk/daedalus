{ name         = "win64"
, nodeArgsOS   =
    [ "--keyfile"
    , "%APPDATA%\\Daedalus\\Secrets-1.0\\secret.key"
    , "--logs-prefix"
    , "%APPDATA%\\Daedalus\\Logs"
    , "--update-latest-path"
    , "%APPDATA%\\Daedalus\\Installer.exe"
    , "--wallet-db-path"
    , "%APPDATA%\\Daedalus\\Wallet-1.0"
    ]
, passOS =
  { nodePath            = "%DAEDALUS_DIR%\\cardano-node.exe"
  , nodeDbPath          = "%APPDATA%\\Daedalus\\DB-1.0"
  , nodeLogPath         = "%APPDATA%\\Daedalus\\Logs\\cardano-node.log"

  , walletPath          = "%DAEDALUS_DIR%\\Daedalus.exe"

  , updaterPath         = "%APPDATA%\\Daedalus\\Installer.exe"
  , updaterArgs         = [] : List Text
  , updateArchive       = [] : Optional Text
  , updateWindowsRunner = ["%APPDATA%\\Daedalus\\Installer.bat"] : Optional Text

  , launcherLogsPrefix  = "%APPDATA%\\Daedalus\\Logs\\pub"
  }
}