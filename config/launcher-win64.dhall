\(perCluster: { keyType : Text, relays : Text, reportServer : Text, updateServer : Text })
->
{ key      = "mainnet_${perCluster.keyType}wallet_win64"
, pass     =
  { reportServer        = perCluster.reportServer
  , nodePath            = "%DAEDALUS_DIR%\\cardano-node.exe"
  , nodeDbPath          = "%APPDATA%\\Daedalus\\DB-1.0"
  , nodeLogPath         = "%APPDATA%\\Daedalus\\Logs\\cardano-node.log"
  , nodeArgs =
      [ "--update-latest-path"
      , "%APPDATA%\\Daedalus\\Installer.exe"
      , "--keyfile"
      , "%APPDATA%\\Daedalus\\Secrets-1.0\\secret.key"
      , "--logs-prefix"
      , "%APPDATA%\\Daedalus\\Logs"
      , "--wallet-db-path"
      , "%APPDATA%\\Daedalus\\Wallet-1.0"
      , "--update-server"
      , perCluster.updateServer
      , "--update-with-package"
      , "--no-ntp"
      , "--tlscert"
      , "tls\\server\\server.crt"
      , "--tlskey"
      , "tls\\server\\server.key"
      , "--tlsca"
      , "tls\\ca\\ca.crt"
      , "--topology"
      , "wallet-topology.yaml" ]

  , walletPath          = "%DAEDALUS_DIR%\\Daedalus.exe"

  , updaterPath         = "%APPDATA%\\Daedalus\\Installer.exe"
  , updaterArgs         = [] : List Text
  , updateArchive       = [] : Optional Text
  , updateWindowsRunner = ["%APPDATA%\\Daedalus\\Installer.bat"] : Optional Text

  , launcherLogsPrefix  = "%APPDATA%\\Daedalus\\Logs\\pub"
  }
}