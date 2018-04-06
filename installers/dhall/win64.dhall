\(cluster : ./cluster.type)      ->
let dataDir = "''${APPDATA}\\Daedalus\\"
    --
    --
in
{ name      = "win64"
, configurationYaml  = "configuration.yaml"
, nodeArgs           =
  { keyfile          = "${dataDir}\\Secrets-1.0\\secret.key"
  , logsPrefix       = "${dataDir}\\Logs"
  , topology         = "wallet-topology.yaml"
  , updateLatestPath = "${dataDir}\\Installer.exe"
  , walletDBPath     = "${dataDir}\\Wallet-1.0"
  }
, pass      =
  { nodePath            = "''${DAEDALUS_DIR}\\cardano-node.exe"
  , nodeDbPath          = "${dataDir}\\DB-1.0"
  , nodeLogConfig       = "log-config-prod.yaml"
  , nodeLogPath         = "${dataDir}\\Logs\\cardano-node.log"

  , walletPath          = "''${DAEDALUS_DIR}\\Daedalus.exe"

  , updaterPath         = "${dataDir}\\Installer.exe"
  , updaterArgs         = [] : List Text
  , updateArchive       = [] : Optional Text
  , updateWindowsRunner = ["${dataDir}\\Installer.bat"] : Optional Text

  , launcherLogsPrefix  = "${dataDir}\\Logs\\pub"
  }
}