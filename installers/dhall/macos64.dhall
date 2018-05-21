\(cluster : ./cluster.type)      ->
let dataDir = "\${HOME}/Library/Application Support/Daedalus${cluster.installDirectorySuffix}/"
    --
    --
in
{ name      = "macos64"
, installDirectory = "Daedalus${cluster.installDirectorySuffix}"
, configurationYaml  = "configuration.yaml"
, nodeArgs           =
  { keyfile          = "${dataDir}/Secrets-1.0/secret.key"
  , logsPrefix       = "${dataDir}/Logs"
  , topology         = "wallet-topology.yaml"
  , updateLatestPath = "${dataDir}/installer.pkg"
  , walletDBPath     = "${dataDir}/Wallet-1.0"
  }
, pass      =
  { nodePath            = "./cardano-node"
  , nodeDbPath          = "${dataDir}/DB-1.0"
  , nodeLogConfig       = "log-config-prod.yaml"
  , nodeLogPath         = "${dataDir}/Logs/cardano-node.log"

  , walletPath          = "./Frontend"
  , walletLogging       = True

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = ["${dataDir}/installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "${dataDir}/Logs/pub/"
  }
}
