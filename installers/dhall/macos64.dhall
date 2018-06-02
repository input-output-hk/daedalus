\(cluster : ./cluster.type)      ->
let dataDir = "\${HOME}/Library/Application Support/Daedalus${cluster.installDirectorySuffix}/"
    --
    --
in
{ name      = "macos64"
, configurationYaml  = "configuration.yaml"
, installDirectory = "Daedalus${cluster.installDirectorySuffix}"
, macPackageName   = "Daedalus${cluster.macPackageSuffix}"
, x509ToolPath       = "./cardano-x509-certificates"
, nodeArgs           =
  { keyfile          = "${dataDir}/Secrets-1.0/secret.key"
  , logsPrefix       = "${dataDir}/Logs"
  , topology         = "wallet-topology.yaml"
  , updateLatestPath = "${dataDir}/installer.pkg"
  , walletDBPath     = "${dataDir}/Wallet-1.0"
  , tlsPath          = "${dataDir}/tls"
  }
, pass      =
  { nodePath            = "./cardano-node"
  , nodeDbPath          = "${dataDir}/DB-1.0"
  , nodeLogConfig       = "log-config-prod.yaml"
  , nodeLogPath         = "${dataDir}/Logs/cardano-node.log"

  , walletPath          = "./Frontend"
  , walletLogging       = True
  , frontendOnlyMode    = True

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = ["${dataDir}/installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "${dataDir}/Logs/pub/"
  }
}
