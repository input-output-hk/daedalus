\(cluster : ./cluster.type)      ->
let dataDir = "\${HOME}/Library/Application Support/Daedalus${cluster.installDirectorySuffix}"
    --
    --
in
{ name      = "macos64"
, configurationYaml  = "\${DAEDALUS_INSTALL_DIRECTORY}/configuration.yaml"
, installDirectory = "Daedalus${cluster.installDirectorySuffix}"
, macPackageName   = "Daedalus${cluster.macPackageSuffix}"
, x509ToolPath       = "\${DAEDALUS_INSTALL_DIRECTORY}/cardano-x509-certificates"
, nodeArgs           =
  { keyfile          = "${dataDir}/Secrets-1.0/secret.key"
  , logsPrefix       = "${dataDir}/Logs"
  , topology         = "\${DAEDALUS_INSTALL_DIRECTORY}/wallet-topology.yaml"
  , updateLatestPath = "${dataDir}/installer.pkg"
  , walletDBPath     = "${dataDir}/Wallet-1.0"
  , tlsPath          = "${dataDir}/tls"
  }
, pass      =
  { statePath           = dataDir
  , workingDir          = dataDir
  , nodePath            = "\${DAEDALUS_INSTALL_DIRECTORY}/\${NODE_EXE}"
  , cliPath             = "\${DAEDALUS_INSTALL_DIRECTORY}/\${CLI_EXE}"
  , nodeImplementation  = "\${NODE_IMPLEMENTATION}"
  , nodeDbPath          = "${dataDir}/DB-1.0"
  , nodeLogConfig       = "\${DAEDALUS_INSTALL_DIRECTORY}/log-config-prod.yaml"
  , nodeLogPath         = [] : Optional Text

  , walletPath          = "\${DAEDALUS_INSTALL_DIRECTORY}/Frontend"
  , walletLogging       = True
  , frontendOnlyMode    = True

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = ["${dataDir}/installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "${dataDir}/Logs/pub/"
  }
}
