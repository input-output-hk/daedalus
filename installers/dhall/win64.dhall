\(cluster : ./cluster.type)      ->
   let installDir = "Daedalus${cluster.installDirectorySuffix}"
in let dataDir = "\${APPDATA}\\${installDir}"
    --
    --
in
{ name      = "win64"
, configurationYaml  = "\${DAEDALUS_INSTALL_DIRECTORY}\\configuration.yaml"
, installDirectory   = installDir
, macPackageName     = "unused"
, x509ToolPath       = "\${DAEDALUS_DIR}\\cardano-x509-certificates.exe"
, nodeArgs           =
  { keyfile          = "${dataDir}\\Secrets-1.0\\secret.key"
  , logsPrefix       = "${dataDir}\\Logs"
  , topology         = "\${DAEDALUS_DIR}\\wallet-topology.yaml"
  , updateLatestPath = "${dataDir}\\Installer.exe"
  , walletDBPath     = "${dataDir}\\Wallet-1.0"
  , tlsPath          = "${dataDir}\\tls"
  }
, pass      =
  { statePath           = dataDir
  , workingDir          = dataDir
  , nodePath            = "\${DAEDALUS_DIR}\\cardano-node.exe"
  , nodeDbPath          = "DB-1.0"
  , nodeLogConfig       = "\${DAEDALUS_INSTALL_DIRECTORY}\\log-config-prod.yaml"
  , nodeLogPath         = [] : Optional Text

  , walletPath          = "\${DAEDALUS_DIR}\\Daedalus.exe"
  , walletLogging       = True
  , frontendOnlyMode    = False

  , updaterPath         = "${dataDir}\\Installer.exe"
  , updaterArgs         = [] : List Text
  , updateArchive       = [] : Optional Text
  , updateWindowsRunner = ["${dataDir}\\Installer.bat"] : Optional Text

  , launcherLogsPrefix  = "${dataDir}\\Logs\\pub"
  }
}
