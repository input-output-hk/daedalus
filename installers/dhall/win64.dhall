\(cluster : ./cluster.type)      ->
   let installDir = "Daedalus${cluster.installDirectorySuffix}"
in let dataDir = "\${APPDATA}\\${installDir}"
    --
    --
in
{ name      = "win64"
, configurationYaml  = "configuration.yaml"
, installDirectory   = installDir
, macPackageName     = "unused"
, x509ToolPath       = "\${DAEDALUS_DIR}\\cardano-x509-certificates.exe"
, nodeArgs           =
  { keyfile          = "${dataDir}\\Secrets-1.0\\secret.key"
  , logsPrefix       = "${dataDir}\\Logs"
  , topology         = "wallet-topology.yaml"
  , updateLatestPath = "${dataDir}\\Installer.exe"
  , walletDBPath     = "${dataDir}\\Wallet-1.0"
  , tlsPath          = "${dataDir}\\tls"
  }
, pass      =
  { nodePath            = "\${DAEDALUS_DIR}\\cardano-node.exe"
  , nodeDbPath          = "${dataDir}\\DB-1.0"
  , nodeLogConfig       = "log-config-prod.yaml"
  , nodeLogPath         = "${dataDir}\\Logs\\cardano-node.log"

  , walletPath          = "\${DAEDALUS_DIR}\\Daedalus.exe"
  , walletLogging       = True
  , frontendOnlyMode    = True

  , updaterPath         = "${dataDir}\\Installer.exe"
  , updaterArgs         = [] : List Text
  , updateArchive       = [] : Optional Text
  , updateWindowsRunner = ["${dataDir}\\Installer.bat"] : Optional Text

  , launcherLogsPrefix  = "${dataDir}\\Logs\\pub"
  }
}
