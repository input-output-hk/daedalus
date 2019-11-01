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
  { keyfile          = "Secrets-1.0\\secret.key"
  , logsPrefix       = "Logs"
  , topology         = "\${DAEDALUS_DIR}\\wallet-topology.yaml"
  , updateLatestPath = "Installer.exe"
  , statePath     = "state"
  , tlsPath          = "tls"
  }
, pass      =
  { statePath           = dataDir
  , workingDir          = dataDir
  , nodePath            = "\${DAEDALUS_DIR}\\jormungandr.exe"
  , cliPath             = "\${DAEDALUS_DIR}\\jcli.exe"
  , nodeLogConfig       = "\${DAEDALUS_INSTALL_DIRECTORY}\\log-config-prod.yaml"
  , nodeLogPath         = None Text

  , walletPath          = "\${DAEDALUS_DIR}\\${installDir}.exe"
  , walletLogging       = True
  , frontendOnlyMode    = True

  , updaterPath         = "Installer.exe"
  , updaterArgs         = [] : List Text
  , updateArchive       = None Text
  , updateWindowsRunner = Some "Installer.bat"

  , launcherLogsPrefix  = "Logs\\pub"
  }
}
