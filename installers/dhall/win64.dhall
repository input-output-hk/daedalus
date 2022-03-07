\(cluster : ./cluster.type)      ->
   let installDir = "Daedalus${cluster.installDirectorySuffix}"
in let dataDir = "\${APPDATA}\\${installDir}"
    --
    --
in
{ name               = "win64"
, installDirectory   = installDir
, macPackageName     = "unused"
, x509ToolPath       = None Text
, nodeArgs           =
  { logsPrefix       = "Logs"
  , topology         = None Text
  , updateLatestPath = None Text
  , statePath        = "state"
  , tlsPath          = None Text
  }
, pass      =
  { statePath           = dataDir
  , workingDir          = dataDir
  , nodeBin             = "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-node.exe"
  , walletBin           = "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-wallet.exe"
  , daedalusBin         = "\${DAEDALUS_INSTALL_DIRECTORY}\\${installDir}.exe"
  , cliPath             = "\${DAEDALUS_INSTALL_DIRECTORY}\\cardano-cli.exe"
  , nodeLogConfig       = None Text
  , nodeLogPath         = None Text

  , walletLogging       = True
  , frontendOnlyMode    = True

  , updaterPath         = None Text
  , updaterArgs         = [] : List Text
  , updateArchive       = None Text
  , updateWindowsRunner = Some "Installer.bat"

  , launcherLogsPrefix  = "Logs\\pub"
  }
}
