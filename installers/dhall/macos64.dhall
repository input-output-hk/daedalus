\(cluster : ./cluster.type)      ->
let dataDir = "\${HOME}/Library/Application Support/Daedalus${cluster.installDirectorySuffix}"
    --
    --
in
{ name      = "macos64"
, installDirectory = "Daedalus${cluster.installDirectorySuffix}"
, macPackageName   = "Daedalus${cluster.macPackageSuffix}"
, x509ToolPath       = None Text
, nodeArgs           =
  { logsPrefix       = "${dataDir}/Logs"
  , topology         = None Text
  , updateLatestPath = None Text
  , statePath        = "${dataDir}/state"
  , tlsPath          = None Text
  }
, pass      =
  { statePath           = dataDir
  , workingDir          = dataDir
  , nodeBin             = "\${DAEDALUS_INSTALL_DIRECTORY}/jormungandr"
  , walletBin           = "\${DAEDALUS_INSTALL_DIRECTORY}/cardano-wallet-jormungandr.exe"
  , daedalusBin         = "\${DAEDALUS_INSTALL_DIRECTORY}/Frontend"
  , cliPath             = "\${DAEDALUS_INSTALL_DIRECTORY}/jcli"
  , nodeLogConfig       = None Text
  , nodeLogPath         = None Text

  , walletLogging       = True
  , frontendOnlyMode    = True

  , updaterPath         = None Text
  , updaterArgs         = [] : List Text
  , updateArchive       = None Text
  , updateWindowsRunner = None Text

  , launcherLogsPrefix  = "${dataDir}/Logs/pub/"
  }
}
