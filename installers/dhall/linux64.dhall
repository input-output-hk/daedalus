\(cluster : ./cluster.type)      ->
let dataDir = "\${XDG_DATA_HOME}/Daedalus/${cluster.name}"
in
{ name               = "linux64"
, installDirectory   = ""
, macPackageName     = "unused"
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
  , nodeBin             = "cardano-node"
  , walletBin           = "cardano-wallet"
  , daedalusBin         = "daedalus-frontend"
  , cliPath             = "cardano-cli"
  , nodeLogConfig       = None Text
  , nodeLogPath         = None Text
  , walletLogging       = False
  , frontendOnlyMode    = True

  -- todo, find some way to disable updates when unsandboxed?
  , updaterPath         = None Text
  , updaterArgs         = [] : List Text
  , updateArchive       = None Text
  , updateWindowsRunner = None Text

  , launcherLogsPrefix  = "${dataDir}/Logs/"
  }
}
