\(cluster : ./cluster.type)      ->
let dataDir = "\${XDG_DATA_HOME}/Daedalus/${cluster.name}/"
in
{ name      = "linux64"
, configurationYaml  = "\${DAEDALUS_CONFIG}/configuration.yaml"
, installDirectory   = ""
, macPackageName     = "unused"
, nodeArgs           =
  { keyfile          = "Secrets/secret.key"
  , logsPrefix       = "Logs"
  , topology         = "\${DAEDALUS_CONFIG}/wallet-topology.yaml"
  , updateLatestPath = "${dataDir}/installer.sh"
  , walletDBPath     = "Wallet/"
  }
, pass      =
  { nodePath            = "cardano-node"
  , nodeDbPath          = "DB/"
  , nodeLogConfig       = "\${DAEDALUS_CONFIG}/daedalus.yaml"
  , nodeLogPath         = "${dataDir}/Logs/cardano-node.log"

  , walletPath          = "daedalus-frontend"
  , walletLogging       = False
  , frontendOnlyMode    = True

  -- todo, find some way to disable updates when unsandboxed?
  , updaterPath         = "/bin/update-runner"
  , updaterArgs         = [] : List Text
  , updateArchive       = [ "${dataDir}/installer.sh" ] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "${dataDir}/Logs/"
  }
}
