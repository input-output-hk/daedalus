\(cluster : ./cluster.type)      ->
let dataDir = "\${XDG_DATA_HOME}/Daedalus/"
in
{ name      = "linux64"
, configurationYaml  = "\${DAEDALUS_CONFIG}/configuration.yaml"
, nodeArgs           =
  { keyfile          = "Secrets/secret.key"
  , logsPrefix       = "Logs"
  , topology         = "\${DAEDALUS_CONFIG}/wallet-topology.yaml"
  , updateLatestPath = "${dataDir}/${cluster.name}/installer.sh"
  , walletDBPath     = "Wallet/"
  }
, pass      =
  { nodePath            = "cardano-node"
  , nodeDbPath          = "DB/"
  , nodeLogConfig       = "\${DAEDALUS_CONFIG}/daedalus.yaml"
  , nodeLogPath         = "${dataDir}/${cluster.name}/Logs/cardano-node.log"

  , walletPath          = "daedalus-frontend"

  -- todo, find some way to disable updates when unsandboxed?
  , updaterPath         = "/bin/update-runner"
  , updaterArgs         = [] : List Text
  , updateArchive       = [ "${dataDir}/${cluster.name}/installer.sh" ] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "${dataDir}/${cluster.name}/Logs/"
  }
}
