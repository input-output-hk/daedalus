\(cluster : ./cluster.type)      ->
let dataDir = "\${XDG_DATA_HOME}/Daedalus/${cluster.name}"
in
{ name      = "linux64"
, configurationYaml  = "\${DAEDALUS_CONFIG}/configuration.yaml"
, installDirectory   = ""
, macPackageName     = "unused"
, x509ToolPath       = "cardano-x509-certificates"
, nodeArgs           =
  { keyfile          = "${dataDir}/Secrets/secret.key"
  , logsPrefix       = "${dataDir}/Logs"
  , topology         = "\${DAEDALUS_CONFIG}/wallet-topology.yaml"
  , updateLatestPath = "${dataDir}/installer.sh"
  , walletDBPath     = "${dataDir}/Wallet/"
  , tlsPath          = "${dataDir}/tls"
  }
, pass      =
  { statePath           = "${dataDir}/${cluster.name}"
  , nodePath            = "cardano-node"
  , nodeDbPath          = "${dataDir}/DB/"
  , nodeLogConfig       = "\${DAEDALUS_CONFIG}/log-config-prod.yaml"
  , nodeLogPath         = [] : Optional Text
  , walletPath          = "daedalus-frontend"
  , walletLogging       = False
  , frontendOnlyMode    = False

  -- todo, find some way to disable updates when unsandboxed?
  , updaterPath         = "/bin/update-runner"
  , updaterArgs         = [] : List Text
  , updateArchive       = [ "${dataDir}/installer.sh" ] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "${dataDir}/Logs/"
  }
}
