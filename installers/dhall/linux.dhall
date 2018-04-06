\(cluster : ./cluster.type)      ->
let dataDir = "''${HOME}/.local/share/Daedalus/"
    -- XXX: deal with XDG_DATA_HOME being an actual variable -- pass dataDir as an argument?
    --    ..or, maybe installation data is a better solution?
in
{ name      = "macos64"
    -- XXX: this is an ugly workaround for the lack of a linux configuration key in configuration.yaml
, configurationYaml  = "''${DAEDALUS_CONFIG}/configuration.yaml"
, nodeArgs           =
  { keyfile          = "Secrets/secret.key"
  , logsPrefix       = "Logs"
  , topology         = "''${DAEDALUS_CONFIG}/wallet-topology.yaml"
  , updateLatestPath = "${dataDir}/${cluster.name}/installer.sh"
  , walletDBPath     = "Wallet/"
  }
, pass      =
  { nodePath            = "''${DAEDALUS_BRIDGE}/bin/cardano-node"
  , nodeDbPath          = "DB/"
  , nodeLogConfig       = "''${DAEDALUS_CONFIG}/daedalus.yaml"
  , nodeLogPath         = "''${HOME}/.local/share/Daedalus/${cluster.name}/Logs/cardano-node.log"

  , walletPath          = "''${DAEDALUS_FRONTEND}/bin/daedalus-frontend"

  , updaterPath         = "/bin/update-runner"
  , updaterArgs         = [] : List Text
  , updateArchive       = [ "${dataDir}/${cluster.name}/installer.sh" ] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "${dataDir}/${cluster.name}/Logs/"
  }
}
-- nodeArgs = [
--   "--wallet-address" "127.0.0.1:8090"
-- ];
