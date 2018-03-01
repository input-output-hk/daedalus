\(cluster :
  { key          : Text
  , nodeArgsOS   : List Text
  , relays       : Text
  , updateServer : Text
  , passCluster  :
    { reportServer        : Text
    }
  , passOS :
    { nodePath            : Text
    , nodeDbPath          : Text
    , nodeLogPath         : Text
    , walletPath          : Text
    , updaterPath         : Text
    , updaterArgs         : List Text
    , updateArchive       : Optional Text
    , updateWindowsRunner : Optional Text
    , launcherLogsPrefix  : Text
  }})
->
{ configuration  =
    { filePath     = "configuration.yaml"
    , key          = cluster.key
    , systemStart  = [] : Optional Integer
    , seed         = [] : Optional Integer
    }
, nodeLogConfig  = "log-config-prod.yaml"
, nodeTimeoutSec = 30
, walletArgs     = [] : List Text
, nodeArgs =
    [ "--no-ntp"
    , "--tlsca"
    , "tls/ca/ca.crt"
    , "--tlscert"
    , "tls/server/server.crt"
    , "--tlskey"
    , "tls/server/server.key"
    , "--topology"
    , "wallet-topology.yaml"
    , "--update-server"
    , cluster.updateServer
    , "--update-with-package"
    ] # cluster.nodeArgsOS
} // cluster.passCluster
  // cluster.passOS
