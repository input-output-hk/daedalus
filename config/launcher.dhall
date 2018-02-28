\(os :
  { key        : Text
  , nodeArgsOS : List Text
  , pass :
    { reportServer        : Text
    , nodePath            : Text
    , nodeDbPath          : Text
    , nodeLogPath         : Text
    , nodeArgs            : List Text
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
    , key          = os.key
    , systemStart  = [] : Optional Integer
    , seed         = [] : Optional Integer
    }
, nodeLogConfig  = "log-config-prod.yaml"
, nodeTimeoutSec = 30
, walletArgs     = [] : List Text
, nodeArgs =
    [ "--update-with-package"
    , "--no-ntp"
    , "--tlscert"
    , "--update-server"
    , perCluster.updateServer
    , "tls/server/server.crt"
    , "--tlskey"
    , "tls/server/server.key"
    , "--tlsca"
    , "tls/ca/ca.crt"
    , "--topology"
    , "wallet-topology.yaml"
    ] # os.nodeArgsOS
} // os.pass
