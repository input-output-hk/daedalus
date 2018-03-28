\(cluster : ./cluster.type) ->
\(os      : ./os.type)      ->
{ configuration  =
    { filePath     = "configuration.yaml"
    , key          = cluster.key
    , systemStart  = [] : Optional Integer
    , seed         = [] : Optional Integer
    }
, nodeLogConfig  = "log-config-prod.yaml"
, nodeTimeoutSec = 30
, reportServer   = cluster.reportServer
, walletArgs     = [] : List Text
, nodeArgs =
    [ "--no-ntp"
    , "--tlsca",               "tls/ca/ca.crt"
    , "--tlscert",             "tls/server/server.crt"
    , "--tlskey",              "tls/server/server.key"
    , "--topology",            "wallet-topology.yaml"
    , "--update-server",       cluster.updateServer
    , "--keyfile",             os.nodeArgs.keyfile
    , "--logs-prefix",         os.nodeArgs.logsPrefix
    , "--wallet-db-path",      os.nodeArgs.walletDBPath
    , "--update-latest-path",  os.nodeArgs.updateLatestPath
    , "--update-with-package"
    ]
} // os.pass
