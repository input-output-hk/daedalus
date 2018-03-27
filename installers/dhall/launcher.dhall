\(cluster : ./cluster.type) ->
\(os      : ./os.type)      ->
{ configuration  =
    { filePath     = os.configurationYaml
    , key          = "${cluster.keyPrefix}_${os.name}"
    , systemStart  = [] : Optional Integer
    , seed         = [] : Optional Integer
    }
, nodeTimeoutSec = 30
, reportServer   = cluster.reportServer
, walletArgs     = [] : List Text
, nodeArgs =
    [ "--tlsca",               "tls/ca/ca.crt"
    , "--tlscert",             "tls/server/server.crt"
    , "--tlskey",              "tls/server/server.key"
    , "--update-server",       cluster.updateServer
    , "--keyfile",             os.nodeArgs.keyfile
    , "--logs-prefix",         os.nodeArgs.logsPrefix
    , "--topology",            os.nodeArgs.topology
    , "--wallet-db-path",      os.nodeArgs.walletDBPath
    , "--update-latest-path",  os.nodeArgs.updateLatestPath
    , "--wallet-address",      "127.0.0.1:8090"
    -- XXX: this is a workaround for Linux
    , "--update-with-package"
    ]
} // os.pass
