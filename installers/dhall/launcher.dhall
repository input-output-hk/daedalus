\(cluster : ./cluster.type) ->
\(os      : ./os.type)      ->
{ configuration  =
    { filePath     = os.configurationYaml
    , key          = "${cluster.keyPrefix}_${os.name}"
    , systemStart  = [] : List Integer
    , seed         = [] : List Integer
    }
, nodeTimeoutSec = 60
, walletArgs     = [] : List Text
, logsPrefix     = os.nodeArgs.logsPrefix
, tlsPath        = os.nodeArgs.tlsPath
, x509ToolPath   = os.x509ToolPath
, nodeImplementation = "\${NODE_IMPLEMENTATION}"
, nodeArgs =
    [ "--tlsca",               "${os.nodeArgs.tlsPath}/server/ca.crt"
    , "--tlscert",             "${os.nodeArgs.tlsPath}/server/server.crt"
    , "--tlskey",              "${os.nodeArgs.tlsPath}/server/server.key"
    , "--no-client-auth"
    , "--log-console-off"
    , "--update-server",       cluster.updateServer
    , "--keyfile",             os.nodeArgs.keyfile
    , "--topology",            os.nodeArgs.topology
    , "--wallet-db-path",      os.nodeArgs.walletDBPath
    , "--update-latest-path",  os.nodeArgs.updateLatestPath
    , "--wallet-address",      "127.0.0.1:0"
    -- XXX: this is a workaround for Linux
    , "--update-with-package"
    ] # cluster.extraNodeArgs
} // os.pass
