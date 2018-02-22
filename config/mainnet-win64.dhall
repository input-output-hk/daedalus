{ configuration = { key = "mainnet_full_wallet_win64" }
, nodeArgs =
      [ "--update-latest-path"
      , "%APPDATA%/Daedalus/Installer.exe"
      , "--keyfile"
      , "%APPDATA%/Daedalus/Secrets-1.0/secret.key"
      , "--logs-prefix"
      , "%APPDATA%/Daedalus/Logs"
      , "--wallet-db-path"
      , "%APPDATA%/Daedalus/Wallet-1.0"
      , "--update-server"
      , "http://update.cardano-mainnet.iohk.io"
      , "--update-with-package"
      , "--no-ntp"
      , "--tlscert"
      , "tls/server/server.crt"
      , "--tlskey"
      , "tls/server/server.key"
      , "--tlsca"
      , "tls/ca/ca.crt"
      , "--topology"
      , "wallet-topology.yaml" ]
}