{ configuration = { key = "mainnet_full_wallet_macos64" }
, nodeArgs =
      [ "--update-latest-path"
      , "$HOME/Library/Application Support/Daedalus/installer.pkg"
      , "--keyfile"
      , "$HOME/Library/Application Support/Daedalus/Secrets-1.0/secret.key"
      , "--logs-prefix"
      , "$HOME/Library/Application Support/Daedalus/Logs"
      , "--wallet-db-path"
      , "$HOME/Library/Application Support/Daedalus/Wallet-1.0"
      , "--update-server"
      , "http://update.cardano-mainnet.iohk.io"
      , "--update-with-package"
      , "--no-ntp"
      , "--tlscert"
      , "./tls/server/server.crt"
      , "--tlskey"
      , "./tls/server/server.key"
      , "--tlsca"
      , "./tls/ca/ca.crt"
      , "--topology"
      , "./wallet-topology.yaml" ]
}