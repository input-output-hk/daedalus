\(perCluster: { keyType : Text, reportServer : Text, updateServer : Text })
->
{   nodePath            = "./cardano-node"
  , nodeDbPath          = "$HOME/Library/Application Support/Daedalus/DB-1.0"
  , nodeLogPath         = "$HOME/Library/Application Support/Daedalus/Logs/cardano-node.log"
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
      , perCluster.updateServer
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

  , walletPath          = "./Frontend"

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = ["$HOME/Library/Application Support/Daedalus/installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "$HOME/Library/Application Support/Daedalus/Logs/pub/"
}
