\(perCluster: { keyType : Text, relays : Text, reportServer : Text, updateServer : Text })
->
{ key        = "mainnet_${perCluster.keyType}wallet_macos64"
, nodeArgsOS =
    [ "--keyfile"
    , "$HOME/Library/Application Support/Daedalus/Secrets-1.0/secret.key"
    , "--logs-prefix"
    , "$HOME/Library/Application Support/Daedalus/Logs"
    , "--update-latest-path"
    , "$HOME/Library/Application Support/Daedalus/installer.pkg"
    , "--update-server"
    , perCluster.updateServer
    , "--wallet-db-path"
    , "$HOME/Library/Application Support/Daedalus/Wallet-1.0"
    ]
, pass =
  { reportServer        = perCluster.reportServer
  , nodePath            = "./cardano-node"
  , nodeDbPath          = "$HOME/Library/Application Support/Daedalus/DB-1.0"
  , nodeLogPath         = "$HOME/Library/Application Support/Daedalus/Logs/cardano-node.log"

  , walletPath          = "./Frontend"

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = ["$HOME/Library/Application Support/Daedalus/installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "$HOME/Library/Application Support/Daedalus/Logs/pub/"
  }
}