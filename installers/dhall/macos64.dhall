let dataDir = "$HOME/Library/Application Support/Daedalus/"
in
{ name       = "macos64"
, nodeArgs   =
  { keyfile          = dataDir ++ "Secrets-1.0/secret.key"
  , logsPrefix       = dataDir ++ "Logs"
  , updateLatestPath = dataDir ++ "installer.pkg"
  , walletDBPath     = dataDir ++ "Wallet-1.0"
  }
, pass       =
  { nodePath            = "./cardano-node"
  , nodeDbPath          = dataDir ++ "DB-1.0"
  , nodeLogPath         = dataDir ++ "Logs/cardano-node.log"

  , walletPath          = "./Frontend"

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = [dataDir ++ "installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = dataDir ++ "Logs/pub/"
  }
}