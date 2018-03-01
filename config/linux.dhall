-- XXX: deal with XDG_DATA_HOME generalisation -- pass dataDir as an argument?
let dataDir = "$HOME/.local/share/Daedalus/"
in
{ name       = "linux"
, nodeArgsOS =
  { keyfile          = dataDir ++ "Secrets-1.0/secret.key"
  , logsPrefix       = dataDir ++ "Logs"
  , updateLatestPath = dataDir ++ "FIXME"
  , walletDBPath     = dataDir ++ "Wallet-1.0"
  }
, passOS     =
  { nodePath            = "./cardano-node"
  , nodeDbPath          = dataDir ++ "DB-1.0"
  , nodeLogPath         = dataDir ++ "Logs/cardano-node.log"

  , walletPath          = "FIXME"

  , updaterPath         = "FIXME"
  , updaterArgs         = ["FIXME"]
  , updateArchive       = [dataDir ++ "FIXME"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = dataDir ++ "Logs/pub/"
  }
}