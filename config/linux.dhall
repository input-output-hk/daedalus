{ name       = "linux"
, nodeArgsOS =        -- XXX: deal with XDG_DATA_HOME generalisation
  { keyfile          = "$HOME/.local/share/Daedalus/Secrets-1.0/secret.key"
  , logsPrefix       = "$HOME/.local/share/Daedalus/Logs"
  , updateLatestPath = "$HOME/.local/share/Daedalus/FIXME"
  , walletDBPath     = "$HOME/.local/share/Daedalus/Wallet-1.0"
  }
, passOS     =
  { nodePath            = "./cardano-node"
  , nodeDbPath          = "$HOME/.local/share/Daedalus/DB-1.0"
  , nodeLogPath         = "$HOME/.local/share/Daedalus/Logs/cardano-node.log"

  , walletPath          = "FIXME"

  , updaterPath         = "FIXME"
  , updaterArgs         = ["FIXME"]
  , updateArchive       = ["$HOME/.local/share/Daedalus/FIXME"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "$HOME/.local/share/Daedalus/Logs/pub/"
  }
}