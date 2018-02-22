{   nodePath            = "./cardano-node"
  , nodeDbPath          = "$HOME/Library/Application Support/Daedalus/DB-1.0"
  , nodeLogPath         = "$HOME/Library/Application Support/Daedalus/Logs/cardano-node.log"

  , walletPath          = "./Frontend"

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = ["$HOME/Library/Application Support/Daedalus/installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "$HOME/Library/Application Support/Daedalus/Logs/pub/"
}
