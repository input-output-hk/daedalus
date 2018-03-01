{ name       = "linux"
, nodeArgsOS =
    -- XXX: deal with XDG_DATA_HOME generalisation
    [ "--keyfile"
    , "$HOME/.local/share/Daedalus/Secrets-1.0/secret.key"
    , "--logs-prefix"
    , "$HOME/.local/share/Daedalus/Logs"
    , "--update-latest-path"
    , "$HOME/.local/share/Daedalus/installer.pkg"
    , "--wallet-db-path"
    , "$HOME/.local/share/Daedalus/Wallet-1.0"
    ]
, passOS =
  { nodePath            = "./cardano-node"
  , nodeDbPath          = "$HOME/.local/share/Daedalus/DB-1.0"
  , nodeLogPath         = "$HOME/.local/share/Daedalus/Logs/cardano-node.log"

  , walletPath          = "./Frontend"

  , updaterPath         = "/usr/bin/open"
  , updaterArgs         = ["-FW"]
  , updateArchive       = ["$HOME/.local/share/Daedalus/installer.pkg"] : Optional Text
  , updateWindowsRunner = [] : Optional Text

  , launcherLogsPrefix  = "$HOME/.local/share/Daedalus/Logs/pub/"
  }
}