\(os :
  { key      : Text
  , pass     : { reportServer        : Text
               , nodePath            : Text
               , nodeDbPath          : Text
               , nodeLogPath         : Text
               , nodeArgs            : List Text
               , walletPath          : Text
               , updaterPath         : Text
               , updaterArgs         : List Text
               , updateArchive       : Optional Text
               , updateWindowsRunner : Optional Text
               , launcherLogsPrefix  : Text
  }})
->
{ configuration  =
    { filePath     = "configuration.yaml"
    , key          = os.key
    , systemStart  = [] : Optional Integer
    , seed         = [] : Optional Integer
    }
, nodeLogConfig  = "log-config-prod.yaml"
, nodeTimeoutSec = 30
, walletArgs     = [] : List Text
} // os.pass
