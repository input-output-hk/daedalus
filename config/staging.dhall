\(os :
  { name       : Text
  , nodeArgsOS :
    { keyfile          : Text
    , logsPrefix       : Text
    , updateLatestPath : Text
    , walletDBPath     : Text
    }
  , passOS     :
    { nodePath            : Text
    , nodeDbPath          : Text
    , nodeLogPath         : Text
    , walletPath          : Text
    , updaterPath         : Text
    , updaterArgs         : List Text
    , updateArchive       : Optional Text
    , updateWindowsRunner : Optional Text
    , launcherLogsPrefix  : Text
  }})
->
{ key          = "mainnet_dryrun_wallet_${os.name}"
, nodeArgsOS   = os.nodeArgsOS
, relays       = "relays.awstest.iohkdev.io"
, updateServer = "https://update-awstest.iohkdev.io"
, passOS       = os.passOS
, passCluster  =
    { reportServer = "http://report-server.awstest.iohkdev.io:8080"
    }
}
