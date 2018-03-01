\(os :
  { name       : Text
  , nodeArgsOS : List Text
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
{ key          = "mainnet_wallet_${os.name}"
, nodeArgsOS   = os.nodeArgsOS
, relays       = "relays.cardano-mainnet.iohk.io"
, updateServer = "http://update.cardano-mainnet.iohk.io"
, passOS       = os.passOS
, passCluster  =
    { reportServer = "http://report-server.cardano-mainnet.iohk.io:8080"
    }
}
