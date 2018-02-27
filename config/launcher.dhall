\(f : { isMainnet : Bool, isStaging : Bool
      , isMacos64 : Bool, isWin64 : Bool })
->
let key_os_suffix  = if f.isMacos64 then "macos64" else
                     if f.isWin64   then "win64"   else
                                         "invalid-OS-spec" in
let  perCluster     = if      f.isMainnet then
                        { keyType      = "full"
                        , reportServer = "http://report-server.cardano-mainnet.iohk.io:8080"
                        , updateServer = "http://update.cardano-mainnet.iohk.io" }
                     else if f.isStaging then
                        { keyType      = "dryrun"
                        , reportServer = "http://report-server.awstest.iohkdev.io:8080"
                        , updateServer = "https://update-awstest.iohkdev.io" }
                     else
                        { keyType      = "INVALID-CLUSTER"
                        , reportServer = "INVALID-CLUSTER"
                        , updateServer = "INVALID-CLUSTER" }
in
{   configuration =
      { filePath    = "configuration.yaml"
      , key         = "mainnet_${perCluster.keyType}_wallet_${key_os_suffix}"
      , systemStart = [] : Optional Integer
      , seed        = [] : Optional Integer
      }
  , nodeLogConfig       = "log-config-prod.yaml"
  , nodeTimeoutSec      = 30
  , walletArgs          = [] : List Text
} // (
   if f.isMacos64 then ./launcher-macos64.dhall perCluster else
                       ./launcher-win64.dhall   perCluster)
