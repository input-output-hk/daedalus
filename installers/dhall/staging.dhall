{ name         = "staging"
, keyPrefix    = "mainnet_dryrun_wallet"
, relays       = "relays.awstest.iohkdev.io"
, updateServer = "https://update-awstest.iohkdev.io"
, reportServer = "http://staging-report-server.awstest.iohkdev.io:8080"
, installDirectorySuffix = " Staging"
, macPackageSuffix       = "Staging"
, walletPort             = 8092
, extraNodeArgs          = [ "--network", "staging" ] : List Text
}
