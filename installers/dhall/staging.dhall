{ name         = "staging"
, keyPrefix    = "mainnet_dryrun_wallet"
, relays       = "relays.awstest.iohkdev.io"
, updateServer = "https://update-awstest.iohkdev.io"
, reportServer = "http://staging-report-server.awstest.iohkdev.io:8080"
, installDirectorySuffix = " Staging"
, macPackageSuffix       = "Staging"
, walletPort             = 8092
, extraNodeArgs          = [ "--metrics", "--ekg-server", "localhost:8082", "+RTS", "-T", "-RTS" ] : List Text
}
