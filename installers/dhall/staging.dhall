\(os : ./os.type)
->
{ key          = "mainnet_dryrun_wallet_${os.name}"
, relays       = "relays.awstest.iohkdev.io"
, updateServer = "https://update-awstest.iohkdev.io"
, reportServer = "http://report-server.awstest.iohkdev.io:8080"
}
