\(os : ./os.type)
->
{ key          = "mainnet_wallet_${os.name}"
, relays       = "relays.cardano-mainnet.iohk.io"
, updateServer = "http://update.cardano-mainnet.iohk.io"
, reportServer = "http://report-server.cardano-mainnet.iohk.io:8080"
}
