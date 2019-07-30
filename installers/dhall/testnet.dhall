{ name         = "testnet"
, keyPrefix    = "testnet_wallet"
, relays       = "relays.cardano-testnet.iohkdev.io"
, updateServer = "http://updates-cardano-testnet.s3.amazonaws.com"
, installDirectorySuffix = " Testnet"
, macPackageSuffix       = "Testnet"
, walletPort             = 8094
, extraNodeArgs          = [ "--network", "testnet" ] : List Text
}
