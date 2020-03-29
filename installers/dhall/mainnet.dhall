{ name         = "mainnet"
, keyPrefix    = "mainnet_wallet"
, relays       = "relays.cardano-mainnet.iohk.io"
, updateServer = "https://update-cardano-mainnet.iohk.io"
, installDirectorySuffix = ""
, macPackageSuffix       = ""
, walletPort             = 8090
, extraNodeArgs          = [ "--network", "mainnet" ] : List Text
}
