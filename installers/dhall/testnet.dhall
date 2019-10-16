{ name         = "testnet"
, keyPrefix    = "testnet_wallet"
, relays       = "relays.cardano-testnet.iohkdev.io"
, updateServer = "https://disabled.iohkdev.io"
, installDirectorySuffix = " Testnet"
, macPackageSuffix       = "Testnet"
, walletPort             = 8094
, extraNodeArgs          = [ "--metrics", "--ekg-server", "localhost:8081", "+RTS", "-T", "-RTS" ] : List Text
}
