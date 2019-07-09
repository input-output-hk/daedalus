{ name         = "demo"
, keyPrefix    = "TODO, default without the suffix"
, relays       = "TODO, ip support missing"
, updateServer = "https://disabled.iohkdev.io"
, installDirectorySuffix = " Demo"
, macPackageSuffix       = "Demo"
, walletPort             = 8092
-- TODO: This is a hack for the custom version of http-bridge. It will actually
-- connect to the local demo cluster. Should change with the Haskell node
, extraNodeArgs          = [ "--network", "testnet" ] : List Text
}
