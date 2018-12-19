{ name         = "demo"
, keyPrefix    = "TODO, default without the suffix"
, relays       = "TODO, ip support missing"
, updateServer = "https://disabled.iohkdev.io"
, reportServer = "http://staging-report-server.awstest.iohkdev.io:8080"
, installDirectorySuffix = " Demo"
, macPackageSuffix       = "Demo"
, walletPort             = 8092
, extraNodeArgs          = [ "--metrics", "--ekg-server", "localhost:8083", "+RTS", "-T", "-RTS" ] : List Text
}
