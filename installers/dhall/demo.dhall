\(os      : ./os.type)      ->
{ name         = "demo"
, keyPrefix    = "TODO, default without the suffix"
, relays       = "TODO, ip support missing"
, updateServer = "https://disabled.iohkdev.io"
, installDirectorySuffix = " JormungandrSelfnode"
, macPackageSuffix       = "JormungandrSelfnode"
, walletPort             = 8088
, extraNodeArgs          = [
    "launch",
    "--state-dir",
    "${os.pass.statePath}",
    "--node-port",
    "8088",
    "--port",
    "8888",
    "--genesis-block",
    "${os.pass.statePath}/block0.bin",
    "--",
    "--secret",
    "${os.pass.statePath}/secret.yaml"
  ] : List Text
}
