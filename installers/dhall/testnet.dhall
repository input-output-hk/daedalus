\(os      : ./os.type)      ->
{ name         = "testnet"
, keyPrefix    = "jormungandr_testnet_wallet"
, relays       = "NA"
, updateServer = "NA"
, installDirectorySuffix = " JormungandrTestnet"
, macPackageSuffix       = "JormungandrTestnet"
, walletPort             = 8088
, extraNodeArgs          = [
    "launch",
    "--state-dir",
    "${os.pass.statePath}"
    "--node-port",
    "8088",
    "--port",
    "8888",
    "--genesis-block-hash",
    "ae57995b8fe086ba590c36dc930f2aa9b52b2ffa92c0698fff2347adafe8dc65",
    "--",
    "--trusted-peer",
    "/ip4/13.230.137.72/tcp/3000@ed25519_pk1w6f2sclsauhfd6r9ydgvn0yvpvg4p3x3u2m2n7thknwghrfpdu5sgvrql9",
    "--trusted-peer",
    "/ip4/13.230.48.191/tcp/3000@ed25519_pk1lzrdh0pcmhwcnqdl5cgcu7n0c76pm7g7p6pdey7wup54vz32gy6qlz5vnq",
    "--trusted-peer",
    "/ip4/18.196.168.220/tcp/3000@ed25519_pk1uufkgu0t9xm8ry04wnddtnku5gjg8typf5z6ehh65uc6nz4j8n4spq0xrl",
    "--trusted-peer",
    "/ip4/3.124.132.123/tcp/3000@ed25519_pk14tqkqnz3eydn0c8c8gmmyzxgnf2dztpy5dnrx09mhfzv0dh93s3qszqgpc",
    "--trusted-peer",
    "/ip4/18.184.181.30/tcp/3000@ed25519_pk178ge2jn6c40vgmrewgmg26nmtda47nk2jncukzj327mp3a9g2qzss2d44f",
    "--trusted-peer",
    "/ip4/184.169.162.15/tcp/3000@ed25519_pk1nk0ne8ez66w5tp2g8ctcakthjpz89eveyg0egcpylenhet83n0sq2jqz8q",
    "--trusted-peer",
    "/ip4/13.56.87.134/tcp/3000@ed25519_pk1ce450zrtn04eaevcn9csz0thpjuhxrysdrq6qlr9pq7e0wd842nsxy6r5k"
  ] : List Text
}
