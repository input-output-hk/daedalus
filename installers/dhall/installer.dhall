\(cluster : ./cluster.type) ->
\( os : ./os.type) ->
{
  installDirectory = os.installDirectory
, macPackageName   = os.macPackageName
, walletPort       = cluster.walletPort
}
