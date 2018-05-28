\(cluster : ./cluster.type) ->
{ wallet = {
      relays    = [[{ host = cluster.relays }]]
    , valency   = 1
    , fallbacks = 7
  }
}