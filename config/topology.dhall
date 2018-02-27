\(f : { isMainnet : Bool, isStaging : Bool
      , isMacos64 : Bool, isWin64 : Bool })
->
let relays  = if f.isMainnet then "relays.cardano-mainnet.iohk.io" else
              if f.isStaging then "relays.awstest.iohkdev.io"      else
              "INVALID-CLUSTER"
in
{ wallet = {
      relays    = [[{ host = relays }]]
    , valency   = 1
    , fallbacks = 7
  }
}