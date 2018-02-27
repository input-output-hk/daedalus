\(f : { keyType : Text, relays : Text, reportServer : Text, updateServer : Text })
->
{ wallet = {
      relays    = [[{ host = f.relays }]]
    , valency   = 1
    , fallbacks = 7
  }
}