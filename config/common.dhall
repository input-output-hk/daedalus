{   configuration =
      { filePath    = "configuration.yaml"
      , systemStart = [] : Optional Integer
      , seed        = [] : Optional Integer
      }
  , nodeLogConfig       = "log-config-prod.yaml"
  , nodeTimeoutSec      = 30
  , walletArgs          = [] : List Text
}