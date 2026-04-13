# Protocol parameters cookbook

```bash
cardano-cli query protocol-parameters       --testnet-magic <MAGIC>       --out-file pparams.json
```

Keep `pparams.json` in the same folder as your tx body and witnesses.

Common debugging checklist:
- Does your CLI default to Conway? If unsure, use era-prefixed commands (e.g. `cardano-cli conway ...`) when supported.
- For Plutus scripts: confirm execution unit prices exist in params and match your network.
