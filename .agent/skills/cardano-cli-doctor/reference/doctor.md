# cardano-cli-doctor reference

## Quick diagnostic commands (copy/paste)
```bash
cardano-cli version
cardano-cli --help | sed -n '1,60p'
cardano-cli conway --help | sed -n '1,40p' || true
cardano-cli latest --help | sed -n '1,40p' || true
cardano-cli transaction build --help | sed -n '1,120p'
```

## What to look for
- Era-prefixed support: `cardano-cli conway ...` exists and returns help without error.
- Legacy era flags: `--babbage-era` / `--alonzo-era` appear under `transaction build --help` or similar.

## Recommended styles
**Era-prefixed (preferred when available):**
```bash
cardano-cli conway query protocol-parameters --mainnet --out-file pparams.json
cardano-cli conway transaction build --mainnet ... --out-file tx.body
```

**Legacy flags (fallback):**
```bash
cardano-cli query protocol-parameters --mainnet --out-file pparams.json
cardano-cli transaction build --babbage-era --mainnet ... --out-file tx.body
```
