---
name: cardano-cli-doctor
description: "Diagnose cardano-cli: version, era-prefixed vs legacy syntax, network flags. Produces compatibility report."
allowed-tools:
  - Bash(cardano-cli:*)
  - Bash(which:*)
  - Read
context:
  - "!cardano-cli version 2>&1 | head -10"
  - "!cardano-cli --help 2>&1 | head -40"
  - "!cardano-cli conway --help 2>&1 | head -20"
  - "!cardano-cli latest --help 2>&1 | head -10"
  - "!cardano-cli transaction build --help 2>&1 | head -30"
---

# cardano-cli-doctor

## When to use
- Before generating CLI workflows to detect correct command style (era-prefixed vs legacy)
- When debugging CLI flag errors or version mismatches
- When setting up a new environment

## Operating rules (must follow)
- Never ask for or log secret key contents
- Prefer **read-only** diagnostics (`--help`, `version`, `query tip`)
- If air-gapped machine, avoid network commands unless explicitly requested
- Output a **Compat Report** with recommended command style

## Workflow
1) Collect environment facts (auto-injected via context)
   - CLI version
   - Available era-prefixed commands (conway, latest)
   - Legacy flag presence (--babbage-era, etc.)

2) Decide command style
   - `cardano-cli conway --help` works → **era-prefixed supported**
   - `--babbage-era` in help → **legacy flags supported**
   - Both exist → prefer **era-prefixed** (newer)

3) Network sanity (optional, if socket available)
   - `cardano-cli query tip --mainnet` or `--testnet-magic <N>`
   - Detect: missing socket, wrong magic, sync status

4) Produce Compat Report
   ```
   CLI Version: X.Y.Z
   Era-prefixed: ✓ conway, latest
   Legacy flags: ✓ --babbage-era
   Recommended: era-prefixed (cardano-cli conway ...)
   Network: --testnet-magic 1 (preprod)
   ```

## Examples

### Example 1: Modern CLI (era-prefixed)
**Input:** User has cardano-cli 10.1.0

**Output:**
```
=== Cardano CLI Compatibility Report ===

CLI Version: 10.1.0.0
Git Rev: abc123

Supported Commands:
  ✓ cardano-cli conway --help (recommended)
  ✓ cardano-cli latest --help
  ✓ cardano-cli babbage --help

Recommended Style: ERA-PREFIXED
  cardano-cli conway transaction build ...
  cardano-cli conway query utxo ...

Network Flags:
  Mainnet: --mainnet
  Preprod: --testnet-magic 1
  Preview: --testnet-magic 2
```

### Example 2: Legacy CLI
**Input:** User has cardano-cli 8.x

**Output:**
```
=== Cardano CLI Compatibility Report ===

CLI Version: 8.24.0.0

Supported Commands:
  ✗ Era-prefixed not available

Legacy Flags Detected:
  ✓ --babbage-era
  ✓ --alonzo-era

Recommended Style: LEGACY FLAGS
  cardano-cli transaction build --babbage-era ...
```

## Safety / key handling
- Avoid commands that print or touch `.skey` files
- Recommend `chmod 600 *.skey` for real funds
- Prefer offline keygen for mainnet

## References
- `shared/PRINCIPLES.md` (repo)
- [Cardano CLI documentation](https://github.com/IntersectMBO/cardano-cli)
