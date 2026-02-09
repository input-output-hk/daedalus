---
name: cardano-cli-plutus-scripts
description: "Plutus script guidance: datums, redeemers, collateral, reference scripts. Templates only—use operator to execute."
allowed-tools:
  - Read
user-invocable: true
---

# cardano-cli-plutus-scripts

> **This is a guidance skill.** Provides templates and explanations. For execution, use `cardano-cli-plutus-scripts-operator`.

## When to use
- Learning Plutus script transaction patterns
- Understanding datums, redeemers, collateral
- Getting templates for script interactions

## Operating rules (must follow)
- Confirm network and script version (V2/V3)
- Never execute—only provide templates
- Always include collateral handling
- Use fresh protocol parameters

## Key concepts

### Script versions
- **V1** (Alonzo): Legacy, limited features
- **V2** (Babbage): Reference scripts, inline datums
- **V3** (Conway): Governance, simplified interface

### Datum handling
- **Inline datum**: Stored on-chain with UTxO
- **Datum hash**: Only hash on-chain, provide full datum when spending
- **No datum**: For some V3 scripts

### Collateral
- Required for all Plutus script transactions
- Must be ADA-only UTxO
- Forfeit if script fails unexpectedly
- Typically 1-5 ADA sufficient

## Workflow templates

### Lock funds at script address
```bash
# 1. Derive script address
cardano-cli conway address build \
  --payment-script-file script.plutus \
  --testnet-magic 1 \
  --out-file script.addr

# 2. Create datum file
echo '{"constructor": 0, "fields": []}' > datum.json

# 3. Lock with inline datum
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --tx-out "$(cat script.addr)+5000000" \
  --tx-out-inline-datum-file datum.json \
  --change-address <payment-addr> \
  --out-file tx.unsigned

# Sign and submit
```

### Spend from script
```bash
# 1. Create redeemer
echo '{"constructor": 0, "fields": []}' > redeemer.json

# 2. Build script spend
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <script-utxo>#<index> \
  --tx-in-script-file script.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --tx-in-collateral <collateral-utxo>#<index> \
  --tx-out <recipient>+<amount> \
  --change-address <payment-addr> \
  --out-file tx.unsigned

# Sign (with collateral key) and submit
```

### With reference script
```bash
# Script already on-chain at reference UTxO
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <script-utxo>#<index> \
  --spending-tx-in-reference <ref-script-utxo>#<index> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file redeemer.json \
  --tx-in-collateral <collateral>#<index> \
  --tx-out <recipient>+<amount> \
  --change-address <payment-addr> \
  --out-file tx.unsigned
```

## Examples

### Example: Simple validator spend
**User request:** "Spend from my always-succeeds script"

**Response:**
```bash
# === Spend from Always-Succeeds Script ===

SCRIPT_ADDR=$(cat script.addr)
PAYMENT_ADDR=$(cat base.addr)

# 1. Query script UTxO
cardano-cli conway query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 1
# Note the UTxO hash and index

# 2. Query payment UTxOs (for collateral)
cardano-cli conway query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 1
# Select ADA-only UTxO for collateral

# 3. Create redeemer (for always-succeeds, can be unit)
echo '{"constructor": 0, "fields": []}' > redeemer.json

# 4. Build transaction
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <script-utxo>#0 \
  --tx-in-script-file always-succeeds.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --tx-in-collateral <collateral-utxo>#0 \
  --tx-out "$PAYMENT_ADDR+4500000" \
  --change-address $PAYMENT_ADDR \
  --out-file tx.unsigned

# 5. Sign and submit (use operator skill)
```

## Debugging

### Calculate execution costs
```bash
cardano-cli conway transaction build \
  ... \
  --calculate-plutus-script-cost costs.json

cat costs.json | jq .
```

### Common failures
- **ExUnitsTooBig**: Script exceeds execution budget
- **CollateralNotFound**: Missing or invalid collateral
- **DatumMismatch**: Provided datum doesn't match hash
- **ScriptFailure**: Validator returned False

## Safety / key handling
- Test scripts on preprod/preview first
- Use minimal collateral
- Datum/redeemer are public—no secrets
- Verify script hash after rebuilds

## References
- `shared/PRINCIPLES.md`
- `cardano-cli-plutus-scripts-operator` (for execution)
- `aiken-smart-contracts` (for writing validators)
