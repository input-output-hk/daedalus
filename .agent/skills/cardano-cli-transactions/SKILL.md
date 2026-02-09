---
name: cardano-cli-transactions
description: "Transaction guidance: build, sign, submit patterns. Provides templates (no execution). Use operator skill to execute."
allowed-tools:
  - Read
user-invocable: true
context:
  - "!cardano-cli version 2>&1 | head -5"
---

# cardano-cli-transactions

> **This is a guidance skill.** Provides templates and explanations. For execution, use `cardano-cli-transactions-operator`.

## When to use
- Learning transaction building patterns
- Understanding UTxO selection and change
- Getting command templates for various tx types

## Operating rules (must follow)
- Confirm network (mainnet/preprod/preview) before providing commands
- Never execute—only provide templates
- Always include fee handling and change
- Include verification steps

## Workflow templates

### Simple ADA transfer
```bash
# 1. Query sender UTxOs
cardano-cli conway query utxo \
  --address <sender-addr> \
  --testnet-magic 1

# 2. Get protocol parameters
cardano-cli conway query protocol-parameters \
  --testnet-magic 1 \
  --out-file pparams.json

# 3. Build transaction
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo-hash>#<index> \
  --tx-out <recipient>+<amount-lovelace> \
  --change-address <sender-addr> \
  --out-file tx.unsigned

# 4. Sign
cardano-cli conway transaction sign \
  --tx-file tx.unsigned \
  --signing-key-file payment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

# 5. Submit
cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed

# 6. Verify
cardano-cli conway transaction txid --tx-file tx.signed
```

### Multi-output transaction
```bash
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --tx-out <addr1>+<amount1> \
  --tx-out <addr2>+<amount2> \
  --tx-out <addr3>+<amount3> \
  --change-address <sender> \
  --out-file tx.unsigned
```

### With native tokens
```bash
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --tx-out "<recipient>+<lovelace>+<amount> <policy-id>.<asset-name>" \
  --change-address <sender> \
  --out-file tx.unsigned
```

### With metadata
```bash
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --tx-out <recipient>+<amount> \
  --change-address <sender> \
  --metadata-json-file metadata.json \
  --out-file tx.unsigned
```

## Examples

### Example: Send 10 ADA on preprod
**User request:** "Send 10 ADA to addr_test1qz..."

**Response:**
```bash
# === Send 10 ADA on Preprod ===

# Setup
SENDER_ADDR="addr_test1..."
RECIPIENT="addr_test1qz..."
AMOUNT=10000000  # 10 ADA in lovelace

# 1. Query UTxOs
cardano-cli conway query utxo \
  --address $SENDER_ADDR \
  --testnet-magic 1

# 2. Build (using first UTxO with sufficient funds)
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <paste-utxo-here>#0 \
  --tx-out "$RECIPIENT+$AMOUNT" \
  --change-address $SENDER_ADDR \
  --out-file tx.unsigned

# 3. Sign
cardano-cli conway transaction sign \
  --tx-file tx.unsigned \
  --signing-key-file payment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

# 4. Submit (use operator skill or run manually)
# cardano-cli conway transaction submit --testnet-magic 1 --tx-file tx.signed
```

## Tx bundle folder structure
```
tx-20240115-143022/
├── pparams.json       # Protocol parameters used
├── tx.unsigned        # Unsigned transaction
├── tx.signed          # Signed transaction
├── utxos.json         # Input UTxOs at build time
└── commands.log       # Commands executed
```

## Safety / key handling
- Never expose signing keys
- Verify recipient address carefully
- On mainnet: small test transfer first
- Keep tx bundle for audit trail

## References
- `shared/PRINCIPLES.md`
- `cardano-cli-transactions-operator` (for execution)
