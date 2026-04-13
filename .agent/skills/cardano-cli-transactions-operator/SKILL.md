---
name: cardano-cli-transactions-operator
description: "Execute transaction builds and submits. Manual invoke only—requires explicit confirmation before submit."
allowed-tools:
  - Bash(cardano-cli:*)
  - Bash(mkdir:*)
  - Bash(cat:*)
  - Read
  - Write
disable-model-invocation: true
user-invocable: true
context:
  - "!cardano-cli version 2>&1 | head -5"
  - "!cardano-cli conway --help 2>&1 | head -10"
---

# cardano-cli-transactions-operator

> **OPERATOR SKILL**: This skill executes commands including transaction submission. Requires explicit human invocation.

## When to use
- When ready to build and submit a transaction
- After reviewing guidance from `cardano-cli-transactions`

## Operating rules (must follow)
- Confirm network before ANY operation
- Show full transaction details before signing
- **REQUIRE explicit "yes" confirmation before submit**
- Create tx bundle folder for reproducibility
- Verify UTxO changes after submit

## Pre-flight checklist
```
[ ] Network confirmed: ___________
[ ] Sender address verified
[ ] Recipient address verified (check first/last 8 chars)
[ ] Amount confirmed: ___________ lovelace
[ ] Signing key path confirmed
[ ] UTxO inputs selected
```

## Execution workflow

### Step 1: Setup tx bundle
```bash
TX_DIR="tx-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$TX_DIR" && cd "$TX_DIR"
```

### Step 2: Query UTxOs
```bash
cardano-cli conway query utxo \
  --address <sender-addr> \
  --testnet-magic 1 \
  --out-file utxos.json
```

### Step 3: Fetch protocol parameters
```bash
cardano-cli conway query protocol-parameters \
  --testnet-magic 1 \
  --out-file pparams.json
```

### Step 4: Build transaction
```bash
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --tx-out <recipient>+<amount> \
  --change-address <sender> \
  --out-file tx.unsigned
```

### Step 5: Sign (show what's being signed)
```bash
echo "=== SIGNING TRANSACTION ==="
echo "Inputs: <utxo>"
echo "Output: <recipient> receives <amount> lovelace"
echo "Change: returns to <sender>"

cardano-cli conway transaction sign \
  --tx-file tx.unsigned \
  --signing-key-file <payment.skey> \
  --testnet-magic 1 \
  --out-file tx.signed
```

### Step 6: Submit (REQUIRES CONFIRMATION)
```
⚠️  CONFIRM SUBMISSION ⚠️
Network: preprod
Sending: X ADA to addr_test1...
From: addr_test1...

Type 'yes' to submit:
```

```bash
cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

### Step 7: Verify
```bash
# Get tx hash
cardano-cli conway transaction txid --tx-file tx.signed

# Query recipient UTxO
cardano-cli conway query utxo \
  --address <recipient> \
  --testnet-magic 1
```

## Safety / key handling
- Never submit without explicit user confirmation
- For mainnet: require double confirmation
- Keep tx bundle for audit trail
- Recommend small test tx first on mainnet

## References
- `cardano-cli-transactions` (guidance skill)
- `shared/PRINCIPLES.md`
