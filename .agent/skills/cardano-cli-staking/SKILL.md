---
name: cardano-cli-staking
description: "Staking guidance: registration, delegation, rewards. Provides templates (no execution). Use operator skill to execute."
allowed-tools:
  - Read
user-invocable: true
---

# cardano-cli-staking

> **This is a guidance skill.** Provides templates and explanations. For execution, use `cardano-cli-staking-operator`.

## When to use
- Learning stake key registration and delegation
- Understanding rewards withdrawal
- Getting command templates for staking operations

## Operating rules (must follow)
- Confirm network before providing commands
- Never execute—only provide templates
- Include deposit/fee information
- Verify pool ID from multiple sources

## Key concepts

### Stake addresses vs payment addresses
- **Payment address**: Holds funds, used for spending
- **Stake address**: Controls delegation, receives rewards
- **Base address**: Combines both (most common)

### Deposits
- Stake key registration: ~2 ADA deposit (refundable)
- Pool registration: ~500 ADA deposit (refundable)

## Workflow templates

### Check stake status
```bash
# Derive stake address
cardano-cli conway stake-address build \
  --stake-verification-key-file stake.vkey \
  --testnet-magic 1 \
  --out-file stake.addr

# Query status
cardano-cli conway query stake-address-info \
  --testnet-magic 1 \
  --address $(cat stake.addr)
```

### Register stake key
```bash
# 1. Create registration certificate
cardano-cli conway stake-address registration-certificate \
  --stake-verification-key-file stake.vkey \
  --out-file stake-reg.cert

# 2. Build transaction with certificate
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --change-address <payment-addr> \
  --certificate-file stake-reg.cert \
  --out-file tx.unsigned

# 3. Sign with BOTH keys
cardano-cli conway transaction sign \
  --tx-file tx.unsigned \
  --signing-key-file payment.skey \
  --signing-key-file stake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

# 4. Submit
cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

### Delegate to pool
```bash
# 1. Create delegation certificate
cardano-cli conway stake-address stake-delegation-certificate \
  --stake-verification-key-file stake.vkey \
  --stake-pool-id pool1... \
  --out-file delegation.cert

# 2. Build, sign (both keys), submit
# Same pattern as registration
```

### Withdraw rewards
```bash
# 1. Check rewards balance
cardano-cli conway query stake-address-info \
  --testnet-magic 1 \
  --address $(cat stake.addr)

# 2. Build withdrawal tx
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --withdrawal $(cat stake.addr)+<reward-lovelace> \
  --change-address <payment-addr> \
  --out-file tx.unsigned

# 3. Sign with BOTH keys, submit
```

### De-register stake key (get deposit back)
```bash
cardano-cli conway stake-address deregistration-certificate \
  --stake-verification-key-file stake.vkey \
  --out-file stake-dereg.cert

# Build tx with certificate, sign, submit
```

## Examples

### Example: Delegate to a pool
**User request:** "Delegate my stake to pool1abc..."

**Response:**
```bash
# === Delegate to Pool ===

POOL_ID="pool1abc..."
STAKE_VKEY="stake.vkey"
PAYMENT_ADDR=$(cat base.addr)

# 1. Verify pool exists
cardano-cli conway query stake-pools --testnet-magic 1 | grep $POOL_ID

# 2. Create delegation certificate
cardano-cli conway stake-address stake-delegation-certificate \
  --stake-verification-key-file $STAKE_VKEY \
  --stake-pool-id $POOL_ID \
  --out-file delegation.cert

# 3. Build transaction
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <your-utxo>#0 \
  --change-address $PAYMENT_ADDR \
  --certificate-file delegation.cert \
  --out-file tx.unsigned

# 4. Sign with both keys
cardano-cli conway transaction sign \
  --tx-file tx.unsigned \
  --signing-key-file payment.skey \
  --signing-key-file stake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

# 5. Submit (use operator skill)
# Delegation takes effect at next epoch boundary
```

## Safety / key handling
- Verify pool ID from multiple sources (adapools, cexplorer)
- Staking never risks your principal
- Delegation can be changed anytime
- Rewards accumulate until withdrawn

## References
- `shared/PRINCIPLES.md`
- `cardano-cli-staking-operator` (for execution)
