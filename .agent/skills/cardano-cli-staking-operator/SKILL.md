---
name: cardano-cli-staking-operator
description: "Execute staking operations: registration, delegation, rewards withdrawal. Manual invoke only."
allowed-tools:
  - Bash(cardano-cli:*)
  - Bash(cat:*)
  - Read
  - Write
disable-model-invocation: true
user-invocable: true
context:
  - "!cardano-cli version 2>&1 | head -5"
---

# cardano-cli-staking-operator

> **OPERATOR SKILL**: Executes staking operations that modify on-chain state. Requires explicit human invocation.

## When to use
- When ready to register stake key, delegate, or withdraw rewards
- After reviewing guidance from `cardano-cli-staking`

## Operating rules (must follow)
- Confirm network and pool ID before delegation
- Verify stake key registration status before operations
- **REQUIRE explicit confirmation before any certificate submission**
- Show deposit/fee costs before proceeding

## Pre-flight checklist
```
[ ] Network: ___________
[ ] Stake key files exist and are correct
[ ] Payment key for fees available
[ ] Pool ID verified (for delegation)
[ ] Current rewards balance checked
```

## Execution workflow

### Check stake status first
```bash
cardano-cli conway query stake-address-info \
  --testnet-magic 1 \
  --address $(cat stake.addr)
```

### Register stake key (if not registered)
```bash
# Create registration certificate
cardano-cli conway stake-address registration-certificate \
  --stake-verification-key-file stake.vkey \
  --out-file stake-reg.cert

# Build tx with certificate
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --change-address <payment-addr> \
  --certificate-file stake-reg.cert \
  --out-file tx.unsigned

# Sign with BOTH payment and stake keys
cardano-cli conway transaction sign \
  --tx-file tx.unsigned \
  --signing-key-file payment.skey \
  --signing-key-file stake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

# Submit (CONFIRM FIRST)
cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

### Delegate to pool
```bash
# Create delegation certificate
cardano-cli conway stake-address stake-delegation-certificate \
  --stake-verification-key-file stake.vkey \
  --stake-pool-id <pool-id-bech32> \
  --out-file delegation.cert

# Build, sign (payment + stake), submit
# ... same pattern as registration
```

### Withdraw rewards
```bash
# Check rewards balance first
cardano-cli conway query stake-address-info \
  --testnet-magic 1 \
  --address $(cat stake.addr)

# Build withdrawal tx
cardano-cli conway transaction build \
  --testnet-magic 1 \
  --tx-in <utxo>#<index> \
  --withdrawal $(cat stake.addr)+<reward-amount> \
  --change-address <payment-addr> \
  --out-file tx.unsigned

# Sign with BOTH keys and submit
```

## Safety / key handling
- Verify pool ID from multiple sources before delegating
- Double-check reward withdrawal amounts
- Keep certificates for records
- Registration deposit is refundable on de-registration

## References
- `cardano-cli-staking` (guidance skill)
- `shared/PRINCIPLES.md`
