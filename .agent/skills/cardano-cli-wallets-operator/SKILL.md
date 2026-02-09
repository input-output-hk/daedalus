---
name: cardano-cli-wallets-operator
description: "Execute wallet operations: key generation, address building. Manual invoke only for safety."
allowed-tools:
  - Bash(cardano-cli:*)
  - Bash(mkdir:*)
  - Bash(chmod:*)
  - Bash(cat:*)
  - Read
  - Write
disable-model-invocation: true
user-invocable: true
---

# cardano-cli-wallets-operator

> **OPERATOR SKILL**: This skill executes commands. It requires explicit human invocation and cannot be auto-triggered by the model.

## When to use
- When you need to actually execute wallet creation commands
- After reviewing the guidance from `cardano-cli-wallets`

## Operating rules (must follow)
- Confirm network (mainnet/preprod/preview) before ANY execution
- Always create a dedicated directory for wallet files
- Set `chmod 600 *.skey` immediately after key generation
- Show the user what will be executed before running
- Verify each step succeeded before continuing

## Execution workflow

### Step 1: Confirm parameters
```
Network: [mainnet/preprod/preview]
Magic: [1 for preprod, 2 for preview, none for mainnet]
Wallet directory: [path]
```

### Step 2: Create directory
```bash
mkdir -p <wallet-dir> && cd <wallet-dir>
```

### Step 3: Generate keys (with confirmation)
```bash
# Payment keys
cardano-cli conway address key-gen \
  --verification-key-file payment.vkey \
  --signing-key-file payment.skey

# Stake keys
cardano-cli conway stake-address key-gen \
  --verification-key-file stake.vkey \
  --signing-key-file stake.skey

# Secure immediately
chmod 600 *.skey
```

### Step 4: Build addresses
```bash
# Base address
cardano-cli conway address build \
  --payment-verification-key-file payment.vkey \
  --stake-verification-key-file stake.vkey \
  --out-file base.addr \
  --testnet-magic 1  # adjust per network
```

### Step 5: Verify and report
```bash
echo "=== Wallet Created ==="
echo "Base Address: $(cat base.addr)"
ls -la *.vkey *.skey *.addr
```

## Safety / key handling
- NEVER display .skey file contents
- ALWAYS chmod 600 immediately after generation
- For mainnet: strongly recommend offline generation
- Verify directory permissions before proceeding

## References
- `cardano-cli-wallets` (guidance skill)
- `shared/PRINCIPLES.md`
