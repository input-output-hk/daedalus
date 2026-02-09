---
name: cardano-cli-wallets
description: "Wallet guidance: key generation, address building, UTxO queries. Provides templates and explanations (no execution)."
allowed-tools:
  - Read
user-invocable: true
---

# cardano-cli-wallets

> **This is a guidance skill.** It provides templates and explanations but does not execute commands. For execution, use `cardano-cli-wallets-operator`.

## When to use
- Learning how to create payment/stake keys and addresses
- Understanding wallet structure and UTxO model
- Getting copy-paste command templates

## Operating rules (must follow)
- Confirm network (mainnet vs preprod/preview) before providing commands
- Never execute commands—only provide templates
- Include verification steps in every template
- Keep secrets out of examples

## Workflow
1) Identify network
   - Ask for **mainnet** or **preprod/preview** and magic number
   - Determine CLI style (era-prefixed vs legacy) via doctor

2) Provide key generation template
   ```bash
   # Create wallet directory
   mkdir -p wallet && cd wallet

   # Generate payment keys
   cardano-cli conway address key-gen \
     --verification-key-file payment.vkey \
     --signing-key-file payment.skey

   # Generate stake keys
   cardano-cli conway stake-address key-gen \
     --verification-key-file stake.vkey \
     --signing-key-file stake.skey

   # Secure keys
   chmod 600 *.skey
   ```

3) Provide address building template
   ```bash
   # Build base address (payment + stake)
   cardano-cli conway address build \
     --payment-verification-key-file payment.vkey \
     --stake-verification-key-file stake.vkey \
     --out-file base.addr \
     --testnet-magic 1  # or --mainnet

   # Build enterprise address (payment only)
   cardano-cli conway address build \
     --payment-verification-key-file payment.vkey \
     --out-file enterprise.addr \
     --testnet-magic 1
   ```

4) Provide UTxO query template
   ```bash
   cardano-cli conway query utxo \
     --address $(cat base.addr) \
     --testnet-magic 1
   ```

5) Wallet dossier output format
   ```
   === Wallet Dossier ===
   Network: preprod (magic 1)
   Base Address: addr_test1...
   Enterprise Address: addr_test1...
   Payment VKey Hash: abc123...
   Stake VKey Hash: def456...
   Files: payment.vkey, payment.skey, stake.vkey, stake.skey
   ```

## Examples

### Example: Create preprod wallet
**User request:** "Create a wallet for preprod testing"

**Response:**
```bash
# === Preprod Wallet Setup ===

# 1. Create directory
mkdir -p ~/cardano/preprod-wallet && cd ~/cardano/preprod-wallet

# 2. Generate payment keypair
cardano-cli conway address key-gen \
  --verification-key-file payment.vkey \
  --signing-key-file payment.skey

# 3. Generate stake keypair
cardano-cli conway stake-address key-gen \
  --verification-key-file stake.vkey \
  --signing-key-file stake.skey

# 4. Build base address
cardano-cli conway address build \
  --payment-verification-key-file payment.vkey \
  --stake-verification-key-file stake.vkey \
  --out-file base.addr \
  --testnet-magic 1

# 5. Secure keys
chmod 600 *.skey

# 6. Display address for funding
echo "Fund this address from faucet:"
cat base.addr

# 7. Verify (after funding)
cardano-cli conway query utxo \
  --address $(cat base.addr) \
  --testnet-magic 1
```

## Safety / key handling
- Never paste `.skey` contents into chat
- Prefer offline key generation for real funds
- Lock permissions: `chmod 600 *.skey`
- Avoid cloud sync for key directories

## References
- `shared/PRINCIPLES.md` (repo)
- [Cardano Docs: Keys and Addresses](https://docs.cardano.org)
