# Staking templates (high level)

## Build stake address
```bash
cardano-cli stake-address build       --stake-verification-key-file stake.vkey       --out-file stake.addr       --testnet-magic <MAGIC>
```

## Query stake address info
```bash
cardano-cli query stake-address-info       --address $(cat stake.addr)       --testnet-magic <MAGIC>
```

## Create certificates
```bash
# Register
cardano-cli stake-address registration-certificate       --stake-verification-key-file stake.vkey       --out-file stake.reg.cert

# Delegate
cardano-cli stake-address delegation-certificate       --stake-verification-key-file stake.vkey       --stake-pool-id <POOL_ID>       --out-file stake.deleg.cert
```

> Transactions that include certificates are built/signed/submitted like standard txs, but add `--certificate-file ...`.
