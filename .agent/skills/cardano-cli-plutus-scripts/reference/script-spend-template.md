# Script spend template (CLI)

> Verify flags with your installed `cardano-cli`. Script spend syntax differs slightly between versions.

```bash
export MAGIC=<MAGIC>
export CHANGE_ADDR=$(cat wallets/alice/base.addr)
export SCRIPT_UTXO=<SCRIPT_TXHASH#IX>
export COLLATERAL_UTXO=<COLLATERAL_TXHASH#IX>

cardano-cli query protocol-parameters --testnet-magic $MAGIC --out-file pparams.json

cardano-cli transaction build       --testnet-magic $MAGIC       --tx-in "$SCRIPT_UTXO"       --tx-in-script-file validator.plutus       --tx-in-inline-datum-present       --tx-in-redeemer-file redeemer.json       --tx-in-collateral "$COLLATERAL_UTXO"       --change-address "$CHANGE_ADDR"       --tx-out "<RECIPIENT_ADDR>+<LOVELACE>"       --out-file tx.body

cardano-cli transaction sign       --testnet-magic $MAGIC       --tx-body-file tx.body       --signing-key-file wallets/alice/payment.skey       --out-file tx.signed

cardano-cli transaction submit       --testnet-magic $MAGIC       --tx-file tx.signed
```
