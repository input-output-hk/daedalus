# Send ADA (standard tx) — template

> This is a template. Verify syntax with your installed `cardano-cli`.

## Online build flow (`transaction build`)
```bash
export MAGIC=<MAGIC>
export FROM_ADDR=$(cat wallets/alice/base.addr)
export TO_ADDR=<RECIPIENT_ADDR>
export AMOUNT=2000000   # 2 ADA in lovelace

cardano-cli query utxo --address "$FROM_ADDR" --testnet-magic $MAGIC

cardano-cli query protocol-parameters --testnet-magic $MAGIC --out-file pparams.json

cardano-cli transaction build       --testnet-magic $MAGIC       --change-address "$FROM_ADDR"       --tx-in <TXHASH#IX>       --tx-out "$TO_ADDR+$AMOUNT"       --out-file tx.body

cardano-cli transaction sign       --testnet-magic $MAGIC       --tx-body-file tx.body       --signing-key-file wallets/alice/payment.skey       --out-file tx.signed

cardano-cli transaction submit       --testnet-magic $MAGIC       --tx-file tx.signed
```
