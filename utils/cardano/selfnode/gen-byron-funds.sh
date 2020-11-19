#!/bin/bash

# Script for generating faucet addresses for some of the integration test
# setup that relies on the CLI.
#
# Generates byron key file, shelley key file, address.
# The addresses can be inserted to the byron genesis.
# The corresponding TxIn will then be blake2d256 of the address.
dir=faucet-addrs
rm $dir/*
for i in $(seq 100);
do
  # Generate payment key and addresses for payment and genesis utxo
  cardano-cli keygen --byron-formats --secret $dir/faucet$i.byron.key --no-password
  cardano-cli \
    signing-key-address \
    --byron-formats \
    --mainnet \
    --secret $dir/faucet$i.byron.key \
    | head -n 1 > $dir/faucet$i.addr

  cardano-cli shelley key convert-byron-key \
    --byron-payment-key-type \
    --byron-signing-key-file $dir/faucet$i.byron.key \
    --out-file $dir/faucet$i.shelley.key
done


