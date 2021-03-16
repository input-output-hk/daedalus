#!/usr/bin/env bash

set -euo pipefail

# Script for generating faucet addresses for some of the integration test
# setup that relies on the CLI.
#
# Generates byron key file, shelley key file, address.
# The addresses can be inserted to the byron genesis.
# The corresponding TxIn will then be blake2d256 of the address.
cd `dirname $0`
dir=faucet-addrs
rm -f $dir/*
for i in $(seq 200);
do
  # Generate payment key and addresses for payment and genesis utxo
  cardano-cli byron key keygen --secret $dir/faucet$i.byron.key
  cardano-cli \
    byron key signing-key-address \
    --byron-formats \
    --mainnet \
    --secret $dir/faucet$i.byron.key \
    | head -n 1 > $dir/faucet$i.addr

  cardano-cli key convert-byron-key \
    --byron-payment-key-type \
    --byron-signing-key-file $dir/faucet$i.byron.key \
    --out-file $dir/faucet$i.shelley.key
done
