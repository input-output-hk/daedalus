#!/usr/bin/env bash

# This loosely follows the instructions at:
#   https://github.com/input-output-hk/cardano-node/blob/master/doc/shelley-genesis.md

set -euo pipefail

cardano-cli shelley genesis create \
  --genesis-dir . \
  --mainnet \
  --gen-genesis-keys 1 \
  --gen-utxo-keys 1 \
  --supply 45000000000000000

mv delegate-keys/delegate1.counter bft-leader.counter
mv delegate-keys/delegate1.skey bft-leader.skey
mv delegate-keys/delegate1.vkey bft-leader.vkey
mv delegate-keys/delegate1.vrf.vkey bft-leader.vrf.vkey
mv delegate-keys/delegate1.vrf.skey bft-leader.vrf.skey

rm -r delegate-keys genesis-keys utxo-keys genesis.spec.json

cardano-cli shelley node key-gen-KES \
    --verification-key-file bft-leader.kes.vkey \
    --signing-key-file bft-leader.kes.skey

cardano-cli shelley node issue-op-cert \
  --hot-kes-verification-key-file bft-leader.kes.vkey \
  --cold-signing-key-file bft-leader.skey \
  --operational-certificate-issue-counter bft-leader.counter \
  --kes-period 0 \
  --out-file bft-leader.opcert

echo "To be added to the genDelegs section of genesis.yaml:"
jq '.genDelegs|{genDelegs: .}' < genesis.json
rm genesis.json
