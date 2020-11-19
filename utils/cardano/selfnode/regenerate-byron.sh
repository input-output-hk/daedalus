#!/usr/bin/env bash

set -euo pipefail

cat > byron.genesis.spec.json <<EOF
{
    "heavyDelThd": "300000000000",
    "maxBlockSize": "2000000",
    "maxHeaderSize": "2000000",
    "maxProposalSize": "700",
    "maxTxSize": "4096",
    "mpcThd": "20000000000000",
    "scriptVersion": 0,
    "PBftSignatureThreshold": 1,
    "slotDuration": "250",
    "softforkRule": {
      "initThd": "900000000000000",
      "minThd": "600000000000000",
      "thdDecrement": "50000000000000"
    },
    "txFeePolicy": {
      "multiplier": "43000000000",
      "summand": "155381000000000"
    },
    "unlockStakeEpoch": "18446744073709551615",
    "updateImplicit": "10000",
    "updateProposalThd": "100000000000000",
    "updateVoteThd": "1000000000000"
}
EOF

cardano-cli genesis \
   --protocol-magic 764824073 \
   --start-time 2500000 \
   --k 8 \
   --n-poor-addresses 0 \
   --n-delegate-addresses 1 \
   --total-balance 50 \
   --byron-formats \
   --delegate-share 1 \
   --avvm-entry-count 0 \
   --avvm-entry-balance 0 \
   --protocol-parameters-file byron.genesis.spec.json \
   --genesis-output-dir tmp

yq -y . < tmp/genesis.json > byron-genesis-init.yaml
mv -vf tmp/delegate-keys.*.key .
mv -vf tmp/delegation-cert.*.json .

echo "Byron genesis created."
echo "Now merge byron-genesis-init.yaml into byron-genesis.yaml"
