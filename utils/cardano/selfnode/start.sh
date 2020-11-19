#!/usr/bin/env bash

set -euo pipefail

if type -p gdate > /dev/null; then
  gnu_date=gdate
else
  gnu_date=date
fi

systemStart=$($gnu_date --iso-8601=s --date="5 seconds")

config_dir=lib/shelley/test/data/cardano-node-shelley

mkdir -p ${state_dir:=bft-node}

yq -y '. + { GenesisFile: "genesis.json", minSeverity: "Info" }' < $config_dir/node.config > $state_dir/node.config

yq ".systemStart=\"$(date --iso-8601=s --date='5 seconds')\"" < $config_dir/genesis.yaml > $state_dir/genesis.json

set -x

exec cardano-node run --port 40000 \
     --config $state_dir/node.config \
     --topology lib/byron/test/data/cardano-node-byron/node.topology \
     --database-path $state_dir/node.db \
     --socket-path $state_dir/node.socket \
     --shelley-vrf-key $config_dir/node-vrf.skey \
     --shelley-kes-key $config_dir/node-kes.skey \
     --shelley-operational-certificate $config_dir/node.opcert
