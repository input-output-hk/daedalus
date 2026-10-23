# shellcheck shell=bash
# Local single-producer Conway testnet and the cardano-wallet process under
# test. Sourced by scenario.sh after lib.sh.
#
# The testnet starts at protocol version 11.0 with 0.2 s slots and 150-slot
# (30 s) epochs, f = 0.2 and k = 5, so 3k/f = 75 and 4k/f = 100 both fit in
# one epoch. Reserves are left in place, so every epoch pays rewards. A
# single-producer testnet cannot resume after a stop longer than its forecast
# window, so every run starts from a fresh genesis.

# A UNIX socket path is limited to 107 bytes, so the socket lives in a short
# directory of its own rather than under OUT.
SOCK_DIR=$(mktemp -d "${XDG_RUNTIME_DIR:-/tmp}/drep-harness.XXXXXX")
SOCK="$SOCK_DIR/node.socket"
export CARDANO_NODE_SOCKET_PATH="$SOCK"
export CARDANO_NODE_NETWORK_ID="$MAGIC"

port_in_use() {
  [ -n "$(ss -Hltn "sport = :$1")" ]
}

cluster_start() {
  local port
  for port in "$NODE_PORT" "$WALLET_PORT"; do
    if port_in_use "$port"; then
      die "port $port is already in use; choose others with --node-port and --wallet-port"
    fi
  done
  mkdir -p "$STATE/spec" "$STATE/node"

  "$CLI" conway genesis create-testnet-data --testnet-magic "$MAGIC" \
    --out-dir "$STATE/probe" >/dev/null
  jq --argjson pv "$PV_MAJOR" '
      .slotLength = 0.2 | .epochLength = 150 | .activeSlotsCoeff = 0.2
    | .securityParam = 5
    | .protocolParams.protocolVersion = {major: $pv, minor: 0}
    | .protocolParams.keyDeposit = 2000000
    | .protocolParams.poolDeposit = 500000000
    | .protocolParams.rho = 0.003 | .protocolParams.tau = 0.2
    | .protocolParams.nOpt = 2
    | .protocolParams.minFeeA = 44 | .protocolParams.minFeeB = 155381
    | .protocolParams.maxTxSize = 16384
    | .protocolParams.maxBlockBodySize = 90112
    | .protocolParams.minUTxOValue = 0' \
    "$STATE/probe/shelley-genesis.json" >"$STATE/spec/shelley.json"
  jq '.dRepDeposit = 500000000 | .govActionDeposit = 100000000000
    | .dRepActivity = 20 | .govActionLifetime = 6' \
    "$STATE/probe/conway-genesis.json" >"$STATE/spec/conway.json"
  cp "$STATE/probe/alonzo-genesis.json" "$STATE/spec/alonzo.json"
  rm -rf "$STATE/probe"

  "$CLI" conway genesis create-testnet-data --testnet-magic "$MAGIC" \
    --spec-shelley "$STATE/spec/shelley.json" \
    --spec-alonzo "$STATE/spec/alonzo.json" \
    --spec-conway "$STATE/spec/conway.json" \
    --pools 2 --stake-delegators 2 --utxo-keys 2 \
    --total-supply 3000000000000000 --delegated-supply 1000000000000000 \
    --out-dir "$STATE/data" >/dev/null
  # The producer must be running before the chain's start time plus one
  # forecast window (3k/f slots, 15 s); after that, with no blocks yet, it
  # cannot tell the current slot and never forges. So the start time is set
  # just before the node launches, and not in the future: a node that waits
  # for a future start and wakes before it reports the slot as unknown and
  # does not look again for about a minute, past the window.
  local start
  start=$(date +%s)
  jq --arg t "$(date -u -d "@$start" +%Y-%m-%dT%H:%M:%SZ)" \
    '.systemStart = $t | .maxLovelaceSupply = 45000000000000000' \
    "$STATE/data/shelley-genesis.json" >"$STATE/data/shelley-genesis.tmp"
  mv "$STATE/data/shelley-genesis.tmp" "$STATE/data/shelley-genesis.json"
  jq --argjson t "$start" '.startTime = $t' "$STATE/data/byron-genesis.json" \
    >"$STATE/data/byron-genesis.tmp"
  mv "$STATE/data/byron-genesis.tmp" "$STATE/data/byron-genesis.json"

  cat >"$STATE/node/config.json" <<JSON
{
  "Protocol": "Cardano",
  "RequiresNetworkMagic": "RequiresMagic",
  "ByronGenesisFile": "$STATE/data/byron-genesis.json",
  "ShelleyGenesisFile": "$STATE/data/shelley-genesis.json",
  "AlonzoGenesisFile": "$STATE/data/alonzo-genesis.json",
  "ConwayGenesisFile": "$STATE/data/conway-genesis.json",
  "DijkstraGenesisFile": "$STATE/data/dijkstra-genesis.json",
  "PBftSignatureThreshold": 0.6,
  "LastKnownBlockVersion-Major": 10,
  "LastKnownBlockVersion-Minor": 0,
  "LastKnownBlockVersion-Alt": 0,
  "ExperimentalProtocolsEnabled": true,
  "TestShelleyHardForkAtEpoch": 0,
  "TestAllegraHardForkAtEpoch": 0,
  "TestMaryHardForkAtEpoch": 0,
  "TestAlonzoHardForkAtEpoch": 0,
  "TestBabbageHardForkAtEpoch": 0,
  "TestConwayHardForkAtEpoch": 0,
  "PeerSharing": false,
  "TraceOptions": {
    "": {"severity": "Notice", "backends": ["Stdout MachineFormat"]},
    "Mempool": {"severity": "Info"},
    "Forge.Loop": {"severity": "Notice"}
  }
}
JSON
  echo '{"localRoots": [], "publicRoots": [], "useLedgerAfterSlot": -1}' \
    >"$STATE/node/topology.json"

  local pool="$STATE/data/pools-keys/pool1"
  "$NODE" run \
    --config "$STATE/node/config.json" --topology "$STATE/node/topology.json" \
    --database-path "$STATE/node/db" --socket-path "$SOCK" \
    --host-addr 127.0.0.1 --port "$NODE_PORT" \
    --shelley-kes-key "$pool/kes.skey" --shelley-vrf-key "$pool/vrf.skey" \
    --shelley-operational-certificate "$pool/opcert.cert" \
    >"$OUT/node.log" 2>&1 &
  echo $! >"$STATE/node.pid"

  local _
  for _ in $(seq 1 90); do
    [ -S "$SOCK" ] && break
    sleep 1
  done
  [ -S "$SOCK" ] || die "node socket did not appear (see $OUT/node.log)"
  local forging=''
  for _ in $(seq 1 60); do
    if "$CLI" conway query tip 2>/dev/null | jq -e '(.block // 0) > 0' >/dev/null; then
      forging=1
      break
    fi
    sleep 2
  done
  [ -n "$forging" ] || die "the node produced no block (see $OUT/node.log)"
  log "node up: $("$CLI" conway query tip | jq -c '{era, epoch, slot}')," \
    "protocol $("$CLI" conway query protocol-parameters | jq -c '.protocolVersion')"
  wallet_start
}

wallet_start() {
  wallet_spawn
  wallet_wait_ready
}

# Starts the wallet and returns once its API answers, before it is ready.
wallet_spawn() {
  "$WALLET" serve --node-socket "$SOCK" \
    --testnet "$STATE/data/byron-genesis.json" \
    --database "$STATE/wallet-db" --listen-address 127.0.0.1 \
    --port "$WALLET_PORT" >>"$OUT/wallet.log" 2>&1 &
  echo $! >"$STATE/wallet.pid"
  local _
  for _ in $(seq 1 240); do
    curl -s -o /dev/null --max-time 2 "$API/v2/network/information" && return 0
    sleep 0.5
  done
  die "the wallet API did not answer (see $OUT/wallet.log)"
}

wallet_wait_ready() {
  local _
  for _ in $(seq 1 120); do
    if curl -sf "$API/v2/network/information" 2>/dev/null |
      jq -e '.sync_progress.status == "ready"' >/dev/null; then
      log "wallet up: $("$WALLET" version)"
      return 0
    fi
    sleep 1
  done
  die "wallet did not become ready (see $OUT/wallet.log)"
}

wallet_stop() {
  local pid
  pid=$(cat "$STATE/wallet.pid" 2>/dev/null || true)
  [ -n "$pid" ] || return 0
  kill "$pid" 2>/dev/null || true
  local _
  for _ in $(seq 1 30); do
    kill -0 "$pid" 2>/dev/null || break
    sleep 1
  done
  kill -9 "$pid" 2>/dev/null || true
  rm -f "$STATE/wallet.pid"
}

# Restart the wallet on the same database. Its DRep registry is a
# local-state-query result cached for 15 minutes and filled at start, so a
# restart is how a run makes the registry reflect the current ledger.
wallet_restart() {
  wallet_stop
  wallet_start
  local id _
  for id in "$@"; do
    for _ in $(seq 1 120); do
      [ "$(wallet_json "$id" | jq -r '.state.status')" = ready ] && break
      sleep 1
    done
  done
}

cluster_stop() {
  wallet_stop
  local pid
  pid=$(cat "$STATE/node.pid" 2>/dev/null || true)
  if [ -n "$pid" ]; then
    kill "$pid" 2>/dev/null || true
    local _
    for _ in $(seq 1 30); do
      kill -0 "$pid" 2>/dev/null || break
      sleep 1
    done
    kill -9 "$pid" 2>/dev/null || true
    rm -f "$STATE/node.pid"
  fi
  rm -rf "$SOCK_DIR"
}
