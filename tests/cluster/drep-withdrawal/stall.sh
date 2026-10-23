#!/usr/bin/env bash
# DRep list latency while the wallet's stake-pool database is busy.
#
# GET /v2/dreps and GET /v2/dreps/{id} read the DRep metadata table, which
# lives in the stake-pool database, and that database runs every query and
# transaction behind one lock. GET /v2/dreps/summary does not read it. This
# scenario times all three while the wallet does stake-pool work against a
# pool_production table of mainnet size:
#
#   startup     the stake-pool garbage collection of a retired pool, run by
#               the first block the wallet applies after it starts
#   pools-warm  GET /v2/stake-pools, which counts blocks per pool over the
#   pools-cold  whole table, with the database file cached and not cached
#   gc          the garbage collection of a pool retired on chain, run when
#               the wallet reaches the second epoch after the retirement
#
# pool_production has no index on pool_id, so the count and the deletion of
# a pool's rows both read the whole table. The cold cases drop the database
# file's pages from the OS page cache first, as a reboot does.
#
# The table is filled with synthetic rows at negative slots, which no
# rollback (slot > point) or real block reaches; real blocks are still
# recorded beside them. The rows go in in random order, which scatters the
# table's leaf pages through the file as a mainnet database's are: in a
# synced mainnet stake-pools.sqlite of about 8 million rows, 98 % of
# consecutive pool_production leaves are not adjacent in the file. Rows
# inserted in slot order would sit in file order and scan in seconds whether
# cached or not.
#
# Started by harness.sh --scenario stall, which sets the binaries, ports, OUT
# and STATE.
# shellcheck disable=SC2034
set -euo pipefail
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=lib.sh
source "$HERE/lib.sh"
# shellcheck source=cluster.sh
source "$HERE/cluster.sh"

mkdir -p "$OUT/api" "$STATE"
: >"$OUT/results.tsv"
: >"$OUT/api-calls.tsv"
: >"$OUT/fixtures.tsv"
: >"$STATE/scenario.env"
trap cluster_stop EXIT

POOL_DB="$STATE/wallet-db/stake-pools.sqlite"
LAT="$OUT/stall-latency.tsv"
: >"$LAT"
PROBES=()
PROBE_TIMEOUT=1800
LOOP_MAX=900
SYNTHETIC_POOLS=2000
RETIRED_POOL_ROWS=2000
MISSING_DREP=''
POOL2_HEX=''
GHOST_HEX=''

# The database file's pages leave the OS page cache; dirty pages are written
# first, since only clean pages can be dropped.
evict_pool_db() {
  sync "$POOL_DB"
  dd if="$POOL_DB" iflag=nocache count=0 status=none
}

warm_pool_db() {
  cat "$POOL_DB" >/dev/null
}

pool_db() {
  sqlite3 -batch -cmd '.timeout 60000' "$POOL_DB" "$@"
}

# sprobe KIND N PATH: one timed request in the background, appended to
# stall-latency.tsv as kind, n, start and end (Unix seconds), HTTP status
# (000 when curl gave up) and seconds.
sprobe() {
  local kind="$1" n="$2" path="$3"
  (
    start=$(date +%s.%N)
    out=$(curl -s -o /dev/null -w '%{http_code} %{time_total}' --max-time "$PROBE_TIMEOUT" "$API$path" || true)
    end=$(date +%s.%N)
    printf '%s\t%s\t%.2f\t%.2f\t%s\t%s\n' "$kind" "$n" "$start" "$end" "${out%% *}" "${out#* }" >>"$LAT"
  ) &
  PROBES+=($!)
}

wait_sprobes() {
  [ "${#PROBES[@]}" = 0 ] || wait "${PROBES[@]}"
  PROBES=()
}

# sstats KIND: median, maximum, count and unanswered requests of one kind.
sstats() {
  awk -F'\t' -v k="$1" '$1 == k { print $6, $5 }' "$LAT" | sort -n |
    awk '{ v[NR] = $1; if ($2 == "000") u++ } END {
      if (NR == 0) { print "no samples"; exit }
      m = (NR % 2) ? v[(NR + 1) / 2] : (v[NR / 2] + v[NR / 2 + 1]) / 2
      printf "median %.3f s, max %.3f s, n=%d", m, v[NR], NR
      if (u) printf ", %d unanswered", u }'
}

sstatuses() {
  awk -F'\t' -v k="$1" '$1 == k { print $5 }' "$LAT" | sort | uniq -c |
    awk '{ printf "%sHTTP %s x%s", (NR > 1 ? ", " : ""), $2, $1 }'
}

# The Unix time at which the wallet logged that it began removing pool HEX,
# or nothing.
removal_time() {
  local line
  line=$(sed -r 's/\x1b\[[0-9;]*m//g' "$OUT/wallet.log" |
    grep "Removing the following pool from the database: $1" | tail -1 || true)
  [ -n "$line" ] || return 0
  date -u -d "$(echo "$line" | sed -E 's/^.*\[([0-9-]+ [0-9:.]+) UTC\].*$/\1/')" +%s.%N
}

# hold_over KIND HEX: the wallet has begun removing pool HEX, and a list
# request of KIND sent after that answered within a second.
hold_over() {
  local t
  t=$(removal_time "$2")
  [ -n "$t" ] || return 1
  awk -F'\t' -v k="$1-list" -v t="$t" \
    '$1 == k && $3 > t && $5 == 200 && $6 < 1 { f = 1 } END { exit !f }' "$LAT"
}

# probe_loop KIND HEX MAX: what the Governance Center sends while it waits
# for the directory: the list every 2 s, a lookup of a DRep the registry does
# not hold every 5 s, the summary every 10 s. Stops once the removal of pool
# HEX is over (hold_over), or after MAX seconds.
probe_loop() {
  local kind="$1" hex="$2" max="$3" t0 i=0
  t0=$(date +%s)
  while [ $(($(date +%s) - t0)) -lt "$max" ]; do
    [ $((i % 2)) != 0 ] || sprobe "$kind-list" "$i" /v2/dreps
    [ $((i % 5)) != 0 ] || sprobe "$kind-lookup" "$i" "/v2/dreps/$MISSING_DREP"
    [ $((i % 10)) != 0 ] || sprobe "$kind-summary" "$i" /v2/dreps/summary
    if [ $((i % 2)) = 1 ] && hold_over "$kind" "$hex"; then
      break
    fi
    sleep 1
    i=$((i + 1))
  done
  wait_sprobes
}

# hold_report KIND HEX T0: when the removal of pool HEX began, relative to
# T0; when the first list request after it was answered, which is when the
# database was released; and how many list requests were waiting for that.
hold_report() {
  local kind="$1" hex="$2" t0="$3" t
  t=$(removal_time "$hex")
  [ -n "$t" ] || {
    echo "no removal of the pool logged"
    return 0
  }
  awk -F'\t' -v k="$kind-list" -v t="$t" -v t0="$t0" '
    FNR == NR { if ($1 == k && $4 > t + 0 && (r == "" || $4 < r)) r = $4; next }
    $1 == k && $3 < r && $4 >= r - 0.5 { waited++; if ($4 > last) last = $4 }
    END {
      printf "removal logged at +%.1f s, database released at +%.1f s (held %.1f s); %d list requests were waiting and answered by +%.1f s",
        t - t0, r - t0, r - t, waited, last - t0
    }' "$LAT" "$LAT"
}

# inflate_pool_db ROWS HEX...: adds ROWS synthetic pool_production rows, at
# slots -ROWS to -1 and in random order, spread over SYNTHETIC_POOLS pool
# ids; each HEX gets RETIRED_POOL_ROWS of them, spread evenly through the
# slots as a pool's blocks are over its lifetime. The wallet must be stopped.
inflate_pool_db() {
  local rows="$1" stride sql="" hex i=0
  shift
  stride=$((rows / RETIRED_POOL_ROWS))
  for hex in "$@"; do
    sql="$sql WHEN x % $stride = $i THEN '$hex'"
    i=$((i + 1))
  done
  pool_db <<SQL
BEGIN;
CREATE TEMP TABLE synthetic_pools (i INTEGER PRIMARY KEY, id TEXT);
WITH RECURSIVE c(x) AS (SELECT 0 UNION ALL SELECT x + 1 FROM c WHERE x < $((SYNTHETIC_POOLS - 1)))
INSERT INTO synthetic_pools SELECT x, lower(hex(randomblob(28))) FROM c;
CREATE TEMP TABLE synthetic_slots (x INTEGER);
WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c WHERE x < $rows)
INSERT INTO synthetic_slots SELECT x FROM c;
INSERT INTO pool_production (pool_id, slot, header_hash, parent_header_hash, block_height)
SELECT CASE $sql ELSE (SELECT id FROM synthetic_pools WHERE i = x % $SYNTHETIC_POOLS) END,
       x - $rows - 1, lower(hex(randomblob(32))), lower(hex(randomblob(32))), x
FROM synthetic_slots ORDER BY random();
COMMIT;
PRAGMA wal_checkpoint(TRUNCATE);
SQL
}

# add_retired_pool HEX: a registration and a retirement in epoch 1 for pool
# HEX, at slots 1 and 2, so the next garbage collection removes it. The
# wallet must be stopped.
add_retired_pool() {
  pool_db <<SQL
INSERT INTO pool_registration (pool_id, slot, slot_internal_index, margin_numerator,
  margin_denominator, cost, pledge, metadata_url, metadata_hash)
VALUES ('$1', 1, 0, 0, 1, 0, 0, NULL, NULL);
INSERT INTO pool_retirement (pool_id, slot, slot_internal_index, epoch) VALUES ('$1', 2, 0, 1);
SQL
}

seconds_to_epoch_end() {
  "$CLI" conway query tip | jq -r '.slotsToEpochEnd * 0.2 | floor'
}

current_epoch() {
  "$CLI" conway query tip | jq -r '.epoch'
}

# Setup ------------------------------------------------------------------------

setup() {
  cluster_start
  local k="$STATE/data/pools-keys/pool2"
  POOL2_HEX=$("$CLI" conway stake-pool id --cold-verification-key-file "$k/cold.vkey" --output-hex)
  GHOST_HEX=$(head -c 28 /dev/urandom | xxd -p -c 28)
  # Genesis pools have no registration certificate on chain, so the wallet's
  # pool registry never lists them. Re-registering is a parameter update and
  # takes no deposit.
  "$CLI" conway stake-pool registration-certificate \
    --cold-verification-key-file "$k/cold.vkey" --vrf-verification-key-file "$k/vrf.vkey" \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --pool-reward-account-verification-key-file "$k/staking-reward.vkey" \
    --pool-owner-stake-verification-key-file "$k/staking-reward.vkey" \
    --out-file "$k/onchain-reg.cert"
  cli_tx register-pool2 1 --certificate-file "$k/onchain-reg.cert" --witness-override 3 \
    -- "$k/cold.skey" "$k/staking-reward.skey" ||
    die "registering pool2: $CLI_RESULT"
  register_extra_dreps "$EXTRA_DREPS"

  mkdir -p "$STATE/missing-drep"
  "$CLI" conway governance drep key-gen --verification-key-file "$STATE/missing-drep/drep.vkey" \
    --signing-key-file "$STATE/missing-drep/drep.skey"
  MISSING_DREP=$("$CLI" conway governance drep id \
    --drep-verification-key-file "$STATE/missing-drep/drep.vkey" --output-cip129)

  local pool2 _ n=0
  pool2=$("$CLI" conway stake-pool id --cold-verification-key-file "$k/cold.vkey" --output-bech32)
  for _ in $(seq 1 60); do
    n=$(curl -s "$API/v2/stake-pools?stake=1000000" |
      jq --arg p "$pool2" '[.[] | select(.id == $p)] | length')
    [ "$n" = 1 ] && break
    sleep 3
  done
  [ "$n" = 1 ] || die "the wallet's pool registry does not list pool2"
}

# Cases ------------------------------------------------------------------------

startup() {
  # The collection removes pools retired by the current epoch minus 2, and
  # runs only from epoch 3; the first block after the restart must be there.
  while [ "$(current_epoch)" -lt 4 ]; do sleep 2; done
  wallet_stop
  local before
  before=$(pool_db "SELECT count(*) FROM pool_production")
  inflate_pool_db "$POOL_PRODUCTION_ROWS" "$GHOST_HEX" "$POOL2_HEX"
  add_retired_pool "$GHOST_HEX"
  local rows pools size
  rows=$(pool_db "SELECT count(*) FROM pool_production")
  pools=$(pool_db "SELECT count(DISTINCT pool_id) FROM pool_production")
  size=$(du -m "$POOL_DB" | cut -f1)
  result S-scale "Stake-pool database at mainnet size" "stake-pools.sqlite" "(setup)" \
    "pool_production $rows rows ($before real, $pools pool ids), file $size MB; $EXTRA_DREPS DReps registered" NOTE

  evict_pool_db
  local t0
  t0=$(date +%s.%N)
  wallet_spawn
  probe_loop startup "$GHOST_HEX" "$LOOP_MAX"
  result S-startup "DRep list latency at wallet start, while the first block's garbage collection removes a retired pool, database not cached" \
    "GET /v2/dreps every 2 s from start" "(measurement)" \
    "list $(sstats startup-list); lookups $(sstats startup-lookup), $(sstatuses startup-lookup); summary $(sstats startup-summary); $(hold_report startup "$GHOST_HEX" "$t0")" NOTE
  wallet_wait_ready
}

idle() {
  local i
  warm_pool_db
  for i in 1 2 3 4 5; do
    sprobe idle-list "$i" /v2/dreps
    wait_sprobes
    sprobe idle-summary "$i" /v2/dreps/summary
    wait_sprobes
  done
  result S-idle "DRep list latency, database cached, nothing else in flight" "GET /v2/dreps" \
    "(measurement)" "$(curl -s "$API/v2/dreps" | jq length) DReps: list $(sstats idle-list); summary $(sstats idle-summary)" NOTE
}

# stake_pools KIND: GET /v2/stake-pools as Daedalus sends it, with the list,
# a lookup and the summary sent 1, 2 and 3 s later.
stake_pools() {
  local kind="$1" i
  sprobe "$kind-stakepools" 1 "/v2/stake-pools?stake=1000000000"
  for i in 1 2 3; do
    sleep 1
    sprobe "$kind-list" "$i" /v2/dreps
    sprobe "$kind-lookup" "$i" "/v2/dreps/$MISSING_DREP"
    sprobe "$kind-summary" "$i" /v2/dreps/summary
  done
  wait_sprobes
}

pools_warm() {
  warm_pool_db
  stake_pools pools-warm
  result S-pools-warm "DRep list latency during GET /v2/stake-pools, database cached" \
    "GET /v2/dreps 1 to 3 s after GET /v2/stake-pools" "(measurement)" \
    "stake-pools $(sstats pools-warm-stakepools); list $(sstats pools-warm-list); lookups $(sstats pools-warm-lookup); summary $(sstats pools-warm-summary)" NOTE
}

pools_cold() {
  evict_pool_db
  stake_pools pools-cold
  result S-pools-cold "DRep list latency during GET /v2/stake-pools, database not cached" \
    "GET /v2/dreps 1 to 3 s after GET /v2/stake-pools" "(measurement)" \
    "stake-pools $(sstats pools-cold-stakepools); list $(sstats pools-cold-list); lookups $(sstats pools-cold-lookup); summary $(sstats pools-cold-summary)" NOTE
}

gc() {
  local k="$STATE/data/pools-keys/pool2" epoch boundary wait_s t0
  epoch=$(($(current_epoch) + 2))
  "$CLI" conway stake-pool deregistration-certificate --cold-verification-key-file "$k/cold.vkey" \
    --epoch "$epoch" --out-file "$k/retire.cert"
  cli_tx retire-pool2 1 --certificate-file "$k/retire.cert" --witness-override 2 -- "$k/cold.skey" ||
    die "retiring pool2: $CLI_RESULT"
  # The wallet removes the pool at the first block of epoch + 2.
  boundary=$((epoch + 2))
  while [ "$(current_epoch)" -lt $((boundary - 1)) ]; do sleep 2; done
  wait_s=$(seconds_to_epoch_end)
  [ "$wait_s" -le 8 ] || sleep $((wait_s - 8))
  log "pool2 retires in epoch $epoch; the wallet removes it from epoch $boundary"
  evict_pool_db
  t0=$(date +%s.%N)
  probe_loop gc "$POOL2_HEX" "$LOOP_MAX"
  result S-gc "DRep list latency while the wallet removes a pool retired on chain, database not cached" \
    "GET /v2/dreps every 2 s across the removal" "(measurement)" \
    "list $(sstats gc-list); lookups $(sstats gc-lookup), $(sstatuses gc-lookup); summary $(sstats gc-summary); $(hold_report gc "$POOL2_HEX" "$t0")" NOTE
}

T0=$(date +%s)
"$WALLET" version >"$OUT/wallet-version.txt"
log "backend $BACKEND: $(cat "$OUT/wallet-version.txt")"
setup
startup
idle
pools_warm
pools_cold
gc
echo $(($(date +%s) - T0)) >"$OUT/duration-seconds"
log "backend $BACKEND finished in $(cat "$OUT/duration-seconds") s"
