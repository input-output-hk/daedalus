#!/usr/bin/env bash
# Retired-DRep reward withdrawal: wallet API regression harness.
#
# Runs the scenarios in scenario.sh on a fresh local Conway testnet, once per
# wallet backend Daedalus ships, and writes every request and response, the
# ledger and wallet views at each step, and a results table.
#
# Run it from the Daedalus repository as a flake app, which takes
# cardano-node, cardano-cli, cardano-address and both cardano-wallet builds
# from the flake inputs:
#
#   nix run .#drep-withdrawal-harness -- [options]
#
# To test another cardano-wallet as the mainnet backend, override the input:
#
#   nix run .#drep-withdrawal-harness \
#     --override-input cardano-wallet github:<owner>/cardano-wallet/<ref> -- --backend upstream
set -euo pipefail
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

usage() {
  cat <<'USAGE'
Usage: drep-withdrawal-harness [options]

  --backend B             upstream (the mainnet build's wallet), cip30 (the
                          mainnet-flight, preprod and preview builds' wallet)
                          or all (default)
  --scenario S            withdrawal (default): the retired-DRep withdrawal
                          scenarios in scenario.sh; stall: DRep list latency
                          while the stake-pool database is busy (stall.sh)
  --pool-production-rows N
                          stall: synthetic pool_production rows added before
                          timing (default: 8050000, a synced mainnet
                          table's size)
  --out DIR               where results go (default: a new timestamped directory
                          under ${XDG_CACHE_HOME:-~/.cache}/daedalus-drep-withdrawal-harness)
  --wallet PATH           run this cardano-wallet binary instead of the flake
                          input's (requires a single --backend)
  --ledger-vote-field P   dot path, in GET /v2/wallets/{id}, of the wallet's
                          report of the ledger vote delegation
                          (default: delegation.ledger_vote)
  --extra-dreps N         further DReps registered before the DRep list is
                          timed (default: 1000)
  --lookup-burst N        lookups of the retired DRep sent at once (default: 50)
  --sustain-seconds N     length of the one-lookup-every-5-seconds probe
                          (default: 60)
  --fixtures DIR          also write the recorded exchanges the Jest specs
                          replay to DIR/<set>/<name>.json
  --fixtures-set NAME     the directory under --fixtures to write to
                          (default: the backend's name)
  --jest DIR              also run the specs that replay those exchanges
                          through Daedalus's own code, from the Daedalus
                          checkout DIR (needs its node_modules)
  --keep-state            keep the testnet, wallet databases and keys
  --node-port N           (default: 13101)
  --wallet-port N         (default: 18190)

Exit status: 0 when every expectation holds, 1 when any does not or could
not be tested, 2 when the run itself failed.
USAGE
}

BACKENDS=(upstream cip30)
SCENARIO=withdrawal
POOL_PRODUCTION_ROWS=8050000
OUT_ROOT=''
WALLET_OVERRIDE=''
LEDGER_VOTE_FIELD=delegation.ledger_vote
EXTRA_DREPS=1000
LOOKUP_BURST=50
SUSTAIN_SECONDS=60
FIXTURES=''
FIXTURES_SET=''
JEST_DIR=''
KEEP_STATE=''
NODE_PORT=13101
WALLET_PORT=18190

while [ $# -gt 0 ]; do
  case "$1" in
  --backend)
    case "$2" in
    upstream | cip30) BACKENDS=("$2") ;;
    all) BACKENDS=(upstream cip30) ;;
    *)
      echo "unknown backend: $2" >&2
      exit 2
      ;;
    esac
    shift 2
    ;;
  --scenario)
    case "$2" in
    withdrawal | stall) SCENARIO=$2 ;;
    *)
      echo "unknown scenario: $2" >&2
      exit 2
      ;;
    esac
    shift 2
    ;;
  --pool-production-rows)
    POOL_PRODUCTION_ROWS=$2
    shift 2
    ;;
  --out)
    OUT_ROOT=$2
    shift 2
    ;;
  --wallet)
    WALLET_OVERRIDE=$(realpath "$2")
    shift 2
    ;;
  --ledger-vote-field)
    LEDGER_VOTE_FIELD=$2
    shift 2
    ;;
  --extra-dreps)
    EXTRA_DREPS=$2
    shift 2
    ;;
  --lookup-burst)
    LOOKUP_BURST=$2
    shift 2
    ;;
  --sustain-seconds)
    SUSTAIN_SECONDS=$2
    shift 2
    ;;
  --fixtures)
    FIXTURES=$(realpath -m "$2")
    shift 2
    ;;
  --fixtures-set)
    FIXTURES_SET=$2
    shift 2
    ;;
  --jest)
    JEST_DIR=$(realpath "$2")
    shift 2
    ;;
  --keep-state)
    KEEP_STATE=1
    shift
    ;;
  --node-port)
    NODE_PORT=$2
    shift 2
    ;;
  --wallet-port)
    WALLET_PORT=$2
    shift 2
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  *)
    echo "unknown option: $1" >&2
    usage >&2
    exit 2
    ;;
  esac
done

if [ -n "$FIXTURES_SET" ] && [ "${#BACKENDS[@]}" != 1 ]; then
  echo "--fixtures-set needs a single --backend" >&2
  exit 2
fi

if [ -n "$WALLET_OVERRIDE" ] && [ "${#BACKENDS[@]}" != 1 ]; then
  echo "--wallet needs a single --backend" >&2
  exit 2
fi

binary() {
  local value="$1" name="$2"
  [ -n "$value" ] || value=$(command -v "$name" || true)
  [ -x "$value" ] || {
    echo "$name not found; run this through the flake app" >&2
    exit 2
  }
  echo "$value"
}

NODE=$(binary "${HARNESS_NODE:-}" cardano-node)
CLI=$(binary "${HARNESS_CLI:-}" cardano-cli)
ADDRESS=$(binary "${HARNESS_ADDRESS:-}" cardano-address)
TOOLS=(curl jq xxd awk ss)
[ "$SCENARIO" != stall ] || TOOLS+=(sqlite3 dd)
for tool in "${TOOLS[@]}"; do
  command -v "$tool" >/dev/null || {
    echo "$tool not found; run this through the flake app" >&2
    exit 2
  }
done

[ -n "$OUT_ROOT" ] ||
  OUT_ROOT="${XDG_CACHE_HOME:-$HOME/.cache}/daedalus-drep-withdrawal-harness/$(date -u +%Y%m%dT%H%M%SZ)"
mkdir -p "$OUT_ROOT"
OUT_ROOT=$(realpath "$OUT_ROOT")

# Every recorded exchange the specs replay, as JSON: the wallet that answered,
# the request (secrets redacted) and the response body exactly as received.
write_fixtures() {
  local backend="$1" dir="$2" set="${FIXTURES_SET:-$1}" name base
  mkdir -p "$FIXTURES/$set"
  while IFS=$'\t' read -r name base; do
    [ -n "$name" ] || continue
    jq -n --arg set "$set" --rawfile wallet "$dir/wallet-version.txt" \
      --slurpfile req "$base.req.json" --rawfile body "$base.res.json" \
      --arg status "$(cat "$base.status")" \
      '{backend: $set, wallet: ($wallet | rtrimstr("\n")),
        recorded: $req[0].time, chain: $req[0].chain,
        request: ($req[0] | {method, path, body}),
        response: {status: ($status | tonumber), body: $body}}' \
      >"$FIXTURES/$set/$name.json"
  done <"$dir/fixtures.tsv"
}

md_row() {
  local field out='|'
  for field in "$@"; do
    out="$out ${field//|/\\|} |"
  done
  echo "$out"
}

write_report() {
  local report="$OUT_ROOT/results.md" backend dir
  {
    echo "# Retired-DRep withdrawal harness"
    echo
    echo "Run $(basename "$OUT_ROOT"). Rows state what the wallet API must do for the request Daedalus sends."
    echo "PASS: the expectation holds. FAIL: it does not. NOTE: a measurement. PRECONDITION: the scenario could not be set up."
    for backend in "${BACKENDS[@]}"; do
      dir="$OUT_ROOT/$backend"
      [ -f "$dir/results.tsv" ] || continue
      echo
      echo "## $backend: $(cat "$dir/wallet-version.txt" 2>/dev/null || echo unknown)"
      echo
      echo "Duration $(cat "$dir/duration-seconds" 2>/dev/null || echo '?') s." \
        "$(cut -f6 "$dir/results.tsv" | sort | uniq -c | awk '{ printf "%s%s %s", (NR > 1 ? ", " : ""), $1, $2 }')."
      echo
      md_row ID Scenario Request "Expectation" Observed Result
      md_row --- --- --- --- --- ---
      while IFS=$'\t' read -r id scenario request expectation seen verdict; do
        md_row "$id" "$scenario" "$request" "$expectation" "$seen" "$verdict"
      done <"$dir/results.tsv"
    done
    if [ -f "$OUT_ROOT/jest-results.tsv" ]; then
      echo
      echo "## Daedalus, replaying the recorded responses (Jest)"
      echo
      md_row Spec Test Result
      md_row --- --- ---
      while IFS=$'\t' read -r spec name verdict; do
        md_row "$spec" "$name" "$verdict"
      done <"$OUT_ROOT/jest-results.tsv"
    fi
  } >"$report"
  echo "$report"
}

JEST_SPECS=(
  tests/jest/api/rewardWithdrawalRefusal.spec.ts
  tests/jest/api/rewardWithdrawalDecision.spec.ts
  tests/jest/api/paymentWithdrawalBody.spec.ts
  tests/jest/api/withdrawalRefusalMessages.spec.ts
  tests/jest/governance/retiredDRep.spec.ts
  source/renderer/app/domains/ApiError.spec.ts
)

run_jest() {
  local status=0
  (
    cd "$JEST_DIR"
    DAEDALUS_KNOWN_DEFECTS=assert node_modules/.bin/jest --coverage=false --json \
      --outputFile "$OUT_ROOT/jest.json" "${JEST_SPECS[@]}" >"$OUT_ROOT/jest.log" 2>&1
  ) || status=$?
  [ -f "$OUT_ROOT/jest.json" ] || {
    echo "jest produced no results (see $OUT_ROOT/jest.log)" >&2
    return 2
  }
  jq -r --arg root "$JEST_DIR/" '.testResults[] | (.name | ltrimstr($root)) as $spec
    | .assertionResults[] | [$spec, .fullName, (if .status == "passed" then "PASS" else "FAIL" end)]
    | @tsv' "$OUT_ROOT/jest.json" >"$OUT_ROOT/jest-results.tsv"
  return "$status"
}

STATUS=0
for backend in "${BACKENDS[@]}"; do
  dir="$OUT_ROOT/$backend"
  mkdir -p "$dir"
  if [ -n "$WALLET_OVERRIDE" ]; then
    wallet=$WALLET_OVERRIDE
  elif [ "$backend" = upstream ]; then
    wallet=$(binary "${HARNESS_WALLET_UPSTREAM:-}" cardano-wallet)
  else
    wallet=$(binary "${HARNESS_WALLET_CIP30:-}" cardano-wallet)
  fi
  echo "== $backend: $wallet" >&2
  run=0
  BACKEND=$backend OUT=$dir STATE="$dir/state" WALLET=$wallet NODE=$NODE CLI=$CLI \
    ADDRESS=$ADDRESS NODE_PORT=$NODE_PORT WALLET_PORT=$WALLET_PORT \
    LEDGER_VOTE_FIELD=$LEDGER_VOTE_FIELD EXTRA_DREPS=$EXTRA_DREPS \
    LOOKUP_BURST=$LOOKUP_BURST SUSTAIN_SECONDS=$SUSTAIN_SECONDS \
    POOL_PRODUCTION_ROWS=$POOL_PRODUCTION_ROWS \
    bash "$HERE/$([ "$SCENARIO" = stall ] && echo stall.sh || echo scenario.sh)" || run=$?
  [ -n "$KEEP_STATE" ] || rm -rf "$dir/state"
  if [ "$run" != 0 ]; then
    echo "$backend: the run failed (exit $run); see $dir/harness.log" >&2
    STATUS=2
  elif cut -f6 "$dir/results.tsv" | grep -qE '^(FAIL|PRECONDITION)$'; then
    [ "$STATUS" = 2 ] || STATUS=1
  fi
  [ -z "$FIXTURES" ] || [ ! -s "$dir/fixtures.tsv" ] || write_fixtures "$backend" "$dir"
done

if [ -n "$JEST_DIR" ]; then
  jest_status=0
  run_jest || jest_status=$?
  if [ "$jest_status" = 2 ]; then
    STATUS=2
  elif [ "$jest_status" != 0 ] && [ "$STATUS" = 0 ]; then
    STATUS=1
  fi
fi

echo "results: $(write_report)" >&2
exit "$STATUS"
