# shellcheck shell=bash
# Helpers shared by the steps in scenario.sh: wallet API recording, cardano-cli
# transactions, ledger and wallet views of a reward account, and the results
# table.
#
# Functions return their findings in global variables (R_*, CF_*,
# CLI_RESULT), read by the sourcing script.
# shellcheck disable=SC2034

MAGIC=42
PV_MAJOR=11
API="http://127.0.0.1:$WALLET_PORT"
# Below this reward balance the wallet leaves a requested self-withdrawal out
# of the transaction, so a request made with less does not test the
# withdrawal at all.
MIN_REWARD=1000000

now() { date -u +%Y-%m-%dT%H:%M:%SZ; }

log() {
  printf '%s %s\n' "$(date -u +%H:%M:%S)" "$*" | tee -a "$OUT/harness.log" >&2
}

die() {
  log "error: $*"
  exit 2
}

chain_tip() {
  "$CLI" conway query tip 2>/dev/null | jq -c '{epoch, slot}' 2>/dev/null || echo null
}

# Secrets never reach the recorded request files.
JQ_REDACT='def redact:
  if type == "object" then
    with_entries(
      if .key == "passphrase" or .key == "mnemonic_sentence"
         or (.key == "withdrawal" and (.value | type) == "array")
      then .value = "<redacted>" else .value |= redact end)
  elif type == "array" then map(redact)
  else . end;'

API_N=0

# api LABEL METHOD PATH [JSON-BODY]
#
# Sends one wallet API request. The request is written to
# api/NNN-LABEL.req.json, the response body exactly as received to
# api/NNN-LABEL.res.json and the HTTP status to api/NNN-LABEL.status, and one
# line is appended to api-calls.tsv. Sets R_STATUS, R_CODE, R_FILE, R_BASE and
# R_SECONDS.
api() {
  local label="$1" method="$2" path="$3" body="${4:-}"
  API_N=$((API_N + 1))
  R_BASE="$OUT/api/$(printf '%03d' "$API_N")-$label"
  R_FILE="$R_BASE.res.json"
  local tip
  tip=$(chain_tip)
  jq -n --arg t "$(now)" --arg m "$method" --arg p "$path" \
    --argjson tip "$tip" --argjson b "${body:-null}" \
    "$JQ_REDACT"' {time: $t, chain: $tip, method: $m, path: $p, body: ($b | redact)}' \
    >"$R_BASE.req.json"
  local args=(-s -o "$R_FILE" -w '%{http_code} %{time_total}' --max-time 600
    -X "$method" -H 'Accept: application/json')
  if [ -n "$body" ]; then
    args+=(-H 'Content-Type: application/json' --data-binary "$body")
  fi
  local out
  out=$(curl "${args[@]}" "$API$path") || true
  R_STATUS=${out%% *}
  R_SECONDS=${out#* }
  [ -n "$R_STATUS" ] || R_STATUS=000
  echo "$R_STATUS" >"$R_BASE.status"
  R_CODE=$(jq -r 'if type == "object" then (.code // "") else "" end' "$R_FILE" 2>/dev/null || true)
  printf '%03d\t%s\t%s\t%s %s\t%s\t%s\t%s\n' "$API_N" "$label" \
    "$(echo "$tip" | jq -r 'if . == null then "-" else "e\(.epoch) s\(.slot)" end')" \
    "$method" "$path" "$R_STATUS" "${R_CODE:-ok}" "$R_SECONDS" >>"$OUT/api-calls.tsv"
}

# api_octets LABEL PATH FILE: POST a binary body, as the hardware-wallet
# submission does.
api_octets() {
  local label="$1" path="$2" file="$3"
  API_N=$((API_N + 1))
  R_BASE="$OUT/api/$(printf '%03d' "$API_N")-$label"
  R_FILE="$R_BASE.res.json"
  jq -n --arg t "$(now)" --arg p "$path" --argjson tip "$(chain_tip)" \
    '{time: $t, chain: $tip, method: "POST", path: $p,
      body: "<signed transaction, application/octet-stream>"}' >"$R_BASE.req.json"
  local out
  out=$(curl -s -o "$R_FILE" -w '%{http_code} %{time_total}' --max-time 600 -X POST \
    -H 'Content-Type: application/octet-stream' -H 'Accept: application/json' \
    --data-binary @"$file" "$API$path") || true
  R_STATUS=${out%% *}
  R_SECONDS=${out#* }
  echo "$R_STATUS" >"$R_BASE.status"
  R_CODE=$(jq -r 'if type == "object" then (.code // "") else "" end' "$R_FILE" 2>/dev/null || true)
  printf '%03d\t%s\t-\tPOST %s\t%s\t%s\t%s\n' "$API_N" "$label" "$path" \
    "$R_STATUS" "${R_CODE:-ok}" "$R_SECONDS" >>"$OUT/api-calls.tsv"
}

# The ledger failures named in the last response's message.
ledger_failures() {
  jq -r '.message // ""' "${1:-$R_FILE}" 2>/dev/null |
    grep -oE 'Conway[A-Za-z]+' | sort -u | paste -sd, - || true
}

# One-line description of the last response.
observed() {
  local failures
  failures=$(ledger_failures)
  printf 'HTTP %s %s%s' "$R_STATUS" "${R_CODE:-ok}" "${failures:+ ($failures)}"
}

# fixture NAME: marks the last recorded exchange for export as a Jest fixture.
fixture() {
  printf '%s\t%s\n' "$1" "$R_BASE" >>"$OUT/fixtures.tsv"
}

save_var() {
  printf '%s=%q\n' "$1" "$2" >>"$STATE/scenario.env"
  printf -v "$1" '%s' "$2"
}

# Results ---------------------------------------------------------------------

# result ID SCENARIO REQUEST EXPECTATION OBSERVED VERDICT
#
# VERDICT is PASS (the expectation holds), FAIL (it does not), NOTE (a
# measurement with no expectation) or PRECONDITION (the scenario could not be
# set up, so the expectation was not tested).
result() {
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$1" "$2" "$3" "$4" "$5" "$6" >>"$OUT/results.tsv"
  log "$6 $1: $5"
}

# check ID SCENARIO REQUEST EXPECTATION OBSERVED CONDITION: PASS when the
# shell expression CONDITION holds. CONDITION is evaluated in the caller's
# scope, so it may name the caller's local variables.
check() {
  if eval "$6"; then
    result "$1" "$2" "$3" "$4" "$5" PASS
  else
    result "$1" "$2" "$3" "$4" "$5" FAIL
  fi
}

is_refused_before_submission() {
  [ "$R_STATUS" = 403 ] && [ "$R_CODE" = withdrawal_not_possible_without_vote ]
}

is_accepted() {
  case "$R_STATUS" in 200 | 201 | 202) return 0 ;; *) return 1 ;; esac
}

# require_no_vote ID WID: records ID as PRECONDITION and fails unless the
# ledger holds no vote delegation for the wallet's reward account.
require_no_vote() {
  local seen
  seen=$(ledger_vote "$2")
  [ "$seen" = none ] && return 0
  result "$1" "Reward account with no vote delegation" "cardano-cli query stake-address-info" \
    "none" "$seen" PRECONDITION
  return 1
}

# Wallet ----------------------------------------------------------------------

wallet_json() { curl -s --max-time 60 "$API/v2/wallets/$1"; }
reward_of() { wallet_json "$1" | jq -r '.balance.reward.quantity'; }
available_of() { wallet_json "$1" | jq -r '.balance.available.quantity'; }
stake_address_of() { curl -s "$API/v2/wallets/$1/stake-keys" | jq -r '.ours[0].key'; }
unused_address_of() { curl -s "$API/v2/wallets/$1/addresses?state=unused" | jq -r '.[0].id'; }
pending_of() {
  curl -s "$API/v2/wallets/$1/transactions" | jq -r '[.[] | select(.status == "pending") | .id] | join(" ")'
}

# availability_line WID LABEL: available, total and pending, for availability.txt.
availability_line() {
  local pending
  pending=$(pending_of "$1")
  wallet_json "$1" | jq -c --arg l "$2" --arg p "$pending" \
    '{after: $l, available: .balance.available.quantity,
      total: .balance.total.quantity, reward: .balance.reward.quantity,
      pending: ($p | split(" ") | map(select(. != "")))}' |
    tee -a "$OUT/availability.txt"
}

wait_ready() {
  local _
  for _ in $(seq 1 120); do
    [ "$(wallet_json "$1" | jq -r '.state.status')" = ready ] && return 0
    sleep 1
  done
  die "wallet $1 did not become ready"
}

# wait_tx WID TXID: until the wallet reports the transaction in_ledger.
wait_tx() {
  local status _
  for _ in $(seq 1 90); do
    status=$(curl -s "$API/v2/wallets/$1/transactions/$2" | jq -r '.status')
    [ "$status" = in_ledger ] && return 0
    sleep 2
  done
  log "transaction $2 of wallet $1 not in ledger (last status: $status)"
  return 1
}

# wait_last_tx WID: waits for the transaction in the last accepted response.
wait_last_tx() {
  local id
  id=$(jq -r '.id // empty' "$R_FILE")
  [ -n "$id" ] || return 1
  wait_tx "$1" "$id"
}

# wait_withdrawable WID: until the wallet's cached reward balance is above
# MIN_REWARD, equals the ledger's, and the epoch has at least 60 of its 150
# slots left. A withdrawal must state the exact balance, and one built from a
# balance that changes at the next epoch boundary is refused with
# ConwayIncompleteWithdrawals.
wait_withdrawable() {
  local id="$1" stake reward ledger slot _
  stake=$(stake_address_of "$id")
  for _ in $(seq 1 120); do
    reward=$(reward_of "$id")
    ledger=$("$CLI" conway query stake-address-info --address "$stake" | jq -r '.[0].rewardAccountBalance // 0')
    slot=$("$CLI" conway query tip | jq -r '.slotInEpoch')
    if [ "${reward:-0}" -gt "$MIN_REWARD" ] && [ "$reward" = "$ledger" ] &&
      [ "$slot" -ge 5 ] && [ "$slot" -le 90 ]; then
      return 0
    fi
    sleep 2
  done
  log "wallet $id: reward $reward, ledger $ledger, slot in epoch $slot; not withdrawable"
  return 1
}

# release_pending WID LABEL: removes every pending transaction the way
# Daedalus's pending-transaction cancel does.
release_pending() {
  local t
  for t in $(pending_of "$1"); do
    api "$2-forget" DELETE "/v2/wallets/$1/transactions/$t"
  done
}

# Ledger ----------------------------------------------------------------------

# Maps every encoding of the run's DReps to their names, so a vote reported by
# the ledger, the wallet history or the wallet's ledger field compares equal.
drep_ids_json() {
  jq -n --arg al "$DREP_A_LEGACY" --arg ac "$DREP_A_CIP129" --arg ah "$DREP_A_HEX" \
    --arg bl "$DREP_B_LEGACY" --arg bc "$DREP_B_CIP129" --arg bh "$DREP_B_HEX" \
    '{($al): "A", ($ac): "A", ($ah): "A", ($bl): "B", ($bc): "B", ($bh): "B"}'
}

# Every vote is read as none, abstain, no_confidence, drep:A or drep:B (the
# run's DReps), drep:key:<id> or drep:script:<id> (any other DRep), or
# unrecognised:<value>, which no expectation matches.
#
# drep: one DRep in the wallet API's encoding (ApiDRep in the wallet's
# swagger.yaml): abstain, no_confidence, or a bech32 identifier with prefix
# drep_vkh (key hash), drep_script (script hash) or drep. A drep identifier
# holds either a 28-byte key hash (CIP-105) or, as the wallet writes it, a
# CIP-129 credential whose header byte 0x22 (key hash) or 0x23 (script hash)
# fixes its first two data characters as y then one of g f 2 t, or y then one
# of v d w 0.
#
# vote: a vote delegation as cardano-cli stake-address-info reports it
# (null, alwaysAbstain, alwaysNoConfidence, or an object naming the
# credential), or as the wallet history reports it (an ApiDRep, or no field).
#
# ledger_field: the wallet's delegation.ledger_vote, which has exactly two
# shapes: {"status": "not_delegated"} and {"status": "delegated", "voting":
# <ApiDRep>}. The wallet omits the field while it has not read the value and
# before Conway; the caller reports that as absent.
#
# shellcheck disable=SC2016 # a jq program, not shell
JQ_VOTE='def drep($ids):
  ("[02-9ac-hj-np-z]") as $c
  | if type != "string" then "unrecognised:" + tojson
    elif . == "abstain" then "abstain"
    elif . == "no_confidence" then "no_confidence"
    elif $ids[.] then "drep:" + $ids[.]
    elif test("^drep_vkh1" + $c + "{51}$") then "drep:key:" + .
    elif test("^drep_script1" + $c + "{51}$") then "drep:script:" + .
    elif test("^drep1y[gf2t]" + $c + "{51}$") then "drep:key:" + .
    elif test("^drep1y[vdw0]" + $c + "{51}$") then "drep:script:" + .
    elif test("^drep1" + $c + "{51}$") then "drep:key:" + .
    else "unrecognised:" + . end;
def vote($ids):
  if . == null then "none"
  elif . == "alwaysAbstain" then "abstain"
  elif . == "alwaysNoConfidence" then "no_confidence"
  elif type == "string" then drep($ids)
  elif type == "object" and (.keyHash | type) == "string" then
    if $ids[.keyHash] then "drep:" + $ids[.keyHash] else "drep:key:" + .keyHash end
  elif type == "object" and (.scriptHash | type) == "string" then "drep:script:" + .scriptHash
  elif type == "object" and (.cip129Bech32 | type) == "string" then .cip129Bech32 | drep($ids)
  else "unrecognised:" + tojson end;
def ledger_field($ids):
  if type == "object" and (keys == ["status"]) and .status == "not_delegated" then "none"
  elif type == "object" and (keys == ["status", "voting"]) and .status == "delegated" then .voting | drep($ids)
  else "unrecognised:" + tojson end;'

# ledger_vote WID: the reward account's vote delegation in the ledger, as
# none, abstain, no_confidence, drep:<name> or unregistered.
ledger_vote() {
  "$CLI" conway query stake-address-info --address "$(stake_address_of "$1")" |
    jq -r --argjson ids "$(drep_ids_json)" "$JQ_VOTE"'
      if length == 0 then "unregistered" else .[0].voteDelegation | vote($ids) end'
}

# wallet_history_vote WID: the vote the wallet reports from its own history,
# the latest pending entry first, as Daedalus reads it.
wallet_history_vote() {
  wallet_json "$1" | jq -r --argjson ids "$(drep_ids_json)" "$JQ_VOTE"'
    .delegation | ((.next // []) | map(select(.voting != null)) | last | .voting)
      // .active.voting | vote($ids)'
}

# wallet_ledger_field WID: the ledger vote delegation as the wallet reports it
# in the field at LEDGER_VOTE_FIELD, read by ledger_field; absent when the
# wallet has not read the value, the era is before Conway or the wallet does
# not report the field (the response has no such field); no wallet when the
# response is not a wallet.
wallet_ledger_field() {
  wallet_json "$1" | jq -rn --argjson ids "$(drep_ids_json)" \
    --arg p "$LEDGER_VOTE_FIELD" "$JQ_VOTE"'
    ($p | split(".")) as $path
    | (try input catch null) as $w
    | if ($w | type) != "object" or ($w | has("id") | not) then "no wallet"
      elif $w | [paths] | any(. == $path) then $w | getpath($path) | ledger_field($ids)
      else "absent" end'
}

# wait_ledger_field WID VALUE: until the wallet's ledger field reads VALUE, for
# up to 10 s. The field is the wallet's last completed read, taken when the
# node tip changes, so it can trail a transaction the wallet already reports
# in the ledger by a block. Returns at once while the field is absent.
wait_ledger_field() {
  local seen _
  for _ in 1 2 3 4 5; do
    seen=$(wallet_ledger_field "$1")
    { [ "$seen" != absent ] && [ "$seen" != "$2" ]; } || return 0
    sleep 2
  done
  return 1
}

snapshot() {
  local label="$1" id
  shift
  {
    echo "## $label $(now) $(chain_tip)"
    for id in "$@"; do
      echo "# wallet $id: ledger (cardano-cli query stake-address-info)"
      "$CLI" conway query stake-address-info --address "$(stake_address_of "$id")" |
        jq -c '.[0] // {} | {rewardAccountBalance, stakeDelegation: .stakeDelegation.stakePoolBech32, voteDelegation}'
      echo "# wallet $id: wallet (GET /v2/wallets/{id})"
      wallet_json "$id" | jq -c '{reward: .balance.reward.quantity, available: .balance.available.quantity, delegation}'
    done
    echo "# ledger DRep registry (cardano-cli query drep-state)"
    "$CLI" conway query drep-state --drep-key-hash "$DREP_A_HEX" | jq -c '{A: .}'
    "$CLI" conway query drep-state --drep-key-hash "$DREP_B_HEX" | jq -c '{B: .}'
  } >>"$OUT/snapshots.txt"
}

utxo_address() {
  "$CLI" conway address build --payment-verification-key-file \
    "$STATE/data/utxo-keys/utxo$1/utxo.vkey"
}

# cli_tx NAME UTXO-KEY [BUILD-ARGS...] -- [SIGNING-KEYS...]
#
# Builds, signs and submits a transaction with cardano-cli, paying from the
# genesis UTxO key, and waits for it on chain. Output is in cli-NAME.*.txt.
# Sets CLI_RESULT to accepted, build-rejected, submit-rejected or not-on-chain
# and returns non-zero unless accepted.
cli_tx() {
  local name="$1" key="$2"
  shift 2
  local build=() skeys=() seen=0 a
  for a in "$@"; do
    if [ "$a" = -- ]; then
      seen=1
    elif [ "$seen" = 0 ]; then
      build+=("$a")
    else
      skeys+=("$a")
    fi
  done
  local from txin
  from=$(utxo_address "$key")
  txin=$("$CLI" conway query utxo --address "$from" --output-json |
    jq -r 'to_entries | max_by(.value.value.lovelace) | .key')
  mkdir -p "$STATE/tx"
  if ! "$CLI" conway transaction build --tx-in "$txin" --change-address "$from" \
    "${build[@]}" --out-file "$STATE/tx/$name.body" >"$OUT/cli-$name.build.txt" 2>&1; then
    CLI_RESULT='build-rejected'
    return 1
  fi
  local sargs=(--signing-key-file "$STATE/data/utxo-keys/utxo$key/utxo.skey") s
  for s in "${skeys[@]}"; do sargs+=(--signing-key-file "$s"); done
  "$CLI" conway transaction sign --tx-body-file "$STATE/tx/$name.body" "${sargs[@]}" \
    --out-file "$STATE/tx/$name.signed"
  if ! "$CLI" conway transaction submit --tx-file "$STATE/tx/$name.signed" \
    >"$OUT/cli-$name.submit.txt" 2>&1; then
    CLI_RESULT='submit-rejected'
    return 1
  fi
  local txid _
  txid=$("$CLI" conway transaction txid --tx-file "$STATE/tx/$name.signed" | jq -r '.txhash // .')
  for _ in $(seq 1 60); do
    if "$CLI" conway query utxo --tx-in "$txid#0" --output-json | jq -e 'length > 0' >/dev/null; then
      CLI_RESULT=accepted
      return 0
    fi
    sleep 2
  done
  CLI_RESULT='not-on-chain'
  return 1
}

# construct_flow LABEL WID BODY
#
# The cip30 builds' send path: transactions-construct, transactions-sign,
# transactions-submit. When the submission is refused, the same signed CBOR is
# also posted to /v2/proxy/transactions (the hardware-wallet submission) and
# submitted with cardano-cli for the ledger's own rendering. Sets
# CF_CONSTRUCT_STATUS, CF_CONSTRUCT_CODE, CF_CONSTRUCT_FILE, CF_SUBMIT_STATUS,
# CF_SUBMIT_CODE, CF_SUBMIT_FILE, CF_PROXY_STATUS, CF_PROXY_BASE, CF_CERTS (the
# certificates in the signed CBOR, as JSON), CF_WITHDRAWAL (lovelace in the
# selection) and CF_TXID.
construct_flow() {
  local label="$1" id="$2" body="$3"
  CF_SUBMIT_STATUS=- CF_SUBMIT_CODE='' CF_SUBMIT_FILE='' CF_PROXY_STATUS=- CF_PROXY_BASE=''
  CF_CERTS=null CF_WITHDRAWAL=0 CF_TXID=''
  api "$label-construct" POST "/v2/wallets/$id/transactions-construct" "$body"
  CF_CONSTRUCT_STATUS=$R_STATUS CF_CONSTRUCT_CODE=$R_CODE CF_CONSTRUCT_FILE=$R_FILE
  is_accepted || return 0
  CF_WITHDRAWAL=$(jq -r '[.coin_selection.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")
  local unsigned signed
  unsigned=$(jq -r .transaction "$R_FILE")
  api "$label-sign" POST "/v2/wallets/$id/transactions-sign" \
    "$(jq -n --arg t "$unsigned" --arg p "$PASS" '{transaction: $t, passphrase: $p, encoding: "base16"}')"
  signed=$(jq -r .transaction "$R_FILE")
  echo "$signed" >"$OUT/$label.signed.cbor.hex"
  jq -n --arg h "$signed" '{type: "Tx ConwayEra", description: "", cborHex: $h}' >"$STATE/$label.tx.json"
  "$CLI" debug transaction view --tx-file "$STATE/$label.tx.json" >"$OUT/$label.tx-view.json" 2>/dev/null || true
  CF_CERTS=$(jq -c '.certificates // []' "$OUT/$label.tx-view.json" 2>/dev/null || echo null)
  api "$label-submit" POST "/v2/wallets/$id/transactions-submit" \
    "$(jq -n --arg t "$signed" '{transaction: $t}')"
  CF_SUBMIT_STATUS=$R_STATUS CF_SUBMIT_CODE=$R_CODE CF_SUBMIT_FILE=$R_FILE
  if is_accepted; then
    CF_TXID=$(jq -r '.id' "$R_FILE")
    wait_tx "$id" "$CF_TXID" || true
    return 0
  fi
  xxd -r -p "$OUT/$label.signed.cbor.hex" >"$STATE/$label.signed.cbor"
  api_octets "$label-proxy" /v2/proxy/transactions "$STATE/$label.signed.cbor"
  CF_PROXY_STATUS=$R_STATUS CF_PROXY_BASE=$R_BASE
  "$CLI" conway transaction submit --tx-file "$STATE/$label.tx.json" \
    >"$OUT/$label.cli-submit.txt" 2>&1 || true
  # Leave R_* describing the wallet's own submission.
  R_STATUS=$CF_SUBMIT_STATUS R_CODE=$CF_SUBMIT_CODE R_FILE=$CF_SUBMIT_FILE
}

# dapp_submit LABEL WID FILE: posts the signed transaction in FILE (base16) to
# the fork's POST /v2/wallets/{id}/transaction-submission, as the cip30
# hardware-wallet path does. The network object is the one the wallet compares
# against its own: network id 0 (a test network), the run's protocol magic and
# the genesis block hash its network parameters report. Returns 1 and sets
# DAPP_SUBMIT_WHY when the hash cannot be read.
dapp_submit() {
  local label="$1" id="$2" file="$3" hash
  DAPP_SUBMIT_WHY=''
  hash=$(curl -s --max-time 60 "$API/v2/network/parameters" | jq -r '.genesis_block_hash // empty')
  if ! [[ "$hash" =~ ^[0-9a-f]{64}$ ]]; then
    DAPP_SUBMIT_WHY="genesis block hash unavailable ('$hash')"
    return 1
  fi
  api "$label" POST "/v2/wallets/$id/transaction-submission" \
    "$(jq -n --arg h "$hash" --arg t "$(cat "$file")" --argjson m "$MAGIC" \
      '{revision: 1, network: {network_id: 0, network_magic: $m, genesis_hash: $h}, transaction: $t}')"
}

# Describes a construct_flow outcome on one line.
construct_observed() {
  if [ "$CF_CONSTRUCT_STATUS" != 202 ]; then
    printf 'construct HTTP %s %s' "$CF_CONSTRUCT_STATUS" "${CF_CONSTRUCT_CODE:-ok}"
    return
  fi
  local failures
  failures=$(ledger_failures "$CF_SUBMIT_FILE")
  printf 'construct 202, submit HTTP %s %s%s' "$CF_SUBMIT_STATUS" "${CF_SUBMIT_CODE:-ok}" \
    "${failures:+ ($failures)}"
  [ "$CF_PROXY_STATUS" = - ] || printf ', proxy HTTP %s' "$CF_PROXY_STATUS"
}

construct_refused_before_submission() {
  [ "$CF_CONSTRUCT_STATUS" = 403 ] &&
    [ "$CF_CONSTRUCT_CODE" = withdrawal_not_possible_without_vote ]
}

# True when the certificates in the last constructed CBOR include a vote
# delegation.
cbor_has_vote_certificate() {
  echo "$CF_CERTS" | jq -e '
    any(.. | objects; (."delegatee type"? // "") | test("vote"))' >/dev/null
}

# selection_fee FILE: the fee a coin selection leaves, in lovelace: what its
# inputs, withdrawals and returned deposits supply, less its outputs, change
# and deposits taken. The selection response carries no fee field.
selection_fee() {
  jq -r 'def sum(f): [f] | add // 0;
    sum(.inputs[].amount.quantity) + sum(.withdrawals[]?.amount.quantity)
    + sum(.deposits_returned[]?.quantity)
    - sum(.outputs[].amount.quantity) - sum(.change[].amount.quantity)
    - sum(.deposits_taken[]?.quantity)' "$1"
}

# selection_body SELECTION-FILE OUT-FILE: builds the coin selection into an
# unsigned transaction body with cardano-cli, as a client that constructs the
# transaction itself does for a hardware wallet, and writes its CBOR (base16)
# to OUT-FILE. Returns non-zero when the selection carries what build-raw is
# not given here (assets, withdrawals, certificates) or cardano-cli refuses.
selection_body() {
  local sel="$1" out="$2" args=() line
  if jq -e '[.inputs[], .outputs[], .change[]] | any(.assets | length > 0)
      or ((.withdrawals // []) | length > 0) or ((.certificates // []) | length > 0)'     "$sel" >/dev/null; then
    return 1
  fi
  while IFS= read -r line; do args+=(--tx-in "$line"); done \
    < <(jq -r '.inputs[] | "\(.id)#\(.index)"' "$sel")
  while IFS= read -r line; do args+=(--tx-out "$line"); done \
    < <(jq -r '(.outputs[], .change[]) | "\(.address)+\(.amount.quantity)"' "$sel")
  "$CLI" conway transaction build-raw "${args[@]}" --fee "$(selection_fee "$sel")" \
    --out-file "$out.json" >"$out.build.txt" 2>&1 || return 1
  jq -r .cborHex "$out.json" >"$out"
}

# vote_selection_body SELECTION-FILE CERT-FILE OUT-FILE: as selection_body for
# the coin selection of a vote-only transaction. The selection must carry
# exactly one certificate, a cast_vote, which CERT-FILE (a vote delegation
# certificate for the wallet's stake key) stands for, and no withdrawal or
# asset. The body expires 7,200 slots after the tip, as Daedalus's TTL does.
vote_selection_body() {
  local sel="$1" cert="$2" out="$3" args=() line tip
  if jq -e '[.inputs[], .outputs[], .change[]] | any(.assets | length > 0)
      or ((.withdrawals // []) | length > 0)
      or ((.certificates // []) | map(.certificate_type) != ["cast_vote"])' "$sel" >/dev/null; then
    return 1
  fi
  while IFS= read -r line; do args+=(--tx-in "$line"); done \
    < <(jq -r '.inputs[] | "\(.id)#\(.index)"' "$sel")
  while IFS= read -r line; do args+=(--tx-out "$line"); done \
    < <(jq -r '(.outputs[], .change[]) | "\(.address)+\(.amount.quantity)"' "$sel")
  tip=$("$CLI" conway query tip | jq -r .slot)
  "$CLI" conway transaction build-raw "${args[@]}" --certificate-file "$cert" \
    --fee "$(selection_fee "$sel")" --invalid-hereafter $((tip + 7200)) \
    --out-file "$out.json" >"$out.build.txt" 2>&1 || return 1
  jq -r .cborHex "$out.json" >"$out"
}

# withdrawal_selection_body SELECTION-FILE OUT-FILE: as selection_body for the
# coin selection of a withdrawal-only transaction. The selection must carry
# exactly one withdrawal and no certificate or asset; the body states the
# withdrawal and expires 7,200 slots after the tip, as Daedalus's TTL does.
withdrawal_selection_body() {
  local sel="$1" out="$2" args=() line tip
  if jq -e '[.inputs[], .outputs[], .change[]] | any(.assets | length > 0)
      or ((.withdrawals // []) | length != 1)
      or ((.certificates // []) | length > 0)' "$sel" >/dev/null; then
    return 1
  fi
  while IFS= read -r line; do args+=(--tx-in "$line"); done \
    < <(jq -r '.inputs[] | "\(.id)#\(.index)"' "$sel")
  while IFS= read -r line; do args+=(--tx-out "$line"); done \
    < <(jq -r '(.outputs[], .change[]) | "\(.address)+\(.amount.quantity)"' "$sel")
  tip=$("$CLI" conway query tip | jq -r .slot)
  "$CLI" conway transaction build-raw "${args[@]}" \
    --withdrawal "$(jq -r '.withdrawals[0] | "\(.stake_address)+\(.amount.quantity)"' "$sel")" \
    --fee "$(selection_fee "$sel")" --invalid-hereafter $((tip + 7200)) \
    --out-file "$out.json" >"$out.build.txt" 2>&1 || return 1
  jq -r .cborHex "$out.json" >"$out"
}

# derive_stake_keys MNEMONIC-FILE DIR WID: the wallet's stake key (DIR/stake.skey
# and stake.vkey), for a transaction built by cardano-cli alone. Fails the run
# when the key is not the wallet's.
derive_stake_keys() {
  local words="$1" keys="$2" id="$3"
  mkdir -p "$keys"
  "$ADDRESS" key from-recovery-phrase Shelley <"$words" >"$keys/root.xsk"
  "$ADDRESS" key child 1852H/1815H/0H/2/0 <"$keys/root.xsk" >"$keys/stake.xsk"
  "$CLI" conway key convert-cardano-address-key --shelley-stake-key \
    --signing-key-file "$keys/stake.xsk" --out-file "$keys/stake.skey"
  "$CLI" conway key verification-key --signing-key-file "$keys/stake.skey" \
    --verification-key-file "$keys/stake.evkey"
  "$CLI" conway key non-extended-key --extended-verification-key-file "$keys/stake.evkey" \
    --verification-key-file "$keys/stake.vkey"
  [ "$("$CLI" conway stake-address build --stake-verification-key-file "$keys/stake.vkey")" = "$(stake_address_of "$id")" ] ||
    die "the stake key derived from the recovery phrase in $words is not wallet $id's"
}

# wait_ledger_vote WID VALUE: until the ledger reads VALUE for the wallet's
# reward account, for up to 90 s.
wait_ledger_vote() {
  local _
  for _ in $(seq 1 45); do
    [ "$(ledger_vote "$1")" != "$2" ] || return 0
    sleep 2
  done
  return 1
}

# fee_within_estimate ESTIMATE-FILE TRANSACTION-FILE: the transaction's fee lies
# within the payment-fees response's estimated_min to estimated_max.
fee_within_estimate() {
  jq -e --slurpfile t "$2" '.estimated_min.quantity <= $t[0].fee.quantity
    and $t[0].fee.quantity <= .estimated_max.quantity' "$1" >/dev/null
}

# register_extra_dreps N: registers N DReps with cardano-cli, 80 to a
# transaction, paying from genesis UTxO key 2.
register_extra_dreps() {
  local count="$1" batch=80 i first last
  [ "$count" -gt 0 ] || return 0
  mkdir -p "$STATE/extra-dreps"
  for i in $(seq 1 "$count"); do
    "$CLI" conway governance drep key-gen --verification-key-file "$STATE/extra-dreps/$i.vkey" \
      --signing-key-file "$STATE/extra-dreps/$i.skey"
    "$CLI" conway governance drep registration-certificate \
      --drep-verification-key-file "$STATE/extra-dreps/$i.vkey" \
      --key-reg-deposit-amt 500000000 --out-file "$STATE/extra-dreps/$i.cert"
  done
  for first in $(seq 1 "$batch" "$count"); do
    local args=() skeys=()
    last=$((first + batch - 1))
    [ "$last" -le "$count" ] || last=$count
    for i in $(seq "$first" "$last"); do
      args+=(--certificate-file "$STATE/extra-dreps/$i.cert")
      skeys+=("$STATE/extra-dreps/$i.skey")
    done
    cli_tx "extra-dreps-$first" 2 "${args[@]}" --witness-override $((last - first + 2)) -- "${skeys[@]}" ||
      die "registering DReps $first to $last: $CLI_RESULT"
  done
  log "registered $count further DReps"
}
