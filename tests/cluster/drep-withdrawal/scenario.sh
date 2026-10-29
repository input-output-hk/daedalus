#!/usr/bin/env bash
# One run of the retired-DRep withdrawal scenarios against one wallet backend.
# Started by harness.sh, which sets the binaries, ports, BACKEND, OUT and
# STATE.
#
# A Conway ledger (protocol version 10 and later) refuses any withdrawal from
# a key-hash reward account that has no vote delegation
# (ConwayWdrlNotDelegatedToDRep), and checks that before the transaction's own
# certificates apply. A DRep's retirement clears the vote delegation of every
# account delegated to it. Each row in results.tsv states what the wallet API
# must do for a request Daedalus sends, and whether the backend under test
# does it.
#
# Requests use the bodies Daedalus sends:
#   mainnet build (BACKEND=upstream)
#     payment fee     POST /v2/wallets/{id}/payment-fees {payments, withdrawal: "self"}
#     payment         POST /v2/wallets/{id}/transactions {payments, passphrase, withdrawal: "self"}
#     hardware wallet POST /v2/wallets/{id}/coin-selections/random {payments, withdrawal: "self", metadata: null},
#                     then POST /v2/proxy/transactions with the signed CBOR
#     hardware wallet POST /v2/wallets/{id}/coin-selections/random {payments, metadata: null} (no withdrawal)
#     undelegation    POST /v2/wallets/{id}/coin-selections/random {delegation_action: {action: "quit", pool}}
#     vote fee        POST /v2/wallets/{id}/transactions-construct {encoding: "base16", vote}
#     vote            PUT /v2/dreps/{drep}/wallets/{id} {passphrase}
#     hardware wallet POST /v2/wallets/{id}/transactions-construct {encoding: "base16", vote}, then
#                     POST /v2/proxy/transactions with the signed CBOR of a body built from its coin selection
#     pool            PUT /v2/stake-pools/{pool}/wallets/{id} {passphrase}
#     undelegation    DELETE /v2/stake-pools/*/wallets/{id} {passphrase}
#     payment, rewards left in place   payment-fees and transactions without the withdrawal key
#     withdraw rewards  POST /v2/wallets/{id}/transactions-construct {encoding: "base16", withdrawal: "self"}, then
#                       transactions-sign and transactions-submit
#     hardware wallet   the same construct, then POST /v2/proxy/transactions with the signed CBOR of a body built
#                       from its coin selection
#   mainnet-flight, preprod and preview builds (BACKEND=cip30)
#     transactions-construct, then transactions-sign, then transactions-submit
#     payment         {encoding, payments, withdrawal: "self"}
#     vote            {encoding, vote}
#     undelegation    {encoding, delegations: [{quit}], withdrawal: "self"}
#     withdraw rewards  {encoding, withdrawal: "self"}
#     hardware wallet   the same construct, signed, then POST /v2/wallets/{id}/transaction-submission
#   not sent by Daedalus: recovery-phrase withdrawal  payment-fees, then transactions, with withdrawal: <recovery phrase>
# Withdrawal-carrying requests are sent in both shapes on both backends, since
# the wallet API serves both to any client.
#
# Conditions passed to check() are single-quoted shell expressions, evaluated
# after the step in the step's own scope; flows return their outcome in
# globals (PAY_*, VOTE_*, QUIT_*) that those conditions read.
# shellcheck disable=SC2016,SC2034
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

# Set by setup through save_var.
PASS='' W1='' W2='' W3='' W4='' W1_ADDR='' W2_ADDR='' W3_ADDR='' W4_ADDR='' POOL1='' POOL2=''
DREP_A_LEGACY='' DREP_A_CIP129='' DREP_A_HEX='' DREP_B_LEGACY='' DREP_B_CIP129='' DREP_B_HEX=''
DEST=''
HELD_SUBMIT_OBSERVED=''
PROBE_T0=0
PROBES=()

GATE='refused before submission: HTTP 403 withdrawal_not_possible_without_vote'

# build_of SHAPE: the Daedalus builds that send requests in that shape.
build_of() {
  if [ "$1" = upstream ]; then echo "mainnet build"; else echo "cip30 builds"; fi
}
PAY_LOVELACE=5000000

payments_to() {
  jq -n --arg a "$1" --argjson q "$PAY_LOVELACE" \
    '[{address: $a, amount: {quantity: $q, unit: "lovelace"}}]'
}

# Request flows ---------------------------------------------------------------------

# pay LABEL WID [SHAPE [FEE-FIXTURE [nowdrl]]]: a payment that requests the
# reward withdrawal, in the shape the Daedalus build for SHAPE (default: this
# backend) sends; FEE-FIXTURE names the fee estimate's recorded exchange. With
# nowdrl the requests leave the withdrawal key out, as a payment whose control
# is off does. Sets
# PAY_ACCEPTED (yes or no), PAY_WITHDRAWAL (lovelace withdrawn) and
# PAY_OBSERVED.
pay() {
  local label="$1" id="$2" shape="${3:-$BACKEND}" fee_fixture="${4:-}" payments wdrl=self
  [ "${5:-}" != nowdrl ] || wdrl=''
  payments=$(payments_to "$DEST")
  PAY_ACCEPTED=no PAY_WITHDRAWAL=0
  if [ "$shape" = upstream ]; then
    api "$label-fee" POST "/v2/wallets/$id/payment-fees" \
      "$(jq -n --argjson p "$payments" --arg w "$wdrl" '{payments: $p} + (if $w == "" then {} else {withdrawal: $w} end)')"
    [ -z "$fee_fixture" ] || fixture "$fee_fixture"
    api "$label" POST "/v2/wallets/$id/transactions" \
      "$(jq -n --argjson p "$payments" --arg pw "$PASS" --arg w "$wdrl" '{payments: $p, passphrase: $pw} + (if $w == "" then {} else {withdrawal: $w} end)')"
    PAY_OBSERVED=$(observed)
    if is_accepted; then
      PAY_ACCEPTED=yes
      PAY_WITHDRAWAL=$(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")
      wait_last_tx "$id" || true
    fi
  else
    construct_flow "$label" "$id" \
      "$(jq -n --argjson p "$payments" --arg w "$wdrl" '{encoding: "base16", payments: $p} + (if $w == "" then {} else {withdrawal: $w} end)')"
    PAY_OBSERVED=$(construct_observed)
    if [ "$CF_SUBMIT_STATUS" = 202 ]; then
      PAY_ACCEPTED=yes
      PAY_WITHDRAWAL=$CF_WITHDRAWAL
    fi
  fi
  PAY_OBSERVED="$PAY_OBSERVED, withdrew $PAY_WITHDRAWAL"
}

# vote LABEL WID TARGET [PUT-FIXTURE [CONSTRUCT-FIXTURE]]: a vote delegation to
# TARGET (a DRep id, abstain or no_confidence) as the Daedalus build for this
# backend sends it. On the mainnet build the governance form first estimates
# the fee with a vote-only transactions-construct and stops if that fails.
# PUT-FIXTURE names the vote request's recorded exchange on the mainnet build;
# CONSTRUCT-FIXTURE names the fee step's, recorded when it succeeds or, for a
# refused vote, when it answers same_vote. Sets VOTE_ACCEPTED and VOTE_OBSERVED.
vote() {
  local label="$1" id="$2" target="$3" vote_fixture="${4:-}" construct_fixture="${5:-}"
  VOTE_ACCEPTED=no
  if [ "$BACKEND" = upstream ]; then
    api "$label-fee" POST "/v2/wallets/$id/transactions-construct" \
      "$(jq -n --arg d "$target" '{encoding: "base16", vote: $d}')"
    VOTE_OBSERVED="fee step $(observed)"
    if ! is_accepted; then
      if [ "$R_CODE" = no_utxos_available ]; then
        fixture vote-construct-funds-held
        if [ "$label" = H-held ]; then
          # The submission the confirmation dialog would send next, and the
          # wallet's balances while the lock holds.
          api "$label-submit" PUT "/v2/dreps/$target/wallets/$id" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
          fixture vote-submit-funds-held
          HELD_SUBMIT_OBSERVED=$(observed)
          if is_accepted; then wait_last_tx "$id" || true; fi
          api "$label-wallet" GET "/v2/wallets/$id"
          fixture wallet-funds-held
          HELD_SUBMIT_OBSERVED="$HELD_SUBMIT_OBSERVED; available $(jq -r '.balance.available.quantity' "$R_FILE"), reward $(jq -r '.balance.reward.quantity' "$R_FILE"), total $(jq -r '.balance.total.quantity' "$R_FILE")"
        fi
      elif [ -n "$construct_fixture" ] && [ "$R_CODE" = same_vote ]; then
        fixture "$construct_fixture"
      fi
      return 0
    fi
    [ -z "$construct_fixture" ] || fixture "$construct_fixture"
    VOTE_OBSERVED="$VOTE_OBSERVED (cast_vote $(jq -r '[.coin_selection.certificates[]? | select(.certificate_type == "cast_vote") | .vote] | join(",")' "$R_FILE"))"
    api "$label" PUT "/v2/dreps/$target/wallets/$id" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
    [ -z "$vote_fixture" ] || fixture "$vote_fixture"
    VOTE_OBSERVED="$VOTE_OBSERVED, vote $(observed)"
    if is_accepted && wait_last_tx "$id"; then VOTE_ACCEPTED=yes; fi
  else
    construct_flow "$label" "$id" "$(jq -n --arg d "$target" '{encoding: "base16", vote: $d}')"
    if [ -n "$construct_fixture" ]; then
      R_BASE=${CF_CONSTRUCT_FILE%.res.json}
      fixture "$construct_fixture"
    fi
    VOTE_OBSERVED=$(construct_observed)
    [ "$CF_SUBMIT_STATUS" != 202 ] || VOTE_ACCEPTED=yes
  fi
}

# undelegate LABEL WID: the undelegation Daedalus sends on this backend. Sets
# QUIT_ACCEPTED, QUIT_WITHDRAWAL and QUIT_OBSERVED.
undelegate() {
  local label="$1" id="$2"
  QUIT_ACCEPTED=no QUIT_WITHDRAWAL=0
  if [ "$BACKEND" = upstream ]; then
    api "$label" DELETE "/v2/stake-pools/*/wallets/$id" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
    QUIT_OBSERVED=$(observed)
    if is_accepted; then
      QUIT_WITHDRAWAL=$(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")
      if wait_last_tx "$id"; then QUIT_ACCEPTED=yes; fi
    fi
  else
    construct_flow "$label" "$id" \
      '{"encoding": "base16", "delegations": [{"quit": {"stake_key_index": "0H"}}], "withdrawal": "self"}'
    QUIT_OBSERVED=$(construct_observed)
    if [ "$CF_SUBMIT_STATUS" = 202 ]; then
      QUIT_ACCEPTED=yes
      QUIT_WITHDRAWAL=$CF_WITHDRAWAL
    fi
  fi
}

# funds_seen BEFORE WID: describes available funds and pending transactions.
funds_seen() {
  printf 'available before %s, after %s; pending [%s]' "$1" "$(available_of "$2")" "$(pending_of "$2")"
}

# funds_unchanged BEFORE WID: the wallet still offers every lovelace it
# offered before, and records no pending transaction.
funds_unchanged() {
  [ "$(available_of "$2")" = "$1" ] && [ -z "$(pending_of "$2")" ]
}

# Setup ---------------------------------------------------------------------------

setup() {
  cluster_start
  save_var PASS "Secret-passphrase-1"
  local w size mnemonic id
  for w in 1:24 2:15 3:24 4:24; do
    size=${w#*:}
    w=${w%:*}
    mnemonic=$("$WALLET" recovery-phrase generate --size "$size")
    echo "$mnemonic" >"$STATE/w$w.mnemonic"
    api "create-w$w" POST /v2/wallets "$(jq -n --arg n "W$w" --arg m "$mnemonic" --arg p "$PASS" \
      '{name: $n, mnemonic_sentence: ($m | split(" ")), passphrase: $p}')"
    id=$(jq -r .id "$R_FILE")
    [ -n "$id" ] && [ "$id" != null ] || die "wallet W$w was not created: $(observed)"
    save_var "W${w}" "$id"
    wait_ready "$id"
    save_var "W${w}_ADDR" "$(unused_address_of "$id")"
  done
  save_var DEST "$(utxo_address 2)"

  local p k
  for p in 1 2; do
    k="$STATE/data/pools-keys/pool$p"
    save_var "POOL$p" "$("$CLI" conway stake-pool id --cold-verification-key-file "$k/cold.vkey" --output-bech32)"
    # Genesis pools have no registration certificate on chain, so the
    # wallet's pool registry never lists them. Re-registering is a parameter
    # update and takes no deposit.
    "$CLI" conway stake-pool registration-certificate \
      --cold-verification-key-file "$k/cold.vkey" --vrf-verification-key-file "$k/vrf.vkey" \
      --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
      --pool-reward-account-verification-key-file "$k/staking-reward.vkey" \
      --pool-owner-stake-verification-key-file "$k/staking-reward.vkey" \
      --out-file "$k/onchain-reg.cert"
  done

  local d vkey
  for d in A B; do
    mkdir -p "$STATE/drep$d"
    vkey="$STATE/drep$d/drep.vkey"
    "$CLI" conway governance drep key-gen --verification-key-file "$vkey" \
      --signing-key-file "$STATE/drep$d/drep.skey"
    "$CLI" conway governance drep registration-certificate --drep-verification-key-file "$vkey" \
      --key-reg-deposit-amt 500000000 --out-file "$STATE/drep$d/reg.cert"
    "$CLI" conway governance drep retirement-certificate --drep-verification-key-file "$vkey" \
      --deposit-amt 500000000 --out-file "$STATE/drep$d/retire.cert"
    save_var "DREP_${d}_LEGACY" "$("$CLI" conway governance drep id --drep-verification-key-file "$vkey" --output-bech32)"
    save_var "DREP_${d}_CIP129" "$("$CLI" conway governance drep id --drep-verification-key-file "$vkey" --output-cip129)"
    save_var "DREP_${d}_HEX" "$("$CLI" conway governance drep id --drep-verification-key-file "$vkey" --output-hex)"
  done

  local pk="$STATE/data/pools-keys"
  cli_tx fund-and-register 1 \
    --tx-out "$W1_ADDR+100000000000" --tx-out "$W2_ADDR+10000000000" --tx-out "$W3_ADDR+1000000000" --tx-out "$W4_ADDR+10000000000" \
    --certificate-file "$STATE/drepA/reg.cert" --certificate-file "$STATE/drepB/reg.cert" \
    --certificate-file "$pk/pool1/onchain-reg.cert" --certificate-file "$pk/pool2/onchain-reg.cert" \
    --witness-override 7 -- "$STATE/drepA/drep.skey" "$STATE/drepB/drep.skey" \
    "$pk/pool1/cold.skey" "$pk/pool1/staking-reward.skey" \
    "$pk/pool2/cold.skey" "$pk/pool2/staking-reward.skey" ||
    die "funding transaction: $CLI_RESULT (see $OUT/cli-fund-and-register.*.txt)"

  local _ n=0
  for _ in $(seq 1 60); do
    n=$(curl -s "$API/v2/stake-pools?stake=1000000" |
      jq --arg a "$POOL1" --arg b "$POOL2" '[.[] | select(.id == $a or .id == $b)] | length')
    [ "$n" = 2 ] && break
    sleep 3
  done
  [ "$n" = 2 ] || die "the wallet's pool registry does not list both pools"
  for id in "$W1" "$W2" "$W3" "$W4"; do
    for _ in $(seq 1 60); do
      [ "$(available_of "$id")" -gt 0 ] && break
      sleep 2
    done
  done

  # W1, W2 and W4 delegate as a Daedalus mainnet user does: stake first, then
  # voting power to DRep A.
  for id in "$W1" "$W2" "$W4"; do
    api setup-delegation-fees GET "/v2/wallets/$id/delegation-fees"
    api setup-join-pool1 PUT "/v2/stake-pools/$POOL1/wallets/$id" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
    { is_accepted && wait_last_tx "$id"; } || die "joining pool1: $(observed)"
    api setup-vote-drepA PUT "/v2/dreps/$DREP_A_CIP129/wallets/$id" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
    { is_accepted && wait_last_tx "$id"; } || die "delegating to DRep A: $(observed)"
  done

  # W1's and W4's stake keys, for transactions built by cardano-cli alone.
  derive_stake_keys "$STATE/w1.mnemonic" "$STATE/w1keys" "$W1"
  derive_stake_keys "$STATE/w4.mnemonic" "$STATE/w4keys" "$W4"

  # The wallet filled its DRep registry when it started, before DReps A and B
  # existed.
  wallet_restart "$W1" "$W2" "$W3" "$W4"
  snapshot "setup: W1, W2 and W4 delegated to DRep A" "$W1" "$W2" "$W4"
}

# Rows for the control that leaves the reward in place and for the
# withdrawal-only transaction, on the wallet that is delegated to DRep A.
withdrawal_rows() {
  local shape tag before reward after expected fee seen withdraws
  for shape in upstream cip30; do
    tag=${shape/upstream/mainnet}
    if ! wait_withdrawable "$W1"; then
      result "D-pay-$tag" "Payment without the withdrawal while delegated to DRep A" "payment, $(build_of "$shape")" \
        "accepted, nothing withdrawn, reward not lower" "reward $(reward_of "$W1")" PRECONDITION
      continue
    fi
    reward=$(reward_of "$W1")
    pay "D-pay-$tag" "$W1" "$shape" "" nowdrl
    after=$(reward_of "$W1")
    check "D-pay-$tag" "Payment without the withdrawal while delegated to DRep A" "payment, $(build_of "$shape")" \
      "accepted, nothing withdrawn, reward not lower" "$PAY_OBSERVED, reward $reward before and $after after" \
      '[ "$PAY_ACCEPTED" = yes ] && [ "$PAY_WITHDRAWAL" = 0 ] && [ "$after" -ge "$reward" ]'
  done

  # The withdrawal-only transaction.
  if wait_withdrawable "$W1"; then
    reward=$(reward_of "$W1")
    before=$(available_of "$W1")
    construct_flow W-only "$W1" '{"encoding": "base16", "withdrawal": "self"}'
    R_BASE=${CF_CONSTRUCT_FILE%.res.json}
    if [ "$CF_CONSTRUCT_STATUS" = 202 ]; then
      fixture withdrawal-only-construct
    fi
    fee=$(jq -r '.fee.quantity // 0' "$CF_CONSTRUCT_FILE")
    after=$(available_of "$W1")
    expected=$((before + CF_WITHDRAWAL - fee))
    check W-only "Withdrawal-only transaction while delegated to DRep A" \
      "construct {withdrawal: self}, sign, submit" \
      "construct 202 with one withdrawal equal to the reward and no payment outputs, submit 202, available up by the withdrawal less the fee" \
      "$(construct_observed), withdrawal $CF_WITHDRAWAL of reward $reward, outputs $(jq -r '.coin_selection.outputs | length' "$CF_CONSTRUCT_FILE" 2>/dev/null), withdrawals $(jq -r '.coin_selection.withdrawals | length' "$CF_CONSTRUCT_FILE" 2>/dev/null), available $before to $after, fee $fee" \
      '[ "$CF_CONSTRUCT_STATUS" = 202 ] && [ "$CF_SUBMIT_STATUS" = 202 ] && [ "$CF_WITHDRAWAL" = "$reward" ] &&
       [ "$(jq -r ".coin_selection.outputs | length" "$CF_CONSTRUCT_FILE")" = 0 ] &&
       [ "$(jq -r ".coin_selection.withdrawals | length" "$CF_CONSTRUCT_FILE")" = 1 ] && [ "$after" = "$expected" ]'
  else
    result W-only "Withdrawal-only transaction while delegated to DRep A" "construct {withdrawal: self}, sign, submit" \
      "accepted, rewards withdrawn" "reward $(reward_of "$W1")" PRECONDITION
  fi

  # The hardware-wallet submission of the same transaction: the selection is
  # built into a body outside the wallet, signed by transactions-sign (standing
  # in for the device) and submitted through the build's hardware endpoint.
  hardware_withdrawal_only "$W1"

  # A wallet with nothing to withdraw: the construct still answers, without a
  # withdrawal.
  api W-only-none-construct POST "/v2/wallets/$W3/transactions-construct" '{"encoding": "base16", "withdrawal": "self"}'
  fixture withdrawal-only-none
  seen=$(observed)
  withdraws=$(jq -r '[.coin_selection.withdrawals[]?] | length' "$R_FILE" 2>/dev/null || echo '?')
  result W-only-none "Withdrawal-only construct for a wallet with no reward" "construct {withdrawal: self}, not submitted" \
    "HTTP 202, no withdrawals" "$seen, withdrawals $withdraws" \
    "$([ "$R_STATUS" = 202 ] && [ "$withdraws" = 0 ] && echo PASS || echo FAIL)"

  # Measurement: the payment forms with an empty payments list.
  api W-forms-transactions POST "/v2/wallets/$W1/transactions" \
    "$(jq -n --arg pw "$PASS" '{payments: [], passphrase: $pw, withdrawal: "self"}')"
  seen="transactions $(observed)"
  api W-forms-fees POST "/v2/wallets/$W1/payment-fees" '{"payments": [], "withdrawal": "self"}'
  seen="$seen; payment-fees $(observed)"
  api W-forms-selection POST "/v2/wallets/$W1/coin-selections/random" '{"payments": [], "withdrawal": "self"}'
  seen="$seen; coin-selections $(observed)"
  release_pending "$W1" W-forms
  result W-forms "Payment forms with an empty payments list" "payments: [], withdrawal: self" "(measurement)" "$seen" NOTE
}

# hardware_withdrawal_only WID: W-hw. The withdrawal-only construct, its coin
# selection built into a body with cardano-cli, signed by transactions-sign and
# submitted through the hardware endpoint of this backend.
hardware_withdrawal_only() {
  local id="$1" seen what="Hardware-wallet withdrawal-only transaction" reward withdrawn txid body="$STATE/w-hw.body.cbor.hex" signed note=''
  local req
  req="construct {withdrawal: self}, sign, $([ "$BACKEND" = upstream ] && echo 'POST /v2/proxy/transactions' || echo 'POST /v2/wallets/{id}/transaction-submission')"
  local want="accepted, in the ledger, withdraws the reward"
  if ! wait_withdrawable "$id"; then
    result W-hw "$what" "$req" "$want" "reward $(reward_of "$id")" PRECONDITION
    return 0
  fi
  reward=$(reward_of "$id")
  api W-hw-construct POST "/v2/wallets/$id/transactions-construct" '{"encoding": "base16", "withdrawal": "self"}'
  if ! is_accepted; then
    result W-hw "$what" "$req" "$want" "construct $(observed)" FAIL
    return 0
  fi
  jq '.coin_selection' "$R_FILE" >"$STATE/w-hw.selection.json"
  local unsigned
  unsigned=$(jq -r .transaction "$R_FILE")
  if ! withdrawal_selection_body "$STATE/w-hw.selection.json" "$body"; then
    result W-hw "$what" "$req" "$want" \
      "the selection could not be built into a body ($(head -c 200 "$body.build.txt" 2>/dev/null))" PRECONDITION
    return 0
  fi
  api W-hw-sign POST "/v2/wallets/$id/transactions-sign" \
    "$(jq -n --arg t "$(cat "$body")" --arg p "$PASS" '{transaction: $t, passphrase: $p, encoding: "base16"}')"
  if ! is_accepted; then
    note="transactions-sign refused the cardano-cli body ($(observed): $(jq -r '.message // ""' "$R_FILE" | head -c 300)); the construct's own CBOR is sent instead"
    api W-hw-sign-own POST "/v2/wallets/$id/transactions-sign" \
      "$(jq -n --arg t "$unsigned" --arg p "$PASS" '{transaction: $t, passphrase: $p, encoding: "base16"}')"
    if ! is_accepted; then
      result W-hw "$what" "$req" "$want" "$note; and its own CBOR refused: $(observed)" PRECONDITION
      return 0
    fi
  fi
  signed=$(jq -r .transaction "$R_FILE")
  echo "$signed" >"$STATE/w-hw.signed.cbor.hex"
  if [ "$BACKEND" = upstream ]; then
    echo "$signed" | xxd -r -p >"$STATE/w-hw.signed.cbor"
    api_octets W-hw /v2/proxy/transactions "$STATE/w-hw.signed.cbor"
    txid=$(jq -r '.id // empty' "$R_FILE")
  else
    if ! dapp_submit W-hw "$id" "$STATE/w-hw.signed.cbor.hex"; then
      result W-hw "$what" "$req" "$want" "no request sent: $DAPP_SUBMIT_WHY" PRECONDITION
      return 0
    fi
    txid=$(jq -r '.transaction_id // empty' "$R_FILE")
  fi
  seen=$(observed)
  if is_accepted && [ -n "$txid" ] && wait_tx "$id" "$txid"; then
    withdrawn=$(curl -s "$API/v2/wallets/$id/transactions/$txid" | jq -r '[.withdrawals[]?.amount.quantity] | add // 0')
    seen="$seen, in ledger, withdrew $withdrawn of reward $reward"
    [ -z "$note" ] || seen="$note; $seen"
    if [ -n "$note" ]; then
      result W-hw "$what" "$req" "$want" "$seen" PRECONDITION
    else
      check W-hw "$what" "$req" "$want" "$seen" '[ "$withdrawn" = "$reward" ]'
    fi
  else
    result W-hw "$what" "$req" "$want" "$seen, not in the ledger" FAIL
  fi
}

# Control: DRep A registered ----------------------------------------------------------

control() {
  api C-reg GET "/v2/dreps/$DREP_A_CIP129"
  fixture drep-registered
  check C-reg "Registry lookup of a registered DRep" "GET /v2/dreps/{A}" \
    "HTTP 200" "$(observed)" '[ "$R_STATUS" = 200 ]'

  local shape seen
  for shape in upstream cip30; do
    if ! wait_withdrawable "$W1"; then
      result "C-pay-${shape/upstream/mainnet}" "Payment while delegated to DRep A" "payment, $(build_of "$shape")" \
        "accepted, rewards withdrawn" "reward $(reward_of "$W1")" PRECONDITION
      continue
    fi
    pay "C-pay-${shape/upstream/mainnet}" "$W1" "$shape"
    check "C-pay-${shape/upstream/mainnet}" "Payment while delegated to DRep A" "payment, $(build_of "$shape")" \
      "accepted, rewards withdrawn" "$PAY_OBSERVED" \
      '[ "$PAY_ACCEPTED" = yes ] && [ "$PAY_WITHDRAWAL" -gt 0 ]'
  done

  withdrawal_rows
  seen=$(wallet_ledger_field "$W1")
  check C-field "Ledger vote delegation reported by the wallet" \
    "GET /v2/wallets/{id} .$LEDGER_VOTE_FIELD" "drep:A" "$seen" '[ "$seen" = drep:A ]'
}

# Retirement ----------------------------------------------------------------------

# retire DREP WID: retires the DRep with cardano-cli and checks that the
# ledger cleared the wallet's vote delegation.
retire() {
  local drep="$1" id="$2" seen
  cli_tx "retire-drep$drep" 1 --certificate-file "$STATE/drep$drep/retire.cert" \
    --witness-override 2 -- "$STATE/drep$drep/drep.skey" ||
    die "retiring DRep $drep: $CLI_RESULT"
  sleep 3
  snapshot "DRep $drep retired" "$W1" "$W2"
  seen=$(ledger_vote "$id")
  if [ "$seen" != none ]; then
    result "R$drep-ledger" "Ledger clears the vote delegation when the DRep retires" \
      "cardano-cli query stake-address-info" "none" "$seen" PRECONDITION
    return 1
  fi
  result "R$drep-ledger" "Ledger clears the vote delegation when the DRep retires" \
    "cardano-cli query stake-address-info" "none" "$seen" PASS
  seen=$(wallet_history_vote "$id")
  result "R$drep-history" "Vote the wallet reports from its own history" \
    "GET /v2/wallets/{id} .delegation.*.voting" "(history, no expectation)" "$seen" NOTE
  wait_ledger_field "$id" none || true
  seen=$(wallet_ledger_field "$id")
  check "R$drep-field" "Ledger vote delegation reported by the wallet" \
    "GET /v2/wallets/{id} .$LEDGER_VOTE_FIELD" "none" "$seen" '[ "$seen" = none ]'
}

# Withdrawal-carrying requests after the retirement ----------------------------------

# current_pool WID: the stake pool the wallet reports as its latest
# delegation, as Daedalus reads it.
current_pool() {
  wallet_json "$1" | jq -r '[.delegation.active.target, (.delegation.next[]?.target)]
    | map(select(. != null)) | last // ""'
}

# hardware_proxy_payment WID: submits the P-hw selection the way a hardware
# wallet's payment is: the selection built into a transaction body outside the
# wallet, signed by transactions-sign (standing in for the device) and posted
# to /v2/proxy/transactions. A body the wallet will not sign is a PRECONDITION.
hardware_proxy_payment() {
  local id="$1" seen what="Hardware-wallet payment submitted through the proxy, no withdrawal"
  local req="POST /v2/proxy/transactions, signed CBOR of the P-hw selection"
  local want="HTTP 202, in the ledger, no withdrawal" body="$STATE/p-hw.body.cbor.hex" signed txid
  if [ ! -s "$STATE/p-hw.selection.json" ]; then
    result P-hw-proxy "$what" "$req" "$want" "no P-hw selection" PRECONDITION
    return 0
  fi
  if ! selection_body "$STATE/p-hw.selection.json" "$body"; then
    result P-hw-proxy "$what" "$req" "$want" \
      "the selection could not be built into a body ($(head -c 200 "$body.build.txt" 2>/dev/null))" PRECONDITION
    return 0
  fi
  api P-hw-proxy-sign POST "/v2/wallets/$id/transactions-sign" \
    "$(jq -n --arg t "$(cat "$body")" --arg p "$PASS" '{transaction: $t, passphrase: $p, encoding: "base16"}')"
  if ! is_accepted; then
    result P-hw-proxy "$what" "$req" "$want" "transactions-sign refused the body: $(observed): $(jq -r '.message // ""' "$R_FILE" | head -c 300)" PRECONDITION
    return 0
  fi
  signed=$(jq -r .transaction "$R_FILE")
  echo "$signed" | xxd -r -p >"$STATE/p-hw.signed.cbor"
  api_octets P-hw-proxy /v2/proxy/transactions "$STATE/p-hw.signed.cbor"
  txid=$(jq -r '.id // empty' "$R_FILE")
  seen=$(observed)
  if is_accepted && [ -n "$txid" ] && wait_tx "$id" "$txid"; then
    seen="$seen, in ledger, withdrew $(curl -s "$API/v2/wallets/$id/transactions/$txid" | jq -r '[.withdrawals[]?.amount.quantity] | add // 0')"
    check P-hw-proxy "$what" "$req" "$want" "$seen" '[ "$R_STATUS" = 202 ] && [[ "$seen" == *"withdrew 0" ]]'
  else
    result P-hw-proxy "$what" "$req" "$want" "$seen, not in the ledger" FAIL
  fi
}

after_retirement() {
  local payments before seen withdrawal
  payments=$(payments_to "$DEST")
  if ! wait_withdrawable "$W1"; then
    result F-rewards "Reward balance above the wallet's withdrawal threshold" "-" \
      "reward > $MIN_REWARD" "$(reward_of "$W1")" PRECONDITION
    return 0
  fi
  api F-wallet GET "/v2/wallets/$W1"
  fixture wallet-held-rewards

  api F-fee POST "/v2/wallets/$W1/payment-fees" \
    "$(jq -n --argjson p "$payments" '{payments: $p, withdrawal: "self"}')"
  fixture payment-fee
  check F-fee "Payment fee estimate requesting the withdrawal" "payment-fees, mainnet build" \
    "$GATE" "$(observed)" is_refused_before_submission

  before=$(available_of "$W1")
  api F-pay POST "/v2/wallets/$W1/transactions" \
    "$(jq -n --argjson p "$payments" --arg pw "$PASS" '{payments: $p, passphrase: $pw, withdrawal: "self"}')"
  fixture payment-refused
  check F-pay "Payment requesting the withdrawal" "POST /transactions, mainnet build" \
    "$GATE" "$(observed)" is_refused_before_submission
  availability_line "$W1" F-pay >/dev/null
  check F-pay-funds "Funds available after the refused payment" "GET /v2/wallets/{id}" \
    "available unchanged, no pending transaction" "$(funds_seen "$before" "$W1")" \
    'funds_unchanged "$before" "$W1"'
  release_pending "$W1" F-pay

  api P-fee POST "/v2/wallets/$W1/payment-fees" "$(jq -n --argjson p "$payments" '{payments: $p}')"
  fixture payment-fee-nowdrl
  check P-fee "Payment fee estimate, no withdrawal requested" "payment-fees, no withdrawal" \
    "HTTP 202" "$(observed)" '[ "$R_STATUS" = 202 ]'
  cp "$R_FILE" "$STATE/p-fee.res.json"

  # An amount the funds other than the reward cannot cover but the total
  # could: the held reward is not spendable.
  local total reward spendable
  total=$(wallet_json "$W1" | jq -r '.balance.total.quantity')
  reward=$(reward_of "$W1")
  spendable=$((total - reward))
  api N-held POST "/v2/wallets/$W1/payment-fees" \
    "$(jq -n --arg a "$DEST" --argjson q $(((spendable + total) / 2)) \
      '{payments: [{address: $a, amount: {quantity: $q, unit: "lovelace"}}]}')"
  fixture payment-fee-held-not-enough
  check N-held "Payment fee estimate above the funds other than the reward, no withdrawal" \
    "payment-fees, amount between spendable ($spendable) and total ($total)" \
    "HTTP 403 not_enough_money" "$(observed)" '[ "$R_STATUS" = 403 ] && [ "$R_CODE" = not_enough_money ]'

  api F-pay-nowdrl POST "/v2/wallets/$W1/transactions" \
    "$(jq -n --argjson p "$payments" --arg pw "$PASS" '{payments: $p, passphrase: $pw}')"
  fixture payment-nowdrl
  check F-pay-nowdrl "Payment without a withdrawal" "POST /transactions, no withdrawal" \
    "accepted" "$(observed)" is_accepted
  if is_accepted; then
    wait_last_tx "$W1" || true
    seen="fee $(jq -r .fee.quantity "$R_FILE") against estimate $(jq -r '"\(.estimated_min.quantity) to \(.estimated_max.quantity)"' "$STATE/p-fee.res.json"), withdraws $(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")"
    check P-agree "Submitted payment agrees with its fee estimate" "POST /transactions against payment-fees" \
      "fee within the estimate, no withdrawal" "$seen" \
      'fee_within_estimate "$STATE/p-fee.res.json" "$R_FILE" && [ "$(jq "[.withdrawals[]?] | length" "$R_FILE")" = 0 ]'
  else
    result P-agree "Submitted payment agrees with its fee estimate" "POST /transactions against payment-fees" \
      "fee within the estimate, no withdrawal" "no payment: $(observed)" PRECONDITION
  fi

  api F-hw POST "/v2/wallets/$W1/coin-selections/random" \
    "$(jq -n --argjson p "$payments" '{payments: $p, withdrawal: "self", metadata: null}')"
  fixture hardware-selection
  seen=$(observed)
  if is_accepted; then
    withdrawal=$(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")
    seen="$seen, selection withdraws $withdrawal"
  fi
  check F-hw "Hardware-wallet payment selection requesting the withdrawal" \
    "coin-selections/random, mainnet build" "$GATE" "$seen" is_refused_before_submission

  local pool
  pool=$(current_pool "$W1")
  api F-hw-quit POST "/v2/wallets/$W1/coin-selections/random" \
    "$(jq -n --arg p "$pool" '{delegation_action: {action: "quit", pool: $p}}')"
  fixture hardware-quit-refused
  seen=$(observed)
  if is_accepted; then
    seen="$seen, selection withdraws $(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")"
  fi
  check F-hw-quit "Hardware-wallet undelegation selection from an account with no vote delegation" \
    "coin-selections/random {delegation_action: quit}, mainnet build" "$GATE" "$seen" is_refused_before_submission

  api P-hw POST "/v2/wallets/$W1/coin-selections/random" \
    "$(jq -n --argjson p "$payments" '{payments: $p, metadata: null}')"
  fixture hardware-selection-nowdrl
  seen=$(observed)
  if is_accepted; then
    seen="$seen, selection withdraws $(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")"
    cp "$R_FILE" "$STATE/p-hw.selection.json"
  fi
  check P-hw "Hardware-wallet payment selection, no withdrawal" "coin-selections/random, no withdrawal" \
    "HTTP 200, no withdrawals" "$seen" \
    '[ "$R_STATUS" = 200 ] && [ "$(jq "[.withdrawals[]?] | length" "$R_FILE")" = 0 ]'
  hardware_proxy_payment "$W1"

  before=$(available_of "$W1")
  construct_flow F-construct "$W1" \
    "$(jq -n --argjson p "$payments" '{encoding: "base16", payments: $p, withdrawal: "self"}')"
  R_BASE=${CF_CONSTRUCT_FILE%.res.json}
  if [ "$CF_CONSTRUCT_STATUS" != 202 ]; then
    [ "$CF_CONSTRUCT_STATUS" != 403 ] || fixture construct-payment-refused
  else
    fixture construct-payment
    if [ -n "$CF_SUBMIT_FILE" ]; then
      R_BASE=${CF_SUBMIT_FILE%.res.json}
      fixture submit-refused
    fi
    if [ -n "$CF_PROXY_BASE" ]; then
      R_BASE=$CF_PROXY_BASE
      fixture proxy-refused
    fi
  fi
  check F-construct "Payment requesting the withdrawal" "construct, sign, submit, cip30 builds" \
    "$GATE (at construct)" "$(construct_observed)" construct_refused_before_submission
  check F-construct-funds "Funds available after the refused payment" "GET /v2/wallets/{id}" \
    "available unchanged, no pending transaction" "$(funds_seen "$before" "$W1")" \
    'funds_unchanged "$before" "$W1"'
  release_pending "$W1" F-construct

  if [ "$BACKEND" = cip30 ]; then
    before=$(available_of "$W1")
    if [ -s "$OUT/F-construct.signed.cbor.hex" ] && dapp_submit F-dapp-submit "$W1" "$OUT/F-construct.signed.cbor.hex"; then
      fixture dapp-submission-refused
      check F-dapp-submit "Hardware-wallet submission of the signed payment with the withdrawal" \
        "POST /v2/wallets/{id}/transaction-submission, cip30 builds" \
        "HTTP 409 dapp_submission_failed, nothing submitted (available unchanged, no pending transaction)" \
        "$(observed); $(funds_seen "$before" "$W1")" \
        '[ "$R_STATUS" = 409 ] && [ "$R_CODE" = dapp_submission_failed ] && funds_unchanged "$before" "$W1"'
      release_pending "$W1" F-dapp-submit
    else
      result F-dapp-submit "Hardware-wallet submission of the signed payment with the withdrawal" \
        "POST /v2/wallets/{id}/transaction-submission, cip30 builds" \
        "HTTP 409 dapp_submission_failed" "no request sent: ${DAPP_SUBMIT_WHY:-no signed payment}" PRECONDITION
    fi
  fi

  construct_flow W-only-gate "$W1" '{"encoding": "base16", "withdrawal": "self"}'
  if [ "$CF_CONSTRUCT_STATUS" = 403 ]; then
    R_BASE=${CF_CONSTRUCT_FILE%.res.json}
    fixture withdrawal-only-refused
  elif [ -n "$CF_SUBMIT_FILE" ] && [ "$CF_SUBMIT_STATUS" != 202 ]; then
    R_BASE=${CF_SUBMIT_FILE%.res.json}
    fixture withdrawal-only-submit-refused
  fi
  check W-only-gate "Withdrawal-only transaction while the account has no vote delegation" \
    "construct {withdrawal: self}, sign, submit" "$GATE (at construct)" "$(construct_observed)" \
    construct_refused_before_submission
  release_pending "$W1" W-only-gate

  construct_flow P-construct "$W1" "$(jq -n --argjson p "$payments" '{encoding: "base16", payments: $p}')"
  if [ "$CF_CONSTRUCT_STATUS" = 202 ]; then
    R_BASE=${CF_CONSTRUCT_FILE%.res.json}
    fixture construct-payment-nowdrl
  fi
  check P-construct "Payment without a withdrawal" "construct, sign, submit, no withdrawal" \
    "construct 202, submit 202, no withdrawal" "$(construct_observed), withdrew $CF_WITHDRAWAL" \
    '[ "$CF_CONSTRUCT_STATUS" = 202 ] && [ "$CF_SUBMIT_STATUS" = 202 ] && [ "$CF_WITHDRAWAL" = 0 ]'

  construct_flow F-pool-wdrl "$W1" \
    "$(jq -n --arg p "$POOL2" '{encoding: "base16", delegations: [{join: {pool: $p, stake_key_index: "0H"}}], withdrawal: "self"}')"
  check F-pool-wdrl "Stake pool delegation combined with the withdrawal" "construct, variant" \
    "$GATE (at construct)" "$(construct_observed)" construct_refused_before_submission
  release_pending "$W1" F-pool-wdrl

  before=$(available_of "$W1")
  api F-vote-retired PUT "/v2/dreps/$DREP_A_CIP129/wallets/$W1" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
  fixture vote-retired-drep
  [ "$R_CODE" != same_vote ] || fixture vote-same-drep
  seen=$(observed)
  check F-vote-retired "Vote delegation to the retired DRep" "PUT /v2/dreps/{A}, mainnet build" \
    "HTTP 404 not_found, nothing submitted (available unchanged, no pending transaction)" \
    "$seen; $(funds_seen "$before" "$W1")" \
    '[ "$R_STATUS" = 404 ] && [ "$R_CODE" = not_found ] && funds_unchanged "$before" "$W1"'
  release_pending "$W1" F-vote-retired

  before=$(available_of "$W1")
  construct_flow F-vote-construct-retired "$W1" \
    "$(jq -n --arg d "$DREP_A_CIP129" '{encoding: "base16", vote: $d}')"
  R_BASE=${CF_CONSTRUCT_FILE%.res.json}
  fixture vote-construct-retired
  check F-vote-construct-retired "Vote delegation to the retired DRep" "construct, both builds" \
    "HTTP 404 not_found at construct, nothing submitted (available unchanged, no pending transaction)" \
    "$(construct_observed); $(funds_seen "$before" "$W1")" \
    '[ "$CF_CONSTRUCT_STATUS" = 404 ] && [ "$CF_CONSTRUCT_CODE" = not_found ] && funds_unchanged "$before" "$W1"'
  release_pending "$W1" F-vote-construct-retired

  construct_flow F-vote-wdrl "$W1" \
    "$(jq -n --arg d "$DREP_B_CIP129" '{encoding: "base16", vote: $d, withdrawal: "self"}')"
  check F-vote-wdrl "Vote delegation combined with the withdrawal" "construct, variant" \
    "$GATE (at construct)" "$(construct_observed)" construct_refused_before_submission
  release_pending "$W1" F-vote-wdrl

  # The ledger rule with no wallet involved: one transaction that
  # vote-delegates W1 to DRep B and withdraws its whole reward balance.
  local stake balance failure
  stake=$(stake_address_of "$W1")
  "$CLI" conway stake-address vote-delegation-certificate \
    --stake-verification-key-file "$STATE/w1keys/stake.vkey" \
    --drep-key-hash "$DREP_B_HEX" --out-file "$STATE/w1-vote-drepB.cert"
  balance=$("$CLI" conway query stake-address-info --address "$stake" | jq '.[0].rewardAccountBalance')
  cli_tx F-ledger 1 --certificate-file "$STATE/w1-vote-drepB.cert" \
    --withdrawal "$stake+$balance" --witness-override 2 -- "$STATE/w1keys/stake.skey" || true
  failure=$(cat "$OUT/cli-F-ledger.build.txt" "$OUT/cli-F-ledger.submit.txt" 2>/dev/null |
    grep -oE 'ConwayWdrlNotDelegatedToDRep' | head -1 || true)
  check F-ledger "Vote delegation and withdrawal in one transaction, ledger only" \
    "cardano-cli transaction" "refused with ConwayWdrlNotDelegatedToDRep" \
    "$CLI_RESULT${failure:+ ($failure)}" '[ -n "$failure" ]'

  construct_flow F-quit-construct "$W1" \
    '{"encoding": "base16", "delegations": [{"quit": {"stake_key_index": "0H"}}], "withdrawal": "self"}'
  check F-quit-construct "Undelegation (always withdraws)" "construct, cip30 builds" \
    "$GATE (at construct)" "$(construct_observed)" construct_refused_before_submission
  if [ "$CF_CONSTRUCT_STATUS" = 403 ]; then
    R_BASE=${CF_CONSTRUCT_FILE%.res.json}
    fixture quit-construct-refused
  elif [ -n "$CF_SUBMIT_FILE" ]; then
    R_BASE=${CF_SUBMIT_FILE%.res.json}
    fixture quit-construct-submit-refused
  fi
  release_pending "$W1" F-quit-construct

  # A pool change, and straight back within the epoch, so W1 keeps earning
  # from the producing pool.
  api F-pool PUT "/v2/stake-pools/$POOL2/wallets/$W1" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
  seen=$(observed) withdrawal=0
  if is_accepted; then
    withdrawal=$(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")
    seen="$seen, withdrew $withdrawal"
    wait_last_tx "$W1" || true
  fi
  check F-pool "Stake pool delegation" "PUT /v2/stake-pools/{pool}, mainnet build" \
    "accepted, no withdrawal" "$seen" 'is_accepted && [ "$withdrawal" = 0 ]'
  api F-pool-back PUT "/v2/stake-pools/$POOL1/wallets/$W1" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
  if is_accepted; then wait_last_tx "$W1" || true; fi
}

# The DRep registry as the Governance Center reads it ---------------------------------

# probe KIND N PATH: one timed request in the background, appended to
# drep-latency.tsv as kind, n, start offset (s), HTTP status, seconds.
probe() {
  local kind="$1" n="$2" path="$3" start
  start=$(date +%s.%N)
  (
    out=$(curl -s -o /dev/null -w '%{http_code} %{time_total}' --max-time 600 "$API$path" || true)
    printf '%s\t%s\t%s\t%s\t%s\n' "$kind" "$n" \
      "$(awk -v a="$start" -v b="$PROBE_T0" 'BEGIN { printf "%.2f", a - b }')" \
      "${out%% *}" "${out#* }" >>"$OUT/drep-latency.tsv"
  ) &
  PROBES+=($!)
}

# Waits for the outstanding probes only; the node and wallet are background
# processes too.
wait_probes() {
  [ "${#PROBES[@]}" = 0 ] || wait "${PROBES[@]}"
  PROBES=()
}

# stats KIND: median, maximum and count of one kind of probe.
stats() {
  awk -F'\t' -v k="$1" '$1 == k { print $5 }' "$OUT/drep-latency.tsv" | sort -n |
    awk '{ v[NR] = $1 } END {
      if (NR == 0) { print "no samples"; exit }
      m = (NR % 2) ? v[(NR + 1) / 2] : (v[NR / 2] + v[NR / 2 + 1]) / 2
      printf "median %.3f s, max %.3f s, n=%d", m, v[NR], NR }'
}

statuses() {
  awk -F'\t' -v k="$1" '$1 == k { print $4 }' "$OUT/drep-latency.tsv" | sort | uniq -c |
    awk '{ printf "%sHTTP %s x%s", (NR > 1 ? ", " : ""), $2, $1 }'
}

governance_registry() {
  release_pending "$W1" G
  release_pending "$W2" G
  release_pending "$W3" G
  release_pending "$W4" G
  wallet_restart "$W1" "$W2" "$W3" "$W4"

  # Daedalus looks up the DRep id the wallet reports as the current vote.
  local current listed
  current=$(wallet_json "$W1" | jq -r '.delegation
    | ((.next // []) | map(select(.voting != null)) | last | .voting) // .active.voting')
  api G-wallet GET "/v2/wallets/$W1"
  fixture wallet-retired-drep
  api G-lookup GET "/v2/dreps/$current"
  fixture drep-lookup-retired
  check G-lookup "Registry lookup of the wallet's retired DRep" "GET /v2/dreps/{A}" \
    "HTTP 404 not_found" "$(observed)" '[ "$R_STATUS" = 404 ]'
  api G-list GET /v2/dreps
  fixture drep-list
  listed=$(jq -r --arg a "$DREP_A_CIP129" --arg b "$DREP_B_CIP129" 'if type == "array"
    then "\(length) DReps, A listed: \(any(.[]; .id == $a)), B listed: \(any(.[]; .id == $b))"
    else "not a list" end' "$R_FILE")
  check G-list "DRep list" "GET /v2/dreps" "HTTP 200, lists B and not A" "HTTP $R_STATUS, $listed" \
    '[ "$R_STATUS" = 200 ] && [[ "$listed" == *"A listed: false, B listed: true" ]]'
  api G-summary GET /v2/dreps/summary
  fixture drep-summary

  # The list, timed while lookups of the missing DRep are outstanding, with
  # the registry at a realistic size.
  register_extra_dreps "$EXTRA_DREPS"
  wallet_restart "$W1" "$W2" "$W3" "$W4"
  : >"$OUT/drep-latency.tsv"
  PROBE_T0=$(date +%s.%N)
  local i size
  size=$(curl -s "$API/v2/dreps" | jq 'length')
  for i in 1 2 3 4 5; do
    probe list-idle "$i" /v2/dreps
    wait_probes
  done
  for i in $(seq 1 "$LOOKUP_BURST"); do probe lookup-burst "$i" "/v2/dreps/$current"; done
  sleep 0.2
  for i in 1 2 3; do probe list-burst "$i" /v2/dreps; done
  wait_probes
  for i in $(seq 0 5 $((SUSTAIN_SECONDS - 5))); do
    probe lookup-sustained "$i" "/v2/dreps/$current"
    [ $((i % 10)) != 0 ] || probe list-sustained "$i" /v2/dreps
    sleep 5
  done
  wait_probes
  result G-latency-idle "DRep list latency, no lookups outstanding" "GET /v2/dreps" \
    "(measurement)" "$size DReps: list $(stats list-idle)" NOTE
  result G-latency-burst "DRep list latency, $LOOKUP_BURST lookups of the retired DRep sent at once" \
    "GET /v2/dreps during GET /v2/dreps/{A}" "(measurement)" \
    "list $(stats list-burst); lookups $(stats lookup-burst), $(statuses lookup-burst)" NOTE
  result G-latency-sustained "DRep list latency, one lookup of the retired DRep every 5 s for $SUSTAIN_SECONDS s" \
    "GET /v2/dreps every 10 s" "(measurement)" \
    "list $(stats list-sustained); lookups $(stats lookup-sustained), $(statuses lookup-sustained)" NOTE
}

# Re-delegation, starting from a refused undelegation ------------------------------------

redelegate() {
  local before seen
  require_no_vote F-quit "$W1" || return 0
  # The refusal is expected only at a cached reward balance above zero: at
  # zero the undelegation builds no withdrawal and is accepted. The wallet
  # reads the balance together with the vote delegation, so a cached balance
  # also means the refusal is decided from the ledger, not the history.
  if wait_withdrawable "$W1"; then
    before=$(available_of "$W1")
    api F-quit DELETE "/v2/stake-pools/*/wallets/$W1" "$(jq -n --arg p "$PASS" '{passphrase: $p}')"
    fixture quit-refused
    check F-quit "Undelegation (always withdraws)" "DELETE /v2/stake-pools/*, mainnet build" \
      "$GATE" "$(observed)" is_refused_before_submission
    check F-quit-funds "Funds available after the refused undelegation" "GET /v2/wallets/{id}" \
      "available unchanged, no pending transaction" "$(funds_seen "$before" "$W1")" \
      'funds_unchanged "$before" "$W1"'
  else
    result F-quit "Undelegation (always withdraws)" "DELETE /v2/stake-pools/*, mainnet build" \
      "reward > $MIN_REWARD" "reward $(reward_of "$W1")" PRECONDITION
  fi

  vote H-held "$W1" "$DREP_B_CIP129" "" vote-construct
  check H-held "Vote delegation right after the refused undelegation" "vote, $(build_of "$BACKEND")" \
    "accepted" "$VOTE_OBSERVED" '[ "$VOTE_ACCEPTED" = yes ]'
  if [ -n "$HELD_SUBMIT_OBSERVED" ]; then
    result H-held-submit "Vote submission while the refused undelegation's funds are held" \
      "PUT /v2/dreps/{B}/wallets/{id}, then GET /v2/wallets/{id}, mainnet build" \
      "(measurement)" "$HELD_SUBMIT_OBSERVED" NOTE
  fi
  if [ "$VOTE_ACCEPTED" != yes ]; then
    release_pending "$W1" H-held
    vote H-vote "$W1" "$DREP_B_CIP129" "" vote-construct
    check H-vote "Vote delegation once no transaction is pending" "vote, $(build_of "$BACKEND")" \
      "accepted" "$VOTE_OBSERVED" '[ "$VOTE_ACCEPTED" = yes ]'
  fi
  seen=$(ledger_vote "$W1")
  check H-ledger "Ledger vote delegation after re-delegation" "cardano-cli query stake-address-info" \
    "drep:B" "$seen" '[ "$seen" = drep:B ]'
  local _
  for _ in 1 2 3 4 5; do
    seen=$(wallet_ledger_field "$W1")
    [ "$seen" != drep:B ] || break
    sleep 2
  done
  check H-field "Ledger vote delegation reported by the wallet" \
    "GET /v2/wallets/{id} .$LEDGER_VOTE_FIELD" "drep:B" "$seen" '[ "$seen" = drep:B ]'
  api H-wallet GET "/v2/wallets/$W1"
  fixture wallet-drep
  if wait_withdrawable "$W1"; then
    pay H-pay "$W1" "$BACKEND" payment-fee-delegated
    check H-pay "Payment after re-delegation" "payment, $(build_of "$BACKEND")" "accepted, rewards withdrawn" \
      "$PAY_OBSERVED" '[ "$PAY_ACCEPTED" = yes ] && [ "$PAY_WITHDRAWAL" -gt 0 ]'
  else
    result H-pay "Payment after re-delegation" "payment, $(build_of "$BACKEND")" "accepted, rewards withdrawn" \
      "reward $(reward_of "$W1")" PRECONDITION
  fi
}

# ITN redemption through a restored Shelley wallet ----------------------------------------

itn_restore() {
  require_no_vote I-redeem "$W2" || return 0
  if ! wait_withdrawable "$W2"; then
    result I-redeem "Redemption with the recovery phrase of an account with no vote delegation" "-" "-" \
      "reward $(reward_of "$W2")" PRECONDITION
    return 0
  fi
  local words stake keyhash selected refused_hash payments seen
  words=$(jq -R 'split(" ")' <"$STATE/w2.mnemonic")
  stake=$(stake_address_of "$W2")
  keyhash=$("$ADDRESS" address inspect <<<"$stake" | jq -r '.stake_key_hash // empty')
  payments=$(jq -n --arg a "$W3_ADDR" '[{address: $a, amount: {quantity: 1000000, unit: "lovelace"}}]')

  api I-selection POST "/v2/wallets/$W3/coin-selections/random" \
    "$(jq -n --argjson p "$payments" --argjson w "$words" '{payments: $p, withdrawal: $w}')"
  selected=$(jq -r '[.withdrawals[]?.stake_address] | join(",")' "$R_FILE" 2>/dev/null || true)
  api I-fee POST "/v2/wallets/$W3/payment-fees" \
    "$(jq -n --argjson p "$payments" --argjson w "$words" '{payments: $p, withdrawal: $w}')"
  api I-redeem POST "/v2/wallets/$W3/transactions" \
    "$(jq -n --argjson p "$payments" --argjson w "$words" --arg pw "$PASS" '{payments: $p, passphrase: $pw, withdrawal: $w}')"
  check I-redeem "Redemption with the recovery phrase of an account with no vote delegation" \
    "payment-fees and transactions with withdrawal: <phrase>, both builds" \
    "refused" "$(observed)" '! is_accepted'
  refused_hash=$(jq -r '.message // ""' "$R_FILE" | grep -oE '[0-9a-f]{56}' | head -1 || true)
  seen="selection withdraws from ${selected:-nothing}; refusal names key hash ${refused_hash:-none}; W2 is $stake ($keyhash)"
  check I-account "The phrase's reward account is the restored wallet's" \
    "coin-selections/random with withdrawal: <phrase>" "W2's reward account" "$seen" \
    '{ [ "$selected" = "$stake" ] || { [ -z "$selected" ] && [ -n "$refused_hash" ]; }; } && { [ -z "$refused_hash" ] || [ "$refused_hash" = "$keyhash" ]; }'
  release_pending "$W3" I-redeem

  vote I-vote "$W2" abstain
  check I-vote "Restored wallet delegates its voting power to Abstain" "vote, $(build_of "$BACKEND")" \
    "accepted" "$VOTE_OBSERVED" '[ "$VOTE_ACCEPTED" = yes ]'
  wait_ledger_field "$W2" abstain || true
  if wait_withdrawable "$W2"; then
    pay I-pay "$W2"
    check I-pay "Restored wallet withdraws once vote-delegated" "payment, $(build_of "$BACKEND")" \
      "accepted, rewards withdrawn" "$PAY_OBSERVED" \
      '[ "$PAY_ACCEPTED" = yes ] && [ "$PAY_WITHDRAWAL" -gt 0 ]'
  else
    result I-pay "Restored wallet withdraws once vote-delegated" "payment, $(build_of "$BACKEND")" \
      "accepted, rewards withdrawn" "reward $(reward_of "$W2")" PRECONDITION
  fi

  # The withdrawal-only transaction on the restored wallet after its vote delegation.
  if wait_withdrawable "$W2"; then
    reward=$(reward_of "$W2")
    before=$(available_of "$W2")
    construct_flow I-withdraw "$W2" '{"encoding": "base16", "withdrawal": "self"}'
    fee=$(jq -r '.fee.quantity // 0' "$CF_CONSTRUCT_FILE")
    after=$(available_of "$W2")
    expected=$((before + CF_WITHDRAWAL - fee))
    check I-withdraw "Restored wallet withdraws its rewards alone once vote-delegated" \
      "construct {withdrawal: self}, sign, submit" \
      "construct 202 with one withdrawal equal to the reward and no payment outputs, submit 202, available up by the withdrawal less the fee" \
      "$(construct_observed), withdrawal $CF_WITHDRAWAL of reward $reward, outputs $(jq -r '.coin_selection.outputs | length' "$CF_CONSTRUCT_FILE" 2>/dev/null), withdrawals $(jq -r '.coin_selection.withdrawals | length' "$CF_CONSTRUCT_FILE" 2>/dev/null), available $before to $after, fee $fee" \
      '[ "$CF_CONSTRUCT_STATUS" = 202 ] && [ "$CF_SUBMIT_STATUS" = 202 ] && [ "$CF_WITHDRAWAL" = "$reward" ] &&
       [ "$(jq -r ".coin_selection.outputs | length" "$CF_CONSTRUCT_FILE")" = 0 ] &&
       [ "$(jq -r ".coin_selection.withdrawals | length" "$CF_CONSTRUCT_FILE")" = 1 ] && [ "$after" = "$expected" ]'
  else
    result I-withdraw "Restored wallet withdraws its rewards alone once vote-delegated" "construct {withdrawal: self}, sign, submit" \
      "accepted, rewards withdrawn" "reward $(reward_of "$W2")" PRECONDITION
  fi
}

# Undelegation from an account with no vote delegation -------------------------------------

undelegation_two_step() {
  retire B "$W1" || return 0
  local reward seen withdrawal quit_certificate
  vote T-vote "$W1" abstain vote-abstain
  check T-vote "Undelegation step 1: vote delegation to Abstain" "vote, $(build_of "$BACKEND")" "accepted" \
    "$VOTE_OBSERVED" '[ "$VOTE_ACCEPTED" = yes ]'
  wait_ledger_field "$W1" abstain || true
  api T-wallet GET "/v2/wallets/$W1"
  fixture wallet-abstain
  if ! wait_withdrawable "$W1"; then
    result T-quit "Undelegation step 2: undelegation with the withdrawal" "undelegation, $(build_of "$BACKEND")" \
      "accepted" "reward $(reward_of "$W1")" PRECONDITION
    return 0
  fi
  reward=$(reward_of "$W1")
  api T-hw-quit POST "/v2/wallets/$W1/coin-selections/random" \
    "$(jq -n --arg p "$(current_pool "$W1")" '{delegation_action: {action: "quit", pool: $p}}')"
  fixture hardware-quit-selection
  seen=$(observed)
  withdrawal=0 quit_certificate=no
  if is_accepted; then
    withdrawal=$(jq -r '[.withdrawals[]?.amount.quantity] | add // 0' "$R_FILE")
    jq -e 'any(.certificates[]?; (.certificate_type // "") | test("quit"))' "$R_FILE" >/dev/null &&
      quit_certificate=yes
    seen="$seen, selection withdraws $withdrawal, quit certificate: $quit_certificate"
  fi
  check T-hw-quit "Hardware-wallet undelegation selection once the account is vote-delegated" \
    "coin-selections/random {delegation_action: quit}, mainnet build" \
    "HTTP 200, withdraws the whole balance ($reward), carries the quit certificate" "$seen" \
    'is_accepted && [ "$withdrawal" = "$reward" ] && [ "$quit_certificate" = yes ]'
  undelegate T-quit "$W1"
  seen=$(ledger_vote "$W1")
  check T-quit "Undelegation step 2: undelegation with the withdrawal" "undelegation, $(build_of "$BACKEND")" \
    "accepted, withdraws the whole balance ($reward), stake key deregistered" \
    "$QUIT_OBSERVED, withdrew $QUIT_WITHDRAWAL, ledger: $seen" \
    '[ "$QUIT_ACCEPTED" = yes ] && [ "$QUIT_WITHDRAWAL" = "$reward" ] && [ "$seen" = unregistered ]'
}

# The vote certificate in a constructed transaction that also withdraws ---------------------

dropped_certificate() {
  if ! wait_withdrawable "$W2"; then
    result D-cert "Vote delegation combined with the withdrawal, account already delegated" "construct" "-" \
      "reward $(reward_of "$W2")" PRECONDITION
    return 0
  fi
  local seen
  construct_flow D-cert "$W2" '{"encoding": "base16", "vote": "no_confidence", "withdrawal": "self"}'
  seen=$(ledger_vote "$W2")
  check D-cert "Vote delegation combined with the withdrawal, account already delegated" \
    "construct {vote, withdrawal: self}" \
    "the signed CBOR carries the vote certificate; ledger then shows no_confidence" \
    "$(construct_observed), CBOR certificates $CF_CERTS, withdrew $CF_WITHDRAWAL, ledger: $seen" \
    'cbor_has_vote_certificate && [ "$seen" = no_confidence ]'
}

# A DRep that registers again after retiring -----------------------------------------------

# hardware_proxy_abstain WID STAKE-KEY-DIR: the Abstain vote preparation and submission of a
# hardware wallet: transactions-construct {vote}, a body built from its coin
# selection outside the wallet with a vote delegation certificate for the
# wallet's stake key, signed by transactions-sign (standing in for the device)
# and posted to /v2/proxy/transactions on the mainnet build, or to the fork's
# transaction-submission on the cip30 builds, which sign and submit the
# construct's own CBOR. A body the wallet will not sign is a PRECONDITION, with
# the construct's own CBOR sent through the proxy instead. Sets HV_OBSERVED and
# HV_SUBMITTED (yes or no); HV_PRECONDITION is non-empty when the body built
# outside the wallet was not used.
hardware_proxy_abstain() {
  local id="$1" keys="$2" unsigned own signed cert="$STATE/hw-vote.cert" body="$STATE/hw-vote.body.cbor.hex"
  HV_SUBMITTED=no HV_PRECONDITION=''
  api P-hw-vote-construct POST "/v2/wallets/$id/transactions-construct" \
    '{"encoding": "base16", "vote": "abstain"}'
  HV_OBSERVED="construct $(observed)"
  is_accepted || return 0
  own=$(jq -r .transaction "$R_FILE")
  unsigned=$own
  jq .coin_selection "$R_FILE" >"$STATE/hw-vote.selection.json"
  if [ "$BACKEND" = upstream ]; then
    "$CLI" conway stake-address vote-delegation-certificate \
      --stake-verification-key-file "$keys/stake.vkey" --always-abstain --out-file "$cert"
    if vote_selection_body "$STATE/hw-vote.selection.json" "$cert" "$body"; then
      unsigned=$(cat "$body")
    else
      HV_PRECONDITION="the selection could not be built into a body ($(head -c 200 "$body.build.txt" 2>/dev/null))"
    fi
  fi
  api P-hw-vote-sign POST "/v2/wallets/$id/transactions-sign" \
    "$(jq -n --arg t "$unsigned" --arg p "$PASS" '{transaction: $t, passphrase: $p, encoding: "base16"}')"
  if ! is_accepted; then
    HV_OBSERVED="$HV_OBSERVED; transactions-sign refused the body: $(observed): $(jq -r '.message // ""' "$R_FILE" | head -c 300)"
    HV_PRECONDITION=${HV_PRECONDITION:-transactions-sign refused the body built outside the wallet}
    api P-hw-vote-sign-construct POST "/v2/wallets/$id/transactions-sign" \
      "$(jq -n --arg t "$own" --arg p "$PASS" '{transaction: $t, passphrase: $p, encoding: "base16"}')"
    is_accepted || return 0
  fi
  signed=$(jq -r .transaction "$R_FILE")
  if [ "$BACKEND" = upstream ]; then
    echo "$signed" | xxd -r -p >"$STATE/hw-vote.signed.cbor"
    api_octets P-hw-vote /v2/proxy/transactions "$STATE/hw-vote.signed.cbor"
  else
    echo "$signed" >"$STATE/hw-vote.signed.cbor.hex"
    dapp_submit P-hw-vote "$id" "$STATE/hw-vote.signed.cbor.hex" || {
      HV_OBSERVED="$HV_OBSERVED; $DAPP_SUBMIT_WHY"
      return 0
    }
  fi
  HV_OBSERVED="$HV_OBSERVED; submission $(observed)"
  if is_accepted; then HV_SUBMITTED=yes; fi
}

reregistered_drep() {
  local seen
  # DRep A registers again; the wallet's DRep registry is refreshed by a
  # restart.
  cli_tx reregister-drepA 1 --certificate-file "$STATE/drepA/reg.cert" \
    --witness-override 2 -- "$STATE/drepA/drep.skey" ||
    {
      result RR-register "DRep A registers again" "cardano-cli transaction" "accepted" "$CLI_RESULT" PRECONDITION
      return 0
    }
  result RR-register "DRep A registers again" "cardano-cli transaction" "(setup)" "$CLI_RESULT" NOTE
  sleep 3
  snapshot "DRep A registered again" "$W4"

  seen=$(ledger_vote "$W4")
  check RR-ledger "Ledger vote delegation of an account whose DRep registered again" \
    "cardano-cli query stake-address-info" "none" "$seen" '[ "$seen" = none ]'
  seen=$(wallet_history_vote "$W4")
  result RR-history "Vote the wallet reports from its own history" \
    "GET /v2/wallets/{id} .delegation.*.voting" "(history, no expectation)" "$seen" NOTE
  wallet_restart "$W1" "$W2" "$W3" "$W4"
  # The wallet reads the field after a restart, not at once.
  local _
  for _ in $(seq 1 15); do
    seen=$(wallet_ledger_field "$W4")
    [ "$seen" != absent ] && break
    sleep 2
  done
  wait_ledger_field "$W4" none || true
  seen=$(wallet_ledger_field "$W4")
  check RR-field "Ledger vote delegation reported by the wallet" \
    "GET /v2/wallets/{id} .$LEDGER_VOTE_FIELD" "none" "$seen" '[ "$seen" = none ]'
  api RR-wallet GET "/v2/wallets/$W4"
  fixture wallet-reregistered-drep
  api RR-lookup GET "/v2/dreps/$DREP_A_CIP129"
  fixture drep-registered-again
  check RR-lookup "Registry lookup of a DRep that registered again" "GET /v2/dreps/{A}" \
    "HTTP 200" "$(observed)" '[ "$R_STATUS" = 200 ]'

  if wait_withdrawable "$W4"; then
    pay RR-wdrl "$W4"
    if [ "$BACKEND" = upstream ]; then
      check RR-wdrl "Payment requesting the withdrawal while the account has no vote delegation" \
        "payment, $(build_of "$BACKEND")" "$GATE" "$PAY_OBSERVED" is_refused_before_submission
    else
      check RR-wdrl "Payment requesting the withdrawal while the account has no vote delegation" \
        "payment, $(build_of "$BACKEND")" "$GATE (at construct)" "$PAY_OBSERVED" construct_refused_before_submission
    fi
    release_pending "$W4" RR-wdrl
  else
    result RR-wdrl "Payment requesting the withdrawal while the account has no vote delegation" \
      "payment, $(build_of "$BACKEND")" "$GATE" "reward $(reward_of "$W4")" PRECONDITION
  fi

  vote RR-vote-back "$W4" "$DREP_A_CIP129" "" vote-construct-reregistered
  wait_ledger_vote "$W4" drep:A || true
  seen=$(ledger_vote "$W4")
  check RR-vote-back "Vote delegation back to the DRep that registered again" "vote, $(build_of "$BACKEND")" \
    "accepted, ledger reads A" "$VOTE_OBSERVED, ledger: $seen" \
    '[ "$VOTE_ACCEPTED" = yes ] && [ "$seen" = drep:A ]'

  hardware_proxy_abstain "$W4" "$STATE/w4keys"
  wait_ledger_vote "$W4" abstain || true
  seen=$(ledger_vote "$W4")
  if [ "$HV_SUBMITTED" = yes ] && [ "$seen" = abstain ] && [ -z "$HV_PRECONDITION" ]; then
    result P-hw-vote "Hardware-wallet vote delegation submitted through the hardware endpoints" \
      "construct {vote}, sign, $([ "$BACKEND" = upstream ] && echo 'POST /v2/proxy/transactions' || echo 'POST /v2/wallets/{id}/transaction-submission')" \
      "accepted, ledger reads Abstain" "$HV_OBSERVED, ledger: $seen" PASS
  elif [ -n "$HV_PRECONDITION" ]; then
    result P-hw-vote "Hardware-wallet vote delegation submitted through the hardware endpoints" \
      "construct {vote}, sign, $([ "$BACKEND" = upstream ] && echo 'POST /v2/proxy/transactions' || echo 'POST /v2/wallets/{id}/transaction-submission')" \
      "accepted, ledger reads Abstain" "$HV_PRECONDITION; $HV_OBSERVED, ledger: $seen" PRECONDITION
  else
    result P-hw-vote "Hardware-wallet vote delegation submitted through the hardware endpoints" \
      "construct {vote}, sign, $([ "$BACKEND" = upstream ] && echo 'POST /v2/proxy/transactions' || echo 'POST /v2/wallets/{id}/transaction-submission')" \
      "accepted, ledger reads Abstain" "$HV_OBSERVED, ledger: $seen" FAIL
  fi

  if wait_withdrawable "$W4"; then
    pay RR-pay "$W4"
    check RR-pay "Payment with the withdrawal once the account is vote-delegated again" \
      "payment, $(build_of "$BACKEND")" "accepted, rewards withdrawn" "$PAY_OBSERVED" \
      '[ "$PAY_ACCEPTED" = yes ] && [ "$PAY_WITHDRAWAL" -gt 0 ]'
  else
    result RR-pay "Payment with the withdrawal once the account is vote-delegated again" \
      "payment, $(build_of "$BACKEND")" "accepted, rewards withdrawn" "reward $(reward_of "$W4")" PRECONDITION
  fi

  # A vote on a wallet whose stake key is not registered: the construct adds
  # the registration and its deposit. Recorded, not submitted.
  api RR-registering-key POST "/v2/wallets/$W3/transactions-construct" \
    "$(jq -n '{encoding: "base16", vote: "abstain"}')"
  fixture vote-construct-registering-key
  result RR-registering-key "Vote construct for a wallet whose stake key is not registered" \
    "transactions-construct {vote: abstain}" "(measurement)" \
    "$(observed), certificates [$(jq -r '[.coin_selection.certificates[]?.certificate_type] | join(",")' "$R_FILE")], deposits taken $(jq -c '[.coin_selection.deposits_taken[]?.quantity]' "$R_FILE")" NOTE
}

T0=$(date +%s)
"$WALLET" version >"$OUT/wallet-version.txt"
log "backend $BACKEND: $(cat "$OUT/wallet-version.txt")"
setup
control
retire A "$W1" || die "the ledger still reports a vote delegation for W1 after DRep A retired"
after_retirement
governance_registry
redelegate
itn_restore
undelegation_two_step
dropped_certificate
reregistered_drep
snapshot "end" "$W1" "$W2" "$W4"
echo $(($(date +%s) - T0)) >"$OUT/duration-seconds"
log "backend $BACKEND finished in $(cat "$OUT/duration-seconds") s"
