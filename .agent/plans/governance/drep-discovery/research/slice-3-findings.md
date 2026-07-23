# Slice-3 Findings — Hardware-Wallet Delegate

> Durable findings from slice-3 planning (2026-07-23). Implementation findings are
> appended at slice close under the final section.

---

## F-1 — Design §8 conflicts with the live repo: no `HwDeviceStatus` enum in `common`, no sub-state literals

`designs/shared-design-tokens.md` §8 (:137-151) prescribes a permanent HW caption plus
five governance copy variants (`governance.hw.disconnected`, `.locked`, `.appNotOpen`,
`.rejected`, `.timeout`) whose sub-state mapping is "driven by the canonical
`HwDeviceStatus` enum in `source/common/types/hardware-wallets.types.ts`".

Verified against the live repo (base `883ffae09`):

- **The referenced enum does not exist.** There is no `HwDeviceStatus` in
  `source/common/types/hardware-wallets.types.ts`. The canonical statuses are
  `HwDeviceStatuses` in `source/renderer/app/domains/Wallet.ts:65-105`.
- **The §8 sub-state literals do not exist as statuses.** `disconnected`,
  `device_locked`, `cardano_app_not_open`, `verification_rejected`, and `timeout`
  appear nowhere in the real status set. The real literals are: `connecting`,
  `connecting_failed`, `trezor_bridge_failure`, `launching_cardano_app`,
  `exporting_public_key(_failed)`, `wrong_firmware`, `wrong_cardano_app_version`,
  `unsupported_device`, `ready`, `verifying_transaction(_failed|_succeeded)`,
  `verifying_address*`, `unrecognized_wallet`.

**Resolution (live repo wins, per prompt.md:39-41).** task-115's AC states map onto the
real statuses; the existing `HardwareWalletStatus` component already carries per-status
copy (`wallet.hardware.deviceStatus.*` in en-US/ja-JP):

| task-115 AC state | Real status(es) exercised | Existing en-US copy (verified) |
|---|---|---|
| device-disconnected | `CONNECTING`, `CONNECTING_FAILED` | "Connect the \"{walletName}\" device and enter your PIN to unlock it" / "Disconnect and reconnect your hardware wallet to restart the process." |
| device-locked | `CONNECTING` (the PIN-unlock copy covers the locked device) | same `connecting.known` copy |
| app-not-open | `LAUNCHING_CARDANO_APP` | "Launch Cardano application on your device" |
| signing-rejected | `VERIFYING_TRANSACTION_FAILED` | "Transaction confirmation failed" |
| Trezor wrong-passphrase/invalid-state | `UNRECOGNIZED_WALLET` (set on Trezor `Device_InvalidState`, `HardwareWalletsStore.ts:2409-2415`) | "We do not recognize this wallet on your device. …" |

The §8 caption + `governance.hw.*` copy set is **deferred**: task-115's acceptance
criteria are test-focused, slice-3 plans no new copy (so invariant #11 is untouched),
and the generic per-status copy already renders via `HardwareWalletStatus` inside the
confirmation dialog. §7's three-representation dialog layout also stays deferred per
slice-2 decision D3. If a later slice implements §8, the design doc's enum reference and
sub-state table must be reconciled to `HwDeviceStatuses` first.

---

## F-2 — The renderer logger applies no sanitization; the `VotingStore` HW catch blocks are a proven leak surface

`source/renderer/app/utils/logging.ts:26-38` forwards `(message, data)` straight to
`electronLog[level]`; `filterLogData` is applied only where call sites invoke it
explicitly (AdaApi methods). Neither the main-process transports
(`source/main/utils/setupLogging.ts`) nor any hook re-sanitizes the payload.

Consequently, before slice-3:

- `VotingStore.ts:347-358` logged `{ error }` on `initializeVPDelegationTx` failure, and
- `VotingStore.ts:403-412` logged `{ error }` on the `delegateVotes` HW failure path.

Error objects on these paths are uncontrolled text (device transport errors,
cardano-wallet API errors that can echo request context). An error message embedding the
selected DRep ID would be written to the on-disk log — violating the inherited
sanitization floor. Key-based `filterLogData` could not help even if applied: it redacts
by key name and cannot scrub substrings inside `error.message`.

**Fix shipped in slice-3:** both catch blocks log only the derived error code
(`{ errorCode }`) — the same value each method already returns. The floor suite proves
it with adversarial errors embedding a DRep ID and both sentinel literals.

**Residual risk (out of slice scope):** `HardwareWalletsStore` logs raw `{ error }` in
many `[HW-DEBUG]` calls (e.g. `:2652`). Transport-level errors there do not embed
certificate contents in practice, and slice-3's ACs cover the `VotingStore` flow; a
broader sweep would be its own hardening task if ever warranted.

---

## F-3 — `@trezor/connect` cannot load under Jest 27; a one-line stub keeps the real `PROTO` enums

Importing `source/renderer/app/utils/shelleyTrezor.ts` in a Jest 27.5.1 spec fails:
`@trezor/connect` → `@trezor/device-authenticity` → `@noble/curves` ships ESM-only
(`import { sha512 } from '@noble/hashes/sha2.js'` → `SyntaxError: Cannot use import
statement outside a module`), and the repo's `transformIgnorePatterns` only transforms
`react-polymorph` inside `node_modules`.

Smoke-tested during planning with the repo's exact Jest config: placing

```ts
jest.mock('@trezor/device-authenticity', () => ({}));
```

as the first statement of the spec lets the whole chain load, with **real**
`PROTO.CardanoDRepType` / `PROTO.CardanoCertificateType` enum values
(`VOTE_DELEGATION === 9`, `KEY_HASH 0 / SCRIPT_HASH 1 / ABSTAIN 2 / NO_CONFIDENCE 3`).
The authenticity module is unrelated to certificate mapping. `shelleyLedger.ts`
(`@cardano-foundation/ledgerjs-hw-app-cardano`, `DRepParamsType`: `KEY_PATH 0 /
KEY_HASH 1 / SCRIPT_HASH 2 / ABSTAIN 3 / NO_CONFIDENCE 4`) loads without any stub.

Also relevant: Jest 27 has no `jest.advanceTimersByTimeAsync`; async code around fake
timers needs explicit microtask flushes (`await Promise.resolve()`) between
`jest.advanceTimersByTime` steps.

---

## F-4 — Verified DRep ID test vectors (round-trip proven via `Cardano.DRepID`)

Generated from two fixed 28-byte credential hashes with
`Cardano.DRepID.cip129FromCredential` / `cip105FromCredential`
(`@cardano-sdk/core`, the exact library the mappers call), and round-trip verified with
`Cardano.DRepID.toCredential` (type + hash both match):

```
key hash    a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c
script hash 0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4

CIP-129 key    drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy
CIP-129 script drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt
CIP-105 key    drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9
CIP-105 script drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc
```

`Cardano.DRepID.toCredential` accepts both encodings: CIP-129 (29-byte payload, header
`0x22` key / `0x23` script) and CIP-105 (28-byte payload, script-ness from the
`drep_script` HRP). These vectors let the mapper specs assert AC-5 byte-equality
precisely: the device-bound hash equals the credential decoded from `vote.chosenOption`.
(The on-device identity is the credential hash the device derives/displays from these
bytes; the bech32 string itself never crosses to the device — §7's equality rule is
about the underlying credential bytes.)

---

## F-5 — Vote-kind analytics precedent (sentinel literals)

Invariant #2's literal text bans `abstain`/`no_confidence` in analytics payloads, while
the task-110 decision (plan :98, risk :336) deliberately reduced the
`Casted governance vote` payload to the vote-kind only — and
`VotingStore._getVoteKind` returns `'drep' | 'abstain' | 'no_confidence'`. Slice-1's
floor suite pinned the `drep` case only. Slice-3 follows that precedent unchanged: the
new HW analytics test also exercises the `drep` case and asserts the 3-arg vote-kind
payload. The sentinel-kind tension is a pre-existing, deliberate scope choice
(vote-kind reveals no DRep identity), not a slice-3 regression; noted here so no later
slice "fixes" it accidentally in either direction.

---

## F-6 — `initializeVPDelegationTx`'s catch also covers software-path failures

The `try` at `VotingStore.ts:301` wraps `selectDelegationCoins` for **both** wallet
types, so software-wallet coin-selection failures (`same_vote`, `no_utxos_available`,
`not_enough_money`) also flow through the catch whose log message says "… with HW". The
slice-3 fix sanitizes the payload but keeps the message string unchanged (log-tooling
stability; smallest truthful change). The misleading wording is cosmetic; rename only if
a future task touches this seam for its own reasons.

---

## Implementation findings (appended at slice close)

### I-1 — `JSON.stringify` cannot see `Error` internals; floor tests need a replacer to be discriminating

The guide's two new floor tests asserted leak-freedom via
`JSON.stringify(errorSpy.mock.calls)`, but `Error` `message`/`stack` are
non-enumerable, so `JSON.stringify(new Error(…))` yields `{}` — verified empirically:
the tests as written **passed against the unfixed catch blocks**, contradicting their
stated fails-pre-fix intent. Fix: a `jsonStrWithErrors` helper (a `JSON.stringify`
replacer expanding any `Error` to `message` + `stack`) used in both tests.
Discriminance then proven by mutation: with `VotingStore.ts` reverted to HEAD the two
tests fail; with the fix all 20 pass. Any future floor test that inspects logger spy
calls for `Error` contents must use this helper (it lives in the floor suite).

### I-2 — `VotingStore.spec.ts` already existed at HEAD; the guide's CREATE was wrong

`source/renderer/app/stores/VotingStore.spec.ts` is tracked at HEAD (DDW-809 Catalyst
fund-phase suite, 9 `test.each` cases). Live code wins: the slice-3 HW describe block
was appended and imports merged instead of overwriting. The suite therefore reports
15 tests (9 pre-existing + 6 new); reviewers comparing against the guide's per-suite
table should expect 49 focused tests total, not 40.

### I-3 — `jest.mock('@trezor/device-authenticity')` goes below the imports, not first

F-3's "first statement" placement trips ESLint `import/first` (4 errors). Jest hoists
`jest.mock` above imports regardless, so placing it after the import block is
semantically identical; repo precedent (`VotingGovernancePage.spec.tsx`) uses the same
ordering. The spec's comment notes the hoisting.

### I-4 — prettier 2.1.2 reformats pre-existing HEAD drift in `VotingStore.ts`

The mandated pre-commit `prettier --write` (repo pins 2.1.2) reformatted ~8 hunks of
untouched regions of `VotingStore.ts` beyond the three fenced edits — the file at HEAD
carries formatting from a newer prettier (known repo-wide drift, see the slice-1
oscillation note). Pure formatting, no behavior change; kept so `prettier --check`
passes. Expect the same on any future edit to drift-carrying files.

### I-5 — the floor suite is outside the eslint gate

`tests/jest/security/governance-sanitization.spec.ts` is excluded by a pre-existing
eslint ignore pattern, so the lint gate exercises only 6 of the 7 files touched this
slice. Conventions there are held by review, not tooling.

### Leak outcome (D-2 / F-2 closure)

The HW logger leak is **fixed and proven**: both catch blocks now log `{ errorCode }`
only, and the floor suite's adversarial errors (embedding a CIP-129 DRep ID and both
sentinel literals in `message`) demonstrate pre-fix failure / post-fix cleanliness via
I-1's helper. HW analytics remain vote-kind-only (F-5 precedent unchanged). The
`HardwareWalletsStore` `[HW-DEBUG] { error }` surfaces stay a noted residual risk
(out of slice scope, F-2).
