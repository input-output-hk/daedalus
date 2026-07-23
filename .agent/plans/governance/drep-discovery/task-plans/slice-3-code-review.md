# Slice-3 Code Review Log

> Append-only transcript: `Planner:` entries (planning open/close), one `Critiquer:`
> entry (required review pass over the PRD + guide), and per-task `Code Review:`
> entries. Companion docs: [slice-3-PRD.md](./slice-3-PRD.md) ·
> [slice-3-implementation-guide.md](./slice-3-implementation-guide.md)

---

## Planner: 2026-07-23 — slice-3 planning complete (status: in_review)

**Scope planned.** Single task, task-115 (`autonomous`; dependency task-113 is
`verified`). The HW production path already exists end-to-end in `VotingStore`
(certificate build + `updateTxSignRequest` + `initiateTransaction` during
`initializeVPDelegationTx`; `_sendMoney` + 2s polling + vote-kind analytics during
`delegateVotes`), so the slice is test-dominant with a deliberately tiny production
diff: remove the stale `// TODO: handle HW case` at `VotingStore.ts:370` and fix the
two HW catch blocks to log `{ errorCode }` instead of `{ error }`. Layered test
strategy: (a) pure-mapper byte-equality specs for `shelleyLedger`/`shelleyTrezor`
(7 + 7 tests, verified CIP-129/CIP-105 key- and script-hash vectors plus sentinels,
device credential === `Cardano.DRepID.toCredential(chosenOption)`); (b) a new
`VotingStore.spec.ts` (6 tests) with a mocked `stores.hardwareWallets` per the
sanitization-suite construction precedent, covering both HW branches incl. the AC-3
never-fallback assertions and Jest-27 fake-timer polling; (c) dialog spec growth
(4 → 12) across the real `HwDeviceStatuses` for Ledger and Trezor plus Confirm gating
at `VERIFYING_TRANSACTION_SUCCEEDED`; (d) flow spec growth (4 → 7) for the HW
browse → select → confirm-on-device → delegate path; (e) sanitization floor growth
17 → 20 with adversarial errors embedding a DRep ID and both sentinel literals on both
HW error paths, plus the HW vote-kind analytics assertion.

**Decisions (PRD D-1…D-4).**
- **D-1**: design §8's copy set is keyed to a `HwDeviceStatus` enum in
  `source/common/types/hardware-wallets.types.ts` that does not exist, with sub-state
  literals that exist nowhere; resolved live-repo-wins — AC states map onto the real
  `HwDeviceStatuses` (`Wallet.ts:65-105`), §8 caption/copy deferred, no new i18n this
  slice. Durable finding F-1.
- **D-2**: the HW error-log leak is **proven**, not hypothetical — the renderer logger
  applies no `filterLogData` (`utils/logging.ts:26-38`), so raw error objects reach
  electron-log; smallest truthful fix = log the derived error code both paths already
  compute. Finding F-2.
- **D-3**: "mocked Ledger and Trezor transports" = store boundary + pure mappers (the
  actual payload seams of `@cardano-foundation/ledgerjs-hw-app-cardano` /
  `@trezor/connect`); no repo precedent exists for real transport mocks and driving the
  real `HardwareWalletsStore` through IPC would be the heaviest possible harness.
  Real-device QA stays task-125.
- **D-4**: Jest-27 constraints resolved by smoke test during planning —
  `jest.mock('@trezor/device-authenticity', () => ({}))` makes the real `PROTO` enums
  importable (finding F-3); fake-timer choreography pinned exactly (no
  `advanceTimersByTimeAsync` in 27.5.1); DRep vectors pre-generated and round-trip
  verified via `Cardano.DRepID` (finding F-4).

**Anchors.** Every line anchor in the guide was re-verified against the live worktree
(base `883ffae09`) on 2026-07-23; the certificate mappers, both `VotingStore` branches,
the dialog gate, the container wiring, the existing spec harnesses, the Storybook HW
story, and the Jest config (`globals.environment`, `@swc/jest`,
`transformIgnorePatterns`) all match the guide's quotes. Expected device-state strings
were read from `en-US.json` (they differ from the `!!!defaultMessage` fallbacks — e.g.
`connecting.known` interpolates the wallet name; `verifying_transaction_failed` is
"Transaction confirmation failed").

**Risks flagged for critique/build.**
1. **Step-order dependency:** the three new floor-suite tests fail against unfixed
   code by design — Step 1 must land before Step 5; a naive reorder would misreport.
2. **Fake-timer fragility:** the polling test's microtask-flush count is a heuristic;
   the guide's choreography is designed to pass even if the first
   `advanceTimersByTime` fires before the timer arms. If it still flakes, adding one
   more `await Promise.resolve()` is the sanctioned adaptation.
3. **`toBeDisabled()` on react-polymorph buttons** relies on `pickDOMProps` forwarding
   `disabled` to the DOM node (verified in `ButtonSkin.js` during planning); fallback
   asserting the attribute directly is noted in the guide.
4. **TS looseness in the Trezor spec** (`as` result shapes): the guide permits `as any`
   fallback — runtime assertions carry the contract.

**What the critiquer should focus on.** (i) Whether the AC ↔ test mapping fully
discharges task-115's six acceptance criteria — especially whether AC-5's
"on-device DRep ID equals vote.chosenOption" is honestly satisfied by
credential-level equality (the bech32 string itself never crosses to the device — §7's
equality rule is about the underlying credential bytes) plus the verbatim-string
`toBe` at the store seam; (ii) whether the D-2 production fix is truly the smallest
truthful change and loses no diagnosability the repo actually relies on; (iii) hidden
manual checkpoints (planning claims none — real-device QA is task-125); (iv) whether
the guide is concise and small-model-implementable end-to-end (full listings, exact
seams, deterministic commands, expected counts); (v) the D-1 conflict resolution —
confirm deferring §8 copy does not silently drop an AC obligation (the ACs are
test-focused; no copy AC exists in task-115).

**Planning-time deviations from the doc set:** none beyond D-1 (recorded). No source or
test file was modified during planning; the feasibility smoke tests ran from a
throwaway config outside the repo tree and were removed.

Decision: n/a (Planner entry — the Critiquer pass follows)

---

## Critiquer: 2026-07-23 — review of slice-3 PRD + implementation guide + findings

**Scope reviewed.** `slice-3-PRD.md`, `slice-3-implementation-guide.md`,
`research/slice-3-findings.md`, cross-checked against the tracker task-115 block
(`governance-drep-discovery-plan-tasks.json` :418-433), the locked invariants, and the
live worktree.

**Verified during this pass.**
- **Anchor accuracy (well beyond the 6-anchor minimum, all exact):** `VotingStore.ts`
  :370 stale TODO, :347-358 and :403-412 catch blocks (raw `{ error }` payloads
  confirmed live), :309-341 HW branch, :166-168 `delegateVotesRequest`, :74-95
  `parseApiCode` (returns `'generic'` for a plain `Error`, as the planned assertions
  assume); `shelleyLedger.ts` :56 guard / :59-69 sentinels / :71 credential derivation /
  :97 `params.dRep`, `keyHashHex`/`scriptHashHex` field names as quoted;
  `shelleyTrezor.ts` :58-84 and :95-101, `keyHash`/`scriptHash` + `VOTE_DELEGATION` as
  quoted; dialog gate :141-147, identity block :151-172, `HardwareWalletStatus` render
  :179-185, software `Input` is `type="password"` (:198) so the planned negative
  assertion is meaningful; dialog spec harness :12/:18-22/:24-48/:50-93; flow spec
  :24/:48-52/:64-67/:89/:101-104/:114/:119-122/:210-211/:244 — every quoted
  replace-target matches byte-for-byte; floor suite :2 header, :17-21 imports, :26-27
  `CIP129_DREP`, :187-189 boundaries describe + `restoreAllMocks`, :256 insertion
  point; `jest.config.js` `clearMocks:17`, `globals.environment:63-67`, `roots:129`,
  `testMatch:156`; `HwDeviceStatus` type export exists (`Wallet.ts:44`) for the Step-7b
  `import type`.
- **Test vectors independently re-verified** (bech32-decoded in this pass): all four
  guide vectors decode exactly as claimed — CIP-129 key `0x22`+key-hash, CIP-129
  script `0x23`+script-hash, CIP-105 key/script bare 28-byte hashes under
  `drep`/`drep_script`, hashes byte-equal to the fixed `KEY_HASH_HEX`/`SCRIPT_HASH_HEX`.
- **Copy strings re-verified against `en-US.json`:** all six `it.each` expectations
  match the live values (`connecting.known` walletName interpolation included), every
  dynamic `messages[hwDeviceStatus]` key exists in `HardwareWalletStatus`, the Trezor
  hint condition (`isTrezor && VERIFYING_TRANSACTION` ∈ passphrase-related statuses)
  holds, and `HardwareWalletStatus` seeds state from props on mount, so fresh-mount
  specs render each status immediately (the 4000 ms delayed-transition path is never
  entered).
- **Semantics of planned store tests:** `initiateTransaction` at `VotingStore.ts:338`
  is not awaited — the guide correctly uses synchronous-throw mocks in both the
  VotingStore spec and the floor-suite addition, so the catch is actually reached; the
  fake-timer choreography is robust to either arming order; the mocked
  `sendMoneyRequest` object is shared by reference so the mid-test `isExecuting` flip
  is observed.
- **AC coverage:** AC-1 (flow HW test + certificate verbatim test), AC-2 (six real
  device states incl. the D-1 mapping, identity/no-password, gating, Trezor
  treatment), AC-3 (`executeSpy` + `api.ada.delegateVotes` never called; software-path
  negative on the init seams), AC-4 (flow Ledger + Trezor + both mapper suites, with
  the "transports = store boundary + pure mappers" reading honestly recorded in D-3),
  AC-5 (credential byte-equality for all four ID forms + sentinels + `toBe` at the
  store seam, honest scope note in F-4), AC-6 (floor 17 → 20, adversarial errors on
  both HW paths, HW vote-kind analytics, enabled by the Step-1 fix). No AC is
  unmapped; the D-2 production fix is required by AC-6, so it stays within smallest
  truthful change.
- **Invariants:** #2 strengthened (leak fixed + spied), #4 untouched flow + never-
  fallback proof, #10 asserted at three layers, #11 untouched (expected strings are
  live en-US values, correctly not the `!!!` fallbacks), #13 sentinel mapping pinned.
  Tracker block matches the PRD scope; Step-9 status rule (`complete`, never
  `verified`) is stated. No hidden manual checkpoints found: the guide resolves the
  known judgment calls in place (`as any` fallback for Trezor result shapes,
  `toBeDisabled` fallback, exact timer choreography, full file listings, exact
  commands with expected counts).

**Blockers.**
1. **Dialog test-count arithmetic is wrong and is baked into the verification gates.**
   Step 6b adds an `it.each` with **6** parameter rows (Jest reports 6 tests) plus
   **3** further `it`s — 9 new tests, so the dialog suite lands at 4 + 9 = **13**, not
   12. The incorrect "+8 / 12" appears in the guide's files-touched table (row 6), the
   Step 6 heading and closing note ("File count after this step: 4 + 8 = 12 tests"),
   the Step 8 expected counts ("dialog 12"), PRD FR-7 ("4 → 12 tests"), and the PRD
   Definition of Done ("dialog 12"). A small model treating Step 8's counts as gates
   will see 13 passed and either misreport, stall, or delete a test to "fix" the
   mismatch — and the tracker `statusReason` would then carry untruthful counts.
   Fix: correct every occurrence to +9 / 13 (PRD FR-7, PRD DoD, guide table, Step 6,
   Step 8). No test should be removed — all nine are pulling weight.

**Notes (non-blocking).**
- The sanctioned fake-timer adaptation (add one extra `await Promise.resolve()` if the
  polling test flakes) currently lives only in the Planner entry above; inlining it in
  Step 4's notes would keep the implementer out of this log. Same for the count fix:
  the "(4 → 12)" in the Planner entry above stays as-is (append-only log); the
  correction belongs in the PRD/guide only.
- PRD/F-1 cite `Wallet.ts:65-105` for `HwDeviceStatuses`; the block's closing brace
  sits at :106 in this worktree. Immaterial — no edit targets those lines.
- Commit subject "harden and verify the hardware-wallet delegate path": "verify" is
  fine per the task-114 precedent, but be careful the tracker never echoes it as a
  `verified` status claim.

Decision: requires_changes

---

## Planner: 2026-07-23 — fix pass after critique (status: approved)

**Blocker 1 (dialog test-count arithmetic) — resolved.** The `it.each` in Step 6b has
6 parameter rows (Jest reports 6 tests) plus 3 further `it`s, so the dialog suite lands
at 4 + 9 = **13** tests. Every "+8 / 12" occurrence was corrected to "+9 / 13": the
guide's files-touched table (row 6), the Step 6 heading, the Step 6 closing note (now
also stating the 6-rows + 3-`it`s arithmetic explicitly), the Step 8 expected-counts
comment ("dialog 13"), PRD FR-7 ("4 → 13 tests"), and the PRD Definition of Done
("dialog 13"). No test was removed — all nine remain, per the critique. The findings
doc carries no count claims (verified by grep), and the "(4 → 12)" in the opening
Planner entry above stays untouched per this log's append-only rule and the critiquer's
own note.

**Non-blocking note adopted.** The sanctioned fake-timer adaptation (one extra
`await Promise.resolve()` if the polling test flakes; never restructure the
choreography) is now inlined in Step 4's notes, so the implementer does not need this
log. The other notes require no doc change: the `Wallet.ts:65-105` vs `:106` anchor
drift is immaterial (no edit targets those lines), and the commit-subject caution is
already covered by Step 9's explicit `complete`-never-`verified` rule.

PRD planning status flipped to **approved**.

Decision: approved
