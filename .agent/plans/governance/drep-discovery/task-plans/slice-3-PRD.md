# Slice-3 PRD: Hardware-Wallet Delegate

> **Planning Status:** approved | **Date:** 2026-07-23 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `slice-3` — "Slice 3 - Hardware-wallet delegate"
> **Tasks:** task-115 (single task; dependency task-113 is `verified`)
> **Implementation guide:** [slice-3-implementation-guide.md](./slice-3-implementation-guide.md)

---

## Executive Summary

Slice-3 completes the hardware-wallet delegation path through the **existing**
`VotingStore` signing flow with DRep-ID-only confirmation. The HW production path already
exists end-to-end (`initializeVPDelegationTx` builds the `cast_vote` certificate and
initiates on-device signing; `delegateVotes` submits the signed blob via
`HardwareWalletsStore._sendMoney`), but it is entirely untested, carries a stale
`// TODO: handle HW case` comment above the already-implemented branch, and — planning
proved — **leaks unsanitized error objects** into the log on both HW catch paths. The
slice therefore ships: (a) a minimal production fix (sanitized error logging + TODO
removal), and (b) layered Jest coverage — pure Ledger/Trezor certificate-mapper
byte-equality specs, `VotingStore` HW-branch specs with a mocked
`stores.hardwareWallets`, container/dialog flow specs across the real
`HwDeviceStatuses`, and growth of the inherited sanitization-floor suite (17 → 20).

**Why now:** the locked slice order (prompt.md:147-148) makes slice-3 the next phase;
slice-2 is closed (task-112/113 `verified`, task-114 `complete`) and task-115's only
dependency (task-113) is `verified`.

---

## Problem Statement

Slice-2 verified the software-wallet delegate path end-to-end. Hardware-wallet users run
through a parallel two-stage flow — sign during `initializeVPDelegationTx` /
`initiateTransaction`, submit the already-signed blob during `delegateVotes` — that no
Jest exercises. Nothing pins that the DRep ID the user selected is the byte-identical
identity bound into the device-facing certificate (invariant #10, design §7's
release-blocking equality rule), nothing asserts the software signing request is never
invoked for HW wallets, and the two HW `logger.error(..., { error })` calls sit outside
`filterLogData` — an adversarial or API-echoed error message would write the vote target
to disk, violating the inherited sanitization floor (invariant #2).

---

## Per-Task Contract

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-115** — Complete the hardware-wallet delegate path (DRep-ID-only confirmation) | `autonomous` | Remove the stale TODO at `VotingStore.ts:370`; sanitize the two HW `logger.error` payloads (proven leak — see D-2); NEW pure-mapper specs for `shelleyLedger`/`shelleyTrezor` `cast_vote` → device `dRep` params (byte-equality, AC-5); NEW `VotingStore.spec.ts` covering both HW branches with a mocked `stores.hardwareWallets` (AC-1/3); extend the dialog spec with the real `HwDeviceStatuses` device states + HW gating (AC-2); extend the flow spec with the HW browse→select→confirm-on-device→delegate path for Ledger and Trezor (AC-1/2/4); grow the sanitization floor suite 17 → 20 (AC-6); strip task-ID comments from `VotingGovernancePage.spec.tsx` while touching it (slice-2 review rider) | No new i18n copy (D-1), no §8 caption/copy set (D-1), no §7 three-representation dialog (slice-2 D3 deferral stands), no Storybook changes (existing HW story covers the dialog states), no `HardwareWalletsStore` changes, no IPC/route/component changes, no real-device QA (task-125), no `verified` self-promotion | task-113 ✔ (`verified`) |

task-115 is not in the locked non-autonomous set (task-125, task-166 remainder,
task-158, release-end `!!!` review). Planning surfaced **no blocking decisions**; D-1–D-4
below resolve everything within the prep-approved strategy.

---

## Planning Decisions (binding, as applied)

- **D-1 — Design §8 conflict resolved in favor of the live repo (copy deferred).**
  `designs/shared-design-tokens.md` §8 (:137-151) keys its five governance HW copy
  variants (`governance.hw.disconnected` / `.locked` / `.appNotOpen` / `.rejected` /
  `.timeout`) to "the canonical `HwDeviceStatus` enum in
  `source/common/types/hardware-wallets.types.ts`". **That enum does not exist there**;
  the canonical statuses are `HwDeviceStatuses` in
  `source/renderer/app/domains/Wallet.ts:65-105`, and §8's sub-state literals
  (`disconnected`, `device_locked`, `cardano_app_not_open`, `verification_rejected`,
  `timeout`) exist nowhere as statuses. Per the sources-of-truth rule (prompt.md:39-41)
  the live repo wins: task-115's AC states map onto real statuses —
  device-disconnected and device-locked exercise `CONNECTING` / `CONNECTING_FAILED`,
  app-not-open exercises `LAUNCHING_CARDANO_APP`, signing-rejected exercises
  `VERIFYING_TRANSACTION_FAILED`, and `UNRECOGNIZED_WALLET` covers the Trezor
  `Device_InvalidState` seam. The §8 permanent caption + copy set is **deferred** (the
  ACs are test-focused; this slice plans no new copy), as is §7's three-representation
  layout (slice-2 decision D3). Recorded as a durable finding in
  [research/slice-3-findings.md](../research/slice-3-findings.md) (F-1).
- **D-2 — The HW error-log leak is real; the smallest truthful fix ships.** The renderer
  logger (`source/renderer/app/utils/logging.ts:26-38`) forwards `data` to electron-log
  **without** `filterLogData` — sanitization happens only at explicit AdaApi call sites.
  So `logger.error(..., { error })` at `VotingStore.ts:347-358` and `:403-412` writes
  raw error objects whose messages can embed the vote target (device/API error text is
  uncontrolled). Fix: log only the derived error code (`{ errorCode }`) on both paths —
  the code the method already computes for its return value. No new abstraction, no
  logging-pipeline change. The floor suite's new adversarial tests (errors embedding a
  DRep ID and both sentinel literals) prove the fix.
- **D-3 — "Mocked Ledger and Trezor transports" = store boundary + pure mappers.** No
  spec in the repo mocks a real device transport today, and driving the real
  `HardwareWalletsStore` through IPC has no precedent and the heaviest surface. The
  device contract is instead pinned at its two real seams: the pure certificate mappers
  (`shelleyLedger.parseVoteDelegation`/`toLedgerCertificate`,
  `shelleyTrezor` `cast_vote` → `PROTO.CardanoCertificateType.VOTE_DELEGATION`) which
  produce the exact payloads handed to `@cardano-foundation/ledgerjs-hw-app-cardano` /
  `@trezor/connect`, and a mocked `stores.hardwareWallets` for the `VotingStore`
  branches (construction precedent: the existing sanitization suite builds
  `new VotingStore(api, {} as any, analytics)` and `Store.configure(stores)` attaches
  the mock). Real-device on-device QA remains task-125 (`manual_execution`).
- **D-4 — Jest 27 constraints pinned during planning.** `@trezor/connect` cannot load
  under Jest 27 (its `@trezor/device-authenticity` dependency pulls an ESM-only
  `@noble/curves` build); a one-line `jest.mock('@trezor/device-authenticity', () => ({}))`
  stub keeps the **real** `PROTO` enums importable — smoke-tested during planning with
  the repo's exact Jest config. `jest.advanceTimersByTimeAsync` does not exist in Jest
  27.5.1; the `delegateVotes` polling-loop tests use `jest.useFakeTimers()` +
  microtask flushes + `jest.advanceTimersByTime(2000)`. DRep test vectors (CIP-129 and
  CIP-105, key- and script-hash) were generated from fixed credential hashes with
  `Cardano.DRepID.cip129FromCredential`/`cip105FromCredential` and round-trip-verified
  via `Cardano.DRepID.toCredential` (F-4 in findings).

---

## User Stories

### US-3.1 — Trustworthy on-device confirmation
**As a** hardware-wallet user delegating voting power,
**I want** the DRep credential my device displays to be byte-derived from exactly the
DRep ID I selected and confirmed on screen,
**So that** approving on the device cannot target a different DRep.

**Acceptance:** mapper specs prove the device-bound credential (key/script hash) equals
the credential decoded from `vote.chosenOption` for CIP-129 and CIP-105, key-hash and
script-hash forms; sentinels map to the device `ABSTAIN`/`NO_CONFIDENCE` types;
`VotingStore` hands the signing layer `vote: chosenOption` verbatim (`toBe`).

### US-3.2 — Honest device-state feedback
**As a** hardware-wallet user whose device is disconnected, locked, without the Cardano
app open, or who rejected the transaction,
**I want** the confirmation dialog to reflect the device state and gate the Confirm
button until my device reports a signed transaction,
**So that** I always know why the flow is not progressing.

**Acceptance:** dialog specs cover `CONNECTING` (disconnected/locked copy),
`CONNECTING_FAILED`, `LAUNCHING_CARDANO_APP`, `VERIFYING_TRANSACTION`,
`VERIFYING_TRANSACTION_FAILED` (rejected), and `UNRECOGNIZED_WALLET` (Trezor
invalid-state), for Ledger and Trezor treatments; Confirm is enabled only at
`VERIFYING_TRANSACTION_SUCCEEDED`; no passphrase input renders for HW wallets.

### US-3.3 — No software fallback, no leaks
**As a** privacy-conscious HW user,
**I want** the flow to never fall back to software signing and never write my vote
target to logs or analytics,
**So that** the slice-1 privacy floor holds on the HW surface too.

**Acceptance:** `delegateVotesRequest.execute` / `api.ada.delegateVotes` are proven
un-called on the HW path; both HW error paths log only a derived `errorCode`; the floor
suite grows 17 → 20 and stays green; HW analytics carry the vote-kind only.

---

## Functional Requirements

| ID | Requirement | Where |
|----|------------|-------|
| FR-1 | Remove the stale `// TODO: handle HW case` above the implemented HW branch | `VotingStore.ts:370` |
| FR-2 | `initializeVPDelegationTx` catch logs `{ errorCode }` (derived via `parseApiCode`) instead of `{ error }` | `VotingStore.ts:347-358` |
| FR-3 | `delegateVotes` HW catch logs `{ errorCode: 'generic' }` instead of `{ error }` | `VotingStore.ts:403-412` |
| FR-4 | Ledger mapper spec: `cast_vote` → `params.dRep` for CIP-129/CIP-105 × key/script + sentinels + non-vote guard, credential byte-equal to `Cardano.DRepID.toCredential(chosenOption)` | `shelleyLedger.spec.ts` (NEW, 7 tests) |
| FR-5 | Trezor mapper spec: same matrix → `VOTE_DELEGATION` type + `dRep` params, with the `@trezor/device-authenticity` stub | `shelleyTrezor.spec.ts` (NEW, 7 tests) |
| FR-6 | `VotingStore` HW-branch spec: certificate shape (verbatim `vote`), `register_reward_account` prepend, `updateTxSignRequest` → `initiateTransaction` order, software-wallet negative, init failure, `_sendMoney` + polling-loop success (fake timers) with AC-3 never-fallback assertions, submission failure | `VotingStore.spec.ts` (NEW, 6 tests) |
| FR-7 | Dialog spec: six device-state renders, Trezor passphrase hint, HW identity (byte-equal raw ID, no passphrase input), Confirm gating | dialog spec (4 → 13 tests) |
| FR-8 | Flow spec: HW row-select → prefill → submit → confirmation → payload (Ledger), Confirm gating at `VERIFYING_TRANSACTION`, Trezor treatment; task-ID comments stripped | `VotingGovernancePage.spec.tsx` (4 → 7 tests) |
| FR-9 | Floor suite: HW init-failure and HW submission-failure adversarial logger tests + HW vote-kind analytics test | `governance-sanitization.spec.ts` (17 → 20 tests) |

---

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | `tsc --noEmit` zero errors after the task (`node_modules/.bin/tsc --noEmit`; `yarn compile` is unreliable under Node v24) |
| NFR-2 | Jest object-argument assertions use `expect.objectContaining` (prettier 2.1.2 oscillation guard); no inline `import { type X }` |
| NFR-3 | New comments: 1–3 plain why-lines, no task IDs / labels / change history |
| NFR-4 | Floor suite count never shrinks below 17; the three additions make it 20 |
| NFR-5 | `prettier --write` only on changed `.ts/.tsx` files — never tracker JSON, locale JSONs, or `translations/messages.json` |

---

## Architecture: the two-stage HW flow under test

```
VotingPowerDelegation (form)  ── Submit ──▶ VotingStore.initializeVPDelegationTx
   │                                          ├─ selectDelegationCoins (join, poolId)
   │                                          ├─ HW: certificates = [cast_vote { vote: chosenOption }]  ← byte-equality seam
   │                                          │      (+ register_reward_account prepend when required)
   │                                          ├─ HW: hardwareWallets.updateTxSignRequest(coinSelection)
   │                                          └─ HW: hardwareWallets.initiateTransaction({ walletId })
   ▼                                                   └─ device signs; hwDeviceStatus walks
VotingPowerDelegationConfirmationDialog                   CONNECTING → … → VERIFYING_TRANSACTION
   │   HardwareWalletStatus(hwDeviceStatus, isTrezor)     → VERIFYING_TRANSACTION_SUCCEEDED (signedTx set)
   │   Confirm enabled ONLY at VERIFYING_TRANSACTION_SUCCEEDED (dialog :141-147)
   ▼
VotingStore.delegateVotes ── HW branch ──▶ hardwareWallets._sendMoney({ selectedWalletId })
   │                                        └─ sendMoneyRequest.execute({ signedTransactionBlob: signedTx })
   │        2s polling loop until !isExecuting && !isTransactionPending
   │        analytics: vote-kind only  ·  delegateVotesRequest NEVER executed (AC-3)
   ▼
device-bound certificate → shelleyLedger.toLedgerCertificate / shelleyTrezor.toTrezorCertificate
                           └─ Cardano.DRepID.toCredential(vote) → key/script hash on device (AC-5)
```

Signing happens **during** `initializeVPDelegationTx`/`initiateTransaction`;
`delegateVotes` only submits the already-signed blob. Every test models this two-stage
reality.

---

## What Slice-3 Deliberately Does NOT Include

- ❌ §8 permanent HW caption + `governance.hw.*` copy variants (deferred with the §8
  conflict — D-1; revisit when a copy-bearing slice touches this surface)
- ❌ §7 three-representation confirmation layout (CIP-105 line, signed-payload line,
  source label) — slice-2 D3 deferral stands
- ❌ Any new i18n keys or locale changes (invariant #11 untouched — nothing new to mark)
- ❌ Storybook changes — `Governance.stories.tsx:462-496` already exposes the HW dialog
  with the `hwDeviceStatusOptions` knob and the Ledger wallet (`GOVERNANCE_WALLETS[1]`)
- ❌ `HardwareWalletsStore` changes (its `[HW-DEBUG] … { error }` logging is a noted
  residual risk, out of slice scope — findings F-2)
- ❌ Real-device QA / on-device visual verification (task-125 `manual_execution`)
- ❌ Any new delegation backend, store field, route, or IPC surface

---

## Docs / Designs / Research / Workflows / Skills Consulted

- **Orchestration contract:** `prompt.md` (:45-89 doc structure, :93-139 invariants,
  :147-148 slice order, :160-219 loop, :202-211 status rule)
- **Plan:** `governance-drep-discovery-plan.md` (:93 existing-HW-path requirement, :116
  HW acceptance criterion, :259 delegation-integration rule, :279 slice-3 definition,
  :332 competing-path risk, :336 log-leak risk)
- **Designs:** `designs/shared-design-tokens.md` §7 (:106-135 identity equality rule +
  release-blocking HW assertion) and §8 (:137-151 — conflicted, resolved per D-1)
- **Research:** `research/slice-2-findings.md` (harness precedents, D3 deferral);
  `research/slice-1-final-pass-findings.md` (jest/tsc direct binaries)
- **Precedent (structure only):** `task-plans/slice-2-PRD.md`,
  `task-plans/slice-2-implementation-guide.md`
- **Live seams:** every file/line anchor in the guide re-verified against this worktree
  (branch `feat/drep-discovery-slice-3`, base `883ffae09`) on 2026-07-23; drift found:
  none material (`HwDeviceStatuses` ends :105, Ledger success block :2646-2650)
- **Workflows/skills at build time:** `.agent/workflows/test.md`,
  `.agent/workflows/frontend.md`; skills `git-commit-formatter`, `evidence-rules`;
  `bech32-encoding-decoding` not needed (vectors pre-generated and verified — F-4)

---

## Locked Invariants Touched

| # | Invariant | How slice-3 honors it |
|---|---|---|
| 2 | Sanitization floor | The two proven HW logger leak surfaces are fixed to `{ errorCode }`-only payloads; floor suite grows 17 → 20 with adversarial-error coverage of both paths + HW analytics; no other logging surface changes |
| 4 | No second delegation backend | Zero production flow changes; AC-3 tests prove `delegateVotesRequest`/`api.ada.delegateVotes` are never invoked for HW wallets; `VotingStore` still never reads `GovernanceStore` |
| 10 | Byte-equality | `vote: chosenOption` verbatim into the certificate (`toBe`), device credential === `Cardano.DRepID.toCredential(chosenOption)` for all four ID forms, rendered dialog ID byte-equal in the HW flow test |
| 11 | Preliminary copy | No new/changed copy; no `!!!` touched |
| 13 | Form-only sentinels | Sentinels map to device `ABSTAIN`/`NO_CONFIDENCE` types, never to DRep credentials; dialog sentinel labels untouched |

Not touched: #1 (no data-source change), #3 (no anchor content), #5/#6 (no IPC/CLI),
#7/#8 (slice-5), #9 (cv-1), #12 (slice-7), #14 (no status vocabulary change).

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| task-113 (confirmation renders DRep ID) | verified |
| HW production path in `VotingStore.ts:281-438` | present (untested; TODO comment stale) |
| `HardwareWalletsStore` seams (`hwDeviceStatus:248`, `_sendMoney:489-549`, `updateTxSignRequest:662-668`, `initiateTransaction:2662-2683`, signing :2149/:2483, `checkIsTrezorByWalletId:3264`) | present, untouched this slice |
| Pure mappers `shelleyLedger.ts:53-100`, `shelleyTrezor.ts:71-107` | present |
| Sanitization-suite `VotingStore` construction precedent (`governance-sanitization.spec.ts:230-238`; `Store.ts:13-17,25-27`) | present |
| Flow/dialog spec harnesses (slice-2) | present (4 + 4 tests) |
| Jest 27.5.1 + `@swc/jest` + jsdom, `globals.environment` injected (`jest.config.js:63-67`) | verified |

---

## Risks Specific to Slice-3

| Risk | Mitigation |
|------|-----------|
| **Untested HW logger leak surfaces** (`VotingStore.ts:347-358`, `:403-412`) — planning **proved** the renderer logger applies no `filterLogData`, so raw error objects (which can embed the vote target from device/API messages) reach electron-log | D-2 production fix (`{ errorCode }`-only payloads); AC-6 floor tests throw adversarial errors embedding a DRep ID + both sentinel literals and assert the logged payload is clean |
| Selector creates a competing delegation path (plan risk :332) | No production flow change; AC-3 never-fallback assertions at the store boundary |
| Vote target leaks into analytics (plan risk :336) | HW analytics test pins the 3-arg vote-kind-only payload, mirroring the existing software test; note the vote-kind precedent (findings F-5) |
| `@trezor/connect` unloadable under Jest 27 breaks the Trezor mapper spec | D-4 smoke-tested stub: `jest.mock('@trezor/device-authenticity', () => ({}))` keeps real `PROTO` enums |
| Fake-timer polling test hangs or races (Jest 27 has no async timer advance) | Deterministic pattern pinned in the guide: microtask flushes + two `advanceTimersByTime(2000)` steps; even if the first advance fires before the timer arms, the second tick still resolves the loop |
| `renderDialog`/`renderFlow` device-state texts drift from en-US values | Expected strings were read from `en-US.json` during planning and are quoted literally in the guide (e.g. `connecting.known` interpolates the wallet name) |
| Invalid DRep vectors crash `Cardano.DRepID` in mappers | Vectors generated from fixed credential hashes and round-trip verified during planning (F-4); listed literally in the guide |
| `initializeVPDelegationTx` catch also covers software-path failures under a message that says "with HW" | Fix keeps the message string unchanged (log-tooling stability, smallest change); recorded in findings F-6 |

**Open questions:** none. D-1–D-4 resolve planning's discoveries autonomously; nothing
meets the stop-conditions bar.

---

## Definition of Done

- [ ] task-115 acceptance criteria met; AC ↔ test mapping in the guide satisfied
- [ ] `node_modules/.bin/tsc --noEmit` → zero errors; eslint clean on touched files
- [ ] Focused Jest: mapper 7+7, `VotingStore.spec.ts` 6, dialog 13, flow 7 — all green
- [ ] Sanitization floor: `yarn test:jest tests/jest/security/governance-sanitization.spec.ts` → **20/20**
- [ ] `VotingStore.ts` diff limited to the TODO removal + two catch blocks; all other production files byte-identical
- [ ] Task-ID comments stripped from `VotingGovernancePage.spec.tsx` and the floor-suite header
- [ ] Code review clean; exactly one subject-only commit; tracker synchronized
  (`status: complete` — never `verified`; truthful `statusReason`; `evidence`;
  `updatedAt: 2026-07-23`)
- [ ] `research/slice-3-findings.md` updated with implementation findings at close
- [ ] Final Outcome below filled at slice close

---

## Final Outcome

_To be filled at slice close._
