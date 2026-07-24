# UX-Refinement PRD: Sync Awareness + Two-Phase First Load

> **Planning Status:** approved | **Date:** 2026-07-23 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `ux-refinement` — "UX refinement - Sync awareness + two-phase first load"
> **Tasks:** task-159 … task-169 (11 tasks; task-166 is `partial` with a locked `manual_execution` remainder)
> **Implementation guide:** [ux-refinement-implementation-guide.md](./ux-refinement-implementation-guide.md)

---

## Executive Summary

This phase hardens the load and sync experience of the already-shipped slice-1 DRep
directory without widening feature scope, implementing the four ratified UX-refinement
decisions from the plan Key Decisions (`governance-drep-discovery-plan.md:161-165`):

1. **Sync behaviour — soft warning, query anyway.** The container passes
   `networkStatus.isNodeInSync` / `syncProgress` into the directory (task-159); the
   directory renders a persistent, non-dismissible syncing banner with the live sync %
   while `!isNodeInSync`, falls back to a new `DRepEmptyState noSync` treatment when the
   syncing query yields zero DReps or an era/availability error, and refetches once the
   node reaches tip (task-160).
2. **First-load shape — two-phase load.** `GovernanceQueryService` splits into Phase 1
   `fetchDRepRegistrations` (`drep-state --all-dreps`, **no** `--include-stake`, plus
   `query tip`; entries carry `votingPower: null`) and Phase 2 `fetchDRepStake`
   (`drep-stake-distribution --all-dreps`; credential→lovelace map) (task-161), delivered
   over the existing channel plus **one** new request/response channel;
   `GovernanceStore` sequences the two requests and merges by DRep id, surfacing the new
   `DRepErrorBanner rankingUnavailable` with `—` on Phase-2 failure (task-162).
3. **CLI timeouts — per-phase budgets.** The single `CLI_TIMEOUT_MS` becomes
   per-call-threaded budgets: 10 s bare-list / 30 s stake, the 30 s value explicitly
   provisional (task-163). `shared-design-tokens.md §6` is already two-phase; task-163
   confirms rather than rewrites it.
4. **Directory names (v1) — ID-only, documented.** task-165 reinforces the ID-only v1
   limitation in the directory design doc; task-166 confirms the deferred
   real-fixture + latency follow-up (autonomous portion only).

The phase also ships the new i18n copy with `!!!` markers (task-164), consolidated Jest
coverage for the sync banner, two-phase transitions, and ranking-unavailable
(task-167), the `DRep-state-snapshot.json` support log file — the one documented
sanitization-floor exception (task-168) — and the CLI robustness cleanups: a structured
era-retry signal plus a real-binary argv smoke test that self-skips without
`cardano-cli` (task-169).

**Why now:** the locked slice order (`prompt.md:147-148`) places `ux-refinement`
directly after slice-3, "because it hardens the already-shipped directory (sync banner,
two-phase load) that users hit first." Slice-2 and slice-3 are closed
(`883ffae09`, `a463c31d0`); every external dependency of this phase points at task-103
(`complete`). The route-scoped fetch trigger (former UX-3) already landed as slice-1
final-pass FP-10 — `GovernanceStore.setup()` calls only `super.setup()` — so this phase
builds on it rather than re-implementing it.

---

## Problem Statement

The as-built directory silently serves an incomplete DRep list while the node is behind
the chain tip: `drep-state` returns a correct-but-stale snapshot as of the node's
*local* tip — never an error — so nothing tells the user the list may be incomplete
(research "Mechanism Correction", `ux-refinement-sync-and-load-research.md:43-58`). The
single-phase query also keeps the officially "potentially expensive" `--include-stake`
computation on the first-paint path behind one unvalidated 10 s timeout
(`GovernanceQueryService.ts:52`, `:187-196`). The designed remedies — syncing banner,
`noSync` empty state, `rankingUnavailable` banner, two-phase budgets — exist **only in
the design docs**: `DRepEmptyState`, `DRepErrorBanner`, and the
`governance.drepDirectory.syncing` / `empty.noSync` / `error.rankingUnavailable` /
voting-power-tooltip i18n keys are all absent from the live tree (verified by grep,
2026-07-23). "The existing DRepEmptyState noSync" in task-160's wording and
"already-designed error.rankingUnavailable" in task-162's are aspirational — this phase
builds exactly the variants it needs (PD-1). Support diagnostics also lack any DRep
directory snapshot, and the conway era-fallback is gated on fragile substring matching
of free-text error output.

---

## Per-Task Contract

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-159** — Container reads node-sync state | `autonomous` | `DRepDirectoryPage` passes `isNodeInSync` + `syncProgress` into `DRepDirectory` as new required props; spec/story call sites updated to keep the tree green; store boundary preserved (no NetworkStatus import in `GovernanceStore`) | No banner rendering (task-160); no store changes | task-103 ✔ |
| **task-160** — Syncing banner + noSync fallback + clear-on-sync refetch | `autonomous` | Persistent inline-warning syncing banner (icon + text + live %) in `DRepDirectory` while `!isNodeInSync`; NEW `_shared/DRepEmptyState.tsx` with **only** the `noSync` variant; fallback gating for zero-result/era-availability failure while syncing; MobX `reaction` in the container refetching on the `isNodeInSync` false→true transition; story knob "Node syncing" | No `noResults`/`selfnode` variants (owned by slices 6/1-follow-up per design); no hard gating of the Governance nav; no dismissal control (banner is persistent by design) | task-159 |
| **task-161** — Split query service into registration + stake phases | `autonomous` | Service: rename `fetchDRepList` → `fetchDRepRegistrations` (drop `--include-stake`; `votingPower` always `null`), add `fetchDRepStake` (`drep-stake-distribution --all-dreps`; dual-shape tolerant parse → CIP-129-keyed decimal-string map; per-phase in-flight dedup); IPC: new `GOVERNANCE_DREP_STAKE_CHANNEL` + `DRepStakeQueryPayload` type + main/renderer channel plumbing; existing service spec updated; synthetic stake mock rewritten to the canonical CLI key shape | No streaming/push IPC; no store changes (task-162); no timeout changes (task-163); no per-DRep CLI calls ever | task-103 ✔ |
| **task-162** — Store drives the two phases | `autonomous` | `GovernanceStore`: new `VotingPowerEnrichState` enum + `votingPowerState` observable + `isRankingUnavailable` computed; `fetchDRepList` sequences Phase 1 (→ `Loaded`, list painted, voting power `null`) then Phase 2 (merge by `drepId`); Phase-2 failure keeps the list and flags ranking-unavailable; NEW `_shared/DRepErrorBanner.tsx` with **only** the `rankingUnavailable` variant; `—` tooltip wiring in `DRepCard` (loading vs unavailable); phase-failure log hygiene (`{ errorType }` only); container/spec/story threading | No Reshuffle/seed work (slice-5 task-118 — see PD-4); no `refresh failed` banner variant (owned by the slice that ships `error.refresh`); no change to the `—` formatting seam (`formatVotingPower` already returns `—` for null) | task-161 |
| **task-163** — Per-phase CLI timeouts | `autonomous` | Replace static `CLI_TIMEOUT_MS` with `REGISTRATION_TIMEOUT_MS = 10_000` / `STAKE_TIMEOUT_MS = 30_000` threaded per call through `_runCliQuery` / `_runCliQueryWithEraFallback`; spec updates incl. a 30 s stake-timeout test; **confirm** `shared-design-tokens.md §6` (already two-phase, lines 89-104 verified) | No re-derivation of the 30 s value (deferred to the task-166 manual follow-up); no §6 rewrite | task-161 |
| **task-164** — i18n: syncing + tooltip copy | `autonomous` | Add 5 keys to en-US + ja-JP (alphabetical, `!!!` kept): `empty.noSync`, `error.rankingUnavailable`, `syncing`, `votingPower.loadingTooltip`, `votingPower.unavailableTooltip`; run `yarn i18n:manage` | No `!!!` removal (release-end manual review); no copy beyond this phase's surfaces (`empty.selfnode`, `error.refresh` stay with their owners) | task-160, task-162 |
| **task-165** — Document ID-only v1 | `autonomous` | Add an explicit "ID-only in v1" section to `designs/drep-discovery-design.md`; verify the plan Key Decisions row (`:165`, already present) and that no card renders a name (live `DRepCard` renders `DRepIdDisplay` only — verified) | No code changes; no plan Key Decisions rewrite | — |
| **task-166** — Record deferred fixture/latency follow-up | autonomous portion only; **remainder `manual_execution`** | Verify/extend the plan Risks mitigation naming all three remaining items (mainnet fixture, p50/p95 latency, promotion into a committed `tests/jest/governance/` fixture); truthful tracker `statusReason`; **status stays `partial`** | **Do not attempt** the mainnet capture or latency measurement (locked non-autonomous, `prompt.md:178-180`); no fixture promotion without real data | — |
| **task-167** — Jest: sync banner, two-phase, ranking-unavailable | `autonomous` | Extend `DRepDirectory.spec.tsx` (+7), `GovernanceStore.spec.ts` (+5), NEW `DRepDirectoryPage.spec.tsx` (3, incl. the clear-on-sync reaction); floor suite re-run (20/20, never below) | No e2e/Cucumber; no re-testing of FP-10's no-startup-fetch (already covered; kept as-is) | task-160, task-162, task-163 |
| **task-168** — DRep-state snapshot log + bundling | `autonomous` | `logDRepStateSnapshot` in `setupLogging.ts` (mirrors `logStateSnapshot`); hooked on Phase-1 success in the main `governanceChannel`; `DRep-state-snapshot.json` in `ALLOWED_LOGS`; boundary documented in `shared-design-tokens.md` (new §12); focused Jest proving public-data-written / no-vote-target / overwrite / registration | Never route through `filterLogData` (the point of the exception); never include wallet/vote state (the payload type carries none); full support-bundle generation proof stays manual (task-125) — recorded as verification debt | task-162 |
| **task-169** — CLI robustness: era-retry signal + argv smoke test | `autonomous` | PART A: classify argv-parse failures structurally at the spawn boundary (`UsageError` error class from the CLI usage-block signature); `_shouldRetryWithConway` gates on the error class only — an unrelated `QueryFailed` can never trigger a conway retry; negative + positive unit tests. PART B: parse-only real-binary smoke test with the exact per-phase argv, no socket, **self-skipping** (`describe.skip`) when `cardano-cli` is not on PATH | No supported-era startup probe (rejected — see PD-8); no behavior change on the happy path; PART B cannot be positively demonstrated in this environment (no `cardano-cli`) — recorded as verification debt | task-103 ✔ (applies to both phase calls since task-161 lands first) |

No task in this phase is `interactive_decision`. The locked non-autonomous set
(prompt.md:176-181) touches this phase only through the **task-166 remainder**
(`manual_execution`: mainnet fixture + p50/p95 latency capture) — it must not be
attempted and task-166 stays `partial`.

---

## Planning Decisions (binding, as applied)

- **PD-1 — Build only this phase's variants of the designed-but-unbuilt components.**
  `DRepEmptyState` and `DRepErrorBanner` do not exist anywhere in the live tree (grep
  verified). They are created at the design's component seams
  (`drep-discovery-design.md:184-185`) with variant-union props ready for future
  slices, but implement **only** `noSync` (task-160) and `rankingUnavailable`
  (task-162). `noResults`/`selfnode` and the `refresh failed` banner stay with their
  owning slices; the variant unions carry a comment saying exactly that.
- **PD-2 — Live directory naming wins over the design doc's `shared/`.** The design map
  says `components/governance/shared/`; the live tree uses
  `components/governance/_shared/` (`DRepIdDisplay`, `DRepSourceLabel`,
  `DRepStatusBadge`). New components land in `_shared/`. Recorded as doc drift; no
  design-doc edit needed (the map is IA-level).
- **PD-3 — Two-phase IPC = one new request/response channel, store-sequenced.** Reuse
  the `RendererIpcChannel`/`MainIpcChannel` pattern exactly: `GOVERNANCE_DREP_LIST_CHANNEL`
  stays Phase 1 (registrations; `votingPower: null`), new
  `GOVERNANCE_DREP_STAKE_CHANNEL` carries Phase 2 as
  `DRepStakeQueryPayload { stakeByDRepId: Record<DRepId, Lovelace>; fetchedAt: number }`
  — a decimal-string lovelace map keyed by the **same** CIP-129 DRep id the list
  payload derives via `Cardano.DRepID.cip129FromCredential`
  (`GovernanceQueryService._credentialToDRepId`, `:481-511`). `GovernanceStore`
  sequences the two requests and merges by `drepId` string equality. No streaming, no
  push additions, no second delegation backend.
- **PD-4 — task-162's Reshuffle AC is scoped to Manual Refresh.** Reshuffle and the
  randomization seed are slice-5 task-118 territory and do not exist yet. This phase
  satisfies "Manual Refresh re-runs both phases" fully; the "Reshuffle reseeds without
  re-querying" half is forward-compatibility only — the store's two-phase logic adds no
  coupling a future seed mechanism would have to fight (re-query happens only inside
  `fetchDRepList`). The tracker `statusReason` for task-162 must say: *"Reshuffle half
  of AC-3 is forward-compat only — Reshuffle/seed do not exist until slice-5 task-118;
  the two-phase path re-queries only via fetchDRepList and adds no seed coupling."*
- **PD-5 — Phase 1 always resets voting power to null; Phase 2 fills it.** On every run
  (first load *and* manual refresh) the rebuilt entries carry `votingPower: null` until
  the stake phase lands. Rationale: never render a stale stake number next to fresh
  registrations; the designed enrich visual (`—` + "Loading voting power…" tooltip,
  design §6) covers the gap, and Phase-2 failure then satisfies AC-2 literally (`—` +
  `rankingUnavailable`). This also keeps the merge logic small-model-simple. Stale-
  while-refresh continues to apply to the **list** (registrations retained on refresh
  failure via the existing `hasExistingData` path), not to stake values.
- **PD-6 — Stake-distribution parsing is dual-shape tolerant, strict inside.**
  `drep-stake-distribution --all-dreps --output-json` cannot be run in this environment
  (no `cardano-cli`), and the CLI's serialization drifted across major versions between
  an object map and an array of pairs, with keys of the form `drep-keyHash-<hex>` /
  `drep-scriptHash-<hex>` / `drep-alwaysAbstain` / `drep-alwaysNoConfidence`. The
  parser accepts **both container shapes**, skips exactly the two sentinel keys
  (invariant #13 — never directory entries), derives the CIP-129 id from the hex
  credential via the existing `_credentialToDRepId`, and throws `ParseFailed` on any
  other key/value shape (a Phase-2 `ParseFailed` degrades gracefully to
  `rankingUnavailable`, never corrupts the list). The committed synthetic mock
  `tests/mocks/governance/drep-stake-distribution.json` uses a **wrong shape**
  (bech32-keyed `{ stake: … }` objects) and is referenced by no test — task-161
  rewrites it to the canonical key shape. Real-shape confirmation is explicitly part of
  the task-166 manual follow-up.
- **PD-7 — Per-phase timeouts are threaded per call, not per-instance.**
  `_runCliQuery(args, timeoutMs)` / `_runCliQueryWithEraFallback(args, timeoutMs)`
  receive the budget from the phase method (`REGISTRATION_TIMEOUT_MS = 10_000` for
  drep-state + tip, `STAKE_TIMEOUT_MS = 30_000` for stake). No single static remains.
- **PD-8 — task-169 PART A uses spawn-boundary error classification, not a probe.** A
  one-time supported-era probe would add an extra spawn, breaking the AC's "spawn
  called exactly twice" negative test and adding a fourth CLI invocation class. Instead
  `_runCliQuery` classifies a non-zero exit whose stderr bears the structural
  optparse-applicative argv-rejection signature
  (`/(invalid (option|argument)|missing:|usage:)/i`) as a new
  `GovernanceQueryErrorType.UsageError`; `_shouldRetryWithConway` becomes a pure
  error-class check (`queryErrorType === UsageError`). A node-side `QueryFailed`
  (query executed, node/ledger error) can never match, no production message needs to
  avoid the word "latest", and the classification happens once at the controlled spawn
  boundary rather than by grepping arbitrary free text for era keywords.
- **PD-9 — task-168 snapshot writes from the Phase-1 registration payload at the IPC
  boundary.** Post-split, "each successful `fetchDRepList()`" maps to each successful
  `fetchDRepRegistrations()` IPC response: the file is literally the *DRep-state*
  snapshot (ids, status, activity, anchor pointers — the anchor-pointer-cache role
  needs nothing from the stake phase), and `DRepListQueryPayload` structurally cannot
  carry wallet/vote data. The writer is called from the main
  `governanceChannel` Phase-1 handler inside its own try/catch so a write failure never
  fails the directory response. It bypasses `filterLogData` by construction
  (`constructMessageBody` applies no filtering — verified) — the one documented
  exception to invariant #2.
- **PD-10 — Voting-power tooltips use native `title`/`aria-label`, ids pinned.** §9's
  microcopy inventory names no ids for the two tooltip strings; they are pinned as
  `governance.drepDirectory.votingPower.loadingTooltip` ("Loading voting power…",
  design §6) and `governance.drepDirectory.votingPower.unavailableTooltip` ("Stake
  distribution unavailable this refresh.", design §3 `:70`). The `—` span carries them
  via the native `title` + `aria-label` attributes (deterministic, accessible, zero new
  dependencies); a react-polymorph `PopOver` upgrade is deliberate polish debt for a
  later pass. The syncing message uses the `{progress}` placeholder (§9 canonical row
  `:179`; §6's `{n}` is the informal variant).
- **PD-11 — Behavior Jest lands consolidated in task-167 by tracker design.** The
  dependency edges (167 ← 160, 162, 163) make task-167 the phase's dedicated test
  task. Tasks 159-163 are verified per-task by `tsc --noEmit`, eslint, and the
  **existing** suites they update (service spec, store spec, component spec — which
  they must keep green), with the new-behavior matrix landing in task-167. task-168 and
  task-169 carry their own focused specs (their ACs demand them).
- **PD-12 — No `.scss.d.ts` files are needed.** The repo types all SCSS modules through
  the global `declare module '*.scss'` in `source/renderer/declaration.d.ts`; the
  existing drep-directory components ship no per-file `.d.ts`. New `.scss` modules
  follow that live pattern (the standing `.scss.d.ts` convention is satisfied
  vacuously — recorded so nobody "fixes" it).

---

## User Stories

### US-UX.1 — Honest directory during sync
**As a** user opening Governance while my node is still syncing,
**I want** a persistent banner telling me the list may be incomplete (with live sync %),
data anyway when available, a clear fallback when nothing can be shown, and the full
list to appear by itself once sync completes,
**So that** I am never silently shown a stale or empty directory.

**Acceptance:** banner renders (icon + text, `--badge-warning-*` slots, never color
alone) with live `{progress}` while `!isNodeInSync` and clears at `isNodeInSync`;
zero-result/era-availability failure while syncing shows `DRepEmptyState noSync`
instead of a bare error; the container reaction triggers exactly one refetch on the
false→true transition.

### US-UX.2 — Fast first paint, enriched ranking
**As a** user loading the directory for the first time,
**I want** the DRep list to paint from the cheap registration read and voting power to
fill in afterward,
**So that** the expensive stake computation never blocks my first view.

**Acceptance:** Phase 1 paints the list (`Loaded`) with `—` + "Loading voting power…"
tooltips; Phase 2 fills `BigNumber` voting power merged by DRep id; Phase-2 failure
keeps the list, shows `—` with the unavailable tooltip and the `rankingUnavailable`
banner; Manual Refresh re-runs both phases; lovelace precision is lossless end to end.

### US-UX.3 — Diagnosable support bundles without a privacy hole
**As a** support engineer reading a user's log archive,
**I want** the latest public DRep directory snapshot in the bundle,
**So that** I can reproduce directory issues — while the user's own vote target can
never appear in any log.

**Acceptance:** `DRep-state-snapshot.json` is written on each successful Phase-1
response (overwriting), registered in `ALLOWED_LOGS`, retains `drepId` values
(deliberate `filterLogData` bypass), structurally cannot contain vote/delegation data;
the floor suite stays at 20/20.

### US-UX.4 — Robust CLI grammar handling
**As a** developer maintaining the CLI integration,
**I want** the conway fallback gated on a structured argv-parse signal and the built
argv proven against the real binary,
**So that** an unrelated query failure can never mask itself behind a spurious era
retry, and grammar regressions (the FP-1 class) are caught before release.

**Acceptance:** non-era `QueryFailed` → exactly two spawns, no retry; era-alias
rejection still retries conway; the parse-only smoke test clears the real parser for
every phase argv — and **skips** (not fails) where `cardano-cli` is absent.

---

## Functional Requirements

| ID | Requirement | Where |
|----|------------|-------|
| FR-1 | `DRepDirectoryPage` passes `isNodeInSync` + `syncProgress` as required `DRepDirectory` props; `GovernanceStore` remains free of NetworkStatus imports | container + component Props (task-159) |
| FR-2 | Persistent syncing banner (`governance.drepDirectory.syncing`, `{progress}`, warning tokens, icon + text, `role="status"`) while `!isNodeInSync` | `DRepDirectory.tsx` (task-160) |
| FR-3 | NEW `_shared/DRepEmptyState.tsx` (`noSync` only) replacing the empty/error branch when `!isNodeInSync` ∧ (Loaded-empty ∨ Failed non-selfnode) | task-160 |
| FR-4 | Container `reaction` refetches once on `isNodeInSync` false→true; disposed on unmount | `DRepDirectoryPage.tsx` (task-160) |
| FR-5 | Phase 1 `fetchDRepRegistrations`: `drep-state --all-dreps --output-json` (no `--include-stake`) + `query tip`; entries `votingPower: null` | `GovernanceQueryService.ts` (task-161) |
| FR-6 | Phase 2 `fetchDRepStake`: `drep-stake-distribution --all-dreps --output-json` → `DRepStakeQueryPayload` (CIP-129-keyed decimal strings; sentinels skipped; dual container shape; per-phase dedup) | task-161 |
| FR-7 | New IPC channel `GOVERNANCE_DREP_STAKE_CHANNEL` with the `__governanceError` plain-object error contract shared with the list channel | `api.ts`, main + renderer `governanceChannel.ts` (task-161) |
| FR-8 | Store sequences Phase 1 → `Loaded` → Phase 2 merge by `drepId`; `VotingPowerEnrichState` observable; failure → `Failed` + list retained; dedup guard covers the enrich window; phase catches log `{ errorType }` only | `GovernanceStore.ts` (task-162) |
| FR-9 | NEW `_shared/DRepErrorBanner.tsx` (`rankingUnavailable` only) rendered when `votingPowerState === Failed`; `—` tooltip = loading vs unavailable by enrich state | task-162 |
| FR-10 | `REGISTRATION_TIMEOUT_MS = 10_000` / `STAKE_TIMEOUT_MS = 30_000` threaded per call; timeout message interpolates the per-call budget | task-163 |
| FR-11 | 5 new i18n keys in en-US + ja-JP, alphabetical, `!!!`-prefixed; `yarn i18n:manage` run | task-164 |
| FR-12 | "ID-only in v1" section added to `drep-discovery-design.md`; plan row `:165` verified | task-165 |
| FR-13 | Plan Risks mitigation names all three remaining manual follow-up items; tracker truthful; status stays `partial` | task-166 |
| FR-14 | Jest: banner render/clear + fallback (component), two-phase transitions + ranking-unavailable + dedup + log hygiene (store), sync props + reaction refetch (container) | task-167 |
| FR-15 | `logDRepStateSnapshot` → `Logs/pub/DRep-state-snapshot.json` on Phase-1 success, overwriting; `ALLOWED_LOGS` registration; §12 boundary doc; focused spec | task-168 |
| FR-16 | `UsageError` classification at the spawn boundary; `_shouldRetryWithConway` = error-class check; negative (2 spawns) + positive era tests | task-169 PART A |
| FR-17 | Parse-only real-binary argv smoke test for all phase argv forms (era × network flag), no socket, `describe.skip` without `cardano-cli` | task-169 PART B |

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | `node_modules/.bin/tsc --noEmit` zero errors after every task (`yarn compile` is unreliable under Node v24 — slice-3 precedent) |
| NFR-2 | Jest object-argument assertions use `expect.objectContaining` (prettier 2.1.2 oscillation guard); no inline `import { type X }` |
| NFR-3 | New comments: 1–3 plain why-lines; no task IDs, review labels, ALL-CAPS tags, or change history |
| NFR-4 | Sanitization floor suite never below 20 passing tests; re-run after tasks 162, 167, 168 |
| NFR-5 | `prettier --write` scoped to changed `.ts/.tsx` only — never tracker JSON, locale JSONs, or `translations/messages.json` (exception: `yarn i18n:manage` may touch its own managed files) |
| NFR-6 | Every new en-US and ja-JP string keeps the leading `!!!`; keys alphabetical within the governance block |
| NFR-7 | Longer banners (syncing, rankingUnavailable, noSync) must wrap to ≥2 lines without truncation (JA/DE expand 30–60%, tokens §9 `:220`) |
| NFR-8 | One Conventional-Commits subject-only commit per task; no pushes, no `gh` (no credentials in this environment) |

---

## Architecture: sync awareness + two-phase load

```
NetworkStatusStore (isNodeInSync, syncProgress)
      │  (container-only coupling — R1)
      ▼
DRepDirectoryPage ── props ──▶ DRepDirectory
      │  reaction(false→true) → governance.refresh()      ├─ !isNodeInSync → syncing banner (live %)
      ▼                                                   ├─ !inSync ∧ (empty|failed) → DRepEmptyState noSync
GovernanceStore.fetchDRepList()                           ├─ votingPowerState=Failed → DRepErrorBanner rankingUnavailable
      │                                                   └─ DRepDirectoryList → DRepCard ('—' + tooltip)
      ├─ Phase 1: governanceDRepListChannel.request()
      │     main: fetchDRepRegistrations()
      │       cardano-cli <era> query drep-state --all-dreps --output-json  (10 s)
      │       cardano-cli <era> query tip --output-json                     (10 s)
      │       → DRepListQueryPayload (votingPower: null each entry)
      │       → logDRepStateSnapshot(payload)  [task-168, filterLogData bypass]
      │     store: Loaded (list paints), votingPowerState=Loading
      │
      └─ Phase 2: governanceDRepStakeChannel.request()
            main: fetchDRepStake()
              cardano-cli <era> query drep-stake-distribution --all-dreps --output-json (30 s)
              → { stakeByDRepId: { <cip129 drepId>: "<lovelace>" }, fetchedAt }
            store: merge by drepId → BigNumber; votingPowerState=Loaded
                   (failure → Failed; list kept; '—' + rankingUnavailable)
```

Both CLI calls keep every slice-1 guarantee: era `latest`→`conway` fallback, network
flag appended after the subcommand from node config only, socket via
`CARDANO_NODE_SOCKET_PATH` in `spawn.env`, json-bigint lossless parse → decimal-string
IPC → renderer BigNumber.

---

## What UX-Refinement Deliberately Does NOT Include

- ❌ `noResults` / `selfnode` empty-state variants and the `refresh failed` banner
  variant (owned by their slices; the variant unions are ready — PD-1)
- ❌ Reshuffle / randomization seed (slice-5 task-118; forward-compat only — PD-4)
- ❌ Cohort, detail, favorites, category-badge, or anchor-pipeline work (owned slices)
- ❌ Mainnet fixture capture, p50/p95 latency measurement, fixture promotion
  (task-166 remainder — locked `manual_execution`; the 30 s budget stays provisional)
- ❌ Streaming/push IPC or any second delegation backend (PD-3)
- ❌ A wallet-restoration-style per-directory progress bar (mechanism correction,
  research `:43-58` — the only relevant wait is global node sync)
- ❌ Names in directory cards or name search (task-165 documents; anchor-1+ owns names)
- ❌ `!!!` marker removal (release-end manual review)
- ❌ Hard-gating the Governance nav during sync (rejected in the plan decision)

---

## Docs / Designs / Research / Workflows / Skills Consulted

- **Orchestration contract:** `prompt.md` (doc structure :45-89, invariants :93-139,
  slice order :147-158, loop :160-219, non-autonomous set :176-181, status rule :202-211)
- **Grounding brief:** `ux-refinement-grounding-brief.md` (2026-07-23) — all per-task
  anchors spot-checked against this worktree (`wt/ux-refinement`, base `a463c31d0`);
  drift found and applied: `_shared/` vs `shared/` (PD-2), the unused wrong-shaped
  stake mock (PD-6), global `*.scss` declaration (PD-12), story/spec call-site churn
  from new required props (guide steps 1/2/4), en-US governance block at `:284-302`
  (task-164 neighbors pinned from live file)
- **Plan:** `governance-drep-discovery-plan.md` Key Decisions `:161-165` (four ratified
  decisions, verbatim), Risks (the "10s CLI timeout" mitigation row — task-166 target)
- **Designs:** `shared-design-tokens.md` §1 (warning tokens + never-color-alone), §3
  (BigNumber|null, `—` + unavailable tooltip `:70`), §6 (two-phase refresh table,
  lines 89-104 — already updated; task-163 confirms), §9 (microcopy ids + en source
  `:157-210`, JA-length `:220`, `!!!` rule `:222`); `drep-discovery-design.md`
  component map `:150-188` + state table `:190-205`
- **Research:** `ux-refinement-sync-and-load-research.md` — decisions R1-R5 (do not
  re-litigate), mechanism correction `:43-58`, LSM findings `:62-74`; its "Gaps"
  table anchors are stale (FP-10 landed) — live anchors used instead
- **Precedent (structure + conventions):** `task-plans/slice-3-PRD.md`,
  `slice-3-implementation-guide.md`, `slice-3-code-review.md`;
  `research/slice-3-findings.md` (tsc-direct under Node v24, renderer-logger-no-filter
  finding driving the FR-8 log hygiene)
- **Workflows/skills at build time:** `.agent/workflows/frontend.md`,
  `.agent/workflows/ipc.md`, `.agent/workflows/test.md`; skills `git-commit-formatter`,
  `i18n-messaging` (task-164), `storybook-creation` (story knob edits),
  `evidence-rules` (doc tasks 165/166); `bech32-encoding-decoding` not needed (no new
  vectors — merge keys derive through the production `cip129FromCredential` path)

---

## Locked Invariants Touched

| # | Invariant | How this phase honors it |
|---|---|---|
| 1 | Local-first | Both phase calls go through `GovernanceQueryService` to the local node; no external endpoint anywhere; the snapshot file is written locally from the same payload |
| 2 | Sanitization floor + one exception | **task-168 is the documented exception** (public directory payload, `filterLogData` bypassed, structurally no vote data, boundary in tokens §12). Everything else tightens the floor: phase-failure catches in `GovernanceStore` log `{ errorType }` only; floor suite re-run at 162/167/168 (20/20, never below) |
| 5 | Lovelace losslessness | Stake values: json-bigint `storeAsString` parse → decimal-string `stakeByDRepId` across IPC → `new BigNumber(...)` in the store merge; `votingPower` stays `BigNumber \| null`, never `Number`; raw JSONbig objects never cross IPC |
| 6 | CLI discipline | Bulk `--all-dreps` once per refresh **per phase** (still zero per-DRep calls); network flag from node config only, appended after the subcommand; socket via `spawn.env` only; era `latest`→`conway` fallback on both phase calls (and hardened by task-169) |
| 7 | Default cohort binding | Untouched (no cohort exists yet); PD-4 keeps the future seed seam clean |
| 11 | Preliminary copy | All 5 new en-US + ja-JP strings carry `!!!`; no existing marker removed |
| 13 | Form-only sentinels | `drep-alwaysAbstain` / `drep-alwaysNoConfidence` stake keys are skipped in the Phase-2 parse — sentinels never become directory entries |
| 14 | DRep status grounding | Syncing/noSync are refresh/availability states only; no new stored-status vocabulary (status stays `active \| inactive`) |

Not in play: #3 (anchor transport — anchor-1), #4 (no delegation-path changes), #8
(category badges — slice-5), #9 (auto-delegation — cv-1), #10 (byte-equality — no
identity-display change), #12 (favorites — slice-7).

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| task-103 walking-skeleton seams (service, channel, store, directory components) | `complete`; all anchors re-verified live 2026-07-23 |
| FP-10 route-scoped fetch (no `setup()` auto-fetch) | landed — `GovernanceStore.setup()` calls only `super.setup()` (`:143-145`); regression test exists (`GovernanceStore.spec.ts:126-134`) |
| `NetworkStatusStore.isNodeInSync` / `syncProgress` observables | present (`:96/:119/:611`); `stores.networkStatus` in `StoresMap` (`stores/index.ts:69`) |
| `formatVotingPower` `—`-for-null seam | present (`DRepCard.tsx:31-42`) |
| `logStateSnapshot` mirror pattern + `ALLOWED_LOGS` + `get-logs` consumer | present (`setupLogging.ts:120`, `config.ts:137-145`, `ipc/get-logs.ts`) |
| `_credentialToDRepId` (cip129FromCredential, keyHash + scriptHash) | present (`GovernanceQueryService.ts:481-511`) |
| Existing suites to keep green | `GovernanceQueryService.spec.ts` (610 lines), `GovernanceStore.spec.ts` (151), `DRepDirectory.spec.tsx` (237), floor suite 20/20 |
| Jest 27.5.1 / jsdom / `clearMocks` / `globals.environment` | verified (`jest.config.js`) |

Intra-phase edges honored by the serialized order 159 → 160 → 161 → 162 → 163 → 164 →
165 → 166 → 167 → 168 → 169 (each task's tracker `dependencies` precede it).

---

## Risks Specific to UX-Refinement

| Risk | Mitigation |
|------|-----------|
| **Real `drep-stake-distribution` output shape unverified in this environment** (no `cardano-cli`; CLI serialization drifted across versions) | PD-6 dual-shape parser, strict inside, sentinel-skip only; Phase-2 `ParseFailed` degrades to `rankingUnavailable` (list never corrupted); real-shape confirmation named in the task-166 manual follow-up; task-169 PART B proves the **argv** against the real binary where available |
| Required-prop additions break the story/spec call sites mid-task | Each prop-adding task (159, 162) updates all three call sites (container, spec helper, story helper) in the same commit; guide lists them exhaustively |
| Re-entrant refresh during the enrich window restarts Phase 1 | Dedup guard extended to `votingPowerState === Loading` (FR-8); store test pins it |
| Phase-2 failure logging leaks CLI stderr into renderer logs (renderer logger applies no `filterLogData` — slice-3 proven) | Both store phase catches log `{ errorType }` only (the normalized type string); UI keeps full message/details via `this.error` where designed; store spec adds an adversarial assertion |
| `yarn i18n:manage` never exercised in this devcontainer (slices 1-3 added no copy); may fail under Node v24 | Run it and report honestly; on environment failure, verify placement manually (alphabetical grep + spec-level intl render) and record the debt in the tracker `statusReason` |
| task-168 test cannot import `source/main/config.ts` (Electron boot guard throws outside a launcher) | Spec mocks `main/config` + `electron-log-daedalus`; the `ALLOWED_LOGS` registration is asserted as a source-text check with a comment saying why; full bundle generation proof deferred to task-125 (verification debt) |
| task-169 PART B self-skips here (no `cardano-cli` on PATH) | **Verification debt, recorded:** the smoke test's positive run requires the Nix shell; in this environment the suite must report `skipped`, never `failed`; the guide's skip mechanism (`describe.skip` on missing binary) is itself asserted by running the suite |
| task-166 remainder accidentally attempted | Locked `manual_execution` restated in the guide step; only the doc note + tracker update are in scope; status stays `partial` |
| Support-bundle inclusion not end-to-end provable in Jest | `ALLOWED_LOGS` membership + `get-logs` `isFileAllowed` logic is name-based; source-text assertion + task-125 release verification (debt recorded) |
| `syncProgress` may be fractional / null mid-boot | Banner interpolates `Math.floor(syncProgress ?? 0)`; component tests pin both |

**Open questions:** none blocking. PD-1…PD-12 resolve every judgment call surfaced
during planning; nothing meets the stop-conditions bar. The two recorded verification
debts (task-169 PART B positive run; task-168 end-to-end bundle proof) belong to the
Nix-shell / task-125 environments by design.

---

## Definition of Done

- [ ] All 11 tasks' acceptance criteria met as scoped above (task-166: autonomous
  portion only, status `partial`)
- [ ] `node_modules/.bin/tsc --noEmit` zero errors; eslint clean on touched files after
  every task
- [ ] Focused Jest green per guide step; floor suite **20/20** after tasks 162, 167, 168
- [ ] `yarn i18n:manage` run for task-164 (result honestly reported)
- [ ] task-169 PART B suite reports **skipped** in this environment (never failed)
- [ ] Design/plan docs updated: tokens §6 confirmed, tokens §12 added,
  `drep-discovery-design.md` ID-only section added, plan Risks row extended
- [ ] Code review clean per task; exactly one subject-only commit per task; tracker
  synchronized (`status`, truthful `statusReason` — incl. the PD-4 Reshuffle wording
  for task-162 — `evidence`, `updatedAt`)
- [ ] `research/ux-refinement-findings.md` written at close (or `no new research`
  recorded below)
- [ ] Final Outcome below filled at phase close

---

## Final Outcome

**Phase closed 2026-07-23.** Eleven tasks executed in guide order, one subject-only
commit each (`fbb93707b` task-159 … `bd6c21e80` task-169 on `wt/ux-refinement`,
above the planning-docs base `807e4df05`); eleven code-review rounds — every task
approved in round 1 with zero blockers.

**Shipped per task:**

- **task-159** — `DRepDirectoryPage` passes `isNodeInSync` + `syncProgress` into
  `DRepDirectory` as required props; store boundary preserved (`NetworkStatus` grep
  on `GovernanceStore.ts` empty, re-verified at close).
- **task-160** — persistent syncing banner (icon + text + floored live `%`,
  `role="status"`, `--badge-warning-*` slots), NEW `_shared/DRepEmptyState.tsx`
  (`noSync` variant only — PD-1), refetch-once container reaction on the
  false→true sync edge.
- **task-161** — service split into `fetchDRepRegistrations` (no `--include-stake`;
  `votingPower` always null) + `fetchDRepStake` (dual-shape-tolerant,
  CIP-129-keyed decimal-string map, sentinels skipped — PD-6); new
  `GOVERNANCE_DREP_STAKE_CHANNEL` with the shared `__governanceError` contract.
- **task-162** — store sequences Phase 1 (paint at `Loaded`) → Phase 2 (merge by
  `drepId`); `VotingPowerEnrichState` + `isRankingUnavailable`; NEW
  `_shared/DRepErrorBanner.tsx` (`rankingUnavailable` only); `—` tooltips by enrich
  state; `{ errorType }`-only phase logging. The Reshuffle half of AC-3 is
  **deferred as forward-compat only** (PD-4 — Reshuffle/seed arrive with slice-5
  task-118; the two-phase path adds no seed coupling).
- **task-163** — `REGISTRATION_TIMEOUT_MS = 10_000` / `STAKE_TIMEOUT_MS = 30_000`
  threaded per call (PD-7); the 30 s stake budget stays **provisional** pending the
  task-166 latency measurement; tokens §6 confirmed, not rewritten.
- **task-164** — 5 keys × 2 locales, alphabetical, `!!!` kept; `yarn i18n:manage`
  exit 0 and idempotent — the anticipated environment debt was not needed.
- **task-165** — "Directory Identity: ID-Only in v1" section added to
  `drep-discovery-design.md`; `givenName` grep empty under governance components.
- **task-166** — plan Risks mitigation now names all three remaining manual items;
  the remainder (mainnet fixture + p50/p95 latency + fixture promotion) is locked
  `manual_execution` and was **not attempted** — **status stays `partial`**.
- **task-167** — consolidated behavior matrix landed: DRepDirectory 19,
  GovernanceStore 13, DRepDirectoryPage 3 (new), VotingGovernancePage 7.
- **task-168** — `logDRepStateSnapshot` → `Logs/pub/DRep-state-snapshot.json` on
  Phase-1 success + `ALLOWED_LOGS` registration + tokens §12 boundary doc — the ONE
  documented sanitization-floor exception; focused spec 4/4. End-to-end
  support-bundle proof deferred to task-125 (verification debt).
- **task-169** — PART A structured `UsageError` era-retry gate at the spawn
  boundary (negative + positive tests); PART B real-binary argv smoke suite
  **self-skipped** in this environment (no `cardano-cli` on PATH) — the positive
  run is Nix-shell verification debt.

**Step-12 verification (run at close, 2026-07-23 — honest results):**

- `node_modules/.bin/tsc --noEmit` → exit 0, zero errors (run directly per NFR-1;
  the `yarn compile` script is this same `tsc --noEmit`).
- `yarn lint` → exit 0; 0 errors (5,566 pre-existing repo-wide warnings, none new).
- `yarn test:jest tests/jest/governance/` → **55 passed, 12 skipped, 0 failed**
  (GovernanceQueryService 38, GovernanceStore 13, logDRepStateSnapshot 4;
  GovernanceCliArgvSmoke **12 skipped** — the expected self-skip, reported as
  skipped, never as passing).
- `yarn test:jest` DRepDirectory + DRepDirectoryPage + VotingGovernancePage →
  **29 passed, 0 failed** (19 + 3 + 7).
- Sanitization floor `tests/jest/security/governance-sanitization.spec.ts` →
  **20/20, never below**.
- `yarn i18n:manage` → exit 0, idempotent (working tree unchanged after the run).
- Invariant greps: `NetworkStatus` in `GovernanceStore.ts`, `CLI_TIMEOUT_MS`, and
  `include-stake` under `source/main/governance/` → all empty. The `filterLogData`
  grep on `setupLogging.ts` prints exactly one line — the `logDRepStateSnapshot`
  doc comment *stating* the deliberate bypass; no functional `filterLogData` call
  exists in the file, so the gate's intent holds (findings F-8).
- `DRep-state-snapshot.json` registered exactly once (`config.ts:143`).

**Recorded verification debts (both by design):** task-169 PART B positive smoke
run (Nix shell); task-168 end-to-end support-bundle proof (task-125).

**Tracker at close:** task-160 promoted to `verified` — task-167's consolidated
suites are dedicated proof beyond the task's own commit (slice-2/task-114
precedent). task-162 raised `partial` → `complete` per PD-4 and guide Step 13a
(`verified` withheld: the Reshuffle half of AC-3 cannot be demonstrated until
slice-5 exists). All other tasks `complete`; task-166 stays `partial`. No
`auditSummary` exists for this phase (slice-2/3 precedent) and none was added;
`metadata.updated` bumped to 2026-07-23.

**Findings:** durable findings F-1…F-9 recorded in
[research/ux-refinement-findings.md](../research/ux-refinement-findings.md).
