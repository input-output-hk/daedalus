# Slice-8 PRD: Refresh-latency, Selfnode & Release Verification

> **Planning Status:** `approved` | **Slice Status:** closed (see [Final Outcome](#final-outcome)) | **Date:** 2026-07-31 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `slice-8` — "Slice 8 - Refresh-latency, selfnode & release verification" (`riskLevel: medium`; tasks JSON `:747-805`)
> **Tasks:** 3 — task-123, task-124, task-125 (all `pending` at the planning anchor `0cdcab581`)
> **Preceding slice:** [anchor-2-PRD.md](./anchor-2-PRD.md) (closed 2026-07-31 at `b99124416`)
> **Place in the locked slice order** (`prompt.md:146-149`): `… → anchor-1 → anchor-2 → **slice-8**`. **slice-8 is the last slice; it closes the feature.**
> **Findings:** `research/slice-8-findings.md` — **mandatory for this slice**, not optional (D-4 requires a recorded stylelint entry; see D-18).
> **Implementation guide:** `slice-8-implementation-guide.md` (authored after this PRD, by a separate author)
> **Release-verification checklist:** `../release-verification-checklist.md` (task-125's only autonomous deliverable; see D-16)
> **Evidence basis.** This PRD was authored against a verified, line-anchored grounding brief whose anchors were re-verified against the worktree at `0cdcab581` (clean tree, byte-identical to the `feat/drep-discovery` tip). Every `path:line` below traces to that verification or to a direct re-read during this planning pass. Where a design doc and the live repo disagree, the repo wins and the conflict is recorded in **Corpus-vs-Repo Corrections slice-8 Inherits**.

---

## Executive Summary

slice-8 is the feature's close-out. Three rows remain, and they are three different
kinds of work: one builds the load-and-refresh contract the design has specified
since slice-1 but the code has only half-shipped, one closes the last unreachable
UI state, and one is not buildable by an agent at all.

- **The refresh contract exists on paper and half-exists in code.** The
  two-phase CLI budgets are real and test-pinned
  (`REGISTRATION_TIMEOUT_MS = 10_000` / `STAKE_TIMEOUT_MS = 30_000`,
  `source/main/governance/GovernanceQueryService.ts:57-58`, pinned at
  `tests/jest/governance/GovernanceQueryService.spec.ts:472-477`), the
  stale-while-refresh demotion is real (`GovernanceStore.ts:352-356` keeps
  `Loaded` and retains `error` when a refresh fails with data on screen), and the
  retained-data banner branch is real (`DRepDirectory.tsx:228-229`). What is
  missing is the *visual* half of the contract: the ≤700 ms phase renders a
  centred `LoadingSpinner` instead of the skeleton list shared-design-tokens §6
  `:97` specifies, the refresh-failed banner is a hand-rolled `<div>` printing the
  raw main-process `error.message` instead of §6 `:100`'s copy, and the
  stale-with-spinner badge sits above the list instead of beside the
  "Last updated {time}" timestamp it is meant to annotate. **task-123 ships the
  visual contract and adds one observational scalar to the wire.**
- **Nothing in slice-8 becomes a second timeout authority.** The >10 s enforcement
  already lives in exactly one place — `_runCliQuery`'s `setTimeout`
  (`GovernanceQueryService.ts:382-390`) — and §6's "banner appears at 10 s" is
  satisfied *by construction*, because a phase-1 CLI timeout rejects at 10 s and
  the renderer never observes an in-flight request older than that. The
  `elapsedMs` field task-123 adds to the IPC payloads is **observational only**
  (D-3): it feeds snapshot age and the deferred task-166 latency measurement. The
  renderer gains **no timer of any kind** — neither a competing timeout nor a
  700 ms delay (D-9). Getting this wrong yields two clocks that disagree, which is
  the single most likely way this slice breaks something that currently works.
- **Selfnode is the last unreachable state, and it currently leaks.** The
  `SelfnodeCliUnsupported` error is produced in exactly one place
  (`GovernanceQueryService.ts:209-213`, thrown from `_assertQueryable()` *before*
  any `spawn`), it survives structured clone as a plain object
  (`source/main/ipc/governanceChannel.ts:32-45`), and `DRepDirectory.tsx:233-238`
  deliberately excludes it from the `noSync` fallback — so today it falls through
  to the generic `Failed` branch at `:253-269` and renders the raw main-process
  string *"DRep data is unavailable in selfnode mode. A synced node is required."*
  straight to the user. **task-124 replaces that path with the designed empty
  state**, using `DRepEmptyState`'s explicitly-named extension point (`:54-56`).
- **The selfnode badge is a directory-level state indicator, not a DRep status.**
  shared-design-tokens §1 `:16` files it in the status-badge table, but
  `DRepStatusBadge` takes `status: DRepStatus` where `DRepStatus = 'active' | 'inactive'`
  is a closed union that locked invariant 7 forbids widening. D-1 resolves this the
  same way anchor-2's D-7 resolved its unimplementable badge claim: mint a new
  message id, render plain markup inside the empty state, touch neither the union
  nor the badge component, and reconcile the design doc.
- **task-125 cannot be completed here and is not attempted.** It is locked
  `manual_execution` and is a listed stop condition (`prompt.md:190-195`, `:249`;
  `README.md:16`). It requires a synced node, a packaged build and real-device
  hardware-wallet confirmation — none of which exist in this environment. Its
  tracker description already names the deliverable an agent *can* produce: "a
  manual release-verification checklist". **That checklist is the whole autonomous
  output; executing it is the user's, and the row stays `pending`** (D-5, D-16).

**Zero npm dependencies are added, zero new IPC channels are opened, and zero new
components are added beyond the one skeleton list D-2 mandates.** slice-8 rides
`GOVERNANCE_DREP_LIST_CHANNEL` / `GOVERNANCE_DREP_STAKE_CHANNEL`
(`source/common/ipc/api.ts:662-668`) unchanged and extends the two variant unions
that ux-refinement built and left named for their owning slice.

## Problem Statement — Why Now

- **slice-8 is last, and release verification must be last.** The verification
  journey task-125 describes — browse → evaluate → select → delegate — traverses
  work from slice-1 (directory), slice-2/slice-3 (software and hardware
  delegation), slice-4 (detail view), slice-5 (cohort), slice-6 (search/show-all),
  slice-7 (favorites), cv-1/cv-2 (current vote) and anchor-1/anchor-2 (verified
  metadata). A checklist written before those rows closed would verify a product
  that later commits changed. Every one of task-125's four declared dependencies —
  task-115, task-116, task-121, task-122 — is `complete`, and so is every row on
  the journey.
- **The loading state the design specifies has never been built.** §6 `:97` and
  `drep-discovery-design.md:195` both say *full skeleton list* for the
  first-load phase. Grep for `skeleton` under
  `source/renderer/app/components/governance/` returns **zero hits**; the live
  render is `<LoadingSpinner /> + "Loading DRep data…"`
  (`DRepDirectory.tsx:240-248`). A spinner and a skeleton are not the same
  contract: the skeleton communicates *how much is coming* and holds layout so the
  list does not jump when 25 cards land. D-2 rules the skeleton in.
- **The refresh-failed banner prints main-process strings to users.** §6 `:100`
  specifies `"Couldn't refresh DRep data. {Retry}. Showing last successful
  snapshot from {time}."` and §9 `:184` gives it the id
  `governance.drepDirectory.error.refresh`. Neither exists in either locale. The
  live banner (`DRepDirectory.tsx:323-341`) renders
  `governance.drepDirectory.error` ("Could not load DRep data.") followed by
  `error.message` and `error.details` — CLI-shaped text, in English regardless of
  locale, with no mention that the list on screen is a retained snapshot. The
  correct component already exists with the correct extension point named in a
  comment: `DRepErrorBanner.tsx:14-16`.
- **Selfnode users see an error, not an explanation.** `empty.selfnode` is
  specified at §9 `:181`, is referenced by name in task-124's own tracker
  description, and exists in neither locale. `DRepEmptyState.tsx:54-55` says in so
  many words: *"the designed selfnode variant joins this union when its owning
  slice lands."* slice-8 is that slice.
- **Two named extension points are still open, and this is the last slice that
  can close them.** `DRepEmptyStateVariant` (`:56`) and `DRepErrorBannerVariant`
  (`:16`) each carry a comment promising a future variant. If slice-8 does not
  land them, those comments become permanent change-history in a codebase whose
  comment convention forbids exactly that.
- **The feature needs a durable, executable release gate.** `prompt.md:232` makes
  manual release verification one of only three ways a tracker row can ever earn
  `verified`. Without a written checklist, that gate has no artifact and the
  feature ships on assertion.

## Canonical Build Order

The tasks JSON listing order for slice-8 is **`123, 124, 125`** (objects at
`:753-769`, `:770-785`, `:786-803`). Dependencies verified per row — **every
dependency of every task is `complete`, so slice-8 has no intra-slice dependency
edges and the JSON order carries no ordering information:**

| # | task | `dependencies` (json) | all `complete`? | in-slice edges |
|---|---|---|---|---|
| 1 | task-123 | task-103 | yes | none |
| 2 | task-124 | task-103 | yes | none |
| 3 | task-125 | task-115, task-116, task-121, task-122 | yes | none |

**Canonical slice-8 build order (binding):**

```
123 → 124 → 125
```

This matches the JSON listing order, but for technical reasons rather than by
inheritance (D-6):

- **123 before 124** — both edit the same `switch (true)` in
  `DRepDirectory.renderContent()` (`:240-282`). task-123 replaces the `Loading`
  arm and the retained-data banner below it; task-124 inserts a new arm ahead of
  the `Failed` arm. Running 123 first means 124 inserts into a settled switch
  instead of rebasing onto one. Both also extend the Storybook state knob
  (`DRepDirectory.stories.tsx:240-290`), and the same argument applies.
- **123 before 124** on the wire, too — task-123 owns the single widening of
  `DRepListQueryPayload` / `DRepStakeQueryPayload` (S-3). task-124 consumes an
  already-widened contract and widens nothing.
- **125 last** — the checklist describes the end state of the directory. Written
  before 123 and 124 land, it would describe a spinner and an error string that
  the slice is in the middle of replacing.

## Per-Task Contract (interaction mode, scope, non-goals, dependencies)

**One slice-8 task is in the locked non-autonomous set.** The set names task-125
(`manual_execution`), the task-166 remainder (`manual_execution`), task-158
(event-driven standing guardrail) and the release-end `!!!` copy review
(user-owned). **task-125 is in this phase and is escalated accordingly.** The
other two rows had exactly four blocking questions between them — the selfnode
badge's union problem, the skeleton-vs-spinner reconciliation, the two-timer risk
and the stylelint debt — and all four were ruled by the user before planning as
D-1…D-4. With those resolved, **task-123 and task-124 are `autonomous`**.

| Task (build pos / json pos) | Mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-123** — Define refresh latency budget and stale-while-refresh visual contract (build 1 / json 1) | `autonomous` | **Owns the single widening of the DRep query payloads (S-3):** `DRepListQueryPayload` (`source/common/types/governance.types.ts:139-146`) and `DRepStakeQueryPayload` (`:148-153`) each gain one plain-`number` `elapsedMs` field, measured around `_doFetchDRepRegistrations` / `_doFetchDRepStake` (`GovernanceQueryService.ts:224-286`) and documented in the IPC contract comment block at `source/common/ipc/api.ts:656-660` (AC-1). **Owns the single widening of `DRepErrorBannerVariant` (S-1):** `'rankingUnavailable'` → `'rankingUnavailable' \| 'refreshFailed'`, deleting the guard comment at `DRepErrorBanner.tsx:14-15` as it does, with the §6 `:100` copy under the new key `governance.drepDirectory.error.refresh` (AC-2). Builds a real DRep-card skeleton list for the first-load phase (D-2) and renders it from the `Loading` arm of `DRepDirectory.renderContent()` (`:240-248`), replacing the centred spinner. Moves the stale-while-refresh spinner badge from `DRepDirectory.tsx:342-347` to sit beside "Last updated {time}" in `DRepDirectoryBanner.tsx:103-109` (D-8). Adds state-transition tests for fresh-load, stale-with-spinner and timeout (AC-3), reusing the `jest.useFakeTimers()` + `advanceTimersByTime` pattern at `GovernanceQueryService.spec.ts:451-470`. Adds `Skeleton` / `Timeout banner` keys to the Storybook state knob (D-15). | **No second timeout authority** (D-3): `_runCliQuery`'s `setTimeout` (`:382-390`) stays the only enforcement, `REGISTRATION_TIMEOUT_MS` / `STAKE_TIMEOUT_MS` (`:57-58`) are neither renamed nor re-derived, and **no renderer timer of any kind is added** — not a timeout, not a 700 ms delay (D-9). No main-process refresh-state machine: `GovernanceRefreshState` (`GovernanceStore.ts:62-68`) stays the single refresh-state authority and is extended in behaviour only, never replaced (D-10). No IPC consumer for `getLastSuccessfulData()` (`:202-204`) — it stays unconsumed (D-12). No probe or warm-up CLI query to measure latency (invariant 3). No change to the two-phase split, the route-scoped fetch trigger (`DRepDirectoryPage.tsx:32-48`) or the soft sync banner (`DRepDirectory.tsx:392-417`). No re-derivation of the provisional 30 s stake budget from a dev-shell measurement — that is the deferred task-166 remainder. The no-cached-data `Failed` arm (`:253-269`) keeps its existing copy (D-7). No stylelint sweep (D-4). | task-103 ✔ |
| **task-124** — Selfnode CLI-unsupported empty-state copy and IPC payload (build 2 / json 2) | `autonomous` | **Owns the single widening of `DRepEmptyStateVariant` (S-2):** `'noSync' \| 'noResults' \| 'noFavorites'` → `+ 'selfnode'` (`DRepEmptyState.tsx:56`), deleting the guard comment at `:54-55`. Adds the `selfnode` early-return body in the file's established per-variant shape (`:73-129`), carrying the §9 `:181` copy under `governance.drepDirectory.empty.selfnode` **plus a plain-markup unavailability badge** — icon and textual label, never colour alone (§1 `:18`) — under the newly minted `governance.drepDirectory.status.selfnodeUnavailable` (D-1). Inserts a selfnode arm into `DRepDirectory.renderContent()` **ahead of** the `Failed` arm (`:253`) so the raw main-process message can never render, and reconciles `showNoSyncFallback` (`:233-238`) with the new arm. Adds a Jest test proving no partial directory and no CLI spawn on the selfnode path (AC-1), a Storybook `Selfnode unavailable` state-knob key and discrete story (AC-2, D-15), and the two design-doc reconciliations in **Doc Reconciliations slice-8 Owns**. | **`DRepStatus` gains no member and `DRepStatusBadge` is not touched** (invariant 7, D-1) — the badge is directory-level state markup inside the empty state, not a per-DRep status. **No IPC payload change** despite the task title: `GovernanceQueryErrorType.SelfnodeCliUnsupported` already exists (`governance.types.ts:157-166`), is already produced (`GovernanceQueryService.ts:209-213`) and already reaches the renderer intact (`governanceChannel.ts:32-45`) — the wire is complete and task-123 owns the only payload widening in the slice (C-2, S-3). No change to the `_assertQueryable()` throw site or its message. No partial list, no cached-data fallback and no retained snapshot on the selfnode path (invariant 6). No suppression or duplication of the syncing banner (`:392-417`). No mount-guard rewrite in `DRepDirectoryPage.tsx` (D-14). No stylelint sweep of `DRepEmptyState.scss` (D-4). | task-103 ✔ |
| **task-125** — Release verification: browse → evaluate → select → delegate without external portals (build 3 / json 3) | **`manual_execution`** — **user-owned stop condition. Never relabel this autonomous.** | **The only autonomous deliverable is the checklist document**, at `.agent/plans/governance/drep-discovery/release-verification-checklist.md` — inside the row's declared `targetPath` and named by the row's own description ("a manual release-verification checklist"). Its required contract is specified in **Release Verification Checklist Contract (task-125)** below. It lands under one commit, `docs(gov): task-125 …`, like any other task. | **Execution is the user's, and the row stays `pending`** (D-5, D-16). No agent may run it, simulate it, or infer its result: it requires a synced node, a **packaged** build (not the dev shell — C-5), and **real-device** hardware-wallet confirmation. No promotion of this row to `in_progress`, `complete` or `verified` by any agent. No re-added provenance rider (`cardano-cli` / `cardano-node` / `LedgerDB.Backend` capture) — considered and dropped at `research/ux-refinement-sync-and-load-research.md:74`. No e2e/Cucumber automation of the journey — there is no e2e in v1. No new tracker task. | task-115 ✔, task-116 ✔, task-121 ✔, task-122 ✔ |

## Acceptance Criteria (verbatim from governance-drep-discovery-plan-tasks.json)

Quoted exactly as the tracker holds them, in canonical build order. Annotations
marked **[D-n]** re-scope or correct a criterion and are binding on the guide
author; the quoted text itself is never edited.

### task-123 — Define refresh latency budget and stale-while-refresh visual contract (build 1, json position 1; json `:753-769`)

*(`acceptanceCriteria` array at `:764-768`)*

1. "Timing budget documented in the IPC contract and the shared design tokens reference."
   **[D-11]** The IPC-contract half is the comment block above the governance
   channel triads at `source/common/ipc/api.ts:656-660` plus the doc comments on
   the two `elapsedMs` fields in `governance.types.ts`. The shared-design-tokens
   half is **already satisfied** — §6 `:95-102` is the budget table, and it is
   already cited by name in the service's own doc comment
   (`GovernanceQueryService.ts:52-56`). Verify and record; **do not add a second
   budget table to `shared-design-tokens.md`.**
2. "Renderer banner copy matches shared-design-tokens §6."
   **[D-7]** Scoped to the **retained-data** refresh banner (`showErrorBanner`,
   `DRepDirectory.tsx:228-229`, rendered `:323-341`), which becomes
   `<DRepErrorBanner variant="refreshFailed" …>` carrying §6 `:100` verbatim. The
   no-cached-data `Failed` arm (`:253-269`) is explicitly **out of scope**: §6's
   sentence promises "the last successful snapshot", which is false when there
   has never been one.
3. "Tests verify state transitions: fresh-load, stale-with-spinner, and timeout banner."
   **[D-9]** "Timeout banner" is asserted from the `GovernanceQueryErrorType.Timeout`
   error arriving with retained data present — **not** from a renderer clock.
   Timer manipulation belongs in the main-process suite
   (`GovernanceQueryService.spec.ts:451-470`, `:479-504`), not in the component
   suite.

### task-124 — Selfnode CLI-unsupported empty-state copy and IPC payload (build 2, json position 2; json `:770-785`)

*(`acceptanceCriteria` array at `:781-784`)*

1. "Selfnode cluster shows a graceful empty state, never a partial directory."
   **[D-1]** "Graceful empty state" = `DRepEmptyState variant="selfnode"` carrying
   the §9 `:181` copy **and** the §1 `:16` unavailability badge as plain markup
   inside it. The "never a partial directory" half is structurally guaranteed:
   `_assertQueryable()` throws before any `spawn` (`GovernanceQueryService.ts:225`),
   so no rows ever exist to be partially rendered — the test pins that property
   rather than asserting an emptied list.
2. "Covered by a Storybook story and at least one Jest test."
   **[D-15]** The story is a new `Selfnode unavailable` key in the existing
   `DIRECTORY_STATE_OPTIONS` / `resolveDirectoryState` knob
   (`DRepDirectory.stories.tsx:240-290`) plus one discrete story — **not** a
   parallel decorator, **not** a local `IntlProvider`, **not** per-locale variants.

### task-125 — Release verification: browse → evaluate → select → delegate without external portals (build 3, json position 3; json `:786-803`)

*(`acceptanceCriteria` array at `:800-802`)*

1. "Release verification confirms users can complete browse -> evaluate -> select -> delegate without external portals on a synced node."
   **[D-5]** **Not dischargeable in this environment and not attempted.** No agent
   may mark this criterion satisfied. The autonomous deliverable is the checklist
   that makes the criterion executable; the criterion itself is closed only by the
   user running that checklist on a synced node with a packaged build and a real
   hardware device. The row stays `pending` at slice close.

## Planning Decisions (binding, as applied)

**D-1 … D-5 were ruled by the user before planning and are reproduced verbatim in
substance, with their rationale recorded so they are durable.** D-6 onward are
this PRD's own rulings, made because the grounding brief left them open and they
are not safe for an implementer to decide mid-edit.

### D-1 — The selfnode badge is a new message id rendered as plain markup inside the empty state; the `DRepStatus` union is not widened. *(user-ruled)*

shared-design-tokens §1 `:16` lists a "Selfnode / CLI unsupported" row in the
status-badge table with labels *"DRep data unavailable on selfnode"* / *"DRepデータ利用不可"*
and `--badge-disabled-bg` tokens, but §9 `:161-222` gives it **no message id**, and
the component that would render it takes a closed union:
`DRepStatusBadge.tsx:20-29` is an exhaustive `Record<DRepStatus, string>` over
`DRepStatus = 'active' | 'inactive'` (`governance.types.ts:35`). Locked invariant 7
forbids widening that union — it is the canonical on-chain status and selfnode is
not an on-chain status of any DRep.

**Ruling.** Mint `governance.drepDirectory.status.selfnodeUnavailable` (shape-consistent
with the existing `governance.drepDirectory.status.active` / `.inactive` at both
locales' `:387-388`) and render the badge as plain markup — icon plus textual
label, per §1 `:18`'s "colour must never be the sole indicator" — **inside** the
`DRepEmptyState` `selfnode` variant. `DRepStatusBadge` is not imported, not
modified, and gains no variant. This is the same resolution class as anchor-2's
D-7: when a design doc asks for a badge the canonical union cannot express, the
doc is reconciled and the union stands.

**Rationale.** The selfnode indicator describes the *directory's* availability,
not a DRep's status; there is no DRep to attach it to, because the query threw
before it ran. Filing it in the status-badge table is a documentation artefact,
not a component requirement.

### D-2 — task-123 builds a real DRep-card skeleton list; the existing spinner is not sufficient. *(user-ruled)*

§6 `:97` ("full skeleton list") and `drep-discovery-design.md:195` ("Full skeleton
list, banner visible, refresh button disabled") both specify a skeleton. The live
first-load render is a centred `LoadingSpinner` with "Loading DRep data…"
(`DRepDirectory.tsx:240-248`), and no skeleton component exists anywhere under
`source/renderer/app/components/governance/`.

**Ruling.** Build the skeleton list, matching §6 as written. It is a new component
under `components/governance/drep-directory/`, mirroring the real card's block
structure (`DRepCard.tsx`) and the list's page size (`CARDS_PER_PAGE = 25`,
`DRepDirectoryList.tsx:14`) closely enough that the layout does not jump when data
lands — the guide author fixes the exact placeholder count and markup.

**This is larger than the tracker's 4-hour estimate for task-123, and that is
accepted.** The estimate is not amended, the design doc is not amended to describe
a spinner, and the work is not silently downgraded. The skeleton is a static,
data-free render: it holds no state, takes no props beyond a count, and runs no
timer (D-9).

### D-3 — The main process stays the single timeout authority; `elapsedMs` is observational. *(user-ruled)*

`_runCliQuery`'s `setTimeout` (`GovernanceQueryService.ts:382-390`) is the only
timeout enforcement in the feature: it `SIGTERM`s the child and rejects with
`GovernanceQueryErrorType.Timeout` after `timeoutMs`, cleared at `:373`/`:393`.
Phase 1 passes `REGISTRATION_TIMEOUT_MS = 10_000`, so a phase-1 hang rejects at
10 s and the renderer **cannot** observe an in-flight registration request older
than that.

**Ruling.** That enforcement is not duplicated, moved, wrapped or re-derived. The
`elapsedMs` task-123 adds to the IPC payloads is **purely observational**: it
records how long the completed query actually took, for snapshot age and for the
deferred task-166 latency measurement. It drives the renderer's skeleton →
stale-with-spinner visual progression and nothing else. **The renderer must not
run a competing timeout timer.**

**As applied — the renderer deliberately does not observe `elapsedMs`.** The
skeleton → stale-with-spinner progression is owned end-to-end by
`GovernanceRefreshState` (`Loading` with no cached data → skeleton; `Refreshing`
with data on screen → stale-with-spinner), because a *completed* query's duration
cannot drive an *in-flight* transition. `GovernanceStore` therefore stores no
`elapsedMs` and no component reads it: it is snapshot-age and deferred
latency-measurement telemetry only. The ruling's operative half — no second
clock anywhere in the renderer — is what the shipped code enforces (NFR-1), and
this clause exists so no tracker `statusReason` claims a consumer that does not
exist.

**Consequence for §6's "banner appears at 10 s".** That timing is satisfied by
construction, not by a clock: the CLI rejection *arrives* at 10 s. Likewise
"banner appears at 30 s" for ranking-unavailable is already satisfied by
`STAKE_TIMEOUT_MS`. Neither needs renderer-side scheduling.

### D-4 — The 118 pre-existing stylelint errors are out of scope and recorded, not fixed. *(user-ruled)*

`yarn stylelint` is **red at HEAD `0cdcab581` with 118 errors**, every one of them
`order/properties-alphabetical-order` and every one of them in this feature's own
governance SCSS — including both files slice-8 is most likely to touch:
`_shared/DRepEmptyState.scss` (3 errors) and `_shared/DRepErrorBanner.scss` (4).

**Ruling.** slice-8 does **not** fix them and adds **no** cleanup task to the
tracker. Keep the lines you touch clean, leave the rest alone. The 118 errors are
recorded in `research/slice-8-findings.md` as a **pre-merge cleanup item the user
owns** (D-18). No `stylelint --fix` sweep, not even on the two touched files —
a sweep would put dozens of unrelated reordered declarations inside a task commit.
See D-13 for the one thing this *does* require of new code.

### D-5 — task-125 is not autonomously buildable; the row stays `pending`. *(user-ruled)*

`prompt.md:190-195` and `README.md:16` both lock task-125 as `manual_execution`,
user-owned, "Never relabel these autonomous"; `prompt.md:249` also lists it as a
stop condition. Its acceptance requires a synced node, and its journey includes
real-device hardware-wallet confirmation.

**Ruling.** The only autonomous deliverable is the release-verification
**checklist document**. Executing it is the user's. The tracker row stays
`pending` through slice close, and no agent may promote it. `prompt.md:232` makes
manual release verification one of only three routes to `verified` in this
tracker — so this row is precisely the one that must not be self-certified.

### D-6 — Build order is `123 → 124 → 125`, on technical grounds, not by inheritance from the JSON.

Both autonomous rows edit `DRepDirectory.renderContent()`'s `switch (true)`
(`:240-282`) and both extend the Storybook state knob (`:240-290`). task-123's
edits are structural (one arm replaced, one banner block replaced, one badge
moved out); task-124's is a single inserted arm. Landing the structural edit first
means the insertion goes into a settled file. task-123 also owns the payload
widening (S-3) that task-124 must not re-open. task-125's checklist describes the
end state and is written after it exists. See **Canonical Build Order**.

### D-7 — The `refreshFailed` banner replaces only the retained-data banner; the no-data `Failed` arm keeps its copy.

§6 `:100`'s copy — *"Couldn't refresh DRep data. {Retry}. Showing last successful
snapshot from {time}."* — asserts that a snapshot is on screen. That is true on
the `showErrorBanner` path (`DRepDirectory.tsx:228-229`), which fires only when
`showAllList.length > 0`, and false on the `Failed` arm (`:253-269`), which fires
only when there is no retained data at all.

**Ruling.** task-123 replaces the hand-rolled banner at `:323-341` with
`<DRepErrorBanner variant="refreshFailed" …>`. The `Failed` arm at `:253-269` is
**untouched** and keeps `governance.drepDirectory.error`. Rendering the snapshot
sentence with no snapshot would be a false statement to the user, and rewriting
that arm is scope task-123's ACs do not ask for.

**Consequence on the `{Retry}` slot.** `governance.drepDirectory.retry`
(en-US/ja-JP `:376`) already exists — **no new retry key is minted.** The rich
`{Retry}` slot embeds a `Link`, following the in-repo precedent at
`DRepEmptyState.tsx:77-99`. To avoid re-declaring an existing id in a second
`defineMessages` block, `DRepErrorBanner` receives the already-formatted retry
label and the handler as props and declares only its own new `refreshFailed`
message. The `{time}` slot formats `lastFetchedAt` relatively, mirroring
`DRepDirectoryBanner.tsx:103-109`; the banner renders only where retained data
exists, so `lastFetchedAt` is non-null on that path by construction.

### D-8 — The stale-while-refresh spinner moves beside the "Last updated {time}" timestamp.

§6 `:99` specifies "small spinner badge next to 'Last updated {time}' timestamp,
list interactive", and `drep-discovery-design.md:198` repeats it. The live badge
is a separate block above the list (`DRepDirectory.tsx:342-347`) while the
timestamp lives in `DRepDirectoryBanner.tsx:103-109`.

**Ruling.** Implement the design rather than reconcile it away. task-123's title
names the deliverable "stale-while-refresh **visual contract**", and placement is
the contract: a spinner beside a timestamp reads as *"this timestamp is being
updated"*, which is the entire point of a stale-while-refresh affordance. The move
is cheap — `DRepDirectoryBanner` already receives `isRefreshing` (`:99`) — and it
is a **deletion** in `DRepDirectory.tsx` plus a small render addition in the
banner. `governance.drepDirectory.refreshing` ("Refreshing…", `:375`) is retained
as the badge's accessible label so no copy is lost, and the existing assertion in
`DRepDirectory.spec.tsx` moves to `DRepDirectoryBanner.spec.tsx` rather than being
deleted.

### D-9 — The skeleton renders immediately on `Loading`; the renderer gains no timer at all.

D-3 forbids a competing *timeout* timer. This decision closes the remaining hole:
whether the renderer needs a 700 ms *delay* timer before painting the skeleton.

**Ruling. No.** The skeleton renders immediately when `refreshState === Loading`
with no cached data. §6's "≤700 ms before skeleton" is a **paint-latency budget** —
at most 700 ms may elapse before the skeleton is on screen — not a delay to
schedule. A static, data-free render satisfies it by construction. Introducing a
`setTimeout` to *withhold* the skeleton for 700 ms would both violate the budget's
intent and reintroduce exactly the second clock D-3 exists to prevent.

**Therefore slice-8 adds zero `setTimeout` / `setInterval` calls to the renderer.**
That is a checkable property, and the guide should make it one.

### D-10 — The wire carries elapsed time only; refresh state stays renderer-owned.

task-123's description says "expose elapsed time **and refresh state** through the
governance IPC payloads". The renderer already owns an authoritative refresh-state
machine — `GovernanceRefreshState { Idle, Loading, Refreshing, Loaded, Failed }`
(`GovernanceStore.ts:62-68`), driven by `fetchDRepList` (`:311-362`) — and the
main process has no notion of "stale-while-refresh": it has in-flight dedup
(`:164-178`, `:187-199`) and a retained last-success (`:202-204`), nothing more.

**Ruling.** The payloads gain `elapsedMs` and nothing else. The "refresh state"
half of the description is discharged by the **existing** renderer enum, which
task-123 extends in behaviour and never replaces, duplicates or mirrors main-side.
Two refresh-state machines on two sides of an IPC boundary would disagree the
first time a request was deduplicated.

**Consequence, mirroring D-3:** the visual progression reads `refreshState` alone.
`elapsedMs` crosses the wire, lands in the snapshot file, and is read by **no**
renderer consumer in this slice.

### D-11 — `elapsedMs` rides the existing payloads, not a sidecar; the snapshot-file consequence is accepted.

Widening `DRepListQueryPayload` widens `Logs/pub/DRep-state-snapshot.json`, because
`governanceChannel.ts:56` hands the payload to `logDRepStateSnapshot`, which
serialises it wholesale (`setupLogging.ts:183-204`, `data: payload as unknown as …`)
and deliberately bypasses `filterLogData`.

**Ruling.** Widen the existing payloads anyway. `elapsedMs` is a non-identifying
scalar in milliseconds — it names no DRep, encodes no bech32 string and reveals no
vote — so it is exactly the class of telemetry the snapshot exception already
covers, and it is *useful* there for the deferred task-166 latency work. A sidecar
channel or a parallel payload type would add a second transport for one number,
against invariant 9 (smallest truthful change, reuse existing seams).

**Obligations this creates on task-123:** re-run
`tests/jest/governance/logDRepStateSnapshot.spec.ts`, and re-assert the task-111
floor (`tests/jest/security/governance-sanitization.spec.ts`) as every slice must.
Per invariant 4, `elapsedMs` is a plain `number` of milliseconds and is never
conflated with a lovelace field or routed through the decimal-string convention.

### D-12 — `getLastSuccessfulData()` stays unconsumed by IPC.

`GovernanceQueryService.getLastSuccessfulData()` (`:202-204`) retains the last
successful `DRepListQueryPayload` and has no IPC consumer today. §6's "Showing
last successful snapshot from {time}" leg looks like its natural customer.

**Ruling.** Do not wire it. The renderer already holds the retained snapshot the
user is looking at: `fetchDRepList`'s catch demotes a failed refresh to `Loaded`
while keeping `drepList`, `lastFetchedAt` and `error` (`GovernanceStore.ts:352-356`).
The banner's `{time}` must name the timestamp of *the data on screen*, and the
store is the only place that knows what is on screen. Serving it from a second,
main-side copy would create two sources of truth for one sentence — and they would
diverge the moment a renderer-side reset or a store re-hydration happened without
a service reset.

**Recorded so no implementer "obviously" wires it.** If a later slice does wire
it, `reset()` (`:146-153`) must clear it in the same edit — it already does.

### D-13 — New SCSS declarations land alphabetically; the new skeleton stylesheet is stylelint-clean at birth.

D-4 keeps the 118 pre-existing errors out of scope, but that is a floor, not a
licence.

**Ruling.** Every declaration slice-8 *adds* to an existing governance stylesheet
goes in alphabetical order, so the error count does not grow past 118. The
skeleton's **new** `.scss` file is created stylelint-clean — a new file has no
pre-existing debt to inherit, and shipping it dirty would be adding to the pile
this slice is explicitly not cleaning. The guide should verify the new file alone
with `node_modules/.bin/stylelint <path>` (expect 0) and record the repo-wide
count, which falls to 111 only because two dead selector blocks leave with the
markup that used them — never because a declaration was reordered.

### D-14 — The selfnode path needs no mount guard; the no-spawn property is pinned by a test instead.

`DRepDirectoryPage.componentDidMount` calls `refresh()` when `refreshState` is
`Idle` **or** `Failed` (`:32-38`), so a selfnode user re-fires the query on every
mount. That looks like a retry loop.

**Ruling.** No guard is added. `_doFetchDRepRegistrations` calls `_assertQueryable()`
as its **first statement** (`GovernanceQueryService.ts:224-225`), which throws
before any `Promise.all` and therefore before any `spawn` — so the "retry" costs
one synchronous throw and issues **no CLI invocation**, satisfying invariant 3
without special-casing. Adding a renderer-side selfnode guard would put
node-capability knowledge in a container that should not have it.

**Obligation on task-124:** pin the property rather than the absence of a symptom —
assert that a selfnode refresh performs no `spawn` and leaves the directory in the
selfnode empty state across repeated mounts.

### D-15 — Storybook coverage extends the existing state knob; no parallel stories, no local providers.

`DRepDirectory.stories.tsx:240-290` maps one `DIRECTORY_STATE_OPTIONS` knob key
through `resolveDirectoryState(stateKey)` to a `(refreshState, entries, error)`
triple; current keys are `Loaded | Empty | Loading | Refreshing | Error`.

**Ruling.** task-123 adds the keys its states need (first-load skeleton and the
timeout/refresh-failed banner) and task-124 adds `Selfnode unavailable`, each with
one discrete story in the file's sentence-case, em-dash-for-sub-variants
convention. **No story wraps itself in an `IntlProvider` and no per-locale
`(en-US)`/`(ja-JP)` variants are created** — `storybook/preview.tsx`'s global
`StoryWrapper` decorator and the English/Japanese toggle own locale
(`DRepDirectory.stories.tsx:292-296`). The integrated `Connected flow` story
(`:303-305`, exemplar `storybook/stories/voting/Governance.stories.tsx:305`) must
still run after both tasks. `yarn storybook:build` is **green at HEAD** and is the
only mechanical check on stories here, so it is run, not waived (C-6).

### D-16 — The release-verification checklist is a standalone sibling doc; task-125 gets a commit but no status change.

task-125's `targetPath` is `.agent/plans/governance/drep-discovery/` — the feature
root, not `task-plans/` — and its description names the artefact directly: "a
manual release-verification checklist".

**Ruling.** The checklist lives at
`.agent/plans/governance/drep-discovery/release-verification-checklist.md`, as its
own document. It is not a section of this PRD: the user executes it away from the
planning corpus, on a different machine, possibly for a later release, and it must
outlive slice-8. This PRD carries its **contract** (below), not its steps.

**Commit and status.** The checklist lands under exactly one commit,
`docs(gov): task-125 <short imperative summary>`, matching the one-commit-per-task
rule. **The tracker row stays `pending`** — the commit delivers the instrument,
not the verification. task-125's `statusReason` (if written at all) must say so
plainly and must not read as partial completion of the acceptance criterion.

### D-17 — slice-8 adds no `auditSummary` to its tracker phase object.

Only `slice-1` and `anchor-2` carry an `auditSummary`; the `slice-8` phase object
has none, and `prompt.md:222` refreshes one only "if one exists".

**Ruling.** Do not add one. anchor-2's shape is
`statusCounts: { complete, partial, verified }` — it has no slot for a row that is
`pending` **by design**, and slice-8 ends with exactly that (task-125, D-5).
Inventing a fourth key on the feature's last slice would drift a convention right
where it is most likely to be read as authoritative. The closure record lives in
this PRD's **Final Outcome** and in `research/slice-8-findings.md` instead.

### D-18 — `research/slice-8-findings.md` is mandatory, and no closed slice's tracker text is edited.

D-4 requires the 118 stylelint errors to be recorded in a slice findings note, so
the file is not optional for this slice. It is created no later than task-124's
commit (the task whose two most likely files are among the failing ones) and
appended at close.

**It must record, at minimum:** (a) the 118 pre-existing `yarn stylelint` errors
as a user-owned pre-merge cleanup item, with the failing-file list and the fact
that all 118 are `order/properties-alphabetical-order` in this feature's own SCSS;
(b) the corrected gate premises from C-6 — `yarn storybook:build` and
`yarn compile` are **green** at HEAD, contradicting anchor-2's tracker text; and
(c) whatever slice-8 itself discovers.

**Ruling on the stale claim.** slice-8 does **not** edit anchor-2's closed
`statusReason` to correct it. A closed slice's tracker text is a record of what
that slice measured; correcting it retroactively destroys the audit trail. The
correction is recorded in the findings note and in **Corpus-vs-Repo Corrections**
below, where a future reader will find it.

## Cross-Task Seam Contracts

### S-1 — `DRepErrorBannerVariant` is widened exactly once, by task-123.

`source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx:14-16`
today:

```ts
// Only the rankingUnavailable variant ships for now; the designed
// refresh-failed variant joins this union when its owning slice lands.
export type DRepErrorBannerVariant = 'rankingUnavailable';
```

task-123 makes it `'rankingUnavailable' | 'refreshFailed'`, adds the matching
`messageByVariant` entry (`:24-26`) and the new `defineMessages` entry, and
**deletes the two-line guard comment** — leaving it would be change-history in a
codebase whose comment convention forbids exactly that. The existing
`rankingUnavailable` render path, its `role="status"` wrapper and its inline
warning-triangle svg (`:29-46`) are reused unchanged. task-124 does not touch this
file.

### S-2 — `DRepEmptyStateVariant` is widened exactly once, by task-124.

`DRepEmptyState.tsx:54-56` today:

```ts
// noSync, noResults and noFavorites ship for now; the designed selfnode
// variant joins this union when its owning slice lands.
export type DRepEmptyStateVariant = 'noSync' | 'noResults' | 'noFavorites';
```

task-124 adds `'selfnode'`, adds one early-return body in the file's established
per-variant shape (`:73-129`; `noSync` remains the fall-through default at
`:125-129`), adds the two new messages to the `defineMessages` block (`:12-52`),
and **deletes the two-line guard comment**. task-123 does not touch this file.

### S-3 — `DRepListQueryPayload` and `DRepStakeQueryPayload` gain `elapsedMs` exactly once, by task-123.

`source/common/types/governance.types.ts:139-146` and `:148-153`. One plain-`number`
field each, doc-commented in the file's existing style, measured around the private
`_doFetch*` methods (`GovernanceQueryService.ts:224-286`) so the in-flight dedup
wrappers (`:164-178`, `:187-199`) are unchanged. `GovernanceQueryErrorType`
(`:157-166`) already carries both `Timeout` and `SelfnodeCliUnsupported` — **no new
enum member is added by either task.** `source/renderer/app/ipc/governanceChannel.ts:16-29`
needs no edit: `RendererIpcChannel`'s types flow from `api.ts`. task-124 consumes a
finished wire and changes nothing on it (C-2).

### S-4 — `DRepDirectory.renderContent()` is edited by both tasks, in order, with disjoint arms.

The `switch (true)` at `DRepDirectory.tsx:240-282`:

| arm | line | owner | change |
|---|---|---|---|
| `refreshState === Loading` | `:242-248` | **task-123** | spinner block → skeleton list (D-2) |
| `showNoSyncFallback` | `:250-251` | **task-124** | reconcile the predicate (`:233-238`) with the new selfnode arm |
| *(new)* selfnode | inserted before `:253` | **task-124** | `<DRepEmptyState variant="selfnode" />` |
| `refreshState === Failed` | `:253-269` | *neither* | untouched (D-7) |
| `showAllList.length === 0 && Loaded` | `:271-282` | *neither* | untouched |

Below the switch, task-123 alone owns the retained-data banner (`:323-341` → S-1)
and the refreshing badge move (`:342-347` → D-8). The `votingPowerState === Failed`
banner (`:348-350`) is the existing correct precedent and is untouched.

### S-5 — The Storybook state knob is the single state-selection seam.

`DIRECTORY_STATE_OPTIONS` / `resolveDirectoryState` (`DRepDirectory.stories.tsx:240-290`)
is extended additively by each task (D-15). Neither task adds a decorator, a
provider, a second `storiesOf` block, or a per-locale export. `_utils/fixtures.ts`
gains fixtures only if the new states need them — the selfnode and skeleton states
need **no** DRep entries at all.

## Doc Reconciliations slice-8 Owns

Two edits, both in task-124's commit, both minimal. Everything else in this
section is verify-and-record with **no edit**.

### Edit 1 (task-124, D-1) — file the selfnode indicator correctly under shared-design-tokens §1

The §1 table at `designs/shared-design-tokens.md:9-16` lists
"Selfnode / CLI unsupported" among per-DRep status badges. Append one clarifying
sentence to the paragraph block below the table (near the existing contrast rule
at `:18` and the status-grounding note at `:20`) stating that the selfnode row is a
**directory-level state indicator rendered inside the selfnode empty state**, not a
`DRepStatus` member and not a `DRepStatusBadge` variant, and naming its message id
`governance.drepDirectory.status.selfnodeUnavailable`. **Do not delete the table
row** — its labels and tokens are the contract for the badge's copy and styling.

### Edit 2 (task-124, D-1) — add the minted id to the §9 microcopy table

§9's table (`:161-222`) lists `governance.drepDirectory.empty.selfnode` at `:181`
but no id for the §1 selfnode label. Add one row for
`governance.drepDirectory.status.selfnodeUnavailable` with the en-US label from §1
`:16` ("DRep data unavailable on selfnode"), placed with the other
`governance.drepDirectory.status.*` rows so the table stays grouped. One row, one
insertion.

### Verify-and-record (no edit)

- **§6 `:95-106` is already the budget table AC-1 asks for**, and
  `GovernanceQueryService.ts:52-56` already cites it by name. Confirm and record;
  do not add a second table (D-11 annotation on AC-1).
- **`drep-discovery-design.md:195-203`'s state rows already describe every state
  slice-8 builds** — full skeleton list, spinner beside the timestamp, error banner
  with retained list, `DRepEmptyState selfnode`. slice-8 makes the code match the
  doc; the doc needs no change.
- **`drep-discovery-design.md:185-186`'s component map already names
  `DRepEmptyState.tsx` with a `selfnode` variant and `DRepErrorBanner.tsx` with a
  refresh-failed variant.** Both are satisfied by S-1/S-2 without a doc edit.
- **`drep-discovery-design.md:195` also says the refresh button is disabled during
  first load**, while `DRepDirectoryBanner.tsx:99` disables it only while
  `isRefreshing`. During first load `refreshState === Loading`, so the button is
  live. **Out of scope for slice-8** — no AC asks for it and it is a one-line prop
  change nobody has requested. Record it in `research/slice-8-findings.md` as an
  unclosed design/code divergence, do not fix it, and do not edit the doc.
- **`drep-discovery-design.md:213-214`'s empty-state copy boundary** — the
  directory's empty-state copy must stay scoped to registered DReps and must never
  suggest the directory is where to find `Abstain` / `No Confidence`. §9 `:181`'s
  selfnode copy already complies. Confirm at implementation, record, no edit.

## User Stories

### US-8.1 — See that the directory is loading, not frozen

**As a** Daedalus user opening the DRep directory on a synced node,
**I want to** see the shape of the list arriving rather than a lone spinner,
**So that** I know how much content is coming and the page does not jump when it lands.

**Acceptance:**
- The skeleton list paints immediately on first load with no cached data.
- Its block structure matches the real card closely enough that no layout shift
  occurs when entries arrive.
- No timer withholds it, and no timer times it out (D-9).

### US-8.2 — Keep using the directory while it refreshes

**As a** user who hit Refresh or re-entered the route with data already loaded,
**I want to** keep browsing the list I already have, with a small spinner beside the
"Last updated" time,
**So that** I can tell the timestamp is being updated without losing my place.

**Acceptance:**
- The list stays interactive throughout (already true — `GovernanceStore.ts:352-356`).
- The spinner badge sits beside the "Last updated {time}" line in the directory
  header (D-8), with `governance.drepDirectory.refreshing` as its accessible label.

### US-8.3 — Be told plainly when a refresh fails, in my own language

**As a** user whose refresh failed while a previous snapshot is on screen,
**I want to** read "Couldn't refresh DRep data. Retry. Showing last successful
snapshot from 3 minutes ago." instead of a CLI error string,
**So that** I know the data is stale rather than wrong, and I know how to retry.

**Acceptance:**
- The retained-data banner carries §6 `:100`'s copy in both locales.
- `error.message` / `error.details` no longer render on that path.
- The `{Retry}` link re-triggers the existing `onRefresh` handler.
- The `{time}` slot names the timestamp of the data actually on screen (D-12).

### US-8.4 — Understand why the directory is empty on a selfnode cluster

**As a** developer or tester running Daedalus against the selfnode cluster,
**I want to** see a clear "DRep directory data is unavailable on the selfnode
cluster" state with an unavailability badge,
**So that** I do not read it as a bug, a broken node, or an empty ledger.

**Acceptance:**
- The selfnode state replaces the entire list area — **never a partial directory**
  (invariant 6).
- No raw main-process error string is shown.
- Repeated route entry does not spawn a CLI process (D-14).

### US-8.5 — Verify the whole journey before release

**As the** release owner,
**I want** a written checklist that walks browse → evaluate → select → delegate on a
synced node with both wallet types and no external portal,
**So that** the feature ships against evidence rather than assertion.

**Acceptance:**
- The checklist exists, is executable by a human, and states its preconditions
  (synced node, packaged build, real hardware device).
- Every step has an unambiguous pass/fail observation.
- **Nothing in it is marked done by an agent** (D-5).

## Functional Requirements

| ID | Requirement | Owner |
|----|------------|-------|
| FR-1 | `DRepListQueryPayload` and `DRepStakeQueryPayload` each carry a plain-`number` `elapsedMs` measured around the phase's CLI work | task-123 |
| FR-2 | The refresh-latency budget is documented at the IPC contract (`api.ts:656-660`) and confirmed present in shared-design-tokens §6 | task-123 |
| FR-3 | A DRep-card skeleton list renders for first load with no cached data, replacing the centred spinner | task-123 |
| FR-4 | `DRepErrorBannerVariant` gains `'refreshFailed'`, carrying §6 `:100` copy under `governance.drepDirectory.error.refresh` | task-123 |
| FR-5 | The retained-data refresh banner uses `DRepErrorBanner` and no longer renders `error.message` / `error.details` | task-123 |
| FR-6 | The stale-while-refresh spinner badge renders beside the "Last updated {time}" line | task-123 |
| FR-7 | State-transition tests cover fresh-load, stale-with-spinner and timeout-banner | task-123 |
| FR-8 | `DRepEmptyStateVariant` gains `'selfnode'` with the §9 `:181` copy | task-124 |
| FR-9 | The selfnode empty state renders an unavailability badge as plain markup under `governance.drepDirectory.status.selfnodeUnavailable`, with an icon and a textual label | task-124 |
| FR-10 | `DRepDirectory.renderContent()` routes `SelfnodeCliUnsupported` to the selfnode empty state ahead of the generic `Failed` arm | task-124 |
| FR-11 | A Storybook state-knob key and discrete story exist for the selfnode state, and for task-123's new states | task-123, task-124 |
| FR-12 | A release-verification checklist document exists at `.agent/plans/governance/drep-discovery/release-verification-checklist.md` | task-125 |

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | slice-8 adds **zero** `setTimeout` / `setInterval` calls to the renderer (D-3, D-9) |
| NFR-2 | `_runCliQuery`'s timeout (`:382-390`) and the two budget constants (`:57-58`) are neither duplicated, renamed nor re-derived |
| NFR-3 | `elapsedMs` is a plain `number` of milliseconds — never a lovelace value, never a decimal string (invariant 4) |
| NFR-4 | No new IPC channel, no new npm dependency, no new `GovernanceQueryErrorType` member |
| NFR-5 | Every new en-US and ja-JP string keeps the leading `!!!` marker; every new key exists in **both** catalogs (invariant 5) |
| NFR-6 | New SCSS declarations are alphabetically ordered; the new skeleton stylesheet is stylelint-clean at birth; the repo-wide error count never rises above 118 and falls only as a side effect of deleting dead selector blocks with their markup — never by a `--fix` sweep (D-4, D-13) |
| NFR-7 | The task-111 sanitization floor (`tests/jest/security/governance-sanitization.spec.ts`) is re-asserted green in **every** slice-8 task |
| NFR-8 | Storybook stories use the global English/Japanese toggle only — no local `IntlProvider`, no per-locale variants (D-15) |
| NFR-9 | Formatting uses `node_modules/.bin/prettier --write <explicit paths>` on files the task created or edited; never `yarn prettier`, never the tasks tracker or the locale catalogs |
| NFR-10 | The skeleton renders no DRep data, holds no state, and takes no observable — it is a pure presentational component |

## Architecture: Data Flow (slice-8 delta)

```
  main                                        renderer
  ────                                        ────────
  GovernanceQueryService
   _doFetchDRepRegistrations()
     t0 = Date.now()
     ├─ query drep-state --all-dreps ──┐
     └─ query tip                      │  _runCliQuery(args, 10_000)
                                       │   setTimeout → Timeout        ◀── the ONLY
     elapsedMs = Date.now() - t0       │   (:382-390)                       timeout
                                       ▼                                    authority
   { dreps, fetchedAt, epoch, elapsedMs }                                   (D-3)
            │
            │ GOVERNANCE_DREP_LIST_CHANNEL  (unchanged triad, api.ts:662-668)
            │   ├──▶ logDRepStateSnapshot → Logs/pub/DRep-state-snapshot.json
            │        (elapsedMs rides along; non-identifying scalar — D-11)
            ▼
  GovernanceStore.fetchDRepList()
   refreshState: Loading (no data) | Refreshing (data)      ◀── single refresh-state
   success → Loaded, lastFetchedAt = payload.fetchedAt           authority (D-10)
   failure + retained data → Loaded, error kept  (:352-356)
   failure + no data       → Failed
            │
            ▼
  DRepDirectory.renderContent()      (S-4)
   Loading            → DRepDirectorySkeleton      ← NEW, immediate, no timer (D-2/D-9)
   selfnode error     → DRepEmptyState 'selfnode'  ← NEW arm, before Failed (task-124)
   Failed             → unchanged (D-7)
   Loaded + retained error
                      → DRepErrorBanner 'refreshFailed'  ← replaces inline div (S-1)
   Refreshing         → spinner badge now lives in DRepDirectoryBanner,
                        beside "Last updated {time}"      (D-8)
```

## i18n Key Inventory

**Three new keys, in both `source/renderer/app/i18n/locales/en-US.json` and
`ja-JP.json`.** All carry the leading `!!!` (invariant 5). The catalogs are
alphabetically sorted and **tool-managed — never prettier-format them.**

| Key | Owner | en-US (verbatim from the design contract) | Sorts after |
|---|---|---|---|
| `governance.drepDirectory.error.refresh` | task-123 | `!!!Couldn't refresh DRep data. {Retry}. Showing last successful snapshot from {time}.` — §9 `:184` | `governance.drepDirectory.error` (`:356`) |
| `governance.drepDirectory.empty.selfnode` | task-124 | `!!!DRep directory data is unavailable on the selfnode cluster.` — §9 `:181` | `governance.drepDirectory.empty.noSync` (`:355`) |
| `governance.drepDirectory.status.selfnodeUnavailable` | task-124 | `!!!DRep data unavailable on selfnode` — §1 `:16` | `governance.drepDirectory.status.inactive` (`:388`) |

**ja-JP.** §1 `:16` **locks** the selfnode badge's Japanese label: `!!!DRepデータ利用不可`.
The other two are slice-level placeholders, permitted by §9 `:159` and `:226`, and
carry `!!!` until the release-end manual copy review. Suggested placeholders, which
the guide author may refine within the placeholder rule:

- `governance.drepDirectory.empty.selfnode` → `!!!selfnodeクラスターではDRepディレクトリのデータを利用できません。`
- `governance.drepDirectory.error.refresh` → `!!!DRepデータを更新できませんでした。{Retry}。{time}時点の最後に成功したスナップショットを表示しています。`

**Keys deliberately reused, not minted:**

- `governance.drepDirectory.retry` (`:376`, "Retry" / "再試行") fills the
  `{Retry}` rich slot (D-7).
- `governance.drepDirectory.refreshing` (`:375`) becomes the relocated spinner
  badge's accessible label (D-8).
- `governance.drepDirectory.loading` (`:370`) becomes the skeleton list's
  accessible label — the skeleton mints no copy of its own.

**JA length note.** §9 `:224` names the refresh error banner specifically as
expanding 30–60% in JA/DE: it must allow **≥2 wrapped lines, reflow vertically and
never ellipsize.** That constrains `DRepErrorBanner.scss` — no fixed height, no
`text-overflow: ellipsis`, no single-line clamp on the new variant.

**Gate.** `yarn i18n:manage` must be run after the copy lands (it is a byte-identical
no-op at HEAD, so any diff it produces beyond the new keys is drift to inspect), and
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts` must pass — its
`:57-64` case fails any `governance.*` key missing `!!!` in **either** locale, and
`:39-46` fails any key present in only one.

## What Slice-8 Deliberately Does NOT Include

- ❌ Any second timeout authority, renderer clock, or 700 ms delay timer (D-3, D-9)
- ❌ A main-process refresh-state machine mirroring `GovernanceRefreshState` (D-10)
- ❌ An IPC consumer for `getLastSuccessfulData()` (D-12)
- ❌ Re-derivation of the provisional 30 s stake budget — that is the deferred
  task-166 remainder, and no dev-shell measurement may size a production timeout (C-5)
- ❌ Any widening of `DRepStatus`, or any change to `DRepStatusBadge` (invariant 7, D-1)
- ❌ A `retired` status, a `Retired` badge, or an "Excluded from default cohort"
  badge — deferred/unimplementable, settled in anchor-2 D-7
- ❌ The stylelint cleanup of the 118 pre-existing governance SCSS errors (D-4)
- ❌ The first-load refresh-button-disabled divergence at `drep-discovery-design.md:195`
  (recorded, not fixed)
- ❌ Any edit to a closed slice's tracker `statusReason` (D-18)
- ❌ An `auditSummary` on the slice-8 phase object (D-17)
- ❌ Execution of the release-verification journey, or any inference of its result (D-5)
- ❌ Automated e2e/Cucumber coverage of the journey — no e2e in v1
- ❌ Removal of any `!!!` marker — that is the release-end manual review (invariant 5)
- ❌ Per-DRep CLI invocations or a latency probe query (invariant 3)
- ❌ Any second delegation backend or auto-delegation (invariant 8)
- ❌ Any hosted explorer, indexer, GovTool, Koios or Blockfrost call (invariant 1)

## Docs / Designs / Research / Workflows / Skills Consulted

Sources marked **(via brief)** were read through the verified, line-anchored
grounding brief for this slice rather than opened directly; the brief re-verified
every anchor against the worktree at `0cdcab581`. Everything else was re-opened
during this planning pass.

- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json`
  — the `slice-8` phase object `:747-805` read directly and parsed: the three task
  rows, their descriptions, `targetPath`s, `dependencies` and `acceptanceCriteria`
  arrays, quoted verbatim above.
- `.agent/plans/governance/drep-discovery/prompt.md` **(via brief)** — the locked
  slice order `:146-149`; the locked invariants `:93-139`; the non-autonomous set
  and task-125's lock `:190-195`; the stop conditions `:249`; the `verified`
  evidence rule `:231-233`; the smallest-truthful-change rule `:236`; the
  auditSummary refresh rule `:222`; the doc naming convention `:61`, `:64-66`; the
  PRD content mandate `:68-73`; the commit convention `:217-220`.
- `.agent/plans/governance/drep-discovery/README.md` **(via brief)** — status
  vocabulary; one commit per task `:15`; task-125's manual lock `:16`; the `!!!`
  rule; no e2e in v1.
- `governance-drep-discovery-plan.md` **(via brief)** — `:161-164` soft sync
  banner, two-phase load, route-scoped trigger, per-phase budgets; `:213` typed
  failures including selfnode capability errors; `:349` the 10 s timeout as an
  unvalidated guess and the deferred latency measurement.
- `designs/shared-design-tokens.md` — **read directly**: §1 status badges `:9-20`
  (the selfnode row `:16`, the contrast rule `:18`, the status-grounding note
  `:20`); §6 Refresh State `:91-106` in full (the two-phase header `:93`, the
  budget table `:95-102`, the soft-banner rationale `:104`, the timestamp format
  `:106`); §9 microcopy `:159`, `:181`, `:184`, the placeholder rules `:159`/`:226`,
  the JA-expansion rule `:224`.
- `designs/drep-discovery-design.md` **(via brief)** — the component map `:185-186`;
  the state table `:195-203`; the empty-state copy boundary `:213-214`; the naming
  drift at `:184` (recorded as ux-refinement PD-2, no edit needed).
- `research/ux-refinement-sync-and-load-research.md` **(via brief)** — `:74`, the
  `LedgerDB.Backend` dev-shell-vs-packaged discrepancy and the rule that no
  dev-shell latency figure may size a production timeout; the dropped task-125
  provenance rider.
- `research/ux-refinement-findings.md` **(via brief)** — `:29-41`, the deliberate
  variant-union scaffolding with future owners named in comments (the direct
  precedent for S-1/S-2); `:55-63`, the provisional 30 s budget.
- `research/slice-7-findings.md` **(via brief)** — F-1, the precedent for treating
  a task's declared `targetPath` as indicative when the live repo disagrees (C-1).
- `task-plans/slice-1-PRD.md` and `task-plans/anchor-2-PRD.md` — **read directly**
  as structure skeletons only (section order, heading depth, decision-entry shape,
  the OWED convention, the Final Outcome convention). anchor-2's D-7 is cited as
  the resolution precedent for D-1.
- **Live code** — re-opened during this pass, not taken on trust:
  `source/main/governance/GovernanceQueryService.ts` (`:49-70`, `:160-210`,
  `:224-250`, `:378-395`), `source/common/types/governance.types.ts:139-172`,
  `source/main/utils/setupLogging.ts:183-204`,
  `source/renderer/app/stores/GovernanceStore.ts` (`:60-82`, `:311-362`),
  `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`
  (`:226-282`, `:320-352`),
  `source/renderer/app/components/governance/_shared/DRepEmptyState.tsx`
  (`:50-60`, `:73-100`),
  `source/renderer/app/components/governance/_shared/DRepErrorBanner.tsx:12-30`,
  `source/renderer/app/components/governance/drep-directory/DRepCard.tsx:1-40`,
  `source/renderer/app/components/governance/drep-directory/DRepDirectoryList.tsx:1-30`,
  `source/renderer/app/containers/governance/DRepDirectoryPage.tsx:20-55`,
  both locale catalogs at the `governance.drepDirectory.*` neighbourhood.
- **Skills flagged for implementation, not invoked at planning:**
  `storybook-creation` (task-123 and task-124 stories, D-15), `i18n-messaging`
  (the three new keys and the ja-JP placeholders), `evidence-rules` (binding on
  every doc in this slice — no uncited factual claim, no unrun command reported
  green), `git-commit-formatter` (one subject-only commit per task).
  `e2e-test-creation` is **not applicable** (no e2e in v1, and task-125 is manual
  by lock). The `cardano-cli-*` skills are **not applicable** — slice-8 issues no
  new CLI query and changes no argv.

## Locked Invariants Touched (inlined)

| # | invariant | tasks | how slice-8 holds it |
|---|---|---|---|
| **1** | **Local-first.** Discovery data comes only from the local node via the main-process `GovernanceQueryService`. No hosted explorers, indexers, GovTool, Koios, Blockfrost or public governance APIs. | 123, 124, 125 | slice-8 opens no socket of any kind. task-125's checklist makes "no external portal" an explicit, observed pass/fail step of the release journey — it is the row that *proves* this invariant to a human, so its checklist must state it as a test, not a preamble. |
| **2** | **Sanitization floor.** No DRep id, no `abstain`/`no_confidence` literal, no CIP-129/CIP-105 bech32 string in any logger, analytics or electron-store payload. Re-asserted via the task-111 spy suite in every slice. | **123** (primary), 124 | **Stressed by D-11.** `elapsedMs` rides `DRepListQueryPayload`, which `governanceChannel.ts:56` serialises wholesale into `Logs/pub/DRep-state-snapshot.json` **bypassing `filterLogData`** (`setupLogging.ts:183-204`). A millisecond integer is non-identifying by construction, but the guide must say so and re-run `tests/jest/governance/logDRepStateSnapshot.spec.ts`. task-124's selfnode path logs nothing new; the existing failure log records `errorType` only (`GovernanceStore.ts:349-351`) and that shape is preserved. **Every task re-asserts `tests/jest/security/governance-sanitization.spec.ts` green.** |
| **3** | **CLI discipline.** Bulk `--all-dreps` once per refresh; per-DRep invocations forbidden. Network flag derives from node config only, never renderer/IPC input. Socket via `CARDANO_NODE_SOCKET_PATH` in `spawn.env`, never argv. | **123**, 124 | No probe or warm-up query is added to measure latency — `elapsedMs` is measured around the query that already runs. `setNetwork` (`:125-140`) and the `spawn.env` socket handling are untouched. task-124 relies on `_assertQueryable()` throwing **before** any `spawn` (`:224-225`), so a selfnode retry issues zero CLI invocations (D-14). |
| **4** | **Lovelace losslessness.** `json-bigint` lossless parse → decimal-string IPC → renderer `BigNumber` rehydration. Never route raw `JSONbig` objects across IPC or into observables. | **123** | `elapsedMs` is a plain `number` of milliseconds, doc-commented as such, sitting beside `fetchedAt` which is already a plain-number timestamp. It is never routed through the decimal-string convention and never confused with `stakeByDRepId`'s `Lovelace` values (`governance.types.ts:148-153`). `_rehydrateDReps` (`GovernanceStore.ts:519-537`) is untouched. |
| **5** | **Preliminary copy.** Every new en-US and ja-JP string keeps the leading `!!!`. Removing `!!!` is a release-end manual review, never a per-task action. | **123, 124** | Binds all three new keys, in both catalogs. `preliminaryCopyMarkers.spec.ts:57-64` enforces it mechanically for `governance.*` keys and `:39-46` enforces both-catalog parity. **No task in this slice removes a `!!!`**, and task-125's checklist explicitly must **not** instruct the verifier to treat `!!!` as a defect. |
| **6** | **No partial directory for selfnode.** When the service reports selfnode CLI-unsupported, the renderer shows the empty state — never a partially populated directory. | **124** | Structurally guaranteed: `_assertQueryable()` throws before the `Promise.all` that would fetch anything, so no entries ever exist. The new arm sits **ahead of** the `Failed` arm in `renderContent()` (S-4) and replaces the whole list area, per `drep-discovery-design.md:201`. The test pins the no-spawn / no-entries property rather than asserting an emptied list. |
| **7** | **DRep status grounding.** Canonical on-chain status is `active \| inactive`; `expiring` is renderer-derived display state; `retired` is deferred. Do not widen the canonical union. | **124** | **The direct cause of D-1.** `DRepStatus` (`governance.types.ts:35`) gains no member, `DRepStatusBadge.tsx:20-29`'s exhaustive `Record` is untouched and not imported by the empty state, and the selfnode indicator is plain markup under a directory-scoped message id. shared-design-tokens §1 is reconciled (Edit 1), not the union. |
| **8** | **No second delegation backend / no auto-delegation.** | *none* | slice-8 touches no delegation path, no `VotingStore`, no confirmation dialog and no `location.state` handoff. task-125's checklist **observes** the delegation journey; it changes nothing on it and must never instruct the verifier to auto-select a DRep. |
| **9** | **Smallest truthful change.** Reuse existing seams (`RendererIpcChannel`, `GovernanceQueryService` + `governanceChannel`, the `_shared` governance components) over new abstractions. | **all three** | Both variant unions are extended at their pre-declared extension points rather than replaced (S-1, S-2). No new IPC channel, no new error-type member, no sidecar payload (D-11), no new store observable beyond what the visual contract needs. The **one** new component in the slice is the skeleton list, and it exists only because D-2 rules the spinner insufficient — an exception made explicitly and on the record, not by drift. |

## Dependencies

- **In-slice chain:** none. slice-8 has zero intra-slice dependency edges; the
  canonical order is technical (D-6), not dependency-derived.
- **Cross-slice prerequisites, all five `complete`:** task-103 (the query service
  and its budgets — the sole dependency of both autonomous rows), task-115 (HW
  delegate path), task-116 (detail view), task-121 (search / show-all), task-122
  (favorites). task-111 is `verified` and its spy suite is the floor every task
  re-asserts.
- **Not a dependency, do not attempt to close:** task-166 is `partial`, and its
  remainder is a locked `manual_execution` latency measurement on a synced node.
  slice-8's `elapsedMs` field *feeds* that future measurement (D-3) but does not
  discharge it.
- **What slice-8 hands forward:** nothing — it is the last slice. Everything it
  cannot discharge becomes a user-owned pre-merge or pre-release obligation, listed
  under **OWED at slice close**.
- **Runtime and tooling:** node v24.16.0, jest 27.5.1, prettier 2.1.2, TypeScript
  4.9.5, Electron 41.3.0, React 16.14.0, MobX 5.15.7, react-intl 2.9.0
  (`injectIntl` / `intlShape` / `FormattedMessage` — no hooks, no
  `FormattedRelativeTime`).
- **Environment.** `nix` is **absent**, so `nix fmt` cannot run and stays a
  pre-merge obligation the user owns; the substitute is
  `node_modules/.bin/prettier --write <explicit changed paths>` — never
  `yarn prettier`, whose package.json script embeds a repo-wide `"**/*.*"` glob.
  Twelve governance files carry **pre-existing** prettier drift at HEAD, including
  five of slice-8's likely targets (`DRepDirectory.tsx`, `GovernanceQueryService.ts`,
  `api.ts`, `governanceChannel.ts`, `GovernanceQueryService.spec.ts`,
  `DRepDirectory.stories.tsx`) — **format only files the task newly creates; on
  pre-drifted files match the surrounding style by hand.** Discard unwanted changes
  with `git restore` / `git checkout -- <paths>`, **never `git stash`** (the stash
  stack is shared across worktrees). `gh` and push credentials are absent, so work
  stays local. There is **no browser**, so Storybook's visual and ja-JP overflow
  passes cannot execute here — `yarn storybook:build` (green at HEAD) is the only
  mechanical check. There is **no synced node and no hardware device**, which is
  why task-125 exists as a checklist and not as a result.

## Corpus-vs-Repo Corrections slice-8 Inherits

Recorded so no guide author re-derives them. **Live repo wins.**

| # | corpus claim | live repo at `0cdcab581` | disposition |
|---|---|---|---|
| C-1 | task-123's `targetPath` is `source/main/` | The visual half of the contract — skeleton list, banner variant, badge relocation — is entirely in `source/renderer/app/components/governance/`, and the copy is in the locale catalogs | `targetPath` is indicative, not binding (precedent: `research/slice-7-findings.md` F-1). The true file set is the Per-Task Contract above |
| C-2 | task-124's title promises an "IPC payload" change | `GovernanceQueryErrorType.SelfnodeCliUnsupported` already exists (`governance.types.ts:157-166`), is already thrown (`GovernanceQueryService.ts:209-213`), and already crosses IPC intact as a plain object preserving `details` (`governanceChannel.ts:32-45`). The renderer already normalises it (`GovernanceStore.ts:562+`) and `DRepDirectory.tsx:233-238` already branches on it | **The wire is complete.** task-124 is renderer-only; the title's IPC clause is already satisfied. Verify and record; add no field, no enum member, no channel |
| C-3 | shared-design-tokens §1 `:16` implies a `DRepStatusBadge` variant for selfnode | `DRepStatusBadge.tsx:20-29` is an exhaustive `Record<DRepStatus, string>` over a closed `'active' \| 'inactive'` union that invariant 7 forbids widening; §9 `:161-222` mints no id for the row | **Live repo + invariant 7 win** (D-1). New directory-scoped message id, plain markup inside the empty state, one clarifying doc sentence (Edit 1) |
| C-4 | §6 `:97` and `drep-discovery-design.md:195` describe the shipped first-load render | The shipped render is `<LoadingSpinner /> + "Loading DRep data…"` (`DRepDirectory.tsx:242-248`); `grep -ri skeleton source/renderer/app/components/governance/` returns **zero hits** | **Doc is ahead of code.** D-2 builds the code to match rather than amending the doc |
| C-5 | The 30 s stake budget is a measured production figure | `GovernanceQueryService.ts:52-56` calls it "provisional until real synced-node latency is measured"; `ux-refinement-sync-and-load-research.md:74` records that the dev-shell reading came from a build with `LedgerDB.Backend: "V2InMemory"` while packaged builds force `"V2LSM"` (`nix/internal/launcher-config.nix:295`) | **Provisional stands.** No dev-shell measurement may size it. The task-166 remainder owns it; slice-8 supplies `elapsedMs` and stops |
| C-6 | `yarn storybook:build` is red at HEAD and `yarn compile` needs a `typed-scss-modules` workaround (asserted in anchor-2's tracker text and carried into the slice-8 briefing) | Both are **green** at `0cdcab581`: `storybook:build` exit 0 in 84.4 s; `compile` exit 0 in 25.9 s with its `precompile` `typedef:sass` hook, leaving the tree clean | **Run both, do not waive them.** anchor-2's closed `statusReason` is **not** edited (D-18); the correction is recorded in `research/slice-8-findings.md` |
| C-7 | `yarn stylelint` is not a listed gate | It is **red at HEAD with 118 errors**, all `order/properties-alphabetical-order`, all in this feature's own governance SCSS — including `DRepEmptyState.scss` (3) and `DRepErrorBanner.scss` (4), both slice-8 targets | Out of scope, recorded (D-4); new declarations alphabetical, new file clean (D-13) |
| C-8 | `drep-discovery-design.md:195` says the refresh button is disabled during first load | `DRepDirectoryBanner.tsx:99` disables it only while `isRefreshing`; during `Loading` it is live | Divergence recorded in the findings note, **not fixed and not doc-edited** — no AC asks for it |
| C-9 | The design map places the shared components under `shared/` (`drep-discovery-design.md:184`) | They live at `components/governance/_shared/` | Known naming drift, recorded as ux-refinement PD-2, "no doc edit needed". Cite the real path |
| C-10 | Tracker `metadata.updated` is `2026-07-27` and `metadata.totalTasks` is `75` | The phases have moved past both | Guidance, not contract; JSON `dependencies` are authoritative. slice-8 adds no tasks, so `totalTasks` needs no change (D-17 also declines the `auditSummary`) |

## Risks and Open Questions

- **R-1 (high) — two clocks.** The single most likely way slice-8 breaks working
  behaviour is a renderer timer that races the main-process timeout: a 10 s banner
  scheduled in the renderer would fire *before* or *after* the CLI rejection
  depending on IPC latency, producing a banner with no error or an error with no
  banner. *Mitigation:* D-3 and D-9 together forbid **every** renderer timer, and
  NFR-1 makes "zero new `setTimeout`/`setInterval` in the renderer" a grep-checkable
  property the reviewer must verify rather than infer.
- **R-2 (medium) — the skeleton exceeds its estimate and can grow without limit.**
  task-123 is estimated at 4 h and D-2 knowingly blows it. A skeleton that tries to
  mirror every card affordance (badges, favourite toggle, source label, two CTAs)
  becomes a second `DRepCard` that must be maintained in lockstep. *Mitigation:*
  NFR-10 caps it as a pure presentational component with no state, no props beyond
  a count and no data; the guide should fix the placeholder block count explicitly
  so it is not an open question at implementation time. **Not a licence to downgrade
  it to a spinner** (D-2).
- **R-3 (medium) — moving the refreshing badge breaks an existing assertion.**
  `DRepDirectory.spec.tsx` covers the badge where it is today. D-8 moves it into
  `DRepDirectoryBanner`. *Mitigation:* the assertion **moves** to
  `DRepDirectoryBanner.spec.tsx` rather than being deleted, and the guide states
  the measured `baseline → expected` count for both suites so a silent net loss of
  coverage is visible.
- **R-4 (medium) — `elapsedMs` widens a log file that bypasses redaction.** The
  snapshot writer serialises the whole payload without `filterLogData`
  (`setupLogging.ts:183-204`). A future field added to this payload by reflex,
  citing slice-8's precedent, might not be a harmless scalar. *Mitigation:* D-11
  states the reasoning explicitly — the field is admissible **because** it is a
  non-identifying millisecond integer, not because the payload is a free-for-all —
  and `logDRepStateSnapshot.spec.ts` is re-run in task-123.
- **R-5 (medium) — the 118 stylelint errors ship.** D-4 leaves them, so
  `yarn stylelint` and therefore `yarn check:all` stay red through slice close, on
  the **last** slice of the feature. A reader could mistake that for slice-8's
  regression. *Mitigation:* D-18 requires the findings note to record the count,
  the rule, the file list and the fact that all 118 pre-date slice-8 at HEAD
  `0cdcab581`; every slice-8 `statusReason` that mentions gates must say the same.
  **Explicitly out of scope and explicitly recorded — not silently ignored.**
- **R-6 (medium) — the feature closes with an unexecuted release gate.** task-125
  stays `pending` by design (D-5), so the feature's own definition of "verified"
  is never reached by any row in the tracker. *Mitigation:* this is the correct
  outcome, not a defect — `prompt.md:232` reserves `verified` for exactly this kind
  of evidence. The checklist must be good enough that the user can close it in one
  sitting, and the Final Outcome must state plainly that no slice-8 row is
  `verified` and why.
- **R-7 (low) — ja-JP overflow on the new banner.** §9 `:224` names the refresh
  error banner as a 30–60% JA/DE expansion risk, and it is now a two-slot rich
  message inside a bordered banner. No browser exists here, so the visual check
  cannot run. *Mitigation:* the styling constraint is stated in the i18n inventory
  (≥2 wrapped lines, vertical reflow, never ellipsize); the visual pass is OWED.
- **R-8 (low) — `showNoSyncFallback` and the new selfnode arm can overlap.** The
  predicate at `DRepDirectory.tsx:233-238` already excludes `SelfnodeCliUnsupported`
  from the `Failed` leg, but it does **not** exclude it from the `Loaded` leg — a
  selfnode error arriving while `!isNodeInSync` with no retained data could route
  to `noSync` instead of `selfnode`. *Mitigation:* task-124 owns reconciling the
  predicate with the new arm (S-4), and the guide should include a test at that
  exact intersection: selfnode error **and** node not in sync.
- **R-9 (low) — the checklist becomes stale.** Written at slice close, it describes
  the UI as of `slice-8`. *Mitigation:* it is a standalone doc (D-16) that a later
  change can edit in place, and it references states by name (skeleton list,
  refresh-failed banner, selfnode empty state) rather than by pixel description.

### OWED at slice close — nothing here may be reported green

1. `nix fmt` — unavailable in this devcontainer; the prettier substitute is not the
   mandated formatter. Pre-merge obligation, user-owned.
2. `yarn stylelint` — **red with 118 pre-existing errors** in this feature's own
   governance SCSS. Out of scope by D-4; a user-owned pre-merge cleanup item.
   `yarn check:all` is red transitively for this reason plus `prettier:check`.
3. The Storybook **visual** pass and the ja-JP overflow check for the new skeleton,
   the refresh-failed banner and the selfnode empty state — no browser here.
   `yarn storybook:build` compiling is not a visual pass.
4. **task-125's release verification itself** — synced node, packaged build, real
   hardware device, both wallet types. The checklist is delivered; the run is not.
   The row stays `pending`.
5. The release-end `!!!` copy review, including the two ja-JP placeholders this
   slice adds — out of scope for every slice by invariant 5.
6. The task-166 latency-measurement remainder — `elapsedMs` feeds it; slice-8 does
   not close it.
7. The pre-existing prettier drift on the five slice-8-adjacent governance files —
   not reformatted by design, carried as a `nix fmt` obligation.
8. The first-load refresh-button-disabled divergence (`drep-discovery-design.md:195`
   vs `DRepDirectoryBanner.tsx:99`) — recorded, unfixed.

## Release Verification Checklist Contract (task-125)

The checklist document at
`.agent/plans/governance/drep-discovery/release-verification-checklist.md` must
satisfy the following. **This is the contract; the guide author writes the steps.**

**Preconditions, stated up front and non-negotiable:**
- A **synced** Cardano node (`isNodeInSync === true`), on mainnet or preprod.
- A **packaged** Daedalus build, not the dev shell — `nix/internal/launcher-config.nix:295`
  forces a different LedgerDB backend on packaged builds, so dev-shell behaviour is
  not evidence (C-5).
- Both wallet types available: a software wallet, and a **real** Ledger or Trezor
  device. On-device confirmation must be observed on the device, not asserted from
  the app.
- The verifier is a human. No step may be closed by an agent.

**Journey legs, each with an unambiguous pass/fail observation:**
1. **Browse** — directory loads on `/governance/dreps`; the cohort banner renders;
   reshuffle works; pagination works.
2. **Evaluate** — open a DRep detail view; on-chain fields render with their source
   label; verified anchor content renders with its own label where an anchor exists;
   the chain-native view still works when no anchor is available.
3. **Search / show-all** — prefix search resolves; show-all reveals excluded DReps;
   filters clear.
4. **Favorites** — toggle a favorite; restart Daedalus; the favorite survives; the
   favorites view renders.
5. **Select** — hand a DRep off to the delegation form from the directory.
6. **Delegate** — complete delegation on the software wallet; then on the hardware
   wallet with on-device confirmation.
7. **Confirm** — the confirmation dialog's DRep identity is **byte-equal** to what
   was selected, and the on-device DRep ID equals `vote.chosenOption`.

**Standing assertions, checked throughout, not as a preamble:**
- **No external portal.** At no point does any leg require GovTool, Koios,
  Blockfrost, an explorer or any hosted governance API. Record this as an observed
  pass, since it is the feature's charter (invariant 1) and the acceptance
  criterion's literal wording.
- No DRep id, `abstain`/`no_confidence` literal or bech32 string appears in the
  user-visible logs after the run (spot-check `Logs/pub/`, noting that
  `DRep-state-snapshot.json` is the one documented exception and carries public
  on-chain directory data only).
- Preliminary `!!!` markers are **expected** and are not defects at this stage.

**Explicitly excluded riders — considered and dropped; do not re-add:**
`cardano-cli` / `cardano-node` / `LedgerDB.Backend` provenance capture
(`research/ux-refinement-sync-and-load-research.md:74`), and the sibling task-125
riders cut in `slice-2-findings.md` / `slice-4-findings.md`.

**Outcome recording:** a result table the verifier fills in (leg, observation,
pass/fail, build hash, node tip, device model). Only a completed run of this table
can justify promoting task-125 past `pending`, and only the user can promote it.

## Definition of Done

Per task, in canonical order, each closed by its own single commit
(`<type>(gov): task-NNN <short imperative summary>` — one Conventional Commits
subject line, no body, no blank line, no trailers, **no `Co-Authored-By`**):

1. Every acceptance criterion is discharged as written or as annotated in this PRD
   (the **[D-n]** annotations on AC quotes are binding), with the annotation
   reflected in the task's tracker `statusReason`.
2. `yarn compile` exits 0 (green at HEAD; no `typed-scss-modules` workaround needed
   — C-6).
3. `yarn lint` exits 0 with **0 errors**. The ~5635 warnings are the pre-existing
   repo baseline; no slice-8 task adds a warning on an added line.
4. Focused Jest runs green for every suite the task touches, each recorded as a
   measured `baseline → expected` **delta**, not a total. Suites in play:
   `tests/jest/governance/GovernanceQueryService.spec.ts`,
   `tests/jest/governance/GovernanceStore.spec.ts`,
   `tests/jest/governance/logDRepStateSnapshot.spec.ts`,
   `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
   (and its snapshot),
   `source/renderer/app/components/governance/drep-directory/DRepDirectoryBanner.spec.tsx`,
   `source/renderer/app/containers/governance/DRepDirectoryPage.spec.tsx`.
   `GovernanceCliArgvSmoke.spec.ts` self-skipping without `cardano-cli` on PATH is
   expected, not a regression. **`jest tests/jest` alone is ~8% of the suite and is
   never reported as "the suite".**
5. The sanitization floor re-proved by running
   `tests/jest/security/governance-sanitization.spec.ts` green **in every task**,
   cited alongside `logDRepStateSnapshot.spec.ts` for task-123 (D-11).
6. i18n: all three new keys present in `en-US.json` and `ja-JP.json` with the
   leading `!!!`; key-set parity holds;
   `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` green; `yarn i18n:manage` run
   and any file it touched that the task did not intend to change reverted with
   `git restore`. **The catalogs and `translations/messages.json` are never
   prettier-formatted.**
7. Formatting via `node_modules/.bin/prettier --write <explicit changed paths>`
   only, on the files the task **created** plus the files it edited that were
   **prettier-clean at HEAD** — never `yarn prettier`, never the tasks tracker,
   never the locale catalogs, never one of the five pre-drifted targets
   (`api.ts`, `GovernanceQueryService.ts`, `DRepDirectory.tsx`,
   `DRepDirectory.stories.tsx`, `GovernanceQueryService.spec.ts`), and never a
   markdown doc (`.md` is outside prettier's configured scope). `.scss` **is** in
   scope and is formatted.
8. `yarn storybook:build` exits 0 (green at HEAD — run it, do not waive it, C-6),
   the new state-knob keys and discrete stories exist, no story declares a local
   `IntlProvider` or a per-locale variant, and the integrated
   `Voting / Governance > Connected flow` story still runs.
9. **NFR-1 verified by inspection:** zero new `setTimeout` / `setInterval` in the
   renderer diff.
10. `yarn stylelint` reports **111** errors — 118 minus the seven that lived in the
    two dead selector blocks removed with the markup that used them
    (`.errorBanner` and `.refreshingBadge` in `DRepDirectory.scss`). **No `--fix`,
    no declaration reordered** (D-4), and **no new error** (new declarations
    alphabetical, new stylesheet clean at birth, D-13).
11. Doc reconciliations applied exactly once — Edit 1 and Edit 2 present, the five
    verify-and-record items confirmed at their anchors with **no duplicate
    paragraph added**.
12. `research/slice-8-findings.md` exists and records at minimum the D-18 items;
    **no closed slice's `statusReason` is edited.**
13. Tracker rows updated with **value-only** edits (the JSON is never
    prettier-formatted), field order `id, title, description, status, statusReason,
    evidence, updatedAt, priority, estimatedHours, dependencies, targetPath,
    acceptanceCriteria`, `updatedAt` as `"YYYY-MM-DD"`, `evidence` a flat array of
    repo-relative path strings. **task-125's row stays `pending`** (D-5, D-16).
    **No slice-8 row is promoted to `verified`** — none has the independent proof
    `prompt.md:231-233` requires. No `auditSummary` is added (D-17).
14. This PRD's **Final Outcome** section filled at slice close, with every OWED item
    restated and **none reported green**, and the planning status advanced from
    `in_review` to `approved` only after the critique pass.

## Final Outcome

slice-8 is closed. Two of its three rows are `complete`; the third is `pending` by
design and is the feature's terminal, user-owned stop condition. **No slice-8 row is
`verified`, and none should be** — the only evidence that could promote one is the
release verification that task-125 describes and that no agent may run.

Last code commit: `45efc1911` (task-124). The close-out commit carrying this
section, the release-verification checklist, the findings note and the tracker rows
sits directly on top of it. All work is local: this container has no `gh` and no push
credentials.

### Final tracker status

| Row | Status | Why it stopped there |
| --- | --- | --- |
| task-123 | `complete` | Every acceptance criterion discharged, reviewed clean in round one. Not `verified`: all proof is the task's own focused test runs, and the independent evidence that would promote it is task-125's. |
| task-124 | `complete` | Same. Both criteria discharged, reviewed clean in round one, no independent proof beyond its own suites. |
| **task-125** | **`pending`** | **Correct and intended (D-5).** The row is locked `manual_execution` and is a stop condition: it needs a synced node, a packaged build, a real hardware wallet and a human. The autonomous deliverable — the checklist — is written and committed at [`../release-verification-checklist.md`](../release-verification-checklist.md). **Writing the checklist does not discharge the row.** Only the user, after a completed run of its §9 result table, may promote it. |

The feature therefore closes with its own definition of "verified" unreached by any
row. That is the designed outcome (R-6), not a defect.

### What shipped, task by task

| Task | Commit | What shipped | Measured Jest deltas |
| --- | --- | --- | --- |
| task-123 — refresh-latency budget and stale-while-refresh contract | `50b23a5f0` `feat(gov): task-123 render the refresh latency and stale-while-refresh contract` | One new wire field: a plain-number `elapsedMs` on `DRepListQueryPayload` and `DRepStakeQueryPayload`, measured around work the service already does — no probe query, no new `spawn`, no argv change — and sampled after `_assertQueryable` so a selfnode throw measures nothing. AC-1 discharged in the governance IPC channel comment block in `source/common/ipc/api.ts` (10 s phase-1 / 30 s phase-2, `elapsedMs` observational only); the design tokens already carried the same budget, so no design doc was edited. AC-2: `DRepErrorBanner` gains a `refreshFailed` variant rendering the new `governance.drepDirectory.error.refresh` key byte-identically to the design tokens, replacing a hand-rolled banner that printed raw main-process error strings to the user; the first-load spinner is replaced by the new 25-card `DRepDirectorySkeleton`; the stale-while-refresh spinner badge moves beside the "Last updated" timestamp in `DRepDirectoryBanner`. | `GovernanceQueryService.spec.ts` 38→40 · `logDRepStateSnapshot.spec.ts` 5→6 · `DRepDirectory.spec.tsx` 60→61 (snapshot unchanged) · `DRepDirectoryBanner.spec.tsx` 7→9 |
| task-124 — selfnode CLI-unsupported empty state | `45efc1911` `feat(gov): task-124 replace the selfnode query error with the unavailable empty state` | No wire change was needed despite the task title: `SelfnodeCliUnsupported` already existed, is thrown in exactly one place, crosses IPC as a plain object and survives `_normalizeError` — nothing under `source/main` or `source/common` was edited. `DRepEmptyStateVariant` gains `selfnode`; `DRepDirectory.renderContent()` gains a selfnode arm ahead of the `Failed` arm and ahead of the default arm that owns the list and the refresh-failed banner, so a selfnode user can never see a partial directory or a retained-snapshot banner. The unavailability indicator is plain markup (icon + textual label) inside the empty state, not a `DRepStatusBadge`. Two new copy keys in both catalogs with the `!!!` marker; two design-doc reconciliations. | `DRepDirectory.spec.tsx` 61→65 · `DRepDirectoryPage.spec.tsx` 9→10 · `GovernanceQueryService.spec.ts` 40→41 |
| task-125 — release verification | *(this close-out commit; the row is unchanged at `pending`)* | `release-verification-checklist.md`: preconditions (packaged build, synced node, both wallet types, real device), four standing assertions including the no-external-portal charter check, the browse → evaluate → search/show-all → favorites → select → delegate legs with software **and** hardware passes, on-device byte-equality as the load-bearing check, the seven refresh-latency states with inducement recipes, the selfnode leg as a separate launch, a localization leg, and a result table the verifier fills in. | none — no code changed |

### Gates at close — measured, not asserted

Re-measured in the slice worktree at `45efc1911` with a clean tree; these supersede
the per-task figures where they differ. Full detail in
[`../research/slice-8-findings.md`](../research/slice-8-findings.md) F-11.

| Gate | Result |
| --- | --- |
| `node_modules/.bin/jest --runInBand` (unfiltered) | **exit 0** — 92 passed + 1 skipped of 93 suites; **1334 passed + 12 skipped of 1346 tests**; 10 snapshots; 39.4 s. The skipped suite is `GovernanceCliArgvSmoke.spec.ts`, self-skipping without `cardano-cli` on PATH. |
| Focused slice-8 surface (7 suites incl. the sanitization floor and the copy-marker suite) | **exit 0** — 175 tests, 1 snapshot. This total reconciles exactly with the per-task deltas above (41 + 65 + 10 + 9 + 6 + 39 + 5 = 175), which is the independent check that no delta was mis-stated. |
| `yarn compile` | **exit 0**, 18.4 s |
| `yarn lint` | **exit 0** — 0 errors, 5635 warnings (pre-existing repo baseline) |
| `yarn i18n:manage` | **exit 0**, byte-identical no-op, tree clean afterwards |
| `yarn stylelint` | **exit 2 — 111 errors** across 13 governance SCSS files, all `order/properties-alphabetical-order`. 118 at the planning anchor; 7 disappeared with two dead selector blocks removed alongside the markup that used them; **0 added**. Out of scope by D-4. |
| `yarn storybook:build` | **exit 1 — waived, not green.** See the D-6 revision below. |
| `yarn check:all` | Red transitively on the `prettier:check` and `stylelint` legs. |
| `nix fmt` | **Not run — no `nix` in this container.** Owed. |

### Planning decisions at close

**Held as written:** D-1 (selfnode indicator as a new message id rendered as plain
markup, `DRepStatus` not widened), D-2 (a real skeleton list, not a spinner — it
overran the 4 h estimate as anticipated and was not downgraded), D-3 (main process
the single timeout authority; `elapsedMs` observational; **zero** new renderer
timers, grep-verified), D-4 (the stylelint debt recorded, not swept), D-5
(task-125 stays `pending`), D-7, D-8, D-9, D-10, D-11, D-12, D-13 (the new
`DRepDirectorySkeleton.scss` is the one governance stylesheet that is
stylelint-clean at birth), D-14 (no mount guard; the no-spawn property pinned by a
test instead), D-15, D-16, D-17 (no `auditSummary` added to the phase object) and
D-18.

**Revised during implementation — one, and it is a correction of this PRD:**

- **Definition of Done item 8 and the C-6 premise it rests on were wrong.**
  This PRD asserted `yarn storybook:build` is green at HEAD and must be run rather
  than waived. Measured at close it **exits 1**, with a manager-bundle
  `ModuleParseError` on `storybook/addons/DaedalusMenu/register.tsx` — a directory
  slice-8 never touched (`git diff 0cdcab581..45efc1911 -- storybook/addons/` is
  empty), reproduced by both task verifiers on a pristine `0cdcab581` tree. The
  corrected disposition is **waived with the reason recorded**. Consequence, stated
  plainly: the stories slice-8 adds have **no bundle-level check** in this
  environment. They are not untyped — `tsconfig.json` has no `include` and excludes
  only `node_modules`, so `yarn compile` typechecks the story file and `yarn lint`
  covers it — but the visual pass was already owed and stays owed. Recorded as
  finding F-2.

**Also worth carrying forward:** the i18n inventory in this PRD placed
`governance.drepDirectory.error.refresh` between `…error` and
`…error.rankingUnavailable`. That is the wrong sort position (`a` < `e`), corrected
during implementation and recorded as F-4. Insert catalog keys by running the sort,
not by eyeballing the dotted segments.

### Review record

Both buildable rows were reviewed and both were **approved in round one with zero
blockers**; the minor observations are recorded in
[`slice-8-code-review.md`](./slice-8-code-review.md) under
`Code Review (task-123, round 1)` and `Code Review (task-124, round 1)`, together
with the planning pass, the critique pass and the fix-pass disposition that preceded
them. Carried-forward minors, none blocking: `DRepDirectorySkeleton`'s `count` prop
has no caller yet; `DRepErrorBanner`'s `retryLabel` defaults to an empty string; no
`DRepDirectory`-level test pins that the list stays rendered while `Refreshing` (a
pre-existing gap — `Refreshing` never had a `renderContent` arm); the design tokens'
absolute-ISO-timestamp tooltip on "Last updated" is still unimplemented; and with
`isNodeInSync === false` the syncing banner and the selfnode empty state render
together, which the design requires.

### Research produced

slice-8 produced durable research beyond the mandatory entries:
[`../research/slice-8-findings.md`](../research/slice-8-findings.md), F-1 … F-11 —
the stylelint accounting, the corrected gate premises, the recorded-not-fixed
first-load refresh-button divergence, the catalog sort correction, the selfnode
indicator rationale, the "retry loop that isn't one", the single-timeout-authority
contract, the skeleton rationale, the `nix fmt` deviation, the prettier-drift
inventory with its scratch-directory false-green trap, and the close-out gate table.

### Residual gaps and user-owned obligations — nothing here is green

Every OWED item is restated as still owed. None was discharged.

1. **`nix fmt`** — unavailable in this container; `prettier --write` on explicit
   paths was the substitute and is not the mandated formatter. Pre-merge, user-owned
   (F-9).
2. **`yarn stylelint` — 111 errors**, all in this feature's own governance SCSS, all
   `order/properties-alphabetical-order`, all pre-dating slice-8. Out of scope by
   D-4; user-owned pre-merge cleanup. `yarn check:all` is red for this and for
   `prettier:check` (F-1, F-11).
3. **Pre-existing prettier drift on 12 governance files** — deliberately not
   reformatted; five of them are slice-8 edit targets and were hand-matched. Carried
   as part of the `nix fmt` obligation (F-10).
4. **The Storybook visual pass and the ja-JP overflow check** for the skeleton, the
   refresh-failed banner and the selfnode empty state — no browser here, and
   `storybook:build` does not compile in this environment either (F-2).
5. **task-125's release verification itself** — synced node, packaged build, real
   hardware device, both wallet types, a human. The checklist is delivered; the run
   is not. **The row stays `pending`.**
6. **The release-end `!!!` copy review**, including the three preliminary keys this
   slice adds — out of scope for every slice by the preliminary-copy invariant.
7. **The task-166 latency-measurement remainder** — `elapsedMs` feeds it; slice-8
   does not close it.
8. **The first-load refresh-button divergence** — the design disables Refresh during
   first load; the code disables it only while refreshing. Recorded, unfixed (F-3).

## References

- Parent plan: [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
- Task tracker: [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json) — `slice-8` phase at `:747-805`
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md) — §1 `:9-20`, §6 `:91-106`, §9 `:159-226`
- Preceding slice PRD: [anchor-2-PRD.md](./anchor-2-PRD.md)
- UX refinement research: [ux-refinement-sync-and-load-research.md](../research/ux-refinement-sync-and-load-research.md), [ux-refinement-findings.md](../research/ux-refinement-findings.md)
- Slice-8 findings: `../research/slice-8-findings.md` (created during the slice, D-18)
- Release verification checklist: `../release-verification-checklist.md` (task-125, D-16)
- Implementation guide: [slice-8-implementation-guide.md](./slice-8-implementation-guide.md)
- Code review log: [slice-8-code-review.md](./slice-8-code-review.md)
