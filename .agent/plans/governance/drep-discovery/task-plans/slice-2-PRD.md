# Slice-2 PRD: Software-Wallet Delegate

> **Planning Status:** approved | **Date:** 2026-07-22 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `slice-2` — "Slice 2 - Software-wallet delegate" (riskLevel: medium)
> **Tasks:** task-112 → task-113 → task-114 (order forced by JSON `dependencies`)
> **Implementation guide:** [slice-2-implementation-guide.md](./slice-2-implementation-guide.md)

---

## Executive Summary

Slice-2 closes the primary delegation journey for software wallets: the user browses the
in-app DRep directory from the delegation form, selects a DRep from a directory **list
row**, returns to `VotingPowerDelegation` with the DRep ID pre-filled (and wallet +
vote-type restored), confirms a dialog that now displays the **raw DRep ID itself**, and
submits through the existing software-wallet `delegateVotes` path. The slice also replaces
the last external GovTool routing in this flow (the gov.tools link on the DRep-ID input)
with the in-app "Browse DReps" affordance, adds flow-level Jest + Storybook coverage, ships
preliminary `!!!` en-US/ja-JP copy, and de-GovTools the governance walkthrough for this
flow.

**Why now:** slice-1 (walking skeleton + sanitization floor) is fully landed; the locked
order (prompt.md:147-148) makes slice-2 the first phase with incomplete tasks; its only
external dependencies (task-107, task-108) are `complete`.

---

## Problem Statement

Slice-1 shipped a browsable DRep directory, but it is a dead end: nothing connects a
directory row to the delegation form. The form still routes discovery to the external
GovTool portal (violating the spirit of local-first invariant #1 for this flow), and the
confirmation dialog shows only a generic "Delegate to DRep" label — the user cannot verify
*which* DRep they are about to sign for. Slice-2 wires selection → pre-fill → ID-visible
confirmation → existing submit path, without introducing any second delegation backend.

---

## Per-Task Contract (interaction modes, scope, dependencies)

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-112** — Integrate DRep selector into VotingPowerDelegation (list-row selection) | `autonomous` | "Browse DReps" affordance in the form (replacing the gov.tools label link, D2); round-trip wallet/vote-type preservation and DRep-ID pre-fill via `location.state` only; row-level "Select for delegation" CTA on `DRepCard`; `location.state` picker helpers; flow Jest incl. two-hop AC via harness-only Detail stub (D1) | No Detail route/surface, no `DREP_DETAIL` literal, no "View details" CTA, no store changes, no query params | task-107 ✔, task-108 ✔ |
| **task-113** — Update delegation confirmation with DRep identity (ID only) | `autonomous` | Widen `VotingPowerDelegationConfirmationDialog` props with `drepIdentity: DRepIdentity \| null`; render the full raw selected ID for drep targets (D3); sentinels keep labels; dialog spec | No CIP-105 dual display, no signed-payload line, no source label, no name slot (anchor-2), no `normalizeDRepIdentity` (cv-1), no VotingStore change | task-112 |
| **task-114** — In-slice verification of the software-wallet delegate path | `autonomous` | End-to-end Jest (select → ID-only confirm → byte-equal `delegateVotes` payload); Storybook selector + confirmation coverage (global locale toggle); `!!!` copy audit; walkthrough de-GovTool-ing (D2 scope); compile/lint gate; sanitization floor re-run | The release-end `!!!` removal review stays user-owned; no `verified` promotion of 114 itself | task-112, task-113 |

No task in this slice is in the locked non-autonomous set (task-125, task-166 remainder,
task-158, release-end `!!!` review). Planning surfaced **no blocking decisions**: D1–D4
resolve all open questions from the grounding brief §G.

---

## Orchestrator Decisions D1–D4 (binding, as applied)

- **D1 — Two-hop AC without a Detail route.** task-112 AC-5 is satisfied by (a) production
  state-forwarding helpers (`pickDelegationFormNavigationState` /
  `pickDelegationFormReturnState` in
  `source/renderer/app/containers/governance/delegationFormState.ts`) used today by the
  row Select CTA and reserved for slice-4's detail push, and (b) a Jest-harness-only
  `DetailRouteStub` (path literal `/governance/dreps/:drepId` defined inside the spec)
  that forwards inherited state + `selectedDRepId` exactly per task-117's AC. Production
  gains ONLY the row-level "Select for delegation" CTA; `routes-config.ts` and
  `Routes.tsx` are untouched. Planning found this fully implementable — no escalation.
- **D2 — GovTool link replaced.** The external gov.tools label link
  (`VotingPowerDelegation.tsx:248-275`) becomes the in-app "Browse DReps" affordance.
  Keys removed as dead from messages + both locales: `drepInputLabelLinkUrl`,
  `drepInputLabelLinkUrlPreview`, and additionally `drepInputLabelLinkText` and
  `drepInputLabelPreprod` (planning consequence P-1 below). Walkthrough sweep scoped to
  this flow: `02-voting-power-delegation.md` (:60, :62, :78, and the now-false "ID is not
  displayed in the dialog" claim at :84 — re-verified anchor; the brief's :88-90 was
  stale), `04-troubleshooting.md` (:17, :57, :140), and the three
  `05-improvements-vs-light-wallets.md` lines that describe THIS flow's routing (:23,
  :204, :252); the Lace note (:33) stays.
- **D3 — Confirmation identity scope.** Dialog accepts `DRepIdentity` but renders
  **`drepIdentity.raw` only** — the exact string that is `chosenOption` and the
  `delegateVotes` `dRepId`, byte-equal (invariant #10). Full ID, monospaced, no
  truncation (confirmation is a security surface; the truncating `DRepIdDisplay` is
  deliberately NOT reused here). No CIP-105 line ("when derivable" — deferred),
  no signed-payload line, no source label, no name slot (anchor-2). The container builds
  the identity from `chosenOption`; `credentialType` is filled by a syntactic
  `drep_script` prefix check that never influences the rendered or submitted bytes.
- **D4 — Format step.** `nix` is unavailable in this container; the pre-commit format is
  `node_modules/.bin/prettier --write` on changed `.ts/.tsx/.scss/.md` files only (never
  tracker JSON, locale JSONs, or `translations/messages.json`). Deviation to be recorded
  in slice findings + Final Outcome so the user can run `nix fmt` before merge.

### Planning decisions (recorded by the planner)

- **P-1 (extends D2):** the label is unified across networks. `drepInputLabelPreprod`
  existed only because gov.tools had no preprod directory; the in-app directory works on
  every network (slice-1 fixtures were captured on preprod), so the `environment.isPreprod`
  / `isMainnet` branches and the preprod-only label are removed with the URL keys. The
  changed `drepInputLabel` value gets a fresh leading `!!!` (changed copy = preliminary
  copy).
- **P-2:** the handoff shape is typed once, in
  `containers/governance/delegationFormState.ts`, and both containers consume it; the
  pickers whitelist fields so arbitrary router state can never reach the form contract.
- **P-3:** the flow spec lives at
  `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` (co-located,
  following `Governance.spec.tsx` precedent) and mocks only the two react-polymorph-heavy
  dropdowns; task-114 appends the payload test to the same file rather than creating a
  parallel harness under `tests/`.
- **P-4:** `onBrowseDRepsClick` (form), `onSelectForDelegation` (directory chain) and
  `drepIdentity` (dialog) are **required** props, so tsc forces every call site (including
  stories) to be updated within the same task — stories get minimal compile fixes in
  112/113 and the richer coverage in 114.
- **P-5 (environment deviation):** `.vscode/` is gitignored, so the walkthrough files
  exist only in the main checkout. Task-114 copies them into the worktree, edits the
  copies, and records that they need a manual sync back to the main checkout at slice
  close (worktree isolation forbids editing the main checkout directly; gitignored files
  do not travel with the branch).

---

## User Stories

### US-2.1 — Browse and select a DRep without leaving the app
**As a** Daedalus user delegating my voting power,
**I want to** open the in-app DRep directory from the delegation form and pick a DRep from
a list row,
**So that** I never have to copy IDs from an external website.

**Acceptance:**
- The DRep-ID input label offers a "Browse DReps" link that navigates to
  `/governance/dreps` (no external URL anywhere in this flow).
- Every directory card shows a "Select for delegation" button (a real `<button>`, native
  Enter/Space semantics, card wrapper not focusable — shared tokens §10).
- Clicking it returns to `/voting/governance` with the row's DRep ID pre-filled.
- Direct DRep-ID paste continues to work unchanged.

### US-2.2 — Round-trip state preservation
**As a** user who has already picked a wallet and vote type,
**I want** those choices restored when I come back from browsing,
**So that** selection never costs me re-entering the form.

**Acceptance:**
- Browse-out carries `{ from: '/voting/governance', selectedWalletId, voteType }` in
  `location.state`; the return hop carries the same fields plus `selectedDRepId`.
- The two-hop Form → Directory → Detail → Form contract is covered by Jest (Detail as a
  harness-only stub per D1) — wallet + vote type restored, ID pre-filled.
- Query params and store-backed pending form state are never used.

### US-2.3 — Verifiable confirmation
**As a** user about to sign a delegation,
**I want to** see the exact DRep ID I selected in the confirmation dialog,
**So that** I can verify the target before entering my spending password.

**Acceptance:**
- For drep targets the dialog renders the full raw bech32 ID (monospaced, breakable) under
  a "DRep ID" label; the generic "Delegate to DRep" label no longer renders for them.
- Abstain / No Confidence still render as labels (form-only sentinels).
- No name — verified or otherwise — renders (slot reserved for anchor-2).
- The rendered string is byte-equal to `chosenOption` and to the `dRepId` submitted via
  `delegateVotes`.

### US-2.4 — Verified, private flow
**As a** privacy-conscious user,
**I want** the whole browse → select → delegate path covered by tests that also prove no
vote target leaks,
**So that** the slice-1 privacy floor survives the new surface area.

**Acceptance:**
- End-to-end Jest covers select → confirm → byte-equal `delegateVotes` payload.
- The task-111 sanitization spy suite stays green (17/17) with zero modifications.
- No new logger/analytics/electron-store call exists in the slice diff.

---

## Functional Requirements

| ID | Requirement | Owner |
|----|------------|-------|
| FR-1 | `VotingPowerDelegation` gains a required `onBrowseDRepsClick` callback and an optional `initialFormState` (`selectedWalletId`/`voteType`/`selectedDRepId`) that seeds the form's initial state, mapping `selectedWalletId → selectedWallet` via `wallets.find` | task-112 |
| FR-2 | The DRep-input label's external gov.tools link is replaced by an in-app "Browse DReps" `Link`; the `environment.isPreprod`/`isMainnet` branches and the four dead `drepInputLabel*` keys are removed | task-112 |
| FR-3 | `VotingGovernancePage` is `withRouter`-wrapped; it derives `initialFormState` from `location.state` via `pickDelegationFormNavigationState` and pushes `ROUTES.GOVERNANCE.DREPS` with `{ from, selectedWalletId, voteType }` on browse | task-112 |
| FR-4 | `DRepCard` gains a "Select for delegation" button (`governance.drepDirectory.card.select`), threaded as a required `onSelectForDelegation(drepId)` through `DRepDirectoryList` and `DRepDirectory` | task-112 |
| FR-5 | `DRepDirectoryPage` is `withRouter`-wrapped; row select pushes `inherited.from ?? '/voting/governance'` with `{ ...inherited, selectedDRepId }` via `pickDelegationFormReturnState` | task-112 |
| FR-6 | `delegationFormState.ts` exports the navigation-state type and the two pickers; the return-state picker is the production forwarding contract slice-4's detail push will reuse | task-112 |
| FR-7 | Jest covers browse-out state, single-hop return restore, and the two-hop sequence with the harness-only Detail stub | task-112 |
| FR-8 | `VotingPowerDelegationConfirmationDialog` accepts `drepIdentity: DRepIdentity \| null` and renders `drepIdentity.raw` (full, monospaced) for drep targets; sentinel labels unchanged | task-113 |
| FR-9 | `VotingGovernancePage` constructs `drepIdentity` from `chosenOption` (null for sentinels; syntactic `credentialType` classification) | task-113 |
| FR-10 | Dialog Jest: ID rendered byte-equal, sentinels labeled, no name field ever | task-113 |
| FR-11 | End-to-end Jest: row select → confirmation ID → `delegateVotes` called with byte-equal `chosenOption` + passphrase | task-114 |
| FR-12 | Storybook: browse affordance in existing form stories, new prefilled-from-directory story, ID-only confirmation via existing dialog stories; locales via the global toggle | task-112/113/114 |
| FR-13 | Walkthrough GovTool-routing language removed for this flow (worktree copies of the gitignored `.vscode` docs; sync-back recorded) | task-114 |
| FR-14 | Sanitization floor re-asserted: task-111 suite green, no new logging surface | task-114 |

---

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | react-intl stays on the 2.9.0 API surface (`injectIntl`/`intlShape`/`defineMessages`/`FormattedMessage`; no hooks) |
| NFR-2 | React Router 5.2 conventions: `withRouter` + class containers; `history.push(path, state)` |
| NFR-3 | All new/changed en-US and ja-JP strings keep the leading `!!!`; no existing `!!!` removed |
| NFR-4 | New Jest object-argument assertions use `expect.objectContaining` (prettier 2.1.2 oscillation guard) |
| NFR-5 | `tsc --noEmit` zero errors after every task (verified-clean baseline in this worktree) |
| NFR-6 | No `.scss.d.ts` committed; prettier substituted for `nix fmt` per D4 |
| NFR-7 | Storybook uses the global English/Japanese toggle; no per-story IntlProvider, no per-locale duplicates |

---

## Architecture: Data Flow

```
VotingPowerDelegation (form, /voting/governance)
   │  "Browse DReps" (renders only in drep mode — sentinel invariant #13)
   │  onBrowseDRepsClick → container push:
   │  ('/governance/dreps', { from:'/voting/governance', selectedWalletId, voteType })
   ▼
DRepDirectoryPage (withRouter, inherits location.state)
   │  row CTA "Select for delegation"                     [slice-4 detail push
   │  push(inherited.from ?? '/voting/governance',         forwards inherited state
   │       { ...inherited, selectedDRepId })               via pickDelegationFormReturnState;
   ▼                                                       Jest DetailRouteStub only this slice]
VotingGovernancePage (withRouter)
   │  pickDelegationFormNavigationState(location.state) → initialFormState
   ▼
VotingPowerDelegation  ── selectedDRepId → drepInputState.value (verbatim)
   │  chosenOption = drepInputState.value        (byte-equal, invariant #10)
   ▼
VotingPowerDelegationConfirmationDialog
   │  drepIdentity.raw rendered (raw === chosenOption)
   ▼
VotingStore.delegateVotes({ chosenOption, passphrase, wallet })
   │  delegateVotesRequest.execute({ dRepId: chosenOption, … })   (VotingStore.ts:417-421)
   ▼
PUT /v2/dreps/:dRepId/wallets/:walletId        (the ONLY delegation backend)
```

No store gains state; the handoff lives exclusively in router `location.state`.

---

## Route Changes

**None.** `ROUTES.VOTING.GOVERNANCE` (`routes-config.ts:37`) and
`ROUTES.GOVERNANCE.{ROOT,DREPS}` (:39-42) already exist; slice-2 adds no literal and does
not touch `Routes.tsx` (:212-239). The Detail path literal appears only inside the Jest
harness (D1). `DREP_DETAIL`/`FAVORITES` remain owned by slice-4/slice-7.

---

## Component Tree (Slice-2 delta)

```
containers/voting/VotingGovernancePage        ← withRouter; initialFormState + browse push + drepIdentity
└── components/voting/voting-governance/
    ├── VotingPowerDelegation                 ← initialFormState seed; Browse DReps in-app link
    └── VotingPowerDelegationConfirmationDialog ← drepIdentity prop; raw-ID render
containers/governance/
    ├── delegationFormState.ts (NEW)          ← location.state type + pickers (fwd contract)
    └── DRepDirectoryPage                     ← withRouter; handleSelectForDelegation
components/governance/drep-directory/
    ├── DRepDirectory                         ← onSelectForDelegation thread-through
    ├── DRepDirectoryList                     ← onSelectForDelegation thread-through
    └── DRepCard                              ← "Select for delegation" row CTA (only CTA, D1)
```

---

## What Slice-2 Deliberately Does NOT Include

- ❌ DRep detail view / `DREP_DETAIL` route / "View details" CTA (slice-4; D1)
- ❌ Hardware-wallet confirmation caption + HW QA (slice-3; the HW code path is untouched)
- ❌ CIP-105 dual-form display, credential/signed-payload derivation, `normalizeDRepIdentity` (cv-1; D3)
- ❌ Verified names anywhere, anchor fetch/verify/render (anchor-1/anchor-2)
- ❌ Default cohort, badges, search, favorites (slice-5/6/7)
- ❌ Sync-gate UX rework for the un-synced return hop (ux-refinement; behavior recorded in findings)
- ❌ Removing any `!!!` marker (release-end manual review, user-owned)
- ❌ Storybook per-locale story duplicates or local IntlProviders

---

## Docs / Designs / Research / Workflows / Skills Consulted

- **Grounding brief** (orchestrator-verified): `/home/node/.claude/jobs/25eb7a06/tmp/slice-2-grounding-brief.md`
- **Orchestration contract:** `prompt.md` (PRD fields :68-73, small-model bar :75-89, locked invariants :93-139, slice order :147-148, status rule :202-211)
- **Tracker:** `governance-drep-discovery-plan-tasks.json` (slice-2 phase; task-112/113/114 acceptanceCriteria)
- **Designs:** `designs/drep-discovery-design.md` (:38 handoff, :45 second entry point, :47 round-trip binding, :49 two-hop rule, :69 CTA labels, :151 component note); `designs/shared-design-tokens.md` §4 (ID display), §7 (confirmation identity + equality rule), §9 (message IDs `card.select`, `card.viewDetails`, `delegationConfirm.hw.caption`), §10 (card keyboard contract)
- **Research:** `research/external-research.md` (two-CTA pattern adopted; wallet-connect phrasing rejected; labels "View details" / "Select for delegation"); `research/slice-1-final-pass-findings.md` (§3 IPC error transport untouched, §4 jest/tsc direct binaries, §8 push-guard location)
- **Precedent (structure only):** `task-plans/slice-1-PRD.md`, `task-plans/slice-1-implementation-guide.md`
- **Live seams:** all files listed in the guide, re-verified with line anchors on 2026-07-22 (notably: walkthrough 02's dialog claim is at :84, not the brief's :88-90; `.vscode` is gitignored — P-5)
- **Workflows/skills applicable at build time:** `.agent/workflows/frontend.md`, `.agent/workflows/test.md`, `.agent/workflows/storybook.md`; skills `i18n-messaging`, `storybook-creation`, `git-commit-formatter` (subject-only per standing convention), `evidence-rules`

---

## Locked Invariants Touched

| # | Invariant | How slice-2 honors it |
|---|---|---|
| 1 | Local-first | Browse DReps targets the in-app `/governance/dreps`; gov.tools keys deleted; walkthrough de-GovTool'd for this flow |
| 2 | Sanitization floor | Zero new logger/analytics/electron-store calls; task-111 suite re-run per task (17/17); `drepIdLength`-only precedent noted in guide |
| 3 | Anchor transport floor (negative) | No anchor-derived content rendered; dialog spec pins "no name ever" |
| 4 | No second delegation backend | Handoff via `location.state` only; typed pickers; `VotingStore`/`GovernanceStore` byte-identical; no query params |
| 10 | Byte-equality | Prefill verbatim → `chosenOption` → rendered `drepIdentity.raw` → `delegateVotes` `dRepId`; asserted end-to-end in Jest |
| 11 | Preliminary copy | 3 new + 1 changed key, all `!!!` in both locales; dead-key deletions only (no marker stripped); release-end review untouched |
| 13 | Form-only sentinels | Browse affordance exists only inside the drep-mode input label; directory rows are DReps only; sentinel labels kept in dialog |

Not touched: #5 (no IPC change), #6 (no CLI change), #7/#8 (slice-5), #9 (cv-1), #12
(slice-7), #14 (status vocabulary unchanged).

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| task-107 (bare directory list) | complete |
| task-108 (routes + Governance sidebar) | complete |
| `Cardano.DRepID.isValid` (existing validation, `VotingPowerDelegation.tsx:106`) | present |
| `DRepIdentity` type (`governance.types.ts:20-31`) | present |
| Existing `delegateVotes` request + `VotingStore` paths (`VotingStore.ts:361-438`) | present, untouched |
| react-router-dom 5.2.0 `location.state` | verified (`package.json:269-270`) |
| Jest harness precedents (`Governance.spec.tsx:41-59`, `DRepDirectory.spec.tsx:39-62`) | present |

---

## Risks Specific to Slice-2

| Risk | Mitigation |
|------|-----------|
| Selector creates a competing delegation path | Handoff is DRep-ID-only via `location.state`; `delegateVotes`/`VotingStore` untouched (plan risk table :332) |
| Vote target leaks into logs/analytics | No new logging; task-111 spy suite re-run every task (plan :336) |
| Two-hop AC drifts from slice-4 reality | Stub uses the production `pickDelegationFormReturnState`; slice-4's real CTA adopts the same helper, so the contract is code, not convention (D1) |
| react-polymorph render friction in jsdom (skinless `Input`/`Link`, Dialog portal) | Harness wraps in the exact `ThemeProvider` from `StoryDecorator.tsx:25-29`; heavy dropdowns mocked per `Governance.spec.tsx` precedent; password input located structurally (`input[type="password"]`) |
| Un-synced return hop lands on `VotingUnavailable` | Expected behavior; pre-fill survives in `location.state`; recorded in findings; owned by `ux-refinement` (brief §G-4 — do not rework) |
| Walkthrough files are gitignored and absent from the worktree | Copy-in/edit/sync-back procedure (P-5); deviation recorded in findings + Final Outcome |
| Changed dialog/story prop contracts break compile mid-slice | Required props + same-task story compile fixes (P-4) keep `tsc --noEmit` at zero after every task |
| ja-JP copy quality | Preliminary `!!!` values; final wording is the release-end user-owned review (invariant #11) |

**Open questions:** none — D1–D4 close the brief's §G items; planning added P-1…P-5
without needing user input.

---

## Definition of Done

- [ ] task-112/113/114 each: acceptance criteria met, focused Jest green, code review clean, one subject-only commit (`<type>(gov): task-NNN …`), tracker JSON synchronized (`status`, `statusReason`, `evidence`, `updatedAt`)
- [ ] `node_modules/.bin/tsc --noEmit` → zero errors; lint clean for touched surfaces
- [ ] Sanitization floor 17/17 after every task (task-114 AC-8)
- [ ] Byte-equality asserted end-to-end (row → confirmation → payload)
- [ ] `routes-config.ts`, `Routes.tsx`, `VotingStore.ts`, `GovernanceStore.ts` byte-identical to base
- [ ] All new/changed locale strings `!!!`-prefixed; `yarn i18n:manage` clean; `translations/messages.json` diffs committed with their task
- [ ] Storybook: selector + prefilled + ID-only confirmation stories render in both locales via the global toggle
- [ ] Walkthrough flow-routing GovTool language removed (worktree copies; sync-back recorded)
- [ ] `research/slice-2-findings.md` written (D1–D4 as applied, P-1/P-5 deviations, un-synced-return note)
- [ ] task-112/113 promoted to `verified` only on task-114's clean completion (prompt.md:207-212); task-114 itself lands at `complete`
- [ ] Final Outcome below filled at slice close

---

## Final Outcome

_Filled at slice close, 2026-07-22._

### What shipped, per task

- **task-112** (`284e4fb71e8f60f1cbedfe41d08b65a54de3e32c`,
  `feat(gov): task-112 integrate DRep directory selection into voting power delegation form`) —
  in-app "Browse DReps" affordance replacing the external gov.tools label link (D2, with
  the four dead `drepInputLabel*` keys removed per P-1); typed `location.state` pickers in
  `containers/governance/delegationFormState.ts`; `withRouter`-wrapped
  `VotingGovernancePage` (browse-out push + `initialFormState` restore) and
  `DRepDirectoryPage` (return push); row-level "Select for delegation" CTA on `DRepCard`
  threaded through `DRepDirectoryList`/`DRepDirectory`; flow Jest including the two-hop
  contract via the harness-only `DetailRouteStub` (D1 — `routes-config.ts`/`Routes.tsx`
  untouched). Tracker status: **verified** (promoted at slice close on task-114's
  dedicated proof, per the orchestration contract's dedicated-proof rule).
- **task-113** (`bdad1d22735f0e31e096d246ef147f17196f3f6a`,
  `feat(gov): task-113 render selected DRep ID in delegation confirmation dialog`) —
  `VotingPowerDelegationConfirmationDialog` widened with
  `drepIdentity: DRepIdentity | null`; drep targets render the full raw selected ID
  (monospaced, breakable, untruncated — D3 raw-ID-only scope), sentinels keep their
  labels via a null identity, and a regression test pins that no name ever renders
  (anchor-2 reservation). Container builds `raw: chosenOption` verbatim. Tracker status:
  **verified** (promoted at slice close on task-114's dedicated proof).
- **task-114** (`35aa4792e4979fff44fc36fe22200d69c4959563`,
  `test(gov): task-114 verify software-wallet delegate path end to end`) — end-to-end
  payload Jest (row Select → pre-fill → confirmation renders the byte-equal raw ID →
  passphrase → `delegateVotes` called once with
  `objectContaining({ chosenOption, passphrase })` and `initializeVPDelegationTx` with
  the same `chosenOption`); the "Voting power delegation - prefilled from directory"
  story (global locale toggle); the `!!!` copy audit (+4 `!!!` values per locale, zero
  removed); the walkthrough de-GovTool-ing (D2 scope, P-5 worktree copies); and
  `research/slice-2-findings.md`. Tracker status: **complete** (it is the verification
  task itself; no further dedicated proof exists — the release-end `!!!` review stays
  user-owned).

### Verification results

- One code-review round per task — three rounds total, each **approved with zero
  blockers** ([slice-2-code-review.md](./slice-2-code-review.md): "Code Review:
  task-112/113/114 — round 1 — 2026-07-22"). Planning itself took one Critiquer round
  (`requires_changes` on a wrong quoted import path) plus a fix pass to `approved`.
- Gates at close (re-run by the reviewer in round 3 and re-confirmed by the slice-close
  assurance run): `tsc --noEmit` zero errors; `yarn lint` exit 0 (pre-existing warnings
  only); focused Jest 20/20 across the flow/dialog/directory suites; sanitization floor
  **17/17 green**; `yarn i18n:manage` idempotent; `prettier --check` clean on touched
  `.ts/.tsx/.scss`.
- Invariants: #1/#2/#4/#10/#11/#13 verified per round — `VotingStore.ts`,
  `GovernanceStore.ts`, `routes-config.ts`, `Routes.tsx` byte-identical to base;
  byte-equality pinned end-to-end; no new logger/analytics/electron-store calls.

### Deviations

- **D4 — `nix fmt` unavailable:** `node_modules/.bin/prettier --write` on changed
  `.ts/.tsx/.scss/.md` files substituted throughout (never tracker JSON or locale/
  `translations` JSONs). **Run `nix fmt` from a nix-capable environment before merge.**
  The task-112 round found prettier 2.1.2 cannot parse inline
  `import { type … }` syntax; resolved in task-113 by separate `import type` statements —
  `prettier --check` is clean at close.
- **P-5 — gitignored walkthroughs:** the edited copies live in the worktree's
  `.vscode/docs/walkthroughs/governance/` and do not travel with the branch. **Manually
  sync them back to the main checkout's `.vscode/docs/walkthroughs/governance/` at
  merge.**
- Expected-drift riders recorded by the reviews: prettier 2.1.2 reformat drift in
  `Governance.stories.tsx`/`VotingPowerDelegation.tsx`/dialog (semantically identical);
  `translations/messages.json` regeneration re-added unrelated
  `daedalus.diagnostics.dialog.*` descriptor hunks (tool-managed output).
- Guide-prescribed comments in `VotingGovernancePage.spec.tsx` embed task IDs (against
  the repo comment convention) — strip to plain "why" comments on the file's next touch.
- Storybook was not launched in this container (no display): stories verified at
  tsc/eslint/fixture level; eyeball the prefilled story in both locales via the global
  toggle before merge.

### Handoffs and notes

- Durable findings are captured in
  [research/slice-2-findings.md](../research/slice-2-findings.md) — D1 (harness
  `DetailRouteStub` contract that slice-4 task-116/117 must honor via
  `pickDelegationFormReturnState`), D2 (GovTool link replacement + P-1 label
  unification), D3 (raw-ID-only scope; derivation deferred to cv-1), D4/P-5 deviations,
  and the two `ux-refinement` behavior notes.
- Handed to `ux-refinement`: the un-synced return hop (lands on `VotingUnavailable` with
  the pre-fill parked in `location.state`).
- **Directory-first entry edge now owned.** The Critiquer N-4 residual
  (`WalletsDropdown` reset wipes a pre-filled ID when no form state was inherited) was
  handed to `ux-refinement` alongside the return-hop note, but that phase closed without
  taking it. It is now owned by task-138 (cv-2), whose AC-2 is replaced by a
  `currentVote` → inherited `selectedDRepId` → blank fallback chain, with the
  byte-identical carry-over and the directory-select-then-pick-wallet Jest regression as
  further criteria.
- **D3 residual now owned.** The pre-anchor shared-tokens §7 identity block D3 deferred —
  CIP-105 secondary line, signed-payload line and the `(Source: On-chain)` label — is
  task-175 (cv-2, after task-173). cv-1 shipped the `normalizeDRepIdentity` helper
  (task-129) but no dialog consumer, so the deferral landed a phase later than the
  findings file records; the verified-name slot stays with task-154 (anchor-2). Slice-2's
  own scope is unchanged by either assignment.
- `auditSummary`: slice-2 has none in the tracker — nothing to refresh; this section is
  the slice's outcome of record.

---

## References

- Parent plan: [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
- Task tracker: [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
- Implementation guide: [slice-2-implementation-guide.md](./slice-2-implementation-guide.md)
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md)
- Research: [external-research.md](../research/external-research.md), [slice-1-final-pass-findings.md](../research/slice-1-final-pass-findings.md)
- Grounding brief: `/home/node/.claude/jobs/25eb7a06/tmp/slice-2-grounding-brief.md`
