# Slice-4 PRD: DRep Detail View (On-Chain Only)

> **Planning Status:** approved | **Date:** 2026-07-24 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `slice-4` — "Slice 4 - DRep detail view (on-chain only)" (riskLevel: medium)
> **Tasks:** task-116 → task-117 (order forced by JSON `dependencies`: 117 depends on 116)
> **Implementation guide:** [slice-4-implementation-guide.md](./slice-4-implementation-guide.md)

---

## Executive Summary

Slice-4 thickens discovery with the first evaluation surface beyond the directory card:
a routed DRep detail view at `/governance/dreps/:drepId` rendering **local on-chain fields
only** — CIP-129 ID (truncated display + copy + inline copied confirmation), active/inactive
status, remaining expiry epochs, voting power in the tokens-§3 detail form (full ADA +
raw lovelace), a graceful "unavailable" state for current-epoch vote positions, and
**anchor presence** (URL + hash as non-interactive text under the "On-chain anchor
reference" source label). Task-116 builds the component tree
(`components/governance/drep-detail/`), the container, Storybook, Jest, and i18n; task-117
adds the route literal + `Routes.tsx` wiring (with the mandatory `exact` fix on the
directory route), the card's "View details" CTA with `location.state` forwarding, migrates
the slice-2 `DetailRouteStub` harness tests onto the production detail page, and **masks
the `:drepId` URL segment out of every Matomo analytics payload** — extending the
inherited sanitization floor suite from 20 to 23 tests.

**Why now:** the locked slice order (prompt.md:147-148) reaches slice-4 after
`ux-refinement` closed (commit `d5e3a03f2`); task-116's only dependency (task-106,
GovernanceStore) is `complete`; `anchor-1` depends on this detail view as its first
verified render surface (plan :296).

---

## Problem Statement

The directory ships ID-only cards with a single "Select for delegation" action. A user
evaluating a DRep has nowhere to see the full on-chain picture — expiry horizon, exact
voting power, whether the DRep published an anchor — before delegating. The slice-2
harness proved the two-hop Form → Directory → Detail → Form state contract against a
test-only stub; production still has no detail route. Additionally, adding a route that
embeds a DRep id in the URL would leak that id into analytics: `MatomoClient` embeds
`window.location.hash` into every tracked event's `url` field
(`MatomoClient.ts:61-63`), so the route cannot ship without an analytics-boundary mask.

---

## Per-Task Contract (interaction modes, scope, dependencies)

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-116** — Build DRep detail view component (on-chain only) | `autonomous` | `drep-detail/` component tree per the design hierarchy (`DRepDetail`, `DRepDetailOnchainSection`, `DRepDetailAnchorSection`, `DRepDetailActions`); `DRepDetailPage` container replicating the directory's refresh contract (refresh on Idle/Failed + sync reaction — D9); anchor-presence section (D7); vote-positions "unavailable" state (D1); shared-component extensions (`DRepSourceLabel` anchor-reference variant, `DRepIdDisplay` copied confirmation); ~21 new `!!!` keys per locale; container Jest incl. ja-JP + deep-link/not-found flows; component Storybook via the global locale toggle | No route/`Routes.tsx`/`routes-config.ts` change; no anchor fetch/render (anchor-1); no favorite toggle (D3, slice-7); no vote positions (D1); no dual-ID display (D5, cv-1); no "expiring" badge variant (D6, slice-5) | task-106 ✔ (`complete`) |
| **task-117** — Wire the DRep detail route | `autonomous` | `GOVERNANCE.DREP_DETAIL` literal; `Routes.tsx` detail `TrackedRoute` + `exact` on the directory route (D8); card "View details" CTA threaded to a `DRepDirectoryPage` push that forwards `pickDelegationFormReturnState(location.state)` (D10); `MatomoClient` URL masking + pure `maskAnalyticsRoute` helper (D2); sanitization-suite extension (20→23); `DetailRouteStub` → production migration in `VotingGovernancePage.spec.tsx` without weakening any slice-2 pin (D10); `slice-4-findings.md` | No detail-specific analytics events; no breadcrumbs/tab changes (Directory tab already covers detail via `startsWith`, `Governance.tsx:48-51`); no search/deep-link entry UI | task-116 |

Neither task is in the locked non-autonomous set (task-125, task-166 remainder, task-158,
release-end `!!!` review). Planning surfaced **no blocking decisions**: the twelve
orchestrator decisions D1–D12 below plus planner resolutions P-1…P-11 close every open
question.

---

## Orchestrator Decisions D1–D12 (pre-resolved, binding; recorded with grounding)

- **D1 — Current-epoch vote positions are OUT of slice-4.** They require a `gov-state`
  query no main-process task owns (the slice-1 service ships `drep-state` + `tip` +
  `drep-stake-distribution` only; the plan's "DRep query shape" Key-Decisions row
  (~:138) defers `gov-state` to "the slices that need them" while also saying "proposal
  vote positions need gov-state in slice-4" — a plan-internal conflict). Task-116's own
  acceptance only requires the view to *stay useful when positions are unavailable*.
  Resolution: render a labeled "Current votes" field with a graceful
  "not available in this version" value; record the conflict as a deferral in
  `research/slice-4-findings.md` (written at slice close, task-117).
- **D2 — Route stays `/governance/dreps/:drepId`, with the id masked out of analytics
  URLs.** The path is pinned by the design (`drep-discovery-design.md:33`) and the
  existing Jest harness (`VotingGovernancePage.spec.tsx:74`). But
  `MatomoClient.getAnalyticsURL` (`MatomoClient.ts:61-63`) embeds
  `window.location.hash` into every `track()` payload for both `sendEvent` and
  `sendPageNavigationEvent` — an unmasked detail route would put the DRep id into every
  analytics event fired while the detail is open, violating the sanitization floor.
  Resolution: mask the `:drepId` segment at that single boundary
  (`maskAnalyticsRoute`, applied inside `getAnalyticsURL`) and extend
  `tests/jest/security/governance-sanitization.spec.ts` with masking regression tests.
- **D3 — Favorite toggle DEFERRED to slice-7.** task-122 owns favorites persistence
  (Electron local store, invariant #12); the README binding scope confines slice-4 to
  "on-chain fields + anchor presence only" (README:39). No stub UI ships —
  `DRepDetailActions` renders the select CTA only, despite the task description's
  "favorite toggle" phrase (tracker text predates the README scope lock).
- **D4 — The wireframe's "Registered: epoch N" row is DROPPED.** `drep-state` output has
  no registration epoch — the slice-1 parser reads expiry/anchor/deposit only, and
  `DRepDirectoryEntry` (`governance.types.ts:51-62`) carries no registration field. The
  design wireframe (`drep-discovery-design.md:92`) drifts from the data model; recorded
  in the findings note.
- **D5 — ID display is CIP-129-only via the existing `_shared/DRepIdDisplay.tsx`.**
  Dual-ID display (tokens §4 "show both forms fully") waits for cv-1 task-129's
  `normalizeDRepIdentity`. No new bech32 dependency, no duplication of task-129 scope.
  The truncated-with-tooltip display is the accepted slice-4 form.
- **D6 — Status badge shows active/inactive only via `_shared/DRepStatusBadge.tsx`.**
  "Expires in {n} epochs" renders as a plain labeled field, not a badge variant — the
  `expiring` display state is staged with slice-5's Threshold window (tokens §1 staging
  note).
- **D7 — Anchor presence section: URL + hash as NON-INTERACTIVE text** with the
  "On-chain anchor reference" source label (tokens §2). No clickable link, no copy
  button on the hash (the older design line `drep-discovery-design.md:212` suggesting a
  copy button is superseded by this decision's "non-interactive text"). The hardened
  open-external path is anchor-1 task-152.
- **D8 — The directory route must gain `exact`.** `Routes.tsx:233-237` mounts
  `DRepDirectoryPage` without `exact`, and the `<Governance>` children (:226-239) are
  not inside a `Switch` — adding the detail route without `exact` would double-render
  both pages on `/governance/dreps/:drepId`. The spec harness already models the fix
  (`VotingGovernancePage.spec.tsx:169-174`).
- **D9 — The detail container replicates the directory's refresh contract**
  (`DRepDirectoryPage.tsx:24-49`): `refresh()` on mount when `Idle`/`Failed`, plus the
  refetch-at-tip sync reaction. "Entry not found after load" shows an inline error with
  a "Back to directory" link (design :205). Deep-link / restart entry must work from an
  empty store — pinned by Jest.
- **D10 — task-117 migrates the `DetailRouteStub` harness tests to the production detail
  page WITHOUT weakening the verified slice-2 pins.** Detail receives
  `{ from, selectedWalletId, voteType }` from the directory's detail push and forwards
  it plus `selectedDRepId` back via `pickDelegationFormReturnState`
  (`delegationFormState.ts:50-62`) to `inherited?.from ?? ROUTES.VOTING.GOVERNANCE` —
  exactly the contract the stub pinned (slice-2 findings, "Binding on slice-4"). The
  slice-2/3 tests that stay untouched: the browse-out push pin, the single-hop restore,
  the byte-equal payload test, and all three HW tests.
- **D11 — Voting power on detail uses the tokens-§3 detail form**: full ADA with
  thousands separators plus the raw lovelace on a secondary line — not the card's
  abbreviated `₳ 688K` form. `null` renders `—` with the loading/unavailable tooltip by
  enrich state (mirrors `DRepCard.tsx:62-71`).
- **D12 — All new keys follow the shared-design-tokens §9 pre-assigned ids**:
  `governance.drepDetail.*` for detail copy, `governance.drepDirectory.card.viewDetails`
  for the card CTA, `governance.drepDirectory.backToDirectory` for the back link, and
  `governance.drepDetail.copyIdToast` for the copy confirmation.

### Planning decisions P-1…P-11 (recorded by the planner)

- **P-1 (extends D7/D12):** the "On-chain anchor reference" label has **no** pre-assigned
  id in §9 (§9 lists only `sourceLabel.onchain|verified|unverified|anchorUnavailable`).
  New id: `governance.drepDetail.sourceLabel.anchorReference`, following the §9
  `drepDetail.sourceLabel.*` family. The live shared component's existing key
  (`governance.drepDirectory.source.onChain`, `DRepSourceLabel.tsx:5-10`) is pre-existing
  slice-1 drift from §9 and is left untouched.
- **P-2 (implements D12's copyIdToast):** copy feedback is a **persistent inline
  confirmation** (`role="status" aria-live="polite"`, tokens §4's aria-live requirement)
  behind a new optional `showCopiedConfirmation` prop on the shared `DRepIdDisplay` —
  no timers, no global toast infrastructure. Cards keep the prop unset (default
  `false`): zero behavior change on the directory.
- **P-3:** the on-chain section's "On-chain" header **is** the §2 provenance treatment for
  that section (per the wireframe's `┌── On-chain ──┐` grouping); no duplicate "On-chain"
  pill renders inside the detail. `DRepSourceLabel` appears in the anchor section only,
  with the new `on-chain-anchor-reference` variant. This also keeps
  `getByText('!!!On-chain')` unambiguous in tests.
- **P-4 (implements D9):** detail render-state mapping is: entry present → content
  (including during `Refreshing` — stale-while-refresh); entry absent +
  `Idle|Loading|Refreshing` → loading treatment; entry absent + `Loaded|Failed` →
  inline not-found error + "Back to directory" link. No separate Retry button on detail:
  the container already re-fires `refresh()` on `Failed` mount, and the back link reaches
  the directory's full error surface.
- **P-5:** `DRepDetailActions` re-declares the existing id
  `governance.drepDirectory.card.select` ("Select for delegation") instead of minting a
  near-duplicate key. Duplicate `defineMessages` ids across files with identical
  `defaultMessage` are established repo precedent (`governance.drepDirectory.title` in
  both `DRepDirectory.tsx:20-24` and `DRepDirectoryBanner.tsx:9-13`; `yarn i18n:manage`
  proven idempotent over it — ux-refinement F-7).
- **P-6:** the back link uses react-polymorph `Link` with an **explicit
  `skin={LinkSkin}`**, so the detail specs render without a `ThemeProvider` harness —
  matching the explicit-skin convention of every governance component (`DRepCard.tsx`
  Button/ButtonSkin) rather than `VotingPowerDelegation`'s skinless-Link-under-app-theme
  pattern.
- **P-7:** "Back to directory" forwards `pickDelegationFormReturnState(location.state)`
  so the round trip survives Detail → Directory → row-select (the user's wallet and vote
  type are not dropped by backing out of a detail).
- **P-8 (implements D2):** the mask is a pure module
  `source/renderer/app/analytics/maskAnalyticsRoute.ts` with one regex that rewrites only
  the first path segment after `governance/dreps/` to the literal `:drepId`; the list
  route `governance/dreps` (no third segment) is untouched. The floor suite grows
  20 → 23: two pure-function tests (CIP-129 + CIP-105 masking; non-detail routes
  untouched) and one `MatomoClient.sendEvent` boundary test with a mocked
  `matomo-tracker`.
- **P-9:** the expiry field renders "Expires in {n} epochs" only when
  `status === 'active'` and `drepActivity != null`; otherwise `—` (a 0-epoch "expires
  in" line on an inactive DRep would be misleading; `drepActivity` is typed "0 when
  inactive, null if unknown", `governance.types.ts:37-38`).
- **P-10:** task-116's container spec pins the detail path locally as
  `` `${ROUTES.GOVERNANCE.DREPS}/:drepId` `` (the `DREP_DETAIL` literal lands only in
  task-117); the literal is derived from the routes table so the two stay consistent.
- **P-11:** the design's focus-management nicety ("focus moves to the back-link, then
  primary heading", `drep-discovery-design.md:263`) is deferred — an accepted gap
  recorded here; the §10 keyboard floor holds via native focusable controls
  (Link/Buttons) in document order (View details before Select on the card, §10's Tab
  order).

---

## User Stories

### US-4.1 — Evaluate a DRep before delegating
**As a** Daedalus user browsing the directory,
**I want to** open a detail view showing everything my own node knows about a DRep,
**So that** I can judge expiry risk, weight, and metadata presence before delegating.

**Acceptance:**
- Every directory card gains a "View details" button (before "Select for delegation" in
  Tab order, both real `<button>`s — tokens §10).
- The detail shows: status badge, "Expires in {n} epochs" as a plain field, voting power
  as full ADA + raw lovelace (tokens §3 detail form), anchor URL + hash as plain text
  with the "On-chain anchor reference" label, and a labeled vote-positions field that
  reads as unavailable.
- No anchor content, name, or external link renders anywhere (anchor-1 boundary).

### US-4.2 — Two-hop delegation round trip
**As a** user who entered the directory from the delegation form,
**I want** my wallet and vote-type choices to survive Form → Directory → Detail → Form,
**So that** inspecting a DRep costs me nothing.

**Acceptance:**
- The directory's "View details" push forwards `{ from, selectedWalletId, voteType }`
  (never `selectedDRepId`) via `pickDelegationFormReturnState`.
- Detail's "Select for delegation" returns `inherited + { selectedDRepId }` to
  `inherited.from ?? /voting/governance`; the ID pre-fills byte-equal.
- The production detail passes the exact contract the slice-2 `DetailRouteStub` pinned;
  the migrated Jest covers it with zero weakening of the slice-2/3 assertions.

### US-4.3 — Deep links and restarts work
**As a** user landing on a detail URL with a cold store,
**I want** the page to load itself,
**So that** restart/deep-link entry is never a dead end.

**Acceptance:**
- Mounting with an `Idle`/`Failed` store fires `refresh()` once; loading treatment shows
  until the list lands; the sync reaction refetches at tip (D9).
- An id absent from the loaded index shows the inline not-found error with a working
  "Back to directory" link.

### US-4.4 — The detail route leaks nothing
**As a** privacy-conscious user,
**I want** the id-bearing URL kept out of analytics,
**So that** opening a detail never reveals which DRep I looked at.

**Acceptance:**
- Every analytics `url` field renders the detail route as
  `http://daedalus/governance/dreps/:drepId`.
- The floor suite proves the mask for CIP-129 and CIP-105 forms and at the
  `MatomoClient.track` boundary; all 20 inherited tests stay green (23 total).
- No new `logger.*`/`analytics.*`/electron-store call exists in the slice diff.

---

## Functional Requirements

| ID | Requirement | Owner |
|----|------------|-------|
| FR-1 | `DRepSourceLabel` gains the `on-chain-anchor-reference` variant (message P-1); `'on-chain'` behavior unchanged | task-116 |
| FR-2 | `DRepIdDisplay` gains optional `showCopiedConfirmation` rendering an aria-live "!!!DRep ID copied" confirmation on successful copy (P-2); default off | task-116 |
| FR-3 | `DRepDetailOnchainSection` renders status badge, expiry field (P-9), §3 detail-form voting power with enrich-state tooltip on `—` (D11), and the vote-positions unavailable field (D1) | task-116 |
| FR-4 | `DRepDetailAnchorSection` renders URL + hash as non-interactive monospace text + the anchor-reference source label, or the "no anchor recorded" message (D7) | task-116 |
| FR-5 | `DRepDetailActions` renders only the "Select for delegation" CTA (D3) reusing `governance.drepDirectory.card.select` (P-5) | task-116 |
| FR-6 | `DRepDetail` composes back link (P-6) + header (`DRepIdDisplay` with confirmation — NO status badge; the badge renders exactly once, in the on-chain section's Status row, per the wireframe) + sections + actions, with the P-4 state mapping | task-116 |
| FR-7 | `DRepDetailPage` reads `match.params.drepId` untransformed, resolves the entry from `drepIndex`, replicates the D9 refresh contract, and implements the D10 forward/return pushes + the P-7 back push | task-116 |
| FR-8 | ~21 new `!!!` keys per locale under `governance.drepDetail.*` + `governance.drepDirectory.backToDirectory` (D12) | task-116 |
| FR-9 | Container Jest: on-chain render, anchor presence/absence, `—` tooltip, deep-link refresh + loading, not-found + back, select/back state forwarding, sync reaction, copy confirmation, ja-JP labels | task-116 |
| FR-10 | Storybook `Governance / DRep Detail`: loaded (with/without anchor, knobs), ranking-unavailable, loading, not-found; global locale toggle only | task-116 |
| FR-11 | `GOVERNANCE.DREP_DETAIL = '/governance/dreps/:drepId'`; `Routes.tsx` detail `TrackedRoute` (pageTitle "DRep Detail") + `exact` on the directory route (D8) | task-117 |
| FR-12 | `DRepCard` "View details" CTA (`card.viewDetails` key) threaded `DRepDirectory` → `DRepDirectoryList` → card as a required prop; `DRepDirectoryPage.handleViewDetails` pushes the detail path with the picked return state (D10) | task-117 |
| FR-13 | `maskAnalyticsRoute` helper + `MatomoClient.getAnalyticsURL` masking (D2/P-8) | task-117 |
| FR-14 | Sanitization suite +3 masking tests (23 total), zero modifications to the inherited 20 | task-117 |
| FR-15 | `VotingGovernancePage.spec.tsx`: stub removed, production `DRepDetailPage` registered on `ROUTES.GOVERNANCE.DREP_DETAIL`, two-hop test driven by the real CTAs, new forwarding-state pin; all slice-2/3 assertions byte-identical (D10) | task-117 |
| FR-16 | `research/slice-4-findings.md` records the D1 deferral/plan conflict, the D4 wireframe drop, and the masking boundary | task-117 |

---

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | react-intl 2.9 API only (`injectIntl`/`intlShape`/`defineMessages`); React Router 5.2 (`withRouter` + class containers) |
| NFR-2 | Every new/changed en-US **and** ja-JP string keeps the leading `!!!`; none removed |
| NFR-3 | Storybook uses the global English/Japanese toggle; no local `IntlProvider`, no per-locale story duplicates |
| NFR-4 | New Jest object-argument assertions use `expect.objectContaining` (prettier 2.1.2 oscillation guard) |
| NFR-5 | `node_modules/.bin/tsc --noEmit` zero errors after every task; scoped `node_modules/.bin/eslint` clean; no `.scss.d.ts` committed (global `declare module '*.scss'` covers the new SCSS) |
| NFR-6 | No nix in this devcontainer: `node_modules/.bin/prettier --write <changed .ts/.tsx/.scss>` substitutes `nix fmt` (never JSONs); `npx` itself is broken here — npm 11.13.0 rejects the repo's string-form `devEngines` before any tool runs, so every tool is invoked as `node_modules/.bin/<tool>` or `yarn <tool>` (guide's cross-cutting note); no push/PR |
| NFR-7 | Comments only where logic isn't self-evident, 1–3 plain lines, no task IDs/labels/history |

---

## Architecture: Data Flow (slice-4 delta)

```
DRepCard  ──"View details"──►  DRepDirectoryPage.handleViewDetails
                                  push(`/governance/dreps/${drepId}`,
                                       pickDelegationFormReturnState(location.state))
                                            │  { from, selectedWalletId, voteType } only
                                            ▼
Routes.tsx  ──exact DREPS──►  TrackedRoute DREP_DETAIL → DRepDetailPage (withRouter)
                                            │  entry = governance.drepIndex.get(match.params.drepId)
                                            │  refresh() on Idle/Failed + sync reaction (D9)
                                            ▼
DRepDetail ── header (id + copy confirmation; no badge)
           ── DRepDetailOnchainSection (status badge/expiry/voting power/votes-unavailable)
           ── DRepDetailAnchorSection (url/hash text + anchor-reference label)
           ── DRepDetailActions ──"Select for delegation"──►
                push(inherited?.from ?? '/voting/governance',
                     { ...inherited, selectedDRepId: match.params.drepId })   [byte-equal]

MatomoClient.getAnalyticsURL:  hash "governance/dreps/<id>" ─maskAnalyticsRoute─► "governance/dreps/:drepId"
```

The renderer reads `GovernanceStore` only; the detail triggers no per-DRep query — its
only data path is the existing bulk two-phase refresh (invariant #6, local-first #1).
No store gains state; the handoff lives exclusively in router `location.state`
(invariant #4).

---

## What Slice-4 Deliberately Does NOT Include

- ❌ Current-epoch vote positions / any `gov-state` query (D1 — deferred, recorded in findings)
- ❌ Favorite toggle or any favorites stub (D3 — slice-7 task-122)
- ❌ "Registered: epoch N" row (D4 — no data source; wireframe drift recorded)
- ❌ Dual CIP-129/CIP-105 display, `normalizeDRepIdentity`, any bech32 dependency (D5 — cv-1 task-129)
- ❌ "Expiring" badge variant or category badges (D6 — slice-5)
- ❌ Anchor fetch, verification, content render, or clickable anchor links (D7 — anchor-1/anchor-2, task-152)
- ❌ Search/deep-link entry UI (slice-6), favorites tab (slice-7)
- ❌ Removing any `!!!` marker (release-end user-owned review)

---

## Docs / Designs / Research / Workflows / Skills Consulted

- **Orchestration contract:** `prompt.md` (PRD fields :68-73, small-model bar :75-89,
  locked invariants :93-139, slice order :147-148, live-repo-wins rule :39-41)
- **Tracker:** `governance-drep-discovery-plan-tasks.json` (slice-4 phase; task-116/117
  `acceptanceCriteria`, `dependencies`, `targetPath`)
- **Plan:** Key Decisions (:127-165, esp. "DRep query shape" :138, status grounding
  :139), Renderer State (:223-229), Discovery UX (:231-241), slice sequencing
  (:263-297, anchor-1 dependency :296)
- **README:** binding scope :33-44 ("slice-4 ships on-chain fields + anchor presence
  only" :39), working conventions :10-19
- **Designs:** `drep-discovery-design.md` (detail wireframe :78-105, IA/route :27-49,
  component hierarchy :145-188, state treatments :190-205, anchor treatment :207-215,
  a11y :258-264); `designs/shared-design-tokens.md` §1 (status staging), §2 (source
  labels), §3 (voting-power forms), §4 (ID display + copy feedback), §9 (message-ID
  inventory), §10 (a11y floor)
- **Research:** `research/slice-2-findings.md` (D1 stub contract binding on slice-4,
  D3 raw-ID confirmation scope); `research/slice-3-findings.md` (I-1 jsonStrWithErrors,
  I-5 floor suite outside eslint gate, F-4 test vectors); `research/ux-refinement-findings.md`
  (F-2 `_shared` naming + variant unions, F-6 jest logging gotcha, F-7 i18n:manage OK,
  F-9 prettier drift trap)
- **Live seams:** every file in the guide, re-verified with line anchors on 2026-07-24
  (drift found vs the planning brief: harness routes at :165-175 not :169-174; the
  directory refresh seam spans :24-49; `pickDelegationFormReturnState` at :50-62; the
  floor suite is 20 tests, not slice-2's documented 17 — slice-3 added 3; §9 has no id
  for the anchor-reference label — P-1)
- **Workflows/skills applicable at build time:** `.agent/workflows/frontend.md`,
  `.agent/workflows/test.md`, `.agent/workflows/storybook.md`; skills `i18n-messaging`,
  `storybook-creation`, `git-commit-formatter` (subject-only), `evidence-rules`

---

## Locked Invariants Touched

| # | Invariant | How slice-4 honors it |
|---|---|---|
| 1 | Local-first | Detail reads `GovernanceStore.drepIndex` only; no per-DRep CLI/IPC; refresh is the existing bulk two-phase path; anchor URL renders as text, never fetched |
| 2 | Sanitization floor | Zero new logger/analytics/electron-store calls in components (the `drepIdLength`-only precedent stands); the D2 analytics-URL mask **closes a would-be leak** the new route creates; floor suite 20 inherited + 3 new, green after every task |
| 3 | Anchor transport floor (negative) | Nothing fetches; URL + hash are inert text under the anchor-reference label; no `onExternalLinkClick` on any anchor surface |
| 4 | No second delegation backend | Handoff via `location.state` + the existing pickers only; `VotingStore`/`GovernanceStore` byte-identical to base |
| 6 | CLI discipline (negative) | No new query; detail refresh = the same bulk `--all-dreps` path |
| 10 | Byte-equality | `match.params.drepId` used untransformed (bech32 has no URL-encoding characters) → `selectedDRepId` → pre-fill; pinned by the migrated two-hop test + the untouched payload test |
| 11 | Preliminary copy | Every new key `!!!` in both locales; no marker stripped |
| 13 | Form-only sentinels | Detail renders directory entries only; sentinels cannot reach the route (no sentinel ever enters `drepIndex`) |
| 14 | Status grounding | Badge = stored `active|inactive` only; expiry is a plain field, `expiring` stays slice-5 (D6) |

Not touched: #5 (no IPC change), #7/#8 (slice-5), #9 (cv-1), #12 (slice-7).

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| task-106 (GovernanceStore + `drepIndex`) | complete (`GovernanceStore.ts:57`) |
| `pickDelegationFormReturnState` forwarding contract | present (`delegationFormState.ts:50-62`, slice-2) |
| `DetailRouteStub` executable spec | present (`VotingGovernancePage.spec.tsx:74-99`) |
| `_shared` components (`DRepIdDisplay`, `DRepStatusBadge`, `DRepSourceLabel`) | present |
| `GOVERNANCE.ROOT`/`DREPS` routes + `Governance` nav container | present (`routes-config.ts:39-42`, `Routes.tsx:226-239`, `Governance.tsx`) |
| Sanitization floor suite baseline | 20/20 green (run verified 2026-07-24) |
| Jest harness precedents | `DRepDirectoryPage.spec.tsx`, `DRepDirectory.spec.tsx`, `VotingGovernancePage.spec.tsx` |

---

## Risks / Open Questions

| Risk | Mitigation |
|------|-----------|
| Detail route double-renders with the directory | D8: `exact` added in the same commit as the route; harness already models it; two-hop Jest would fail loudly otherwise |
| Analytics leak via `window.location.hash` | D2/P-8: masked at the single `getAnalyticsURL` boundary; 3 regression tests incl. the `track()` boundary; pageTitles are static strings |
| Migration silently weakens slice-2 pins | D10: only the two-hop test body and harness wiring change; the diff must leave the browse-push, single-hop, payload, and HW tests byte-identical (guide enumerates the allowed edits) |
| Duplicate visible "On-chain" text breaks queries | P-3: single provenance treatment per section; tests query distinct strings |
| Shared-component edits regress the directory | `showCopiedConfirmation` defaults off; `'on-chain'` variant unchanged; `DRepDirectory.spec.tsx` re-run in task-116's gate |
| `drepActivity` semantics mislead on inactive DReps | P-9 field rule |
| ja-JP copy quality | Preliminary `!!!` values; release-end user-owned review (invariant #11) |
| Storybook cannot be launched in this devcontainer (no display) | Stories verified at tsc/eslint level; eyeball in both locales via the global toggle before merge (slice-2 precedent) |

**Open questions:** none — D1–D12 and P-1…P-11 resolve all judgment calls; no
interactive checkpoint exists in this slice.

---

## Definition of Done

- [ ] task-116/117 each: acceptance criteria met, focused Jest green, code review clean,
      one subject-only commit (`<type>(gov): task-NNN …`), tracker JSON synchronized
      (`status`, `statusReason`, `evidence`, `updatedAt`)
- [ ] `node_modules/.bin/tsc --noEmit` zero errors and scoped eslint clean after every
      task (`npx` is unusable in this devcontainer — NFR-6)
- [ ] Sanitization floor: 20/20 after task-116; 23/23 after task-117; the 20 inherited
      tests unmodified
- [ ] Two-hop contract proven against the **production** detail route; slice-2/3
      assertions byte-identical
- [ ] Analytics URL for the detail route asserted as `…/governance/dreps/:drepId` at the
      `track()` boundary
- [ ] All new locale strings `!!!`-prefixed in both files; `yarn i18n:manage` clean
      (tool-managed `translations/messages.json` diffs ride with their task)
- [ ] Storybook detail stories render via the global locale toggle (no local IntlProvider)
- [ ] `research/slice-4-findings.md` written (D1 plan-conflict deferral, D4 wireframe
      drop, D2 masking boundary, any build-time findings)
- [ ] task-116 promoted beyond `complete` only with dedicated proof (none scheduled
      in-slice; the floor suite extension is task-117's own evidence)
- [ ] Final outcome below filled at slice close

---

## Final Outcome

Slice-4 is complete: both tasks shipped, each code-review approved on pass 1 with zero
blockers, one commit per task on `job/slice-4`.

**task-116 — DRep detail view (on-chain only)** (`34296ec16`,
`feat(gov): task-116 build DRep detail view with on-chain fields and anchor presence`).
The `drep-detail/` component tree (`DRepDetail`, `DRepDetailOnchainSection` with the
single status badge per the B-1 fix, `DRepDetailAnchorSection` rendering anchor URL +
hash as inert text under the "On-chain anchor reference" label, `DRepDetailActions`
select-CTA-only) plus a `DRepDetailPage` container replicating the directory refresh
contract with deep-link/not-found handling. Shared extensions: `DRepSourceLabel` gains
the `on-chain-anchor-reference` variant; `DRepIdDisplay` gains an opt-in copied
confirmation (defaults off — directory cards behaviorally unchanged). 21 `!!!`-prefixed
keys per locale, an 11-test container spec, five Storybook stories under the global
locale toggle.

**task-117 — detail route wiring + analytics mask** (`f4fec59c9`,
`feat(gov): task-117 wire DRep detail route and mask drep id from analytics urls`).
`ROUTES.GOVERNANCE.DREP_DETAIL` (`/governance/dreps/:drepId`) registered in
`Routes.tsx` with the mandatory `exact` added to the directory route (D8, no double
render); the card "View details" CTA threaded Directory → List → Card into
`DRepDirectoryPage.handleViewDetails`, which pushes the byte-equal DRep id via the
route param with only the picked `{ from, selectedWalletId, voteType }` state; the pure
`maskAnalyticsRoute` helper applied inside `MatomoClient.getAnalyticsURL` (D2) keeps
the id out of every tracked URL; the slice-2 `DetailRouteStub` harness migrated to the
production `DRepDetailPage` within the nine-edit whitelist (D10), slice-2/3 pins
byte-identical; one new `!!!` key per locale.

**Verification executed** (all via `node_modules/.bin/<tool>` — `npx` is unusable in
this devcontainer, F-6): `tsc --noEmit` zero errors after each task; scoped eslint 0
errors (warnings confined to pre-existing baseline classes); focused Jest green —
task-116: `DRepDetailPage` 11/11, directory regression 22/22, floor 20/20; task-117:
42/42 (`VotingGovernancePage` 8, `DRepDirectory` 20, `DRepDetailPage` 11,
`DRepDirectoryPage` 3), floor 23/23 with the inherited 20 tests byte-identical. The
two-hop delegation contract was proven against the production detail route with the
slice-2/3 final assertions unchanged; the tracked-URL boundary test pins
`http://daedalus/governance/dreps/:drepId` and asserts the CIP-129 id absent at the
real `sendEvent` path. Anchor-floor grep (`onExternalLinkClick|<a |href=`) over
`drep-detail/` came back empty; no logger/analytics/storage call exists in the new
code. Message JSONs (`defaultMessages.json`, `translations/messages.json`) were
regenerated tool-managed with each task. Storybook was verified at tsc/eslint level
only (no display in this devcontainer — slice-2 precedent); eyeball both locales via
the global toggle before merge.

**Deferrals.** Current-epoch vote positions → a future `gov-state` slice (D1/F-1: no
`gov-state` query exists; the detail renders a graceful "unavailable" value; the plan's
Key-Decisions row must be reconciled when that slice lands). Favorite toggle → slice-7
(D3). Dual-ID (CIP-105 alongside CIP-129) display → cv-1; slice-4 is CIP-129-only
(D5). The wireframe's "Registered: epoch N" row is dropped outright (D4/F-2:
`drep-state` carries no registration epoch — restoring it needs a new data source, not
a UI change).

**Research.** [research/slice-4-findings.md](../research/slice-4-findings.md) records
F-1…F-7: the vote-positions plan conflict, the registration-epoch data gap, the
`getAnalyticsURL` single-boundary mask, the `exact` requirement, the stub-migration
contract, the broken-`npx` devcontainer convention, and earlier-slice anchor/count
drift for future planners.

---

## References

- Parent plan: [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
- Task tracker: [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
- Implementation guide: [slice-4-implementation-guide.md](./slice-4-implementation-guide.md)
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md)
- Research: [slice-2-findings.md](../research/slice-2-findings.md),
  [slice-3-findings.md](../research/slice-3-findings.md),
  [ux-refinement-findings.md](../research/ux-refinement-findings.md)
