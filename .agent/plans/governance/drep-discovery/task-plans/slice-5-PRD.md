# Slice-5 PRD: Default Cohort + Category Badges + BMVG Banner

> **Planning Status:** approved | **Date:** 2026-07-24 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `slice-5` — "Slice 5 - Default cohort + category badges + BMVG banner" (riskLevel: medium)
> **Tasks:** task-118 → task-119 → task-120 (order forced by JSON `dependencies`: 120 depends on 118; 119's deps task-107 + task-116 are both complete)
> **Implementation guide:** [slice-5-implementation-guide.md](./slice-5-implementation-guide.md)

---

## Executive Summary

Slice-5 turns the raw full-list directory into the binding default cohort: exclude the
35 largest DReps by voting power, keep up to the next 200 eligible entries (active,
remaining `drepActivity` > 6 epochs), and display them in seeded-random order.
`GovernanceStore` takes ownership of the randomization-seed lifetime (one seed per app
session, preserved across refresh, replaced only by a Reshuffle action that never
queries) — closing a previously unowned risk named in the phase description
(tasks JSON, slice-5 `description`). Task-118 lands the cohort computed state, the seed
+ `reshuffleCohort()` action, the banner's primary cohort line + Reshuffle control, and
the ranking/randomization Jest. Task-119 adds the informational
`DRepCategoryBadge` (`_shared/`, Primary / Threshold / Non-metadata; High value waits
for anchor-1) rendered on every `DRepCard` and on `DRepDetail`, with tooltip copy and
the binding Threshold > Primary > Non-metadata priority. Task-120 appends the BMVG
citation as the banner's secondary line. All copy ships `!!!`-preliminary in en-US and
ja-JP.

**Why now:** the locked slice order (prompt.md:147) reaches slice-5 after slice-4
closed (commit `b6b94268e`); task-118's dependency task-107 and task-119's task-116 are
both `complete`; `anchor-2`'s `doNotList` exclusion depends on this cohort rule
(plan :297), and slice-6's search/show-all (task-121) builds on the cohort/banner seams
this slice creates.

---

## Problem Statement

The directory currently renders **every** DRep in CLI order
(`DRepDirectoryPage.tsx:84` passes `governanceStore.drepList` straight through). That
maximizes popularity bias — the exact failure mode the BMVG-derived default is designed
to avoid (README:36) — and makes no per-DRep evaluation statement. There is no
randomization, no seed owner, no cohort eligibility rule, no category explanation, and
no user-facing disclosure that a default view even exists. Slice-5 ships the binding
default (invariant #7), the explanation surface (category badges, invariant #8), and
the transparency surface (cohort banner + BMVG citation) in one coherent slice.

---

## Per-Task Contract (interaction modes, scope, dependencies)

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-118** — Default-cohort rule + randomization seed in GovernanceStore | `autonomous` | `seededShuffle` util (mulberry32 + Fisher–Yates, no new dependency — R8); `GovernanceStore` gains `cohortSeed` observable, `reshuffleCohort()` action, `isCohortActive` / `defaultCohort` / `displayedDRepList` computeds (P-1…P-4); container passes the displayed list + cohort flag + reshuffle callback; `DRepDirectoryBanner` gains the primary cohort line + Reshuffle link (R6/R7); `cohortBanner` + `cohortBanner.reshuffle` keys en-US + ja-JP; cohort Jest in `GovernanceStore.spec.ts` + util spec + directory/container/harness spec updates; stories compile fixes | No search/show-all or `cohortBanner.showAll` key (slice-6 task-121 — R5); no BMVG line (task-120); no filter-active banner copy switch (tokens :86, slice-6); no `doNotList` handling (anchor-2, README:72); no main-process/IPC change of any kind | task-107 ✔ (`complete`) |
| **task-119** — `DRepCategoryBadge` (Primary / Threshold / Non-metadata) | `autonomous` | New `_shared/DRepCategoryBadge.tsx` + `.scss` on the `DRepStatusBadge` pattern (:25-42); exported pure `getDRepCategory` (P-6) with binding priority Threshold > Primary > Non-metadata (tokens :39); native-`title` tooltip (P-7); rendered in `DRepCard` topRow and `DRepDetail` header (P-8); 6 i18n keys per locale (tokens :186-193); component spec + snapshot tests at both call sites (P-9); one Storybook story under the global locale toggle | No High value category or its keys (anchor-1 — tokens :28, README:68); no verified-metadata flag consumption (task-151; interim anchor-presence proxy — R3); no cohort ordering/filtering influence in any code path (invariant #8); no `DRepStatusBadge` change (R4) | task-107 ✔, task-116 ✔ (both `complete`) |
| **task-120** — BMVG citation secondary line | `autonomous` | `DRepDirectoryBanner` renders the §9 `cohortBanner.source` string beneath the primary cohort line (P-10); `showSource` story-only prop default `true` (P-11); banner spec; banner stories with/without the citation slot; 1 key per locale | No copy beyond the §9 pre-assigned string (P-10); no narrow-width tooltip collapse (P-12 — accepted gap); no dismissal control (banner not dismissible, tokens :85) | task-118 |

None of the three tasks is in the locked non-autonomous set (task-125, task-166
remainder, task-158, release-end `!!!` review — prompt.md:176-180). Planning surfaced
**no blocking decisions**: the eight orchestrator resolutions R1–R8 below plus planner
resolutions P-1…P-14 close every open question.

---

## Orchestrator Resolutions R1–R8 (verified against live code/docs; residuals recorded)

- **R1 — Ranking derives ONLY from Phase-2-enriched BigNumber voting power.** Verified:
  the two-phase load is live (`GovernanceStore.fetchDRepList` :113-162 paints Phase 1
  with `votingPower` null; `_enrichVotingPower` :169-195 merges the
  `governanceDRepStakeChannel` payload as `new BigNumber(stake)` at :179). The plan's
  Key-Decisions "DRep query shape" row (:139) still says "cohort ranking in slice-5 may
  derive top-35 from the inline `--include-stake` per-DRep stake" — **stale** (the
  slice-1 inline-stake read was replaced by ux-refinement's two-phase load, findings
  F-1). *Residual:* recorded as a findings item; the plan row is reconciled at slice
  close.
- **R2 — The cohort exists only when `votingPowerState === Loaded`.** While `Loading`,
  the directory keeps today's Phase-1 full-list paint; on `Failed`
  (`isRankingUnavailable` :102-104) the full list renders with the existing
  `rankingUnavailable` banner (`DRepDirectory.tsx:168-170`) and **no cohort claim** —
  the banner's cohort line renders only when the cohort is active. Verified to compose
  with pagination: `DRepDirectoryList` recomputes `totalPages` from the entries prop
  and resets an out-of-bounds page to 0 (:47-55), so cohort activation (235-entry list
  → ≤200 cohort) and reshuffle reorders are safe. *No residual.*
- **R3 — Interim metadata completeness: anchor presence, badge-only.** Until anchor-1
  (task-151) supplies a verified-metadata flag, metadata completeness does **not**
  gate cohort eligibility (eligibility = `status === 'active'` AND remaining
  `drepActivity` > 6). On-chain anchor presence (`entry.anchor != null`,
  `governance.types.ts:60-62`) is the interim completeness proxy and feeds ONLY the
  category badge (Primary vs Non-metadata). This keeps Non-metadata DReps reachable in
  the default view — consistent with tokens §1a (:35, Non-metadata is a cohort-eligible
  category) and the README scoping the metadata criterion to "when verified anchor
  pipeline lands" (:36). *Residual:* interim state, documented here and in the guide;
  anchor-1 upgrades the proxy.
- **R4 — The "Expiring soon" status-badge variant is OUT of slice-5.** Conflict
  verified: tokens §1 says "`Expiring soon` joins with the slice-5 Threshold category
  window" (:20, table row :13), but no slice-5 task in the tasks JSON asks for it and
  canonical `DRepStatus` stays `'active' | 'inactive'` (`governance.types.ts:35`,
  invariant #14). `DRepStatusBadge.tsx` is untouched this slice; the **Threshold
  category badge** covers the 7–12-epoch window display. *Residual:* design-vs-tasks
  conflict recorded as a findings item; the status-badge variant needs a future owner.
- **R5 — Banner scope: primary cohort line + Reshuffle + BMVG secondary line only.**
  No Show-all / Search links (slice-6 task-121); the shipped `cohortBanner` key carries
  no link placeholders, deliberately diverging from the tokens §5 sentence containing
  `{ShowAllLink} or {SearchLink}` (:84) — the §9 key inventory already models this by
  keeping `cohortBanner` (:168) link-free with `cohortBanner.showAll` as a separate key
  (:169) that slice-6 will consume. Keys this slice: `governance.drepDirectory.cohortBanner`,
  `.cohortBanner.reshuffle` (task-118), `.cohortBanner.source` (task-120). *No residual.*
- **R6 — Extend the existing `DRepDirectoryBanner.tsx`** (63 lines; title :44, Refresh
  :45-50, lastUpdated :52-58; rendered from `DRepDirectory.tsx:184-188`). No second
  banner component. *No residual.*
- **R7 — Key/task assignment.** task-118 lands the cohort logic, the banner primary
  cohort line, and the Reshuffle control (+ `cohortBanner`, `cohortBanner.reshuffle`
  keys, both locales). task-120 adds only the BMVG secondary line
  (`cohortBanner.source`) and its story variants. *No residual.*
- **R8 — Deterministic seeded shuffle, no new dependency.** Fisher–Yates driven by an
  inline mulberry32 PRNG (grep-verified: no shuffle/seeded-random util exists in
  `source/renderer/app`). The seed is created once per session at store construction,
  preserved across `refresh()`, replaced ONLY by `reshuffleCohort()` (which triggers no
  fetch). Cohort ordering is a pure function of (enriched list, seed) — see P-3 for the
  strict stability construction. *No residual.*

### Planner decisions P-1…P-14 (recorded by the planner)

- **P-1 (store seam):** the cohort lives in `GovernanceStore` as computeds —
  `isCohortActive` (`votingPowerState === Loaded && drepList.length > 0`),
  `defaultCohort` (`AppDRepDirectoryEntry[] | null`, null unless active), and
  `displayedDRepList` (`defaultCohort ?? drepList`). The container swaps
  `drepList={governanceStore.displayedDRepList}` (currently `drepList`,
  `DRepDirectoryPage.tsx:84`) and passes `isCohortActive` + `onReshuffle`, so the
  banner's claim and the rendered list share one source of truth and can never diverge.
- **P-2 (deterministic ranking):** rank by BigNumber `comparedTo` descending with a
  drepId-ascending tie-break — a total, deterministic order (invariant #5: never
  coerce to `Number`). Entries with `votingPower === null` (absent from the stake map,
  `_enrichVotingPower` :179) rank below every non-null entry and can never occupy a
  top-35 slot; they remain cohort-eligible (eligibility ignores voting power — R3).
- **P-3 (canonicalized shuffle input):** after rank-ordered selection of the ≤200
  eligible entries, the selection is re-sorted by drepId ascending **before** the
  seeded shuffle. Display order is therefore a pure function of (membership set, seed):
  a refresh that jiggles voting powers without changing membership yields a
  byte-identical order — the strict reading of R8's stability clause, pinned by Jest.
- **P-4 (PRNG/seed):** mulberry32 + Fisher–Yates + `generateCohortSeed()`
  (`Math.floor(Math.random() * 2**32) >>> 0`) live in a new
  `source/renderer/app/utils/seededShuffle.ts` with a co-located spec
  (`mithrilBehindness.spec.ts` precedent). The seed value is never logged — the entire
  cohort code path makes zero `logger`/analytics/storage calls (invariant #2).
- **P-5 (Reshuffle control):** a react-polymorph `Link` with explicit `skin={LinkSkin}`
  (slice-4 P-6 precedent — specs render without a ThemeProvider harness), rendered in
  the banner only when the cohort is active. §5 calls it "a subtle 'Reshuffle' link"
  (:87).
- **P-6 (category rules reduce to two inputs):** under R3, the §1a slice-5 rules
  reduce to: `drepActivity` in [7,12] → Threshold; else `anchor != null` → Primary;
  else Non-metadata. `status` is not consulted in slice-5 (an inactive DRep has
  `drepActivity` 0/null — `governance.types.ts:37-38` — and falls to Primary /
  Non-metadata by anchor presence). The exported pure function takes the entry
  snapshot (`Pick<AppDRepDirectoryEntry, 'status' | 'drepActivity' | 'anchor'>`) so
  anchor-1 can extend the rules without a signature change.
- **P-7 (tooltip mechanism):** native `title` + `aria-label`
  (`DRepCard.tsx:69-78` voting-power-placeholder precedent). No PopOver dependency.
- **P-8 (badge placement):** card — inside `topRow` immediately after
  `DRepStatusBadge` (`DRepCard.tsx:82-85`); detail — inside the `styles.header` flex
  row after `DRepIdDisplay` (`DRepDetail.tsx:101-103`), **not** inside the "On-chain"
  section: the category is renderer-derived, and placing it under the On-chain heading
  would misstate provenance (tokens §2's anti-misleading rule). Each surface renders
  exactly one category badge (tokens :26).
- **P-9 (snapshot tests):** the repo has zero existing snapshot tests (grep-verified).
  The acceptance's "snapshot tests cover both call sites" is satisfied with Jest
  `toMatchSnapshot()` on the badge DOM node in `DRepDirectory.spec.tsx` (card site) and
  `DRepDetailPage.spec.tsx` (detail site); the generated `__snapshots__/*.snap` files
  are committed with task-119.
- **P-10 (BMVG copy):** the user-facing string is the §9 pre-assigned copy —
  "Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation
  analysis." (:194). The task description's longer phrasing ("…Phase-1 … (2026-05-19)")
  stays in docs/tracker, not UI copy; the string is `!!!`-preliminary and the
  release-end review owns final wording. Minor task-text vs token-inventory
  discrepancy, recorded here.
- **P-11 (with/without citation story):** banner gains `showSource?: boolean`
  defaulting to `true`; production call sites never pass it (the citation is never
  removed in production — tokens :84 "must never be removed"). The `false` variant
  exists only for the acceptance-required story. The source line renders only when
  `isCohortActive && showSource` (no cohort claims when the cohort is inactive — R2).
- **P-12 (narrow-width collapse):** §5's "may collapse into a tooltip on narrow widths"
  (:84) is optional and NOT implemented; the line wraps/reflows per the §9 JA-length
  rule (:220, ≥2 wrapped lines, reflow not ellipsize). Accepted gap.
- **P-13 (static banner copy):** the primary line's "up to 200" stays static — the §9
  key (:168) has no `{n}` placeholder, so no count interpolation is added.
- **P-14 (harness ripple):** the container's switch to `displayedDRepList` /
  `isCohortActive` / `reshuffleCohort` requires the plain-object governance-store mocks
  in `DRepDirectoryPage.spec.tsx` (:26-33) and `VotingGovernancePage.spec.tsx` (:87-95)
  to gain those three fields, and `DRepDirectory.stories.tsx` /
  `DRepDirectory.spec.tsx` to pass the two new required component props. All updated in
  task-118 (slice-4 step-13 "stories compile fixes" precedent); no slice-2/3/4
  assertion changes.

---

## User Stories

### US-5.1 — Browse an unbiased default view
**As a** Daedalus user opening the DRep directory,
**I want** a default view that excludes the giants and randomizes the rest,
**So that** my delegation choice is not steered by popularity ranking.

**Acceptance:**
- With voting power loaded, the directory shows only the default cohort: top 35 by
  BigNumber voting power excluded, up to the next 200 eligible (active, remaining
  `drepActivity` > 6 epochs), in seeded-random order.
- While voting power is loading, the full Phase-1 list shows (today's behavior); when
  the stake phase failed, the full list shows with the existing `rankingUnavailable`
  banner and no cohort claim (R2).
- Excluded/non-cohort DReps stay in `drepList`/`drepIndex` and remain reachable via
  the detail route; search/show-all reachability lands in slice-6 (no work here).

### US-5.2 — Understand and control the randomization
**As a** user seeing an order I didn't choose,
**I want** the banner to say what the default view is and a Reshuffle control,
**So that** refresh-induced reorder is never mysterious and I can reorder on demand.

**Acceptance:**
- The banner's primary line renders the §9 `cohortBanner` copy whenever the cohort is
  active; it is not dismissible.
- Reshuffle replaces the session seed and reorders the cohort **without any CLI query
  or IPC re-fetch** (invariant #6); explicit Refresh preserves the seed; membership is
  unchanged by Reshuffle.
- The seed is created once per app session at store construction.

### US-5.3 — See why a DRep is categorized
**As a** user comparing DReps,
**I want** exactly one informational category badge with a tooltip on every card and on
the detail view,
**So that** I can see at a glance whether a DRep has metadata or is approaching expiry.

**Acceptance:**
- Primary / Threshold / Non-metadata render with the §1a rules, labels, and tooltip
  copy; High value is absent until anchor-1.
- A DRep in the 7–12-epoch window shows Threshold even with metadata (binding
  priority, unit-tested tie-break).
- The badge never reorders, filters, or overrides the cohort in any code path
  (invariant #8) — it has no callbacks and its module is imported by render surfaces
  only, never by `GovernanceStore`.

### US-5.4 — Know where the default comes from
**As a** user (or auditor) questioning the "35/200" numbers,
**I want** the banner to credit the BMVG analysis,
**So that** the cohort sizing is transparent, not arbitrary.

**Acceptance:**
- The §9 `cohortBanner.source` line renders beneath the primary cohort line in both
  locales whenever the cohort line renders; stories cover with/without the citation
  slot (defaults to with).

---

## Functional Requirements

| ID | Requirement | Owner |
|----|------------|-------|
| FR-1 | `seededShuffle.ts`: `mulberry32` (private), `generateCohortSeed()`, `seededShuffle<T>(items, seed)` — pure, input-non-mutating; co-located spec | task-118 |
| FR-2 | `GovernanceStore`: `cohortSeed` observable (initialized at construction), `reshuffleCohort()` action (reseed only, zero IPC), `isCohortActive` / `defaultCohort` / `displayedDRepList` computeds implementing P-1…P-3 with module-level constants 35/200/6 | task-118 |
| FR-3 | `DRepDirectoryPage` passes `displayedDRepList`, `isCohortActive`, `onReshuffle`; `DRepDirectory` threads both new props to the banner | task-118 |
| FR-4 | `DRepDirectoryBanner`: primary cohort line + Reshuffle `Link` rendered only when `isCohortActive`; `cohortBanner` + `cohortBanner.reshuffle` keys (`!!!`, both locales) | task-118 |
| FR-5 | Cohort Jest: 11 store cases (activation gating, top-35 exclusion, lossless boundary ranking, eligibility floor incl. the binding >6 rule, 200 cap, seed determinism/stability, membership-stable reshuffle without IPC, seed-preserving refresh, reachability) + 5 util cases + 4 directory-component cases + container reshuffle wiring | task-118 |
| FR-6 | `DRepCategoryBadge.tsx` + `.scss`: exported `getDRepCategory` (P-6), badge render on the `DRepStatusBadge` pattern, native-title tooltip (P-7); 6 keys per locale | task-119 |
| FR-7 | Badge rendered in `DRepCard` topRow and `DRepDetail` header (P-8); snapshot tests at both call sites (P-9); component spec covers rules incl. the 7–12-with-metadata tie-break, window edges 6/7/12/13, null activity, ja-JP | task-119 |
| FR-8 | `DRepCategoryBadge` Storybook story rendering all three categories under the global locale toggle | task-119 |
| FR-9 | `DRepDirectoryBanner`: `cohortBanner.source` secondary line (P-10) gated `isCohortActive && showSource` (P-11); banner spec; banner stories with/without citation (defaults with) | task-120 |
| FR-10 | `yarn i18n:manage` run after every copy change; tool-managed JSON diffs ride with their task | all |

---

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | react-intl 2.9 API only (`injectIntl`/`intlShape`/`defineMessages`); no hooks |
| NFR-2 | Every new en-US **and** ja-JP string keeps the leading `!!!`; none removed |
| NFR-3 | Storybook uses the global English/Japanese toggle; no local `IntlProvider`, no per-locale story variants |
| NFR-4 | New Jest object-argument assertions use `expect.objectContaining` (prettier 2.1.2 oscillation guard) |
| NFR-5 | `node_modules/.bin/tsc --noEmit` zero errors after every task; scoped `node_modules/.bin/eslint` clean; no `.scss.d.ts` committed (global `declare module '*.scss'` covers new SCSS — ux-refinement F-2) |
| NFR-6 | No nix / broken `npx` in this devcontainer (slice-4 F-6): every tool via `node_modules/.bin/<tool>` or `yarn <tool>`; scoped prettier substitutes `nix fmt` (never on JSON); no push/PR |
| NFR-7 | Comments only where logic isn't self-evident, 1–3 plain lines, no task IDs/labels/history |
| NFR-8 | Voting power stays `BigNumber` end-to-end in ranking; no `.toNumber()` anywhere in the cohort path |

---

## Architecture: Data Flow (slice-5 delta)

```
governanceDRepStakeChannel ──Phase 2──► GovernanceStore._enrichVotingPower (:169-195)
                                            │ votingPowerState = Loaded
                                            ▼
GovernanceStore (all renderer-side; ZERO new CLI/IPC):
  cohortSeed (session; reshuffleCohort() reseeds, refresh() preserves)
  isCohortActive   = votingPowerState === Loaded && drepList.length > 0
  defaultCohort    = shuffle( canonicalize( take200eligible( dropTop35( rankBigNumber(drepList) ))), cohortSeed )
  displayedDRepList = defaultCohort ?? drepList          [full list while Loading/Failed — R2]
                                            │
DRepDirectoryPage ── drepList={displayedDRepList} · isCohortActive · onReshuffle ──► DRepDirectory
                                            │
        DRepDirectoryBanner ── cohort line + Reshuffle (118) + BMVG source line (120), only when active
        DRepDirectoryList ── pagination recomputes/resets on entry changes (:47-55)
              DRepCard ── DRepStatusBadge · DRepCategoryBadge(entry) ── informational only
DRepDetail (header) ── DRepIdDisplay · DRepCategoryBadge(entry)
```

Reshuffle and cohort computation never touch `GovernanceQueryService`,
`governanceChannel`, or any main-process file (invariant #6: bulk query once per
refresh; reshuffle reseeds without re-querying). `drepList`/`drepIndex` stay complete —
the cohort is a derived view, never a mutation.

---

## What Slice-5 Deliberately Does NOT Include

- ❌ High value category, its i18n keys, or any cohort-median computation (anchor-1 — tokens :28, README:68)
- ❌ Verified-metadata flag consumption (anchor-1 task-151; interim anchor-presence proxy per R3)
- ❌ "Expiring soon" status-badge variant or any `DRepStatusBadge`/`DRepStatus` change (R4; invariant #14)
- ❌ Show-all / search / `cohortBanner.showAll` / filter-active banner copy / "Excluded from default cohort" top-35 badge (slice-6 — tokens :15, design :226-236)
- ❌ `doNotList` exclusion (anchor-2; accepted interim per README:72)
- ❌ Any main-process, `GovernanceQueryService`, or IPC-channel change
- ❌ Seed persistence across app restarts (seed is per-session — README:57)
- ❌ Removing any `!!!` marker (release-end user-owned review)

---

## Docs / Designs / Research / Workflows / Skills Consulted

- **Orchestration contract:** `prompt.md` (PRD fields :68-73, small-model bar :75-89,
  locked invariants :93-139 esp. #7 :121-124 / #8 :125-127, slice order :147,
  live-repo-wins rule :39-41, non-autonomous set :176-180)
- **Tracker:** `governance-drep-discovery-plan-tasks.json` slice-5 phase (task-118/119/120
  `acceptanceCriteria`, `dependencies`, `targetPath`; phase `description` naming the
  seed-ownership risk)
- **Plan:** requirements :83-86; Key Decisions "DRep query shape" :139 (stale — R1),
  "Default cohort" :143, "Randomization seed" :144, "Recommended framing" :145,
  "High Value badge" :146, "DRep category badges" :147; Track D :283; anchor-2 cohort
  dependency :297; `doNotList` risk :333
- **README:** binding scope :33-44 (BMVG sizing :36, Recommended framing :37, badge
  surface :38, slice-5 staging :43); Locked Decisions :54-58 (seed lifetime :57);
  stakeholder resolutions :60-68 (6-epoch floor :66, High value staging :68);
  `doNotList` interim :72
- **Designs:** `shared-design-tokens.md` §1 status staging :20 (R4 conflict), §1a
  :24-39 (rules table :30-35, priority :39), §5 :80-87 (banner copy :84, not
  dismissible :85, seed ownership :87), §9 keys :168-194 + JA-length :220 + ja
  placeholders :222; `drep-discovery-design.md` state treatments :190-205 (two-phase
  rows :195-199), default-cohort UX :217-224
- **Research:** `research/ux-refinement-findings.md` (F-1 two-phase as built, F-6 jest
  logging gotcha, F-7 i18n:manage OK, F-9 prettier drift trap);
  `research/slice-4-findings.md` (F-6 broken npx, F-7 stale-anchor warning);
  `research/drep-state-preprod-epoch295-sample.json` (258-DRep realistic scale)
- **Live seams:** every file in the guide, re-verified with line anchors on 2026-07-24
  at HEAD `b6b94268e` (notably: store :19-30/:57-74/:78-104/:113-201; container
  :82-95; banner 63 lines; card :81-98; detail :101-113; on-chain section :98-100;
  status badge :25-42; list pagination :10/:45-61; error banner variant :16; stories
  :136-154/:235-371; store spec mocks :17-31 + helper :41-53; en-US governance keys
  :284-329)
- **Workflows/skills applicable at build time:** `.agent/workflows/frontend.md`,
  `.agent/workflows/test.md`, `.agent/workflows/storybook.md`; skills `i18n-messaging`,
  `storybook-creation`, `git-commit-formatter` (subject-only), `evidence-rules`

---

## Locked Invariants Touched

| # | Invariant | How slice-5 honors it |
|---|---|---|
| 2 | Sanitization floor | Cohort/seed/badge code makes **zero** logger/analytics/electron-store calls — no DRep id, no `abstain`/`no_confidence` literal, no CIP-129/CIP-105 string can reach any sink because nothing in the new code writes to one; the 23-test spy suite (`tests/jest/security/governance-sanitization.spec.ts`) re-run green after every task |
| 5 | Lovelace losslessness | Ranking compares rehydrated `BigNumber`s via `comparedTo` (P-2); a Jest case pins correct ordering at the top-35 boundary for values one lovelace apart beyond `Number` precision; no `.toNumber()` in the diff |
| 6 | CLI discipline | Zero main-process/IPC changes; the cohort is computed from the already-loaded list; `reshuffleCohort()` reseeds only — Jest asserts both channel mocks' call counts are unchanged by a reshuffle; refresh remains the existing bulk two-phase path |
| 7 | Default cohort binding | Exclude top 35; up to next 200 eligible (`active` AND `drepActivity > 6`), randomized; the 6-epoch floor is strict (`> 6`, so 6 is out) and Jest-pinned; **no fixture places a sub-floor DRep inside a cohort** (sub-floor values appear in fixtures only to assert their exclusion); the cohort IS "Recommended" — no tab, no per-card Recommended badge anywhere in the diff |
| 8 | Badges informational only | `DRepCategoryBadge` exports a pure classifier + a presentational component with no callbacks; it is never imported by `GovernanceStore` or any ordering/filtering code; the cohort computeds never read categories |
| 11 | Preliminary copy | All 9 new keys per locale `!!!`-prefixed; no marker stripped; `yarn i18n:manage` after each copy change |
| 14 | Status grounding | `DRepStatus` union untouched; the Threshold **category** is renderer-derived display state; no `expiring` status value introduced (R4) |

Not touched: #1 (no new data source — only existing store data re-derived), #3 (no
anchor fetch; anchor presence read as a boolean proxy only), #4/#10 (delegation handoff
unchanged), #9 (no delegation defaults — the cohort recommends a *view*, never picks a
DRep), #12 (slice-7), #13 (sentinels can never enter `drepList`, so never the cohort).

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| task-107 (two-phase directory list + refresh) | complete (`GovernanceStore.ts:113-201`) |
| task-116 (DRep detail render surface for the badge) | complete (`DRepDetail.tsx:97-114`) |
| Phase-2 stake enrichment (`governanceDRepStakeChannel`) | live (`governanceChannel.ts:18`, store :172) |
| `DRepStatusBadge` pattern to copy | present (`_shared/DRepStatusBadge.tsx:25-42` + `.scss`) |
| `rankingUnavailable` error banner | present (`_shared/DRepErrorBanner.tsx:16`, rendered `DRepDirectory.tsx:168-170`) |
| Pagination out-of-bounds reset | present (`DRepDirectoryList.tsx:47-55`) |
| Sanitization floor suite baseline | 23 tests green (grep-verified 3 describes, 23 `it(` cases) |
| Jest harness precedents | `GovernanceStore.spec.ts`, `DRepDirectory.spec.tsx`, `DRepDirectoryPage.spec.tsx`, `DRepDetailPage.spec.tsx`, `VotingGovernancePage.spec.tsx` |

---

## Risks / Open Questions

| Risk | Mitigation |
|------|-----------|
| Cohort silently biased by lossy ranking | P-2 BigNumber comparator + boundary Jest with values differing by 1 lovelace above 2^53 |
| Reshuffle accidentally wired to a fetch | Jest pins channel-mock call counts unchanged across `reshuffleCohort()`; the action body is a single seed assignment |
| Shuffle order churns on every refresh | P-3 canonicalized shuffle input; Jest pins order stability under voting-power jiggle with unchanged membership |
| Banner claims a cohort that isn't showing | P-1 single source of truth: `displayedDRepList` and `isCohortActive` derive from the same computed chain; R2 Jest covers Loading/Failed |
| A ≤35-DRep network yields an empty cohort with the "up to 200" banner | Accepted edge (copy says "up to"); preprod reality is 258 DReps; slice-6 show-all is the escape hatch. Recorded, not built around |
| Category badge added to cards breaks existing text queries | New labels are distinct strings; task-119 verification re-runs `DRepDirectory`, `DRepDetailPage`, and `VotingGovernancePage` suites |
| Container prop rename ripples into harnesses | P-14 enumerates all four affected harness/story files; updated in task-118 with no slice-2/3/4 assertion changes |
| Badge SCSS contrast (WCAG AA, tokens :18) unverifiable in this devcontainer | Fallback hexes follow the shipped `DRepStatusBadge.scss` token families (info/warning/neutral); final contrast check rides the theme pass / release verification (task-125) |
| ja-JP copy quality | Preliminary `!!!` values; release-end user-owned review (invariant #11) |
| Storybook cannot be launched here (no display) | Stories verified at tsc/eslint level; eyeball both locales via the global toggle before merge (slice-2/4 precedent) |

**Open questions:** none — R1–R8 and P-1…P-14 resolve all judgment calls; no
interactive checkpoint exists in this slice. **Recorded conflicts for the findings
note:** R1 (stale plan Key-Decisions row), R4 (tokens §1 "Expiring soon in slice-5" vs
tasks JSON), P-10 (task-text vs §9 BMVG copy).

---

## Definition of Done

- [ ] task-118/119/120 each: acceptance criteria met, focused Jest green, code review
      clean, one subject-only commit (`<type>(gov): task-NNN …`), tracker JSON
      synchronized (`status`, `statusReason`, `evidence`, `updatedAt`)
- [ ] `node_modules/.bin/tsc --noEmit` zero errors and scoped eslint clean after every
      task (`npx` unusable here — NFR-6)
- [ ] Sanitization floor 23/23 after every task; zero modifications to the suite
- [ ] Cohort invariants Jest-pinned: top-35 exclusion, strict >6 floor, 200 cap,
      BigNumber boundary ranking, seed-per-session, reshuffle-without-IPC,
      refresh-preserves-seed, membership-stable order
- [ ] Badge priority Threshold > Primary > Non-metadata unit-tested incl. the
      metadata-in-window tie-break; snapshots at both call sites committed
- [ ] Banner: cohort line + Reshuffle render only when the cohort is active; BMVG line
      beneath it by default; no cohort claim in Loading/Failed states
- [ ] All new locale strings `!!!`-prefixed in both files; `yarn i18n:manage` clean
      (tool-managed `defaultMessages.json`/`translations/messages.json` diffs ride with
      their task)
- [ ] Storybook: category-badge story + banner with/without-citation stories +
      directory story compile fixes, all under the global locale toggle
- [ ] `research/slice-5-findings.md` written at slice close (R1 plan-row conflict, R4
      design-vs-tasks conflict, P-10 copy discrepancy, any build-time findings)
- [ ] No task promoted past `complete` without dedicated proof (none scheduled
      in-slice)
- [ ] Final outcome below filled at slice close

---

## Final Outcome

_To be filled at slice close._

---

## References

- Parent plan: [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
- Task tracker: [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
- Implementation guide: [slice-5-implementation-guide.md](./slice-5-implementation-guide.md)
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md)
- Research: [ux-refinement-findings.md](../research/ux-refinement-findings.md),
  [slice-4-findings.md](../research/slice-4-findings.md)
