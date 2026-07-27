# Slice-6 PRD: DRep ID Search + Show-All / Reachability Filters

> **Planning Status:** approved | **Date:** 2026-07-24 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `slice-6` — "Slice 6 - Search & show-all" (riskLevel: medium)
> **Tasks:** task-121 (single-task slice; dependency task-118 is `complete`)
> **Implementation guide:** [slice-6-implementation-guide.md](./slice-6-implementation-guide.md)

---

## Executive Summary

Slice-6 makes every registered DRep reachable without weakening the slice-5 default
cohort. Task-121 ships (a) the shared-design-tokens §11 DRep-ID search contract —
8-character post-HRP minimum, prefix matches that never auto-select, exact
checksum-valid full-ID entry that opens the detail view directly, both CIP-105 and
CIP-129 encodings searched and deduplicated by underlying credential, bech32
validation run entirely in the renderer — and (b) the show-all / reachability filter
framework (status, metadata, expiry-threshold window, top-35 exclusion,
default-cohort vs show-all view, favorited predicate, search) with the opt-in
show-all sorts and the popularity-sort disclosure from the design's
"Show-All Without Re-introducing Bias" section (design :226-236). Everything is a
pure renderer derivation over the already-loaded `drepList` / `drepIndex` — **zero
new IPC, zero CLI queries, zero logging** (the search query is never logged
anywhere). The banner switches to the "Showing {n} DReps matching your filters"
copy whenever the pure default view no longer applies, so the randomization claim
is never false.

**Why now:** the locked slice order (prompt.md:147) reaches slice-6 after slice-5
closed (commit `9a17bc891`); task-121's only dependency task-118 is `complete`
(tracker-verified; cohort seams live at `GovernanceStore.ts:143-180`); and slice-7's
favorites (task-122) explicitly depends on the filter framework this slice creates
("the generic favorited filter predicate is owned by the slice-6 filter framework
(task-121)" — task-122 description, tasks JSON).

---

## Problem Statement

Since slice-5 the directory renders **only** the default cohort once voting power
loads (`DRepDirectoryPage.tsx:84` passes `displayedDRepList`). The top-35, sub-floor
(≤6 remaining epochs), and inactive DReps are invisible except via a manually typed
detail URL — slice-5 findings F-5 records reachability as an open gap owned by
task-121. There is no search, no filters, no show-all, and the banner cannot yet
describe a filtered view. Invariant #12 (reachability of excluded/non-cohort DReps)
is unsatisfied until this slice lands.

---

## Per-Task Contract (interaction mode, scope, dependencies)

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-121** — DRep ID search + show-all / reachability filters | `autonomous` | New `drep-directory/helpers.ts` (pure search/filter/sort functions on the `stake-pools/helpers.ts` precedent) + co-located spec; new `DRepDirectorySearch.tsx` + `DRepDirectoryFilters.tsx` (+`.scss`); `GovernanceStore` gains `top35DRepIds` + `showAllList` computeds (renderer-derived, zero IPC); `DRepDirectory` gains view state (query, show-all, facets, sort) and the visible-entries pipeline; `DRepDirectoryBanner` gains the filtered-view line (D-4 minted key); `DRepEmptyState` gains the `noResults` variant with Clear-filters / Show-all actions; `DRepDirectoryPage` passes `drepIndex` / `showAllList` / `top35DRepIds`; 21 i18n keys per locale (6 §9-inventoried + 15 minted, all `!!!`); Jest per acceptance rule (store, helpers, component, container, banner) + harness/story compile fixes | No search IPC or main-process change of any kind (D-1); no favorites persistence, favorite toggle, or Favorited UI control / `filter.favorited` key (task-122 — D-3); no "Expiring soon" status badge (D-5); no "Excluded from default cohort" top-35 badge (P-13); no verified-name search (deferred beyond v1 — tokens §11 :246); no dual-ID stacked card display (pre-existing drift, P-14); no new Storybook stories (P-16); no `DRepStatus`/`DRepStatusBadge`/`DRepCategoryBadge`/`seededShuffle` change | task-118 ✔ (`complete`) |

task-121 is not in the locked non-autonomous set (task-125, task-166 remainder,
task-158, release-end `!!!` review — prompt.md:176-180). Planning surfaced **no
blocking decisions**: drift resolutions D-1…D-5 plus planner resolutions P-1…P-16
below close every open question.

---

## Drift Resolutions D-1…D-5 (required by the orchestrator; verified against live code/docs)

- **D-1 — No search IPC exists, and none may be added.** Verified: the only
  governance channels are `governanceDRepListChannel` / `governanceDRepStakeChannel`
  (`GovernanceStore.ts:4-7`); no search endpoint exists in
  `source/main/governance/GovernanceQueryService.ts` and this slice touches nothing
  under `source/main/`. Search is a **pure renderer filter** over the already-loaded
  `drepList` / `drepIndex` (store :90, :93). Tokens §11's "no IPC call is made" (:241)
  and "Validation before IPC … before `GovernanceQueryService` is called" (:245)
  clauses are therefore satisfied **structurally**: bech32 validation
  (`Cardano.DRepID.isValid`, the task-103 checks — precedent
  `VotingPowerDelegation.tsx:133`) gates only the renderer-side
  exact-match/detail-open path, and Jest pins that no channel request and no
  `refresh()` fires during any search interaction. *No residual.*
- **D-2 — The 8-character minimum counts characters AFTER the HRP.** Tokens §11
  (:241) is explicit: "Minimum prefix length: 8 characters (after the `drep1` /
  CIP-129 HRP prefix)". The task JSON's bare "minimum 8-character prefix before
  filtering" wording is the imprecise restatement; **§11 is the named design
  contract and wins**. Implementation: the normalized query is split into
  (`drep`/`drep_script` HRP | none) + data part; the data part must be ≥ 8
  characters to activate filtering; below that the input shows the §11 help text
  and the list is unaffected. `drep1` + 7 characters is below minimum; 8 bare
  characters with no HRP is at minimum. Jest pins both boundaries. *Recorded; no
  residual.*
- **D-3 — Favorited filter: plumb the predicate against an empty favorites source.**
  Favorites persistence is task-122 (slice-7), which **depends on this task** and
  whose tracker description already assigns ownership: "The generic favorited filter
  predicate is owned by the slice-6 filter framework (task-121); [slice-7] owns
  favorites persistence + the toggle and feeds the favorited set into that
  framework, so filter semantics are defined in exactly one place." Decision:
  `helpers.ts` owns the `favoritedOnly` facet and its predicate
  (`favoriteDRepIds.has(entry.drepId)`), the filter context carries
  `favoriteDRepIds: ReadonlySet<string>`, and `DRepDirectory` accepts an optional
  `favoriteDRepIds` prop defaulting to a shared frozen empty set — so the contract
  exists and is Jest-covered (with an injected non-empty set) **without dead UI**:
  no Favorited control is rendered and no `filter.favorited` key lands this slice
  (an always-empty favorites source would make the control a guaranteed-zero-results
  trap). Task-122 renders the control, adds the key, and feeds the persisted set —
  with zero change to filter semantics. *No residual.*
- **D-4 — The filtered-banner copy has no §9 message ID: one is minted.** Tokens §5
  (:86) specifies the copy — "Showing {n} DReps matching your filters. Default
  randomized order does not apply." — but the §9 inventory (:157-210) has no ID for
  it. Minted in the §9 naming style, inside the existing `cohortBanner.*` family
  (`cohortBanner` :168, `.showAll` :169, `.reshuffle` :170, `.source` :194):
  **`governance.drepDirectory.cohortBanner.filtered`**, en source = the full §5
  sentence pair with `{n}`. Recorded here and in the guide's key table; the
  slice-close findings note flags the §9 inventory gap. *Residual:* §9 needs the row
  added at a future tokens edit (docs-only).
- **D-5 — The "Expiring soon" badge drift is NOT adopted.** Design :221 says the
  "Expiring in {n} epochs" status badge "may also appear for entries surfaced via
  search / show-all that fall below the floor" — but that badge variant is the same
  unowned drift slice-5 R4 declined (slice-5 findings F-4): no task in the tasks
  JSON asks for it, canonical `DRepStatus` stays `'active' | 'inactive'`
  (`governance.types.ts:35`, invariant #14), and `DRepStatusBadge.tsx` is untouched.
  Sub-floor, inactive, and top-35 entries surfaced via search/show-all render the
  **existing** status + category badges only — no new badge, no status value, no new
  badge key. *Residual:* the status-badge variant still needs a future owner
  (carried from slice-5 F-4).

### Planner decisions P-1…P-16 (recorded by the planner)

- **P-1 (store seams):** `GovernanceStore` gains two computeds, both pure
  derivations of existing state: `top35DRepIds: Set<string>` (ids of the 35
  largest by BigNumber ranking; **empty unless
  `votingPowerState === Loaded`** — honoring the shipped
  `rankingUnavailable` copy "Ranking-based filters disabled",
  `DRepErrorBanner.tsx:8-9`) and `showAllList: AppDRepDirectoryEntry[]` (the FULL
  `drepList` — every registration — drepId-canonicalized then `seededShuffle`d with
  the existing session `cohortSeed`, so show-all shares the cohort's seed lifetime:
  stable across refresh, reordered only by Reshuffle). `displayedDRepList`
  (:178-180) is untouched. The shared drepId comparator is extracted to a
  module-level `compareDRepIdAsc` reused by `defaultCohort`.
- **P-2 (view-state locality):** search query, show-all, facets, and sort are React
  component state in `DRepDirectory` (the `stake-pools/helpers.ts` +
  `DRepDirectoryList` `useState` precedent), never store observables and never
  persisted. Nothing in the new code calls `logger.*`, analytics, or electron-store
  — the search query in particular **never reaches any sink** (invariant #2).
- **P-3 (search matching semantics):** queries are trimmed + lowercased, then split
  on a leading `drep_script1` / `drep1` HRP. Matching is prefix-only (v1 contract,
  tokens §11 :246 / design :236): HRP-qualified queries match full-form
  `startsWith` against the encodings carrying that HRP; bare queries match the
  post-HRP data part of **both** encodings. Each index row is one entry keyed by
  its canonical CIP-129 id, so a credential matching via both forms yields exactly
  one row — dedupe-by-credential holds by construction and is Jest-pinned (the same
  entry found via its CIP-105 form and via its CIP-129 form is one identical row).
  "Sorted by relevance only" reduces, for prefix-only matching, to a deterministic
  drepId-ascending order (recorded interpretation).
- **P-4 (exact-match open):** reactive, not submit-driven — there is **no Enter
  handler at all**, so a prefix can never auto-open (§11 "the user must explicitly
  pick a row" holds even for a unique match + Enter). When the input value becomes
  a checksum-valid full bech32 (`Cardano.DRepID.isValid`), it is canonicalized via
  `toCip129DRepID` and looked up in `drepIndex`; on a hit, `onViewDetails` fires
  once with the **canonical CIP-129 id** (entries are stored CIP-129 —
  `GovernanceQueryService.ts:624-641` derives ids with `cip129FromCredential`), so
  a pasted CIP-105 form opens the same detail route as its CIP-129 twin. A
  checksum-valid id **not** in the index falls through to the `noResults` empty
  state without navigating (auto-navigation to a not-found page on paste would be a
  worse failure mode than an inline empty state; the detail route's own
  `notFound` state remains the deep-link fallback).
- **P-5 (invalid full-form detection):** a query with an HRP whose data part is at
  least 51 characters (the complete CIP-105 data-part length; CIP-129 is 53 —
  derived from `@cardano-sdk/core` payload lengths, `DRepID.js:31-33`) but which
  fails `isValid` is classified `invalidFullForm`: the search surface shows the
  "Invalid DRep ID" error (mirroring the existing task-103 copy
  `voting.governance.drepInputError`, en-US.json:926 / ja-JP.json:926) and the list
  shows no results; nothing navigates and nothing is queried. Shorter or HRP-less
  strings can never be full-form and stay in prefix mode.
- **P-6 (dual-encoding derivation):** `buildDRepSearchIndex` derives each entry's
  CIP-105 form once per list identity via
  `Cardano.DRepID.toCip105DRepID(Cardano.DRepID(id))` (memoized with `useMemo`),
  falling back to `null` on any throw (the entry stays searchable via its CIP-129
  form). **No new bech32 dependency** — `@cardano-sdk/core` is already a renderer
  dependency (`VotingPowerDelegation.tsx:9`, `dataSerialization.ts:6`) and its
  `Cardano.DRepID` API (`isValid` / `toCredential` / `toCip105DRepID` /
  `toCip129DRepID`, d.ts :4-13) covers validation and both encodings.
- **P-7 (show-all = the full list):** show-all renders **every** registration —
  top-35, sub-floor, and inactive included — not just "all eligible + top-35"
  (design :228's narrower phrasing). Grounds: the task acceptance requires
  "excluded top-35 **and non-cohort** DReps" reachable, and design :221 itself
  contemplates sub-floor entries "surfaced via search / show-all". The phrasing
  delta is recorded for the findings note. Show-all order is the seeded-random
  session order (P-1), so enabling it never introduces ranking bias by default.
- **P-8 (facet model):** `DRepFilterState` = `status: all|active|inactive`,
  `metadata: all|withMetadata|withoutMetadata` (on-chain `anchor` presence — the
  same interim proxy as slice-5 R3), `expiry: all|thresholdWindow` (remaining
  `drepActivity` in [7,12]), `excludeTop35: boolean`, `favoritedOnly: boolean`.
  Facets apply in both views (cohort and show-all) and on top of search results.
  `excludeTop35` is offered only under show-all with ranking loaded (the cohort
  already excludes the top 35; without ranking the set is unknowable — P-1).
- **P-9 (sorts):** opt-in and show-all-only (design :228): `randomized` (default —
  returns the seeded order untouched), `votingPowerDesc`, `votingPowerAsc`,
  `expiryAsc`. Voting-power sorts compare `BigNumber` via `comparedTo` — never
  `.toNumber()` (invariant #5, boundary Jest at 1 lovelace beyond `Number`
  precision); `null` voting power and `null` drepActivity always sort last;
  drepId-ascending tie-break. Active search suppresses the sort control and sort
  application ("Search results are sorted by relevance only" — design :236).
  Leaving show-all resets sort to `randomized`. The popularity-sort disclosure
  (`showAll.sortBiasWarning`, §9 :218) renders exactly while
  `sort === 'votingPowerDesc'` — the design's "dismisses with the same user action
  that returns to default sort. Dismissal is not persisted" reduces to a pure
  render condition with no dismiss control.
- **P-10 (banner mode):** the banner is in **filtered mode** whenever the pure
  default view no longer applies: search active (including invalid-full-form),
  show-all on, any facet non-default, or a sort override. Filtered mode replaces
  the cohort claim + Reshuffle + BMVG line with the D-4 line
  (`{n}` = the count of currently visible entries). The BMVG citation renders only
  with the cohort claim it explains — §5's "must never be removed" governs the
  default banner (the citation must not be dropped from it), not the
  filtered-copy state, whose §5-specified copy contains no citation. The filtered
  line renders independently of `isCohortActive` (searching while ranking failed
  still shows an accurate count).
- **P-11 (show-all control placement):** one control, not two — a react-polymorph
  `Checkbox` in `DRepDirectoryFilters` labeled with the §9 key
  `cohortBanner.showAll` ("Show all DReps"). No banner link is added: slice-5 R5
  locked the link-free `cohortBanner` sentence (divergence from §5's
  `{ShowAllLink} or {SearchLink}` placeholders, recorded in slice-5 findings F-5),
  and a second toggle in the banner would duplicate state.
- **P-12 (noResults empty state):** `DRepEmptyState` gains the designed
  `noResults` variant (design :184, :203) using the §9 key `empty.noResults` with
  `{ClearFilters}` / `{ShowAll}` rendered as react-polymorph `Link`s
  (`FormattedMessage` element values — `VotingPowerDelegation` precedent).
  Clear-filters resets search + facets + sort (stays in the current view); Show-all
  activates show-all and resets search + facets. The `{ShowAll}` link label reuses
  `cohortBanner.showAll` (no second "show all" string). The bare
  "No DReps found on-chain" state now triggers only when the **full** list is empty
  (`showAllList.length === 0`); an empty cohort with data present (the ≤35-DRep
  network edge accepted in slice-5) falls to `noResults` with a working Show-all
  escape hatch — closing that accepted edge.
- **P-13 (top-35 "Excluded from default cohort" badge NOT built):** tokens §1 (:15)
  and design :222 describe it, and slice-5's non-goals pointed it at slice-6 — but
  no tasks-JSON task asks for it (task-121's criteria don't mention it), it has no
  §9 message ID, and D-5's principle (search/show-all surfaces render existing
  badges only) covers it. Declined on the slice-5 R4 precedent (tasks JSON wins
  over design drift); recorded as unowned design drift for the findings note,
  needing a future owner alongside the "Expiring soon" variant.
- **P-14 (dual-ID stacked rows deferred):** §11 :244 says the deduped row "shows
  both ID forms stacked", and design :240-242 calls card identity "the dual-ID
  display" — but the shipped `DRepIdDisplay` renders the single CIP-129 form
  (:69-77), a pre-existing gap from slice-2/3 (reviews approved). Card identity is
  not in task-121's acceptance; the dedupe requirement itself is fully met (one row
  per credential). Recorded as pre-existing drift for the findings note; no card
  change this slice.
- **P-15 (facet/sort controls are native `<select>` elements):** the three facet
  dropdowns and the sort dropdown are native `<select>`s with `aria-label`s
  (styled via SCSS); the two toggles (show-all, exclude-top-35) are react-polymorph
  `Checkbox`es. Rationale: react-polymorph `Select` interaction is untestable-in-jsdom
  territory for the RTL harness this feature uses, and slice-5 P-7 set the
  precedent of preferring native elements (native `title` tooltip over PopOver)
  when the polymorph widget adds risk without value. `fireEvent.change` on a
  labeled native select is deterministic.
- **P-16 (no new stories):** task-121's acceptance asks for Jest + i18n only (the
  phase's "In-slice coverage" is test coverage; contrast task-122, which names
  Storybook explicitly). `DRepDirectory.stories.tsx` receives compile fixes for the
  new required props (slice-5 P-14 precedent); banner stories are untouched because
  the banner's new props are optional with production-safe defaults.

---

## User Stories

### US-6.1 — Find any DRep by ID
**As a** Daedalus user who knows (part of) a DRep ID,
**I want** to search by CIP-105 or CIP-129 ID from the directory,
**So that** I can find any registered DRep — including the top-35 and non-cohort ones.

**Acceptance:**
- Below 8 post-HRP characters, the help text shows and the list is unaffected
  (D-2); no query of any kind fires (D-1).
- At ≥ 8 characters, prefix matches list with full IDs; nothing auto-selects and
  nothing opens — even a unique match with Enter pressed (P-4).
- A checksum-valid full ID (either encoding) opens the detail view directly with
  the canonical CIP-129 route id; a full-form-shaped string with a bad checksum
  shows "Invalid DRep ID" and never navigates (P-4/P-5).
- The same credential matched via both encodings is one result row (P-3).

### US-6.2 — See everything, deliberately
**As a** user who wants the complete picture,
**I want** a Show-all toggle and directory filters,
**So that** excluded and non-cohort DReps are reachable without changing my default.

**Acceptance:**
- Show-all renders every registration in the seeded session order (P-7/P-1);
  turning it off restores the untouched default cohort.
- Status / metadata / expiry-window facets apply in both views; exclude-top-35 is
  available under show-all when ranking is loaded (P-8).
- Sorts are opt-in under show-all only; choosing voting-power-descending shows the
  §9 sort-bias disclosure until the sort returns to default (P-9).
- Zero filter results show the noResults state with working Clear-filters /
  Show-all actions (P-12).

### US-6.3 — Never be misled by the banner
**As a** user filtering or searching,
**I want** the banner to stop claiming a randomized default,
**So that** the cohort explanation is never false.

**Acceptance:**
- Any deviation from the pure default view switches the banner to
  "Showing {n} DReps matching your filters. Default randomized order does not
  apply." with a live count (P-10, D-4 key).
- The cohort line, Reshuffle, and BMVG citation render only in the pure default
  view (P-10).

### US-6.4 — Favorites contract without favorites (framework only)
**As a** slice-7 implementer,
**I want** the favorited predicate and plumbing to already exist,
**So that** task-122 only feeds a persisted set and renders a control.

**Acceptance:**
- `favoritedOnly` facet + predicate live in `helpers.ts`, Jest-covered with an
  injected non-empty set; `DRepDirectory` threads `favoriteDRepIds` (default
  empty); no Favorited UI and no `filter.favorited` key ship (D-3).

---

## Functional Requirements

| ID | Requirement | Owner |
|----|------------|-------|
| FR-1 | `helpers.ts`: `normalizeDRepQuery`, `getDRepQueryKind` (empty / belowMinimum / prefix / exactValid / invalidFullForm; 8-char post-HRP minimum, 51-char full-form floor), `buildDRepSearchIndex` (CIP-105 derivation, null on throw), `searchDRepsByIdPrefix` (both encodings, dedupe by construction, drepId-asc order), `resolveExactDRepMatch` (isValid → CIP-129 canonicalization → `drepIndex` lookup), `filterDReps` + `DRepFilterState` + `DEFAULT_DREP_FILTER_STATE` + `isDefaultFilterState` + `EMPTY_DREP_ID_SET`, `sortDReps` — all pure, input-non-mutating, zero logging | task-121 |
| FR-2 | `GovernanceStore`: `top35DRepIds` (empty unless `votingPowerState === Loaded`) and `showAllList` (full list, drepId-canonicalized, seeded-shuffled with `cohortSeed`) computeds; `compareDRepIdAsc` extracted and reused by `defaultCohort`; no other store change, no IPC | task-121 |
| FR-3 | `DRepDirectorySearch` (input + placeholder + min-length hint + invalid-ID error) and `DRepDirectoryFilters` (show-all Checkbox, three native facet selects, exclude-top-35 Checkbox, sort select per P-8/P-9/P-15) | task-121 |
| FR-4 | `DRepDirectory`: view state + `visibleEntries` pipeline (search over full membership → facets → opt-in sort), reactive exact-match open (P-4), sort-bias disclosure, `isFilteredView` + count to the banner, noResults wiring, empty-state precedence on `showAllList` (P-12) | task-121 |
| FR-5 | `DRepDirectoryBanner`: `cohortBanner.filtered` line replaces cohort claim + Reshuffle + BMVG in filtered mode (optional props, default off) | task-121 |
| FR-6 | `DRepEmptyState`: `noResults` variant with `{ClearFilters}` / `{ShowAll}` Link values | task-121 |
| FR-7 | `DRepDirectoryPage` passes `drepIndex`, `showAllList`, `top35DRepIds`; no container logic change | task-121 |
| FR-8 | 21 `!!!` keys per locale (guide key table: 6 §9-inventoried, 15 minted incl. D-4); `yarn i18n:manage` run; tool-managed JSON diffs ride with the commit | task-121 |
| FR-9 | Jest one-per-acceptance-rule: 8-char minimum (both boundaries), no auto-select on prefix (incl. Enter on a unique match), exact-match detail open (both encodings → canonical id), dual-encoding search deduped by credential, pre-IPC bech32 validation (invalid full-form: error + zero channel/refresh calls), the full filter set (incl. favorited via injected set), reachability of top-35 and non-cohort entries via show-all and via search, show-all sorts incl. the BigNumber boundary case, banner filtered mode, noResults actions, ja-JP smoke | task-121 |

---

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | react-intl 2.9 API only (`injectIntl`/`intlShape`/`defineMessages`/`FormattedMessage`); no hooks from react-intl |
| NFR-2 | Every new en-US **and** ja-JP string keeps the leading `!!!`; none removed |
| NFR-3 | No new Storybook stories; existing stories compile via prop fixes only; global locale toggle untouched |
| NFR-4 | New Jest object-argument assertions use `expect.objectContaining` (prettier 2.1.2 oscillation guard) |
| NFR-5 | `node_modules/.bin/tsc --noEmit` zero errors; scoped `node_modules/.bin/eslint` clean; no `.scss.d.ts` committed |
| NFR-6 | Devcontainer discipline: no nix, `npx` broken — every tool via `node_modules/.bin/<tool>` or `yarn <tool>`; scoped prettier on changed `.ts/.tsx/.scss` only (never JSON/locale/defaultMessages/translations/`.snap`); no push, no PR |
| NFR-7 | Comments only where logic isn't self-evident, 1–3 plain lines, no task IDs/labels/history |
| NFR-8 | Voting power stays `BigNumber` end-to-end in sorts; no `.toNumber()`/`Number(` on any voting-power value |

---

## Architecture: Data Flow (slice-6 delta)

```
GovernanceStore (renderer only; ZERO new CLI/IPC):
  drepIndex (:90)                     — exact-match lookup source
  displayedDRepList (:178-180)        — unchanged default-view base
  top35DRepIds   = ids of ranked[0..34] when votingPowerState === Loaded, else ∅
  showAllList    = seededShuffle(canonicalize(drepList), cohortSeed)   [full membership]
        │
DRepDirectoryPage ── + drepIndex · showAllList · top35DRepIds ──► DRepDirectory
        │                                            view state: query · showAll · facets · sort
        │   visibleEntries = sort?(filter(facets, base))
        │   base = searchActive ? searchByPrefix(fullMembership) : showAll ? showAllList : drepList
        │   exactValid(query) ∧ drepIndex hit ──► onViewDetails(canonical CIP-129 id)   [no IPC]
        │
        ├── DRepDirectorySearch (placeholder · min-length hint · invalid-ID error)
        ├── DRepDirectoryFilters (show-all ✓ · status/metadata/expiry selects · exclude-top-35 ✓ · sort)
        ├── DRepDirectoryBanner (cohort claim+Reshuffle+BMVG ⟷ "Showing {n} …" filtered line)
        ├── sort-bias disclosure (iff sort === votingPowerDesc)
        └── DRepDirectoryList (pagination auto-resets out-of-bounds page :47-55)
              └── DRepEmptyState noResults ({ClearFilters} · {ShowAll}) when visibleEntries = ∅
```

Nothing touches `GovernanceQueryService`, `governanceChannel`, or any main-process
file. `drepList`/`drepIndex` stay complete; every new list is a derived view.

---

## What Slice-6 Deliberately Does NOT Include

- ❌ Any search IPC, main-process, or CLI change (D-1)
- ❌ Favorites persistence, favorite toggle, Favorited filter UI, `filter.favorited` key (task-122 — D-3)
- ❌ "Expiring soon" status-badge variant or any `DRepStatus` change (D-5; unowned drift carried from slice-5 F-4)
- ❌ "Excluded from default cohort" top-35 badge (P-13; unowned drift, recorded)
- ❌ Dual-ID stacked card display / any `DRepIdDisplay`/`DRepCard` change (P-14; pre-existing drift, recorded)
- ❌ Verified-name search (deferred beyond v1 — tokens §11 :246)
- ❌ New Storybook stories (P-16) or the `filter.favorited`/`category.highValue`/status keys
- ❌ Changes to `seededShuffle.ts`, `DRepCategoryBadge`, `DRepStatusBadge`, the cohort computeds' semantics, or the sanitization suite
- ❌ Removing any `!!!` marker (release-end user-owned review)

---

## Docs / Designs / Research / Workflows / Skills Consulted

- **Orchestration contract:** `prompt.md` (slice order :147, per-slice planning
  :158-171, non-autonomous set :176-180, dependencies-authoritative rule :151-153)
- **Tracker:** `governance-drep-discovery-plan-tasks.json` — slice-6 phase +
  task-121 (`acceptanceCriteria`, `dependencies: [task-118]` ✔ complete,
  `targetPath: source/renderer/app/components/`); task-122 (favorited-predicate
  ownership statement grounding D-3); task-103/106/118 statuses
- **Designs:** `shared-design-tokens.md` §11 :237-246 (the complete v1 ID-search
  contract — D-2's authority), §9 :157-222 (key inventory + Additional keys :214-218
  + JA-length rule :220), §5 :80-87 (banner copy switch :86 — D-4), §1 :13-15
  (Expiring soon / Top-35 rows — D-5/P-13), §1a :24-39 (7–12 window definition);
  `drep-discovery-design.md` :145-252 — component hierarchy :157-186
  (DRepDirectorySearch/Filters/helpers slots), state treatments :190-205,
  default-cohort UX :217-224 (:221 sub-floor-via-show-all, :222 top-35 badge),
  show-all/sort/search :226-236, ID-only identity :238-252
- **Precedent docs:** `slice-5-PRD.md` (R2/R4/R5, P-6/P-7/P-14 precedents),
  `slice-5-implementation-guide.md` (conventions + verification-block style),
  `slice-5-code-review.md` (transcript format)
- **Research:** `research/slice-5-findings.md` (F-2 cohort gating, F-4 expiring
  drift, F-5 link-free banner + reachability gap, F-6 seed lifetime);
  `research/slice-4-findings.md` (F-6 broken npx); `research/ux-refinement-findings.md`
  (F-7 i18n:manage OK, F-9 prettier drift)
- **Live seams (re-verified 2026-07-24 at HEAD `6f828d573`):**
  `GovernanceStore.ts` :90 (drepIndex), :110 (cohortSeed), :138-148, :157-180
  (cohort chain + displayedDRepList), :284-287 (reshuffleCohort);
  `DRepDirectoryPage.tsx` :68-73 (handleViewDetails), :82-97 (render props);
  `DRepDirectory.tsx` :58-72 (Props), :89 (hasRetainedData), :132-143 (bare-empty
  case), :145-182 (default branch), :188-194 (banner render);
  `DRepDirectoryList.tsx` :10, :47-55 (pagination reset);
  `DRepEmptyState.tsx` :16 (variant union); `DRepDirectoryBanner.tsx` :45-54,
  :85-101; `DRepIdDisplay.tsx` :69-77; `staking/stake-pools/helpers.ts` :1-24;
  `VotingPowerDelegation.tsx` :133 + en-US.json:926 (task-103 validation + copy);
  `GovernanceQueryService.ts` :624-641 (CIP-129 id derivation);
  `node_modules/@cardano-sdk/core/dist/cjs/Cardano/Address/DRepID.{d.ts,js}`
  (API surface + accepted forms + payload lengths); spec/story harnesses
  (`DRepDirectory.spec.tsx` :40-84, `DRepDirectoryPage.spec.tsx`,
  `VotingGovernancePage.spec.tsx`, `DRepDirectory.stories.tsx`)
- **Workflows/skills applicable at build time:** `.agent/workflows/frontend.md`,
  `.agent/workflows/test.md`; skills `i18n-messaging`, `git-commit-formatter`
  (subject-only), `evidence-rules`

---

## Locked Invariants Touched

| # | Invariant | How slice-6 honors it |
|---|---|---|
| 2 | Sanitization floor | Every new code path makes **zero** `logger.*`/analytics/electron-store calls — the search query, DRep ids, and filter state never reach any sink; no `abstain`/`no_confidence` literal or CIP-129/CIP-105 string in any payload because no payload exists; the 23-test spy suite (`tests/jest/security/governance-sanitization.spec.ts`) is never edited and re-runs 23/23 after the task |
| 5 | Lovelace losslessness | `sortDReps` voting-power sorts use `BigNumber.comparedTo` with null-last + drepId tie-break; a Jest case pins ordering for values one lovelace apart beyond `Number` precision; no `.toNumber()`/`Number(` in the diff |
| 6 | CLI discipline | Zero main-process/IPC changes; search/filter/sort/show-all derive from already-loaded state; Jest pins channel mocks and `refresh` uncalled across search interactions (D-1); `top35DRepIds`/`showAllList` are computeds over existing observables |
| 7 | Default cohort binding | The default view is byte-identical to slice-5 (`displayedDRepList` untouched); show-all/search are explicit, user-initiated escape hatches; no fixture places a sub-floor DRep inside a *cohort* (sub-floor fixtures exist only in show-all/search reachability assertions); no Recommended tab/badge |
| 8 | Badges informational | No badge module is imported by any ordering/filtering code: `helpers.ts` restates the 7–12 window constants locally instead of importing `getDRepCategory`; badges gain no callbacks; filters read entry fields only |
| 11 | Preliminary copy | All 21 new keys per locale `!!!`-prefixed; no marker stripped; `yarn i18n:manage` after the copy change |
| 12 | Reachability | Satisfied by this slice: top-35, sub-floor, and inactive DReps reachable via show-all, via ≥8-char search, and via exact full-ID entry — each path Jest-pinned |
| 14 | Status grounding | `DRepStatus` union untouched; no `expiring` value; no status-badge variant (D-5) |

Not touched: #1 (no new data source), #3 (no anchor fetch; anchor read as boolean
presence only), #4/#10 (delegation handoff unchanged — `handleViewDetails`/
`handleSelectForDelegation` bodies byte-identical), #9 (no delegation defaults),
#13 (sentinels can never enter `drepList`, so never any derived view).

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| task-118 (default cohort + seed) | `complete` (tracker; `GovernanceStore.ts:143-180`, commit `c4bafff9c`) |
| task-103 bech32 validation precedent | `complete` (`VotingPowerDelegation.tsx:133`; `voting.governance.drepInputError` en:926/ja:926) |
| `@cardano-sdk/core` `Cardano.DRepID` API | present (d.ts :4-13: `isValid`, `toCredential`, `toCip105DRepID`, `toCip129DRepID`, `cip129FromCredential`); already a renderer import — no new dependency |
| CIP-129 canonical ids in `drepList`/`drepIndex` | live (`GovernanceQueryService.ts:624-641`) |
| `seededShuffle` util | present (`source/renderer/app/utils/seededShuffle.ts`, slice-5) |
| Pagination out-of-bounds reset (search/filter shrink safety) | present (`DRepDirectoryList.tsx:47-55`) |
| `rankingUnavailable` banner ("Ranking-based filters disabled" promise) | present (`DRepErrorBanner.tsx:6-11`) |
| Sanitization floor suite baseline | 23 `it(` cases green (grep-verified) |
| Jest harness precedents | `GovernanceStore.spec.ts`, `DRepDirectory.spec.tsx`, `DRepDirectoryPage.spec.tsx`, `DRepDirectoryBanner.spec.tsx`, `VotingGovernancePage.spec.tsx` |

---

## Risks / Open Questions

| Risk | Mitigation |
|------|-----------|
| Exact-match auto-open surprises on paste | Scoped tightly: fires only for a checksum-valid full ID that resolves in `drepIndex` (P-4); valid-but-unknown ids stay inline (noResults); Jest pins one `onViewDetails` call with the canonical id and zero calls for every other kind |
| A prefix that is also a valid full ID (impossible-by-construction check) | `getDRepQueryKind` checks `isValid` first; kinds are mutually exclusive and unit-tested at the 7/8 and 50/51 boundaries |
| CIP-105 derivation throws on exotic ids | `buildDRepSearchIndex` try/catches per entry → `null` (CIP-129 search still works); helpers spec covers an invalid-id fixture |
| Search/filter shrink strands pagination | Existing `DRepDirectoryList` out-of-bounds reset (:47-55) recomputes; no change needed |
| Banner claims randomization while filtered | P-10 single `isFilteredView` predicate derived from the same state as the pipeline; banner spec covers both modes |
| `showAllList` reshuffles when membership changes mid-session | Same canonicalize-then-shuffle construction as the cohort (pure function of membership + seed); store spec pins stability and reshuffle behavior with zero channel calls |
| react-polymorph Select untestable in jsdom | Avoided by design — native selects + Checkboxes (P-15) |
| eslint jsx-a11y flags on native selects | `aria-label` on every select; if a legacy `no-onchange` rule fires, scoped eslint output is reviewed at verification (expected off in this config) |
| ja-JP copy quality | Preliminary `!!!` values; release-end user-owned review (invariant #11) |
| §9 inventory drift (D-4 minted key + 14 support keys) | All mints recorded in the PRD + guide key table; findings note flags the §9 gap for a docs pass |

**Open questions:** none — D-1…D-5 and P-1…P-16 resolve all judgment calls; no
interactive checkpoint exists in this slice. **Recorded conflicts/drift for the
findings note:** D-4 (§9 missing filtered-banner ID + minted support keys), D-5 +
P-13 (unowned badge drifts, carried), P-7 (design :228 "all eligible + top-35"
phrasing vs full-list show-all), P-14 (dual-ID stacked rows pre-existing gap).

---

## Definition of Done

- [ ] task-121 acceptance criteria met; focused Jest green; code review clean; one
      subject-only commit (`feat(gov): task-121 …`); tracker JSON synchronized
      (`status`, `statusReason`, `evidence`, `updatedAt`)
- [ ] `node_modules/.bin/tsc --noEmit` zero errors; scoped eslint clean
- [ ] Sanitization floor 23/23; suite file byte-identical; zero new
      logger/analytics/storage calls in the diff; the search query provably
      unlogged (grep the diff for `logger.`, `analytics`, `electron-store`)
- [ ] §11 contract Jest-pinned: 8-char post-HRP minimum boundaries, no auto-select
      on prefix (incl. Enter), exact-match open with CIP-129 canonicalization from
      both encodings, dedupe by credential, invalid-full-form error with zero
      queries
- [ ] Filter set Jest-pinned: status, metadata, expiry window (6/7/12/13 edges),
      exclude-top-35, favorited (injected set), show-all, search composition
- [ ] Reachability Jest-pinned: top-35 + sub-floor + inactive entries visible under
      show-all and findable via search
- [ ] Sorts Jest-pinned: BigNumber boundary, nulls-last, expiryAsc, randomized
      passthrough; sort-bias disclosure appears/disappears with votingPowerDesc
- [ ] Banner filtered mode + noResults actions covered; default view byte-identical
      when no search/filter/sort/show-all is active
- [ ] All 21 keys per locale `!!!`-prefixed; `yarn i18n:manage` clean; tool-managed
      JSON diffs ride with the commit
- [ ] Stories/harnesses compile with the new required props; no new stories
- [ ] `research/slice-6-findings.md` written at slice close (D-4 §9 gap, D-5/P-13
      unowned badges, P-7 phrasing delta, P-14 dual-ID gap, build-time findings)
- [ ] Final outcome below filled at slice close

---

## Final Outcome

Slice-6 is complete: its single task shipped, passed code review on round 1 with
zero blockers, and is `complete` in the tracker (not `verified` — that status needs
dedicated proof beyond the task's own unit tests, which slice-8's
release-verification acceptance provides). At close the full diff sits on
`wt/slice-6` (base `6f828d573`); the single subject-only commit
(`feat(gov): task-121 …`) is the implementer's close-out step, per the round-1
review note.

**task-121 — DRep ID search + show-all / reachability filters** — **complete**.
New pure-function module `drep-directory/helpers.ts` (query normalization +
kind classification with the 8-char post-HRP minimum and 51-char full-form floor,
dual-encoding search index deduped by credential by construction, exact-match
resolution canonicalized to CIP-129, the eight-facet `filterDReps` +
`DRepFilterState`, and opt-in `sortDReps` with lossless `BigNumber.comparedTo`)
with a 24-test node-environment spec. `GovernanceStore` gains exactly two
computeds — `top35DRepIds` (empty unless ranking Loaded,
`GovernanceStore.ts:193-201`) and `showAllList` (full membership,
drepId-canonicalized, seeded with the session `cohortSeed`, :203-211) — with
`displayedDRepList` and the cohort rule untouched. New `DRepDirectorySearch` +
`DRepDirectoryFilters` components (native facet/sort selects per P-15, polymorph
Checkboxes for show-all / exclude-top-35); `DRepDirectory` owns the view state and
`visibleEntries` pipeline with the reactive exact-match open (no Enter handler —
a prefix can never auto-open, `DRepDirectory.tsx:167-178`) and the sort-bias
disclosure; `DRepDirectoryBanner` gains the D-4 filtered line with live count
(`DRepDirectoryBanner.tsx:114-118`); `DRepEmptyState` gains the `noResults`
variant with Clear-filters / Show-all Link actions, and the bare "No DReps found"
state now keys off `showAllList` — closing slice-5's accepted empty-cohort edge.
21 `!!!`-prefixed keys per locale via `yarn i18n:manage`; zero new IPC, zero
logging — the search query never reaches any sink.

**Verification executed at slice close** (2026-07-24, all via
`node_modules/.bin/<tool>` — NFR-6): `tsc --noEmit` zero errors. Focused Jest all
green — **109/109** across the six slice suites (1 snapshot pass):
`helpers.spec.ts` 24, `GovernanceStore.spec.ts` 28, `DRepDirectory.spec.tsx` 38
(incl. both critiquer-mandated pins: non-cohort search + exact-open with show-all
off, and no-auto-select on Enter with a unique match),
`DRepDirectoryBanner.spec.tsx` 6, `DRepDirectoryPage.spec.tsx` 5 (incl. the
zero-fetch-from-search pin), `VotingGovernancePage.spec.tsx` 8. **Sanitization
floor 23/23** (`tests/jest/security/governance-sanitization.spec.ts`
`--no-coverage --runInBand`) with the suite file untouched by the slice. Scoped
eslint and prettier were verified clean in the round-1 code review (0 errors;
33 warnings, all pre-existing baseline classes; no JSON/locale/`.snap` file
formatted; no `.scss.d.ts` in the diff).

**Findings.** Durable slice decisions and gotchas (F-1 structurally pre-IPC
search, F-2 post-HRP minimum, F-3 full-form floor edge, F-4 full-list show-all
delta, F-5 §9 inventory gap, F-6 unowned badge drifts carried, F-7 favorited
contract without UI, F-8 dual-ID display gap, F-9 jsdom/bech32 harness fixes,
F-10 accepted runtime edges) live in
[research/slice-6-findings.md](../research/slice-6-findings.md).

---

## References

- Parent plan: [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
- Task tracker: [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
- Implementation guide: [slice-6-implementation-guide.md](./slice-6-implementation-guide.md)
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md)
- Research: [slice-5-findings.md](../research/slice-5-findings.md),
  [slice-4-findings.md](../research/slice-4-findings.md),
  [ux-refinement-findings.md](../research/ux-refinement-findings.md)
