# Slice-6 Code Review Log

> Append-only. Entries are added in chronological order: Planner (planning),
> Critiquer (planning review), Code Review (per-task implementation review).
> Never rewrite or delete an earlier entry.

---

## Planner: slice-6 planning — 2026-07-24

**Scope planned.** One autonomous task. task-121 ships the shared-design-tokens §11
DRep-ID search contract and the show-all / reachability filter framework as pure
renderer derivations — zero new IPC, zero logging (the search query never reaches
any sink). New `drep-directory/helpers.ts` (stake-pools/helpers.ts precedent) owns
the pure functions: query normalization + kind classification (8-char post-HRP
minimum, 51-char full-form floor), a dual-encoding search index built with the
already-present `@cardano-sdk/core` `Cardano.DRepID` API (no new bech32
dependency), prefix search deduped by credential by construction, exact-match
resolution canonicalized to CIP-129 against `drepIndex`, the facet filter set
(status / metadata / expiry-window / exclude-top-35 / favorited / defaults), and
the opt-in show-all sorts with lossless BigNumber comparison. `GovernanceStore`
gains two computeds only — `top35DRepIds` (empty unless ranking Loaded, honoring
the shipped "Ranking-based filters disabled" copy) and `showAllList` (full
membership, drepId-canonicalized, shuffled with the existing session seed) —
leaving `displayedDRepList` and the cohort rule byte-identical. `DRepDirectory`
owns the view state (query, show-all, facets, sort) and the visible-entries
pipeline, opens exact matches reactively (no Enter handler exists, so a prefix
can never auto-open, even unique-match + Enter), shows the §9 sort-bias
disclosure exactly while voting-power-descending is active, and drives the
banner's new filtered mode. `DRepEmptyState` gains the designed `noResults`
variant with Clear-filters / Show-all link actions; the bare "No DReps found"
state now keys off the full list, closing slice-5's accepted empty-cohort edge.
21 `!!!` keys per locale; Jest one-per-acceptance-rule across helpers, store,
component, banner, and container (pre-IPC pin: search interactions trigger no
channel/refresh call).

**The five orchestrator drift items are resolved and recorded (PRD D-1…D-5).**
D-1: no search IPC exists or may be added — §11's "before IPC" clauses are
satisfied structurally; bech32 validation gates only the renderer-side
exact-match/detail-open path, and search is a pure filter over
`drepList`/`drepIndex`. D-2: the 8-character minimum counts characters AFTER the
HRP — tokens §11 (:241) is the named design contract and wins over the task
JSON's looser wording; both boundaries are Jest-pinned. D-3: the favorited
filter is plumbed against an empty favorites source — helpers own the
`favoritedOnly` predicate and the context carries `favoriteDRepIds`
(component prop defaulting to a shared empty set), but no Favorited control and
no `filter.favorited` key ship, because an always-empty source would be dead UI;
grounded in the tasks JSON itself, whose task-122 entry assigns the predicate to
this framework and the persisted set + toggle to slice-7. D-4: the
filtered-banner copy (tokens :86) has no §9 message ID — minted as
`governance.drepDirectory.cohortBanner.filtered` in the §9 `cohortBanner.*`
naming style, with the full §5 sentence pair as the en source; the §9 inventory
gap is flagged for the findings note. D-5: the unowned "Expiring soon" badge
drift is NOT adopted — sub-floor/inactive/top-35 entries surfaced via
search/show-all render the existing status + category badges only; the same
principle (planner P-13) also declines the §1 "Excluded from default cohort"
top-35 badge, which no tasks-JSON task owns and which has no §9 message ID —
both recorded as unowned drift needing future owners (slice-5 R4/F-4 precedent).

**Notable planner resolutions (PRD P-1…P-16).** Show-all renders the FULL
registration list (design :228's "all eligible + top-35" phrasing recorded as a
delta; reachability acceptance and design :221 win) in the same seeded session
order, so enabling it adds no ranking bias. Exact-match open fires only for
checksum-valid full IDs that resolve in `drepIndex`; valid-but-unknown ids fall
to the inline noResults state rather than auto-navigating to a not-found page.
Full-form detection uses the 51/53 data-part lengths derived from the
`@cardano-sdk/core` payload sizes, so a bad-checksum full ID surfaces the
"Invalid DRep ID" copy (minted twin of the existing task-103
`voting.governance.drepInputError`). Facet and sort dropdowns are native
`<select>` elements with aria-labels (react-polymorph `Select` is not reliably
drivable under the jsdom/RTL harness; slice-5's native-title precedent), the two
toggles are polymorph Checkboxes, and the show-all control is exactly one
Checkbox labeled with §9 `cohortBanner.showAll` — no banner link, consistent with
slice-5 R5's link-free banner sentence. The banner switches to the filtered line
(with live count) on ANY deviation from the pure default view, and the BMVG
citation travels with the cohort claim it explains. `helpers.ts` restates the
7–12 window constants locally so no filtering code imports the badge module
(invariant #8). **Intentionally out of scope:** favorites persistence/toggle/UI
key (task-122), both unowned badges (D-5/P-13), verified-name search (post-v1),
dual-ID stacked card rows (pre-existing `DRepIdDisplay` gap, recorded P-14), any
main-process change, new Storybook stories (P-16 — compile fixes only), and every
`!!!` removal (release-end). Planning status: in_review — awaiting the Critiquer
pass.

---

## Critiquer: slice-6 planning review — 2026-07-24

**Scope reviewed.** One broad pass over `slice-6-PRD.md` + `slice-6-implementation-guide.md`
against the task-121 tracker entry, shared-design-tokens §5/§9/§11, design :157-252,
the prompt.md locked invariants, and the live worktree at `6f828d573`.

**Coverage verified.** Every task-121 acceptance criterion maps to a named FR + Jest
step: 8-char post-HRP minimum (both boundaries, helpers + component), no auto-select
on prefix incl. Enter, exact-match open with CIP-129 canonicalization from both
encodings, dedupe-by-credential by construction (one index row per canonical id),
pre-IPC validation (structural: zero renderer→main paths; container pin that search
fires no `refresh`/`reshuffleCohort`), the full eight-facet filter set (favorited via
injected set, no UI — D-3 correctly grounded in the task-122 tracker text),
reachability, en/ja localization (21 `!!!` keys per locale; the six §9-inventoried
ids verified at §9 :167/:169/:173/:174/:176/:218), and the inherited sanitization
floor (23 `it(` baseline grep-confirmed; no new sink anywhere in the planned diff;
the search query provably never logged). No hidden manual checkpoint: task-121 is
autonomous and outside the locked non-autonomous set (prompt.md :176-180). D-1…D-5
and P-1…P-16 are each grounded in a verifiable source; the deliberate deltas (P-7
full-list show-all vs design :228 phrasing, P-13/D-5 unbuilt badges, P-14 dual-ID
rows, D-4 §9 gap) are recorded for the findings note rather than silently dropped —
consistent with the tasks-JSON-authoritative rule and slice-5 R4/R5 precedent.

**Anchor spot-checks (all against live code).** `GovernanceStore.ts` :69-84/:90/:110/
:143-148/:157-175 (inline sort :169-173)/:178-180/:284-287 — byte-exact.
`DRepDirectoryBanner.tsx` :10-43/:45-54 (`showSource` :52)/:85/:97 — exact.
`DRepDirectory.tsx` Props :58-72, `hasRetainedData` :89, bare-empty :132-143, banner
render :188-194 — exact (file is 226 lines, guide says 227 — off-by-one, cosmetic).
`DRepEmptyState.tsx` 38 lines, union :16 — exact. `DRepDirectoryPage.tsx` :58-66/
:68-73/:82-97 — exact. en-US.json :313/:314/:315/:318/:319/:321/:322/:329/:330 and
:926, ja-JP.json same lines — exact. `DRepDirectoryList.tsx` :47-55,
`DRepErrorBanner.tsx` :6-11, `stake-pools/helpers.ts` :1-24,
`GovernanceQueryService.ts` `_credentialToDRepId` ~:624-641,
`governance.types.ts` :35, `VotingPowerDelegation.tsx` :9/:133,
`@cardano-sdk/core` `DRepID.d.ts` :4-13 + `DRepID.js` :31-33 — all verified,
including the API surface (`isValid`/`toCip105DRepID`/`toCip129DRepID`/
`cip129FromCredential`/`cip105FromCredential`) and the 51/53 data-part arithmetic
(28/29-byte payloads → 45/47 words + 6 checksum). Step 16's
`await store.fetchDRepList()` reaching `VotingPowerEnrichState.Loaded` is valid:
`fetchDRepList` awaits `_enrichVotingPower` before returning, and the existing spec
uses the identical pattern.

**Small-model implementability.** Yes, modulo the blockers: full file contents for
every CREATE/replace, pinned-and-verified anchors for every EDIT, ordered steps,
exact devcontainer-safe commands (`node_modules/.bin/*`, no npx, scoped prettier),
invariants restated inline, and the collision-prone assertion fixes (17d) identified
precisely (spec :94/:205 are the only two).

**Blockers:**

1. **Missing test — search/direct-ID reachability of non-cohort entries
   (guide Step 17e).** Task-121 acceptance requires excluded top-35 and non-cohort
   DReps reachable "through search/show-all filters **and direct DRep ID entry**",
   and the PRD DoD promises them "findable via search". The guide pins the show-all
   leg with a `showAllList ⊃ drepList` fixture, but every search and exact-match
   test uses entries already in `drepList` (the harness defaults
   `showAllList = drepList`). The load-bearing wiring — `buildDRepSearchIndex(showAllList)`
   and exact-match lookup via `drepIndex` — is unpinned: regressing the index base
   to `drepList` would pass the entire planned suite. Fix: add one component test
   where an entry exists only in `showAllList`/`drepIndex` (not `drepList`) and,
   with show-all OFF, is (a) found via a ≥8-char prefix search and (b) opened via
   exact full-ID entry.
2. **Guide Step 20 is factually wrong and breaks compile if followed literally.**
   `VotingGovernancePage.spec.tsx` `buildStores().governance` (:87-97) ALREADY
   contains `drepIndex: new Map([[VALID_DREP_ID, drepEntry]])`. Step 20 and the
   file-table row ("3 mock fields only") instruct adding all three fields including
   `drepIndex`, which produces a duplicate object key (TS1117). Fix: Step 20 adds
   only `showAllList` and `top35DRepIds` (2 fields); update the table row.

**Non-blocking observations (no action required before build):**

- P-5's 51-char full-form floor means a partially pasted CIP-129 id at 51-52 data
  chars transiently shows "Invalid DRep ID" instead of the prefix state. §11 is
  silent on this boundary and the PRD records the interpretation with both edges
  Jest-pinned; acceptable, worth a line in the findings note.
- `helpers.spec.ts` lossless-sort fixture names are inverted relative to their
  values (`smaller` holds 9007199254740993, `larger` 9007199254740992); assertions
  are correct and genuinely discriminate the float-collapse case — rename if touched.
- The `abstain`/`no_confidence` sentinels are unsearchable structurally (they can
  never enter `drepList`, hence never any derived index); no explicit Jest pin
  exists, which invariant #13 does not require — the structural argument in the
  PRD suffices.
- Step 10's "227 lines" is 226; the trailing-newline miscount does not affect any
  edit since the file is replaced whole.
- `fireEvent.click` on the react-polymorph Checkbox label text is a plausible but
  unverified-in-this-repo interaction (the skin's root onClick receives the
  bubbled label click); if it fails at build time, click the rendered input role
  instead — no plan change needed.

**Decision: requires_changes**

---

## Planner: slice-6 fix pass — 2026-07-24

Both blockers addressed in `slice-6-implementation-guide.md`; the PRD needed no
content change beyond the status line.

1. **Non-cohort search/direct-ID reachability now pinned.** Step 17e gains one
   component test ("finds and opens a non-cohort entry by ID with show-all
   off"): entry 2 exists only in `showAllList` (and, via the 17c harness
   default, in `drepIndex`) while `drepList` holds only entry 1. With show-all
   off, a 20-post-HRP-char prefix of entry 2's id yields exactly one result
   card without any `onViewDetails` call, then the exact full CIP-129 id opens
   the detail view once with the canonical id. A regression of the search-index
   base (or exact-match source) to `drepList` now fails this test. The
   task-121 acceptance checklist bullet was extended to name the pin.
2. **Step 20 corrected to 2 mock fields.** `VotingGovernancePage.spec.tsx`
   `buildStores().governance` already contains `drepIndex` (:89, keyed by
   `VALID_DREP_ID`); Step 20 now adds only `showAllList` and `top35DRepIds`
   and warns against re-adding `drepIndex` (TS1117 duplicate key). File-table
   row 21 updated to "EDIT (2 mock fields only)". Step 19a is unchanged —
   `DRepDirectoryPage.spec.tsx` has none of the three fields, so its
   three-field instruction stands.

No scope change. Planning status set to **approved** (one critique pass + one
fix pass consumed); proceeding to build.

---

## Code Review: task-121 round 1 — 2026-07-24

**Scope reviewed.** Full uncommitted working-tree diff at `wt/slice-6`
(base `6f828d573`): 18 modified files + 6 new source/spec files, checked against
the approved implementation guide, the PRD's scope/non-goals and D-1…D-5 /
P-1…P-16 resolutions, and every task-121 acceptance criterion in the tracker.

**Independent verification (all re-run, not trusted from claims):**

- `node_modules/.bin/tsc --noEmit` — exit 0, zero errors.
- Focused Jest (helpers, GovernanceStore, DRepDirectory, DRepDirectoryBanner,
  DRepDirectoryPage, VotingGovernancePage; `--no-coverage --runInBand`) —
  6 suites, **109/109 pass**, 1 snapshot pass.
- Sanitization floor `tests/jest/security/governance-sanitization.spec.ts` —
  **23/23**, and the suite file has zero working-tree changes.
- Scoped eslint on all touched .ts/.tsx — 0 errors (33 warnings, all the
  pre-existing enum-import/no-shadow pattern, none introduced by this diff).
- Prettier `--check` on every changed .ts/.tsx/.scss — clean; no JSON/locale/
  `.snap` file was prettier-formatted; no `.scss.d.ts` appears in the diff.

**Locked-invariant audit:**

- **#2 sanitization floor** — `git diff` grep for `logger.`, `analytics`,
  `electron-store`, `console.*`, `localStorage`, `.set(`, `track(`: zero
  additions in production code. The search query lives only in `useState` and
  never reaches any sink; no new logging call sites exist anywhere in the diff.
- **#6 no new IPC/CLI** — zero files under `source/main/`; new helpers/components
  import nothing channel- or ipc-related; the container pin (`refresh`/
  `reshuffleCohort` uncalled across prefix + invalid-full-form input) passes.
- **#7 default cohort** — `displayedDRepList`, `defaultCohort` semantics,
  `fetchDRepList`, `_enrichVotingPower`, `reshuffleCohort`, and the
  35/200/6 constants are untouched; the only cohort-path edit is the
  behavior-identical `compareDRepIdAsc` extraction. No fixture places a
  sub-floor DRep inside a cohort (sub-floor entries appear only in
  show-all/search reachability fixtures, as required).
- **#8 badges informational** — no `DRepCategoryBadge`/`getDRepCategory`/
  `DRepStatusBadge` import in helpers or any filter/sort/pipeline code; the
  7–12 window is restated locally in `helpers.ts`.
- **#5 lovelace losslessness** — voting-power sorts use `comparedTo` only; no
  `.toNumber()`/`Number(votingPower)` anywhere in the diff; boundary Jest at
  one lovelace past `Number` precision passes.
- **CIP-129/CIP-105 byte-equality** — exact-match open passes `match.drepId`
  (the store's canonical entry id) to the unchanged `handleViewDetails`;
  Jest pins `onViewDetails` called with the byte-identical CIP-129 id from
  both encodings. `location.state`/route paths untouched.
- **#11** — all 21 new keys in BOTH locales carry leading `!!!`; no existing
  marker stripped; `defaultMessages.json` + `translations/messages.json`
  diffs are tool-generated and ride with the task.
- **#12 reachability** — store pin (full membership incl. top-35/sub-floor/
  inactive), show-all component pin, and the critiquer-mandated
  non-cohort-only search + exact-open pin (entry present only in
  `showAllList`/`drepIndex`) are all present and passing.
- **#13/#14** — sentinels structurally unreachable (no new list source);
  `DRepStatus`, `DRepStatusBadge`, `DRepCategoryBadge`, `seededShuffle`
  untouched.

**Acceptance-criteria mapping.** All six tracker criteria covered: §11 search
rules (8-char post-HRP boundaries in helpers and component, no auto-select
incl. Enter-on-unique-match, exact-match open both encodings, dedupe by
credential, pre-IPC validation), both list/detail paths, all eight filters
(favorited via injected set, no UI — D-3), reachability, en/ja copy with a
ja-JP smoke test, inherited sanitization floor. Both critiquer blockers are
fixed in the diff (non-cohort search/direct-ID pin present;
`VotingGovernancePage.spec` gained exactly 2 fields, no duplicate `drepIndex`).

**Blockers:** none.

**Non-blocking observations (record in the slice findings note):**

1. Three build-time harness fixes go beyond the guide's literal spec text but
   sit inside the task's declared "harness fixes" scope and are all
   test-only: (a) `@jest-environment node` docblock on `helpers.spec.ts`;
   (b) a commented, file-scoped `global.Uint8Array` realm patch in
   `DRepDirectory.spec.tsx` (jsdom realm vs Buffer, needed for the SDK's
   bech32 encoder; Jest isolates globals per file); (c) react-polymorph
   `ThemeProvider` wrappers in `DRepDirectory.spec.tsx` and
   `DRepDirectoryPage.spec.tsx` (Input/Checkbox skins require theme context).
2. If ranking transitions Loaded→Failed mid-session after a user enabled
   exclude-top-35, the checkbox hides while the facet stays true (a no-op
   filter, since `top35DRepIds` is then empty, with the banner truthfully in
   filtered mode). Inherent in the approved guide design; reachable only via
   a refresh that loses ranking; Clear-filters via noResults remains the
   escape hatch when results are empty.
3. `stripHrp` produces a garbage slice for non-bech32 ids in bare-query
   matching; unreachable for production ids (all derived via
   `cip129FromCredential`) and cannot false-positive in practice.

**Decision: approved**

---

Planner: 2026-07-24 — slice-6 close: The slice closes with its single task.
task-121 (DRep ID search + show-all / reachability filters) is implemented and
code-review approved (round 1, zero blockers); the tracker entry is `complete` —
not `verified`, which needs dedicated proof beyond the task's own unit tests and
arrives with slice-8's release-verification acceptance. No `auditSummary` field
exists for slice-6 in the tracker, so this entry is the slice summary. Full slice
verification re-run at close: `tsc --noEmit` zero errors; focused Jest 109/109
across the six slice suites (`helpers` 24, `GovernanceStore` 28, `DRepDirectory`
38, `DRepDirectoryBanner` 6, `DRepDirectoryPage` 5, `VotingGovernancePage` 8;
1 snapshot pass); sanitization floor 23/23 with the suite file untouched; scoped
eslint and prettier verified clean in the round-1 review. Invariant #12
(reachability) is now satisfied: top-35, sub-floor, and inactive DReps are
reachable via show-all, ≥8-char search, and exact full-ID entry — each path
Jest-pinned, including the non-cohort-only index-base pin mandated by the
critiquer. Durable findings F-1…F-10 (structurally pre-IPC search, post-HRP
minimum, full-form floor edge, full-list show-all delta, §9 inventory gap, unowned
badge drifts carried, favorited contract without UI for task-122, dual-ID display
gap, jsdom/bech32 harness fixes, accepted runtime edges) are recorded in
`research/slice-6-findings.md`, and the PRD Final Outcome is filled. The tree is
intentionally uncommitted at close; the single subject-only
`feat(gov): task-121 …` commit remains the implementer's close-out step.

---
