# Slice-5 Code Review Log

> Append-only. Entries are added in chronological order: Planner (planning),
> Critiquer (planning review), Code Review (per-task implementation review).
> Never rewrite or delete an earlier entry.

---

## Planner: slice-5 planning — 2026-07-24

**Scope planned.** Three autonomous tasks. task-118 lands the binding default cohort as
renderer-side derived state: a new `seededShuffle` util (mulberry32 + Fisher–Yates, no
new dependency), `GovernanceStore` gains a session `cohortSeed` observable, a
`reshuffleCohort()` action (reseed only — zero IPC, Jest-pinned at store and container
level), and `isCohortActive` / `defaultCohort` / `displayedDRepList` computeds
(BigNumber-ranked top-35 exclusion → strict `drepActivity > 6` + active eligibility →
≤200 selection → drepId-canonicalized seeded shuffle); the container swaps to
`displayedDRepList` and the existing `DRepDirectoryBanner` gains the primary cohort
line + Reshuffle link, rendered only while the cohort is active; 2 `!!!` keys per
locale; an 11-case cohort describe in `GovernanceStore.spec.ts` plus util, directory,
container, and harness spec updates. task-119 adds the informational
`_shared/DRepCategoryBadge` (Primary / Threshold / Non-metadata; High value deferred to
anchor-1) on the `DRepStatusBadge` pattern with an exported pure `getDRepCategory`,
native-title tooltips, render sites in the `DRepCard` topRow and the `DRepDetail`
header, committed snapshot tests at both call sites, one Storybook story under the
global locale toggle, and 6 keys per locale. task-120 appends the §9 BMVG citation as
the banner's secondary line behind a story-only `showSource` default-true prop, with a
banner spec, with/without-citation stories, and 1 key per locale. The orchestrator's
R1–R8 resolutions are adopted verbatim and verified against live code: ranking derives
only from Phase-2 stake enrichment (R1 — the plan's Key-Decisions `--include-stake` row
:139 is confirmed stale, findings item); the cohort exists only at
`votingPowerState === Loaded`, full list + no cohort claims otherwise (R2, composing
with the existing pagination reset); anchor presence is the interim badge-only metadata
proxy — never an eligibility filter (R3); the "Expiring soon" status-badge variant is
out of scope with the tokens-§1-vs-tasks-JSON conflict recorded (R4); no
Show-all/Search links or keys (R5); one extended banner, not a second component (R6);
the R7 key/task split; and a session-seed deterministic shuffle whose display order is
a pure function of (membership, seed) via drepId canonicalization (R8, planner P-3).
Planner resolutions P-1…P-14 close the remaining judgment calls — notably the
store-owned `displayedDRepList` seam so the banner claim and rendered list cannot
diverge, the lossless-boundary Jest construction where a float-coerced tie would invert
the top-35 boundary, badge placement in the detail header rather than the On-chain
section (provenance truthfulness), `toMatchSnapshot` as the repo's first snapshot tests
to satisfy the acceptance literally, and the §9-verbatim BMVG string over the task
description's longer phrasing (discrepancy recorded). **Intentionally out of scope:**
High value category and cohort-median math (anchor-1), verified-metadata flag
(task-151), search/show-all/`cohortBanner.showAll`/filter-copy switch and the top-35
"Excluded" badge (slice-6), `doNotList` (anchor-2, accepted interim), any
main-process/IPC change, seed persistence across restarts, the §5 narrow-width
tooltip collapse (accepted gap P-12), and every `!!!` removal (release-end). Planning
status: in_review — awaiting the Critiquer pass.

---

## Critiquer: slice-5 planning review — 2026-07-24

Critiquer: 2026-07-24 — none — Decision: approved

**Scope of the pass.** One broad review of `slice-5-PRD.md` + `slice-5-implementation-guide.md`
against the tasks JSON (task-118/119/120 acceptance criteria), `shared-design-tokens.md`
§1a + §5 + §9, `drep-discovery-design.md` :217-224, and the locked invariants
(#2, #5, #6, #7, #8, #11, #14), with 16+ line anchors spot-checked against live code at
`b6b94268e`.

**Blockers: none.**

- Coverage: every acceptance criterion of the three tasks maps to a numbered FR/step;
  the task-118 "completed metadata when available" clause is resolved by R3 (badge-only
  anchor proxy, no eligibility gate) consistently with §1a and the README staging.
- Invariants: #7 pipeline (rank → drop 35 → active ∧ `> 6` → ≤200 → canonicalize →
  seeded shuffle) is Jest-pinned per stage with no sub-floor fixture inside a cohort;
  #8 holds structurally (pure classifier, no callbacks, never imported by ordering
  code); #6 holds (reshuffle = one seed assignment; channel call counts pinned at store
  AND container level; zero `source/main/` files touched); #14 untouched; #5 pinned by
  a genuinely discriminating boundary fixture (9007199254740992 vs …93, larger stake on
  larger drepId, so a float tie + drepId tie-break inverts the boundary); #2 holds by
  construction (zero sink calls in new paths) plus the 23-test floor rerun per task;
  #11 all 9 keys × 2 locales `!!!`-prefixed.
- Judgment calls: R1–R8 + P-1…P-14 leave no unresolved checkpoint; the three recorded
  conflicts (stale plan Key-Decisions row, tokens §1 "Expiring soon" vs tasks JSON,
  task-text vs §9 BMVG copy) are correctly routed to `research/slice-5-findings.md`.
- Anchors: all spot-checked anchors accurate, including the load-bearing ones
  (store seams, container render :82-95, banner 63 lines, spec harness lines, story
  call sites, locale insert positions :306/:307 in both files, `fireEvent` present in
  `DRepDirectory.spec.tsx` but absent from `DRepDirectoryPage.spec.tsx` exactly as the
  guide states, fixture values `baseEntries[0]` activity 12 + null anchor and detail
  `baseEntry` anchor + activity 34 matching the predicted categories).
- Tests/docs: per-task Jest, both badge call-site snapshots (committed), the
  7–12-with-metadata tie-break, window edges 6/7/12/13, banner with/without-citation
  stories, and the sanitization rerun are all explicitly scheduled.

**Non-blocking observations (for `research/slice-5-findings.md`, no guide change
required):**

1. The snapshot-only `getDRepCategory` (P-6, mandated by the task JSON's "computes its
   category from the same DRep snapshot" contract) means out-of-cohort DReps — a top-35
   entry opened via the detail route, or the full-list fallback while Phase 2 is
   Loading/Failed — carry tooltips claiming "Inside the default Recommended view…".
   Copy is `!!!`-preliminary; the release-end copy review should reword or the
   anchor-1/slice-6 owner should revisit once out-of-cohort surfaces multiply.
2. Trivial anchor drift, no implementer impact (quoted edit blocks are exact):
   `DRepDirectoryBanner.scss` is 26 lines, not 27; `DRepCard.scss` `.topRow` sits at
   :16-20, not :17-21.
3. The Step-9 parenthetical justifying `runInAction` via `index.tsx` `enforceActions`
   slightly overstates (that config is not loaded in Jest), but the construction is
   correct and harmless either way.

---

## Code Review: task-118 pass 1 — 2026-07-24

Code Review: task-118 pass 1 2026-07-24 — none — Decision: approved

**Scope of the pass.** One broad review of the full uncommitted diff (14 modified files
+ 2 new `seededShuffle` files) against the task-118 guide section and the PRD
acceptance (FR-1…FR-5), with every verification command re-run independently.

**Blockers: none.**

- Implementation matches the approved guide steps 1–13 essentially verbatim: util,
  store constants/comparator/computeds/action, container swap to `displayedDRepList`,
  banner cohort line + Reshuffle link gated on `isCohortActive`, SCSS append, both
  locale keys, 11 store cases + 5 util cases + 4 directory cases + container wiring
  test + harness mock fields + story updates.
- Verified independently: `tsc --noEmit` 0 errors; focused Jest 65/65 across the 5
  suites; sanitization floor 23/23 with the suite file untouched; scoped eslint 0
  errors (29 warnings are pre-existing decorator/enum false positives); prettier
  clean on all touched files; `defaultMessages.json`/`translations/messages.json`
  rewrites present (i18n:manage was run).
- Invariants: #7 pipeline pinned per stage, boundary 6 excluded / 7 included, sub-floor
  fixtures appear only to prove exclusion; #5 pinned by the discriminating 2^53
  boundary pair (checked the arithmetic: a float compare ties 9007199254740992/…93 and
  the drepId tie-break inverts the boundary); #6 reshuffle is one seed assignment with
  channel call counts pinned at store AND container level, zero `source/main/` or
  channel files touched; #8 trivially holds (no category code exists yet; the store
  imports only `seededShuffle`; `DRepDirectory` renders the list as given); #2 grep of
  the diff finds zero logger/analytics/storage touchpoints and the seed reaches no
  sink; #11 both new keys `!!!`-prefixed in both locales at the correct alphabetical
  positions; #14 fixtures use only `active`/`inactive`.
- No IPC/contract drift, no new dependencies, no tracked `.scss.d.ts`, no local
  IntlProvider in stories, banner has no consumers outside `DRepDirectory`.

**Non-blocking notes:**

1. Empty-cohort edge: with `isCohortActive` true but zero eligible entries (e.g., a
   network with ≤35 DReps), the banner's cohort claim renders above the "No DReps
   found on-chain" empty state — a slightly contradictory composition. This is exactly
   the guide's prescribed code, unrealistic on mainnet, and slice-6's show-all seam is
   the natural place to absorb it; recording only.
2. Guide-internal drift, no implementer impact: the task-118 file table says the store
   spec gains "2 imports" while step 9a (and the implementation) needs only one
   (`runInAction`; `DRepDirectoryEntry` was already imported).

---

## Code Review: task-119 pass 1 — 2026-07-24

Code Review: task-119 pass 1 2026-07-24 — none — Decision: approved

**Scope of the pass.** One broad review of the full uncommitted diff (4 modified
source/test files + 4 i18n artifacts + 4 new files + 2 generated snapshots) against
the task-119 guide section and the PRD acceptance (FR-6…FR-8), with every
verification command re-run independently.

**Blockers: none.**

- Implementation matches the approved guide steps 1–9 verbatim: `getDRepCategory`
  with binding priority Threshold > Primary > Non-metadata (7–12 window strict on
  both edges, `!= null` activity guard, anchor-presence proxy otherwise), the
  presentational badge on the `DRepStatusBadge` pattern with native `title` tooltip
  and combined aria-label, SCSS on the info/warning/neutral token families, card
  topRow + detail header placements per P-8, 7 classifier + 4 render spec cases
  (edges 6/7/12/13, null activity, both tie-breaks, ja-JP), one snapshot per call
  site with the badge span's class/title/label captured, one story under the global
  locale toggle, 6 `!!!`-prefixed keys per locale in correct alphabetical position.
- Verified independently: `tsc --noEmit` 0 errors; focused Jest 56/56 across the 4
  suites (2 snapshots pass); sanitization floor 23/23 with the suite file untouched;
  scoped eslint 0 errors (6 warnings pre-existing in touched files); prettier
  `--check` clean on all changed .tsx/.scss; `defaultMessages.json` /
  `translations/messages.json` rewrites present (i18n:manage was run).
- Invariants: #8 holds in every code path — the module exports a pure classifier and
  a callback-free presentational component; grep shows its only importers are
  `DRepCard`, `DRepDetail`, and the story; `GovernanceStore`, `DRepDirectoryList`,
  and all ordering/filtering code untouched. #7 untouched (no cohort code in the
  diff; no Recommended tab or badge; the "Recommended view" wording appears only
  inside approved tooltip copy). #6/#5: zero IPC, CLI, or votingPower touchpoints.
  #14: `DRepStatusBadge` and `DRepStatus` untouched; `status` is carried in
  `DRepCategorySource` but unread, per the documented anchor-1 extension seam.
  #2: no logger/analytics/electron-store call anywhere in the new code. #11: all 12
  new locale strings `!!!`-prefixed. No High value category or key anywhere.
- Snapshot fixture claims verified against the harnesses: `baseEntries[0]`
  (drepActivity 12, anchor null) does hit the Threshold upper edge on the card;
  `DRepDetailPage` `baseEntry` (anchor present, drepActivity 34) is Primary. The
  card test also pins exactly one category badge per card via the three-label regex.
- No new dependencies, no tracked `.scss.d.ts`, no local IntlProvider in the story
  (jest specs use one per established spec convention), no main-process or channel
  drift.

**Non-blocking notes:**

1. The 6 scoped eslint warnings (unused `drepId` render-prop args, `as any` /
   non-null assertions in the `DRepDetailPage` test harness) all pre-date this diff.
2. The badge formats all six messages on every render though only one
   label/tooltip pair is used; this is the guide's exact prescribed code and the
   cost is negligible — recording only.
