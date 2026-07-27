# Slice-7 Code Review Log

> Append-only. Entries are added in chronological order: Planner (planning),
> Critiquer (planning review), Code Review (per-task implementation review).
> Never rewrite or delete an earlier entry.

---

## Planner: slice-7 planning — 2026-07-27

**Scope planned.** One autonomous task. task-122 ships DRep favorites end-to-end:
a `DREP-FAVORITES` electron-store key registered in the shared config/types and
served by the existing generic main-process handler (zero `source/main/` changes,
zero new IPC), `LocalStorageApi.getDRepFavorites`/`setDRepFavorites`/
`unsetDRepFavorites` on the TOKEN-FAVORITES precedent, and a
`GovernanceStore.favoriteDRepIds: Set<string>` observable with `loadFavorites()`
(setup-kicked, string-sanitized, silently failure-tolerant) and
`toggleFavorite()` (reassign-a-fresh-Set contract; persists the full array with a
silent catch). The directory card gains the tokens-§10 favorite toggle — a native
`<button aria-pressed>` first in the card's tab order — and the Favorited filter
checkbox lands in `DRepDirectoryFilters`, completing what slice-6 D-3/F-7
deliberately deferred to this task. The designed Favorites surface ships as a
real route `/governance/favorites`: a second nav item in the **existing**
Governance-section `Navigation` (`containers/voting/Governance.tsx` already
renders the tab bar — the decisive live seam), rendered by the same
`DRepDirectoryPage`/`DRepDirectory` pair in a `favorites` view mode whose
membership is computed exclusively through the slice-6 `filterDReps`
`favoritedOnly` predicate over `showAllList` (full membership, so favorited
non-cohort/top-35/inactive entries stay visible; the predicate is never
re-implemented — AC-2). The view gets the design's replaced banner line with a
live count, a `noFavorites` empty state with a Back-to-directory CTA, and the
AC-5 stale-favorite mechanism. 9 `!!!` i18n keys per locale (8 exact §9 ids + 1
minted); Jest across store/helpers/component/banner/container including a
two-store app-restart simulation over one backing record; four new Storybook
stories plus a favorites-aware Connected flow (global locale toggle only).

**The five orchestrator decisions are resolved and recorded (PRD R-1…R-5).**
R-1: the tasks JSON's `targetPath: source/main/` conflicts with the live
persistence pattern — resolved per prefer-live-repo to renderer `LocalStorageApi`
reuse + common key registration, consumed via `this.api.localStorage`
(AssetsStore precedent); the generic main handler needs no change; conflict
recorded for the findings note. R-2: the favorites surface is the real
`/governance/favorites` route + Governance-nav Favorites tab + a `favorites`
view mode of the existing directory pair — the smallest surface that satisfies
AC-1…AC-5 *and* the design's route/tabs/banner/empty-state contract, because the
tab bar already exists in `Governance.tsx`; recorded reconciliations: the shipped
`governance.tabs.directory` id is pre-existing drift (the new tab uses the exact
§9 `governance.drepDirectory.tabs.favorites`; no rename), the empty state ships
text + CTA without the designed illustration (no asset exists), the favorites
view renders no search/filter/sort controls, and a favorited id whose
registration vanishes entirely from the chain cannot render a card yet is never
purged (accepted edge). R-3: AC-5's stale favorites are built as a
forward-compat mechanism — `isStaleFavorite` helper (checks status against a
stale set containing `'retired'`; type-safe today and false for every live
entry), an injectable `isStaleFavoriteEntry` seam so Jest and Storybook render
synthetic stale favorites without fake status values, the exact §9
`governance.drepFavorites.staleCaption` copy, existing status badges only (the
Retired / Excluded-from-cohort badge variants remain unowned drift, slice-6
F-6), and no auto-purge anywhere; verification against real `Retired`/`doNotList`
states is explicitly deferred to invariant #14's future signal and anchor-2
(task-153). R-4: the sanitization spy suite was read in full — it spies
`filterLogData`, the renderer logger, analytics, and Matomo URL masking, and
intercepts no electron-store call, so the favorites write cannot trip it and the
suite stays byte-identical at 23/23; the `DREP-FAVORITES` record is recorded as
the second documented exception to invariant #2's electron-store wording,
sanctioned by invariant #12 and the TOKEN-FAVORITES/task-168 precedents; in
exchange the slice enforces zero logger/analytics calls in every favorites path
(silent failure handling, no toggle analytics event — a deliberate divergence
from AssetsStore), with Jest pinning that no logger call ever contains a
favorite id. R-5: i18n uses the exact §9 ids and copy for all eight inventoried
keys; the design-specified favorites banner line has no §9 id and is minted as
`governance.drepFavorites.banner` (D-4/F-5 precedent; the §9 inventory gap is
flagged again for the findings note); the copy's apostrophe is the plain ASCII
character exactly as in the design source, which a lone ICU apostrophe renders
literally.

**Notable planner resolutions (PRD P-1…P-10).** Reassign-only observable Set on
the `drepIndex` Map precedent; persistence as a JSON array of canonical CIP-129
ids in a record the shared handler makes per-device *and per-network*; native
toggle button (slice-5/6 native-element precedent) with star glyphs and
add/remove aria-labels; always-visible Favorited checkbox whose zero-favorites
case falls to the existing noResults escape hatch; banner favorites mode via two
optional props so the banner stories compile untouched; the exact-match-open
search effect gated off in the favorites view; container derives the view from
`location.pathname` with both routes on one `DRepDirectoryPage` and the
Back-to-directory CTA preserving the delegation-form return state. Storybook
follows the global-locale rule (no local IntlProvider, no per-locale variants)
and extends the Connected flow with the Favorites tab and a working
`withState`-backed toggle. **Intentionally out of scope:** any `source/main/`
change, predicate re-implementation, `Retired`/`doNotList` status work, the
unowned badge variants, favorites analytics, a detail-view toggle, renaming
`governance.tabs.directory`, and any auto-purge.

**Planning status:** PRD `in_review`; implementation guide written to the
small-model bar (27 ordered file edits with quoted line-anchored seams, full
code for every new block, inline invariants, exact Jest/story additions, and the
ordered verification commands). No blocking questions for the user; next step
per prompt.md:167-172 is the REQUIRED critique subagent pass over PRD + guide.

---

## Critiquer: slice-7 planning review — 2026-07-27

One broad pass over `slice-7-PRD.md` + `slice-7-implementation-guide.md`,
judged against the tasks-JSON task-122 entry (all 5 acceptance criteria), the
prompt.md locked invariants (:93-138) and per-slice doc rules (:45-89), the
design contracts (`drep-discovery-design.md:107-111`,
`shared-design-tokens.md:161-166,175,198-199`), the grounding brief's section-G
risks, and the live worktree code (every pinned seam in
`electron-store.types.ts`, `electron-store.config.ts`, `localStorage.ts`,
`GovernanceStore.ts`, `helpers.ts`, `DRepCard.tsx`, `DRepDirectoryList.tsx`,
`DRepDirectoryFilters.tsx`, `DRepDirectoryBanner.tsx(+scss)`,
`DRepEmptyState.tsx(+scss)`, `DRepDirectory.tsx`, `routes-config.ts`,
`Routes.tsx`, `containers/voting/Governance.tsx`, `DRepDirectoryPage.tsx`, the
five spec harnesses, and `DRepDirectory.stories.tsx` was spot-checked).

**What holds up.** All 5 ACs are covered with pinned tests: AC-1 via the
two-store restart simulation over one backing record plus malformed/rejecting
reads; AC-2 via the card toggle, the Favorited checkbox driving the existing
`favoritedOnly` facet, and a favorites pipeline that calls `filterDReps`
(predicate verified single-sourced at `helpers.ts:215`); AC-3 via
store/component/container Jest, a ja-JP render test, the toggle story, and 9
`!!!`-prefixed keys per locale with exact §9 ids (locale placement anchors
verified: `card.select` :305, `thresholdWindow`/`metadata` :330-331,
`syncing`/`title` :354-355); AC-4 via zero-logging enforcement, the no-logging
Jest pins, and the diff grep — I independently confirmed the sanitization suite
has exactly 23 `it(` cases and spies no electron-store path, so the R-4
documented-exception reconciliation is factually grounded; AC-5 via the
injectable staleness seam, the stale story, and no-purge-by-construction, with
the `Retired`/`doNotList` deferral correctly recorded against invariant #14 and
anchor-2. All four section-G risks (targetPath conflict, favorites surface,
stale forward-compat, sanitization reconciliation) are resolved AND recorded
(R-1…R-4). Locked invariants are stated inline in the guide as required. No
hidden manual checkpoints — the autonomous classification matches the locked
non-autonomous set. Every component gaining required props has all its call
sites covered (grep-verified: `DRepDirectory` renders only in the container,
its spec, and the stories file — all addressed, including the standalone
Ranking-unavailable story). The `Governance.tsx` activeItem prefix-match claim
and the reassign-only MobX Set contract both check out against live code.

**Blockers (2):**

1. **Guide Step 23 tests 3-4 assert row identity with
   `screen.getByText(realDrepId(n))`, which cannot match live rendering.**
   `DRepIdDisplay` truncates every id longer than 18 chars to
   `first8…last6` (`DRepIdDisplay.tsx:35-42`); CIP-129 ids are ~58 chars, and
   the full id appears only in the tooltip `tip` and the `<code>` `aria-label`,
   never as text content. Both the Favorited-checkbox test and the
   non-cohort-favorites-reachability test fail as written. The guide's fallback
   note ("reuse whatever id-matching helper the file's existing tests use")
   is an unresolved judgment call the small-model bar forbids: the existing
   tests assert row presence by card count
   (`expect(screen.getAllByText('!!!View details')).toHaveLength(n)`,
   e.g. `DRepDirectory.spec.tsx:564-568,613`) — there is no id-matching helper
   to reuse. **Fix:** replace the two `getByText(realDrepId(n))` assertions
   with pinned working ones — card-count assertions plus, where the specific
   entry matters, a truncated-prefix match such as
   `screen.getByText(new RegExp(realDrepId(2).slice(0, 8)))` (and the
   corresponding `queryByText` negative for the excluded entry).

2. **Guide Step 6 pins a nonexistent anchor.** It cites the `'./helpers'`
   import block at `helpers.spec.ts:427-438`, but the file is 356 lines and the
   import block sits at lines 10-21. The semantic instruction (insert
   `isStaleFavorite,` between `isDefaultFilterState,` and `normalizeDRepQuery,`)
   is correct and recoverable, but the guide's header promises every anchor was
   live-verified, and a small model sent to :427-438 finds nothing. **Fix:**
   correct the anchor to the actual import block location.

**Non-blocking notes (fix opportunistically in the same pass):**
- Step 14's `.favoritesLine` snippet (14px / `margin: 8px 0 0`) does not match
  the live `.filteredLine` (13px, `margin: 0`, secondary color —
  `DRepDirectoryBanner.scss:48-52`); the guide's own conditional ("copy those
  instead") resolves it, but the snippet should just be corrected so the
  primary path is right.
- Step 20's placement note references `governance.hw.*`/`governance.nav.*`
  neighbors that do not exist at that point in `en-US.json` (the next key after
  the `drepDirectory.*` block is `governance.tabs.directory` :359); the
  governing alphabetical rule is stated and sufficient, so harmless.
- Minor anchor drift, each rescued by exact matching quotes: the Filters
  excludeTop35 conditional is :192-202 (guide :190-201), the DREPS
  `TrackedRoute` is :234-239 (guide :235-240), the Ranking-unavailable story is
  :381-408 (guide :361-390).
- Step 27a says the Ranking-unavailable story needs "the two new required
  props"; only `onToggleFavorite` is required (`favoriteDRepIds` stays
  optional). The instruction itself is correct.
- A few new doc comments run 4 content lines against the 1-3-line comment
  convention (existing file precedent is similar; trim if convenient).

The PRD itself needs no changes — scope, non-goals, interaction mode,
reconciliations, invariants table, DoD, and doc-structure compliance are all
correct and consistent with the tasks JSON and the designs. Both blockers are
confined to the implementation guide and are mechanical to fix in the single
allowed Planner fix pass.

**Decision: requires_changes**

---

## Planner: slice-7 fix pass — 2026-07-27

Single allowed fix pass after the critique. Both blockers resolved in
`slice-7-implementation-guide.md`; the PRD needed no content change and its
planning status is now `approved`.

1. **Step 23 row-identity assertions (blocker 1).** Tests 3-4 no longer match
   full CIP-129 ids. The favorites describe block now pins a
   `truncatedDrepId(n)` helper mirroring `DRepIdDisplay`'s `first8…last6`
   truncation (`DRepIdDisplay.tsx:35-42`) and asserts rows via
   `'!!!View details'` card counts (the file's existing pattern, :564-568,
   :613) plus exact truncated-string positives and `queryByText` negatives.
   The unresolved "reuse whatever id-matching helper" fallback was replaced
   with a pinned rule. Deliberate divergence from the critique's example fix:
   a bare 8-char prefix regex is forbidden, not adopted — react-polymorph
   keeps the tooltip `tip` (the full id) in the DOM as hidden text, so
   `new RegExp(realDrepId(2).slice(0, 8))` matches both the visible `<code>`
   and the hidden tooltip copy and `getByText` throws on multiple elements;
   the exact truncated string matches exactly one element. Fixture ambiguity
   was ruled out live: `realDrepId(1)` starts `drep1ygq`, `realDrepId(2)`
   starts `drep1ygp` — distinct at char 8.
2. **Step 6 anchor (blocker 2).** The `'./helpers'` import block anchor was
   corrected from the nonexistent `helpers.spec.ts:427-438` to the live
   `:10-21`; the semantic insertion instruction was already correct and is
   unchanged.

Non-blocking critique notes fixed in the same pass: Step 14 `.favoritesLine`
now copies the live `.filteredLine` declarations (13px, secondary color,
`margin: 0` — `DRepDirectoryBanner.scss:48-52`); Step 20's placement neighbors
corrected to `governance.tabs.directory` :359 (the `hw.*`/`nav.*` keys do not
exist there); anchor drift corrected (Filters excludeTop35 Checkbox :192-202,
DREPS `TrackedRoute` :234-239, Ranking-unavailable story :381-408); Step 27a
now states only `onToggleFavorite` is required for that story
(`favoriteDRepIds` optional); the 4-line `isStaleFavorite` doc comment trimmed
to 3 lines per the comment convention.

**Planning status: approved** — guide ready for the implementation pass.

---

## Code Review: task-122 — 2026-07-27 (pass 1)

Broad pass over the full uncommitted diff (29 modified files) against the
approved `slice-7-implementation-guide.md`, `slice-7-PRD.md`, and the
task-122 acceptance criteria in the tasks JSON. Every verification command
was re-run live in the worktree.

**Verified green:**

- `node_modules/.bin/tsc --noEmit` → 0 errors.
- Focused Jest (helpers, GovernanceStore, DRepDirectory, DRepDirectoryBanner,
  DRepDirectoryPage, VotingGovernancePage) → 6 suites, 130/130 passed,
  including the two-store app-restart simulation (AC-1/AC-3), toggle
  add/remove with full-array persistence, reference-replacement pin,
  malformed/rejecting-read degradation, the no-logging pins, favorites-route
  container rendering, back-to-directory navigation, the injected stale
  predicate, and the ja-JP empty-state render.
- Sanitization floor: `tests/jest/security/governance-sanitization.spec.ts`
  → 23/23; the suite file is untouched
  (`git diff --name-only | grep sanitization` → empty).
- Diff hygiene: `git diff | grep -E "logger\.|analytics"` shows logger
  references only inside the new Jest assertions that pin its *absence*;
  zero production logger/analytics calls in any new code path. No
  `source/main/` change; `VotingStore.ts` untouched (invariant #4 holds —
  favorites live in `GovernanceStore` only).
- Favorited predicate single-sourced (AC-2): `favoritedOnly` is evaluated
  only inside `filterDReps` (`helpers.ts:215`); the favorites pipeline in
  `DRepDirectory.tsx` and the Favorited checkbox both route through it;
  `DRepDirectoryList`'s `favoriteDRepIds.has(...)` is star display state
  only, as sanctioned by the guide.
- Badges informational (invariant #8): `isStaleFavorite` reads
  `entry.status` only, drives the caption only in the favorites view
  (`isFavoritesView && isStaleFavoriteEntry(entry)`), never ordering or
  filtering; no badge module imported into filtering code; `DRepStatus`,
  `DRepStatusBadge`, `DRepCategoryBadge`, `governance.types.ts` untouched
  (invariant #14).
- i18n: all 9 contract keys present in en-US and ja-JP with leading `!!!`,
  ids byte-identical to `shared-design-tokens.md` §9 rows
  (:161-166, :175, :198-199) plus the minted
  `governance.drepFavorites.banner`; alphabetical placement correct; no
  existing `!!!` stripped; regenerated `defaultMessages.json` /
  `translations/messages.json` ride with the diff and contain every new id
  exactly once.
- Scoped eslint → 0 errors (warnings only, same `no-unused-vars`-on-type-
  parameter class the untouched lines of these files already carry).
- Prettier clean on all touched files except the pre-existing
  `Governance.tsx` parse failure (see notes). Jest assertion style clean:
  no mixed string+object-literal `toHaveBeenCalledWith` anywhere in the new
  tests (single-array and single-string arguments only).
- Storybook: 4 new stories + favorites-aware Connected flow; no local
  IntlProvider, no per-locale variants; the standalone Ranking-unavailable
  story received the new required prop. No `.scss.d.ts` files staged.
- Routing/nav: `/governance/favorites` registered on `DRepDirectoryPage`,
  Favorites nav item added, view derived from `location.pathname`,
  Back-to-directory CTA preserves the delegation-form return state.

**Blockers:** none.

**Non-blocking notes (recorded, no action required before commit):**

1. `governance.drepDirectory.backToDirectory` already existed in both locale
   files (minted by `DRepDetail.tsx` in an earlier slice), so the locale
   diffs add 8 lines, not 9 — the final state still satisfies the contract.
   `DRepEmptyState.tsx` now defines the same message id a second time with
   an identical `defaultMessage` and a different description; react-intl
   resolves by id so behavior is correct, and `defaultMessages.json` lists
   both descriptors. Harmless; a future docs pass may unify the description.
2. `Governance.tsx` fails prettier 2.1.2 with a parse error on the
   pre-existing line 4 inline type import
   (`import { withRouter, type RouteComponentProps } ...`) — present at HEAD
   `73f983a3a`, not introduced or touched by this diff. Known repo drift;
   flag for the pre-merge `nix fmt` outside the devcontainer.
3. Prettier reflowed the untouched `searchIndex` `useMemo` in
   `DRepDirectory.tsx` (prettier 2.1.2 style on a legitimately touched
   file). Cosmetic churn, acceptable.

**Decision: approved**

---

Planner: 2026-07-27 — slice-7 close: The slice closes with its single task.
task-122 (DRep favorites via Electron local store) is implemented and
code-review approved (pass 1, zero blockers); the tracker entry is `complete` —
not `verified`, because AC-5's real `Retired`/`doNotList` stale-state rendering
cannot be exercised until the phases that deliver those status signals land
(invariant #14 / anchor-2 task-153; the shipped stale predicate is false for
every current entry by construction), and the journey-level proof arrives with
slice-8's release-verification acceptance. No `auditSummary` field exists for
slice-7 in the tracker, so this entry is the slice summary. Shipped end-to-end
with zero `source/main/` changes: the `DREP-FAVORITES` key + `LocalStorageApi`
methods, the reassign-only `GovernanceStore.favoriteDRepIds` with setup-kicked
load and full-array-persisting toggle (silent failure handling, zero
logger/analytics calls — the record stands as the second documented invariant-#2
exception, sanctioned by invariant #12), the aria-pressed card toggle, the
Favorited checkbox over the untouched slice-6 predicate, the real
`/governance/favorites` route + Favorites nav tab in a `favorites` view mode
over full `showAllList` membership, the minted banner line with live count, the
`noFavorites` empty state with Back-to-directory CTA, and the forward-compat
stale mechanism with no auto-purge. Verification per the pass-1 review: `tsc
--noEmit` zero errors; focused Jest 130/130 across the six slice suites (incl.
the two-store app-restart simulation and the no-logging pins); sanitization
floor 23/23 with the suite file untouched; scoped eslint clean; prettier clean
on touched files except the pre-existing `Governance.tsx` parse failure (repo
drift, flagged for the pre-merge `nix fmt`). Durable findings F-1…F-5
(targetPath conflict resolved to renderer reuse with the per-network record,
favorites-surface reconciliations + the accepted deregistered-favorite edge,
stale-favorite deferral, the documented sanitization exception, the minted
`drepFavorites.banner` key + duplicated `backToDirectory` descriptor) are
recorded in `research/slice-7-findings.md`, and the PRD Final Outcome is
filled. The tree is intentionally uncommitted at close; the single subject-only
`feat(gov): task-122 …` commit remains the implementer's close-out step.

---
