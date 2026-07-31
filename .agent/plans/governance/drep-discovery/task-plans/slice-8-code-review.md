# slice-8 Code Review Log

> Append-only transcript: `Planner:` entries (planning open/close), one `Critiquer:` entry
> (required review pass over the PRD + guide), and per-task `Code Review:` entries.
> Companion docs: [slice-8-PRD.md](./slice-8-PRD.md) ·
> [slice-8-implementation-guide.md](./slice-8-implementation-guide.md)

---

## Planner: 2026-07-31 — slice-8 planning pass

**Scope.** Three tasks — task-123, task-124, task-125 — all `pending` at
`0cdcab581`. slice-8 is the **last slice of the DRep Discovery feature**; it closes
Track D and the feature. task-123 ships the refresh-latency budget and the
stale-while-refresh *visual* contract the design has specified since slice-1 but the
code only half-implements. task-124 closes the last unreachable UI state, the selfnode
CLI-unsupported empty state, which today leaks a raw main-process string to the user.
task-125 is a release-verification gate that cannot be executed by any agent.

**Interaction mode.** task-123 `autonomous` · task-124 `autonomous` ·
**task-125 `manual_execution`** — a locked, user-owned stop condition. Its tracker row
stays `pending` through slice close and no agent may promote it or infer its result.

**Build order (binding): `123 → 124 → 125`.** slice-8 has **zero intra-slice dependency
edges** — every dependency of every row (task-103, task-115, task-116, task-121,
task-122) is already `complete` — so the JSON listing order encodes no ordering
information. The order is forced by file-level coupling instead: both autonomous rows
edit the same `switch (true)` in `DRepDirectory.renderContent()`
(`DRepDirectory.tsx:240-282`) and both extend the same Storybook state knob
(`DRepDirectory.stories.tsx:242-290`); task-123's edits are structural (one arm
replaced, one banner block replaced, one badge relocated) and task-124's is a single
inserted arm, so landing the structural edit first means the insertion goes into a
settled file. task-123 also owns the slice's only widening of the IPC payload types,
which task-124 must not re-open. task-125 is last because its checklist describes the
end state of the directory.

### User-ruled decisions carried into the plan (D-1 … D-5)

- **D-1 — the selfnode badge is a new message id rendered as plain markup inside the
  empty state; the `DRepStatus` union is not widened.** The design files a
  "Selfnode / CLI unsupported" row in the *status-badge* table, but `DRepStatusBadge`
  takes `status: DRepStatus` where `DRepStatus = 'active' | 'inactive'`
  (`governance.types.ts:35`, `DRepStatusBadge.tsx:20-29`) is a closed union that the
  locked status-grounding invariant forbids widening — selfnode is not an on-chain
  status of any DRep. Resolution: mint
  `governance.drepDirectory.status.selfnodeUnavailable`, render icon-plus-label markup
  inside the `DRepEmptyState` `selfnode` variant, touch neither the union nor the badge
  component, and reconcile the design doc with one clarifying sentence plus one
  microcopy row. Same resolution class as anchor-2's D-7.
- **D-2 — task-123 builds a real DRep-card skeleton list; the existing spinner is not
  sufficient.** The design specifies a full skeleton list for the first-load phase; the
  live render is a centred `LoadingSpinner` (`DRepDirectory.tsx:242-248`) and no
  skeleton component exists anywhere under `components/governance/` (grep: zero hits).
  The guide fixes the placeholder markup and pins the card count at **25**, mirroring
  `CARDS_PER_PAGE` (`DRepDirectoryList.tsx:14`), so the first paint holds the height the
  loaded page will occupy. **This exceeds the row's 4-hour estimate and that is
  accepted** — the estimate is not amended, the design doc is not amended to describe a
  spinner, and the work is not downgraded.
- **D-3 — the main process stays the single timeout authority; `elapsedMs` is
  observational.** `_runCliQuery`'s `setTimeout` (`GovernanceQueryService.ts:382-390`)
  is the only timeout enforcement in the feature and is neither duplicated, moved,
  wrapped nor re-derived. The new `elapsedMs` records how long a completed query took;
  it feeds snapshot age and the deferred latency-measurement remainder, and drives only
  the renderer's skeleton → stale-with-spinner visual progression. **The renderer must
  not run a competing timeout timer.**
- **D-4 — the 118 pre-existing stylelint errors are out of scope and recorded, not
  fixed.** `yarn stylelint` is red at HEAD with 118 errors, every one
  `order/properties-alphabetical-order` and every one in this feature's own governance
  SCSS, including both files task-124 touches. slice-8 fixes none of them, adds no
  cleanup task to the tracker, keeps the lines it touches clean, and records the debt in
  `research/slice-8-findings.md` as a user-owned pre-merge item.
- **D-5 — task-125 is not autonomously buildable; the row stays `pending`.** Its
  acceptance needs a synced node, a packaged build (not the dev shell) and real-device
  hardware-wallet confirmation. The only autonomous deliverable is a
  release-verification **checklist document**; executing it is the user's, and it is
  precisely the row that must not be self-certified.

### Planner rulings added while writing the guide

- **P-1 — locale sort correction (binding, corrects the PRD).** The PRD's i18n
  inventory places `governance.drepDirectory.error.refresh` immediately after
  `governance.drepDirectory.error`. That is wrong: both catalogs are strictly
  `Array.prototype.sort()` ordered (verified — 1652 keys, 0 mismatches), and
  `…error.rankingUnavailable` sorts **before** `…error.refresh` (`a` < `e` at the first
  differing character). The guide pins the corrected position — immediately after
  `…error.rankingUnavailable`, before `…filter.active` — and the correction is recorded
  in the slice findings note. The other two keys' positions in the PRD were correct and
  are re-verified: `empty.selfnode` after `empty.noSync`;
  `status.selfnodeUnavailable` after `status.inactive`.
- **P-2 — stylelint count moves 118 → 111, and that is expected, not a sweep.** The
  PRD's definition of done asks for "exactly 118" at close. task-123 deletes two
  selector blocks that its own edits make dead — `.errorBanner` and `.refreshingBadge`
  in `DRepDirectory.scss` — and deleting a block deletes the errors it contained.
  Measured: those two blocks hold **7** of that file's 19 errors, so the repo total is
  **111** after task-123 and unchanged at 111 after task-124. No `--fix` is run and no
  existing declaration is reordered; every declaration slice-8 adds is alphabetical, and
  the new skeleton stylesheet is clean at birth (0 errors). Shipping dead CSS to keep a
  count at 118 would be the worse outcome. The guide states the expected number so the
  reviewer sees a recorded consequence rather than drift.
- **P-3 — the relocated refreshing badge is a net coverage addition, not a move.** The
  PRD's risk register assumes an existing assertion on the badge would migrate from
  `DRepDirectory.spec.tsx` to `DRepDirectoryBanner.spec.tsx`. Verified: **no test
  anywhere in the repo asserts `!!!Refreshing…`**, so nothing migrates and nothing is
  lost; the guide adds two new assertions in the banner suite instead.
- **P-4 — the single jest snapshot is out of the blast radius.**
  `__snapshots__/DRepDirectory.spec.tsx.snap` contains only the category-badge `<span>`
  (19 lines), so neither the skeleton, the banner replacement nor the badge relocation
  can change it. The guide requires it to stay byte-identical and forbids `jest -u`.
- **P-5 — `<p>` → `<div>` for the last-updated line.** `LoadingSpinner` renders a
  `<div>`, which cannot legally nest inside the existing `<p className={styles.lastUpdated}>`.
  The guide changes that one element to a `<div>` and adds a new alphabetical
  `.refreshingBadge` block, rather than restructuring or reordering `.lastUpdated`,
  which is one of the pre-existing stylelint offenders.
- **P-6 — the selfnode arm fires on the error type alone**, independent of retained
  data, so the "never a partial directory for selfnode" invariant holds even in the
  otherwise-unreachable case where a previously loaded list is on screen. Correspondingly
  `showNoSyncFallback` excludes selfnode on **every** refresh state, not only on its
  `Failed` leg, closing the syncing-plus-selfnode intersection the PRD's risk register
  flagged. A dedicated test pins that intersection.

### Guide shape and shard plan

The guide is written to be implementable end-to-end by a small model reading **only the
guide** — no PRD, no design docs, no orchestrator context. Every step names an
absolute-from-repo-root file, quotes the exact code being replaced with a verified line
anchor, and gives the exact replacement; every locked invariant is reproduced inline
rather than cross-referenced; every design string appears verbatim in both locales.
Tracker updates, the formatter pass and the git commit are explicitly excluded from the
numbered steps and confined to a trailing appendix, because later pipeline stages own
them.

Steps are numbered per task and cut on surface seams so the build pipeline can chain a
fresh agent over each contiguous range:

| task | commit type | shard | steps | surfaces |
|---|---|---|---|---|
| task-123 | `feat` | A | 1–3 | main process + IPC payload types + IPC contract comment |
| task-123 | `feat` | B | 4–9 | skeleton component/SCSS, error-banner variant, directory + header wiring |
| task-123 | `feat` | C | 10–11 | locale catalogs + Storybook state knob and story |
| task-123 | `feat` | D | 12–15 | jest suites (query service, snapshot writer, directory, header) |
| task-124 | `feat` | A | 1–3 | empty-state variant + SCSS + directory routing |
| task-124 | `feat` | B | 4–5 | locale catalogs + Storybook state knob and story |
| task-124 | `feat` | C | 6–10 | jest suites, design-doc reconciliations, slice findings note |

### Measured baselines the guide pins

Focused suites at HEAD `0cdcab581`, all green: **8 suites / 219 tests / 1 snapshot**
(`GovernanceQueryService` 38 · `GovernanceStore` 56 · `logDRepStateSnapshot` 5 ·
`governance-sanitization` 39 · `preliminaryCopyMarkers` 5 · `DRepDirectory` 60 +1 snap ·
`DRepDirectoryBanner` 7 · `DRepDirectoryPage` 9). Expected at slice close: **231 tests**
(task-123 +6, task-124 +6), 1 snapshot unchanged. `yarn compile`, `yarn lint` and
`yarn storybook:build` are all **green at HEAD** and are run, not waived;
`yarn stylelint` is red at HEAD with 118 pre-existing errors and is **not** a gate for
this slice; `yarn check:all` is red transitively and is not used as the gate.

### Owed at slice close — nothing here may be reported green

`nix fmt` (absent in this container, user-owned pre-merge); the 118 pre-existing
stylelint errors; the Storybook **visual** and ja-JP overflow passes for the skeleton,
the refresh-failed banner and the selfnode empty state (no browser here —
`storybook:build` compiling is not a visual pass); **task-125's release verification
itself**; the release-end `!!!` copy review including the two ja-JP placeholders this
slice adds; the deferred latency-measurement remainder that `elapsedMs` feeds but does
not close; the pre-existing prettier drift on the five slice-8-adjacent governance
files; and the first-load refresh-button-disabled design/code divergence, recorded and
unfixed.

---

## Critiquer: 2026-07-31 — required review pass over the PRD + implementation guide

**Verdict: `requires_changes`** — six findings, one of which makes a specified test fail
as written. Nothing in the plan's shape, decisions or scope needs rework: coverage,
D-1…D-5 fidelity, the locked invariants and the small-model implementability bar are all
met. The findings are localized edits to the guide plus one sentence in the PRD.

**What was re-verified against the worktree at `0cdcab581`** (not taken from the brief):
every line anchor the guide pins (payload types `governance.types.ts:139-153`, IPC
comment `api.ts:656-660`, `_doFetchDRepRegistrations` `:224-258` / `_doFetchDRepStake`
`:260-284`, `DRepDirectory.tsx` message entries `:40-44`/`:60-64`, `renderContent`
`:240-282`, the retained-data banner `:324-341`, the refreshing badge `:342-347`,
`DRepDirectoryBanner.tsx:103-109`, `DRepEmptyState.tsx:54-56`/`:73-129`,
`DRepErrorBanner.tsx:14-16`/`:24-26`/`:47-49`, `DRepDirectory.spec.tsx`
`:251-270`/`:272-288`/`:313-320`/`:165`, `DRepDirectoryBanner.spec.tsx:9-43`,
`DRepDirectoryPage.spec.tsx:39-103`, `logDRepStateSnapshot.spec.ts:38-54`,
`GovernanceQueryService.spec.ts` `:174`/`:189`/`:316`/`:609-617`, the stories file's
`SOCKET_ERROR` `:110-113` / `REFRESH_ERROR` `:115-119` / `DIRECTORY_STATE_OPTIONS`
`:242-248` / discrete stories `:420-432` / the locale guard comment `:172-174`, both
locale catalogs at `:352-358` and `:387-389`); the design copy quoted verbatim
(shared-design-tokens `:16`, `:18`, `:93-106`, `:181`, `:184`, `:224`, and the §9
`status.*` rows `:186-189` that Edit 2 groups into); the measured baselines (**8 suites /
219 tests / 1 snapshot green**, exactly the guide's table; 66 `governance.drepDirectory.*`
keys per catalog); the stylelint arithmetic (`DRepDirectory.scss` = 19 errors, of which
**4 in `.errorBanner` + 3 in `.refreshingBadge` = the claimed 7**, so 118 → 111 is
correct); and the two new files the guide dictates verbatim — the proposed
`DRepDirectorySkeleton.scss` measures **0 stylelint errors** and both new files are
already prettier-clean as written.

### Blockers

1. **[high] task-123 Step 14b — `screen.getByText(/a minute ago/)` matches two elements
   and throws.** `DRepDirectory.tsx:381-391` renders `DRepDirectoryBanner` in *every*
   state with `lastFetchedAt`, and `renderComponent` passes `lastFetchedAt={Date.now() -
   60_000}` (`DRepDirectory.spec.tsx:165`), so the header line "!!!Last updated a minute
   ago" is in the DOM alongside the new banner's `{time}` slot. Measured with a throwaway
   harness on this worktree's react-intl 2.9.0 + RTL: `getAllByText(/a minute ago/)` →
   **2**, `getAllByText(/Showing last successful snapshot from a minute ago/)` → **1**.
   Replace the assertion with the unique fragment (or scope the query to the banner
   element). Note the failure mode is not merely a throw: if the regex ever matched only
   the header, the test would pass while proving nothing about the new banner.
2. **[medium] task-123 has no step for AC-1's second half.** AC-1 is "Timing budget
   documented in the IPC contract **and the shared design tokens reference**". Step 3
   discharges the IPC half; the "§6 `:95-102` is already the budget table — confirm it,
   add nothing" instruction lives only in **task-124's** Step 9, which the task-123
   implementer never reads (the guide's own rule is one task at a time, guide-only). Add
   a verify-and-record line to task-123 naming `designs/shared-design-tokens.md:95-102`
   and the existing citation at `GovernanceQueryService.ts:52-56`, with an explicit "add
   no second budget table".
3. **[medium] The zero-renderer-timer gate cannot see the new files.**
   `git diff -- source/renderer storybook | grep -nE '^\+.*(setTimeout|setInterval)'`
   (task-123 matrix step 13, task-124 matrix step 12) diffs tracked content only, and
   `DRepDirectorySkeleton.tsx` / `.scss` are untracked when the check runs — the one
   grep-checkable property NFR-1 rests on silently skips the only new component. Use
   `git add -A` then `git diff HEAD -- source/renderer storybook`, or add a direct
   `grep -rn 'setTimeout\|setInterval' source/renderer/app/components/governance`.
4. **[medium] The formatting rule is wrong in one fact and too narrow in effect.**
   `.prettierignore` re-includes `!*.scss` under `source/`, so the parenthetical in
   task-123's *Files this task edits* — "`.scss` is not in this repo's prettier scope" —
   is false; the new stylesheet must be in the `--write` list (it happens to be clean as
   written, so this is a rule error, not a diff). Separately, measured at HEAD: only
   **5** of the 13 edited files carry pre-existing drift (`api.ts`,
   `GovernanceQueryService.ts`, `DRepDirectory.tsx`, `DRepDirectory.stories.tsx`,
   `GovernanceQueryService.spec.ts`); the other eight — `governance.types.ts`,
   `DRepErrorBanner.tsx`, `DRepEmptyState.tsx`, `DRepDirectoryBanner.tsx`,
   `DRepDirectory.spec.tsx`, `DRepDirectoryBanner.spec.tsx`, `DRepDirectoryPage.spec.tsx`,
   `logDRepStateSnapshot.spec.ts` — are prettier-clean today, so "format only files you
   newly create" lets hand-edits drift them with nothing to catch it. Name the clean
   files as `--write` targets and keep the hand-match rule scoped to the five drifted
   ones.
5. **[minor] PRD D-3 / FR-1 assert a renderer behaviour the guide does not build.** D-3
   says `elapsedMs` "drives the renderer's skeleton → stale-with-spinner visual
   progression"; in the guide the renderer never reads the field at all (correctly — a
   completed-query duration cannot drive an in-flight transition), and the progression is
   driven entirely by `GovernanceRefreshState`. The guide is right; the PRD sentence will
   otherwise be copied into a tracker `statusReason` as a claim that is not true of the
   code. Add one clause to D-3/D-10: the field is snapshot-age and task-166 telemetry
   only, and `GovernanceStore` deliberately does not observe it.
6. **[low] Two mechanical nits in the guide.** task-123 Step 12a says the
   `caches lastSuccessfulData after a successful fetch` test "ends `:327`" — it ends at
   `:324` (the `describe` closes at `:325`); and task-124 Step 8a's code block uses a
   bare `...` elision for the unchanged `buildGovernanceStore` body, which a weak model
   can paste literally. Correct the anchor and replace the elision with an explicit
   "keep the existing properties unchanged; add the third parameter and the trailing
   spread".

### Checked and clean (no action)

- **D-1** — `DRepStatus` is not widened, `DRepStatusBadge` is neither imported nor
  edited, the badge is plain icon-plus-label markup inside the `selfnode` empty state
  under the newly minted `governance.drepDirectory.status.selfnodeUnavailable`, and both
  doc reconciliations are one-line and land in task-124. **D-2** — a real 25-card
  skeleton component with its own stylesheet, no downgrade to a spinner anywhere in the
  guide, and the design doc is left describing the skeleton. **D-3/D-9** — no renderer
  timer of any kind, `_runCliQuery`'s `setTimeout` and both budget constants untouched,
  the budget-pinning test explicitly not edited, and the timeout leg tested from an
  arriving `Timeout` error rather than a clock. **D-4** — no `--fix`, no reordering; the
  118 → 111 movement is purely dead-block deletion and is measured, explained and
  recorded. **D-5** — task-125 has no build steps, its row stays `pending`, and the guide
  forbids simulating or inferring the result.
- **Invariants.** Sanitization floor re-asserted in both task matrices, with the snapshot
  writer's `filterLogData` bypass called out and `logDRepStateSnapshot.spec.ts` re-run and
  extended; `elapsedMs` is a plain millisecond `number`, never a lovelace value or
  decimal string; no probe query, no argv/network-flag change, and the selfnode no-spawn
  property pinned main-side (`_assertQueryable()` throws before any `Promise.all`); the
  selfnode arm fires on error type alone and is proved to replace the list area even with
  a retained list; all three new keys land in both catalogs with `!!!`, byte-matching the
  design copy and the components' `defaultMessage`s.
- **Coverage/consistency spot-checks.** The locale-sort correction the guide makes
  against the PRD is right (`…error.rankingUnavailable` < `…error.refresh`); the guide's
  observation that no existing test asserts the refreshing badge is right, so R-3's
  "assertion moves" becomes a net +2 addition; `renderComponent` already supports
  `locale`, so the ja-JP selfnode test is executable; `renderPage` already returns
  `unmount`; `DirectoryError`, `renderCentered`'s six-parameter signature and the story
  knob all match the fixtures the guide writes; test-count arithmetic (219 → 225 → 231)
  is internally consistent with every step.
- **Concision.** ~2 330 lines for two buildable tasks is proportionate given the
  guide-only mandate; the repeated per-task invariant blocks are required by that mandate,
  not bloat.

---

## Planner (fix pass): 2026-07-31 — disposition of the critique blockers

One pass over both planning documents; every blocker was re-measured against the
worktree at `0cdcab581` before it was accepted. **All six are fixed; none rejected.**
Step numbering is unchanged — task-123 keeps Steps 1–15, task-124 keeps Steps 1–10,
task-125 keeps none — so the build shards are unaffected.

- **C-1 (high) — fixed** (guide, task-123 Step 14b). The two-match assertion
  `getByText(/a minute ago/)` is replaced with
  `getByText(/Showing last successful snapshot from a minute ago/)`, and the note under
  the snippet now names the collision: `DRepDirectoryBanner.tsx:103-109`'s
  `!!!Last updated a minute ago` renders in the same state, so the bare fragment matches
  two elements and `getByText` throws.
- **C-2 (medium) — fixed** (guide, task-123 Step 3 + its verification checklist). Step 3
  gained a closing "confirm the design-token half — no edit" block naming
  `designs/shared-design-tokens.md:95-102` as the existing budget table and
  `source/main/governance/GovernanceQueryService.ts:52-56` as the existing citation, with
  an explicit "do not add a second budget table"; the same confirmation is now the first
  bullet of task-123's confirm-by-inspection list. The criterion is discharged on both
  halves inside task-123, without the implementer reading task-124.
- **C-3 (medium) — fixed** (guide, both full matrices). The untracked-file blind spot is
  closed by a recursive scan run alongside the diff grep:
  `grep -rn 'setTimeout\|setInterval' source/renderer/app/components/governance source/renderer/app/containers/governance storybook/stories/governance`,
  with the diff form changed to `git diff HEAD`. Measured at HEAD: the recursive scan
  returns nothing, so it is a true zero-baseline check.
- **C-4 (medium) — fixed** (guide Shared conventions > Formatting, both "Files this task
  edits" blocks, and the PRD's Definition of Done item 7). Both defects confirmed by
  measurement. (a) `.prettierignore` re-includes `!*.scss`: a deliberately misformatted
  probe stylesheet under `source/renderer/app/components/governance/` was flagged by
  `prettier --check`, so `.scss` is in scope and `DRepDirectorySkeleton.scss` is now in
  the `--write` list. The same probe run on `.md` files under `.agent/` and `source/` was
  **not** flagged — markdown is outside prettier's scope everywhere, so the guide's
  instruction to prettier `slice-8-findings.md` was a silent no-op and has been removed.
  (b) Re-measured drift over all 18 files the two tasks touch: exactly five are dirty at
  HEAD (`source/common/ipc/api.ts`, `source/main/governance/GovernanceQueryService.ts`,
  `DRepDirectory.tsx`, `storybook/stories/governance/DRepDirectory.stories.tsx`,
  `tests/jest/governance/GovernanceQueryService.spec.ts`); the other thirteen are clean.
  The rule is now "format what you create plus the edited files that are clean at HEAD",
  with both lists spelled out by name per task.
- **C-5 (minor) — fixed** (PRD, D-3 and D-10). The user-ruled D-3 text is left verbatim
  and annotated with an "As applied" clause: the renderer deliberately does not observe
  `elapsedMs` — a completed query's duration cannot drive an in-flight transition — so
  `GovernanceStore` stores it nowhere and no component reads it; the
  skeleton → stale-with-spinner progression is owned by `GovernanceRefreshState`, and
  `elapsedMs` is snapshot-age plus deferred latency telemetry. D-10 carries the mirrored
  clause. The ruling's operative half (no second clock in the renderer) is untouched.
- **C-6 (low) — fixed** (guide, task-123 Step 12a and task-124 Step 8a). `:327` corrected
  to `:324` with the enclosing `describe` close at `:325` named so the insertion point is
  unambiguous; the bare `...` elision in `buildGovernanceStore` is replaced with an
  explicit "keep every existing property byte-identical; the only edits are the new
  parameter and the trailing spread", split into two quoted fragments so nothing can be
  pasted literally. The optional rename was applied: `storeOverrides` →
  `governanceOverrides` in all three call sites, matching
  `DRepDetailPage.spec.tsx:96`, `:103`, `:112`.

**Two collateral corrections made in the same pass** (not raised as blockers, but the
documents contradicted each other): the PRD's Definition of Done item 10 and NFR-6 both
required `yarn stylelint` to stay at **exactly 118**, while the guide correctly predicts
**111** after the two dead selector blocks leave with the markup that used them. The PRD
now states 111 with the arithmetic and re-asserts that no `--fix` ran and no declaration
was reordered; D-13's closing sentence was aligned to match.

**Planning status advanced `in_review` → `approved`.** No `source/` file was touched by
this pass.

---

## Code Review (task-123, round 1): 2026-07-31

Scope: the uncommitted working-tree diff at `wt-slice-8` (18 modified files + 2 new
source files), reviewed against the task-123 section of
`slice-8-implementation-guide.md` (guide lines 191-1558) and the tracker's three
acceptance criteria. Doc/tracker/commit/prettier state is owned by later stages and
was not reviewed.

### Verdict

**Approved. Zero blockers.**

Every numbered implementation step (1-15) is present and matches the guide, in most
places byte-for-byte. Nothing outside the guide's "Files this task edits" list was
touched, and no design doc was edited (`git diff --stat -- .agent/.../designs/` is
empty, as Step 3's second half requires).

### Blockers

None.

### Verified independently (not taken from the verifier report)

- **Steps 1-3.** `elapsedMs: number` added as a required field to both
  `DRepListQueryPayload` and `DRepStakeQueryPayload`; measured in the main process
  from a `startedAt` sample taken *after* `_assertQueryable()`, so a selfnode or
  socket-unavailable throw performs no measurement. `fetchedAt` is hoisted to a
  local in both `_do*` methods and reused for the subtraction, so `elapsedMs` and
  `fetchedAt` cannot disagree. The IPC contract comment block carries the 10s/30s
  budget and the "observational, no consumer may schedule a timer from it" clause.
- **D-3 / no second timeout authority.** `git diff HEAD -- GovernanceQueryService.ts`
  contains no change to `REGISTRATION_TIMEOUT_MS`, `STAKE_TIMEOUT_MS` or
  `_runCliQuery`'s `setTimeout`; the only `TIMEOUT_MS` token in the diff is an
  unchanged context line. `grep -rn 'setTimeout\|setInterval'` over
  `source/renderer/app/components/governance`,
  `source/renderer/app/containers/governance` and `storybook/stories/governance`
  returns nothing, and `git diff HEAD -- source/renderer storybook | grep -E
  '^\+.*(setTimeout|setInterval)'` returns nothing. Zero renderer timers.
- **D-2 / real skeleton.** `DRepDirectorySkeleton.tsx` is a genuine 25-card card
  skeleton (three placeholder rows per card), not a re-dressed spinner: no state, no
  effect, no store, no observable, no timer. `LoadingSpinner` is gone from
  `DRepDirectory.tsx` entirely (`grep -n LoadingSpinner` returns nothing).
- **Sanitization floor.** `elapsedMs` is the only new field crossing the wire and it
  is a plain millisecond integer — it names no DRep, carries no bech32 string and
  encodes no vote. `tests/jest/security/governance-sanitization.spec.ts` is green at
  its full 39 tests, and `logDRepStateSnapshot.spec.ts`'s
  `never contains user vote or delegation fields` stays green unmodified, which is
  the actual proof the payload did not widen into anything identifying.
- **Lovelace losslessness.** `elapsedMs` is `number`, never a decimal string, never
  conflated with `Lovelace`. `stakeByDRepId` is untouched.
- **CLI discipline.** No new spawn, no probe/warm-up query, no argv change. The
  measurement wraps the queries that already run.
- **`DRepStatus` union.** Untouched; `DRepStatusBadge` is not in the diff.
- **Copy (AC-2).** `en-US.json:358` is byte-identical to
  `designs/shared-design-tokens.md:184` plus the `!!!` marker — verified with `od -c`
  on the design line, including the straight apostrophe in `Couldn't`. ja-JP carries
  the marker and both `{Retry}` / `{time}` placeholders, spelled identically.
- **No duplicate message ids.** `governance.drepDirectory.loading`,
  `.refreshing` and `.error.refresh` each resolve to exactly one `defineMessages`
  declaration in `source/`; the first two were *moved* to their new owning
  components rather than re-declared, which is tighter than the guide allowed.
- **Comment convention.** Three new comments, all plain sentence case stating a
  why/invariant (`governance.types.ts` `elapsedMs` JSDoc, the skeleton's card-count
  rationale, the banner's retained-data note). No task id, no `CAT-*`/`CP-*`, no
  plan name, no PR number, no change-history narration, no ALL-CAPS emphasis — in
  source or in test names. The stale `// Only the rankingUnavailable variant ships
  for now…` guard comment was correctly deleted rather than amended.
- **Commands re-run in this worktree, not inherited:** `yarn compile` exit 0 (18.4s,
  `git status` unchanged afterwards); `eslint --quiet` over the seven touched source
  files exit 0; focused jest — `DRepDirectory.spec.tsx` + `DRepDirectoryBanner.spec.tsx`
  = 70 passed / 1 snapshot passed, and `GovernanceQueryService` + `logDRepStateSnapshot`
  + `governance-sanitization` + `preliminaryCopyMarkers` + `GovernanceStore` +
  `DRepDirectoryPage` = 155 passed. Both totals reconcile exactly to the guide's
  predicted per-suite deltas (40/6/39/5/56/9 and 61/9).

### Minor (recorded, no change required)

1. `DRepDirectory.spec.tsx` — the renamed test
   `replaces the retained-data banner text with the snapshot-age copy` narrates the
   change ("replaces") rather than stating the behaviour. It is verbatim from the
   guide, so it is accepted as-is; a future pass could read
   `renders the snapshot-age copy in the retained-data banner`.
2. `DRepDirectorySkeleton`'s `count` prop has no caller — the guide justified it as a
   story escape hatch, but no story uses it. Harmless unused surface.
3. `DRepErrorBanner`'s `retryLabel` defaults to `''`, so a future caller that selects
   `variant="refreshFailed"` without a label would render an empty link inside the
   sentence. The single production caller passes it.
4. AC-3's stale-with-spinner leg is asserted at the `DRepDirectoryBanner` level
   (badge present / absent) but no `DRepDirectory`-level test pins that the list
   stays rendered while `refreshState === Refreshing`. Behaviour is unchanged from
   HEAD — `Refreshing` never had a `renderContent()` arm and still falls through to
   the list — so this is a pre-existing gap, not a regression.
5. `shared-design-tokens.md:106`'s "absolute ISO timestamp in tooltip" on the
   Last-updated line remains unimplemented. Pre-existing; not one of the guide's
   numbered steps.

### Owed at close (for the scribe and the slice-close stage, not this diff)

- **`yarn storybook:build` must not be reported green.** The guide's verification
  matrix step 12 (guide `:1487-1489`) and the per-shard row (`:1430`) assert exit 0
  on the strength of the grounding brief's §7/C-9(a) measurement; the verifier
  measured it red (manager webpack has no JSX loader for
  `storybook/addons/DaedalusMenu/register.tsx`). Independently confirmed here that
  this cannot be task-123's doing: `git diff HEAD --name-only -- .storybook
  storybook/addons package.json webpack.config.js '*.config.js'` is **empty**, so
  every manager-compilation input is byte-identical to HEAD, and the only storybook
  file this task touched (`storybook/stories/governance/DRepDirectory.stories.tsx`)
  feeds the preview build. Whatever its exit code, the gate is unattributable to
  this task — the tracker `statusReason` must not claim it green, and the guide row
  should be corrected to a waiver.
- **118 pre-existing `yarn stylelint` errors** (all `order/properties-alphabetical-order`,
  all in this feature's own governance SCSS) stay out of scope per D-4 and belong in
  the slice findings note as a user-owned pre-merge cleanup item. This diff adds
  none: the new `DRepDirectorySkeleton.scss` is alphabetical throughout, both
  appended blocks (`.retryLink`, `.refreshingBadge`) are alphabetical, and the only
  SCSS removals are the two dead selector blocks Steps 8 and 9d mandate.
- **`nix fmt`** remains unrunnable in this container; the pre-drifted files were
  hand-matched as the guide requires.

---

## Code Review (task-124, round 1): 2026-07-31

Scope: the uncommitted working-tree diff at `wt-slice-8` (12 modified files + 1 new
research note) on top of the committed task-123 tip `50b23a5f0`, reviewed against the
task-124 section of `slice-8-implementation-guide.md` (guide lines 1562-2342) and the
tracker's two acceptance criteria. Tracker row, commit and prettier state are owned by
later stages and were not reviewed.

### Verdict

**Approved. Zero blockers.**

All ten numbered implementation steps are present and match the guide, most of them
byte-for-byte. Nothing outside the guide's "Files this task edits" list was touched.

### Blockers

None.

### Verified independently (not taken from the verifier report)

- **D-1 held in full.** `git diff --stat` shows **no** file under `source/main/` or
  `source/common/` in this task's diff. `DRepStatusBadge.tsx` and `.scss` are
  untouched and never imported by the new code; `GovernanceQueryErrorType`
  (`source/common/types/governance.types.ts:165-173`) gained no member; the
  `DRepStatus` union is unchanged. The badge ships as plain markup — a
  `<span className={styles.unavailableBadge}>` holding an `aria-hidden` warning
  triangle plus the textual label — inside the new `selfnode` early return of
  `DRepEmptyState`, exactly as the decision requires. Icon **and** text satisfy the
  §1 contrast rule's "colour must never be the sole indicator".
- **The task title's "IPC payload" is correctly a no-op.** The selfnode error already
  crosses the wire end to end and I traced every hop: thrown once at
  `GovernanceQueryService._assertQueryable()` (byte-identical to base — it is not in
  the diff) → re-thrown as the marked plain object by `governanceChannel.ts` → read
  back verbatim by `GovernanceStore._normalizeError()` (`:573-589`, `type` passed
  through as a raw string) → compared in `DRepDirectory.tsx:221-222` against
  `GovernanceQueryErrorType.SelfnodeCliUnsupported`, whose literal value is
  `'SELFNODE_CLI_UNSUPPORTED'`. The string the tests and the Storybook fixture use is
  that same literal, so the fixtures are not a fiction.
- **Invariant "no partial directory for selfnode" is genuinely enforced, not just
  asserted.** `isSelfnodeUnsupported` is derived from the error type alone
  (`DRepDirectory.tsx:221-222`) and its `switch (true)` arm sits at `:243-244`, ahead
  of both the `Failed` arm and the `default:` arm that owns the list. Because
  task-123 moved `showErrorBanner` inside `default:` (`:317-324`), the selfnode path
  now short-circuits before the refresh-failed banner too — a selfnode user cannot
  see "Showing last successful snapshot from …" next to an empty state. The
  `Loaded` + retained-list case (which the store really produces, since a failure
  with retained data is demoted to `Loaded`) is the third component test, and it is a
  true behavioural test: without the new arm that case falls to `default:` and renders
  cards, so the `queryByText('!!!Voting power:')` assertion would fail.
- **`showNoSyncFallback` rewrite is behaviour-preserving except where intended.** Old
  predicate excluded selfnode only on its `Failed` leg; new predicate hoists
  `!isSelfnodeUnsupported` out of the parenthesis so the `Loaded` leg is covered too.
  For every non-selfnode error the truth table is unchanged. The syncing banner at
  `:369-394` is outside `renderContent()` and is neither suppressed nor duplicated;
  the second component test pins that the selfnode copy wins over the `noSync` copy
  while `isNodeInSync` is false.
- **No renderer timer added (D-3).** `grep -rn 'setTimeout\|setInterval'` over
  `source/renderer/app/components/governance`,
  `source/renderer/app/containers/governance` and `storybook/stories/governance`
  returns nothing, and the diff adds no timer of any kind. The main process remains
  the sole timeout authority.
- **CLI discipline / no retry loop.** The `DRepDirectoryPage.tsx` container is *not*
  in the diff — the guide's ruling that no mount guard be added was followed. The
  cost of the per-mount `refresh()` is pinned main-side instead:
  `tests/jest/governance/GovernanceQueryService.spec.ts` gains
  `issues no CLI invocation across repeated selfnode refreshes`, and I confirmed the
  suite's `beforeEach` runs `mockSpawn.mockReset()` (`:161-162`) so
  `expect(mockSpawn).not.toHaveBeenCalled()` is a real assertion rather than a
  survivor of an earlier test's state.
- **Sanitization floor.** The selfnode path logs nothing new; no logger, analytics or
  electron-store call appears in the diff. `tests/jest/security/governance-sanitization.spec.ts`
  re-run green (39/39). No DRep id, no `abstain`/`no_confidence`, no bech32 string
  appears in any string this task adds.
- **i18n.** Both new ids are present in **both** catalogs with the leading `!!!`,
  inserted at the correct `Array.prototype.sort()` positions
  (`empty.noSync` < `empty.selfnode` < `error`; `status.inactive` <
  `status.selfnodeUnavailable` < `syncing`). The en-US empty-state string is verbatim
  from the design's §9 row `:181` and the ja-JP badge label `!!!DRepデータ利用不可` is
  verbatim from the §1 table row `:16`. `defaultMessages.json` and
  `translations/messages.json` carry the same two entries, i.e. `yarn i18n:manage`
  output, not hand edits. Neither new id is defined anywhere else in `source/`, so
  there is no duplicate-id collision.
- **Design-doc reconciliations are exactly the two the guide allows.** The §1
  "Selfnode / CLI unsupported" table row at `:16` still exists untouched; one
  sentence was appended to the status-grounding paragraph at `:20`; one microcopy row
  was added at `:190` inside the `status.*` group. No other doc changed
  (`git diff --stat -- .agent/` lists only `shared-design-tokens.md`).
- **Stylelint (measured, not a gate).** `node_modules/.bin/stylelint …/DRepEmptyState.scss`
  reports exactly the 3 pre-existing `order/properties-alphabetical-order` errors at
  `:4`, `:6`, `:14`. Both appended blocks (`.unavailableBadge`, `.unavailableIcon`)
  are alphabetical and contribute zero. `--badge-disabled-bg` is undeclared in the
  themes, but so are `--badge-success-bg` / `--badge-neutral-fg` in the shipped
  `DRepStatusBadge.scss`; the `var(--token, fallback)` shape matches the existing
  convention and the token name is the one §1 specifies.
- **Comment convention.** The diff adds no comment to any source file except two
  lines appended to the existing `showNoSyncFallback` rationale in
  `DRepDirectory.tsx:226-227`. No task id, no `CAT-*`/`CP-*`, no plan name, no PR
  number, no change-history narration and no ALL-CAPS emphasis in any added comment
  or test name. The deleted two-line `DRepEmptyStateVariant` guard comment was
  removed rather than amended, as the guide requires.
- **Commands re-run here.** `yarn compile` exit 0 (18.5 s); focused Jest over
  `DRepDirectory.spec.tsx` + `DRepDirectoryPage.spec.tsx` +
  `GovernanceQueryService.spec.ts` + `preliminaryCopyMarkers.spec.ts` +
  `governance-sanitization.spec.ts` → 5 suites / **160** tests / 1 snapshot passed,
  exit 0 (65 + 10 + 41 + 5 + 39, matching the guide's predicted 65 / 41 / 10 and the
  unchanged 5 / 39). `git status --short` unchanged by the runs — the regenerated
  `*.scss.d.ts` are gitignored.

### Minor (non-blocking, no change requested)

1. `DRepDirectory.tsx:224-227` — the `showNoSyncFallback` rationale is now a
   four-line comment block, one line over the convention's "1-3 plain lines". Both
   sentences carry real why-information, the text is guide-mandated verbatim, and
   nothing in it is banned narration. Left as is.
2. `keeps the selfnode empty state across remounts` asserts one `refresh()` per mount
   against two *different* store mocks, so it proves "a remount re-fires exactly one
   refresh and re-renders the empty state" rather than any cross-mount retention. That
   is the property the guide asks for and the no-CLI half is proved main-side; the
   name is slightly more ambitious than the assertions.
3. The selfnode empty state offers no retry affordance while `DRepDirectoryBanner`'s
   Refresh button stays live, so a selfnode user can still click Refresh and get the
   same state back. Correct per the guide (the guard throws before any spawn) and
   deliberate — recorded only because it is the one interaction the empty state does
   not explain.
4. With `isNodeInSync === false` the syncing banner and the selfnode empty state show
   together ("still syncing (n%)" above "unavailable on the selfnode cluster"). The
   guide explicitly forbids suppressing the banner, so this is the intended rendering,
   but it is the one place where two overlapping explanations appear at once.

### Owed at close (for the scribe and the slice-close stage, not this diff)

- **`yarn storybook:build` must not be reported green for this task either.** The
  verifier measured it red and controlled for it by extracting a pristine
  `0cdcab581` tree to `/home/node/.claude/jobs/3bad97d1/tmp/base-sb` with the same
  `node_modules` symlink and reproducing the identical manager-bundle
  `ModuleParseError` on `storybook/addons/DaedalusMenu/register.tsx`. Consistent with
  the task-123 round-1 finding above. Consequence to state plainly in the tracker: the
  new `Selfnode unavailable` story has **no bundle-level check** in this environment.
  It is not unchecked at the type level, though — `tsconfig.json` has no `include`
  and excludes only `node_modules`, so `yarn compile` typechecks
  `storybook/stories/governance/DRepDirectory.stories.tsx`, and `yarn lint` covers it.
- **The 118 → 111 pre-existing `yarn stylelint` errors** stay out of scope per D-4 and
  belong in the slice findings note as a user-owned pre-merge cleanup item. task-124
  adds 0 and fixes 0.
- **`nix fmt`** remains unrunnable in this container; the three pre-drifted files this
  task edits (`DRepDirectory.tsx`, `DRepDirectory.stories.tsx`,
  `GovernanceQueryService.spec.ts`) were hand-matched as the guide requires.

---

## Planner: slice-8 slice close (2026-07-31)

**Status: slice-8 is closed, and with it the DRep Discovery feature's build phase.** The
three rows were enumerated by loading `governance-drep-discovery-plan-tasks.json` and walking
the `slice-8` phase, not read off by eye: task-123 `complete`, task-124 `complete`,
**task-125 `pending`**. **No row is promoted to `verified`, and the word appears on no
slice-8 row** — `verified` needs proof beyond a task's own unit tests, and the only such
proof this feature defines is task-125's release verification, which is precisely the row
that stays open. The feature therefore closes with its own definition of "verified"
unreached by any row. That is the designed outcome, not a defect.

**What shipped, in build order.** `50b23a5f0` task-123 — one new wire field, a plain-number
`elapsedMs` on `DRepListQueryPayload` and `DRepStakeQueryPayload` measured around work
`GovernanceQueryService` already performs (no probe query, no new `spawn`, no argv change)
and sampled after `_assertQueryable` so a selfnode throw measures nothing; the budget
documented in the governance IPC channel comment block; `DRepErrorBanner` gaining a
`refreshFailed` variant that renders the design tokens' `error.refresh` copy byte-identically
and retires a hand-rolled banner that had been printing raw main-process error strings to the
user; the first-load spinner replaced by a real 25-card `DRepDirectorySkeleton`; and the
stale-while-refresh spinner badge moved beside the "Last updated" timestamp. →
`45efc1911` task-124 — the selfnode arm, which needed **no** wire change at all despite the
task title, since `SelfnodeCliUnsupported` already existed, is thrown in exactly one place,
crosses IPC as a plain object and survives `_normalizeError`; not one file under
`source/main` or `source/common` was edited, and the whole task is a renderer arm placed
ahead of the `Failed` arm plus two copy keys. → this close-out commit, which adds task-125's
only autonomous deliverable, `release-verification-checklist.md`, and changes no code.

**Both buildable rows were approved in round one with zero blockers.** The minors are
recorded above under `Code Review (task-123, round 1)` and `Code Review (task-124, round 1)`
and are carried forward in the PRD's Final Outcome rather than silently dropped:
`DRepDirectorySkeleton`'s `count` prop has no caller; `DRepErrorBanner`'s `retryLabel`
defaults to an empty string; no `DRepDirectory`-level test pins that the list stays rendered
while `Refreshing`, which is a pre-existing gap because `Refreshing` never had a
`renderContent` arm; the absolute-ISO-timestamp tooltip on "Last updated" is unimplemented;
and the syncing banner renders together with the selfnode empty state when the node is not in
sync, which the design requires.

**The two user-ruled decisions that were most likely to be quietly downgraded both held.**
D-2 asked for a real skeleton list and explicitly accepted that it exceeds task-123's 4 h
estimate; the cheap move — keep the spinner, amend the design doc — was available and was not
taken. D-3 kept the main process as the single timeout authority; `elapsedMs` is purely
observational and the renderer gained **zero** timers, which is grep-checkable and was
checked. D-1's selfnode indicator shipped as plain markup under a newly minted id rather than
by widening the locked `DRepStatus` union. D-4's stylelint debt was recorded, not swept.
D-5 held: the checklist is a document, and writing it does not close the row.

**One planning premise was wrong, and the correction runs against this slice's own text.**
The planning pass recorded `yarn storybook:build` as green at HEAD and wrote "run it, do not
waive it" into the guide's verification matrix and into Definition of Done item 8. Both task
verifiers measured it **red** and controlled for it by reproducing the identical
manager-bundle `ModuleParseError` on `storybook/addons/DaedalusMenu/register.tsx` from a
pristine `0cdcab581` tree. Re-measured at slice close: **exit 1**, same error, and
`git diff 0cdcab581..45efc1911 -- storybook/addons/` is empty. The guide's matrix row and its
"corrected gate premises" note were corrected at close, the measurement is written up as
finding F-2, and the disposition is now **waived with the reason recorded**. The consequence
is stated rather than buried: the stories slice-8 adds have no bundle-level check in this
environment, though `yarn compile` typechecks them and `yarn lint` covers them. The
`yarn compile` half of that same premise held — it is green, no `typed-scss-modules`
substitute needed.

**Tracker verification performed at close, not assumed.** task-123's and task-124's rows were
re-read against the commits that landed after the scribe stage wrote them. Field order,
`updatedAt` format and `evidence` shape are correct on all three rows; both evidence arrays
match their commits file-for-file; neither row was over-promoted, so neither needed a
downgrade. One stale clause was corrected: task-123's `statusReason` said the guide's
storybook entry "is owed a correction to a waiver" — that correction has now landed, so the
clause was rewritten to say where it landed and what the residual consequence is. task-125
gained a `statusReason` naming it as user-owned manual release verification with the
environment it needs, and an `evidence` array pointing at the checklist. **No `auditSummary`
was added** — the `slice-8` phase object has none and none was invented. No other phase and
no `summary` field was touched, and the JSON was edited value-only and re-parsed.

**Gates at close, re-measured at `45efc1911` with a clean tree** (full table in the PRD and in
findings F-11): unfiltered `node_modules/.bin/jest --runInBand` **exit 0**, 92 passed + 1
skipped of 93 suites and 1334 passed + 12 skipped of 1346 tests; the focused slice-8 surface
7 suites / 175 tests / 1 snapshot, a total that reconciles exactly with the per-task deltas
(41 + 65 + 10 + 9 + 6 + 39 + 5 = 175) and is the independent check that no delta was
mis-stated; `yarn compile` exit 0; `yarn lint` exit 0 with 0 errors; `yarn i18n:manage` a
byte-identical no-op; `yarn stylelint` **red at 111** errors, all pre-existing governance
SCSS ordering, 0 added; `yarn storybook:build` red and waived as above.

**Owed at close — nothing here is green, and no promotion docket is handed forward.**
`nix fmt` never ran (no `nix` in this container; explicit-path `prettier --write` was the
substitute and is not the mandated formatter). The 111 stylelint errors and the pre-existing
prettier drift on 12 governance files are user-owned pre-merge cleanup. The Storybook visual
pass and the ja-JP overflow check have no environment here. The release-end `!!!` copy review
and the task-166 latency remainder stay open by design. And **task-125's run itself** — a
packaged build, a synced node, both wallet types and a real device, performed by a human — is
the feature's terminal stop condition. The checklist is delivered; the run is the user's.
