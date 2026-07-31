# Slice-8 Findings — Refresh-latency, Selfnode & Release Verification

> Durable findings from slice-8 (2026-07-31). Facts only; every figure below was
> measured in the `wt-slice-8` working tree, at HEAD `0cdcab581` for the
> pre-slice baselines and after the task-123 commit `50b23a5f0` for the
> post-state. Companion docs:
> [slice-8-PRD.md](../task-plans/slice-8-PRD.md) |
> [slice-8-implementation-guide.md](../task-plans/slice-8-implementation-guide.md) |
> [slice-8-code-review.md](../task-plans/slice-8-code-review.md)

---

## F-1 (slice-8) — `yarn stylelint` is red at HEAD with 118 errors, all of them this feature's own SCSS; slice-8 moved it to 111 by deleting dead code, not by sweeping

At HEAD `0cdcab581` `yarn stylelint` exits 2 with **118** errors. Every one is
`order/properties-alphabetical-order`, and every one is in a stylesheet this
feature shipped: `governance/drep-detail/DRepDetail.scss`;
`governance/drep-directory/{DRepCard,DRepDirectoryBanner,DRepDirectoryFilters,DRepDirectoryList,DRepDirectory,DRepDirectorySearch}.scss`;
`governance/_shared/{DRepCategoryBadge,DRepEmptyState,DRepErrorBanner,DRepIdDisplay,DRepStatusBadge}.scss`;
`voting/voting-governance/CurrentVoteSummary.scss`. No file outside governance
fails. The debt is **out of scope for slice-8** and remains a user-owned
pre-merge cleanup item — the slice ran no `--fix` and reordered no existing
declaration.

Measured after task-123: **111** errors across the same 13 files. The
seven-error drop is entirely a consequence of task-123 deleting the two selector
blocks that became dead when the code using them was replaced — `.errorBanner`
and `.refreshingBadge` in `DRepDirectory.scss` (commit `50b23a5f0`). Every
declaration slice-8 added is alphabetical inside its block, so the new
`DRepDirectorySkeleton.scss` is absent from the failing list (clean at birth) and
`DRepEmptyState.scss` / `DRepErrorBanner.scss` / `DRepDirectoryBanner.scss` each
hold exactly their pre-slice error count. task-124 adds 0 and fixes 0; the total
stays at 111.

## F-2 (slice-8) — `yarn compile` is green without the workaround; `yarn storybook:build` is green in the checkout and red only in a symlinked-`node_modules` worktree

Two gate premises were in dispute across the slice. Both are now settled by
measurement at the slice-close HEAD `2f2011edd`.

**`yarn compile` — green, no workaround.** Exit 0 (18.4 s), its `precompile` hook
regenerating the gitignored `*.scss.d.ts` and leaving `git status` clean. A
preceding slice's tracker `statusReason` asserts it needs a `typed-scss-modules` +
`tsc --noEmit` substitute. That does not hold here; the substitute is unnecessary.

**`yarn storybook:build` — green in the working checkout; the red readings were a
worktree artifact.** The same commit `2f2011edd` was measured twice:

| Location | Result |
| --- | --- |
| `/workspaces/daedalus` (the real checkout) | **exit 0**, 73.4 s |
| detached worktree with `node_modules` symlinked in | **exit 1**, `ModuleParseError` |

The worktree failure is *"Module parse failed: Unexpected token (12:18)"* on
`storybook/addons/DaedalusMenu/register.tsx` — the JSX at
`render: () => <DaedalusMenu api={api} />`. Because the code is byte-identical
between the two runs, the manager webpack's loader resolution is what differs: it
does not resolve its loaders through a symlinked `node_modules`. The failure is a
property of the **isolation setup**, not of the repository.

This corrects the direction the slice's own verifiers recorded. Their reproductions
were real, but every one of them ran inside a symlinked worktree, so the shared
premise rather than the code produced the shared error; the planning pass's
"exit 0, 84 s" reading was the accurate one, because it ran in the checkout. The
slice-8 PRD's Definition of Done item 8 ("green at HEAD — run it, do not waive
it") therefore stands as written, and the two task `statusReason` fields that
recorded a waiver were corrected in the same commit as this note. Any earlier
background job that recorded `yarn storybook:build` as red at HEAD should be
re-read with this in mind — the reading is only valid from the checkout.

**Consequence.** The three stories slice-8 adds — the refresh-latency knob states,
the skeleton and the `Selfnode unavailable` story — **are** covered by a
bundle-level check, provided it is run from the checkout rather than a worktree.
They are additionally typechecked: `tsconfig.json` declares no `include` and
excludes only `node_modules`, so `yarn compile` covers
`storybook/stories/governance/DRepDirectory.stories.tsx`, as does `yarn lint`. The
**visual** pass remains owed regardless — a compiling bundle was never a visual
pass.

**No closed slice's tracker entry was edited.** A closed slice's `statusReason` is
the record of what that slice measured at its own HEAD; rewriting it
retroactively would destroy the audit trail. Corrections to a closed measurement
belong here. The correction to slice-8's *own* planning text belongs here too,
which is why it is stated as a correction rather than quietly applied.

## F-3 (slice-8) — Recorded, not fixed: the design disables the refresh button during first load; the code disables it only while refreshing

`drep-discovery-design.md:195` states that the first-load row renders the
skeleton list with the "refresh button disabled", but
`DRepDirectoryBanner.tsx:105` passes `disabled={isRefreshing}`
only — during `GovernanceRefreshState.Loading`, with the skeleton list on screen,
the button is live. No slice-8 acceptance criterion asks for the first-load
disable, so the divergence is recorded here and left unchanged rather than fixed
opportunistically.

## F-4 (slice-8) — Locale-catalog sort correction: `error.refresh` sorts *after* `error.rankingUnavailable`

The slice PRD's i18n inventory placed `governance.drepDirectory.error.refresh`
between `governance.drepDirectory.error` and
`governance.drepDirectory.error.rankingUnavailable`. That is wrong under
`Array.prototype.sort()`: at the first differing character `a` < `e`, so
`…error.rankingUnavailable` < `…error.refresh`. Verified against the catalogs'
actual ordering — 1655 keys after slice-8's three insertions, **0** sort
mismatches, with the three new keys landing after `empty.noSync`, after
`error.rankingUnavailable` and after `status.inactive` respectively. Any future
insertion should be placed by running the sort, not by eyeballing the dotted
segments.

## F-5 (task-124) — The selfnode indicator could not be a status badge, so it ships as plain markup inside the empty state under a newly minted id

The design's §1 status-badge table carries a `Selfnode / CLI unsupported` row
with both locale labels, but §9 minted no message id for it, and the row cannot
be implemented as a `DRepStatusBadge` variant: `DRepStatusBadge` is an exhaustive
`Record<DRepStatus, string>` over the closed `'active' | 'inactive'` union that
the status-grounding invariant forbids widening. The resolution is that the
selfnode indicator is a **directory-level state indicator, not a per-DRep
on-chain status** — it renders as plain markup (inline warning-triangle `<svg>`
plus textual label, so colour is never the sole indicator) inside the new
`DRepEmptyState` `selfnode` variant, under the id
`governance.drepDirectory.status.selfnodeUnavailable` minted by task-124. Both
halves of the doc were reconciled in place: one clarifying sentence under the §1
table and one §9 microcopy row. The §1 table row itself was kept — its labels and
`--badge-disabled-bg` token remain the contract for the badge's copy and styling.

## F-6 (task-124) — A selfnode user re-fires `refresh()` on every mount, and that is correct because the guard throws before any `spawn`

`DRepDirectoryPage.componentDidMount` refreshes when `refreshState` is `Idle`
**or** `Failed`, so a selfnode user — permanently `Failed` — issues one
`refresh()` per mount. This looks like a retry loop and is not one:
`_doFetchDRepRegistrations()` calls `_assertQueryable()` as its first statement,
which throws `SelfnodeCliUnsupported` before the `Promise.all` and therefore
before any `spawn`. The cost of the "retry" is one synchronous throw and **zero**
CLI invocations.

The correct fix was therefore to pin the property rather than to teach the
container about node capabilities: a main-process test asserts `mockSpawn` is
never called across repeated selfnode refreshes, and a container test asserts the
empty state survives a remount with exactly one `refresh()` per mount. Adding a
mount guard would have put node-capability knowledge in a container that should
not carry it, and would have hidden the real guarantee behind a suppressed call.

## F-7 (task-123) — The main process stays the single timeout authority; `elapsedMs` is observational and the renderer runs no timer at all

The task text asks for "elapsed time and refresh state through the governance IPC
payloads", which reads like a licence to schedule the ≤700 ms and 10 s thresholds
in the renderer. It is not. `_runCliQuery`'s `setTimeout` in
`GovernanceQueryService` is the **only** timeout enforcement in the feature:
phase 1 rejects at `REGISTRATION_TIMEOUT_MS` (10 s) and phase 2 at
`STAKE_TIMEOUT_MS` (30 s), so the renderer never observes an in-flight request
older than the budget and a renderer clock could only race it — firing a banner
with no error, or leaving an error with no banner, depending on IPC latency.

The shipped resolution: the wire carries exactly one new field, a plain-number
`elapsedMs` on `DRepListQueryPayload` and `DRepStakeQueryPayload`, sampled around
work the service already performs (no probe query, no extra `spawn`, no argv
change) and sampled **after** `_assertQueryable`, so a selfnode throw measures
nothing. It is **purely observational** — it feeds the "last updated" reasoning
and the still-open task-166 latency measurement. Refresh *state* stays
renderer-owned in the existing `GovernanceRefreshState` machine; the skeleton is
driven by `refreshState === Loading`, not by a clock. The property is
grep-checkable and was checked: **zero** new `setTimeout` / `setInterval` in
`components/governance`, `containers/governance` or `storybook/stories/governance`.

`elapsedMs` is admissible under the sanitization floor for a stated reason, not by
default: widening `DRepListQueryPayload` widens `Logs/pub/DRep-state-snapshot.json`,
which deliberately bypasses `filterLogData`, and a millisecond integer names no
DRep id, no bech32 string and no vote. **That reasoning does not generalize** — the
next field added to this payload needs its own argument, and
`logDRepStateSnapshot.spec.ts` must be re-run when one is.

## F-8 (task-123) — The ≤700 ms first-load phase ships as a real skeleton list; the spinner was not sufficient

Both the design tokens' refresh-state table and the discovery design's state table
specify a **full skeleton list** for the initial load, and the code had been
rendering a centred `LoadingSpinner` with "Loading DRep data…" since the state
machine was built. The gap was closed rather than reconciled away: slice-8 adds
`DRepDirectorySkeleton`, a pure presentational component rendering 25 placeholder
cards — the directory's page size, so the first paint holds the height the loaded
page will occupy and the list does not jump when real cards arrive.

The component is deliberately inert: no state, no store, no timer, no data, one
optional `count` prop, `role="status"` + `aria-busy` + the existing
`governance.drepDirectory.loading` string as its accessible label (no new copy
key). This knowingly exceeded task-123's 4 h estimate. Recording it because the
cheap alternative — keeping the spinner and amending the design doc — is the
tempting move for anyone revisiting this, and it was explicitly rejected: the
skeleton is a first-paint layout-stability property, not decoration. Its
stylesheet is also the one governance SCSS file that is stylelint-clean at birth
(see F-1).

## F-9 (slice-8) — `nix fmt` cannot run in this devcontainer; the substitute is `prettier --write` on explicit paths, and the real formatter remains owed

The repository's mandated formatter is `nix fmt`. **There is no `nix` in this
devcontainer**, so every slice-8 task substituted
`node_modules/.bin/prettier --write <explicit changed paths>` — explicit paths
only, never `yarn prettier`, whose script is
`./node_modules/.bin/prettier "**/*.*"` and would reformat ~240 unrelated
pre-drifted files in one commit. The tasks tracker JSON, the locale catalogs and
`translations/messages.json` are tool-managed and were never prettier-formatted.
Markdown under `.agent/` is outside prettier's scope entirely: the repo's
`.prettierignore` ignores everything at the root and re-admits only `source/`,
`features/`, `storybook/`, `hardware-wallet-tests/` and `tests/`, so a
`prettier --write` on these plan docs matches zero files.

**`nix fmt` is therefore a user-owned pre-merge obligation for this whole slice**,
not a discharged gate. Nothing in slice-8 may be read as claiming the mandated
formatter ran.

## F-10 (slice-8) — Twelve governance files carry pre-existing prettier drift; slice-8 deliberately did not reformat them and added none

`prettier --check` over the governance surfaces flags **12** files as already
drifted at HEAD `0cdcab581`, before slice-8 touched anything:

```
source/renderer/app/components/governance/_shared/DRepCategoryBadge.spec.tsx
source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx
source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx
source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx
source/main/governance/GovernanceQueryService.ts
source/common/ipc/api.ts
source/main/ipc/governanceChannel.ts
tests/jest/governance/AnchorFetchService.spec.ts
tests/jest/governance/GovernanceQueryService.spec.ts
tests/jest/governance/GovernanceStore.spec.ts
storybook/stories/governance/DRepDirectory.stories.tsx
storybook/stories/governance/_utils/fixtures.ts
```

Re-measured at the slice-close HEAD `45efc1911`: the **same 12 files, no more** —
slice-8 introduced no new drift. Five of them are files slice-8 edited
(`api.ts`, `GovernanceQueryService.ts`, `DRepDirectory.tsx`,
`DRepDirectory.stories.tsx`, `GovernanceQueryService.spec.ts`); each was
hand-matched to the surrounding style and left otherwise untouched, because
`--write`-ing them would have buried a 50-line task diff under hundreds of lines
of unrelated reformatting.

Two traps worth carrying forward. First, the root cause is that prettier 2.1.2
does not stabilize on some constructs in this repo (~240 files carry the same
drift), so "prettier disagrees with the file" here does not mean "the author was
sloppy". Second, **checking formatting in a scratch directory gives a false
green**: an `.editorconfig` in the repo alters the resolved options, so a drifted
file copied elsewhere can pass. Verify with `--stdin-filepath` pointing at the
real repo path, or run the check from the repo root.

## F-11 (slice-8, close-out) — Gates at the slice-close HEAD, measured

Measured in `wt-slice-8` at `45efc1911` with a clean working tree. These supersede
every per-task figure quoted earlier in this note where they differ.

| Gate | Result |
| --- | --- |
| `node_modules/.bin/jest --runInBand` | **exit 0** — 92 passed + 1 skipped of 93 suites; 1334 passed + 12 skipped of 1346 tests; 10 snapshots; 39.4 s. The skipped suite is `GovernanceCliArgvSmoke.spec.ts`, which self-skips without `cardano-cli` on PATH. |
| `yarn compile` | **exit 0**, 18.4 s |
| `yarn lint` | **exit 0** — 0 errors, 5635 warnings (the pre-existing repo-wide baseline) |
| `yarn i18n:manage` | **exit 0**, byte-identical no-op; `git status` clean afterwards |
| `yarn stylelint` | **exit 2 — 111 errors** across 13 governance SCSS files, all `order/properties-alphabetical-order`. See F-1: 118 at `0cdcab581`, 7 removed with two dead selector blocks, **0 added**. Out of scope by decision; user-owned pre-merge cleanup. |
| `yarn storybook:build` | **exit 0**, 73.4 s, run from the checkout. Exits 1 with a manager-bundle `ModuleParseError` when run from a worktree whose `node_modules` is a symlink — an isolation artifact, not a repo failure. See F-2. Green, not waived. |
| `yarn check:all` | Red transitively, on the `prettier:check` and `stylelint` legs (F-9, F-10, F-1). Never run `yarn prettier` / `yarn prettier:check` to "fix" it. |
| `nix fmt` | **Not run — unavailable.** See F-9. |
