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

## F-2 (slice-8) — Two gate premises carried in a preceding slice's tracker text are wrong at this HEAD; the correction lives here, not in that closed entry

The slice-8 planning pass re-measured the gates at HEAD `0cdcab581` with a clean
tree: `yarn storybook:build` exits **0** (~84 s, output to `dist/storybook`) and
`yarn compile` exits **0** (~26 s, its `precompile` hook regenerating the
gitignored `*.scss.d.ts`). A preceding slice's tracker `statusReason` asserts
that `storybook:build` is red for a manager-webpack JSX-loader reason and that
`yarn compile` needs a `typed-scss-modules` + `tsc --noEmit` workaround; neither
holds here. Consequences: slice-8 runs both gates rather than waiving them, and
the `typed-scss-modules` fallback is unnecessary.

**The closed tracker entry was deliberately not edited.** A closed slice's
`statusReason` is the record of what that slice measured at its own HEAD;
rewriting it retroactively would destroy the audit trail. Corrections to a closed
measurement belong in the later slice's findings note.

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
