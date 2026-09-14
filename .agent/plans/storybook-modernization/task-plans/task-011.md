# Task task-011: Make the sidebar grouping and ordering explicit

## Task ID and Title

- ID: `task-011`
- Title: `Make the sidebar grouping and ordering explicit`

## Why Chosen Now

`task-011.dependencies` is `[task-010]`, which is complete. Until the barrel went there was an
implicit order and no need to state it; now there is neither, so this is the task that stops the
tree reshuffling. `task-012` depends on it.

## Interaction Mode

- Mode: `agent_execution`

The change is a `parameters` export in `storybook/preview.tsx` and the check is
`nix build .#checks.x86_64-linux.storybook`. The resulting order is verified by running the
installed comparator over the extracted corpus, for the reason given under Verification Plan.

## Scope

- Add a `parameters.options.storySort.order` array to `storybook/preview.tsx` that reproduces the
  group and panel order the barrel produced.

## Non-Goals

- No change to any `storiesOf` title. The order is expressed in `preview.tsx`, not by renaming
  panels.
- No change to `storybook/main.ts`. It is in the task's `targetPaths` and needs no edit: the glob it
  declares controls what is indexed, not what order the sidebar renders in.
- No `includeNames` and no story-level order array. See the recorded difference below.
- No CSF conversion and no registration change.

## Dependencies

- `task-010`, complete.
- `task-012` depends on this task.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the settled-by-evidence
  note that sidebar ordering is currently implicit in barrel import order and becomes explicit when
  the barrel goes, at `:355-359`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-011` and
  `task-012`
- `.agent/plans/storybook-modernization/task-plans/task-001.md` and its baseline artifact
- `.agent/plans/storybook-modernization/task-plans/task-010.md`, for what the glob changed
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
- Workflows:
  - `.agent/workflows/storybook.md`, which documents the barrel model and is rewritten by
    `task-060`.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `68ff39c4c`, 2026-09-15, against the working tree.

- **The `task-001` baseline does not record an order, so the first acceptance criterion cannot be met
  as worded.** That was deliberate and is written down in `task-001.md`: order at 6.4.22 was implicit
  in barrel import order, the settled decision was to replace the barrel, so a captured order would
  have encoded the thing the plan intended to change. The baseline sorts alphabetically instead. The
  criterion's second limb applies: the order this task establishes is stated here and is a
  deliberate choice.
- The order the barrel produced is recoverable and was recovered. At 6.4.22 `storiesOf` registers as
  a side effect of module evaluation, and the barrel evaluated its imports depth-first in source
  order, so a depth-first walk of the barrel's import graph at `5311ce0d0`, the last commit before
  `task-010`, reproduces the sequence users saw. It gives 14 groups and 49 titles, which is exactly
  the count the corpus carries now, so no title is missing from the reconstruction.
- The group order it produced: `Nodes`, `Loading`, `Wallets`, `Decentralization`, `dApps`, `Voting`,
  `Governance`, `Settings`, `Assets`, `News`, `Navigation`, `Common`, `Discreet Mode`, `Analytics`.
- Without a `storySort` parameter the order is not arbitrary but it is not this either.
  `@storybook/client-api/dist/cjs/StoryStoreFacade.js:128-148` reads
  `projectAnnotations.parameters.options.storySort` and passes it to `sortStoriesV6` together with
  `fileNameOrder`, which is `Object.keys(this.csfExports)`. With no parameter, `sortStoriesCommon`
  at `@storybook/store/dist/cjs/sortStories.js:28-35` falls through to that file order, which under
  the glob is `require.context` order. So the tree currently renders in path order and this task is
  what replaces that with a stated one.
- `storySort` splits titles on `/\s*\/\s*/`
  (`@storybook/store/dist/cjs/storySort.js:16`), so the corpus's `' / '` separators tokenise cleanly
  and the `order` array is written with trimmed segment names.
- Nested ordering is supported: at each depth the comparator looks up the current segment in `order`
  and, if the following element is an array, descends into it
  (`storySort.js`, the `order.indexOf(nameA)` block). Three-level titles therefore need one nested
  level, which affects `Loading / Mithril / *` only. `Nodes / Diagnostic` and
  `Nodes / Diagnostic / Mithril Partial Sync Confirmation` need no nesting, because the comparator
  returns `-1` for the shorter title when one side runs out of segments.
- **Story order inside a panel is not controlled by this task and cannot be without a 258-entry
  list.** `storySort.js:22-24` returns `0` when two stories share a title unless `includeNames` is
  set, and `sortStoriesV6` then breaks the tie on `fileNameOrder`. Seven of the 49 panels are
  assembled from more than one file and hold 45 of the 258 registrations between them:
  `Wallets / Settings` (7 files), `Wallets / Add Wallet` (5), `Wallets / Transactions` (4),
  `News / Overlays` (3), `Wallets / Tokens` (3), `Navigation / Sidebar` (2) and
  `Wallets / Summary` (2). In those seven, story order moves from barrel order to file path order.
  The other 42 panels come from a single file each and keep their in-file definition order, which
  nothing in this phase touches.
- `storybook/preview.tsx` currently exports only `decorators`. It has no `parameters` export, so
  this is an addition rather than a merge.
- The sidebar carries 258 registrations across 49 titles in 14 groups, `UNREACHABLE 0`. Membership
  must not move; only order is in question.
- The checks are green at `68ff39c4c`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` all exit 0.

## Files Expected To Change

- `storybook/preview.tsx`, one added `parameters` export

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-011.status` and
  the corrections its prose carries
- `.agent/plans/storybook-modernization/task-plans/task-011.md`
- `.agent/plans/storybook-modernization/task-plans/task-011-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-011-impl-review.md`

No file is removed, so this task needs no deletion hand-off.

## Implementation Approach

1. Generate the nested `order` array from the reconstructed barrel order rather than transcribing
   it. A 49-entry nested literal typed by hand is a transcription error waiting to happen, and the
   reconstruction is already machine-readable.
2. Add it to `storybook/preview.tsx` as `export const parameters = { options: { storySort: { order:
   [...] } } };`, with a comment recording that the order is the one the barrel produced and where
   it came from.
3. Verify the resulting sequence by running the installed comparator. `storySort` is a pure function
   exported from `@storybook/store`; feeding it the 258 extracted registrations and the new `order`
   array reproduces exactly the sort the preview will perform on the group and panel levels. This is
   the only way to check the outcome without a browser, and it uses the real implementation rather
   than a model of it.
4. Confirm membership is unchanged against the `task-001` baseline.
5. Run `storybook`, and `compile` and `lint` since `preview.tsx` is TypeScript in the lint and
   compile programs.
6. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- The sidebar order matches the `task-001` baseline, or every difference is deliberate and recorded.
  The baseline records no order, so the second limb applies. Evidence: the order is the one
  reconstructed from the barrel at `5311ce0d0`, it is stated in full in `preview.tsx`, and the one
  difference from the barrel era, story order within seven multi-file panels, is recorded above with
  its cause and its size.
- `yarn storybook:build` passes. Evidence: `nix build .#checks.x86_64-linux.storybook` succeeds.

Added for this plan:

- Running the installed `storySort` comparator over the corpus with the new `order` array yields the
  49 titles in exactly the barrel-era sequence.
- Sidebar membership is unchanged: 258 registrations, 49 titles, 14 groups.

The task entry says "15 top-level groups and 53 panel titles have to come out in the order the
baseline recorded". Both figures predate phase 1: the corpus now has 14 groups and 49 titles, after
`task-002` removed three panels, `task-003` removed a root-level group, and `task-006` added a
registration to an existing panel. The entry is corrected.

## Verification Plan

Already run for planning:

- The barrel order reconstructed from `5311ce0d0` by depth-first evaluation of the import graph: 14
  groups, 49 titles.
- `storySort.js` read in full: the separator regexp, the nested-array descent, the same-title
  shortcut and the wildcard handling.
- `StoryStoreFacade.js:128-148` read, confirming the parameter path and the `fileNameOrder`
  fallback.
- The seven multi-file panels enumerated with their file counts and their 45 registrations.

To run for the build:

- The installed comparator applied to the extracted corpus with the new `order`, expecting the
  49 titles in barrel-era sequence.
- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, expecting
  membership unchanged.
- `nix build --no-link .#checks.x86_64-linux.{storybook,compile,lint}`.
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a moved path.

If the comparator run produces a different sequence, the `order` array is wrong and the array is
fixed rather than the expectation adjusted, because the target sequence is a recovered fact rather
than a preference.

## Risks and Open Questions

- The reconstruction is a static model of module evaluation, not a recording of the running
  workbench. It is faithful for this corpus because every registration is a top-level side effect of
  an ES module import and there is no conditional or dynamic import anywhere in the graph, both of
  which were checked during `task-062`. If it were wrong, the symptom would be a panel appearing in
  an unexpected place, which is cosmetic and correctable in one line.
- Story order inside the seven multi-file panels changes and is not restored. Restoring it needs
  `includeNames: true` and an order array naming all 258 stories, which would have to be
  regenerated by hand every time a story is added and would defeat the readability the task entry
  asks for. The new order is file path order, which is deterministic and arguably more predictable
  than the barrel's. It is recorded rather than hidden.
- Freezing the group order in `preview.tsx` means a new domain added later appears last unless
  someone edits the array. That is the cost of an explicit order and is what the task asks for. A
  `'*'` wildcard entry is available if a future task wants new groups to land somewhere specific.
- Rollback is `git revert` of a single commit, which returns the tree to file path order.

## Required Docs, Research, and Tracking Updates

- Set `task-011.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Correct the entry's "15 top-level groups and 53 panel titles" to 14 and 49, and record that the
  `task-001` baseline holds no order, so the criterion's second limb is the one that applies.
- Record in the entry which order was chosen, where it was recovered from, and that story order
  inside seven multi-file panels moves to file path order.
- No PRD change.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-011-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-011-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- `storybook/preview.tsx` gains a `parameters` export carrying a 27-entry nested
  `options.storySort.order` array, generated from the reconstruction rather than transcribed.
- No other file changed. No title was renamed and no registration moved.

## Final Outcome

- `task-011` completed. `task-012` is unblocked.
- The order is verified against the shipped implementation. `storySort` from the installed
  `@storybook/store` was run over the 258 extracted registrations with the order array read
  directly out of `preview.tsx`, and the 49 titles came out in exactly the sequence the barrel
  produced at `5311ce0d0`. That is what decides the question; `storybook:build` only proves the
  parameter parses.
- Sidebar membership is byte-identical to the previous capture: 258 registrations, 49 titles, 14
  groups, `UNREACHABLE 0`.
- `nix build --no-link .#checks.x86_64-linux.storybook`, `.compile` and `.lint` all exit 0, on a
  `compile` derivation that moved to `lkq92rv2p1ac5hwh353sdv7dl3hynkp2-daedalus-compile.drv`.
- `prettier --check` on `preview.tsx` is clean under the repository's own prettier.
- One difference from the barrel era is accepted: story order inside the seven multi-file panels is
  now file path order, affecting 45 of 258 registrations. It is recorded in the task entry with the
  reason it is not pinned.

## Self-Review

- The first acceptance criterion could not be met as worded, because the `task-001` baseline holds
  no order. The plan said so rather than quietly diffing against an alphabetical sort and declaring
  a match, and took the criterion's second limb instead.
- The order was recovered from the barrel rather than chosen. Inventing an order here would have
  been easy to defend and would still have reshuffled a sidebar people know.
- Verification used the real comparator rather than a reimplementation of it. A model of
  `storySort` would have had to get the `/\s*\/\s*/` separator, the nested-array descent and the
  shorter-title rule all right, and any one of those wrong would have produced a confident wrong
  answer.
- The limitation on story order inside multi-file panels is quantified, at seven panels and 45
  registrations, rather than mentioned. The number is what tells a reader whether to care.
