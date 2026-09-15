# Task task-063: Replace withState with a local wrapper and remove @dump247/storybook-state

## Task ID and Title

- ID: `task-063`
- Title: `Replace withState with a local wrapper and remove @dump247/storybook-state`

## Why Chosen Now

`task-063.dependencies` is `[task-010]`, which is complete. It is the last task of phase 2 and it
blocks phase 3 outright: the hop cannot land while this dependency is installed.

The reason is mechanical rather than stylistic. `@dump247/storybook-state` imports
`@storybook/addons` at `dist/index.js:11` and calls `addons.getChannel()` at `:157` inside
`withState` itself, so every one of the 17 call sites touches that channel at module evaluation.
Phase 3 removes `@storybook/addons` and no version of it exists in the Storybook 8 line, so the
corpus cannot cross the hop carrying this.

## Interaction Mode

- Mode: `agent_execution`

Every edit and every check reproduces here. The one file to remove goes through the operator. The
second acceptance criterion is about interactive behaviour and is answered as far as this
environment allows, with the limit stated rather than glossed.

## Scope

- Add a local stateful wrapper under `storybook/stories/_support/` exposing the same `withState`
  signature and the same `Store<T>` type the package exposes, with no Storybook API surface.
- Repoint the 17 call sites and the one type import at it.
- Remove `@dump247/storybook-state` from `package.json` and its block from `yarn.lock`.

## Non-Goals

- No conversion to `useArgs`. Locked decision 9 makes that phase 4's work, when these sites become
  args-backed. The wrapper is deleted then.
- No change to the shape of any call site. The wrapper takes the same arguments in the same order
  and hands back the same store, so the diff at each site is the import specifier and nothing else.
- No change to `storybook/main.ts`, `preview.tsx` or any registration.
- No `@storybook/addons` change. Phase 3 removes it.

## Dependencies

- `task-010`, complete.
- Phase 3 depends on this task.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 9 at
  `:253-258` and the 2026-09-14 Status Log entry extending it to cover this package
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-063`
- `.agent/plans/storybook-modernization/research/02-storybook-upgrade-path.md`, for why the 8.6.x
  stopover is the route
- `.agent/plans/storybook-modernization/task-plans/task-007.md`, for the lockfile gating precedent
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions, including that Jest specs are colocated
    as `<Unit>.spec.ts`
- Workflows:
  - `.agent/workflows/storybook.md`, which teaches `@dump247/storybook-state` and is made wrong by
    this task. `task-060` rewrites it at the end of the epic; the divergence is recorded here.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `642f5b9e1`, 2026-09-15, against the working tree.

- The package's `withState` is exactly as the task entry describes.
  `@dump247/storybook-state/dist/index.js:11` is `import addons from '@storybook/addons';` and the
  `withState` body calls `var channel = addons.getChannel();` before branching on the signature, so
  the channel is acquired whether or not anything listens.
- There are 17 `withState(` call sites across 9 files, and **every one uses the legacy two-argument
  form** `withState(initialState, (store) => ...)`. None uses the curried single-argument form the
  package also supports. That is what makes an import-only change possible.
- The store surface the corpus actually uses is two members. `store.state` appears 44 times and
  `store.set(` 30 times. `store.reset()` appears **zero** times. The wrapper implements `reset`
  anyway, because the type declares it and a call site could be added.
- `Store` is also imported as a type, at `storybook/stories/settings/utils/helpers.tsx:1`, where
  `onLocaleValueChange` takes a `Store<LocaleStoryStore>`. The task's `targetPaths` names only
  `General.stories.tsx` and the new wrapper; the real edit set is 10 files.
- The package's typings at `index.d.ts` define `Store<T>` as `{ state: T; set(next: Partial<T>):
  void; reset(): void }`. The wrapper reproduces that type so `helpers.tsx` needs only its import
  path changed.
- **The shape of the returned value matters and is easy to get wrong.** For the legacy signature the
  package returns `function () { return React.createElement(StoryState, { store, storyFn, channel });
  }`, a plain function whose result is an element. Storybook calls the story function directly, so
  any hook the replacement uses has to live inside a component React renders, not inside the
  function Storybook calls. A wrapper that returned a hook-using function component directly would
  have its hooks invoked outside a render and break at the first story.
- The package's store semantics, from `dist/index.js`: `initialState` is frozen on construction,
  `set` merges and refreezes with `Object.freeze({ ...this.state, ...state })`, `reset` restores the
  initial object and only notifies if it changed, and subscribers are notified on both. The store is
  created once per `withState()` call, at module evaluation, so its state survives a story remount.
  The wrapper reproduces all of that, including the freeze, because a call site that mutates
  `store.state` directly would throw against the package and silently work against a naive
  replacement.
- `package.json:79` declares `"@dump247/storybook-state": "1.6.1"`. `yarn.lock:1110` holds its
  block.
- `jest.config.js:129` sets `roots: ['<rootDir>/tests', '<rootDir>/source']`. **`storybook/` is not
  a Jest root**, so a colocated spec beside the wrapper would not run, and there are zero specs
  under `storybook/` today. Making the wrapper's behaviour a permanent CI guarantee would mean
  adding a root, which is a change to what CI runs and is not in this task.
- The `jest` flake check exists at `perSystem/checks.nix:47` for every system except
  `x86_64-darwin`, so it can be run here if a spec were ever in scope.
- The checks are green at `642f5b9e1`:
  `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` all exit 0, and `compile` is
  `lkq92rv2p1ac5hwh353sdv7dl3hynkp2-daedalus-compile.drv`.

## Files Expected To Change

Added, one file:

- `storybook/stories/_support/WithLocalState.tsx`

Edited, 11 files:

- the nine story files holding the 17 call sites: `common/ItemsDropdown.stories.tsx`,
  `dapps/TransactionRequest.stories.tsx`, `governance/DRepDirectory.stories.tsx`,
  `governance/Delegation.stories.tsx`, `notifications/Notifications.stories.tsx`,
  `settings/general/General.stories.tsx`, `settings/language/Language.stories.tsx`,
  `wallets/tokens/WalletTokens.stories.tsx`, `wallets/tokens/WalletTokensList.stories.tsx`
- `storybook/stories/settings/utils/helpers.tsx`, the `Store` type import
- `package.json`

Also edited:

- `yarn.lock`, the `@dump247/storybook-state@1.6.1` block

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-063.status` and
  the corrections its prose carries
- the three `task-063` plan documents

No file is removed, so this task needs no deletion hand-off.

## Implementation Approach

1. Write `WithLocalState.tsx` to reproduce the package's observable behaviour rather than an
   approximation of it: a store created once per `withState()` call, frozen state, merging `set`,
   `reset` that only notifies on a real change, and a subscription so the rendered component updates.
   Return a plain function that renders an internal component, matching the package's shape, so the
   hooks are inside something React renders.
2. Repoint the nine story files and `helpers.tsx`. The change at each is the import specifier alone,
   which is the property that makes the behaviour argument below hold.
3. Remove the dependency from `package.json` and its single block from `yarn.lock`, leaving any
   transitively orphaned tail as `task-007` did.
4. Gate the manifest and lockfile edits on `nix build .#internal.x86_64-linux.node_modules`, which
   installs with `--frozen-lockfile` and derives its offline cache from `yarn.lock`. That build is
   what decides whether the two agree.
5. Exercise the wrapper's state machinery directly, under React, with the CI `node_modules`: mount
   it, read the initial render, call `set` and confirm the render changes and the state merged, call
   `reset` and confirm it returns, and confirm the state object is frozen. This is the part of the
   second acceptance criterion that can be answered here.
6. Verify the diff at each of the nine story files is the import line only, which is what carries
   the argument from "the wrapper behaves like the package" to "the 17 sites behave as before".
7. Run `compile`, `lint` and `storybook`.
8. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- `grep -rn '@dump247/storybook-state' source storybook package.json` returns nothing. Evidence: the
  post-change grep, extended to `yarn.lock` as well.
- All 17 former `withState` call sites render and their interactive state still changes when driven.
  This cannot be answered in full here and the plan says so rather than implying otherwise. There is
  no display, the e2e suite cannot execute, and `storybook:build` bundles without evaluating preview
  modules, so no automated check in this repository renders a story. What is established instead is
  two things that together carry it: the wrapper's state machinery is exercised directly under React
  and behaves as the package's does, and the diff at every one of the nine call-site files is the
  import specifier alone, so no site's arguments, store usage or render function changed. A site that
  worked before works now unless the wrapper differs from the package, and the wrapper was written
  against the package's source and tested against its semantics.
- `yarn compile`, `yarn lint` and `yarn storybook:build` all pass. Evidence:
  `nix build .#checks.x86_64-linux.{compile,lint,storybook}` on a derivation that moved.

Added for this plan:

- `.#internal.x86_64-linux.node_modules` builds, which is what proves the manifest and lockfile
  edits agree under `--frozen-lockfile`.
- Sidebar membership is unchanged at 258 registrations. This task touches no registration.

## Verification Plan

Already run for planning:

- The package's `dist/index.js` read for the `@storybook/addons` import, the `getChannel()` call and
  the full `Store` implementation including the freezes and the notify-on-change condition.
- Its `index.d.ts` read for the `Store<T>` type the corpus depends on.
- All 17 call sites classified: every one legacy two-argument.
- Store member usage counted: `state` 44, `set` 30, `reset` 0.
- The `Store` type import at `helpers.tsx:1` found, which the task's `targetPaths` omits.
- `jest.config.js:129` read, establishing that `storybook/` is not a Jest root.

To run for the build:

- `nix build --no-link .#internal.x86_64-linux.node_modules`, before anything else.
- The wrapper driven under React with the CI `node_modules`: initial state, `set` merge and
  re-render, `reset`, and frozen state.
- `git diff` restricted to the nine story files, expecting one changed line each.
- `grep -rn '@dump247' source storybook package.json yarn.lock`, expecting nothing.
- `node .agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js .`, expecting
  258.
- `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}`.

If the `node_modules` build fails, the manifest and lockfile disagree and the yarn error is read
rather than the edit reverted blindly. If a story file's diff is more than the import line, the
wrapper's signature does not match the package's and the wrapper is fixed rather than the call site.

## Risks and Open Questions

- The second acceptance criterion is not fully satisfiable in this environment and the plan says so
  in those words. The residual risk is that a call site depends on some behaviour of the package the
  wrapper does not reproduce. That risk is bounded by reading the package's implementation rather
  than its documentation, by reproducing the freeze and the notify-on-change condition rather than
  only the happy path, and by the diff at each site being one line.
- The wrapper's behaviour is not guarded by any check. Making it so would mean adding `storybook/`
  to `jest.config.js` `roots`, which changes what CI runs and belongs to whoever decides that rather
  than to this task. It is recorded as an open decision rather than taken quietly. The wrapper is
  deleted in phase 4, so the window is bounded.
- Reproducing the module-level store means state survives a remount, including a theme or locale
  switch, exactly as it does today. A `useState` wrapper would have reset instead. The more
  conservative choice was taken because "behaves identically either side of the hop" is what the
  task asks for, and a behaviour change here would surface as a story mysteriously forgetting its
  state.
- `.agent/workflows/storybook.md` teaches this package and becomes wrong with this commit. It is
  already listed for rewrite by `task-060` and the divergence is recorded rather than fixed here.
- Rollback is `git revert` of a single commit; the manifest and lockfile revert together.

## Required Docs, Research, and Tracking Updates

- Set `task-063.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- Record in the entry that the edit set is 10 source files rather than the two its `targetPaths`
  names, and that `helpers.tsx` imports `Store` as a type.
- Record in the entry what was and was not established about interactive behaviour, and that a
  permanent guard would need `storybook/` added to the Jest roots.
- No PRD change. Locked decision 9 and the Status Log entry extending it remain accurate.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-063-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-063-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- `storybook/stories/_support/WithLocalState.tsx` provides `withState` and the `Store<T>` type with
  no Storybook API surface.
- Ten source files repointed at it. The diff at each of the nine story files is one line.
- `@dump247/storybook-state` is out of `package.json` and out of `yarn.lock`.

## Final Outcome

- `task-063` completed, and with it phase 2. Phase 3 is unblocked: nothing left in the corpus
  imports `@storybook/addons`, directly or through this package.
- `nix build --no-link .#internal.x86_64-linux.node_modules` rebuilt in 3 minutes 15 seconds to
  `jxnvn5rb5cwxam9840zx2p5dkgqny81d`, which is what establishes that the manifest and the lockfile
  agree under `--frozen-lockfile`. As in `task-007`, only the package's own block was removed and no
  transitively orphaned tail was hand-pruned.
- The wrapper's semantics were verified by driving it under React with jsdom: nine assertions, all
  passing, covering the initial render, the freeze, a rejected direct mutation, `set` re-rendering
  and merging, `reset` restoring, state surviving a remount, per-call store isolation, and one store
  not disturbing another.
- `nix build --no-link .#checks.x86_64-linux.compile`, `.storybook` and `.lint` all exit 0, on a
  `compile` derivation that moved to `57ar6a9ayk739qnym9mrwih0p3qskfrm-daedalus-compile.drv`. Lint
  is at 5397 warnings against 5391, the six new ones being the wrapper's own.
- Sidebar membership is unchanged at 258 registrations across 49 titles in 14 groups. This task
  touches no registration.

## Self-Review

- The second acceptance criterion was answered as far as this environment allows and the limit was
  stated rather than covered by three green checks. Nothing here renders a story, so "the 17 sites
  still change state when driven" was replaced by two things that together carry it: the wrapper's
  machinery driven directly, and a one-line diff at every call site.
- Reading the package's implementation rather than its README is what made an import-only change
  possible. The signature, the freeze, the merge, the notify-on-change condition and the module-level
  store lifetime were all reproduced deliberately; a wrapper built on `useState` inside the returned
  component would have compiled, passed every check, and silently reset story state on each remount.
- The shape constraint was identified in planning rather than hit in implementation. Returning a
  hook-using component where the package returns a function rendering a component breaks at the
  first story and reads as a Storybook fault.
- The acceptance grep does not come back empty and the entry says so, rather than the comment being
  reworded to slip past it. One hit, a comment naming what the shim replaced.
- Two of the three files needing an import reorder were already prettier-dirty under the
  repository's own prettier, at 6 and 18 lines. Formatting them would have put unrelated reformats
  of a newer prettier's output into the diff, so the dirtiness was measured before and after instead
  and confirmed unchanged.
