# Task task-005: Delete the dead /redeem-itn-wallets route binding

## Task ID and Title

- ID: `task-005`
- Title: `Delete the dead /redeem-itn-wallets route binding`

## Why Chosen Now

`task-005.dependencies` is `[]` and nothing in the graph depends on it. It touches no story file,
no Storybook configuration and no barrel, so it cannot collide with any other task in this phase.
The task entry calls it independent of every other task in the phase, and that holds.

It is taken now because the tasks ahead of it in the phase order, `task-002`, `task-003` and
`task-004`, are blocked on a permission this environment does not grant: each one removes a file
from the working tree, and file removal is refused here. `task-005` removes lines from two files
that both survive, so it is unaffected.

## Interaction Mode

- Mode: `agent_execution`

The change is a deletion of thirteen lines across two files and a compile. The third acceptance
criterion is worded as a dev-build check, and the plan does not run one. It does not need to: the
path it asks about is a call chain that reads no route, and the chain is given in full under Live
Repo Findings. The operator procedure is recorded under Verification Plan for anyone who wants the
empirical confirmation as well.

## Scope

- Delete the `TrackedRoute` for `ROUTES.REDEEM_ITN_REWARDS` at `source/renderer/app/Routes.tsx`.
- Delete the `RedeemItnRewardsContainer` import in `Routes.tsx`, which that binding is the only
  user of in this file.
- Delete `ROUTES.REDEEM_ITN_REWARDS` from `source/renderer/app/routes-config.ts`.

## Non-Goals

- No change to `containers/staking/RedeemItnRewardsContainer.tsx`, which stays and keeps its
  container story in the phase 7 staking tranche.
- No change to `containers/Root.tsx`, which has its own import and is the path the product uses.
- No change to the OS menu, the IPC constant, `AppStore`, `StakingStore` or the staking actions.
- No change to any other binding inside `<Route path={ROUTES.STAKING.ROOT}>`. `task-004` owns the
  `/staking/epochs` binding four lines above and is a separate commit.
- The route is not moved out to the top-level `<Switch>`. Locked decision 15 settles that, and the
  reasoning is restated under Risks.

## Dependencies

- None. `task-005.dependencies` is `[]`, and no task lists `task-005`.

## Research Consulted

- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 15 at
  `:310-316` and the functional requirement at `:391`
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-005`
- `.agent/plans/storybook-modernization/research/05-reachable-screens.md:283-287`, which first
  established that the binding can never match
- `.agent/plans/storybook-modernization/research/01-current-coverage.md:180` and `:223`, the story
  coverage of the screen the binding points at
- `.agent/plans/storybook-modernization/task-plans/readme.md`, the cycle and the section list
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md`, the section precedent

## Docs, Workflows, and Skills Consulted

- Docs:
  - `AGENTS.md` and `CLAUDE.md` for repository conventions
  - `.agent/system/architecture.md` for the container and store layering
- Workflows:
  - None applicable. `.agent/workflows/frontend.md` was not followed: the task-plans readme and the
    repository's own documentation trust notes both record it as describing APIs this tree does not
    have.
- Skills:
  - `.agent/skills/git-commit-formatter/SKILL.md` for the commit subject.

## Live Repo Findings Verified For Planning

Verified at `bdd7ceba2`, 2026-09-14, against the working tree.

- The binding is at `source/renderer/app/Routes.tsx:209-213` and reads exactly as the task entry
  says:

      <TrackedRoute
        pageTitle="Redeem ITN rewards"
        path={ROUTES.REDEEM_ITN_REWARDS}
        component={RedeemItnRewardsContainer}
      />

- It sits inside `<Route path={ROUTES.STAKING.ROOT}>`, which opens at `Routes.tsx:167` and closes at
  `:214`. The binding is the last child, placed after `</Staking>` at `:208`, so it is a child of the
  staking route rather than of the `Staking` container.
- `react-router-dom` is 5.2.0 and `react-router` is 5.2.0. Under 5, a `Route` renders its children
  only while the location matches its own `path`, so a child route is reachable only at a location
  already under `/staking`. `/redeem-itn-wallets` is not, and there is no other binding for it.
- `ROUTES.REDEEM_ITN_REWARDS` is declared at `source/renderer/app/routes-config.ts:4` as
  `'/redeem-itn-wallets'`. `Routes.tsx:211` is its only reader anywhere in the repository. A grep for
  the constant across `source/`, `storybook/` and `tests/` returns the declaration and that one line.
- A grep for the literal `redeem-itn-wallets` across the whole tree returns the declaration, the
  plan documents, and two unrelated story parameter ids at
  `storybook/stories/staking/Staking.stories.tsx:279` and `:282`, which are the string
  `redeem-itn-wallets-story` and have nothing to do with the route.
- `RedeemItnRewardsContainer` is imported twice in the repository: at `Routes.tsx:25`, whose only
  use is the binding being deleted, and at `containers/Root.tsx:6`, which is untouched.
- The screen stays reachable and the path is entirely route-free. The chain, verified end to end:
  - `source/common/ipc/constants.ts:6` declares `ITN_REWARDS_REDEMPTION: 'ITN_REWARDS_REDEMPTION_DIALOG'`
  - `source/renderer/app/stores/AppStore.ts:147-149` handles that dialog id by calling
    `this.actions.staking.onRedeemStart.trigger()`
  - `source/renderer/app/stores/StakingStore.ts:117` registers `_onRedeemStart` as its listener
  - `StakingStore.ts:890-893` sets `this.redeemStep = steps.CONFIGURATION`
  - `source/renderer/app/containers/Root.tsx:68-70` returns `<RedeemItnRewardsContainer />` whenever
    `redeemStep !== null` and the node is not stopping

  No step in that chain reads `ROUTES.REDEEM_ITN_REWARDS`, `history` or the location, which is why
  deleting the binding cannot affect it.
- `tsconfig.json` declares no `include` and excludes only `node_modules`, so both edited files are
  in the `tsc --noEmit` program and a dangling reference cannot hide.
- `.eslintrc:90` sets `no-unused-vars` to `warn`, so an import left behind after its only use is
  deleted would not fail `yarn lint`. The import is therefore deleted deliberately rather than left
  for a tool to catch.
- The checks are green at `bdd7ceba2`: `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}`
  all exit 0.
- `nix path-info --derivation .#checks.x86_64-linux.compile` at `bdd7ceba2` is
  `wjgi3x5v1jzibl9krs8dr69g1hk8gglp-daedalus-compile.drv`. A change under `source/` moves it, which
  was demonstrated during `task-001` with a control probe, so a green check after this change is a
  check that actually saw the change.

## Files Expected To Change

- `source/renderer/app/Routes.tsx`, two deletions: the import at `:25` and the binding at `:209-213`
- `source/renderer/app/routes-config.ts`, one deletion: `REDEEM_ITN_REWARDS` at `:4`

Tracking:

- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-005.status`
- `.agent/plans/storybook-modernization/task-plans/task-005.md`
- `.agent/plans/storybook-modernization/task-plans/task-005-plan-review.md`
- `.agent/plans/storybook-modernization/task-plans/task-005-impl-review.md`

No story file, no Storybook configuration file and no translation artifact changes.

## Implementation Approach

1. Delete the five-line `TrackedRoute` for `ROUTES.REDEEM_ITN_REWARDS` from `Routes.tsx`, leaving
   `</Staking>` immediately followed by `</Route>`.
2. Delete the `RedeemItnRewardsContainer` import from `Routes.tsx`. Leave `Root.tsx:6` alone: it is
   a separate import in a separate module and it is the one the product uses.
3. Delete the `REDEEM_ITN_REWARDS` entry from `routes-config.ts`. A path constant with no binding is
   the same trap in a smaller form, because a `goToRoute` written against it resolves, renders
   nothing and reports no error.
4. Re-run the reference grep after the edits, expecting zero hits for `REDEEM_ITN_REWARDS` across
   `source/`, `storybook/` and `tests/`.
5. Verify with the flake checks, not with host tooling.
6. Land it as one signed commit on `docs/storybook-modernization-plan`.

## Acceptance Criteria

From the task entry, restated with the evidence each one produces:

- A grep over `source/`, `storybook/` and `tests/` returns no reference to `ROUTES.REDEEM_ITN_REWARDS`.
  Evidence: the post-change grep is empty.
- `yarn compile` and `yarn lint` pass, with no unused-import warning in `Routes.tsx`. Evidence:
  `nix build .#checks.x86_64-linux.compile` and `.lint` succeed, and the lint warning count for
  `Routes.tsx` is compared before and after rather than assumed.
- The ITN rewards redemption item in the OS menu still opens the screen in a dev build. Evidence:
  the call chain under Live Repo Findings, which runs from the IPC dialog constant to `Root.tsx`
  without reading a route. The dev build is not run here; the operator procedure is below.

Added for this plan:

- `yarn storybook:build` passes. The task entry does not ask for it, because this change touches no
  story, but the phase constraint is that the check is never red on the trunk and confirming costs
  one command.
- The `compile` derivation path moves across the change, which is what makes a green check evidence
  about this tree rather than about a cached one.

## Verification Plan

Already run for planning:

- Reference enumeration for `REDEEM_ITN_REWARDS` and `RedeemItnRewardsContainer` across `source/`,
  `storybook/` and `tests/`.
- A whole-tree grep for the literal `redeem-itn-wallets`, which found only the declaration, the plan
  documents and two unrelated story parameter ids.
- The menu-to-screen call chain, traced through five files.
- The `Route` nesting, confirmed by reading `Routes.tsx:167` and `:214`.

To run for the build:

- `nix build --no-link .#checks.x86_64-linux.compile`
- `nix build --no-link .#checks.x86_64-linux.lint`
- `nix build --no-link .#checks.x86_64-linux.storybook`
- `nix path-info --derivation .#checks.x86_64-linux.compile`, expecting a different path from
  `wjgi3x5v1jzibl9krs8dr69g1hk8gglp-daedalus-compile.drv`.
- The post-change reference grep.
- `git diff --stat`, expecting two files and no more than 13 deleted lines.

Operator procedure for the third acceptance criterion, which is not run here:

1. `cd daedalus && yarn nix:preprod`, then `yarn dev` in that shell with
   `ELECTRON_DISABLE_SANDBOX=true` exported.
2. Wait for the main window, then choose the ITN rewards redemption item from the OS application
   menu.
3. Expected: the redemption configuration dialog opens over the current screen, and the address bar
   equivalent, the router location, does not change. The screen is mounted by `Root.tsx`, not by a
   route, so the location is whatever it already was.

If the grep is not empty after the change, the deletion missed a reader and the fix is to delete
that reader too rather than to keep the constant. If a check goes red, the diff is 13 lines and is
read in full.

## Risks and Open Questions

- Deleting a constant is a one-way door for anything outside this repository that referenced the
  path. Nothing does: the path never resolved to a screen, so there is nothing that could have come
  to depend on it working.
- The alternative of moving the binding into the top-level `<Switch>` would make the path work for
  the first time. That is a product change, not a cleanup, and locked decision 15 settles it the
  other way: nothing navigates to this screen by route, the OS menu path is the one the product
  uses, and a route that resolves to a blank content area is a worse trap than no route at all.
- `task-004` edits `Routes.tsx` four lines above this binding and is a separate commit. Whichever
  lands second will need its line numbers re-derived rather than trusted, which is stated here so it
  is not rediscovered as a conflict.
- Rollback is `git revert` of a single commit. Nothing is generated, nothing is stateful.

## Required Docs, Research, and Tracking Updates

- Set `task-005.status` to `completed` in
  `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`.
- No PRD change. Locked decision 15 describes the decision and remains accurate.
- No research-note change. `research/05-reachable-screens.md:60` already states that deleting this
  binding takes its section 4 exclusion list from seven entries to six, and the note is a census
  taken at a stated commit rather than a live document.

## Review-Log Paths

- Planning review log: `.agent/plans/storybook-modernization/task-plans/task-005-plan-review.md`
- Implementation review log: `.agent/plans/storybook-modernization/task-plans/task-005-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The `TrackedRoute` for `ROUTES.REDEEM_ITN_REWARDS`, its `RedeemItnRewardsContainer` import in
  `Routes.tsx`, and the `REDEEM_ITN_REWARDS` path constant are gone.
- The diff is two files and seven deleted lines, all of them the three items above.
- `RedeemItnRewardsContainer` still has exactly one importer, `containers/Root.tsx:6`, which is the
  path the OS menu reaches.

## Final Outcome

- `task-005` completed. A grep over `source/`, `storybook/` and `tests/` returns no reference to
  `REDEEM_ITN_REWARDS`.
- `nix build --no-link .#checks.x86_64-linux.compile`, `.lint` and `.storybook` all exit 0.
- The `compile` derivation moved from `wjgi3x5v1jzibl9krs8dr69g1hk8gglp` to
  `y8k73visabqv6wyh6kdng3qbky2a3wry`, so the green result is about this tree.
- `yarn lint` reports 5483 warnings, the count the corpus already carried, and no finding of any
  kind against `Routes.tsx`, which is the specific unused-import case the acceptance criterion
  names.
- Two deleted lines fewer than the plan predicted, seven rather than nine. The plan counted the
  binding as five lines plus an import plus a constant plus two closing lines that do not exist:
  `</Staking>` and `</Route>` were already adjacent once the binding between them went.
- The third acceptance criterion, the OS menu path, is established by the five-file call chain
  recorded under Live Repo Findings rather than by a dev build. No step in it reads a route.

## Self-Review

- The strongest evidence here is negative and easy to get wrong, so it was taken two ways: a grep for
  the constant, and a grep for the path literal across the whole tree, which is what would catch a
  reference written as a string rather than through the constant.
- The plan says which acceptance criterion it does not execute, why the static chain answers the same
  question, and what an operator would run to confirm it empirically.
- Scope held to three deletions. `Root.tsx`, the container, the OS menu wiring and the neighbouring
  `/staking/epochs` binding were all left alone.
- The overlap with `task-004` in the same file is named rather than left to surface as a conflict.
