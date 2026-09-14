Planner: Iteration 1
Timestamp: 2026-09-14T19:26:40Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-005.md` with the 21 sections the
  task-plans readme requires.
- Scope held to three deletions: the `TrackedRoute`, the import it is the only user of, and the path
  constant it is the only reader of.
- Classified the task `agent_execution`, and recorded that the third acceptance criterion is a
  dev-build check the plan does not run, with the reason it does not need to.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 15 at
  `:310-316` and the functional requirement at `:391`.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-005`.
- `.agent/plans/storybook-modernization/research/05-reachable-screens.md:283-287` and `:60`, and
  `research/01-current-coverage.md:180` and `:223`.
- `.agent/plans/storybook-modernization/task-plans/readme.md` for the cycle and section list, and
  `.agent/plans/mithril-partial-sync/task-plans/task-001.md` for the section precedent.
- `.agent/system/architecture.md` for the container and store layering.
- `.agent/workflows/frontend.md` deliberately not followed, per the repository's own record that it
  describes APIs this tree does not have.

Repo-Verified Findings Used To Shape The Plan:
- Confirmed the binding at `Routes.tsx:209-213` sits inside `<Route path={ROUTES.STAKING.ROOT}>`,
  which opens at `:167` and closes at `:214`, and that `react-router-dom` is 5.2.0, under which a
  child route is only evaluated while the location already matches the parent path.
- Confirmed `Routes.tsx:211` is the only reader of `ROUTES.REDEEM_ITN_REWARDS` anywhere in the
  repository, and that a whole-tree grep for the literal `redeem-itn-wallets` finds only the
  declaration, the plan documents and two unrelated story parameter ids.
- Confirmed `RedeemItnRewardsContainer` has a second, independent importer at `Root.tsx:6`.
- Traced the OS menu path end to end through five files, from `common/ipc/constants.ts:6` to
  `Root.tsx:68-70`, and established that no step in it reads a route or the location.
- Confirmed `.eslintrc:90` sets `no-unused-vars` to `warn`, so a left-behind import would not fail
  the lint check and has to be deleted deliberately.
- Recorded the `compile` derivation path at `bdd7ceba2` so a green check after the change can be
  shown to be about the changed tree.

Planned Approach:
- Delete the binding, the import and the constant.
- Re-run the reference grep.
- Verify with the flake checks.

Scope Guard / Self-Review:
- No change to the container, to `Root.tsx`, to the OS menu wiring or to the neighbouring
  `/staking/epochs` binding.
- The route is not moved into the top-level `<Switch>`, which locked decision 15 settles.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-14T19:30:15Z

Blocking gaps:
- The plan asserts the OS menu path is unaffected and cites `Root.tsx:68-70`, which is where the
  task entry stops. That is the last link of the chain, not the chain. Between the menu item and
  `redeemStep` becoming non-null there are three more files, and the claim the criterion actually
  makes is that none of them reads a route. Naming the endpoint and calling it proof is exactly the
  kind of restatement of the task entry the task-plans readme says a plan must not do.
- The acceptance criterion about the unused import is stated as "yarn lint passes", which it would
  do whether or not the import were deleted: `no-unused-vars` is a warning in this repository. The
  plan has to say what it will actually compare, which is the finding count against `Routes.tsx`,
  or the criterion is unfalsifiable.

Non-blocking observations:
- The plan should say that `task-004` edits the same file a few lines above, so whichever lands
  second re-derives its line numbers. Both are in this phase and both are single commits.
- Nothing states why the task is being taken out of the phase order. It is, and the reason it can be
  is that it shares no file with the three tasks ahead of it. A reader six months from now will want
  that on the record rather than inferring it from the commit dates.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-14T19:35:50Z

Plan Updates In Response To Critique:
- Replaced the single `Root.tsx` citation with the full five-file call chain, from the IPC dialog
  constant through `AppStore`, the staking action, `StakingStore._onRedeemStart` and `redeemStep`,
  to `Root.tsx:68-70`, and stated the property that matters: no step in it reads a route.
- Added the `.eslintrc:90` finding and changed the second acceptance criterion's evidence from
  "lint passes" to a comparison of the lint findings against `Routes.tsx`.
- Added a Risks entry recording the `task-004` overlap in the same file.
- Added to Why Chosen Now the reason the task is taken ahead of its phase position.

Resulting Approved Plan Shape:
- Three deletions across two files.
- Verification by reference grep, by the three flake checks, and by a derivation-path comparison
  that establishes the checks saw the change.
- One tracking update, the task status.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing to the change itself.
- The verification of record is the flake check.
- Scope is unchanged: the binding, its import, its constant.

Outcome: Canonical task plan revised after critique and approved for build execution
