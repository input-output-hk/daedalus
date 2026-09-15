Planner: Iteration 1
Timestamp: 2026-09-15T00:48:55Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-007.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the re-verified orphans, the zero-byte preview head, and the unused dependency in
  the manifest and the lockfile.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, the settled-by-evidence
  cleanups at `:360-366` and the webpack 5 builder decision.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-007`.
- `.agent/plans/storybook-modernization/research/01-current-coverage.md`.
- `.agent/plans/storybook-modernization/task-plans/task-002.md`, `task-003.md`, `task-004.md`,
  `task-plans/readme.md`, and `.agent/plans/mithril-partial-sync/task-plans/task-001.md`.

Repo-Verified Findings Used To Shape The Plan:
- Re-verified orphan status by walking the module graph from all four workbench entry points rather
  than by grepping: 119 files under `storybook/`, 116 reachable, 3 orphaned, matching the task entry
  exactly and finding no additional orphan created by `task-002`, `task-003` or `task-004`.
- Confirmed no cascade: the three import only corpus-wide `_support` modules and `source/`
  components.
- Confirmed five of the seven modules in `storybook/stories/wallets/_utils/` are reachable and stay,
  including `HardwareWalletsWrapper.tsx`, whose name is three characters from one that goes.
- Identified the `defaultWalletProps` grep hits as a local const in an unrelated spec file.
- Confirmed `storybook-addon-swc` is referenced nowhere and that the SWC rule Storybook uses is
  handwritten in `main.ts:68-95` with the four legacy-decorator settings.
- Traced the lockfile block and found two of its four dependency ranges have it as their only
  requester, both distinct from the pinned direct dependencies the repository uses.
- Established that `--frozen-lockfile` makes the `node_modules` derivation the arbiter of whether
  the manifest and lockfile agree.

Planned Approach:
- Edit `package.json` and the one `yarn.lock` block, build `node_modules` as the gate, hand off four
  paths, re-run the orphan walk, require an unchanged sidebar, then run three flake checks.

Scope Guard / Self-Review:
- No change to `storybook/main.ts`, to `swc-loader` or `@swc/core` in the manifest, or to any
  reachable `_utils` module.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T00:53:10Z

Blocking gaps:
- The plan edits a lockfile by hand and does not say how it knows the edit is right. Reading a
  lockfile diff tells you almost nothing; the property that matters is whether yarn still resolves
  the tree under `--frozen-lockfile`, and that is only observable by building the derivation that
  does the install. The plan has to name that build as a gate that runs before any check, not as one
  more item in a list.
- The plan removes one lockfile block and leaves two entries with no requester, without saying that
  is what it is doing. A reviewer who notices `swc-loader@^0.1.15` still present after the only
  package that wanted it has gone will read it as an incomplete edit. Either prune them or state the
  choice and the reason.

Non-blocking observations:
- This task's real work is the re-verification, and the plan should say what a null result means.
  Finding exactly the three the entry predicted is the expected outcome and is still worth
  recording, because the dependency on `task-002` and `task-003` exists precisely on the chance that
  it would not be.
- `HardwareWalletWithNavigationLayout.tsx` and `HardwareWalletsWrapper.tsx` are three characters
  apart and only one goes. Worth naming the survivor.
- The strongest correctness statement available here is a negative: nothing this task removes
  registers a story, so every sidebar count must be unchanged. That is a better acceptance criterion
  than "the checks pass" and the plan should say so.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T00:57:40Z

Plan Updates In Response To Critique:
- Made `nix build .#internal.x86_64-linux.node_modules` step 3 of the implementation approach and
  the first item of the verification plan, with the reason stated: it installs with
  `--frozen-lockfile` and its `offlineCache` comes from `yarn.lock`, so it is what decides whether
  the two edits agree. Added the failure response.
- Added step 2 stating that the two now-unrequested range entries are deliberately left, with the
  reason: pruning them means removing thirteen `@swc/core-*@1.2.175` platform entries behind one of
  them, and a hand-pruned lockfile is harder to trust than one with a stale tail that the next full
  install removes. Repeated it in Risks.
- Added to the findings that the re-verification found no additional orphan, and to Required Docs
  that this be recorded in the task entry.
- Named `HardwareWalletsWrapper.tsx` as the survivor in the findings and in Risks, and recorded that
  the deletion list is generated rather than typed.
- Added the unchanged-sidebar requirement to the acceptance criteria with the reasoning that a
  change removing only unreachable modules must move nothing.

Resulting Approved Plan Shape:
- Four files removed, two edited, one tracking update.
- Verification gated on the `node_modules` build, then the orphan walk, an unchanged sidebar and
  three flake checks.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing to the removal set.
- Scope is unchanged: the three orphans, the empty preview head, the dependency.

Outcome: Canonical task plan revised after critique and approved for build execution
