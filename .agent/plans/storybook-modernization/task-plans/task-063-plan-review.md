Planner: Iteration 1
Timestamp: 2026-09-15T03:52:10Z

Plan Summary:
- Created `.agent/plans/storybook-modernization/task-plans/task-063.md` with the 21 sections the
  task-plans readme requires.
- Scope held to the wrapper, the call-site import changes, and the dependency removal from the
  manifest and the lockfile.
- Classified the task `agent_execution`.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/storybook-modernization/storybook-modernization-prd.md`, locked decision 9 at
  `:253-258` and the Status Log entry extending it.
- `.agent/plans/storybook-modernization/storybook-modernization-tasks.json`, `task-063`.
- `.agent/plans/storybook-modernization/research/02-storybook-upgrade-path.md`.
- `.agent/plans/storybook-modernization/task-plans/task-007.md` for the lockfile gating precedent.
- `.agent/workflows/storybook.md`, which teaches this package and is made wrong by this task.

Repo-Verified Findings Used To Shape The Plan:
- Read the package's `dist/index.js` and confirmed the `@storybook/addons` import and the
  `getChannel()` call inside `withState`, so the channel is touched at module evaluation regardless
  of use.
- Classified all 17 call sites: every one uses the legacy two-argument form, which is what makes an
  import-only change possible.
- Counted the store surface the corpus uses: `state` 44, `set` 30, `reset` 0.
- Found a tenth file the entry does not name, `settings/utils/helpers.tsx:1`, importing `Store` as a
  type.
- Read the package's `Store` implementation for the freezes, the merge in `set` and the
  notify-only-on-change in `reset`, and its module-level store lifetime.
- Established the shape constraint: the package returns a plain function that renders a component,
  so a replacement that returned a hook-using component directly would call hooks outside a render.
- Found that `jest.config.js:129` excludes `storybook/` from the Jest roots, so a colocated spec
  would not run.

Planned Approach:
- Write the wrapper against the package's implementation, repoint ten files, remove the dependency,
  gate on the `node_modules` build, exercise the wrapper under React, then run three checks.

Scope Guard / Self-Review:
- No `useArgs`, no call-site reshaping, no Storybook configuration change.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T03:57:25Z

Blocking gaps:
- The second acceptance criterion asks that 17 interactive sites still change state when driven, and
  the plan quietly proposes to answer it with a compiling build. Nothing in this repository renders
  a story: there is no display, the e2e suite cannot execute, and `storybook:build` bundles without
  evaluating preview modules. The plan has to say that plainly and then say what it substitutes,
  rather than letting three green checks stand in for a claim they do not support.
- The plan describes the wrapper as behaving like the package without saying which behaviours it
  reproduces. "Holds React state and passes it down" is not enough to write it against: the package
  freezes state, merges on `set`, notifies only when `reset` changes something, and keeps one store
  per call site for the life of the module. A wrapper that missed the freeze would turn a throwing
  mutation into a silent one, and a wrapper built on `useState` inside the component would reset
  state on every remount.

Non-blocking observations:
- The shape constraint deserves to be stated as a finding rather than discovered during
  implementation. Returning a component with hooks in it, where the package returns a function
  rendering a component, breaks at the first story and looks like a Storybook bug.
- The entry names two target paths and the real edit set is ten source files. Worth correcting in
  the entry.
- `reset` is used zero times. Implementing it anyway is right, since the type declares it, but the
  zero is worth recording so nobody treats it as load-bearing.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T04:02:40Z

Plan Updates In Response To Critique:
- Rewrote the second acceptance criterion to say in terms that it cannot be answered in full here,
  why, and what is offered instead: the wrapper's state machinery exercised directly under React,
  plus the diff at every call-site file being the import specifier alone, so a site that worked
  before works now unless the wrapper differs from the package. Added the residual risk and its
  bounds to Risks.
- Added the package's exact store semantics to the findings, freeze on construction, merge and
  refreeze on `set`, notify-only-on-change in `reset`, and the module-level lifetime, and made
  reproducing them step 1 of the implementation approach rather than an implied goal.
- Added the shape constraint as a finding in its own right, with the consequence of getting it wrong.
- Added the ten-file edit set and the `helpers.tsx` type import to the findings and to the tracking
  updates.
- Recorded that `reset` has zero call sites and is implemented because the type declares it.
- Added that a permanent guard on the wrapper would need `storybook/` in the Jest roots, that this
  task does not do it, and that the window is bounded because phase 4 deletes the wrapper.

Resulting Approved Plan Shape:
- One new wrapper, ten repointed files, a manifest and lockfile removal gated on the `node_modules`
  build, and an honest account of what interactive verification was and was not possible.

Scope Guard / Self-Review:
- The revision closes both blocking gaps and adds nothing to the change itself.
- Scope is unchanged: the wrapper, the imports, the dependency.

Outcome: Canonical task plan revised after critique and approved for build execution
