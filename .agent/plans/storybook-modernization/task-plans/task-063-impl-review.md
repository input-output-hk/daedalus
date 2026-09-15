Implementation: Iteration 1
Timestamp: 2026-09-15T04:34:50Z

Changes made:
- Added `storybook/stories/_support/WithLocalState.tsx`: a `withState` with the same signature as the
  package's legacy form, a matching `Store<T>` type, and no Storybook API surface.
- Repointed ten source files at it: nine story files holding the 17 call sites, and
  `settings/utils/helpers.tsx` for the `Store` type.
- Removed `@dump247/storybook-state` from `package.json` and its block from `yarn.lock`.
- Recorded five findings in the `task-063` entry and set its status to `completed`.

Files touched:
- `storybook/stories/_support/WithLocalState.tsx` (added)
- 10 repointed source files
- `package.json`, `yarn.lock`
- the tasks JSON and the three `task-063` plan documents

Verification run:
- Gate first: `nix build --no-link .#internal.x86_64-linux.node_modules` rebuilt in 3 minutes 15
  seconds to `jxnvn5rb5cwxam9840zx2p5dkgqny81d-daedalus-node_modules`. That derivation installs with
  `--frozen-lockfile` and derives its offline cache from `yarn.lock`, so it is what decides whether
  the manifest and lockfile agree. Only the package's own block was removed; no transitive tail was
  hand-pruned, as in `task-007`.
- `git diff --numstat` over the nine call-site files: one insertion and one deletion each. The
  changed line in every case is the import specifier. That is the property the behaviour argument
  rests on.
- The wrapper driven under React with jsdom and `react-dom/test-utils` `act`, using the CI
  `node_modules`. Nine assertions, all passing: initial render shows the initial state; the state
  object is frozen; a direct mutation does not take effect; `set` re-renders with the new value;
  `set` merged rather than replaced; `reset` restores the initial state; state survives a remount,
  as the module-level store did; each `withState` call gets its own store; and the first store is
  undisturbed by a second.
- `nix build --no-link .#checks.x86_64-linux.compile` exit 0 on derivation
  `57ar6a9ayk739qnym9mrwih0p3qskfrm-daedalus-compile.drv`, `.storybook` exit 0, `.lint` exit 0 at
  5397 warnings against 5391, the difference being the wrapper's own.
- The sidebar artifact reports 258 registrations across 49 titles in 14 groups, unchanged.

Three lint errors surfaced and were fixed, all consequences of the swap rather than of the wrapper's
design:

- `import/order` in `common/ItemsDropdown.stories.tsx`, `governance/DRepDirectory.stories.tsx` and
  `governance/Delegation.stories.tsx`. Moving a specifier from a package to a relative path changes
  its import group, so the line had to move below the last package import in each. Worth expecting
  wherever else this substitution is made.
- `react/function-component-definition` on the arrow function `withState` returned. It now returns a
  named function expression, which also matches the package's shape more closely and gives the story
  a useful name.

One measurement worth recording, because the obvious remedy would have been wrong:

- `governance/Delegation.stories.tsx` and `governance/DRepDirectory.stories.tsx` were already
  prettier-dirty under the repository's own prettier 2.1.2, at 18 and 6 lines, before this change.
  They were written against a newer prettier whose output for type parameters, `as` casts and
  curried arrow functions differs. Running `prettier --write` on them reformatted unrelated code, so
  it was reverted and the dirtiness was instead measured before and after and confirmed identical.
  An earlier check that suggested `Delegation.stories.tsx` was clean at `HEAD` was wrong: it ran
  against a copy in the scratch directory, which `.prettierignore` excludes, so prettier matched no
  files and reported success. The re-check used `--stdin-filepath` so config and ignore rules
  resolve against the real path.

The second acceptance criterion, stated honestly:

- It asks that all 17 former call sites render and their interactive state still changes when
  driven. That was not done and cannot be done here. There is no display, the Cucumber e2e suite
  cannot execute against this Electron and Spectron pairing, and `storybook:build` bundles without
  evaluating preview modules, so no check in this repository renders a story. What stands in its
  place is the nine-assertion run above, which exercises the machinery every one of those sites
  depends on, plus the one-line diff at each site, which means no site's arguments, store usage or
  render function changed. A site that worked before works now unless the wrapper differs from the
  package, and the wrapper was written against the package's source.
- Making the wrapper's behaviour a standing guarantee would mean adding `storybook/` to
  `jest.config.js` `roots`, which changes what CI runs. That is left as a decision rather than taken
  here, and the window is bounded because phase 4 deletes the wrapper.

The acceptance grep, stated honestly:

- `grep -rn '@dump247/storybook-state' source storybook package.json` returns one hit, a comment in
  the replacement naming the package it replaces. No import, no dependency and no lockfile entry
  remains. The comment is kept because whoever removes this shim in phase 4 needs to know what it
  stood in for, and rewording it to slip past the grep would be worse than reporting the hit.

Deviations from the approved plan:
- None in the change. No deletion hand-off was needed.

User interaction is now required:
- No, though the Jest roots question above is a decision the owner may want to take.

Outcome: The dependency is gone, the wrapper reproduces its semantics under test, the call-site
diffs are one line each, and three checks are green; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T04:39:30Z

Summary:
- Approved. The blocker on phase 3 is removed, and the replacement was built against the package's
  implementation rather than its description, which is what made a one-line diff per call site
  possible.

Blocking findings:
- None.

Non-blocking observations:
- The one-line-per-site diff is the strongest thing in this change. It converts a question about 17
  interactive stories into a question about one wrapper, and the wrapper is testable in a way the
  stories are not.
- Reproducing the module-level store lifetime rather than reaching for `useState` was the right
  call and would have been easy to get wrong. A `useState` wrapper compiles, passes every check, and
  silently resets story state on each remount, including on a theme switch. The assertion covering
  it is the one most likely to have caught a lazier implementation.
- The shape constraint, that the returned value must be a function rendering a component rather than
  a component, was found by reading `dist/index.js` during planning. Discovered during
  implementation it would have looked like a Storybook fault rather than a design requirement.
- Reporting the acceptance grep as one hit rather than rewording the comment is the right handling.
  The criterion exists to establish the dependency is gone, and it is; a comment naming what was
  replaced is information the next reader needs.
- The prettier episode is instructive and is recorded rather than buried. The first check said the
  file was clean at `HEAD` and was measuring nothing, because the scratch copy fell under
  `.prettierignore` and prettier reports success when it matches no files. A verification that
  cannot fail is worse than no verification, and `--stdin-filepath` is the right instrument.
- Stating plainly that the 17 sites were not driven, rather than letting three green checks imply
  they were, is consistent with how this phase has handled every claim it could not evidence.

Approval bar:
- Met. `task-063` is complete, phase 2 is complete, and phase 3 is unblocked.

Decision: approved
