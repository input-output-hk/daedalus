Implementation: Iteration 1
Timestamp: 2026-09-15T02:14:35Z

Changes made:
- Replaced the single barrel entry in `storybook/main.ts` with two globs covering both naming
  conventions and both locations.
- Removed the seven barrel `index.ts` files and the two import-only `_support` modules the change
  orphans.
- Updated `task-001-sidebar-extract.js` to take its reachability roots from the glob rather than
  from the barrel it walked before.
- Recorded four findings in the `task-010` entry and set its status to `completed`.

Files touched:
- `storybook/main.ts`
- 9 removals under `storybook/stories/`
- `.agent/plans/storybook-modernization/task-plans/task-001-sidebar-extract.js`
- the tasks JSON and the three `task-010` plan documents

Verification run:
- The glob was resolved through the version's own machinery rather than a globbing library.
  `normalizeStories` and `toRequireContext` from `@storybook/core-common@6.4.22` produce two
  recursive `require.context` calls over `./storybook/stories` and `./source/renderer/app`, with
  matchers `/^\.(?:...)[^/]*?\.stories\.(ts|tsx))$/` and
  `/^\.(?:...)[^/]*?\.(stories|story)\.(ts|tsx))$/`. Applied to the tracked file list they match 61
  and 4 files respectively, 65 in total, and no `_support` module.
- `nix build --no-link .#checks.x86_64-linux.storybook` was run once with the barrels still on disk
  but no longer referenced by `stories`. Green in 72 seconds wall, 40 seconds of yarn time against
  42 for the barrel build. That run tests the glob in isolation and leaves the barrels available as
  a one-line revert if it had failed.
- The probe, which is the only evidence here that distinguishes a working glob from an empty one.
  `import './GlobProbeDoesNotExist';` was appended to
  `storybook/stories/loading/chain-storage/ChainStorageLocationPicker.stories.tsx`, chosen because
  it sat two sub-barrels deep before this change and is reachable only through the glob after it.
  The check failed with `Can't resolve './GlobProbeDoesNotExist'`. The line was reverted, the file
  greps clean for the probe name, and the check returned to exit 0.
- The orphan walk, re-rooted on the glob-matched files plus `main.ts`, `preview.tsx` and the addon
  register module: 107 files under `storybook/`, 107 reachable, 0 orphaned.
- The sidebar artifact regenerated after the extractor change is byte-identical to the pre-change
  capture under `cmp`: 258 registrations, 49 titles, 14 groups, `UNREACHABLE 0`. First and second
  acceptance criteria met.
- `nix build --no-link .#checks.x86_64-linux.compile` exit 0 on derivation
  `6idfm6qdwmhdhrmczr6hd7z3gvvm5xsd-daedalus-compile.drv`, `.storybook` exit 0, `.lint` exit 0 at
  5391 warnings, unchanged.
- `storybook/preview.tsx` was not edited, so `timemachine.config` at `:9-11` is untouched and the
  third acceptance criterion holds by construction: the frozen clock was never in the barrel.

Unplanned consequence, fixed in this task rather than deferred:

- `task-001-sidebar-extract.js` hardcoded `storybook/stories/index.ts` as the root of its
  reachability walk and threw `ENOENT` the moment that file was removed. The committed baseline
  artifact depends on that script being runnable, so it was updated here: the roots are now every
  file matching the story naming convention under the two directories the globs name. The output
  format is unchanged and the regenerated artifact matches the previous capture byte for byte. Worth
  noting what the change costs: the reachability column is now close to tautological for a
  registering file, because the glob matches all of them. The guarantee it used to provide has moved
  into the build, which is the point of this phase rather than a loss.

Deviations from the approved plan:
- The extractor update above. The plan did not anticipate it. The removals were performed by the
  operator against the list this task produced.

User interaction is now required:
- No.

Outcome: All three acceptance criteria met, the glob is verified against the real matcher and proved
loading by a failing probe, and three checks are green; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T02:19:50Z

Summary:
- Approved. The barrel is gone, the glob is verified against the mechanism the pinned version
  actually uses, and the claim that it loads the corpus rests on a build that failed when it was
  supposed to rather than on one that passed.

Blocking findings:
- None.

Non-blocking observations:
- The probe is the whole difference between this being verified and being assumed. Every other
  signal available here, a green build, a matching regexp, an unchanged sidebar extracted from
  source, is equally consistent with a glob that indexes nothing. Choosing a file that was two
  sub-barrels deep makes it a test of the glob specifically rather than of webpack in general.
- Correcting the verification instrument during planning mattered more than it looks. `globby` and
  `globToRegexp` agree on these two patterns, so the first draft would have reached the right
  answer by luck, and the plan would have recorded a method that does not generalise to the next
  pattern anyone writes.
- The stylesheet finding is the kind of thing this exercise is for. A global stylesheet being run
  through `css-loader` with `modules` enabled is a defect, it was masked by a duplicate import
  taking the correct path, and removing the barrel resolves it as a side effect. Recording the
  symptom in the task entry is the right handling for a change whose effect no check in this
  repository can observe.
- The extractor breaking was foreseeable and was not foreseen. It is fixed in the same commit, which
  is right, and the honest note about the reachability column losing its meaning is better than
  quietly keeping a column that now always reads the same.
- Deleting the two import-only `_support` modules here rather than leaving them for a later sweep
  keeps the orphan count at zero, which `task-007` established one commit ago and which is worth
  more as an invariant than as a one-off measurement.

Approval bar:
- Met. `task-010` is complete. `task-011` and `task-063` are unblocked.

Decision: approved
