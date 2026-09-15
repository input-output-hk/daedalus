Implementation: Iteration 1
Timestamp: 2026-09-15T08:52:40Z

Changes made:
- `preview.tsx`: `globalTypes` for `themeName`, `localeName` and `osName` with toolbar controls, and
  a decorator reading `context.globals`.
- `StoryWrapper.tsx`: reads the three as props with defaults; no state, no channel subscription. The
  prop pass-through to the story stays.
- `General.stories.tsx`: the Themes story writes back with `useGlobals` instead of `updateParam`.
- `main.ts`: the local addon entry removed from `addons`.
- `package.json` and `yarn.lock`: `@storybook/preview-api@8.6.18` declared.
- Two comments describing the removed channel corrected.
- The five addon files removed by the operator; `storybook/addons/` is gone from disk.

Verification run:
- `compile`: exactly 68 errors, all `TS2305`, down from 70. Both `TS2307` cleared. The prediction was
  made before the run.
- `storybook`: manager built in 228ms, then `Error: Unable to index files: Unexpected storiesOf
  usage`. The failure has moved from configuration to the corpus, which is the state `task-015`
  predicted but could not reach because the manager failed first.
- `lint`: exit 0, zero errors, 5414 warnings.
- `node_modules` rebuilds under `--frozen-lockfile` after the manifest change.
- No `DaedalusMenu` reference survives in `storybook`, `source` or `tests`.

Corrections during the work, three of them:

- The new `@storybook/preview-api` import tripped `import/order`, because a package import has to
  precede a relative one. Moved.
- `prettier --check` flagged `General.stories.tsx`, but the only complaint is a `require(...)` call
  at line 108 that was already non-clean at `HEAD` and that this task does not touch, so it was left
  rather than reformatted.
- The `config.ts` import added to `preview.tsx` was written across five lines; prettier puts it on
  one, because it fits. Caught by running prettier over every file the task touched rather than only
  the one that had previously complained, which is the same sweep habit that found `os-browserify`.

Two comments were also reworded before the commit. Both described what a later change will do rather
than what the code does now, and this tree is public.

Deviations from the approved plan:
- None.

Outcome: The addon is gone, the count is at 68, and the build now fails where the conversion will
fix it; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T08:57:15Z

Summary:
- Approved. The count moved exactly as predicted and the failure moved to where the next task works.

Blocking findings:
- None.

Non-blocking observations:
- Predicting 68 before running is what makes the number worth tracking. A count that is only read
  after the fact cannot distinguish an expected change from an unexplained one.
- Taking the toolbar option lists from `_support/config.ts` rather than restating them means the
  nine themes cannot drift from the nine the wrapper resolves.
- Deleting `getInitialState`'s reason to exist rather than porting it is the right shape for this
  change: the addon reimplemented persistence and URL encoding that the platform now provides, and
  porting it would have carried that duplication across the hop.
- Declaring `@storybook/preview-api` while writing up a finding about undeclared transitive
  dependencies is the consistent call, and cheap.
- The three behavioural criteria remain unexecuted and are reported as such. That is the fourth time
  this phase a criterion has needed that treatment, and the discipline is holding.

Approval bar:
- Met. `task-016` is complete and `task-024` has its precondition.

Decision: approved
