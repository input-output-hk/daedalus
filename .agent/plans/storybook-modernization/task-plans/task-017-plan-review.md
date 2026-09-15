Planner: Iteration 1
Timestamp: 2026-09-15T09:20:05Z

Plan Summary:
- Created `task-017.md` with the 21 sections. Three transforms in the documented order over 68
  files, `nix fmt`, then measurement.

Repo-Verified Findings Used To Shape The Plan:
- 68 story files, all `.tsx`, one `storiesOf` call each, no non-literal `.add()` label.
- Pre-conversion reading: 258 registrations, 49 panels, 14 groups.
- The 8.6 CLI cannot run from the repository root, and its `--glob` reaches a shell.

Outcome: Canonical task plan drafted and ready for critique

Critique Of Iteration 1
Timestamp: 2026-09-15T09:24:40Z

Blocking gaps:
- Both stated acceptance criteria are satisfiable by a run that silently discards registrations.
  "Every file the codemod can take has been taken" is read off the transform's own report, and
  "the commit contains no hand edits" is a property of the diff. Neither sees a lost story. The
  criterion this task actually needs is the one the window is being measured by, and it was not
  written down.
- The plan proposed one after-instrument, an AST reading. That is a model of what Storybook does,
  written by the same person who is deciding whether the result is correct. On a task whose whole
  risk is silent loss, one model checking one transform is not enough separation.

Non-blocking observations:
- Within-panel order is not part of the baseline, which records a sorted tree. It is still visible
  to a reader of the sidebar, so it should be compared even though nothing requires it.
- Formatting has to happen before the measurement, or the measurement reads something other than
  what gets committed.

Outcome: Requires revision before approval

Planner: Iteration 2
Timestamp: 2026-09-15T09:29:15Z

Plan Updates In Response To Critique:
- Added the label-set criterion explicitly, and said why the two original criteria cannot detect
  the failure mode.
- Added a second after-instrument: `index.json` from a real build, which is Storybook's own indexer.
  The two after-readings have to agree with each other before either is compared to the before.
- Added the per-file ordered sequence comparison.
- Moved `nix fmt` ahead of the measurement in the approach.

Outcome: Canonical task plan revised after critique and approved for build execution
