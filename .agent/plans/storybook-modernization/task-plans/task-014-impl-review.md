Implementation: Iteration 1
Timestamp: 2026-09-15T07:02:15Z

Changes made:
- Ran the codemod chain in the `task-013` scratch project over the `nodes` and `settings` tranche,
  over `staking/` separately for the dynamic registrations, and over a remedied copy of
  `Staking.stories.tsx`.
- Recorded the findings and the corpus-wide decision in the `task-014` entry, corrected two stale
  counts, and set the status to `completed`.

Files touched:
- On the branch: the tasks JSON and the three `task-014` plan documents. No source change.
- Outside the repository: four dry-run copies in the scratch project, not committed.

Verification run:
- `storybook migrate --list` at 8.6.18 lists all three codemods the plan names.
- Chain over `nodes` + `settings`, 9 files, 28 registrations:
  `storiesof-to-csf` 0 errors, 9 ok; `csf-hoist-story-annotations` 0 errors, 3 ok, 6 unmodified;
  `csf-2-to-3` 0 errors, 4 ok, 5 unmodified.
- Label comparison against the `task-001` baseline for those files: 28 before, 28 after, every label
  preserved. Five carry an explicit `storyName` the codemod attached, for
  `Partial Sync CTA Ready`, `Partial Sync CTA Blocked`, `Terms of Service`,
  `Select Language - initial` and `Select Language - submitting`; the other 23 derive correctly from
  the export identifier.
- Structural check: 1 of the 9 converted files has two `export default` statements,
  `nodes/status/Diagnostics.stories.tsx`, which is the only one of the nine with two `storiesOf`
  calls. Corpus-wide that shape appears in exactly 3 of 65 files, adding
  `staking/Staking.stories.tsx` and `voting/Voting.stories.tsx`.
- Chain over `staking/`: reported `1 ok, 0 errors`, and the output carries 14 of the 19
  registrations. The five lost are exactly the non-string-literal labels, `Delegation Center`,
  `Pools Index`, `Pools Index - Loading`, `Rewards` and `Stake Pools List`.
- Remedy tested: with the four `pageNames` lookups and the one template literal replaced by the
  string literals they evaluate to, the same chain preserves all 19.
- `context.kind` and `context.story` at 8.6.18, read through `composeStories`: both populated,
  carrying `"Preflight / Context"` and `"Context Fields"` respectively, mirroring `title` and `name`.
- `'Wallet Add'` matches no label anywhere in the 258-registration corpus. Neither it nor `'Empty'`
  matches any of the 40 registrations across the 16 files `WalletsWrapper` decorates.
- On the branch, `nix build --no-link .#checks.x86_64-linux.{compile,lint,storybook}` stay green.

Decision recorded against the kill criterion:

- Not triggered. 62 of 65 files convert with zero hand correction and no label loss. Three need a
  mechanical split before the chain runs, and one of those also needs five labels inlined first.
  Hand conversion would be 65 files and 258 registrations. The codemod is used corpus-wide, with two
  mandatory pre-steps and one mandatory post-check per tranche, all recorded in the entry.

Correction to my own work, recorded rather than quietly fixed:

- The first label comparison reported `Step 1` and `Step 2` as lost from the staking file. They were
  not: my hand-written `startCase` did not split a letter-digit boundary, so it derived `Step1` from
  the export `Step1` where Storybook derives `Step 1`. Switching to lodash `startCase`, which is
  what `storyNameFromExport` uses, removed both false positives. This is the second time in this
  phase my own instrument produced a wrong answer, after the `withKnobs` false negative in
  `task-013`, and it would have been the more damaging of the two: it argued for dropping a tool
  that works.
- A first attempt at demonstrating the split remedy produced an invalid file, because the script I
  wrote to separate the two `storiesOf` blocks copied lines beginning with `import` and so broke
  multi-line import statements. Rather than patch scaffolding, the remedy test was narrowed to the
  variable actually in question, inlining the dynamic labels, which is the part that was in doubt.
  Splitting a file with two default exports is not in doubt.

Deviations from the approved plan:
- None in substance.

User interaction is now required:
- No.

Outcome: Both acceptance criteria met, both defects characterised with tested remedies, and the kill
criterion applied to a measured ratio; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T07:07:40Z

Summary:
- Approved. The task asked for an observation instead of a prediction and produced one, including
  two defects the prediction did not contain and a decision that rests on 62 of 65 rather than on
  an impression.

Blocking findings:
- None.

Non-blocking observations:
- The dropped-registration defect is the important one and it is important because of how it fails.
  The run reports success, the file compiles, and five stories are gone. Nothing downstream would
  have said so until a sidebar diff three tranches later showed a shortfall with no obvious cause.
  Measuring the label set rather than the exit status is what caught it, and that was a deliberate
  change made during the critique.
- Testing the remedy rather than proposing it changes what `task-021` inherits. "Inline the dynamic
  labels first" is now a step with a known outcome rather than an idea that might not work.
- The `startCase` false positive deserves its place in the log. A tool that reports a real library
  as broken is more dangerous than one that misses a defect, because the response is to abandon
  something that works, and the only thing separating the two here was checking what Storybook
  actually uses.
- Establishing that two of the four control-flow strings are already dead is a genuine reduction in
  the conversion's risk surface rather than a reassurance. `WalletsWrapper`'s ternary has been
  taking the same branch for as long as no story under it has been called `Empty` or `Wallet Add`.
- Correcting 73 to 68 and 9 to 5 with the arithmetic attached keeps the next reader from re-deriving
  the pre-phase-1 figures, which is the same failure mode `task-012`'s 257 had.

Approval bar:
- Met. `task-014` is complete and `task-015` is unblocked.

Decision: approved
