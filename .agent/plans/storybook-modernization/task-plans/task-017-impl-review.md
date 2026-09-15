Implementation: Iteration 1
Timestamp: 2026-09-15T10:05:30Z

Changes made:
- Applied `storiesof-to-csf`, then `csf-hoist-story-annotations`, then `csf-2-to-3` to all 68 story
  files, 64 under `storybook/stories` and 4 colocated under `source/renderer/app`.
- Ran `nix fmt` over the result. No hand edits.

Run report: `0 errors, 0 skipped` at every stage. `storiesof-to-csf` modified all 68.
`csf-hoist-story-annotations` modified 40 and left 28 unchanged. `csf-2-to-3` modified 45 and left
23. No warning was emitted and no file was skipped.

Measurement, which is the acceptance rather than the run report:
- Before: 258 registrations, 49 panels, read from the `storiesOf` corpus with the committed
  extractor.
- After, first instrument: 258 registrations, 49 panels, read from the converted files by resolving
  each export's effective label the way Storybook does.
- After, second instrument: 258 entries, 49 titles, read from `index.json` emitted by a real
  Storybook build, which is Storybook's own indexer rather than a model of it.
- The three sets are identical. Not similar in size: the same 258 `title | label` pairs, byte for
  byte, in all three.
- Per file, the title and the ordered sequence of labels are identical for all 68, so no panel has
  been reordered either.
- 151 of the 258 come back carrying an explicit `name`, every one of them a case where the export
  identifier cannot spell the display name. Those are what `task-023` checks individually.

Check results:
- `compile`: 19 errors, down from 68, and in a new category. All 19 are `TS2339` in one shape:
  `csf-2-to-3` cannot turn `export const X = ImportedStory` into an object literal, so it leaves
  `X.parameters = {}` and `X.storyName = ''` attached to a const TypeScript has typed as
  `() => Element`. 8 files, spread across four of the five tranches.
- `lint`: 49 errors, up from 0, all `react/function-component-definition`, across 25 files. Every one
  of the 49 is an `export const X = () => ...` story function. The old `.add('X', () => <Y />)` form
  did not trigger the rule because the arrow sat in a call argument, which the rule does not inspect;
  as a `const` initialiser it does. This needs a decision in `task-018`, which is the tranche that
  settles patterns for the other four: either the story functions take a form the rule accepts, or
  the rule is scoped off for story files. Nothing in the plan anticipated it.
- `storybook`: indexing now succeeds and the preview build fails instead. The cause is in
  `storybook/main.ts`, not in any story. It takes `ProvidePlugin` from the root `webpack` at
  5.106.2 while `@storybook/builder-webpack5` runs its own nested copy at 5.111.0, so the plugin
  constructs a `Dependency` from a different class than the compiler expects and the `loc` write
  throws. 156 modules fail to parse behind that one mismatch. It is a configuration defect from the
  version hop rather than conversion work, and it is fixed in its own commit.

Two corrections during the work:

- The first run converted 1 of the 64 files under `storybook/stories` and reported
  `=> Applying storiesof-to-csf: 1 files ... 0 errors, 1 ok`. `storybook migrate` re-launches itself
  as `npx @storybook/cli migrate ...` through `child_process.spawn` with `shell: true`, so the
  `--glob` value is pathname-expanded by `/bin/sh` before the CLI parses arguments. `/bin/sh` has no
  `**`, so `a/**/*.stories.tsx` collapsed to `a/*/*.stories.tsx`, the first match became the value
  of `--glob` and the remaining 63 became positional arguments the CLI ignores. The dry run over a
  copy had not shown this: there, the collapsed pattern matched nothing, so the shell left the word
  alone and the correct pattern reached the tool. The remedy is to drive `runCodemod` from
  `@storybook/codemod` directly, which does its own globbing in process. The five converted files
  were reverted and the whole corpus was run again from the pre-conversion state.
- `nix fmt` reformatted one line in `storybook/stories/_support/WithLocalState.tsx`, a file
  committed earlier in this branch. The reason is that formatting had been checked with
  `node_modules/.bin/prettier`, pinned at 2.1.2, and nothing in this repository runs that copy:
  `.eslintrc` extends `eslint-config-prettier`, which only switches formatting rules off, and
  `nix fmt` runs prettier 3.6.2. The wrong instrument had been in use for several tasks. It produced
  exactly one divergence, corrected in its own commit ahead of this one.

Deviations from the approved plan:
- The plan said to run the three transforms from the 8.6 CLI. They are run from the codemod package
  the CLI would have used, for the reason above.

Outcome: 68 files converted with the label set provably unchanged; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T10:12:45Z

Summary:
- Approved. The conversion is complete and the acceptance is met on the measurement that matters.

Blocking findings:
- None.

Non-blocking observations:
- Three independent readings agreeing on the same 258 pairs is the strongest form this evidence can
  take, and it is what makes the rest of the window checkable. Each later tranche now has a set to
  return to rather than a count to match.
- The glob defect is the third instance this epic of a tool reporting success for work it did not
  do, and the second where the report was a count that looked plausible. `1 files` is not obviously
  wrong the way `0 files` would have been. The habit that caught it is reading what the instrument
  says it measured, not only whether it failed.
- The dry run passing where the real run failed is the part worth remembering. The two differed only
  in directory depth, which no reasonable person would have listed as a variable. A rehearsal that
  does not run against the real paths is a rehearsal of something else.
- Discovering that the formatter being checked was not the formatter being enforced is the same
  shape again, and it had been quietly wrong for several tasks. One file's worth of damage, because
  the two versions agree nearly everywhere.
- The lint regression is a genuine decision rather than a defect, and it is right to surface it
  rather than pick silently: 49 render bodies could be reshaped, or one rule could be scoped off for
  story files, and those are not the same change.

Approval bar:
- Met. `task-017` is complete and the five tranches have their input.

Decision: approved

Correction: Iteration 1
Timestamp: 2026-09-15T10:31:20Z

The check-results section above first said the 49 `react/function-component-definition` errors fire
on `render: () => <X />`. They do not. All 49 sites are `export const X = () => ...`, read back from
the reported line of every one rather than from the two files that had been looked at. `csf-2-to-3`
leaves a plain story function as a function, which CSF 3 permits, and the rule objects to the arrow
rather than to anything CSF asked for. The distinction matters for the decision it hands `task-018`,
because the two shapes would be reshaped differently.

Measured while establishing that: `eslint --fix` rewrites all 49 into
`export function X() { return ...; }`, which the rule accepts and which leaves the export name, and
so the derived label, untouched. So the option is a mechanical pass and not a rewrite, which is
worth knowing before choosing between it and scoping the rule off.
