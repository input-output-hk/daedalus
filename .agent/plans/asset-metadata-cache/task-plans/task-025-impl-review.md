Implementation: Iteration 1
Timestamp: 2026-09-15T17:10:00Z

Changes made:
- `source/renderer/app/components/assets/AssetSettingsDialog.tsx`: the plural
  form on three messages.
- `source/renderer/app/components/wallet/send-form/messages.ts`: the plural form
  on `assetInputDecimalUnitsLabel`.
- `source/renderer/app/i18n/locales/{en-US,ja-JP}.json`: the same four defaults,
  by hand, for the reason below.
- `source/renderer/app/i18n/locales/defaultMessages.json` and
  `translations/messages.json`: regenerated.
- `AssetSettingsDialog.spec.tsx` and `AssetInput.spec.tsx`: four cases.

Files touched:
- the two message sources, the two specs and the four artifacts
- `.agent/plans/asset-metadata-cache/task-plans/task-025.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-025-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-025-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One detail discovered during implementation, and it changes what "do not
hand-edit the artifacts" means:

**`yarn i18n:manage` does not propagate a changed default into the language
files.** `react-intl-translations-manager` adds a key it cannot find and leaves
alone a key it can, so after the four source messages changed, `i18n:manage`
rewrote `defaultMessages.json` and `translations/messages.json` and left
`en-US.json` and `ja-JP.json` holding the old sentences. Since the application
renders from the language file and not from the source default, the plural form
would have had no effect at all: measured, not reasoned about, by running
`i18n:manage` and reading `git diff --stat`, which named two files rather than
four.

The four entries in each language file were therefore edited by hand, keeping
the `!!!` marker in `ja-JP.json` because those ids are untranslated and dropping
it in `en-US.json` because that is the source language. `i18n:manage` was re-run
afterwards and changed nothing further, which is what the `i18n` check then
confirms.

The task's note "do not hand-edit them" is correct for adding a message and wrong
for changing one. It is corrected in the graph rather than worked around.

Verification run:

- `jest` over the two specs — 43 passed, of which 4 are new.
- Each of the four new cases drives a count of one and asserts the singular
  sentence on rendered text, through the same `IntlProvider` and `en-US.json` the
  application loads. A malformed ICU string fails there rather than rendering as
  itself, which is what makes the case worth writing.
- The complement is the seven existing cases that drive six and zero and assert
  the plural wording. They pass unchanged, so the change cannot have made
  everything singular. Zero is the one worth naming: English takes the plural for
  it and ICU's `other` category covers it, so "0 decimal places" is unchanged.
- The audit behind the plan's findings, over the extracted catalogue rather than
  over the eleven: 1,640 messages, one without a description and eleven without
  the `!!!` marker, none of the twelve from this branch; zero id-shape
  violations; 1,692 `defineMessages` properties scanned for one named nowhere but
  its own definition, 67 candidates, all pre-existing and mostly looked up by a
  computed key, none from this branch.
- `git diff` over `ja-JP.json` touches four lines, all four of them ids with no
  Japanese translation. No translated message was edited.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `x6bhdjp807d32md9jz2yxwyx0gbcx80r-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `vh5dpgdwpnvdr1gzcmg6yajrylnvs4zy-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `r6cfj98czax3xig9lvyjqp0nxwa13pwf-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `kk4dnqs99iaimxdgn8nxdaf2p2ynsw3g-daedalus-i18n.drv`, exit 0. The result of the
  task rather than a guard: it is what says the four artifacts in this commit are
  the ones the source now produces.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 89 suites passed, 1350
  tests with 1347 passed and 3 skipped, exit 0. The previous state of this branch
  was 89 suites and 1346 tests, so four tests were added to two existing suites
  and nothing else moved.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

`nix fmt` reported no change.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The hand edit to the two language files, for the reason above. The plan said
  the manager regenerates them; it regenerates two of the four.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T17:16:00Z

Acceptance criteria, each against the evidence:

1. *`i18n:manage` leaves the artifacts unchanged.* Met, and it is the check
   rather than the command that says so.

2. *Every message this branch adds is compliant and has a reader.* Met, and
   measured against the whole catalogue rather than the eleven, which is the only
   way the answer means anything.

3. *A count of one reads correctly.* Met on all four surfaces, with the existing
   six and zero cases as the complement.

4. *No translated message edited.* Met, by diff.

5-6. *All six checks, suppressions, dependencies.* Met.

The finding worth naming is the one about the tool. "Regenerate, do not hand-edit"
is the right rule for adding a message and is silently wrong for changing one,
because the manager only ever adds. Had the four source defaults been changed and
the artifacts regenerated with nothing else done, every check would have passed
and no user would have seen a different sentence. That is the shape of failure
this pass exists to catch, and it was caught by reading what the regeneration
actually touched rather than by trusting that it touched everything.

The restraint worth naming is the two translated siblings. `assets.warning.available`
and `assets.warning.notUsing` have the same defect and keep it, because
correcting the English would leave their Japanese silently describing something
else. The pair now reads inconsistently and that is recorded in the plan's Risks
as a decision for the next translation round, rather than fixed here at the cost
of a translation nobody would notice going stale.

Summary: Eleven messages, checked against the convention as a set and against the
values they are handed. Four of them said "1 decimal places" and now do not, on
the four surfaces that say it, in both the file the application reads and the
file a translator will.

Decision: approved
