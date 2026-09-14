Implementation: Iteration 1
Timestamp: 2026-09-15T11:45:00Z

Changes made:
- `source/renderer/app/components/wallet/tokens/wallet-token/helpers.ts`:
  `isNonRecommendedDecimalSettingUsed` becomes `decimalSettingDisagreement`,
  returning `DecimalSettingDisagreement` and taking the verdict as a third field.
- `source/renderer/app/components/wallet/tokens/wallet-token/helpers.spec.ts`:
  the five existing cases rewritten to assert the verdict, plus five new ones.
- `source/renderer/app/components/wallet/tokens/wallet-token/WalletToken.tsx`:
  passes the verdict and derives its boolean from the result.
- `source/renderer/app/components/assets/AssetSettingsDialog.tsx`: two new
  messages for the unattested pair, and the branch that picks between the four.
- `source/renderer/app/components/assets/AssetSettingsDialog.spec.tsx`: four
  cases.
- the four translation artifacts, regenerated.

Files touched:
- the five source and spec files above
- `.agent/plans/asset-metadata-cache/task-plans/task-020.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-020-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-020-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One detail decided during implementation, and it is a change beyond the task's
wording:

**The warning icon carries its sentence as an `aria-label`.** The copy this task
is about lives in a `PopOver`, and `@tippyjs/react` mounts its content only when
the tip is shown. Driven rather than assumed: a probe rendered the dialog, fired
`mouseEnter` on the icon and dumped `document.body.textContent`, and the
sentence was not in it. So the copy could not be asserted through the rendered
output, and a task whose entire subject is which sentence appears cannot be
verified by the icon's presence alone.

Putting the same sentence on the icon as a label makes it assertable and is a
real improvement in its own right: a pop-over is mouse-only, so until now the
reason for the mark was reachable by hovering and by nothing else. Recorded here
because it is a user-facing change the task did not ask for.

Verification run:

- `jest .../wallet-token/helpers .../assets/AssetSettingsDialog --coverage=false`
  — 16 passed.
- The five existing helper cases keep their eight assertions and their verdicts:
  each one that returned `false` now returns `None`, and each one that returned
  `true` now returns a disagreement. The diff shows every one of them, which is
  what the task asked for.
- The absent-verdict case is driven twice, with the field missing and with it
  `null`, and both give the weaker verdict. An argument object that has not been
  told the value was checked must not claim it was.
- The dialog cases assert the sentence a user gets, not the message object: the
  plain wording for a setting that contradicts a policy-bound value, the weaker
  wording for one that contradicts an unattested value, the weaker "offered
  rather than applied" wording when there is no setting at all, and no mark when
  the setting agrees with a verified value.
- The two pre-existing dialog cases pass unchanged, which is what says the row's
  behaviour did not move.

Checks, all four through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `yfbhl4avs4npyf2mavzi1chlngxkbdk0-daedalus-compile.drv`, exit 0. This is the
  check that matters for a rename: both call sites fail here if either is missed.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `i3q7k1d46208a8d8xm99wb9fv7j6xys2-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `kyakl5lpr7c7yh3r5bac2jv1pascqljh-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 86 suites passed, 1304
  tests with 1301 passed and 3 skipped, exit 0. The previous state of this branch
  was 86 suites and 1295 tests, so nine tests were added to two existing suites
  and nothing else moved.

`nix fmt` reported no change.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.
`grep -rn "isNonRecommendedDecimalSettingUsed" source tests` returns nothing.

Deviations from the approved plan:
- The `aria-label`, for the reason above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T11:50:00Z

Acceptance criteria, each against the evidence:

1. *A setting equal to a verified recommended value produces no disagreement.*
   Met, at the helper and in the dialog.

2. *Stronger for verified, weaker for unverified.* Met, and asserted as the
   sentence rather than as an identifier, which is the only version of this claim
   a translator or a reviewer can check.

3. *All eight existing assertions keep their verdicts.* Met. Nothing that
   reported a disagreement stopped, and nothing that stayed quiet started.

4. *No second helper exists.* Met, and the old name returns nothing anywhere.

5-6. *Compile, lint, i18n, jest, suppressions, dependencies.* All met.

The rename is the judgement to weigh, and it is the right one: a function called
`isNonRecommendedDecimalSettingUsed` that answers with one of three values is a
trap for whoever reads it next. Four files, and the compiler finds every caller.

What stays as it was, stated so it is not read as an oversight: the token row's
warning icon appears in exactly the cases it appeared in before, and the three
components that pick their copy from `typeof decimals === 'number'` rather than
from this helper keep the plain wording. The PRD names the settings dialog as
the place the distinction is worth making, and it is the screen where the user is
being asked to decide about the number.

Decision: approved
