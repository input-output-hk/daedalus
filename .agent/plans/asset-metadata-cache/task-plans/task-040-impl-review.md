Implementation: Iteration 1
Timestamp: 2026-09-15T11:05:00Z

Changes made:
- `source/renderer/app/components/wallet/send-form/assetDenominations.ts`: new.
  `AssetDenominations`, holding the per-row snapshot and applying both rules.
- `source/renderer/app/components/wallet/send-form/assetDenominations.spec.ts`:
  new. Fifteen cases.
- `source/renderer/app/components/wallet/WalletSendForm.tsx`: the snapshot taken
  in `addAssetFields`, forgotten in `removeAssetRow`, dropped wholesale on reset,
  reconciled in `componentDidUpdate`, and the two new props passed to each row.
  The notice is dismissed through the asset field's existing `onChange` hook.
- `source/renderer/app/components/wallet/send-form/AssetInput.tsx`: `decimals`
  and `hasDenominationChanged` as props; the component no longer reads
  `asset.decimals` at all.
- `source/renderer/app/components/wallet/send-form/messages.ts` and
  `AssetInput.scss`: the notice and its styles.
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx` and
  `WalletSendForm.spec.tsx`: eight cases between them.
- `tests/assets/unit/features/send-form-denomination-change.feature` and
  `tests/assets/unit/steps/send-form-denomination-change.steps.ts`: new. Four
  scenarios.
- the four translation artifacts, regenerated.

Files touched:
- the source, spec and test files above
- `.agent/plans/asset-metadata-cache/task-plans/task-040.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-040-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-040-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Three details decided during implementation:

**The snapshot is taken in `addAssetFields`, not in `addAssetRow`.** Both ways a
row comes into being reach `addAssetFields`: the picker through `addAssetRow`,
and `onChangeAsset` when a row is pointed at a different token. Snapshotting in
`addAssetRow` would have left a row created by `onChangeAsset` with no snapshot,
and the reconciliation would have taken one on the next update, a render later.
The snapshot is taken before the field is created, so there is no moment at
which a row can be typed into without a denomination recorded for it.

**A row whose asset is no longer in the wallet is left out of the
reconciliation.** `AssetInput` renders nothing when the lookup misses, and a
notice on an invisible row helps nobody. The filter is explicit rather than a
consequence of `undefined` flowing through.

**A correction to `task-039`'s spec, recorded rather than edited into it.** The
case named "moves with the denomination it describes, in the same render" drove
the change by mutating `asset.decimals`. After this task the row obeys its prop
and not the asset, so that case would have been asserting a path the component
no longer has. It now drives the prop. The property it pins is unchanged and its
name is unchanged; what changed is that the denomination is now a different
thing, which is this task's whole point.

Verification run, the negative cases first:

- **Cucumber, and the two that matter were driven by breaking the code.** With
  the clear suppressed (`if (false && holdsAnAmount(field))`), exactly two
  scenarios fail, "A resolution arrives while an amount is on screen" and "A
  cached value changes on re-read while an amount is on screen", and the two that
  assert nothing is disturbed keep passing. With the snapshot left unmoved
  instead, three scenarios fail on "that row is denominated in N decimal places"
  and the clearing assertions still pass. Both halves of the rule are therefore
  pinned by a test that fails when that half is removed, rather than by a suite
  that would pass against a stub.
- The step file drives a real `mobx-react-form` field built from the same
  `ReactToolboxMobxForm` the send form uses, and calls the same `reconcile` the
  form calls, so "the field is cleared" is the field being cleared and not a
  boolean being set. `mobx-react-form` runs under Cucumber without a DOM, which
  was checked rather than assumed.
- The first scenario asserts the amount the form would submit at three points:
  `1500000` before the resolution, then not `1500000` and not
  `1500000000000` after it. The second of those is the defect named by its own
  number, so a regression that reinterpreted the digits fails on the number it
  would have produced.
- **Jest, rule 1 on its own.** `AssetInput` rendered with a snapshot of `null`
  while the asset it is handed carries `decimals: 6`: a typed separator is still
  refused, the submitted string is `1500000`, and the label still says whole
  units. The balance beside the field is asserted to be drawn in the snapshotted
  denomination too, so the number the user compares against is in the units the
  field accepts.
- **Jest, through the whole form.** Four cases drive a resolution into an open
  form by re-rendering it with the asset resolved: the amount is cleared and the
  notice names the token; an empty field gets no notice and the next keystroke
  lands in the new denomination; typing again takes the notice back; and a
  re-render that agrees with the snapshot disturbs nothing.
- The twelve existing `WalletSendForm` cases pass unchanged, and the twenty
  existing `AssetInput` cases pass with the two new props supplied.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `51yc1r3f68469nqa36wj1qna96d3xj3v-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `jf3r0zzqv8cv04c1ikrk4mh63s8rgf52-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `jbz82jslpnd973a9zambfyzwlw1dix8k-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `5vfi6n5yjrrr5g8gssgcklvba31c2jms-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, exit 0. The previous state of this branch was 32
  scenarios and 102 steps.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 86 suites passed, 1295
  tests with 1292 passed and 3 skipped, exit 0. The previous state of this branch
  was 85 suites and 1272 tests, so one suite and twenty-three tests were added
  and nothing else moved.

`nix fmt` was run and changed four files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None. The `addAssetFields` placement and the missing-asset filter are both
  refinements of "when the row is added" rather than departures from it, and are
  recorded above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T11:12:00Z

Acceptance criteria, each against the evidence:

1. *A Cucumber `@unit` scenario for a field holding a raw-units amount.* Met, and
   it fails when the clear is removed, which is the only version of this claim
   worth anything.

2. *A Cucumber `@unit` scenario for an empty field.* Met, and it fails when the
   snapshot is left unmoved. The two mutations fail disjoint sets of scenarios,
   so neither scenario is carrying the other.

3. *A Jest spec asserting the submitted value is computed against the snapshot.*
   Met. `AssetInput` given a snapshot that disagrees with its asset submits
   `1500000`, which is the snapshot's reading and not the asset's.

4. *Driving a resolution into an open form by hand does not change what the form
   would submit without the user re-entering it.* Met, and met in the strong
   form: there is nothing left to submit. The field is empty, the row says why,
   and the form is invalid until an amount is entered again, through the
   validator that already refuses an empty value.

5-6. *All five checks, suppressions, dependencies.* Met, with `stylelint` run as
   a sixth because a stylesheet changed.

Two judgements worth naming.

**A move from unknown to zero clears the field**, even though both denominations
read the same digits the same way, so the clear is unnecessary in that case. It
was implemented that way deliberately. The rule this phase was approved under is
that a change in the decimal places clears; an implementation that decided for
itself which changes were harmless would be making exactly the kind of call that
has to be right every time. The cost of being wrong in this direction is a
retype. In the other direction it is an amount.

**The snapshot moves for a cleared row as well as an empty one.** A row that kept
its old snapshot after being cleared would ask the user to type the amount again
into a field denominated in something the application no longer believes, which
is the original defect with an extra step. Pinned by its own case.

What remains open, named rather than left to be found: a resolution arriving
while the confirmation dialog is open is not covered. The amount was computed
when the dialog opened, and the dialog renders the raw and the formatted value
side by side, which is the backstop the PRD describes and explicitly calls
insufficient on its own. Covering it means re-validating behind an open modal.

Decision: approved
