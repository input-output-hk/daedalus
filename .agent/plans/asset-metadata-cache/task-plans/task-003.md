# Task task-003: Stop the amount field accepting a decimal separator when decimal places are unknown

## Task ID and Title

- ID: `task-003`
- Title: `Stop the amount field accepting a decimal separator when decimal places are unknown`

## Why Chosen Now

`task-003` has no dependencies in the task graph and is the third task in phase 1. It is
renderer-only: no cache, no IPC channel, no network call and no new dependency.

It is in the **wrong-amount-signed class**, which the plan workspace readme defines as the four tasks
that touch how a typed amount is interpreted before it is signed. The rule those tasks turn on is
that the amount submitted is a pure function of the display string with no record of its
denomination, so anything that changes the denomination under an open form is a defect until proven
otherwise. Here the denomination does not change: the field simply accepts a character that the
submit path then deletes, which turns `1.5` into fifteen raw units.

Two later tasks name it as a dependency, `task-021` and `task-039`, and both are in the overspend
half of the decimals work. This is the underpayment half and it is live today, independent of the
cache, and revertible on its own.

## Interaction Mode

- Mode: `agent_execution`

Every acceptance criterion resolves to a Jest spec or to one of the four Nix checks. Nothing needs a
running node, a network fetch or an operator.

## Scope

- `AssetInput` passes `allowOnlyIntegers` to the `NumericInput` whenever the resolved decimal places
  are unknown or zero, which is the whole set of cases where the field is denominated in raw units.
- The `onKeyPress` decimal-separator guard is deleted. It covers a strict subset of what
  `allowOnlyIntegers` covers and it never covered paste. `handleSubmitOnEnter` stays.
- A colocated `AssetInput.spec.tsx` driving the negative cases explicitly: typed separator, pasted
  decimal string, and the two denominations that must keep working.

Revertible on its own. Reverting the commit restores the guard and the current props exactly. No
other phase-1 task touches `AssetInput.tsx`: `task-002` touched `utils/assets.ts`, `task-004` deletes
three symbols this file does not read, and `task-005` adds a new file.

## Non-Goals

- No blocking and no refusing to send. Locked decision 7 in the PRD: a user holding a token the
  registry has never heard of must still be able to spend it, and raw units are the units it is
  actually denominated in.
- No resolution order for `decimals`. `AssetInput` keeps reading whatever `getAssetByUniqueId`
  gives it. `task-019` owns the resolution order.
- No durable unit label on the field. `task-039` owns it, depends on this task, and its acceptance
  criteria require a translated message and a screenshot of each state. The reasoning for not doing a
  partial version here is under Files Expected To Change.
- No snapshotting of `decimals` per row and no mid-edit denomination change. `task-040` owns both,
  and nothing in this tree can change `decimals` under an open form yet.
- No change to `WalletSendForm.tsx`, to `formattedAmountToNaturalUnits`, or to the send confirmation
  dialog.
- No change to the ada amount field, which has a fixed six decimal places and is not affected.

## Dependencies

- None in the task graph. `task-003` has `"dependencies": []`.
- Practical dependency: Nix, for the four checks.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the send-path section at
  `:1294-1345`, which is the specification this task is measured against, and the problem statement's
  severity framing.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-003` entry, plus
  `task-019`, `task-021`, `task-039` and `task-040` to establish what is deliberately not done here.
- `.agent/plans/asset-metadata-cache/task-plans/readme.md`, the caution on the wrong-amount-signed
  class.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
  - `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.
- Workflows:
  - `.agent/workflows/test.md` for the Jest invocation, read against the `CLAUDE.md` trust map.
- Skills:
  - `.agent/skills/i18n-messaging/SKILL.md` was read and does not apply: this task adds no message.
    The one place a message would go is the unit label, which is `task-039`'s.

## Live Repo Findings Verified For Planning

Verified at `b7e6a028a` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The defect, as it stands.**

- `components/wallet/send-form/AssetInput.tsx:116` is `decimalPlaces={decimals}`, and `decimals`
  comes from `getAssetByUniqueId(uniqueId)` at `:77-83`. `domains/Asset.ts:18` declares
  `decimals: number | null | undefined` with no initialiser, so it is `undefined` for every asset the
  user has never configured.
- With `decimalPlaces` undefined, `NumericInput.js:198` selects `validInputNoSignsRegExp`, which is
  `^([0-9.,]+)?$` once the separators are substituted in, so a decimal separator is accepted.
- `NumericInput.js:393` is `number.toFixed(decimalPlaces, roundingMode)`. Measured with the
  repository's Node (v22.23.1): `new BigNumber('1.23456789').toFixed(undefined, 4)` returns
  `'1.23456789'` rather than throwing, and `undefined > 0` is `false`, so the branches at `:285` and
  `:319` keep the separator in the displayed value instead of dropping it.
- `WalletSendForm.tsx:235` and `:916` and `:939` all run
  `formattedAmountToNaturalUnits(field.value)`. `utils/formatters.ts:163-167` implements that by
  deleting every `.`, `,` and whitespace character. Measured: `'1.5'` becomes `'15'`. So the value
  validated and the value submitted are both fifteen raw units for a field that displayed one and a
  half.
- The same helper is the input to `validateAssetAmount` at `:938-940` and to the balance check at
  `:915-921`, so nothing downstream catches it: fifteen raw units is a perfectly valid amount.

**What `allowOnlyIntegers` actually changes.**

- `NumericInput.js:197` defines `validInputOnlyIntegersRegExp` as `^([0-9]+)?$` and `:199` selects it
  over both sign variants when `allowOnlyIntegers` is set. The check at `:206` returns the previous
  value unchanged when the pattern fails, and `:87-91` only calls `onChange` when the value actually
  changed. So a typed or pasted separator is refused at the source rather than corrected afterwards.
- `NumericInput.js:374` and `:380`: with `allowOnlyIntegers` the displayed string is
  `new BigNumber(number).toString()` instead of `toFormat(decimalPlaces, ...)`. **Group separators
  are therefore dropped from the field.** Measured: `new BigNumber('1234567').toFormat(0)` is
  `'1,234,567'` and `.toString()` is `'1234567'`.
- That is a visible change and it is an improvement here rather than a cost.
  `config/profileConfig.ts:19-32` offers three number formats and `number-2` is
  `8.638.301.639,283542`, where the group separator is `.` and the decimal separator is `,`. A raw
  units amount rendered grouped under that profile reads `1.500.000`, which is exactly what one and a
  half million tokens written in decimal notation looks like. Dropping the grouping removes that
  ambiguity from the one field where the ambiguity is expensive. Recorded under Risks as a
  readability trade the owner may want to revisit.
- `NumericInput.js:449-455` lists the defaults: `allowOnlyIntegers` is `false`, so nothing else in
  the repository is affected by reading this prop.

**The keypress guard it replaces.**

- `AssetInput.tsx:128-140` calls `evt.preventDefault()` for `charCode` 190, 110 or 46 but only when
  `decimals === 0`. Three facts about it:
  - It does not fire when `decimals` is `undefined`, which is the case the defect is about.
  - `onKeyPress` does not fire for a paste, so pasting `1.5` into a zero-decimal field today is
    accepted and then floored by `toFixed(0, ROUND_FLOOR)` to `1`. Measured:
    `new BigNumber('1.5').toFixed(0, BigNumber.ROUND_FLOOR)` is `'1'`.
  - `charCode` 190 and 110 are key *codes*, not character codes; a `keypress` event carries the
    character code, which for `.` is 46. So two of the three constants have never matched anything.
- `allowOnlyIntegers` covers the zero case, the unknown case, the typed case and the pasted case, so
  the guard is strictly redundant afterwards and is deleted rather than left beside it.

**What the send confirmation already does, which is the asymmetry the task names.**

- `containers/wallet/dialogs/send-confirmation/DialogContentWithAssets.tsx:75-79` renders the
  formatted amount and `:109-111` renders `assetsAmounts[index]`, the raw natural-units value, under
  the label at `:84-88` and the explanation at `sibling messages.ts:87-97`. The confirmation is
  honest about there being two representations. The input converts one into the other by deleting a
  character.

**Where the unit label already comes from.**

- `AssetInput.tsx:113` passes `label={<Asset asset={asset} hidePopOver small />}`, and
  `components/assets/Asset.tsx:213-217` renders the fingerprint inside that pill.
  `AssetInput.tsx:153-158` renders the ticker in its own span to the right of the field whenever the
  registry published one. So the field is already labelled with the token's ticker or its
  fingerprint, which is what the PRD's send-path section asks for at `:1302-1303`.
- The placeholder at `:105-111` is `0` followed by the decimal separator and `decimals` zeroes when
  `decimals` is set, and the single character `0` when it is not. There is therefore **no**
  formatted-amount placeholder in the case this task changes: `'0'` is a bare integer, and once the
  field is integers-only it is an accurate hint rather than a misleading one.

**Existing test coverage.**

- There is no `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx`. The directory
  holds `AssetInput.tsx`, `AssetInput.scss`, its generated `.scss.d.ts`, `helpers.ts` and
  `messages.ts`.
- `components/wallet/WalletSendForm.spec.tsx` renders the whole form and drives asset amount fields
  through the `assetInput:` test id set at `AssetInput.tsx:114`. Its fixtures at `:58-74` set
  `decimals: 0`, so that suite exercises the zero-decimal branch this task changes and is a real
  regression signal rather than an unrelated suite.
- `tests/_utils/TestDecorator.tsx` supplies `IntlProvider` with the real `en-US` messages and
  react-polymorph's `ThemeProvider`, which is what `AssetInput` needs from context. The send-form
  spec also wraps in `BrowserLocalStorageBridge` and `DiscreetModeFeatureProvider`, both of which
  `AssetInput` needs for `DiscreetTokenWalletAmount` at `:94-98`.
- `utils/ReactToolboxMobxForm.ts` plus `mobx-react-form/lib/validators/VJF`, imported at
  `WalletSendForm.tsx:11`, are enough to build one real field for the spec, so the spec drives the
  same `Field` object the application passes rather than a stub.

**A note on the Nix checks.** `perSystem/checks.nix:15-32` builds from `srcWithoutNix`, derived from
`inputs.self`, so a new spec file must be `git add`ed before any check can see it.

## Files Expected To Change

- `source/renderer/app/components/wallet/send-form/AssetInput.tsx` — the prop, the deleted guard.
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx` — new.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-003` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-003*.md` — this plan and its two review logs.

One deviation from the task graph's implementation notes, recorded rather than taken silently:

1. **The placeholder is not replaced with the ticker or the fingerprint.** The note asks for it; three
   findings argue against doing it here. The field already carries both, through the `Asset` pill
   passed as `label` and through the ticker span to the right of the input. The `'0'` placeholder is
   not a formatted-amount placeholder and becomes accurate rather than misleading once the field is
   integers-only. And `task-039` owns the durable unit label, depends on this task, and requires a
   translated message with a `description` plus a screenshot of each state; a partial version here
   would add an i18n id that `task-039` then has to replace. The outcome the PRD's send-path section
   specifies at `:1302-1303`, a field labelled with the token's ticker or fingerprint rather than a
   formatted-amount placeholder, holds after this change. Flagged for the owner rather than assumed.

## Implementation Approach

1. **One derived flag, named for what it means.**

   ```ts
   // A ledger quantity is an integer and decimal places are presentation only,
   // so a field whose decimal places are unknown, or known to be zero, is
   // denominated in raw units and a decimal separator in it means nothing.
   const areDecimalsKnown = decimals != null;
   const isInRawUnits = !areDecimalsKnown || decimals === 0;
   ```

   `decimals != null` is the sanctioned loose comparison in `CLAUDE.md` and is the one that treats
   `null` and `undefined` alike while keeping `0` known. Written out rather than as `!decimals`,
   because in a wrong-amount-signed change the reader should not have to work out which falsy values
   are in the set.

2. **Pass it.** `allowOnlyIntegers={isInRawUnits}` on the `NumericInput`. Every other prop is
   unchanged, including `decimalPlaces={decimals}` and
   `bigNumberFormat={decimals ? currentNumberFormat : null}`, so the decimal-denominated path is
   byte-for-byte what it is today.

3. **Delete the guard.** `onKeyPress` becomes `handleSubmitOnEnter` directly. The `evt.persist()`,
   `preventDefault` and `stopPropagation` block and its three `charCode` constants go.

4. **Spec.** `AssetInput.spec.tsx`, colocated, building one real `ReactToolboxMobxForm` field and
   rendering the component inside the same provider stack `WalletSendForm.spec.tsx` uses.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **`yarn test:jest` passes.** Run as `nix build '.#checks.x86_64-linux.jest' --no-link`. The
   existing `WalletSendForm.spec.tsx` is the regression signal, since its fixtures are zero-decimal
   assets driven through this exact input.
2. **The amount entered and the amount in the built transaction agree for every combination of
   decimals set, unset and zero.** Settled by a spec that, for each of the three, drives the input
   and asserts `formattedAmountToNaturalUnits(field.value)` against the number of raw units the
   entered string means. This is the criterion the task exists for and it is asserted as an equality
   on the submitted value, not as the absence of an error.

Three criteria this task adds to its own closure:

3. The negative cases are driven, not implied. A typed separator, a pasted decimal string and a
   pasted non-numeric string each have their own case with their own assertion, for the unknown and
   the zero denomination.
4. `compile`, `lint` and `i18n` are green from `nix build`, not from host tooling.
5. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

Repository verification already done for planning is under Live Repo Findings. This task is in the
wrong-amount-signed class, so the plan lists and drives its negative cases rather than asserting that
the positive path still works.

A note on what the spec can and cannot distinguish. In jsdom a typed character and a paste both
arrive as one `change` event; react-polymorph tells them apart only through `inputType` on the native
event. That distinction does not matter for this change, because the integers-only pattern is tested
at `NumericInput.js:206`, before any branch that reads `inputType`. The spec therefore drives both
shapes as `change` events, one cumulative event per keystroke for typing and one event carrying the
whole string for a paste, and says so rather than implying it covers two mechanisms when it covers
one.

Negative cases, `decimals` **unknown**:

- Typing `1.5` as the successive cumulative values a keystroke sequence produces: the field value
  never contains a separator, and after the separator keystroke the value is still `1`. Asserted on
  the form field's value, which is what the submit path reads, not on the rendered string alone.
- The refused keystroke does not reach the form at all: the `onChange` the component passes to the
  input is not called for it. Asserting that the value is unchanged would also pass if the form were
  handed the same value twice, and the property worth pinning is that a refused character never
  becomes an amount.
- A single `change` event carrying `1.5`, which is what a paste produces: the field value is
  unchanged from before the event. Asserted as equal to the prior value, not merely as "not 1.5".
- A single `change` event carrying `1,5`, the same paste under a number format whose decimal
  separator is a comma: same assertion.
- A `change` event carrying `abc`: the field value is unchanged.
- The submitted value: `formattedAmountToNaturalUnits(field.value)` after entering `1500000` is
  `'1500000'`, so a user who means one and a half of a six-decimal token can still express it.
- Both spellings of unknown are driven, `undefined` and `null`, because the flag is written as
  `decimals != null` and a spec that only drives one of them does not check the comparison that was
  chosen.

Negative cases, `decimals` **zero**:

- A single `change` event carrying `1.5`: the field value is unchanged. Today this is accepted and
  floored to `1`, so this case asserts the new behaviour explicitly rather than assuming the guard
  covered it.
- Typing `.` after a digit: no separator appears in the value.

Positive cases that must not regress:

- `decimals` set to `6`, a `change` event carrying `1.5`: the field value is `1.500000` and
  `formattedAmountToNaturalUnits` of it is `'1500000'`.
- `decimals` set to `6`, a `change` event carrying `0.000001`: the submitted value is `'1'`, the
  smallest expressible amount, which is the boundary the rounding mode would eat if
  `decimalPlaces` were wrong.
- `decimals` unknown, a large integer: the field renders it and the submitted value equals it
  digit for digit. This is also where the loss of group separators is asserted, so the change in the
  rendered string is pinned by a test rather than discovered later.

Commands:

- `nix build '.#checks.x86_64-linux.jest' --no-link`, with the new spec staged.
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `git diff` read for new `@ts-ignore`, of which there must be none.

Before the change is applied, the same spec is run against the current component to record which of
the negative cases pass today. A negative case that passes before and after proves nothing, and the
point of the exercise is to know which ones this change actually closes.

If a check reports a substituted result rather than a local build, it is re-run with `--rebuild`.

## Risks and Open Questions

1. **The field loses its group separators wherever it is integers-only.** Evidence and the counter
   argument are under Live Repo Findings: grouping a raw-units integer under the `number-2` profile
   produces `1.500.000`, which is indistinguishable from a decimal amount, and that is the profile
   where a mistyped separator is most likely in the first place. The cost is that a large raw amount
   is harder to read at a glance. Named for the owner; not mitigated here, because the only way to
   keep grouping is to keep accepting the separator.
2. **A zero-decimal token's field now refuses a pasted `1.5` instead of flooring it to `1`.** That is
   a behaviour change beyond the strict wording of the task graph's fourth test case, which says
   behaviour with `decimals === 0` is unchanged from today. It is unchanged for every input a user
   can type; it differs for paste, in the direction of refusing input rather than silently altering
   it. Stated here rather than buried, because the task is in the wrong-amount-signed class and an
   unstated behaviour change in it is the thing the class exists to prevent.
3. **The change makes an unresolved token's amount field stricter, not more permissive.** It cannot
   make a token unspendable: every integer is still accepted and raw units are the units the ledger
   holds. The one thing a user can no longer do is type a decimal amount for a token whose decimal
   places nobody has published, which was never a meaningful thing to type.
4. **Open question for the project owner, not blocking:** whether the unit label from `task-039`
   should be pulled forward. The PRD's argument that the raw-units field "needs no warning beyond the
   label" leans on a label that this task does not add, because `task-039` owns it and depends on
   this. Until it lands, the field's unit is stated only by the ticker span and the fingerprint in
   the pill.

## Required Docs, Research, and Tracking Updates

- Update `task-003`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-003-plan-review.md` and `task-003-impl-review.md` as the cycle requires.
- No PRD change. The send-path section describes what is built.
- Two task-graph inconsistencies found while tracing ownership, recorded and not edited from inside
  this task: `task-039` and `task-040` both list
  `source/renderer/app/components/wallet/widgets/AssetInput.tsx` in their `targetPaths`, and no such
  file exists. The component is at `source/renderer/app/components/wallet/send-form/AssetInput.tsx`.
  Surfaced in the handoff.
- One source defect found in passing, recorded per `CLAUDE.md` and fixed only incidentally by the
  deletion: the keypress guard tested `charCode` against 190 and 110, which are `keyCode` values.
  A `keypress` event carries the character code, so only 46 could ever have matched.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-003-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-003-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-003` complete. Reviewed and approved in `task-003-impl-review.md`.
- The asset amount field accepts only integers whenever its decimal places are unknown or zero, so a
  decimal separator can no longer be typed or pasted into a field whose submit path would delete it.
  The keypress guard it replaces is gone.
- Seven of the spec's fourteen cases fail against the unchanged component and pass after, which is
  recorded case by case in the implementation review. The two zero-decimal cases pass in both states
  and are kept as regression guards rather than claimed as closures.
- Two behaviour changes beyond the task graph's wording are pinned by tests: the field renders
  without group separators wherever it is integers-only, and a pasted amount carrying separators is
  refused rather than parsed. Both are argued in the review, the first as a safety improvement under
  the number profile whose group separator is a full stop.
- Checks, all from `nix build` and all built locally: `compile` exit 0, `lint` exit 0, `i18n` exit 0,
  `jest` 75 suites and 989 tests with 986 passed and 3 skipped.
- The one judgement call: the `'0'` placeholder was not replaced with the ticker or the fingerprint.
  Reasons are under Files Expected To Change and it is flagged for the owner.

## Self-Review

- The plan changes one prop and deletes one handler body. Everything else in the task is evidence and
  tests, which is the right shape for a wrong-amount-signed change.
- Every claim under Live Repo Findings carries a `path:line`, a command or a measured value taken at
  `b7e6a028a`.
- The one deviation from the task graph's implementation notes is recorded with its three reasons and
  flagged for the owner rather than taken quietly.
