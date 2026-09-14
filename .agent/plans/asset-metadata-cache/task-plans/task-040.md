## Task ID and Title

`task-040` — Snapshot decimals per asset row and block on a mid-edit
denomination change.

## Why Chosen Now

This is the condition on which optimistic decimals was approved. `task-019`
made the denomination of an open field a value that arrives asynchronously, and
`task-039` put a label on it. Neither stops the value moving under a field that
already holds digits.

## Interaction Mode

`agent_execution`.

## Scope

The two safety rules, and nothing else:

1. Snapshot `decimals` for an asset when its row is added to the send form. The
   row renders and validates against the snapshot, not the live observable.
2. If a resolution changes `decimals` while that row's field is non-empty, clear
   the field and show a blocking notice on that row. If the field is empty,
   update the snapshot silently and show nothing.

## Non-Goals

- No change to what the field accepts for a given denomination, no change to the
  unit label's wording, no change to the resolution order.
- No confirmation-dialog change. The dialog is a backstop and the PRD is explicit
  that it is not a sufficient one; making it sufficient is not this task.

## Dependencies

`task-019`, `task-039`.

## Research Consulted

- `asset-metadata-cache-prd.md:1319-1346`, the whole of it, which is where the
  two rules and the mechanism behind them are set out.
- `asset-metadata-cache-prd.md:1509-1517` for the two `@unit` scenarios this task
  owes and why they are Cucumber rather than Jest.
- `task-003-impl-review.md`, finding 1: react-polymorph does not rewrite the
  string at the moment the denomination flips; the damage happens on the next
  keystroke.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md` for the notice's message.
- `package.json:23`, `test:unit` is the Cucumber `@unit` suite, and
  `perSystem/checks.nix:48` puts it in CI on every system except
  `x86_64-darwin`.

## Live Repo Findings Verified For Planning

1. **The amount submitted is the display string with its separators deleted.**
   `WalletSendForm.tsx:229-236` maps `selectedAssetUniqueIds` through
   `formattedAmountToNaturalUnits(assetFields[uniqueId].value)`, and
   `utils/formatters.ts:156-169` removes `.`, `,` and whitespace. Nothing records
   which denomination produced the string.
2. **`AssetInput` is an `@observer` reading the asset live.**
   `AssetInput.tsx:32` and `:77-83`. `getAssetByUniqueId` at
   `WalletSendForm.tsx:247-250` searches `props.assets`, which the container
   rebuilds from the store, so a resolution reaches the row as new props.
3. **A row is added in exactly two places.** `addAssetRow` at
   `WalletSendForm.tsx:830-840`, called from `componentDidMount` for a
   preselected asset at `:205-209` and from the token picker at `:1374`.
   `onChangeAsset` at `:992-1009` adds one and removes another. `removeAssetRow`
   at `:841-869` is the only removal.
4. **The validator needs no denomination of its own.**
   `WalletSendForm.tsx:894-925` compares `formattedAmountToNaturalUnits(value)`
   against `asset.quantity`, both in natural units, so it is correct for whatever
   denomination produced the string. Snapshotting the input's denomination is
   therefore sufficient; there is no second place the number is interpreted.
5. **An empty field already blocks submission.** The validator at `:882-889`
   returns `[false, fieldIsRequired]` for `null` or `''`, so a cleared field
   makes the form invalid without any new gate. "Blocking" is the notice saying
   why, not a new disable path.
6. **Cucumber `@unit` runs without a DOM and reaches `source/`.**
   `tests/common/unit/steps/mnemonics-form-validation.steps.ts` imports from
   `source/renderer/app/utils/validations`, and the suite runs green at 32
   scenarios and 102 steps at the time of writing.
7. **`tests/assets/unit/` exists and holds `assets.spec.ts`, a Jest spec.** There
   is no `features/` or `steps/` directory under it yet, so this task creates the
   first Cucumber feature for assets.
8. **The `hooks.onChange` slot on the asset field is already used.**
   `WalletSendForm.tsx:878-880` sets one that zeroes the transaction fee. It is
   where the notice is dismissed, because a user typing a new amount has answered
   the notice.

## Files Expected To Change

- `source/renderer/app/components/wallet/send-form/assetDenominations.ts` — new.
- `source/renderer/app/components/wallet/send-form/assetDenominations.spec.ts` — new.
- `source/renderer/app/components/wallet/send-form/AssetInput.tsx`
- `source/renderer/app/components/wallet/send-form/AssetInput.scss`
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx`
- `source/renderer/app/components/wallet/send-form/messages.ts`
- `source/renderer/app/components/wallet/WalletSendForm.tsx`
- `source/renderer/app/components/wallet/WalletSendForm.spec.tsx`
- `tests/assets/unit/features/send-form-denomination-change.feature` — new.
- `tests/assets/unit/steps/send-form-denomination-change.steps.ts` — new.
- the four translation artifacts, regenerated.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

1. **One module owning both rules**, `assetDenominations.ts`, holding a map from
   `uniqueId` to the snapshotted decimals and a `reconcile` that takes the rows
   as they stand now and returns which of them were cleared and which adopted a
   new denomination silently. It clears the field itself, through a two-member
   structural interface (`value`, `clear()`) that `mobx-react-form`'s `Field`
   satisfies, so the form and the Cucumber steps run the same code rather than
   the steps re-implementing the decision.

2. **The snapshot moves in both branches.** A field cleared under the old
   denomination has to be re-entered under the new one, so a row that is cleared
   adopts the new value at the same moment. Not doing this would leave the user
   re-typing into a field still denominated in something the application no
   longer believes.

3. **Unknown and zero are different snapshots.** `null` and `0` both put the
   field in raw units today, so a move between them changes no interpretation,
   and comparing denominations rather than values would suppress the clear. The
   rule as written is that a change in `decimals` clears, and it is implemented
   as written: the comparison normalises `undefined` to `null` and nothing else.
   The cost is one unnecessary clear in a case that is rare and harmless to be
   cautious in; the benefit is a rule that is one sentence long and cannot be
   wrong in the direction that matters.

4. **`WalletSendForm` snapshots in `addAssetRow`, forgets in `removeAssetRow`,
   and reconciles in `componentDidUpdate`.** `componentDidUpdate` is after the
   render that brought the new value, which is exactly what makes the snapshot
   load-bearing: that render draws the row under the *old* denomination, and the
   clear happens in the same commit before the user can type again.

5. **`AssetInput` takes the denomination as a prop** and no longer reads
   `asset.decimals`. Every consumer inside the row moves with it: the
   placeholder, the number format, `decimalPlaces`, `allowOnlyIntegers`, the unit
   label from `task-039`, and the balance rendered beside the field, so the whole
   row is in one denomination.

6. **The notice** is a translated message naming the token, stating that its
   decimal places changed, and that the amount must be entered again. It is
   dismissed by the user typing into the field, through the `hooks.onChange`
   slot that already exists on the asset field.

## Acceptance Criteria

1. A Cucumber `@unit` scenario: a field holding a raw-units amount when a
   resolution arrives asserts the field is cleared and the notice is shown.
2. A Cucumber `@unit` scenario: an empty field when a resolution arrives asserts
   the snapshot updates and no notice is shown.
3. A Jest spec asserts that the value submitted for an asset is computed against
   the snapshotted decimals, not the live store value.
4. Driving a resolution into an open form by hand does not change the number of
   natural units the form would submit without the user re-entering it.
5. `compile`, `lint`, `i18n`, `jest` and `cucumber-unit` green from `nix build`.
6. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

The negative cases are the point of this task, so they are listed as what is
driven rather than as what is asserted about.

- **Cucumber, the non-empty field.** A row added while the token's decimal places
  are unknown, `1500000` typed into it, then the cache resolving the token to six
  decimal places. Asserted: the field is empty, the row is listed as cleared, and
  the amount the form would submit for it is no longer `1500000` and is not
  `1500000000000`. The last assertion is the defect named by its number.
- **Cucumber, the empty field.** The same row with nothing typed. Asserted: the
  row is not listed as cleared, nothing is reported for it, and the row's
  denomination is now six, so the user's first keystroke lands in the new
  denomination.
- **Cucumber, a cached value changing on re-read.** Six to two, with an amount on
  screen. The rules are not only about unknown-to-known, and a comparison written
  against `null` on one side would pass both cases above and fail this one.
- **Jest, rule 1 in isolation.** `AssetInput` rendered with a snapshot of `null`
  while the asset it is given carries `decimals: 6`. Asserted: a typed separator
  is still refused and the submitted string is the raw integer. This is the rule
  stated as a property of the component: what the row obeys is the snapshot.
- **Jest, through the whole form.** The send form with one token, its decimals
  unknown, `1500000` typed, then the same form re-rendered with that token
  resolved to six decimal places. Asserted: the input is empty, the notice is on
  that row, and the notice names the token.
- **Jest, the empty field through the whole form**, asserting no notice appears
  and the field now accepts a decimal separator.
- All five Nix checks, `cucumber-unit` included.

## Risks and Open Questions

- **A resolution arriving while the confirmation dialog is open** is not covered.
  The amount was computed when the dialog opened and the dialog shows the raw and
  the formatted value side by side. Named here rather than fixed, because
  covering it means re-validating behind an open modal, which is its own change.
- **One extra render per reconciliation.** `componentDidUpdate` sets state, which
  renders again. The intermediate frame is drawn under the old snapshot, which is
  the safe one, and the clear lands before the user can type.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-040.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-040-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-040-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A row is denominated in what it was opened in, and a denomination that moves
under an amount clears it and says so.

## Final Outcome

Complete.

## Self-Review

The failure this cannot catch is a second reader of `asset.decimals` appearing
inside the row later. `AssetInput` no longer destructures it, so a future edit
would have to reach past the prop it is given to reintroduce the defect, and the
rule-1 Jest case fails the moment it does.
