## Task ID and Title

`task-039` — Label the send amount field with the unit it is accepting.

## Why Chosen Now

`task-019` made the denomination of the amount field a function of a value that
now arrives from the cache. The one-time notice in `task-021` is dismissible and
the label is not, so the label is the part of the overspend mitigation that still
works on the tenth run. `task-040` depends on this task because the notice it
raises sits beside the label and describes the same thing.

`task-003` deliberately left the `'0'` placeholder alone and recorded that the
durable translated unit label belongs here. This task takes it.

## Interaction Mode

`agent_execution`.

## Scope

- `AssetInput.tsx` renders a translated label stating the unit the field is
  currently accepting, in both the raw-units and the applied-decimals states.
- Two messages and their styles.

## Non-Goals

- No snapshot, no clearing, no blocking notice. That is `task-040`.
- No change to what the field accepts. `allowOnlyIntegers` and `decimalPlaces`
  keep the values `task-003` and `task-019` give them.
- No banner, no storage, no settings copy.

## Dependencies

`task-003`, `task-019`.

## Research Consulted

- `asset-metadata-cache-prd.md:1194-1199` for what the label is for: the habit
  case, where a user types `1500000` after the migration and sends a million and
  a half tokens.
- `asset-metadata-cache-prd.md:1299-1302` for the raw-units half: "the field is
  labeled with the token's ticker or fingerprint rather than a formatted-amount
  placeholder".
- `task-003-impl-review.md`, the recorded deviation: the placeholder stays `'0'`
  and the translated unit label is this task's.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`, which the trust map lists as accurate:
  ids shaped `namespace.context.messageKey`, a `description` on every message,
  and `!!!` on every new `defaultMessage`.

## Live Repo Findings Verified For Planning

1. **The path in the task graph was wrong and is already corrected.**
   `AssetInput.tsx` is at
   `source/renderer/app/components/wallet/send-form/AssetInput.tsx`; the graph
   said `widgets/`. Corrected in commit `ca0c7be00`.
2. **The `label` prop is taken.** `AssetInput.tsx:120` passes
   `<Asset asset={asset} hidePopOver small />` as the `NumericInput` label, and
   `:130` passes `currency={ticker}`, which `AmountInputSkin.tsx:51-54` renders
   in the top-right corner. Neither is a place to put a sentence.
3. **There is room below the input.** `AssetInput.scss:23-25` gives `.assetItem`
   a 20px bottom margin, and `.rightContent` at `:39-47` is positioned from the
   top of `.inputBlock`, over-constrained with `top`, `height` and `bottom`, so
   `top` wins and content added below the input does not move it.
   `.removeAssetBlock` at `:104-108` is a sibling of `.inputBlock` and is
   unaffected.
4. **`AssetInput.scss.d.ts` is generated, not tracked.** `git ls-files` over the
   directory lists four files and not the `.d.ts`, and `yarn compile` regenerates
   it through `precompile`.
5. **The ticker is already the unit where there is one.** `metadata.ticker` at
   `AssetInput.tsx:84`; where there is none the pill at `Asset.tsx:213-217`
   renders `ellipsis(fingerprint, 9, 4)` for a small pill, which is the spelling
   this label matches so the two agree on screen.
6. **A decoded asset name is not a candidate for this label.** `task-001`
   established that a minter-chosen name is rendered only with a marking that
   separates it from a published one, because an asset whose name bytes spell an
   existing ticker is free to exist. A unit label is exactly the place that
   confusion would be expensive, so the label uses the published ticker or the
   fingerprint and never the decoded name.
7. **Both locale files and `translations/messages.json` are tracked and checked.**
   `perSystem/checks.nix:57-77` snapshots them, re-runs `yarn i18n:manage` and
   requires the result to be byte-identical, so the regenerated artifacts are
   part of this commit.

## Files Expected To Change

- `source/renderer/app/components/wallet/send-form/AssetInput.tsx`
- `source/renderer/app/components/wallet/send-form/AssetInput.scss`
- `source/renderer/app/components/wallet/send-form/messages.ts`
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx`
- `source/renderer/app/i18n/locales/defaultMessages.json`,
  `en-US.json`, `ja-JP.json` and `translations/messages.json` — regenerated.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

1. **One derived unit string.** The published ticker where there is one,
   otherwise the fingerprint in the same ellipsised spelling the pill uses.
   Never the decoded asset name.

2. **Two messages, chosen by whether the denomination is known.**
   - Unknown (`decimals == null`): the field accepts whole units of the token as
     the ledger holds them, and says so. This is the state where a typed
     separator is refused, so the label explains a refusal the user would
     otherwise meet without explanation.
   - Known (including zero): the field accepts that many decimal places of the
     named unit. Zero reads correctly in this wording, which is why it does not
     need a third message.

   The known branch covers both the verified-registry value and an explicit user
   setting, because the label describes what the field accepts and not where the
   number came from.

3. **Always rendered**, not conditional on any notice, and computed from the same
   local the `NumericInput` props are computed from, so it cannot describe a
   denomination the field is not using. `task-040` moves both to the snapshot in
   one edit, which keeps that property.

4. **Below the input**, in the gap `.assetItem` already reserves, with the margin
   moved onto the label so the row rhythm is unchanged.

## Acceptance Criteria

1. The field carries a unit label in both the raw-units and the applied-decimals
   states.
2. Changing an asset's decimals between unknown and verified changes the label in
   the same render as the field's interpretation.
3. Both messages are translated, carry a `description`, and follow the id
   convention; `i18n:manage` leaves the tracked artifacts unchanged.
4. `compile`, `lint`, `i18n` and `jest` green from `nix build`.
5. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The spec renders the component in three denominations and asserts on the text
  the user sees, not on a prop: unknown, verified six, and zero.
- The label is asserted to name the ticker where there is one and the
  fingerprint where there is not, with a case for each, because a label that
  silently names nothing is worse than no label.
- Criterion 2 is driven rather than argued: one render, the asset's decimals
  changed under the component, a re-render, and both the label text and the
  field's acceptance of a separator asserted to have moved together. Asserting
  the label alone would pass even if the input had kept the old denomination.
- A case asserts the decoded asset name never appears in the label, driven with
  an asset whose name bytes decode to printable text and whose metadata carries
  no ticker. Without it, a later change that reached for `resolveAssetName` would
  pass every other case.
- All four Nix checks.

## Risks and Open Questions

- **The screenshots the task graph asks for cannot be produced here.** This
  environment has no display and the e2e suite cannot run. What replaces them is
  an assertion on the rendered text in both states, which is the same claim
  checked by machine. The operator procedure, for the record: open the send form
  for a token the registry does not know and read the label under the amount
  field; open it for a token with verified nonzero decimals and read it again.
  Recorded as a deviation rather than marked met.
- **Label length.** A token with no ticker gets a 15-character ellipsised
  fingerprint in the label, which wraps on a narrow window rather than
  overflowing, because the label is a block element with no fixed width.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-039.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-039-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-039-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The amount field states the unit and the precision it is accepting, in both
denominations.

## Final Outcome

Complete, with the screenshot half of criterion 1 recorded as not producible in
this environment.

## Self-Review

The way this becomes decoration is if the label and the input ever read
different values. They are computed from one local today and from one snapshot
after `task-040`, and the spec case that changes decimals under an open
component and asserts on both is what keeps it that way.
