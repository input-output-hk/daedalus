## Task ID and Title

`task-022` — State in the settings dialog when published decimals could not be
verified.

## Why Chosen Now

`task-019` made "could not be verified" a decision the application acts on: such
a value formats nothing. A user looking at the decimal places field sees a number
under the word "recommended" that the application has chosen not to use, and
nothing on the screen says why.

## Interaction Mode

`agent_execution`.

## Scope

One sentence, beside the decimal places field in the asset settings dialog, when
the issuer published decimal places that could not be checked against the token's
minting policy.

## Non-Goals

- Nowhere else. No badge on the token row, no second icon, no change to the list.
- No icon here either. A line of text is the whole change.
- Not the refresh control. That is `task-029`, which lands in the same place
  immediately after this and reuses the container this task creates.

## Dependencies

`task-019`.

## Research Consulted

- `asset-metadata-cache-prd.md:1214-1226` for the sentence, the place, and the
  argument against putting it on the row: a badge that appears on most rows is
  decoration rather than a warning, and the signal the user already gets is the
  absence of formatting.
- `asset-metadata-cache-prd.md:1148-1156` for the population: 3,027 subjects
  publish zero decimal places with no `policy` field and 320 publish a nonzero
  count with none, so a per-row badge for unverified metadata would appear on
  42 percent of registry-known rows.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`.

## Live Repo Findings Verified For Planning

1. **The dialog already has everything it needs.** `task-019` put
   `recommendedDecimalsVerified` on `AssetToken` and `task-020` destructures it
   in `AssetSettingsDialog.tsx`'s render.
2. **The condition is not the same as the disagreement verdict.** A disagreement
   needs the published value to differ from the setting; this sentence is about
   the value being uncheckable and is true whether or not the user has set
   anything. `AssetSettingsDialog.tsx`'s `WithUnverified` verdict is suppressed
   when the published value is zero, and the sentence should not be.
3. **The dialog is 236 lines and its stylesheet has no footer area.**
   `AssetSettingsDialog.scss` styles a label, a description, paragraphs, the
   select and the warning icon, so the container for this sentence is new.
4. **The task graph asks the two dialog additions to land in an order that does
   not leave two separate additions to the same layout.** This task creates one
   container under the select; `task-029` puts its control in it.

## Files Expected To Change

- `source/renderer/app/components/assets/AssetSettingsDialog.tsx`
- `source/renderer/app/components/assets/AssetSettingsDialog.scss`
- `source/renderer/app/components/assets/AssetSettingsDialog.spec.tsx`
- the four translation artifacts, regenerated.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

One message, one condition, one element. The condition is "the issuer published
a number and it did not verify", written as
`typeof recommendedDecimals === 'number' && recommendedDecimalsVerified !== true`,
which is independent of what the user has set.

The sentence names the number, because a sentence about a figure the user cannot
see in it is harder to act on than one that names it.

## Acceptance Criteria

1. A subject with unverified published decimals shows the sentence.
2. A subject with verified published decimals does not.
3. A subject with no published decimals does not.
4. No new warning surface appears anywhere in the token list.
5. `compile`, `lint`, `i18n`, `stylelint` and `jest` green from `nix build`.
6. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The dialog spec drives the three cases from the task graph and asserts on the
  rendered sentence, not on a flag.
- A fourth case covers a published zero that did not verify, which is the
  combination the disagreement verdict suppresses and this sentence should not:
  writing the condition in terms of the verdict would fail here and pass
  everything else.
- A fifth case covers a subject with an explicit user setting and an unverified
  published value, asserting the sentence still appears, because the sentence is
  about the published number and not about what the user chose.
- Criterion 4 is checked by grep rather than by a test: the diff touches the
  dialog and its stylesheet and nothing under `components/wallet/tokens`.
- All five Nix checks.

## Risks and Open Questions

- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-022.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-022-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-022-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The settings dialog says, once, when the issuer's published decimal places could
not be checked.

## Final Outcome

Complete.

## Self-Review

The temptation is to reuse `task-020`'s verdict, since it already knows about
verification and is right there in the same render. It answers a different
question, and the zero case is where the difference shows.
