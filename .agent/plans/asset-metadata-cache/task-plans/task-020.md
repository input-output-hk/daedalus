## Task ID and Title

`task-020` — Extend the disagreement helper with verification state.

## Why Chosen Now

`task-019` made the difference between a policy-bound published value and an
unattested one matter to the user: the first is applied automatically and the
second is not. The helper that reports a disagreement between the user's setting
and the published value cannot see that difference, so it says the same thing
about both.

## Interaction Mode

`agent_execution`.

## Scope

- The helper gains the verification verdict as a third field on its existing
  argument object.
- Its two consumers pass it, in the same commit.
- The settings dialog says something weaker when the published value was not
  verified.

## Non-Goals

- No new warning surface. The row keeps exactly the warning it has today, in the
  same cases; what changes is the wording inside the dialog's pop-over.
- Not the settings-dialog advisory sentence. That is `task-022`, which is a
  different statement in a different place: this task is about a disagreement
  between two numbers, that one is about a number nobody can vouch for.

## Dependencies

`task-019`.

## Research Consulted

- `asset-metadata-cache-prd.md:1201-1212`: the helper "gains a third argument,
  whether the recommended value was verified, because the two cases carry
  different copy", and "the existing copy at `assets.warning.notUsing`
  overstates" the unattested case.
- `asset-metadata-cache-prd.md:1150-1156` for how often each case fires: 320 of
  7,977 registry subjects publish nonzero decimals with no `policy` field, which
  is 4.0 percent and is the population the weaker wording is for.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`.

## Live Repo Findings Verified For Planning

1. **The helper has exactly two consumers.** `WalletToken.tsx:52-55` and
   `AssetSettingsDialog.tsx:157-160`. `grep -rn
   "isNonRecommendedDecimalSettingUsed" source` finds those two, the declaration
   and the spec.
2. **`helpers.spec.ts` pins eight assertions across five cases**, and they are
   the record of behaviour this task must not change.
3. **Three other components pick between the same two messages on
   `typeof decimals === 'number'`**: `Asset.tsx:306-314`,
   `WalletTokenFooter.tsx:37-42` and `AssetContent.tsx`. They take `hasWarning`
   as a prop and do not call the helper, so they are out of scope here and their
   wording is unchanged.
4. **The message ids are shared across four `defineMessages` blocks.**
   `assets.warning.available` and `assets.warning.notUsing` are each declared in
   `Asset.tsx`, `AssetContent.tsx`, `WalletToken.messages.ts` and
   `AssetSettingsDialog.tsx` with identical text, so adding a variant to one of
   them adds a new id and touches no existing one.
5. **`recommendedDecimalsVerified` reaches both consumers.** `task-019` put it on
   the `Asset` type and carried it through `getAssetTokenFromToken`, so both
   consumers already hold an `AssetToken` that has it.
6. **One combination is no longer reachable.** A verified published value with no
   user setting is applied, so the resolved `decimals` equals
   `recommendedDecimals` and there is no disagreement to report. The dialog keeps
   a branch for it anyway, because `intl.formatMessage(undefined)` throws and the
   reachability depends on a resolution order that is one commit away from
   changing.

## Files Expected To Change

- `source/renderer/app/components/wallet/tokens/wallet-token/helpers.ts`
- `source/renderer/app/components/wallet/tokens/wallet-token/helpers.spec.ts`
- `source/renderer/app/components/wallet/tokens/wallet-token/WalletToken.tsx`
- `source/renderer/app/components/assets/AssetSettingsDialog.tsx`
- `source/renderer/app/components/assets/AssetSettingsDialog.spec.tsx`
- the four translation artifacts, regenerated.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

1. **One helper, renamed to what it now answers.** It returns which of three
   things is true rather than a boolean: no disagreement, a disagreement with a
   policy-bound value, or a disagreement with an unattested one. A function named
   `is...` that returns three values is worse than a rename, and a second helper
   is what the task exists to avoid.

2. **The verdict is read with `===`**, so an argument that never arrives reads as
   unverified, which is the weaker claim and the safe direction.

3. **The row is unchanged.** `WalletToken` derives its boolean from the verdict,
   so every case that showed a warning icon still shows one, and no case that did
   not now does.

4. **Two new messages** in the settings dialog for the unattested pair. The
   existing two ids are untouched, which keeps the three other components that
   share them exactly as they are.

## Acceptance Criteria

1. An explicit setting equal to a verified recommended value produces no
   disagreement.
2. An explicit setting differing from a verified recommended value produces the
   stronger verdict; differing from an unverified one produces the weaker.
3. All eight existing assertions keep their verdicts: every case that reported a
   disagreement still reports one, and every case that did not still does not.
4. No second helper exists.
5. `compile`, `lint`, `i18n` and `jest` green from `nix build`.
6. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- `helpers.spec.ts` keeps its five existing cases, each rewritten to assert the
  verdict rather than the boolean, so the diff shows what each of the eight
  assertions became. New cases cover the verified and unverified pairs and an
  absent verdict.
- The absent-verdict case is the one that matters for safety: an argument object
  built without the third field must produce the weaker verdict, not the
  stronger.
- `AssetSettingsDialog.spec.tsx` asserts the copy the user sees for each verdict,
  through the rendered pop-over content, rather than asserting on the message
  object.
- All four Nix checks.

## Risks and Open Questions

- **The three components that do not call the helper keep the strong wording.**
  `Asset.tsx`, `AssetContent.tsx` and `WalletTokenFooter.tsx` pick their copy
  from `typeof decimals === 'number'` and take `hasWarning` as a prop. Bringing
  them onto the verdict is a larger change than this task, and the PRD names the
  dialog as where the distinction is worth making. Recorded rather than done.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-020.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-020-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-020-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The disagreement helper reports how strongly to put it, and the settings dialog
says something weaker about a value nobody could check.

## Final Outcome

Complete.

## Self-Review

The rename is the part a reviewer should push on. It touches four files rather
than one, and the alternative is a function called `isNonRecommended...` whose
return value is not a boolean, which is the kind of thing the next reader
believes for a while before finding out.
