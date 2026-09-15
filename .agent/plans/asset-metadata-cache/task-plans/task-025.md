## Task ID and Title

`task-025` — Translations pass over the new strings.

## Why Chosen Now

Phases 1 to 5 added eleven messages across five surfaces, each landing in the
commit that needed it. A pass over the set is the first time they are read
together rather than one at a time, and agreement between a message and the
values it interpolates is exactly the class of defect that only shows when they
are.

## Interaction Mode

`agent_execution`.

## Scope

The eleven messages this branch adds. Their ids, their descriptions, their
`!!!` markers, whether each has a reader, and whether each renders correctly for
every value it can be given.

## Non-Goals

- No Japanese. The three amount-field labels, the two advisories, the settings
  sentence, the refresh label, the two minter-chosen-name strings and the two
  notice strings are all untranslated and carry the `!!!` marker in `ja-JP.json`
  that says so. Supplying machine Japanese for copy about how an amount is
  entered would be worse than leaving the marker.
- **No edit to a message that already has a Japanese translation.** Changing an
  English default leaves the Japanese for that id in place and stale, and nothing
  in the toolchain detects it. `assets.warning.available` and
  `assets.warning.notUsing` therefore keep their wording even though the finding
  below applies to them too.
- No new message and no new surface.

## Dependencies

`task-021`, `task-022`.

## Research Consulted

- `.agent/skills/i18n-messaging/SKILL.md`, which report 01 records as the most
  accurate standards document in the repository.

## Docs, Workflows, and Skills Consulted

- `perSystem/checks.nix:59-75`, which is what makes the artifacts a check rather
  than a convention: it snapshots them, regenerates, and fails on any difference.

## Live Repo Findings Verified For Planning

1. **Eleven messages, and the set is closed.**
   `git diff ec6954d9a..HEAD -- source/renderer/app/i18n/locales/en-US.json`
   shows eleven added keys and one removed, the removal being
   `wallet.transaction.fetchingTokenData` in `task-023`.
2. **Every one carries a description and the `!!!` marker in source.** Checked
   over the whole catalogue rather than the eleven: of 1,640 extracted messages,
   one lacks a description and eleven lack the marker, and none of those twelve
   is from this branch.
3. **Every id matches `namespace.context.messageKey`.** Checked as a regex over
   all 1,640: zero violations.
4. **Every one has a reader.** A scan over 1,692 `defineMessages` properties in
   `source`, `storybook` and `tests` for a property named nowhere but its own
   definition returns 67 candidates, all pre-existing and most of them looked up
   dynamically by a computed key. None is from this branch.
5. **The `!!!` marker is stripped in `en-US.json` and kept in `ja-JP.json`** for
   all eleven, which is the convention `task-001` established at `366da044`.
6. **ICU plural is established here.** Eleven messages in `en-US.json` already
   use `{n, plural, one {...} other {...}}`, for example
   `governance.drepDirectory.expiry.detail`. `{n, select, ...}` is used nowhere.
7. **Four of the eleven interpolate a count into an unpluralised noun.**
   `assets.warning.availableUnverified`, `assets.warning.notUsingUnverified`,
   `assets.settings.dialog.unverifiedDecimals` and
   `wallet.send.form.assetInput.decimalUnitsLabel` all render
   "1 decimal places" when the value is one. The value is reachable in every
   case: the registry publishes a decimal place count of one for real subjects,
   and the settings dialog's own select offers one as a choice.
8. **The straight apostrophe is not a hazard here.** Four messages in
   `en-US.json` contain both an apostrophe and a placeholder, and in all four the
   apostrophe is followed by a letter, which `intl-messageformat` treats as a
   literal rather than as the start of a quoted section. The branch's own
   messages use the typographic apostrophe throughout, which is never special.
9. **The value passed is a number in all four cases**, so `plural` selects
   rather than falling through: `AssetSettingsDialog.tsx:271-284` and `:305-307`
   pass `recommendedDecimals`, and `AssetInput.tsx:122-127` passes `decimals`,
   both read off the asset where they are typed as numbers.

## Files Expected To Change

- `source/renderer/app/components/assets/AssetSettingsDialog.tsx`
- `source/renderer/app/components/assets/AssetSettingsDialog.spec.tsx`
- `source/renderer/app/components/wallet/send-form/messages.ts`
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx`
- `source/renderer/app/i18n/locales/{defaultMessages,en-US,ja-JP}.json`
- `translations/messages.json`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

The four messages take the plural form the repository already uses, with `#` for
the number so the count is formatted by the same rule everywhere.

The artifacts are regenerated with `yarn i18n:manage` and never hand-edited,
except for the one step the manager does not do: it writes new keys into every
language file with the marker intact, and the source language is cleaned by
hand. No key here is new, so no cleaning is needed; the four English defaults
change in place.

## Acceptance Criteria

1. `yarn i18n:manage` leaves the tracked artifacts unchanged, which is the `i18n`
   check.
2. Every message this branch adds has a description, an id of the documented
   shape, the `!!!` marker in source, and a reader.
3. A published count of one renders "1 decimal place" on all four surfaces, and
   a count of six and of zero render "decimal places" as before.
4. No message that already has a Japanese translation is edited.
5. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
   `nix build`.
6. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- Four new cases, one per message, each driving the count of one and asserting
  the singular sentence. Asserted on rendered text through the same
  `IntlProvider` and `en-US.json` the application loads, so a malformed ICU
  string fails the case rather than rendering as itself.
- The existing cases are the other half: they drive six and zero and assert the
  plural wording unchanged, so the change cannot pass by making everything
  singular.
- Criterion 2 is checked by script over the extracted catalogue rather than by
  reading the eleven, because the interesting answer is whether the branch is
  the exception and that needs the whole set as its denominator.
- Criterion 4 by `git diff` over `ja-JP.json`: the only lines it may touch are
  the four defaults the manager copies, and those four ids have no translation.
- All six Nix checks.

## Risks and Open Questions

- **`assets.warning.available` and `assets.warning.notUsing` keep the defect.**
  They are translated, and correcting the English silently invalidates the
  Japanese with nothing to catch it. The pair now reads inconsistently: the
  unverified variants agree with their count and the verified ones do not.
  Recorded rather than fixed, and it is a decision for whoever commissions the
  next translation round.
- The nine remaining messages have no Japanese and will read as English to a
  Japanese user until a translation round covers them. That is the state the
  `!!!` marker exists to record.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-025.targetPaths` widened to the two
  message sources and the two specs the pass changes; `task-025.status` to
  `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-025-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-025-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The eleven messages this branch adds are compliant, read, and correct for every
count they can be given.

## Final Outcome

Complete.

## Self-Review

A translations pass that regenerates the artifacts and reports them unchanged
has checked that the extractor still works. The questions worth asking are
whether each message has a reader, whether its id and description are what a
translator needs, and whether it renders correctly for every value it can be
handed. The last of those is what this pass actually found.
