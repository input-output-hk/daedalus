Implementation: Iteration 1
Timestamp: 2026-09-15T09:55:00Z

Changes made:
- `source/renderer/app/components/wallet/send-form/messages.ts`: two messages,
  `assetInputRawUnitsLabel` and `assetInputDecimalUnitsLabel`.
- `source/renderer/app/components/wallet/send-form/AssetInput.tsx`: the derived
  `unit`, the chosen `unitLabel`, and the element that renders it.
- `source/renderer/app/components/wallet/send-form/AssetInput.scss`: the label's
  styles, with `.assetItem`'s bottom margin moved onto it so the row rhythm is
  unchanged.
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx`: six
  cases, and the render helper reshaped to allow a re-render.
- `source/renderer/app/i18n/locales/{defaultMessages,en-US,ja-JP}.json` and
  `translations/messages.json`: regenerated.

Files touched:
- the five source files above
- `.agent/plans/asset-metadata-cache/task-plans/task-039.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-039-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-039-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

**The re-render case needed a new element, not a mutated one.** The first
version of the denomination-change case mutated the asset and called `rerender`
with the same element. It failed, and the reason is the point of the case:
`AssetInput` is an `@observer`, so mobx-react gives it a shallow prop
comparison and identical props skip the render entirely. The helper now builds a
fresh element, which is what the application does: the lookup is a new function
on every container render. A case that had "passed" by mutating an object the
component was already holding would have asserted nothing about the real path.

**`en-US.json` carries the message without the `!!!` marker.**
`react-intl-translations-manager` writes new keys into every language file with
the marker intact. The source language is then cleaned by hand, which is what
`task-001` did at commit `366da04dd`, and `ja-JP.json` keeps the marker because
the string is genuinely untranslated. Re-running `i18n:manage` does not undo the
cleaning, because the manager only adds keys it does not find, which is why the
`i18n` check is green with the marker stripped.

Verification run:

- `jest source/renderer/app/components/wallet/send-form/AssetInput --coverage=false`
  — 20 passed, of which 6 are new. The fourteen from `task-003` are unchanged
  and pass, which is the regression guard for what the field accepts.
- The label is asserted on rendered text, through the same `IntlProvider` and
  `en-US.json` the application loads, so a message id that does not resolve fails
  the case rather than rendering a raw default.
- Criterion 2 is driven, not argued. One case types `1`, `1.`, `1.5` into a
  raw-units field and asserts the field holds `1`; sets the asset's decimals to
  six; re-renders; asserts the label now reads six decimal places; then clears
  and types `2`, `2.`, `2.5` and asserts the field holds `2.500000`. The label
  and the field's acceptance are asserted in the same case, because a label that
  moved while the input did not would invite the user to type something the
  input drops.
- The fingerprint fallback is driven twice: once for an issuer who published a
  name but no ticker, and once for an asset with no metadata at all. Both assert
  the ellipsised spelling the pill uses.
- The decoded-name case is the negative one: the asset name bytes decode to
  `Cointest`, and the label is asserted not to contain it. Without that case a
  later change reaching for `resolveAssetName` would pass everything else.

Checks, all five through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `na70c17lv12drx90m7y7mfq1lmn4yyw7-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `ifgylp60805sfjbz5171b3ggqg59nvmk-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `p4vlpanl6b9dvdp62f7pky55z7x2873v-daedalus-i18n.drv`, exit 0. This is a result
  rather than a guard here: it is what says the regenerated artifacts in this
  commit are the ones the source produces.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `inzzlmlhkd7vjiafi9qbfxk4kxf2sxkj-daedalus-stylelint.drv`, exit 0. Run because
  this is the first commit on the branch to touch a stylesheet.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 85 suites passed, 1272
  tests with 1269 passed and 3 skipped, exit 0. The previous state of this branch
  was 85 suites and 1266 tests, so six tests were added to an existing suite and
  nothing else moved.

`nix fmt` was run and changed one file before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The screenshots in acceptance criterion 1 are not produced. There is no display
  in this environment and the e2e suite cannot run. What stands in their place is
  an assertion on the rendered text in each state, and the operator procedure is
  in the plan's Risks section. Recorded rather than marked met.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T10:00:00Z

Acceptance criteria, each against the evidence:

1. *The field carries a unit label in both states.* Met in substance and asserted
   by machine; **the screenshot half is not met and is recorded as a deviation**
   rather than rounded up.

2. *Toggling between unknown and verified changes the label in the same render
   as the field's interpretation.* Met, and the case that proves it is the one
   that failed first for the right reason. The label and the input read one
   local, so there is no render in which they disagree.

3. *Translated messages with descriptions, following the id convention.* Met.
   Both ids are `wallet.send.form` + `assetInput` + a key, both carry a
   `description` naming the state they belong to, and both defaults are prefixed
   `!!!` in source.

4-5. *Compile, lint, i18n, stylelint, jest, suppressions, dependencies.* All met.

The judgement worth naming is the unit source. A decoded asset name is the most
available string on an unresolved token and the wrong one here: `task-001`
established that minter-chosen bytes render only with a marking that separates
them from a published name, and a unit of account is where an unmarked one would
cost the most. The label says `asset1cvm…kvpa` for a token nobody has published
a ticker for, which is uglier and true.

What this task does not do: the label describes the live value, so a resolution
arriving mid-edit still moves both the label and the field's interpretation under
an open form. That is exactly the case `task-040` closes, and it moves both
readings onto one snapshot in a single edit.

Decision: approved
