Implementation: Iteration 1
Timestamp: 2026-09-15T08:55:00Z

Changes made:
- `source/renderer/app/utils/assetDecimals.ts`: new. `AssetDecimalsProvenance`,
  `ResolvedAssetDecimals` and `resolveAssetDecimals`.
- `source/renderer/app/utils/assetDecimals.spec.ts`: new. Nine cases.
- `source/renderer/app/api/assets/types.ts`: `recommendedDecimalsVerified` on
  `Asset`, which `AssetToken` inherits.
- `source/renderer/app/domains/Asset.ts`: the same field, observable, and in the
  `update` pick list.
- `source/renderer/app/utils/assets.ts`: the merge helper carries it.
- `source/renderer/app/stores/AssetsStore.ts`: `_assetFor` resolves once;
  `_unresolvedAsset` carries `false`.
- `source/renderer/app/stores/AssetsStore.spec.ts`: five cases.

Files touched:
- the six source files above and the two specs
- `.agent/plans/asset-metadata-cache/task-plans/task-019.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-019-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-019-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One detail decided during implementation:

`decimals` for a subject with no user setting and no verified value is now
`null` where it was `undefined`. Every reader was checked rather than assumed:
`AssetSettingsDialog.tsx:91` and `:138` test `typeof savedDecimals === 'number'`,
`helpers.ts:10-11` does the same, `AssetInput.tsx:90` tests `decimals != null`,
and `formattedTokenDecimals` at `formatters.ts:105` falls back on `||`. All four
treat the two spellings alike, which is why the change is invisible to them and
is stated here rather than left to be discovered.

Verification run:

- `jest source/renderer/app/utils/assetDecimals source/renderer/app/stores/AssetsStore --coverage=false`
  — 33 passed, of which 14 are new.
- The rule is driven at two levels. The pure function takes the four
  combinations from the task graph plus the cases that separate a correct
  implementation from a truthy one: a user setting of `0` against a verified `6`,
  a verified value of `0`, a `verified: true` row with no number to format with,
  and both spellings of an absent setting asserted to give the same answer.
- The store cases assert the same rule through what a component actually reads.
  `never applies an unverified registry value` is the one that matters: the row
  carries `decimals: null` and `recommendedDecimals: 6` at the same time, so a
  regression that promoted the unverified value would fail on the first
  assertion rather than passing because some number was present.
- The last store case runs the row through `getAssetTokenFromToken`, because a
  field the store sets and the merge helper drops would otherwise pass every
  store-level assertion and reach no component.
- Criterion 2, one reader: `grep -rn "resolveAssetDecimals" source` returns the
  declaration in `utils/assetDecimals.ts`, the import and the single call in
  `AssetsStore.ts`, and the spec.

Checks, all four through Nix with every new file staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `pgbfqvaq7pmvfxvwn2z8sgvwbw96c4yq-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `1bwjm6d04f7m3sjz9d3lbda6wigx8mph-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `akdw4bdrms0lbxsf3ba2n6xg5f2m7dc0-daedalus-i18n.drv`, exit 0. No message
  changed, so this is a regression guard rather than a result.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 85 suites passed, 1266
  tests with 1263 passed and 3 skipped, exit 0. The previous state of this branch
  was 84 suites and 1252 tests, so one suite and fourteen tests were added and
  nothing else moved.

`nix fmt` was run and changed two files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None. The new-file placement and the untouched `utils/formatters.ts` were both
  recorded in the plan before the work.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T09:00:00Z

Acceptance criteria, each against the evidence:

1. *All four combinations asserted.* Met, and five more that the graph's four do
   not cover. The user-setting-of-zero case is the one worth naming: it is the
   difference between reading the rule as "is there a setting" and "is the
   setting truthy", and the second reading silently overrides a deliberate
   choice with the issuer's.

2. *One function is the only reader of the rule.* Met by grep. The resolution
   happens where the two inputs meet and nowhere else, which is what stops the
   display and the send path from disagreeing.

3. *An unverified value never reaches `decimals`.* Met at the store level, with
   the recommended value asserted present in the same case so the test cannot
   pass by losing the value altogether.

4-5. *Compile, lint, i18n, jest, suppressions, dependencies.* All met.

The judgement worth naming is `recommendedDecimalsVerified` rather than
`decimalsVerified`. The field is the verdict for the registry's published value,
not for whatever ended up applied, and those differ exactly when a user setting
wins. A name that blurred them would be read as "the number on screen is
verified", which for a user-set value it is not.

What this does not do, stated so it is not mistaken for finished: the send
field's denomination now changes when a resolution arrives, and nothing yet
guards an open form against that. `task-039` labels the field and `task-040`
snapshots the value and blocks the change. Neither could be written before the
value they guard existed.

Decision: approved
