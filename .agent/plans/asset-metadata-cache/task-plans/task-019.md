## Task ID and Title

`task-019` — Resolve decimal places by provenance.

## Why Chosen Now

`task-016` put the cached row in the renderer with its `verified` flag intact,
and `task-001` established the shape a provenance-ordered resolution takes in
this codebase. Until this task lands, a verified six-decimal token still
transacts as a raw integer, because the registry's value is filed under
`recommendedDecimals` and only a user opening a dialog ever promotes it.

This is the first task of the wrong-amount-signed phase, and the three that
follow it on the send path (`task-021`, `task-039`, `task-040`) all describe
their behaviour in terms of the value this one produces.

## Interaction Mode

`agent_execution`.

## Scope

- One function that answers "what decimal places apply to this subject", and one
  caller of it.
- The verification verdict carried from the cached row onto the merged row every
  surface renders, because three consumers downstream need to distinguish a
  policy-bound value from an unattested one.

## Non-Goals

- No copy change anywhere. The weaker warning wording is `task-020`, the settings
  advisory is `task-022`, the migration notice is `task-021` and the amount
  field's unit label is `task-039`.
- No change to how a user's explicit choice is stored or read.
  `api/utils/localStorage.ts:313-327` is untouched.
- No snapshot and no mid-edit guard. That is `task-040`, which depends on this.

## Dependencies

`task-001`, `task-016`.

## Research Consulted

- `asset-metadata-cache-prd.md:1132-1141` for the resolution order, and
  `:1143-1177` for the corpus measurement behind it: of 7,977 registry subjects,
  850 (10.7%) publish nonzero decimals that verify, and those are the only
  subjects whose displayed and entered amounts change.
- `asset-metadata-cache-prd.md:1178-1192` for why the risk of this change is on
  the input path and not on the balance.
- `task-001`'s `utils/assetName.ts` as the precedent for a provenance-ordered
  resolver: one pure function, an enum for the rung that answered, and `null`
  when none did.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md` — not needed here, no message changes.
- The trust map in `CLAUDE.md`: MobX strict mode is live, so the store's
  mutations stay inside actions.

## Live Repo Findings Verified For Planning

1. **The security gate this task is conditioned on was met.**
   `task-009-impl-review.md:16-32` records the corpus run at registry commit
   `363982b9`: 4,579 of 4,579 policy-bearing subjects bound to their own subject,
   19,209 of 19,209 attestations satisfied their script, and 19,208 of 19,209
   signatures verified, with the single failure traced to the registry's own
   record. Applying a verified value automatically is therefore gated on a
   verifier that has been run over the whole corpus.
2. **`verified` is the verdict for `decimals` specifically**, not a row-wide
   quality score: `assetMetadataResolver.ts:86-106` computes it from
   `verifyRegistryProperty(subject, policy, 'decimals', property)` and returns
   `false` when the entry publishes no `decimals` property at all.
3. **The store is the only place the two inputs meet.** `AssetsStore.ts:216-232`
   builds every `Asset` the renderer sees; `decimals` comes from
   `this._localDecimals` and `recommendedDecimals` from `entry.decimals`. There
   is no second construction site: `api.ts`'s `_createAssetFromServerData` went
   in `task-018`, and `grep -rn "new Asset(" source` finds `AssetsStore.ts:223`
   and `:243` only.
4. **`entry.verified` reaches the renderer already.**
   `common/types/asset-metadata.types.ts:65` declares it on `AssetMetadataEntry`
   and `main/ipc/assetMetadataChannel.ts:82` fills it from the row. Nothing in
   the renderer reads it yet.
5. **The merge helper drops any field it does not name.**
   `utils/assets.ts:60-70` destructures exactly `fingerprint`, `metadata`,
   `decimals` and `recommendedDecimals` from the lookup, so a new field on the
   domain object does not reach `AssetToken` without an edit here. This is why
   `utils/assets.ts` is in the task's target paths.
6. **Three components already branch on `typeof decimals === 'number'`** to pick
   between the "not using" and "available" warning copy: `Asset.tsx:306-314`,
   `WalletTokenFooter.tsx:35-42` and `AssetSettingsDialog.tsx:164-168`. Under the
   new resolution these still pick correctly, because the case where a verified
   value is auto-applied is also the case where there is no disagreement to
   report. Verified by walking each branch rather than assumed.
7. **`utils/formatters.ts` needs no change.** It is in the task's target paths,
   but `formattedTokenDecimals` at `:101-111` and `formattedTokenWalletAmount` at
   `:53-99` both take `decimals` as a parameter and neither resolves anything.
   The resolution happens before they are called.
8. **Nothing collides.** `grep -rn "assetDecimals\|resolveAssetDecimals" source
   tests` returns nothing.

## Files Expected To Change

- `source/renderer/app/utils/assetDecimals.ts` — new.
- `source/renderer/app/utils/assetDecimals.spec.ts` — new.
- `source/renderer/app/api/assets/types.ts` — one field on `Asset`.
- `source/renderer/app/domains/Asset.ts` — the same field, observable, and in the
  `update` pick list.
- `source/renderer/app/utils/assets.ts` — carry it through the merge.
- `source/renderer/app/stores/AssetsStore.ts` — call the resolver.
- `source/renderer/app/stores/AssetsStore.spec.ts` — store-level cases.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — status.
- the three review-log files for this task.

A deviation from the task's `targetPaths`, recorded before the work: the rule
goes in a new `utils/assetDecimals.ts` rather than into `utils/assets.ts`.
`utils/assets.ts` is a grab-bag of token list helpers that imports `Wallet` and
`BigNumber`; a rule the send path depends on is easier to find, and cheaper to
test, in a file of its own. `task-001` set this precedent with `utils/assetName.ts`.
`utils/formatters.ts` is named in `targetPaths` and is not changed, for the
reason in finding 7.

## Implementation Approach

1. **`resolveAssetDecimals`**, pure, in `utils/assetDecimals.ts`:

   ```ts
   resolveAssetDecimals({
     userDecimals,            // the explicit per-subject setting
     registryDecimals,        // the published value, verified or not
     registryDecimalsVerified // the verdict for that property
   }): { decimals: number | null; provenance: AssetDecimalsProvenance }
   ```

   Highest rung first: a numeric `userDecimals`; then `registryDecimals` when
   `registryDecimalsVerified === true`; then `{ decimals: null, provenance: None }`.

   The verification test is written `=== true` rather than as a truthiness check.
   Under `strict: false` the field can arrive `undefined` from a row built before
   this field existed, and `!verified` would then read as "unverified", which is
   the safe direction, while a truthy test on a string would not be. `=== true`
   states the one value that unlocks formatting.

2. **`recommendedDecimalsVerified` on the asset**, defaulting to `false` for a
   subject with no cached row. It is the verdict for `recommendedDecimals`, which
   is why it is named after it.

3. **One caller.** `AssetsStore._assetFor` resolves once and passes the result as
   `decimals`. `recommendedDecimals` keeps its present meaning exactly: the
   registry's published value whether or not it verified, which is what the
   settings dialog offers and what the disagreement helper compares against.

4. **The merge helper carries the new field**, so every `AssetToken` a component
   receives has it.

## Acceptance Criteria

1. All four combinations asserted: user setting present and different from a
   verified value (setting wins); no setting with a verified value (applied); no
   setting with an unverified value (raw units, and the value still offered as
   recommended); neither present (raw units).
2. One function is the only reader of the rule: `grep` finds a single call site.
3. An unverified value never reaches `decimals`, asserted at the store level and
   not only on the pure function.
4. `compile`, `lint`, `i18n` and `jest` green from `nix build`.
5. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- `assetDecimals.spec.ts` drives the four combinations from the task graph, plus
  the two spellings of an absent setting (`undefined` and `null`), plus a zero
  user setting, which is the case a truthiness check would get wrong: a user who
  has explicitly chosen zero decimal places must not have a verified six silently
  applied over the top.
- `AssetsStore.spec.ts` asserts the same rule through the store, because the
  property that matters is what a component reads off the merged row, not what a
  pure function returns. Three cases: verified applied, unverified not applied
  with the recommended value still present, and a user setting winning over a
  verified value.
- The existing store cases are the regression guard. The fixture entry carries
  `decimals: 6, verified: true`, so any case that asserted on `decimals` before
  this change and still passes is asserting the new behaviour.
- All four Nix checks.

## Risks and Open Questions

- **A user who has never opened the dialog now sees formatted amounts for a
  verified token.** That is the intended change and the reason `task-021`
  exists. It moves no balance: the ledger quantity is an integer and decimal
  places are presentation.
- **The send field's denomination changes with it**, which is the whole of the
  risk. This task does not mitigate that; `task-039` labels the field and
  `task-040` snapshots the value and blocks a mid-edit change. Neither can be
  written until the value they guard exists, which is why they follow rather
  than precede.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-019.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-019-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-019-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

Decimal places resolve by provenance, and an unverified published value formats
nothing.

## Final Outcome

Complete.

## Self-Review

The quiet failure mode is a second reader of the rule appearing later, in a
component that has both `decimals` and `recommendedDecimals` in scope and
decides for itself which to use. The grep in criterion 2 catches it today; what
keeps it caught is that `decimals` on the merged row is now the answer, so a
component has no reason to ask again.
