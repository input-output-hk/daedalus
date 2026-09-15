## Task ID and Title

`task-017` — Move the two enumerating call sites to the holdings-driven form.

## Why Chosen Now

`task-016` put the cache behind `getAsset`. Two surfaces still build their token
lists by mapping over the endpoint's list instead, so they are the only reason
`AssetsStore.all` is still there, and `all` is the only reason the endpoint
cannot be removed.

## Interaction Mode

`agent_execution`.

## Scope

- `source/renderer/app/containers/wallet/WalletTokensPage.tsx` and
  `WalletSummaryPage.tsx`: from `getAssetTokens(all, walletTokens)` to
  `getNonZeroAssetTokens(walletTokens, getAsset)`.
- `source/renderer/app/containers/wallet/WalletSendPage.tsx`: the third reader of
  `all`, which resolves the clicked token.
- `source/renderer/app/utils/assets.ts`: `getAssetTokens`, `getAssetToken`,
  `getToken` and `getZeroToken` lose their last callers and go; `searchAssets`
  stops coercing undefined fields.
- `source/renderer/app/stores/AssetsStore.ts`: `all` goes.
- `source/renderer/app/utils/assets.spec.ts`: cases for the search fix.

## Non-Goals

- The endpoint, the poll and the request machinery stay. `_refreshAssetsData`
  still calls `_retrieveAssetsRequest`, and `task-018` removes all of it
  together.
- No spinner removal. `isLoadingAssets` is now permanently false at all four
  sites and `task-023` owns taking it out.

## Dependencies

`task-016`.

## Research Consulted

- `asset-metadata-cache-prd.md:1276-1295` for the three consumers that lose
  historically held assets, and specifically why `all` must be derived from
  holdings rather than from the cache.
- `asset-metadata-cache-prd.md:1386-1394` for the two helpers' ordering
  difference.
- `task-016-impl-review.md`, the correction: filling the fingerprint removes one
  of four sources of the `searchAssets` coercion, and the other three are fixed
  here because they live in this file.

## Live Repo Findings Verified For Planning

1. `AssetsStore.all` has exactly three readers:
   `WalletTokensPage.tsx:20`, `WalletSummaryPage.tsx:96` and
   `WalletSendPage.tsx:134`. Verified by grep over `source/renderer`.
2. `getNonZeroAssetTokens` at `utils/assets.ts:151-157` is already used by
   `WalletSendPage.tsx:144`,
   `containers/wallet/dialogs/send-confirmation/SendConfirmation.container.tsx`
   and `components/wallet/transactions/WalletTransactionsList.tsx:240`.
3. The two helpers differ in ordering, not in content. `getNonZeroAssetTokens`
   sorts by fingerprint ascending; `getAssetTokens` does not sort.
   `WalletSummaryPage.tsx:123-125` re-sorts by token name afterwards, so nothing
   changes there. `WalletTokensPage` does not sort, so its list gains a stable
   order in place of the endpoint's arbitrary one.
4. `getAssetTokens` is the only caller of `getAssetToken` and of `getToken`, and
   `getToken` is the only caller of `getZeroToken`, so all four go together.
5. `WalletSendPage.getAssetByUniqueId` at `:115-117` takes `Array<Asset>` and is
   called at `:137` with the cache-derived list. The holdings-derived list is
   already computed in the same render at `:144`, seven lines later, so the fix
   is an ordering change rather than a new derivation.
6. `searchAssets` at `utils/assets.ts:283-300` builds a list of eight fields and
   calls `regex.test(item)` on each. `RegExp.prototype.test` coerces, so an
   undefined field is tested as the string `"undefined"`. On an unresolved row
   four of the eight are undefined, and one of them, `metadata`, is an object
   that coerces to `"[object Object]"` when it is present. Measured in
   `task-016`: a row with a filled fingerprint and a metadata object still
   matches a search for `und`, through `ticker`.

## Files Expected To Change

- `source/renderer/app/containers/wallet/WalletTokensPage.tsx`
- `source/renderer/app/containers/wallet/WalletSummaryPage.tsx`
- `source/renderer/app/containers/wallet/WalletSendPage.tsx`
- `source/renderer/app/utils/assets.ts`
- `source/renderer/app/utils/assets.spec.ts`
- `source/renderer/app/stores/AssetsStore.ts`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task

## Implementation Approach

Both list surfaces switch to the holdings-driven helper, which builds a row from
the token the wallet reports and overlays whatever the cache has. The send page
resolves the clicked token against that same list rather than against the cache,
so a token with no cached row still opens its send form with the asset selected.

`searchAssets` tests only the fields that are strings. The object field is
dropped from the list entirely: it was contributing `"[object Object]"`, which
matches a search for `obj` and nothing a user would mean. Its useful contents,
the name, the ticker and the description, are already in the list individually.

`AssetsStore.all` goes with its last reader.

## Acceptance Criteria

1. The token list and the wallet summary render every held token on a cold
   cache, not only on a warm one.
2. The send form resolves the clicked token from holdings, so a token with no
   cached row still opens with its asset selected.
3. A three-letter search matches nothing through a field that is absent, and
   still matches what is present.
4. `getAssetTokens`, `getAssetToken`, `getToken`, `getZeroToken` and
   `AssetsStore.all` are gone, with no reference left.
5. CSV export resolves a fingerprint for an asset the wallet held historically,
   so `'unknown fingerprint'` is unreachable for any subject with a well-formed
   identity.
6. `yarn test:jest`, `yarn lint` and `yarn compile` pass.
7. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- `utils/assets.spec.ts` gains a group for `searchAssets`: an unresolved row is
  not matched by `und`, by `obj`, or by `ect`, and the same three-letter search
  that matches a published name still matches. The negative cases are driven
  against a row built by `getAssetTokenFromToken` with a lookup that misses,
  which is exactly the shape a cold cache produces, rather than against a
  hand-made object.
- The store's existing cases cover what the two containers now depend on: a row
  exists for a held subject with no cached entry, and it carries a fingerprint.
  Criterion 5 follows from that and from `transactionsCsvGenerator.ts:190`
  reading `assetData?.fingerprint`, and is asserted as a unit case rather than by
  generating a file: the fallback is reachable only when `getAsset` returns
  nothing, which now happens only for a malformed identity.
- A repository-wide grep for each deleted name returns only history.
- All four Nix checks.

## Risks and Open Questions

- **`WalletSendPage.tsx` is not in the task's declared `targetPaths`.** It is the
  third reader of `all` and the task's own acceptance criteria name it, so it
  cannot be left out. Recorded here before the work.
- **`WalletTokensPage`'s list order changes** from the endpoint's arbitrary order
  to fingerprint ascending. This is a user-visible change, argued in the PRD as
  an improvement, and it is stated here so it is not discovered as a surprise.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-017.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-017-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-017-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

Nothing reads the endpoint's list. Every surface builds its rows from what the
wallet holds and overlays what the cache knows.

## Final Outcome

Complete.

## Self-Review

The change is small and the risk is concentrated in one line: the send page's
selected asset. If it resolved against the cache it would silently fail for
exactly the assets this whole plan is about, and the spec case for it is the one
worth keeping.
