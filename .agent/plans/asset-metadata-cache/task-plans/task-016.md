## Task ID and Title

`task-016` — Rewire `AssetsStore` to read the cache.

## Why Chosen Now

Every dependency is in: the name resolution from `task-001`, the merge helper
from `task-002`, the CIP-14 fingerprint from `task-005`, the main handler from
`task-014` and the renderer client from `task-015`. This is the commit where the
cache starts feeding the screen.

## Interaction Mode

`agent_execution`.

## Scope

- `source/renderer/app/stores/AssetsStore.ts`: `details` and `getAsset` become
  reads of an observable map fed by the request and update channels; a reaction
  derives the subject list from holdings and rendered transactions; the
  per-token decimal setting moves into the store's own observable.
- `source/renderer/app/stores/AssetsStore.spec.ts`, new.

## Non-Goals

- **The endpoint stays.** `all`, `assetsRequests`, `_createWalletTokensRequest`,
  `_retrieveAssetsRequest`, `_refreshAssetsData` and the poll are all still here
  when this commit lands. See Implementation Approach for why.
- No change to the resolution order for decimal places. `decimals` still comes
  only from the per-user setting and `recommendedDecimals` only from the
  registry, exactly as `api.ts:3386-3388` has it today. `task-019` changes that,
  and changing it here would put a denomination change inside a commit that is
  about where rows come from.
- No logo. `task-024` owns the image channel's consumer.
- No spinner removal. `task-023` owns `isLoadingAssets`.

## Dependencies

`task-001`, `task-002`, `task-005`, `task-014`, `task-015`, all complete.

## Research Consulted

- `asset-metadata-cache-prd.md:1236-1275` for the cold-cache rendering rules and
  the merge helper's contract.
- `asset-metadata-cache-prd.md:1276-1295` for the three consumers that would
  otherwise lose historically held assets.
- `task-002-impl-review.md`, the findings carried forward: `searchAssets` tests a
  regex against a `fingerprint` that is now reachable as `undefined`, which
  coerces to the string `"undefined"`, so a search for `und` matches every
  unresolved row. It is recorded there as closed by this task.

## Docs, Workflows, and Skills Consulted

- `.agent/system/state-management.md` for the store conventions, against the
  trust map: its `Store` constructor signature has never existed, so the shape
  was taken from `source/renderer/app/stores/lib/Store.ts:8-39` instead.
- `source/renderer/app/stores/BackendStore.ts:78-105` for subscribing to push
  channels from a store and for labelled `runInAction`.

## Live Repo Findings Verified For Planning

1. `details` at `AssetsStore.ts:63-70` and `getAsset` at `:72-73` are the whole
   public surface for metadata. Callers are all point lookups: `:98`,
   `utils/assets.ts:140` and `utils/transactionsCsvGenerator.ts:190`.
2. `all` at `:51-61` reads `_retrieveAssetsRequest`, which creates a request
   through `_createWalletTokensRequest` at `:180-186`. Removing the request
   machinery in this commit would break `all`, whose two readers do not move
   until `task-017`; `_refreshAssetsData` at `:157-170` also calls it and does
   not go until `task-018`. So the machinery stays and the deletion is
   `task-018`'s, which is what the task note means by sequencing the commits.
3. `configure({ enforceActions: 'observed' })` is live at
   `source/renderer/app/index.tsx:35`, so a post-`await` mutation throws rather
   than warns.
4. `Store.registerReactions` (`stores/lib/Store.ts:19-23`) wraps each function in
   `Reaction`, which is `autorun` (`stores/lib/Reaction.ts:12`). A reaction that
   reads holdings and transactions and writes only non-observed state cannot
   loop.
5. `TransactionsStore.all` (`TransactionsStore.ts:157-169`) returns the active
   wallet's transactions, each carrying `assets: Tokens`
   (`domains/WalletTransaction.ts:41`). `_getTransactionsAllRequest`
   (`:435-443`) does not mutate an observable, so reading it from a reaction is
   safe.
6. `api.localStorage.getAssetsLocalData()`
   (`api/utils/localStorage.ts:309-310`) returns the whole per-asset store keyed
   by `policyId + assetName`, which is the same key the cache uses for a subject.
7. `Asset` (`domains/Asset.ts:6-51`) takes `fingerprint` as a string and
   `metadata` as `AssetMetadata`, whose `name` and `description` are not
   optional. A row built from a cache entry therefore supplies both, with the
   empty string where the registry published nothing, and the empty string is
   falsy so `resolveAssetName` (`utils/assetName.ts:41-53`) still falls through
   to the decoded asset name.
8. `assetFingerprint` (`utils/assetFingerprint.ts:43-68`) throws on a policy id
   that is not 28 bytes or an asset name over 32, so every call is guarded and a
   failure means the row has no fingerprint rather than that the render fails.

## Files Expected To Change

- `source/renderer/app/stores/AssetsStore.ts`
- `source/renderer/app/stores/AssetsStore.spec.ts` (new)
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task

## Implementation Approach

**Two observable maps and a memo.** `_metadata` holds one cache entry per
subject, fed by the request response and by the update push. `_localDecimals`
holds the per-token setting the user chose. Fingerprints are memoised in a plain
`Map`, because a fingerprint is a pure function of identity and computing a
blake2b digest per row per paint is not free.

**`getAsset` answers for any subject it can name.** A cache entry produces a full
row. A subject with no entry produces a row carrying identity and the locally
computed fingerprint and nothing else, which is what the token list, the send
form and the CSV export need on a cold cache. It returns nothing only when the
identity cannot produce a fingerprint at all.

This is a deliberate departure from the task graph's test case, which reads
"getAsset returns undefined for an unresolved subject". Taken literally it
contradicts the same task's note to attach the locally computed fingerprint, and
it leaves `searchAssets` matching every unresolved row on a search for `und`,
which `task-002`'s review records as closed here. The property the test case is
really about, that a caller renders when metadata has not arrived, is asserted
directly: the row comes back with `metadata` undefined and the caller renders.
The undefined case survives as the malformed-identity case, which is the only
state in which this store genuinely knows nothing about a subject.

**The subject list comes from two places already in the renderer.** Holdings from
`wallets.active.assets.total` and the assets of the active wallet's transactions
from `transactions.all`. A reaction collects both, subtracts what has already
been asked for this session, and sends the remainder. Subjects the response
reports unresolved are not re-asked: the main process scheduled them and pushes
them when they resolve.

**The per-token decimal setting stops travelling through the endpoint.** Today
the poll reads browser storage and stamps `decimals` onto the asset it builds.
The store now reads the same storage once at startup, holds it, and writes to it
when the settings dialog submits. Nothing about which value formats an amount
changes in this commit.

**Every post-`await` mutation is inside a labelled `runInAction`**, and the push
handler is an `@action`, because `enforceActions: 'observed'` is live.

## Acceptance Criteria

1. `getAsset` returns a cached row with no request made.
2. `getAsset` returns a row carrying the locally computed fingerprint for a
   subject with no cached entry, and the caller renders.
3. `getAsset` returns nothing only when the identity is malformed.
4. A pushed update is visible to an observer without a further request.
5. `searchAssets('und', ...)` does not match an unresolved row.
6. The reaction asks once per subject, with holdings and transaction assets
   merged and deduplicated.
7. The per-token decimal setting round-trips through the store and reaches
   browser storage.
8. No MobX strict-mode error under `enforceActions: 'observed'` while reading and
   merging inside an observed context.
9. `yarn test:jest`, `yarn lint` and `yarn compile` pass.
10. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
    `yarn.lock` unchanged.

## Verification Plan

A colocated `AssetsStore.spec.ts` constructing the store directly, as
`BackendStore.spec.ts:11-15` does, with the renderer channel module mocked so no
IPC is touched, and with `configure({ enforceActions: 'observed' })` set in the
suite so a mutation outside an action fails the case rather than passing quietly.

- A merged entry is readable through `getAsset` and `details`, with `ticker`,
  `name` and `recommendedDecimals` from the entry.
- An unresolved subject returns a row whose `fingerprint` is the CIP-14 value for
  that identity, asserted against `assetFingerprint` rather than against a
  literal, with `metadata` undefined.
- A malformed identity returns nothing rather than throwing.
- `searchAssets('und', [row])` over an unresolved row returns no matches, and the
  same search over a row whose name contains it does match, so the case cannot
  pass by matching nothing at all.
- A push is observed: an `autorun` reading `getAsset(...)` sees the ticker appear
  without a second request.
- The reaction: holdings and transaction assets are merged and deduplicated, one
  request; run again with the same inputs and nothing further is requested; a new
  holding is requested on its own.
- The settings submit writes the value to browser storage and the next
  `getAsset` carries it, including for a subject with no cached entry.
- Reads inside an `autorun` with strict mode on complete without throwing.

Then all four Nix checks.

## Risks and Open Questions

- **`details` constructs domain objects on each recomputation.** It recomputes
  only when an entry or a setting changes, and the fingerprint, which is the
  expensive part, is memoised. Stated so it is not mistaken for an oversight.
- **A subject that fails to resolve in the main process is not re-asked this
  session.** The retry window lives in the main process and the next launch asks
  again. This is the intended division and the alternative, re-asking from a
  reaction, is the polling this plan exists to remove.
- **The `getAsset` departure from the graph's test case** is argued above and is
  the one item a reviewer should weigh.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-016.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-016-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-016-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

`details` and `getAsset` keep their signatures and now answer from the cache,
with a locally computed fingerprint for anything the cache has not resolved.

## Final Outcome

Complete.

## Self-Review

The riskiest thing here is what `getAsset` returns for an unresolved subject,
because every caller destructures the result. The spec drives the unresolved
case through the same helper the three surfaces use rather than asserting on the
store in isolation.
