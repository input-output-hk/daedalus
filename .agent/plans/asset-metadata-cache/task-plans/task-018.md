## Task ID and Title

`task-018` — Drop the assets endpoint and the one-minute poll.

## Why Chosen Now

`task-017` moved the last reader. Everything the endpoint feeds is now derived
from holdings and from the cache, so what is left is a request issued every
minute per wallet whose result nothing looks at.

## Interaction Mode

`agent_execution`.

## Scope

- `source/renderer/app/stores/AssetsStore.ts`: the interval, the refresh, the
  one-shot listener that also triggers it, and the request machinery.
- `source/renderer/app/api/api.ts`: `getAssets`, `_createAssetFromServerData`,
  the in-memory metadata carry-over, and the imports that go with them.
- `source/renderer/app/api/assets/requests/getAssets.ts`: deleted.
- `source/renderer/app/api/assets/types.ts`: `ApiAsset`, `ApiAssets`,
  `GetAssetsRequest`, `GetAssetsResponse` and `StoredAssetMetadata`.

## Non-Goals

- No change on the wallet side. cardano-wallet keeps its
  `--token-metadata-server` wiring at `source/main/index.ts:215-218`; this design
  simply stops exercising it, and `mock-token-metadata-server` stays in the dev
  shell and in the packaging.
- No spinner removal, no decimals resolution order, no logo. Those are
  `task-023`, `task-019` and `task-024`.

## Dependencies

`task-017`.

## Research Consulted

- `asset-metadata-cache-prd.md:40-46` for what the poll costs and why it exists.
- `asset-metadata-cache-prd.md:1356-1366` for the exact deletions.
- The task's own note, which records that a repository-wide grep at the time of
  writing found exactly two references to the endpoint path, both inside request
  modules, and that `getUnknownAsset` has already gone in `task-004`.

## Live Repo Findings Verified For Planning

1. `AssetsStore.ts:13` sets a 60-second interval and `:33` starts it; `:42`
   registers a one-shot listener that also triggers a refresh;
   `:157-170` is the refresh itself.
2. After `task-017`, `_retrieveAssetsRequest` has exactly one caller left,
   `_refreshAssetsData`, and `_refreshAssetsData` has two: the interval and the
   one-shot listener. Removing the three together leaves nothing dangling.
3. `_onAssetSettingsSubmit` still calls `_refreshAssetsData()`. It exists to make
   the endpoint re-read the browser storage the dialog just wrote; the store now
   holds that value itself, so the call goes with the function.
4. `api.ts:783-820` is the only caller of the request module, and
   `api.ts:802` is the only caller of `_createAssetFromServerData`.
   `api.ts:362` declares the carry-over, `:805` passes it and `:3391` writes it.
5. `storedAssetMetadata` exists because the endpoint omits metadata on a later
   poll when the registry is unavailable. There is no later poll.
6. `new Asset(...)` at `api.ts:3394` is the only construction of the domain class
   in that file, so its import goes too. `AssetLocalData` is imported at `:244`
   and used only at `:3375`.
7. A repository-wide grep for `getAssets`, `ApiAsset`, `StoredAssetMetadata` and
   `ASSETS_REFRESH_INTERVAL` over `source`, `tests` and `features` finds them
   only in the files this task edits. `AssetLocalData` is separately used by
   `api/utils/localStorage.ts` and stays.

## Files Expected To Change

- `source/renderer/app/stores/AssetsStore.ts`
- `source/renderer/app/api/api.ts`
- `source/renderer/app/api/assets/requests/getAssets.ts` (deleted)
- `source/renderer/app/api/assets/types.ts`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task

## Implementation Approach

Delete, in one commit, from the consumer end inwards: the interval and its
trigger, the requests they drove, the API method behind those requests, the
mapping that method used, the carry-over that mapping wrote, the request module
and the four response types. Nothing is left behind as unreachable code, and the
compiler is the check that nothing was missed.

## Acceptance Criteria

1. No HTTP request to the wallet's `/assets` path can be issued: the only two
   references to that path are the request module, which is deleted, and the
   method that called it, which is deleted.
2. A repository-wide grep for `getAssets`, `ASSETS_REFRESH_INTERVAL`,
   `ApiAsset`, `GetAssetsResponse` and `storedAssetMetadata` returns nothing
   outside history.
3. No timer in `AssetsStore`.
4. `yarn test:jest`, `yarn lint` and `yarn compile` pass.
5. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The compiler is the principal check. Every deletion here removes an export, so
  a missed reader fails `compile` rather than failing silently at runtime, which
  is why the deletions go in one commit rather than one per layer.
- `grep -rn` over `source`, `tests` and `features` for each of the five names,
  asserted to return nothing.
- The store spec gains a case asserting `setup` registers no interval, driven by
  a fake timer rather than by reading the source, so a timer reintroduced later
  fails a test.
- The existing store cases are the regression guard for the parts that stay: the
  per-token decimal setting still round-trips with the refresh call gone.
- All four Nix checks.

## Risks and Open Questions

- **This is the revert boundary.** Reverting this commit alone would leave the
  renderer with no source of metadata at all, because `task-016` and `task-017`
  have already moved every consumer onto the cache. Phases 2 and 3 roll back
  together from here on. Recorded in the task graph and repeated here.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-018.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-018-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-018-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The wallet's assets endpoint is no longer called, and no timer in the renderer
asks for asset metadata.

## Final Outcome

Complete.

## Self-Review

The only way this goes wrong quietly is a consumer that reads one of the deleted
types through an `any`, which the compiler would not catch. The grep over three
directories is what covers that, and it is cheap enough to run rather than
reason about.
