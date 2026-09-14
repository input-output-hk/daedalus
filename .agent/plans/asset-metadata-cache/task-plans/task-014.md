## Task ID and Title

`task-014` — Main-side handler and its registration.

## Why Chosen Now

`task-013` declared the channels and `task-010` finished the resolver, so both
halves of this task exist. It is the last thing between the cache and a
consumer: until the handler is registered, phase 2's five modules have no
caller at all.

## Interaction Mode

`agent_execution`.

## Scope

- `source/main/ipc/assetMetadataChannel.ts`, new: the three `MainIpcChannel`
  instances, the row-to-entry mapping, the two request handlers, the push, and
  one registration function.
- `source/main/ipc/index.ts`: one line beside `handleGovernanceAnchorRequests()`.
- `source/main/assets/assetMetadataDb.ts`: `readImageSubjects`, the existence
  query behind `hasImage`.
- `source/main/ipc/assetMetadataChannel.realfs.spec.ts`, new.
- `source/main/assets/assetMetadataDb.realfs.spec.ts`: cases for the new
  accessor.

## Non-Goals

- No renderer code. `task-015` owns the clients and `task-016` the store.
- No `ipcMain.handle` and no `ipcRenderer.invoke`. Neither appears anywhere
  under `source/`; the mechanism is `IpcChannel` deriving three wire names from
  one base (`source/common/ipc/lib/IpcChannel.ts:91-93`).
- No new resolution policy. The refresh window, the retry window, the claim and
  the verification all belong to `task-010` and are not re-decided here.

## Dependencies

`task-010` (the resolver) and `task-013` (the channels). Both complete at
`df625ad89`.

## Research Consulted

- `asset-metadata-cache-prd.md:606-626` for what the request channel answers and
  what `unresolved` is for.
- `asset-metadata-cache-prd.md:628-641` for the correlation requirement.
- `task-010-impl-review.md`, the closing note: the handler pushes from the
  `onResolved` callback rather than awaiting `pending()`, because awaiting would
  put the network in front of a render, which is the property `readCached`
  exists to preserve.

## Docs, Workflows, and Skills Consulted

- `.agent/workflows/ipc.md`, read and not followed, for the reason in Non-Goals.
- `source/main/ipc/governanceAnchorChannel.ts:11-34` for the handler shape and
  `source/main/ipc/nodePushChannel.ts:16-19` for the push direction.
- `source/main/ipc/governanceAnchorChannel.spec.ts:6-43` for how a main-side
  handler is tested without Electron: mock `./lib/MainIpcChannel`, register, and
  take the handler out of the mock's first call.

## Live Repo Findings Verified For Planning

1. `source/main/ipc/index.ts:32` exports one default function taking the window,
   called from `source/main/windows/main.ts:72` inside `createMainWindow`, which
   `source/main/index.ts:275` calls once. So registration happens once per
   process under the current call graph.
2. Registering twice would break correlation rather than merely duplicating
   work. Two `ipcMain.on` listeners answer one request with two responses; the
   second response is consumed by another in-flight request's one-shot listener,
   whose own response then has no listener left. A request that never resolves is
   worse than a duplicate, so registration is made idempotent.
3. `AssetMetadataResolver.request` (`assetMetadataResolver.ts:194-211`) reads
   from disk synchronously, claims what is due and queues the fetch on
   `this._pending`. It returns rows without awaiting anything, which is what lets
   the handler answer without the network.
4. `AssetMetadataDatabase` has no existence query for images. `readImage`
   (`assetMetadataDb.ts:324-347`) returns the blob, and calling it per subject to
   fill a boolean would read every logo the cache holds on every bulk read.
   `subject` is the primary key of `asset_image`, so
   `SELECT subject FROM asset_image WHERE subject IN (...)` is answered from the
   index without touching a row.
5. `openAssetMetadataResolver` and `openAssetImageStore` each default to
   `openAssetMetadataDatabase()` (`assetMetadataResolver.ts:170`,
   `assetImageStore.ts:137`), so letting both default would open two handles on
   one file from one process. One database is opened here and passed to both.
6. `AssetImageStore.fetch` (`assetImageStore.ts:157-175`) reads the database
   first and returns a stored row without a request, shares an in-flight promise
   per subject, and remembers a subject the registry answered without a logo. So
   the image handler needs no caching of its own.
7. `asset_image.subject` references `asset_metadata (subject)`
   (`assetMetadataDb.ts:57-58`), so an image for a subject with no metadata row
   is refused by the engine and `writeImage` returns false. An image request for
   an unresolved subject therefore answers `absent` rather than storing anything.
8. `assetMetadataDb.ts:142-158` stores `metadata` as a TEXT column holding JSON,
   while `AssetMetadataEntry.metadata` is an object. The mapping parses, and a
   value that does not parse to an object becomes null rather than throwing
   inside a handler.

## Files Expected To Change

- `source/main/ipc/assetMetadataChannel.ts` (new)
- `source/main/ipc/assetMetadataChannel.realfs.spec.ts` (new)
- `source/main/ipc/index.ts`
- `source/main/assets/assetMetadataDb.ts`
- `source/main/assets/assetMetadataDb.realfs.spec.ts`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task

## Implementation Approach

**The request handler answers from what the cache holds now.** It calls
`resolver.request(subjects)`, which reads the database and schedules whatever is
due, and returns immediately. Nothing in the handler awaits the registry. A
subject with a row is an entry; a subject without one is an `unresolved` member
carrying the state `asset_resolution` holds for it, or `pending` when there is no
row there either, which is true by the time the response is sent because this
request has just scheduled it.

**The handler never throws.** Every path is wrapped, and a failure answers with
empty lists and the request's own id. `IpcChannel.onRequest` converts a throw
into an `isOk: false` response, which the renderer client would see as a
rejection it cannot attribute to a request, so refusing to throw is what keeps
the correlation scheme sound.

**The push comes from `onResolved`.** The resolver hands over the rows that
changed, and those rows go to the renderer on the update channel. The handler
never awaits `pending()`.

**Subjects are sanitised before they reach the database.** Non-strings are
dropped and duplicates collapse, so a single subject asked for thirty times by
thirty components costs one bound parameter rather than thirty.

**One database, two consumers.** The database is opened once here and handed to
both the resolver and the image store, rather than letting each open its own.

**The image handler is the one place a request may wait.** `fetch` answers from
disk when the row is there and otherwise makes one bounded request. That is the
reason the channel is per subject and separate: a logo that takes ten seconds
delays a picture, never a row, a name or an amount.

## Acceptance Criteria

1. `yarn compile` and `yarn lint` pass.
2. A renderer request with an empty cache returns an empty entry list rather than
   an error, and names every requested subject under `unresolved` as `pending`.
3. The response echoes the request's `requestId` exactly.
4. A request for a cached subject answers without the transport being called at
   all, asserted with a transport whose promise never settles.
5. `hasImage` is true only for a subject with an image row, and answering it
   reads no blob.
6. A resolved row reaches the renderer on the update channel without a second
   request.
7. The image handler answers `present` with bytes and a media type for a stored
   image, `absent` for a subject with none, and `absent` rather than a rejection
   when the transport fails.
8. Registering twice registers one set of handlers.
9. `yarn test:jest` passes.
10. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
    `yarn.lock` unchanged.

## Verification Plan

A colocated `realfs` spec against a real database file in a `mkdtemp` directory
and a stubbed transport, with `./lib/MainIpcChannel` mocked so no Electron
object is touched, following `governanceAnchorChannel.spec.ts`.

Cases, each named for the property it holds:

- Cold cache: entries empty, every requested subject `pending`, no throw.
- The echo: a response carries the id the request carried, and a second request
  with a different id gets its own back.
- Never waits: a transport whose `post` returns a promise that never settles;
  the handler still resolves with the cached rows. This is the criterion the
  whole design turns on, so it is driven rather than argued.
- `unresolved` states: a subject with an `unregistered` resolution row, one with
  `failed`, and one with no row at all produce three different states.
- A subject with a metadata row never appears in `unresolved`.
- `hasImage`: true for a subject with an image row, false for one without, in
  the same response.
- Mapping: `verified`, `source`, `decimals` and the parsed `metadata` object
  survive; a metadata column holding text that is not JSON maps to null.
- Duplicates: the same subject three times yields one entry.
- The push: a resolve driven through a stubbed transport produces one `send` on
  the update channel carrying the changed rows, with `hasImage` filled.
- The image handler: present, absent, and a failing transport.
- Idempotence: two registrations, one handler each on the request channels.

Then all four Nix checks with every new file staged.

## Risks and Open Questions

- **`readImageSubjects` is outside this task's declared `targetPaths`.**
  Recorded here before the work. `hasImage` cannot be answered without it, and
  answering it with `readImage` would read every cached logo on every bulk read.
- **The database is opened at registration rather than on first use.** Opening
  creates a directory and applies a schema, cannot throw, and degrades to an
  empty cache, so the cost of doing it at startup is a file the user would get on
  their first token render anyway. Stated so a later reader does not take it for
  an oversight.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-014.status` to `completed`.
- `.agent/workflows/ipc.md` stays wrong until `task-028`, which depends on this
  task and now has its example to describe.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-014-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-014-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The cache has a caller. A renderer request is answered from disk, what is
missing is scheduled, and rows reach the renderer as they resolve.

## Final Outcome

Complete.

## Self-Review

The property most at risk of being lost later is that the request handler does
not await the network. It is a single missing `await` away from becoming a
resolve-and-wait, and the spec case that would catch that uses a transport that
never settles, so the test hangs the handler rather than slowing it.
