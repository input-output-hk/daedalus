Implementation: Iteration 1
Timestamp: 2026-05-20T19:20:30Z
Changes made:
- Added `source/renderer/app/stores/MithrilPartialSyncStore.ts` as a dedicated renderer store for Mithril partial sync state.
- Registered `mithrilPartialSync` in `source/renderer/app/stores/index.ts` so the store is available through the normal injected stores map.
- Added `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` with focused coverage for cached status sync, explicit field clearing, backend-driven recovery actions, payload-free action delegation, and reset-safe no-listener behavior.
- Intentionally kept `App.tsx` and modal ownership untouched to preserve the approved task-300 scope boundary.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` ⚠️ blocked by pre-existing unrelated errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None in scope. The implementation took the approved safe default and did not add push-status listener wiring because renderer IPC still provides no unsubscribe path across `window.daedalus.reset()`.

Whether user interaction is now required:
- No.

Outcome: Dedicated partial-sync renderer store is in place with store-map wiring and focused lifecycle-safe tests, ready for code review.

Implementation: Iteration 2
Timestamp: 2026-05-20T19:23:50Z
Changes made:
- Extended `source/renderer/app/stores/MithrilPartialSyncStore.ts` with a teardown-safe polling seam instead of an unsafe long-lived IPC broadcast listener.
- Added explicit interval setup/clear methods and a store `teardown()` override so `window.daedalus.reset()` store recreation cannot accumulate polling work.
- Updated all partial-sync action methods to refresh cached backend status immediately after their payload-free IPC request resolves.
- Expanded `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` to cover action-triggered refresh, polling updates, teardown cleanup, and reset-safe store recreation.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` was previously attempted and remains blocked by pre-existing unrelated errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation now follows the approved lifecycle-safe fallback by using bounded polling plus explicit post-action refresh instead of `onReceive()` listener wiring.

Whether user interaction is now required:
- No.

Outcome: The partial-sync store now stays truthful after initialization and actions without introducing a reset-unsafe IPC listener model, ready for a second code review pass.

Implementation: Iteration 3
Timestamp: 2026-05-20T19:26:26Z
Changes made:
- Serialized `MithrilPartialSyncStore.syncStatus()` with a shared in-flight promise so overlapping renderer `request()` calls cannot race on the shared IPC response channel.
- Narrowed polling to active restore states only by starting the interval only while the backend reports a working status and clearing it again for idle/terminal states and teardown.
- Added the planned documentation follow-ups: `.agent/system/architecture.md` now documents the new renderer store seam, and `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md` records the durable task-300 decisions.
- Expanded the store spec to cover single-flight request reuse in addition to active-only polling and reset-safe cleanup.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` remains previously blocked by unrelated pre-existing errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation still uses the approved lifecycle-safe fallback instead of `onReceive()` listener wiring, but now avoids overlapping IPC requests and permanent idle polling.

Whether user interaction is now required:
- No.

Outcome: The partial-sync renderer store now provides a truthful, reset-safe, single-flight status seam with the required task-300 research and architecture updates, ready for the next review pass.

Code Review: Iteration 3
Timestamp: 2026-05-20T19:27:36Z

Blocking findings:
1. `source/renderer/app/stores/MithrilPartialSyncStore.ts:63-66` clears the current interval during `teardown()`, but `source/renderer/app/stores/MithrilPartialSyncStore.ts:125-139` still lets any already-in-flight `syncStatus()` complete, and `source/renderer/app/stores/MithrilPartialSyncStore.ts:178-182` will restart polling from that late response if it reports a working state. In a real `window.daedalus.reset()` race, the old torn-down store can therefore recreate its own polling loop after teardown and continue issuing status requests in parallel with the new store. That reintroduces the reset-time lifecycle leak task-300 was explicitly supposed to avoid.

Non-blocking observations:
1. The new research note and architecture update are present and match the intended task-300 scope well.
2. `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` now covers single-flight reuse and active-only polling, but it still does not cover the teardown-while-request-is-in-flight case that exposes the remaining leak.

Approval bar:
1. Make torn-down stores ignore late `syncStatus()` completions before they call `_updateStatus()` or restart polling.
2. Add one focused test that tears the store down while a status request is still pending and proves no interval or follow-up polling is recreated afterward.

Decision: requires_changes

Implementation: Iteration 4
Timestamp: 2026-05-20T19:27:59Z
Changes made:
- Added a generation guard to `MithrilPartialSyncStore.syncStatus()` so late responses from an old torn-down store are ignored instead of mutating stale state or restarting polling.
- Extended the store spec with an explicit teardown-while-request-is-in-flight regression test that proves no late completion recreates polling after `teardown()`.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` remains previously blocked by unrelated pre-existing errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation remains within the approved lifecycle-safe polling fallback and now explicitly guards against late completions across `window.daedalus.reset()`.

Whether user interaction is now required:
- No.

Outcome: The partial-sync store now stays truthful across setup, active polling, actions, store recreation, and teardown races, ready for the next review pass.

Code Review: Iteration 4
Timestamp: 2026-05-20T19:29:58Z

Blocking findings:
1. `source/renderer/app/stores/MithrilPartialSyncStore.ts:193-196` still cannot surface live partial-sync progress from an idle start. The store only updates state after `await mithrilPartialSyncStartChannel.request()` resolves, and polling only starts from `_updateStatus()` when the store is already in a working state (`source/renderer/app/stores/MithrilPartialSyncStore.ts:100-111`, `source/renderer/app/stores/MithrilPartialSyncStore.ts:185-189`). But the main-side start request does not return until the whole backend operation finishes: `source/main/ipc/mithrilPartialSyncChannel.ts:138-142` awaits `chainStorageCoordinator.startPartialSync(...)`, which in turn awaits `MithrilPartialSyncService.start(...)` through the full flow (`source/main/utils/chainStorageCoordinator.ts:264-268`, `source/main/mithril/MithrilPartialSyncService.ts:191-307`). Result: from `idle`, the renderer store stays `idle` for the whole run, never starts polling, and only sees a terminal state after completion/failure. That fails task-300’s core requirement to provide a truthful renderer status seam for later diagnostics/progress work.

Non-blocking observations:
1. The generation guard added in iteration 4 does fix the specific teardown-race I called out previously.
2. `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts:145-167` still uses an unrealistically immediate mocked start request plus a separate mocked status fetch, so it masks the live main-process contract where `start` itself is long-running.

Approval bar:
1. Make `startPartialSync()` transition the store into a state where active status polling can begin before the long-running main-process start request resolves, without reintroducing reset-unsafe listeners or overlapping status requests.
2. Add one focused test that models the real main-side behavior: `start` remains pending while backend cached status advances to a working state, and the store must stop showing `idle` and begin following progress during that window.

Decision: requires_changes

Implementation: Iteration 5
Timestamp: 2026-05-20T19:30:45Z
Changes made:
- Updated `startPartialSync()` to leave `idle` immediately by seeding a transient working status and starting lifecycle-safe polling before the long-running main-process start request resolves.
- Added a focused deferred-start regression test that matches the live main-process contract: `start` remains pending while cached backend status advances through working states, and the store now follows those transitions instead of staying `idle` until completion.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` remains previously blocked by unrelated pre-existing errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation still avoids reset-unsafe listeners and overlapping status requests, but now also surfaces live progress from an idle start while the backend start IPC call is still in flight.

Whether user interaction is now required:
- No.

Outcome: The partial-sync store now provides truthful live status transitions for later diagnostics/progress work even when the backend start IPC call blocks until restore completion, ready for the next review pass.

Code Review: Iteration 5
Timestamp: 2026-05-20T19:33:02Z

Blocking findings:
1. `source/renderer/app/stores/MithrilPartialSyncStore.ts:58-63`, `source/renderer/app/stores/MithrilPartialSyncStore.ts:122-147`, and `source/renderer/app/stores/MithrilPartialSyncStore.ts:193-213` still allow a pre-existing `syncStatus()` request to overwrite the new seeded working state in `startPartialSync()`. A realistic path is: store setup requests cached status, user starts partial sync before that request resolves, `startPartialSync()` seeds `stopping-node`, then the older cached-status request resolves with `idle` and `_updateStatus()` clears polling again. Because the main-side start IPC stays pending for the whole operation (`source/main/ipc/mithrilPartialSyncChannel.ts:137-142`, `source/main/utils/chainStorageCoordinator.ts:223-268`), the store can fall back to `idle` and stop following live progress until the operation finishes. That means the core live-status gap is still present under an in-flight setup/status race.

Non-blocking observations:
1. The new deferred-start test is a good addition, but it only models a pending start request with no earlier in-flight cached status request; it does not exercise the stale-setup-response race above.
2. The task docs and architecture note remain present and consistent with the intended task-300 scope.

Approval bar:
1. Prevent any pre-start in-flight status request from applying after `startPartialSync()` seeds its working state. A small truthful fix is fine, but the store cannot let an older cached `idle` response tear down polling during an active start.
2. Add one focused regression test for: pending setup/status request, then `startPartialSync()`, then old status response returns `idle`, and the store must keep following live working-state updates.

Decision: requires_changes

Implementation: Iteration 6
Timestamp: 2026-05-20T20:33:42Z
Changes made:
- Invalidated any pre-start in-flight cached status request before `startPartialSync()` seeds its transient working state, so an older `idle` response can no longer tear down polling during an active start.
- Added the exact stale-setup-response regression test requested by review: pending setup status, then start, then old `idle` response, and the store still follows live working-state updates.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` remains previously blocked by unrelated pre-existing errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation still uses the approved lifecycle-safe polling fallback and now also guards the stale setup-status race during long-running start requests.

Whether user interaction is now required:
- No.

Outcome: The partial-sync store now remains truthful across setup, long-running start, active polling, teardown, and reset races, ready for the next review pass.

Implementation: Iteration 7
Timestamp: 2026-05-20T20:36:44Z
Changes made:
- Preserved the stale-setup-response fix without reopening overlapping IPC requests by queueing one follow-up status sync behind an existing in-flight request instead of nulling the shared promise reference.
- Added a focused regression test for the exact transport hazard called out in review: an older pending status request survives into `startPartialSync()`, the store seeds a working state, and the implementation still avoids overlapping `MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL.request()` calls while catching up to live progress.
- Tightened the stale-setup-response test to reflect the actual queued-sync ordering under fake timers.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` remains previously blocked by unrelated pre-existing errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation still uses the approved lifecycle-safe polling fallback and now keeps both stale-response safety and single-flight IPC transport safety during long-running start transitions.

Whether user interaction is now required:
- No.

Outcome: The partial-sync store now stays truthful across setup, long-running start, queued catch-up syncs, active polling, teardown, and reset races without overlapping IPC status requests, ready for the next review pass.

Code Review: Iteration 7
Timestamp: 2026-05-20T20:37:44Z

Blocking findings:
1. `source/renderer/app/stores/MithrilPartialSyncStore.ts:145-152` now unconditionally runs a queued follow-up `await this.syncStatus()` after any in-flight request finishes when `_pendingStatusSyncAfterCurrent` is set. `teardown()` only bumps `_statusSyncGeneration` and clears the interval (`source/renderer/app/stores/MithrilPartialSyncStore.ts:66-69`), but it does not clear `_pendingStatusSyncAfterCurrent`. In a reset/teardown race, an old store can therefore queue a second status request after teardown: the stale first response is ignored by generation, then the `finally` block sees the pending flag and starts a fresh `request()` anyway. That recreates post-teardown IPC activity from a dead store and reintroduces the lifecycle leak task-300 was explicitly trying to avoid across `window.daedalus.reset()`.

Non-blocking observations:
1. The queued-follow-up approach does fix the overlapping-request hazard from iteration 6 while preserving live catch-up behavior.
2. The current teardown test only covers one late completion with no queued follow-up flag set, so it does not exercise the remaining reset-time edge above.

Approval bar:
1. Ensure torn-down stores cannot execute queued follow-up syncs after an in-flight request settles. A small teardown-time guard or pending-flag reset is enough, but dead stores must not issue any new status requests.
2. Add one focused regression test where a pending status request exists, `startPartialSync()` sets the queued-follow-up path, then `teardown()` happens before the first request resolves, and no second status request is issued.

Decision: requires_changes

Implementation: Iteration 8
Timestamp: 2026-05-20T20:39:29Z
Changes made:
- Cleared queued follow-up syncs on teardown and gated the queued catch-up path on a separate teardown generation so dead stores cannot issue new status requests after reset.
- Split stale-status invalidation from teardown invalidation so the store can still perform the intended queued catch-up sync after `startPartialSync()` without reopening post-teardown leaks.
- Added the exact teardown-with-queued-follow-up regression test requested by review and filled the queued catch-up tests with realistic follow-up status responses.
- Reset Jest mock implementations between tests so the lifecycle timing coverage reflects the current store behavior cleanly.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` remains previously blocked by unrelated pre-existing errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation still uses the approved lifecycle-safe polling fallback and now covers stale setup responses, queued catch-up syncs, single-flight transport safety, and teardown/reset suppression together.

Whether user interaction is now required:
- No.

Outcome: The partial-sync store now stays truthful across setup, long-running start, queued catch-up syncs, active polling, teardown, and reset races without overlapping IPC status requests or post-teardown follow-up requests, ready for the next review pass.

Code Review: Iteration 8
Timestamp: 2026-05-20T20:41:18Z

Blocking findings:
1. `source/renderer/app/stores/MithrilPartialSyncStore.ts:126-164` still has no guard against calling `syncStatus()` after the store has already been torn down. That matters because `startPartialSync()` always does `await this.syncStatus()` in its `finally` block (`source/renderer/app/stores/MithrilPartialSyncStore.ts:210-233`), and the other action methods do the same after their request resolves (`source/renderer/app/stores/MithrilPartialSyncStore.ts:236-252`). In a real `window.daedalus.reset()` race (`source/renderer/app/index.tsx:57-60`), an old store can be torn down while `startPartialSync()` is still awaiting the long-running main-process start IPC call, then issue a fresh status request after teardown when that await finishes. Because `syncStatus()` captures the already-incremented teardown generation as its new baseline, that post-teardown request is treated as valid and can mutate dead-store state or restart polling. This reintroduces reset-time post-teardown IPC activity through a different path than the queued-follow-up bug.

Non-blocking observations:
1. Iteration 8 does fix the specific queued-follow-up-after-teardown leak from the previous review.
2. The new teardown regression test is useful, but it only covers the queued-follow-up path with an existing in-flight status request; it does not cover the more direct “action settles after teardown, then calls `syncStatus()`” path above.

Approval bar:
1. Make `syncStatus()` a no-op once the store has been torn down, or otherwise ensure post-teardown action completions cannot trigger any new status request from a dead store.
2. Add one focused regression test for: `startPartialSync()` begins with no in-flight status request, the store is torn down while the start IPC is still pending, then the start request settles, and no new status request is issued afterward.

Decision: requires_changes

Implementation: Iteration 9
Timestamp: 2026-05-20T20:41:46Z
Changes made:
- Made torn-down stores treat `syncStatus()` and `_updateStatus()` as no-ops so action `finally` blocks and any late status application cannot issue new IPC requests or mutate dead-store state after reset.
- Added the focused regression test requested by review for the direct post-teardown path: `startPartialSync()` begins with no in-flight status request, the store is torn down while the long-running start IPC call is still pending, then the start request settles and no new status request is issued.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` ✅
- `yarn compile` remains previously blocked by unrelated pre-existing errors in `source/main/cardano/setup.spec.ts`, `source/main/ipc/getHardwareWalletChannel.ts`, and `source/main/trezor/manifest.ts`

Deviations from the approved plan:
- None. The implementation still uses the approved lifecycle-safe polling fallback and now also blocks direct post-teardown status requests triggered by late action completion.

Whether user interaction is now required:
- No.

Outcome: The partial-sync store now remains truthful across setup, long-running start, queued catch-up syncs, active polling, teardown, reset races, and late action completion without overlapping or post-teardown IPC status requests, ready for the next review pass.

