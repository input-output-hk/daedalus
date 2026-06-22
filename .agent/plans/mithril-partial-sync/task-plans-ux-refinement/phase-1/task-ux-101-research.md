# task-ux-101 — Research Note (durable findings)

## Decisions
- **Routing seam (RESOLVED):** the availability read is owned by `chainStorageCoordinator`
  (already the kill-switch source of truth via `launcherConfig` import and
  `_assertPartialSyncFeatureEnabled`) and reached through a thin `MithrilController` passthrough,
  matching the existing two-hop routing every partial-sync channel uses
  (`channel → getMithrilController().<m>() → chainStorageCoordinator.<m>()`). This keeps the new
  handler a one-line sibling of the other five and gives **task-ux-102 a single extension point** —
  the body of `chainStorageCoordinator.getPartialSyncAvailability()` — to plug in the cached
  certified-immutable-gap math without touching the channel, type, controller passthrough, or
  renderer wrapper.
- **`isEnabled` polarity:** `launcherConfig.mithrilPartialSyncEnabled === true` is the exact inverse
  of the existing guard `... !== true` at `chainStorageCoordinator.ts:375-379`.
- **Availability is a SEPARATE channel** from `MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL` /
  `MithrilPartialSyncStatusSnapshot` (operation status), per PRD D3.

## Gotchas / evidence (verified against live code at implementation time)
- **IPC generic ordering is inverted between sides:** main is
  `MainIpcChannel<RendererRequest, MainResponse>`; renderer is
  `RendererIpcChannel<MainResponse, RendererRequest>`. Copy `mithrilPartialSyncStatusChannel`
  verbatim to avoid swapped generics.
- **Spec positional `mockChannels` indexing:** `new MainIpcChannel(...)` module-eval order is
  start[0] → status[1] → cancel[2] → restart-normal[3] → wipe[4]. Declaring the availability channel
  AFTER wipe makes it `mockChannels[5]` and leaves indices 0–4 stable. The mock pushes a fresh object
  per construction, so handler retrieval is by channel object (`mockChannels[5].onRequest.mock.calls[0][0]`),
  not by registration sequence. Registration was placed LAST to mirror declaration order.
- **`api-endpoints.md`:** the Mithril count cell was a live-verified bump (10→11 = 5 bootstrap + 6
  partial-sync). Do not trust stale counts in plans — verify the live table.

## Residual gaps / hand-off to task-ux-102
- The behind-ness fields are STUBBED here (`isSignificantlyBehind: false`, `behindByImmutables:
  undefined`). task-ux-102 replaces the coordinator method body with the certified-immutable-gap
  computation (reuse `resolveLatestSnapshotMetadata` + `derivePartialSyncRange` math; cached, no
  restore; `false` when no certified range).
- **Open question for 102 planning (non-blocking):** the coordinator read is currently synchronous
  and lock-free (matches `isPartialSyncInProgress()`). Confirm 102's behind-ness read can stay
  lock-free off a CACHED value (not a live FS/aggregator read on the hot path). If 102 must read disk
  under the mutation lock, the seam becomes async + queued and BOTH the coordinator and controller
  signatures (and the channel's already-`async` handler) change together. Decide at 102 planning.

## Conflicts found between PRD / research / tasks JSON / live repo
- None. D2/D3, research-19, and the tasks JSON were consistent with the live repo for this task. The
  only targetPaths nuance (`MithrilController.ts` not listed) was resolved per the DESIGN DECISION and
  is consistent with the prompt's "targetPaths is guidance, not a whitelist" + "smallest truthful
  change reusing existing seams".
