# task-ux-202 — Research note (durable)

Scope: backend success-path finalization for Mithril partial sync — finalize IPC channel, dismiss-driven
reset/cleanup, `node-start-verified` marker (Boundary C2), and cross-session staging reclamation.
Implements PRD D9 (`...-ux-refinement-prd.md:402-485`), fixing BUG1, BUG2, gap #41, gap #44.

## Decisions (with evidence)

### D1 — A new finalize IPC channel is warranted; renderer wrapper added here
- `dismissCompletedOverlay` is today a renderer-only flag flip with no backend hook (PRD `:446-460`), so
  there is NO existing seam to reuse → a fifth action channel `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL`
  (`void → void`) is the correct shape, mirroring the cancel channel (`api.ts:468-471`). This does NOT
  contradict the `retry`=start reuse policy (that reused a real start seam; finalize has none).
- **Renderer-wrapper boundary reconciliation:** task-ux-202's JSON targetPaths OMIT
  `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`; task-ux-404's INCLUDE it. Every partial-sync
  channel pairs a main + renderer wrapper, and task-ux-101 set the precedent by adding the **availability**
  channel on both sides ahead of its renderer consumer. So the renderer wrapper
  `mithrilPartialSyncFinalizeChannel` is added here (strictly additive, unused-yet export) to complete the
  contract end-to-end; task-ux-404 imports it with no missing-export churn. **Hand-off:** task-ux-404 owns
  `dismissCompletedOverlay` invoking this channel; task-ux-301 owns `isActionBlocked`-from-`isWorking`.

### D2 — Controller routes finalize direct-to-service (no coordinator)
- cancel/restart/wipe go through `chainStorageCoordinator` because they orchestrate node stop/restart and
  take the coordinator mutation lock (`MithrilController.ts:404-426`, `chainStorageCoordinator.ts:280-340`).
- Finalize does NO node orchestration — pure post-success cleanup from a terminal state (node already
  proven RUNNING, marker `node-start-verified`). No concurrency hazard (`_activeWorkDir` null post-`start()`;
  `assertStartAllowed` would reject a concurrent start). So `MithrilController.finalizePartialSync()` calls
  `this._partialSyncService.finalizeCompletedPartialSync()` directly. Adding it to the
  `_getPartialSyncDependencies()` handlers bag + a coordinator method would be over-engineering.

### D3 — Cross-session staging path: persist the RESOLVED root in the marker (`stagingRootPath`)
- **In-session evidence (happy path):** after cutover, `start()` ends in `finalizing`; the post-cutover node
  restart runs via `configureMithrilPartialSyncRuntime({ restartStartupFlow })` →
  `handleCheckDiskSpace(false)` (`index.ts:250-252`) — a startup re-entry in the **same process**, NO
  `app.relaunch` (grep for `relaunch`/`app.exit` in `index.ts`/`handleDiskSpace.ts`/`chainStorageCoordinator.ts`
  → none). `finalizeInstalledNodeStart` (`MithrilStartupGate.ts:503`, after `cardanoNode.start()`) and the
  later dismiss IPC both hit the same `MithrilController` + `MithrilPartialSyncService` singletons. So the
  dismiss finalize is in-session and `_stagingChainDir` is still set (sticky, not cleared by
  `_clearRuntimeWorkState` `:576-582`).
- **Cross-process evidence (the reason the marker field is REQUIRED):** the C2-reclaim branch runs from
  `handleInterruptedRecovery` on a **fresh boot** (`MithrilStartupGate.ts:196`) after a
  close-without-dismiss / crash. There is NO service instance / `_stagingChainDir = null`, so re-resolving
  `_getStagingRootPath()` falls back to the legacy `stateDirectoryPath/mithril-partial-sync` and MISSES a
  relocated/symlinked chain's colocated staging (task-ux-201 colocates staging as a sibling of the realpath
  chain — `MithrilPartialSyncService.ts:588-598`). The durable marker is the only carrier across that
  boundary.
- **Realization:** extend the marker type + `writeMithrilPartialSyncMarker` options with
  `stagingRootPath?: string` (`mithrilPartialSyncMarker.ts:11-15,22-39`); persist
  `this._getStagingRootPath()` at the two cutover writes (`MithrilPartialSyncService.ts:226,230`, while
  `_stagingChainDir` is set); carry it into `node-start-verified`. C2 reclaim removes the exact
  `marker.stagingRootPath`; dismiss finalize removes `marker.stagingRootPath ?? _getStagingRootPath()`.
- **Rejected:** re-resolve everywhere (wrong volume cross-session); reconstruct from
  `marker.managedChainPath` (the symlink, not the realpath — wrong volume per task-ux-201).

### D4 — `node-start-verified` (Option A) with deferred clear
- `finalizeInstalledNodeStart` (`mithrilPartialSyncNodeStartup.ts:147-172`) currently clears the marker at
  `:165` right after the RUNNING assertion (`:159-163`), so the `node-start-verified` state is never written
  and the `:60-63` C2 branch is dead (gap #44, research-19 `:397`). Replace the clear with
  `writeMithrilPartialSyncMarker('node-start-verified', { managedChainPath, stagingRootPath })`; keep the
  `completed` emission (`:166-171`); defer the clear to the dismiss-driven service finalize. This satisfies
  PRD D9 steps 1–3 and realizes backend-PRD Boundary C2 (`mithril-partial-sync-prd.md:187-194`): an
  interruption in the verify→clear window now resumes a normal boot, not a C1 re-drive. Option B (delete the
  state) rejected per PRD `:435-436`.

### D5 — C2 reclaim extension + `fs-extra` import
- Extend the `:60-63` branch to `if (marker.stagingRootPath) await fs.remove(marker.stagingRootPath)` before
  `clearMithrilPartialSyncMarker()`; `mithrilPartialSyncNodeStartup.ts` does NOT import `fs` today → add
  `import fs from 'fs-extra'` (matching `MithrilPartialSyncService.ts:2` style). Guarded so a legacy/absent
  path is a safe no-op. Closes gap #41 on the completed-but-not-dismissed window (PRD `:461-466`).

### D6 — Finalize idempotency
- `finalizeCompletedPartialSync()` is safe from any state: `_resetToIdleStatus()` re-asserts idle;
  `fs.remove` on a missing path is a no-op (fs-extra); `clearMithrilPartialSyncMarker()` =
  `fs.remove(markerPath)` is a no-op when absent. No status guard. Order: read marker → resetToIdle →
  fs.remove(root) → clearMarker → clearRuntimeWorkState.

## Gotchas / evidence
- **PRD anchor drift:** PRD/research cite `MithrilPartialSyncService.ts:205` (cutover-in-progress) and `:209`
  (installed-awaiting-node-start); LIVE code is `:226` and `:230`. `finalizeInstalledNodeStart` clear at
  `:165` matches live.
- **`_stagingChainDir` is sticky** (task-ux-201): set in `start()` (`:138`), deliberately NOT cleared in
  `_clearRuntimeWorkState()` (`:576-582`). Load-bearing for the in-session dismiss path resolving the
  colocated root.
- **`idle` is NOT an overlay status** (`mithril-partial-sync.types.ts:79-96`): confirms WHY resetting to
  idle at verified success would hide the success screen (lock #16 / D9). We keep `completed` until dismiss.
- **Mock-channel index order** (`mithrilPartialSyncChannel.spec.ts:117-131,182-203`): channels indexed by
  construction order — start(0),status(1),cancel(2),restartNormal(3),wipe(4),availability(5). Appending
  finalize LAST makes it `mockChannels[6]`; existing index assertions (e.g. `mockChannels[5]` =
  availability) stay valid. The implementer MUST append, not insert.
- **BUG1 reader:** `MithrilController.isPartialSyncActive()` (`:194-199`) returns `status !== 'idle'`;
  finalize's `_resetToIdleStatus` re-arms it (backend half of BUG1; the renderer `isWorking` derivation is
  task-ux-301).
- **No dedicated spec existed** for `mithrilPartialSyncNodeStartup.ts` or `mithrilPartialSyncMarker.ts` —
  both new spec files are created by this task. The node-startup behavior is otherwise only mocked in
  `handleDiskSpace.spec.ts`.

### D7 — In-session overlay survival hinges on `_startupCheckDone` (confirmed at code review)
- The deferred-clear is only safe in-session because `MithrilStartupGate._startupCheckDone` (`:213`)
  short-circuits a second `handleInterruptedRecovery` on the in-session `restartStartupFlow` re-entry. That
  flag is reset ONLY by `resetOnDirectoryChange` (`MithrilStartupGate.ts:218-224`), NOT by
  `restartStartupFlow`. So after verified success the in-session re-entry hits the early-return (`:174-176`)
  and does NOT prematurely reclaim staging / clear the `node-start-verified` marker — the `completed`
  overlay stays visible until the explicit dismiss IPC (lock #9/#16 / D9 hold). **Carry-forward:** if a
  future change (task-ux-204 native-dialog, task-ux-701 cutover re-arch) alters the startup-gate flow or
  resets `_startupCheckDone` on `restartStartupFlow`, this safety property must be re-checked.

### D8 — On-disk staging path ≡ persisted `stagingRootPath` (no leak, confirmed at code review)
- The on-disk staging root is `preparePartialSyncStagingDirectory(this._getStagingRootPath(), …)` which
  `path.resolve`s the same value (`mithrilPartialSyncStaging.ts:24-52`); the marker persists exactly
  `this._getStagingRootPath()` at the two cutover writes (`MithrilPartialSyncService.ts:228,232`). On-disk
  path ≡ persisted path → the C2 reclaim / dismiss finalize `fs.remove` the **exact** colocated dir, no
  legacy-dir removal and no colocated-dir leak. Fallback test resolves to
  `/tmp/daedalus-state/mithril-partial-sync` for a fresh service (`_stagingChainDir = null`).

## Residual gaps / hand-offs
- **task-ux-404** — renderer `dismissCompletedOverlay` invokes `mithrilPartialSyncFinalizeChannel` (added
  here, both sides — exported from `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`, no import churn);
  plus the always-syncStatus-after-cancel work.
- **task-ux-301** — `isActionBlocked` derived from `isWorking` (renderer CTA re-arm half of BUG1).
- **task-ux-503** — cross-cutting E2E / integration coverage of the full dismiss → reset-to-idle → C2-reclaim
  loop. This task's coverage is unit-level only (61 tests across the 4 specs); no E2E/Cucumber scenario was
  added. The end-to-end seam (renderer dismiss → finalize channel → service triple) only becomes wireable
  once task-ux-404 lands the renderer invocation.
- **UX9 / gap #43** (research-19 `:396`) — the transient nix dev-env overlay error; expected to be mitigated
  by this task's staging/marker cleanup; no dedicated fix unless QA reproduces (tracked under D5b).
- **Invalid-marker fallback** carries no `stagingRootPath` — acceptable: that path is the unsafe-cutover
  (B/C1) branch handled by wipe-and-full-sync (which re-derives and wipes the chain anyway), so it cannot
  leak the SUCCESS staging dir.

## Conflicts reconciled
- **JSON targetPaths (renderer file)** — task-ux-202's targetPaths OMIT
  `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`; task-ux-404's INCLUDE it. Reconciled by adding the
  renderer wrapper here (both-sides precedent, task-ux-101 availability), strictly additive. Recorded as the
  task-ux-404 hand-off (D1).
- **PRD-vs-live line anchors** — PRD/research cite `MithrilPartialSyncService.ts:205/:209` for the cutover
  writes; LIVE is `:226/:230` (now `:228/:232` after the additive `stagingRootPath` lines). No PRD edit
  needed — D9 already specifies the ordered sequence; the `stagingRootPath` marker field is the engineering
  realization of D9's cross-process staging removal.

## Status
- Planning status: `approved` (see `task-ux-202.md`).
- Build status: `completed` (tsc exit 0; 61/61 target-spec tests pass; 0 new eslint warnings).
