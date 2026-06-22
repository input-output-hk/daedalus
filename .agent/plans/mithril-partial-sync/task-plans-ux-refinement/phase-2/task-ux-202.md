# task-ux-202 — Success-path finalization: finalize channel, reset-to-idle, staging cleanup, and node-start-verified marker (Boundary C2)

- Sprint: Mithril Partial Sync UX Refinement — phase-2 (Backend UX-Enabling Correctness)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `completed`
- Interaction mode: `autonomous`
- Priority: critical · Estimated: 6h · Dependencies: `task-ux-201` → COMPLETED

## Why now
A **verified** partial-sync success today parks the backend in `completed` forever (until app restart) and
leaves the staging dir on disk:
- **BUG1** — `status` stays `completed`, and `isPartialSyncActive()` =
  `chainStorageCoordinator.isPartialSyncInProgress() || status !== 'idle'`
  (`MithrilController.ts:194-199`), so the diagnostics CTA stays disabled until restart.
- **BUG2 / gap #41** — `_cleanupPartialSyncArtifacts()` / `fs.remove(stagingRoot)` runs only on cancel
  (`MithrilPartialSyncService.ts:289`), restart-normal (`:338`), wipe (`:338`) — **never on success**.
  Install removes only the staged DB dir; `finalizeInstalledNodeStart` clears only the marker
  (`mithrilPartialSyncNodeStartup.ts:165`). The colocated staging dir (task-ux-201) leaks.
- **gap #44** — the marker is a 3-state machine `cutover-in-progress | installed-awaiting-node-start |
  node-start-verified` (`mithrilPartialSyncMarker.ts:6-9`), but **`node-start-verified` is never written**
  (`finalizeInstalledNodeStart` jumps from C1 straight to clearing the marker at `:165`). That makes its
  startup-recovery branch (`mithrilPartialSyncNodeStartup.ts:60-63`) **dead code** and leaves the backend
  PRD's **Boundary C2** (`mithril-partial-sync-prd.md:187-194`, `:194`: "Daedalus may clear the marker and
  resume normal startup") unimplemented — an interruption in the verify→clear window is mishandled as the
  heavier **C1 re-drive** instead of a clean normal boot.

This task implements PRD **D9** (`...-ux-refinement-prd.md:402-485`) — the *Ordered finalization sequence*
— which **amends locked decision #16**: the success overlay still follows the lifecycle through
`completed` and still requires an explicit user dismiss, **but on that dismiss the backend resets to
`idle`, removes staging, and clears the marker**, returning the feature to a clean, re-runnable state with
no app restart. It also realizes Boundary C2 (Option A) and reclaims leftover staging on a
close-without-dismiss / crash. The renderer halves (the dismiss-overlay IPC invocation, and the
`isActionBlocked`-from-`isWorking` derivation) are **task-ux-404** and **task-ux-301** — NOT in scope here.

## Interaction mode justification
`autonomous`: backend-only. No user-facing copy is authored (the success overlay copy already exists; the
renderer invocation is task-ux-404). No destructive/irreversible action — the dismiss-driven cleanup only
`fs.remove`s the **scratch staging dir** (never the live chain; the installed chain has already passed a
proven node start) and clears a lock-file marker; both are idempotent and re-derivable. D9 is **fully
specified** by the PRD (exact ordered sequence, exact reset/cleanup triple, exact marker state, exact C2
extension), so there is no open product decision. The marker-persisted staging-path detail is an
**engineering** correctness fix (cross-session path resolution), explicitly the same class as task-ux-201's
in-scope "engineering placement of scratch space".

## Scope
1. **New finalize IPC request channel** — a fifth partial-sync action channel
   (`MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL`) with `void → void` request/response types, a main wrapper +
   `onRequest` handler calling `controller.finalizePartialSync()`, and (to complete the contract for
   task-ux-404) the matching **renderer wrapper** export.
2. **Controller route** — `MithrilController.finalizePartialSync()` calling
   `this._partialSyncService.finalizeCompletedPartialSync()` **directly** (no coordinator; finalize does no
   node orchestration).
3. **Service finalize method** — `finalizeCompletedPartialSync()`: idempotently
   `_resetToIdleStatus()` + `fs.remove(<resolved staging root>)` + `clearMithrilPartialSyncMarker()`
   (PRD D9 step 4 / gap #41).
4. **Marker `stagingRootPath` field** — extend the marker type + `writeMithrilPartialSyncMarker` options
   with `stagingRootPath?: string`; persist the **resolved** staging root at the two cutover writes (while
   `_stagingChainDir` is set) and carry it forward into `node-start-verified`. This makes the staging path
   correct **cross-session** for both the C2 reclaim and the dismiss finalize.
5. **Activate `node-start-verified` (Option A)** — in `finalizeInstalledNodeStart`, after the node is proven
   `RUNNING`, replace the marker **clear** with `writeMithrilPartialSyncMarker('node-start-verified', …)`
   (carrying `managedChainPath` + `stagingRootPath`); keep the `completed` emission. The clear is **deferred**
   to the dismiss-driven service finalize (PRD D9 steps 1–3).
6. **Extend the C2 startup branch** — in `handleInterruptedRecovery` (`:60-63`), before clearing the marker,
   `fs.remove(marker.stagingRootPath)` (when present) so a close-without-dismiss / crash reclaims leftover
   staging on next launch.
7. **Tests** — new specs for `mithrilPartialSyncMarker`, `mithrilPartialSyncNodeStartup`, the
   `finalizeCompletedPartialSync` service method, and the new channel handler.

## Non-goals
- **Do NOT** implement the renderer store/overlay invocation (`dismissCompletedOverlay` → finalize channel)
  — that is **task-ux-404**. **Do NOT** change `isActionBlocked` derivation (`isWorking` vs `isActive`) —
  that is **task-ux-301**. This task only provides the backend channel + finalize behavior they consume.
- **Do NOT** reset to idle at verified success. Pushing `idle` would drop the overlay status (the renderer
  derives `shouldShowOverlay`/overlay-status from the live `status`; `idle` is not an overlay status —
  `mithril-partial-sync.types.ts:79-96`), hiding the success screen before the user sees it — **violates
  lock #16 / PRD D9**. The dismiss-driven finalize is the authoritative reset/cleanup trigger.
- **Do NOT** re-architect cutover/merge or the install machinery (that is task-ux-701).
- **Do NOT** change the post-cutover cancel rejection (`MithrilPartialSyncService.ts:270-274`) — lock #6.
- **Do NOT** change the native startup-interrupted dialog (Boundary B/C1 path) — that is task-ux-204.

## Dependencies
`task-ux-201` (COMPLETED) — introduced the **sticky** `_stagingChainDir` field and the colocated
`_getStagingRootPath()`. This task depends on that sticky field surviving `_clearRuntimeWorkState()` (so the
in-session dismiss finalize resolves the colocated path) **and** persists the resolved root into the marker
so the **cross-session** C2 reclaim is correct (task-ux-201 explicitly handed this off:
`task-ux-201.md:49-50,387-391`).

## Research / docs / workflows / skills consulted
- **PRD D9** (`...-ux-refinement-prd.md:402-485`) — the decision this task implements; the *Ordered
  finalization sequence* (`:437-445`), the dismiss-seam channel (`:446-460`), the never-dismissed/crash
  safety C2-extend (`:461-466`), the #16 amendment (`:467-481`).
- **Backend PRD** Boundary C2 model (`mithril-partial-sync-prd.md:187-194`).
- **Research-19** gaps (`research/19-ux-refinement-state-and-gaps.md`): #41 staging-leftover (`:394`), #44
  never-written `node-start-verified` (`:397`), #43/UX9 (`:396`), the boundary model (`:195,:251,:271,:283`).
- **Tasks JSON** task-ux-202 entry (`...-ux-refinement-tasks.json:153-190`); task-ux-404 deps (`:484-490`).
- Live code: `MithrilPartialSyncService.ts`, `MithrilController.ts`, `mithrilPartialSyncNodeStartup.ts`,
  `mithrilPartialSyncMarker.ts`, `MithrilStartupGate.ts`, `chainStorageCoordinator.ts`, `index.ts`,
  `mithrilPartialSyncChannel.ts` (main + renderer), `common/ipc/api.ts`,
  `common/types/mithril-partial-sync.types.ts`, and existing specs
  (`mithrilPartialSyncChannel.spec.ts`, `MithrilPartialSyncService.spec.ts`, `handleDiskSpace.spec.ts`).
- Format/rigor template: `task-ux-201.md` (phase-2). Project memory: type-check with
  `./node_modules/.bin/tsc --noEmit` (not `yarn compile` — sass hook fails on Node 24).

## Verified seams (re-checked against live code at planning time; PRD/JSON anchors had drifted)

| What | Live location | Note |
|------|---------------|------|
| Channel-name constants | `common/ipc/api.ts:457-487` | START/STATUS/CANCEL/RESTART_NORMAL/WIPE_AND_FULL_SYNC/AVAILABILITY. Append FINALIZE after `:487`. |
| Cancel request/response types (the `void → void` shape to mirror) | `api.ts:468-471` | `CancelRendererRequest = void; CancelMainResponse = void` |
| Main channel const + import | `mithrilPartialSyncChannel.ts:38-41` (`cancelChannel`); imports `:3-24` | new `MainIpcChannel` appended after availability (`:53-56`); imports extended |
| Main `onRequest` handler bag | `mithrilPartialSyncChannel.ts:101-118` (cancel at `:107-109` → `controller.cancelPartialSync()`) | append `finalizeChannel.onRequest(async () => { await controller.finalizePartialSync(); })` |
| Renderer wrapper consts | `renderer/app/ipc/mithrilPartialSyncChannel.ts:25-53` | append `mithrilPartialSyncFinalizeChannel` (precedent: availability added on both sides) |
| Controller direct action methods | `MithrilController.ts:404-426` (cancel/restartNormal/wipe — all route via `chainStorageCoordinator` for node orchestration) | add a **direct** `finalizePartialSync()` → `this._partialSyncService.finalizeCompletedPartialSync()` (no coordinator) |
| Handlers bag | `MithrilController.ts:428-446` (`_getPartialSyncDependencies`) | **unchanged** — finalize bypasses the coordinator/handlers bag |
| `_partialSyncService` field | `MithrilController.ts:84` | the singleton finalize calls |
| Cutover marker writes | `MithrilPartialSyncService.ts:226` (`'cutover-in-progress'`), `:230` (`'installed-awaiting-node-start'`) | **PRD said `:205`/`:209` — DRIFTED.** Both pass `managedChainPath`; add `stagingRootPath: this._getStagingRootPath()` (computed while `_stagingChainDir` is set) |
| `_getStagingRootPath()` | `MithrilPartialSyncService.ts:588-598` | sticky-`_stagingChainDir`-derived colocated root (task-ux-201); fallback `stateDirectoryPath` |
| `_resetToIdleStatus()` | `MithrilPartialSyncService.ts:563-574` | reused by finalize (already used by cancel/restart/wipe) |
| `_cleanupPartialSyncArtifacts()` | `MithrilPartialSyncService.ts:558-561` | `fs.remove(stagingRoot)` + `clearMarker` — the close model |
| `finalizeWipeAndFullSync()` | `MithrilPartialSyncService.ts:345-349` | `clearMarker` + `_resetToIdleStatus` + `_clearRuntimeWorkState` — closest model for the new finalize |
| `_clearRuntimeWorkState()` | `MithrilPartialSyncService.ts:576-582` | does **NOT** clear `_stagingChainDir` (sticky, task-ux-201) — so in-session dismiss still resolves the colocated path |
| Service imports | `MithrilPartialSyncService.ts:1-2` (`path`,`fs`), `:26-29` (`clearMarker`,`writeMarker`) | all already imported |
| C2 startup branch (DEAD) | `mithrilPartialSyncNodeStartup.ts:60-63` | `if (marker.state === 'node-start-verified') { await clearMarker(); return false; }` — extend with `fs.remove(marker.stagingRootPath)` |
| `finalizeInstalledNodeStart` | `mithrilPartialSyncNodeStartup.ts:147-172` | read marker `:148`; RUNNING assert `:159-163`; **clear at `:165`** → replace with `writeMarker('node-start-verified', …)`; keep `completed` emit `:166-171` |
| node-startup imports | `mithrilPartialSyncNodeStartup.ts:11-14` (`clearMarker`,`readMarker`) | add `writeMithrilPartialSyncMarker`; add `import fs from 'fs-extra'` (NOT currently imported) |
| Marker type + state union | `mithrilPartialSyncMarker.ts:6-9` (union), `:11-15` (type) | add `stagingRootPath?: string` to the type |
| `writeMithrilPartialSyncMarker` | `mithrilPartialSyncMarker.ts:22-39` | add `stagingRootPath?` to options; spread it like `managedChainPath` |
| `readMithrilPartialSyncMarker` | `mithrilPartialSyncMarker.ts:41-76` | returns the marker verbatim when valid → `stagingRootPath` passes through; the invalid/absent fallbacks need no change |
| Startup wiring (in-session) | `MithrilStartupGate.ts:482` (`startInstalledNode`), `:503` (`finalizeInstalledNodeStart` after `cardanoNode.start()`); `index.ts:250-252` (`restartStartupFlow` → `handleCheckDiskSpace(false)`, **no `app.relaunch`**) | confirms happy-path finalize runs **in the same process / same singletons** |
| C2 reclaim is cross-process | `MithrilStartupGate.ts:196` (`handleInterruptedRecovery` on a fresh boot) | a fresh process has `_stagingChainDir = null` → **must** use `marker.stagingRootPath` |
| Status union incl. `completed` | `mithril-partial-sync.types.ts:3-15` | `completed` valid; overlay statuses `:79-96`; `idle` is NOT an overlay status |
| `isPartialSyncActive` (BUG1 reader) | `MithrilController.ts:194-199` | `status !== 'idle'` → finalize's `_resetToIdleStatus` re-arms it (backend half of BUG1) |
| Mock-channel index order (tests) | `mithrilPartialSyncChannel.spec.ts:117-131,182-203` | channels indexed by construction order: start(0),status(1),cancel(2),restartNormal(3),wipe(4),availability(5) → finalize will be **mockChannels[6]**; existing index assertions unaffected |

## DESIGN DECISIONS (RESOLVED)

### (a) New finalize channel: name, types, and renderer wrapper added here
**Chosen.** Add a fifth action channel exactly mirroring the cancel channel's `void → void` shape:
- `api.ts`: `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL = 'MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL'`;
  `MithrilPartialSyncFinalizeRendererRequest = void`; `MithrilPartialSyncFinalizeMainResponse = void`.
- Main wrapper `mithrilPartialSyncFinalizeChannel` + an `onRequest` calling
  `controller.finalizePartialSync()`.
- **Renderer wrapper added here too** (`mithrilPartialSyncFinalizeChannel` export). **Reconciliation of the
  targetPaths boundary:** the JSON targetPaths for task-ux-202 OMIT the renderer file while task-ux-404's
  INCLUDE it. Every partial-sync channel pairs a main + a renderer wrapper, and task-ux-101 set the
  precedent by adding the **availability** channel on **both** sides even though its renderer consumer
  landed later — an unused-yet export is fine and keeps the contract complete end-to-end so task-ux-404 can
  `import { mithrilPartialSyncFinalizeChannel }` with no missing-export churn. Adding the renderer wrapper
  here is strictly additive and does not implement any renderer store/overlay logic (still task-ux-404).
  Justified per the orchestrator's explicit recommendation; recorded in the research note.
- **Why a new channel (not reuse):** finalize is genuinely new behavior — `dismissCompletedOverlay` is
  today a renderer-only flag flip with no backend hook (PRD `:446-460`). There is no existing seam to reuse,
  so this does **not** contradict the `retry`=start reuse policy (that policy reused an existing start seam;
  there is simply no finalize seam to reuse).

### (b) Controller routes finalize DIRECT-to-service (no coordinator)
**Chosen.** `MithrilController.finalizePartialSync()` calls
`this._partialSyncService.finalizeCompletedPartialSync()` **directly**, NOT via `chainStorageCoordinator`.
- **Why:** cancel/restart/wipe route through the coordinator because they orchestrate **node stop/restart**
  and acquire the coordinator mutation lock (`MithrilController.ts:404-426`,
  `chainStorageCoordinator.ts:280-340`). Finalize does **no** node orchestration — it is pure post-success
  cleanup that runs only from a **terminal** state (the node is already proven `RUNNING`; the marker is
  `node-start-verified`). There is no concurrency hazard: a partial sync cannot be `start()`-ing while in
  the dismiss window (`assertStartAllowed`/`_assertPartialSyncStartAllowed` would have rejected, and
  `_activeWorkDir` is null post-`start()`), and finalize touches only the scratch staging dir + marker, both
  idempotently. Routing through the coordinator would add an unnecessary `_ensureNodeStopped…` /
  mutation-lock dance for a no-op-on-the-node action. So a **direct** method is the smallest truthful wiring.
- **Rejected:** adding `finalize` to the `_getPartialSyncDependencies()` handlers bag + a coordinator
  method — over-engineering; the handlers bag exists to give the coordinator the node-orchestrating
  callbacks, none of which finalize needs.

### (c) Cross-session staging path: persist the RESOLVED root in the marker (`stagingRootPath`)
**Chosen.** Extend the marker with `stagingRootPath?: string`; write it at the two cutover marker writes
(while `_stagingChainDir` is set, so `_getStagingRootPath()` resolves the **colocated** root) and carry it
into `node-start-verified`. Then:
- **C2 reclaim** (`handleInterruptedRecovery`) does `fs.remove(marker.stagingRootPath)` — the **exact**
  persisted dir, no re-resolution. This is **required** because that branch runs in a **fresh process** with
  **no service instance** / `_stagingChainDir = null`, where re-resolving would fall back to the legacy
  `stateDirectoryPath/mithril-partial-sync` and **miss** a relocated/symlinked chain's colocated staging.
- **Dismiss finalize** (`finalizeCompletedPartialSync`) removes
  `(await readMithrilPartialSyncMarker())?.stagingRootPath ?? this._getStagingRootPath()`. **In-session**
  (the verified happy path runs on the same singleton — see Decision (d)) `_stagingChainDir` is still set,
  so both expressions agree; the marker value makes it correct **even if** the dismiss ever arrives
  cross-session (defense-in-depth).

**Why this and not a simpler approach:**
- **In-session vs cross-session — verified.** The verified happy path is **in-session**: `start()` finishes
  in `finalizing`, then the post-cutover node restart runs via `restartStartupFlow` →
  `handleCheckDiskSpace(false)` (`index.ts:250-252`) — a startup re-entry **in the same process** with **no
  `app.relaunch`** (grep for `app.relaunch`/`relaunch` in `index.ts`/`handleDiskSpace.ts`/coordinator returns
  nothing). `finalizeInstalledNodeStart` (`MithrilStartupGate.ts:503`) and the later dismiss IPC both hit the
  **same** `MithrilController` + `MithrilPartialSyncService` singletons. So an in-session-only fix would
  *technically* work for dismiss. **BUT** the C2-reclaim branch is genuinely **cross-process** (a fresh
  boot after a close-without-dismiss / crash — `MithrilStartupGate.ts:196`) and has **no** service instance
  at all. The marker is the only durable carrier across that boundary. Persisting `stagingRootPath` is
  therefore **necessary** for C2 and **harmless-and-safer** for dismiss — one mechanism, correct either way.
- **Rejected (i):** re-resolve via a fresh `_getStagingRootPath()` everywhere — wrong for C2 (no
  `_stagingChainDir`, wrong volume on a relocated chain). **(ii)** reconstruct from `marker.managedChainPath`
  by re-running the realpath resolver at C2 time — fragile, duplicates resolution logic, and
  `managedChainPath` is the symlink not the realpath (task-ux-201 showed `path.dirname(managedChainPath)` is
  the wrong volume). Persisting the already-resolved root is simpler and exact.

### (d) Exact `node-start-verified` write + deferred clear (Option A)
**Chosen.** In `finalizeInstalledNodeStart`, after the `RUNNING` assertion (`:159-163`), **replace** the
`await clearMithrilPartialSyncMarker()` (`:165`) with
`await writeMithrilPartialSyncMarker('node-start-verified', { managedChainPath: marker.managedChainPath, stagingRootPath: marker.stagingRootPath })`,
then keep the existing `completed` emission (`:166-171`) unchanged. The marker clear is **deferred** to the
dismiss-driven `finalizeCompletedPartialSync`. This satisfies PRD D9 steps 1–3 and makes the `:60-63` C2
branch live: an interruption in the verify→clear window now finds `node-start-verified` and resumes a
**normal boot** (C2) instead of a C1 re-drive. Carrying `marker.stagingRootPath` forward keeps the durable
root available for both the dismiss finalize and the C2 reclaim. **Rejected Option B** (delete the unused
state) per PRD `:435-436` — it forfeits the lighter C2 recovery.

### (e) C2 reclaim: `fs.remove(stagingRoot)` + `fs-extra` import
**Chosen.** Extend `handleInterruptedRecovery`'s `:60-63` branch to, **before** clearing the marker,
`if (marker.stagingRootPath) { await fs.remove(marker.stagingRootPath); }`. `fs-extra` is **not** imported
in `mithrilPartialSyncNodeStartup.ts` today — add `import fs from 'fs-extra';`. Guard on
`marker.stagingRootPath` truthiness so a legacy/absent value is a safe no-op (the marker-clear + normal-boot
still happen). This closes the orphan-staging edge on the completed-but-not-dismissed window (PRD `:461-466`,
gap #41).

### (f) Service finalize idempotency
**Chosen.** `finalizeCompletedPartialSync()` is safe to call from **any** state (including already `idle`),
because each step is individually idempotent: `_resetToIdleStatus()` simply re-asserts the idle snapshot;
`fs.remove` on a non-existent path is a no-op (fs-extra); `clearMithrilPartialSyncMarker()` =
`fs.remove(markerPath)` on a possibly-absent file is a no-op. The method does **not** guard on current
status — calling it twice (double-dismiss) or when already idle is harmless. It also calls
`_clearRuntimeWorkState()` last (mirroring `finalizeWipeAndFullSync`) for parity, though by the dismiss
window runtime state is already cleared. It resolves the staging root from the marker first (Decision (c))
**before** `_resetToIdleStatus` mutates nothing relevant — order: read marker → resetToIdle → fs.remove(root)
→ clearMarker → clearRuntimeWorkState.

### (g) Tests: which, and to which specs
- **`mithrilPartialSyncMarker.spec.ts` (NEW)** — `stagingRootPath` round-trips through write/read.
- **`mithrilPartialSyncNodeStartup.spec.ts` (NEW)** — (1) `finalizeInstalledNodeStart` stamps
  `node-start-verified` (not clear) then emits `completed`; (2) the C2 branch `fs.remove`s
  `marker.stagingRootPath` then clears + returns `false` (normal boot); (3) interrupted-after-verification
  resumes normal boot (C2), not C1.
- **`MithrilPartialSyncService.spec.ts` (EXTEND)** — `finalizeCompletedPartialSync` resets to idle, removes
  the marker-persisted root, clears the marker exactly once; is idempotent from idle; persists
  `stagingRootPath` at the two cutover writes (assert the `writeMithrilPartialSyncMarker` mock calls include
  `stagingRootPath`).
- **`mithrilPartialSyncChannel.spec.ts` (EXTEND)** — the new finalize handler (mockChannels[6]) delegates to
  `controller.finalizePartialSync()`; add the mock method. **Caution:** the existing index-based assertions
  (`mockChannels[5]` = availability) stay valid since finalize appends at index 6.

## Files expected to change (exact paths)
1. `source/common/ipc/api.ts` — add the FINALIZE channel-name const + the two `void` request/response types
   (after `:487`).
2. `source/main/ipc/mithrilPartialSyncChannel.ts` — import the new const + types; add the `MainIpcChannel`
   const; add the `onRequest` handler calling `controller.finalizePartialSync()`.
3. `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` — add the renderer wrapper export (Decision (a)).
   **(In JSON only via task-ux-404; added here per the both-sides precedent — see Decision (a).)**
4. `source/main/mithril/MithrilController.ts` — add `finalizePartialSync()` (direct-to-service, Decision (b)).
5. `source/main/mithril/MithrilPartialSyncService.ts` — add `finalizeCompletedPartialSync()`; pass
   `stagingRootPath: this._getStagingRootPath()` at the two cutover marker writes (`:226`,`:230`).
6. `source/main/mithril/mithrilPartialSyncNodeStartup.ts` — `import fs from 'fs-extra'` +
   `writeMithrilPartialSyncMarker`; extend the `:60-63` C2 branch with `fs.remove(marker.stagingRootPath)`;
   replace the `:165` clear with the `node-start-verified` write.
7. `source/main/mithril/mithrilPartialSyncMarker.ts` — add `stagingRootPath?: string` to the type + write
   options.
8. **Specs:** new `mithrilPartialSyncMarker.spec.ts`, new `mithrilPartialSyncNodeStartup.spec.ts`; extend
   `MithrilPartialSyncService.spec.ts` and `mithrilPartialSyncChannel.spec.ts`.

**Targets in the JSON that do NOT change (and why):**
- `chainStorageCoordinator.ts` — finalize bypasses the coordinator (Decision (b)); no change.
- `MithrilStartupGate.ts` — `startInstalledNode`/`finalizeInstalledNodeStart`/`handleInterruptedRecovery`
  are called from it unchanged; the behavior change is inside the node-startup module.

## Implementation approach — ordered, mechanical steps

### Step 1 — `mithrilPartialSyncMarker.ts`: add `stagingRootPath`
- In the `MithrilPartialSyncMarker` type (`:11-15`), add a field:
  ```ts
  export type MithrilPartialSyncMarker = {
    state: MithrilPartialSyncMarkerState;
    updatedAt: string;
    managedChainPath?: string;
    stagingRootPath?: string;
  };
  ```
- In `writeMithrilPartialSyncMarker` (`:22-39`), extend the options type and spread it like
  `managedChainPath`:
  ```ts
  export async function writeMithrilPartialSyncMarker(
    state: MithrilPartialSyncMarkerState,
    options: { managedChainPath?: string; stagingRootPath?: string } = {}
  ): Promise<MithrilPartialSyncMarker> {
    const marker = {
      state,
      updatedAt: new Date().toISOString(),
      ...(options.managedChainPath
        ? { managedChainPath: options.managedChainPath }
        : {}),
      ...(options.stagingRootPath
        ? { stagingRootPath: options.stagingRootPath }
        : {}),
    };
    // …unchanged ensureDir/writeJson/return
  }
  ```
- `readMithrilPartialSyncMarker` (`:41-76`) needs **no change** — when valid it returns the parsed marker
  verbatim, so `stagingRootPath` passes through; the absent/invalid fallbacks (`null` /
  `{ state:'cutover-in-progress', updatedAt:'' }`) carry no staging path, which the consumers guard for.

### Step 2 — `MithrilPartialSyncService.ts`: persist `stagingRootPath` at the two cutover writes
- At `:226-228` change:
  ```ts
  await writeMithrilPartialSyncMarker('cutover-in-progress', {
    managedChainPath: context.layoutResult.managedChainPath,
    stagingRootPath: this._getStagingRootPath(),
  });
  ```
- At `:230-232` change:
  ```ts
  await writeMithrilPartialSyncMarker('installed-awaiting-node-start', {
    managedChainPath: context.layoutResult.managedChainPath,
    stagingRootPath: this._getStagingRootPath(),
  });
  ```
  Both are inside `start()` where `_stagingChainDir` is set, so `_getStagingRootPath()` resolves the
  colocated root.

### Step 3 — `MithrilPartialSyncService.ts`: add `finalizeCompletedPartialSync()`
Add a public method near `finalizeWipeAndFullSync()` (`:345-349`). It must read the marker BEFORE clearing,
to resolve the durable staging root:
```ts
async finalizeCompletedPartialSync(): Promise<void> {
  // PRD D9 step 4 (dismiss-driven). Idempotent: safe even when already idle (gap #41).
  // Resolve the staging root from the durable marker first so it is correct cross-session;
  // fall back to the in-session resolver if the marker carries no path.
  const marker = await readMithrilPartialSyncMarker();
  const stagingRoot = marker?.stagingRootPath ?? this._getStagingRootPath();

  this._resetToIdleStatus();
  await fs.remove(stagingRoot);
  await clearMithrilPartialSyncMarker();
  this._clearRuntimeWorkState();
}
```
- Add `readMithrilPartialSyncMarker` to the marker import at `:26-29`:
  ```ts
  import {
    clearMithrilPartialSyncMarker,
    readMithrilPartialSyncMarker,
    writeMithrilPartialSyncMarker,
  } from './mithrilPartialSyncMarker';
  ```
  (`fs` and `path` are already imported `:1-2`.)

### Step 4 — `MithrilController.ts`: add `finalizePartialSync()` (direct-to-service)
Add after `wipeAndFullSyncFromPartialSync()` (`:419-426`), BEFORE `_getPartialSyncDependencies()`:
```ts
async finalizePartialSync(): Promise<void> {
  // Dismiss-driven success finalize (PRD D9). No node orchestration → direct to the service,
  // bypassing the coordinator (Decision (b)). Idempotent / terminal-state only.
  await this._partialSyncService.finalizeCompletedPartialSync();
}
```
Do **NOT** add `finalize` to `_getPartialSyncDependencies()` (it does not go through the coordinator).

### Step 5 — `common/ipc/api.ts`: declare the channel + types
After the AVAILABILITY block (`:483-487`), add:
```ts
export const MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL =
  'MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL';
export type MithrilPartialSyncFinalizeRendererRequest = void;
export type MithrilPartialSyncFinalizeMainResponse = void;
```

### Step 6 — `main/ipc/mithrilPartialSyncChannel.ts`: main wrapper + handler
- Add `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL` to the value import (`:3-10`) and
  `MithrilPartialSyncFinalizeRendererRequest`/`MithrilPartialSyncFinalizeMainResponse` to the type import
  (`:11-24`).
- Add the channel const after `mithrilPartialSyncAvailabilityChannel` (`:53-56`):
  ```ts
  const mithrilPartialSyncFinalizeChannel: MainIpcChannel<
    MithrilPartialSyncFinalizeRendererRequest,
    MithrilPartialSyncFinalizeMainResponse
  > = new MainIpcChannel(MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL);
  ```
  **Construction order matters:** append it LAST (after availability) so existing
  `mockChannels[5]`-is-availability test assertions stay valid; the finalize handler becomes
  `mockChannels[6]`.
- Add the `onRequest` after the availability handler (`:116-118`):
  ```ts
  mithrilPartialSyncFinalizeChannel.onRequest(async () => {
    await controller.finalizePartialSync();
  });
  ```

### Step 7 — `renderer/app/ipc/mithrilPartialSyncChannel.ts`: renderer wrapper (Decision (a))
- Add `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL` to the value import (`:2-9`) and the two finalize types to the
  type import (`:10-23`).
- Append after `mithrilPartialSyncAvailabilityChannel` (`:50-53`):
  ```ts
  export const mithrilPartialSyncFinalizeChannel: RendererIpcChannel<
    MithrilPartialSyncFinalizeMainResponse,
    MithrilPartialSyncFinalizeRendererRequest
  > = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL);
  ```

### Step 8 — `mithrilPartialSyncNodeStartup.ts`: activate C2 + defer clear
- Imports: add `import fs from 'fs-extra';` (top, beside the electron import) and add
  `writeMithrilPartialSyncMarker` to the marker import (`:11-14`):
  ```ts
  import {
    clearMithrilPartialSyncMarker,
    readMithrilPartialSyncMarker,
    writeMithrilPartialSyncMarker,
  } from './mithrilPartialSyncMarker';
  ```
- **C2 branch** (`:60-63`) — reclaim staging before clearing:
  ```ts
  if (marker.state === 'node-start-verified') {
    // Boundary C2: a prior run already proved one successful node start on the installed DB.
    // Reclaim leftover staging on a close-without-dismiss / crash (PRD D9, gap #41), then resume
    // a normal boot. stagingRootPath is the durable colocated root persisted at cutover.
    if (marker.stagingRootPath) {
      await fs.remove(marker.stagingRootPath);
    }
    await clearMithrilPartialSyncMarker();
    return false;
  }
  ```
- **`finalizeInstalledNodeStart`** — replace the clear at `:165` with the verified-marker write; keep the
  `completed` emit (`:166-171`) unchanged:
  ```ts
    if (this._cardanoNode.state !== CardanoNodeStates.RUNNING) {
      throw new Error(
        'Cardano node stopped responding during startup after Mithril partial sync cutover.'
      );
    }

    // PRD D9 steps 2–3: stamp node-start-verified (Boundary C2) and emit completed. The marker clear
    // is DEFERRED to the dismiss-driven service finalize (finalizeCompletedPartialSync). Carry the
    // durable stagingRootPath forward so the dismiss finalize / C2 reclaim can remove the exact dir.
    await writeMithrilPartialSyncMarker('node-start-verified', {
      managedChainPath: marker.managedChainPath,
      stagingRootPath: marker.stagingRootPath,
    });
    await emitMithrilPartialSyncStatus({
      ...getMithrilPartialSyncStatus(),
      status: 'completed',
      allowedRecoveryActions: [],
      error: null,
    });
  ```

### Step 9 — No other production changes
The startup-gate call sites (`MithrilStartupGate.ts:482,503`) and the coordinator are unchanged; the
behavior change lives entirely inside the modules above.

## Locked invariants this task honors (inline)
- **#1 Staged-only restore / no in-place mutation:** finalize and the C2 reclaim only `fs.remove` the
  **scratch staging dir** (never the live chain; the installed chain has already started successfully).
  The marker writes are unchanged in semantics; no new write touches the live chain.
- **#2 Boundary recovery is backend-authoritative:** activating `node-start-verified` realizes the backend
  PRD's Boundary C2 (`mithril-partial-sync-prd.md:187-194`) — `restart-normal`/`wipe` on C2, `retry`
  rejected. The C2 branch resumes a normal boot from the **marker state**, not from any renderer inference.
  No renderer recovery change here.
- **#6 Cancellation forbidden after cutover:** untouched — the `cancel()` guard
  (`MithrilPartialSyncService.ts:270-274`) is not modified; finalize is a distinct terminal-only path.
- **#8 No synthetic throughput / no raw JSON in UI:** finalize and the marker carry only paths/states, never
  mithril-client JSON; no progress/% is synthesized.
- **#9 / locked decision #16 (as amended by D9):** the overlay still follows the lifecycle through
  `completed` (we keep the `completed` emission and do **NOT** reset to idle at verified success) and clears
  only on explicit user dismiss; **on that dismiss** the backend resets to idle, removes staging, and clears
  the marker. The success screen stays visible until dismiss because the backend stays at `completed` (not
  `idle`) for the whole completed-but-not-dismissed window (marker `node-start-verified`).
- **#10 Kill switch:** unchanged — finalize runs only after a sync that the kill switch already gated.
- **#11 No empty-chain bootstrap regression:** this task touches only the partial-sync marker/finalize/C2
  paths; the bootstrap flow and shared progress components are untouched.

## Acceptance criteria (carried verbatim from tasks JSON)
1. A finalize/dismiss channel exists and is routed to the service idempotently.
2. node-start-verified is written after the node is proven RUNNING; Boundary C2 startup branch is live.
3. Verified success keeps the completed overlay visible; reset + staging removal + marker clear happen on
   dismiss.
4. Close-without-dismiss reclaims leftover staging via the C2 branch on next launch.
5. Locked decision #16 amendment (D9) is implemented as documented.

## Verification plan (exact commands)
From repo root:
- `./node_modules/.bin/tsc --noEmit` — type-check (project memory: `yarn compile` fails only on the
  pre-existing `typedef:sass` hook under Node 24; tsc directly is the source of truth).
- `./node_modules/.bin/jest source/main/mithril/mithrilPartialSyncMarker.spec.ts source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts source/main/mithril/MithrilPartialSyncService.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  — the new + extended specs pass.
- `./node_modules/.bin/eslint source/common/ipc/api.ts source/main/ipc/mithrilPartialSyncChannel.ts source/renderer/app/ipc/mithrilPartialSyncChannel.ts source/main/mithril/MithrilController.ts source/main/mithril/MithrilPartialSyncService.ts source/main/mithril/mithrilPartialSyncNodeStartup.ts source/main/mithril/mithrilPartialSyncMarker.ts`
  — clean on touched files (compare warning count to pre-change baseline).

### Test plan (map each tasks-JSON testCase to a concrete test)

1. **"Verified success stamps node-start-verified then emits completed; marker not cleared until dismiss"**
   → `mithrilPartialSyncNodeStartup.spec.ts`: mock `readMithrilPartialSyncMarker` to return
   `{ state:'installed-awaiting-node-start', managedChainPath:'/chain', stagingRootPath:'/vol/mithril-partial-sync' }`;
   stub `cardanoNode.state = RUNNING`, `getGeneration` stable, and the `waitForNodeStartupProof` delay (use
   fake timers or a small mock). Call `finalizeInstalledNodeStart(gen)`; assert
   `writeMithrilPartialSyncMarker` was called once with `'node-start-verified'` and
   `{ managedChainPath:'/chain', stagingRootPath:'/vol/mithril-partial-sync' }`, that
   `clearMithrilPartialSyncMarker` was **NOT** called, and that `emitMithrilPartialSyncStatus` was called
   with `status:'completed'`.

2. **"Dismiss-driven finalize resets to idle, removes stagingRoot, and clears the marker exactly once"**
   → `MithrilPartialSyncService.spec.ts`: mock `readMithrilPartialSyncMarker` →
   `{ state:'node-start-verified', stagingRootPath:'/vol/mithril-partial-sync' }`. Call
   `service.finalizeCompletedPartialSync()`; assert `fs.remove` was called with
   `'/vol/mithril-partial-sync'`, `clearMithrilPartialSyncMarker` exactly once, and `service.status.status
   === 'idle'` with empty `progressItems`/`allowedRecoveryActions`. Add a sub-assertion that with **no**
   marker stagingRootPath it falls back to `_getStagingRootPath()`.

3. **"Finalize channel is idempotent (safe when already idle)"** → two parts:
   (a) `MithrilPartialSyncService.spec.ts`: call `finalizeCompletedPartialSync()` twice in a row with the
   marker absent (`readMithrilPartialSyncMarker` → `null`); assert no throw, status stays `idle`,
   `fs.remove`/`clearMarker` are no-op-safe (called, resolve). (b) `mithrilPartialSyncChannel.spec.ts`: add
   `finalizePartialSync: jest.fn()` to the controller mock; after `handleMithrilPartialSyncRequests`, invoke
   `mockChannels[6].onRequest.mock.calls[0][0]()` and assert `controller.finalizePartialSync` called once
   and resolves to `undefined`.

4. **"Close-without-dismiss leaves node-start-verified; next launch C2 branch resumes normal boot and
   reclaims staging"** → `mithrilPartialSyncNodeStartup.spec.ts`: mock `readMithrilPartialSyncMarker` →
   `{ state:'node-start-verified', stagingRootPath:'/vol/mithril-partial-sync' }`. Call
   `handleInterruptedRecovery(gen)`; assert `fs.remove('/vol/mithril-partial-sync')` then
   `clearMithrilPartialSyncMarker` were called and the return value is `false` (normal boot, not blocked).
   Add a variant with **no** `stagingRootPath` → `fs.remove` not called, still clears + returns `false`.

5. **"Interrupted-after-verification run resumes a normal boot (C2), not a C1 re-drive"** → same spec:
   assert that for a `node-start-verified` marker, `handleInterruptedRecovery` does **NOT** call
   `emitMithrilPartialSyncStatus` with `status:'failed'` and does **NOT** invoke `dialog.showMessageBox`
   (the C1/unsafe-cutover surface), and returns `false` — i.e. the heavier C1 re-drive path is not taken.

**Marker round-trip (supporting test, `mithrilPartialSyncMarker.spec.ts`):** with `fs.writeJson`/`fs.readJson`
mocked, `writeMithrilPartialSyncMarker('installed-awaiting-node-start', { managedChainPath:'/c',
stagingRootPath:'/s' })` writes a marker object containing `stagingRootPath:'/s'`, and
`readMithrilPartialSyncMarker` returns it verbatim (state valid). Also assert `stagingRootPath` is **omitted**
when not provided (parity with `managedChainPath`).

**Cutover-write persistence (supporting test, `MithrilPartialSyncService.spec.ts`):** in an existing
`start()`-driving test, assert the `writeMithrilPartialSyncMarker` mock was called for
`'cutover-in-progress'` and `'installed-awaiting-node-start'` with an options object whose `stagingRootPath`
equals the colocated `_getStagingRootPath()` (e.g. `/tmp/mithril-partial-sync` for the default fixture).

## Risks / open questions
- **`readMithrilPartialSyncMarker` invalid-fallback has no `stagingRootPath` (ACCEPTED).** A corrupt/absent
  marker returns `{ state:'cutover-in-progress', updatedAt:'' }` — no staging path. That path is the
  unsafe-cutover (Boundary B/C1) branch, which is handled by wipe-and-full-sync (which re-derives and wipes
  the chain anyway), so a missing staging path there cannot leak the **success** staging dir. The C2 and
  dismiss consumers both guard `stagingRootPath` truthiness.
- **Double-dismiss / dismiss-after-restart (RESOLVED, Decision (f)).** Finalize is idempotent; a second call
  (or a dismiss arriving after a manual restart already cleared the marker) is a safe no-op.
- **Renderer wrapper added here despite JSON omission (RESOLVED, Decision (a)).** Strictly additive,
  unused-yet export; the both-sides precedent (task-ux-101 availability) makes this the conventional shape.
  Recorded in research for the task-ux-404 hand-off.
- **`fs-extra` default import in node-startup (LOW).** The module currently imports no `fs`; adding
  `import fs from 'fs-extra'` matches the service's import style (`MithrilPartialSyncService.ts:2`). Verify
  no lint `no-unused-vars` if a branch is guarded out (it is used in the C2 branch).
- **In-session reliance for the happy-path dismiss (MITIGATED).** The happy path is in-session (verified via
  `index.ts:250-252`, no relaunch), but the marker-persisted `stagingRootPath` keeps the dismiss correct
  even if a future change makes it cross-session — defense-in-depth, no extra cost.

## Required doc / research updates
- `task-ux-202-research.md` (this sprint): record the in-session-vs-cross-session evidence
  (`index.ts:250-252` no relaunch; `MithrilStartupGate.ts:196,503`), the marker-`stagingRootPath` decision
  and why re-resolution fails for C2, the direct-to-service controller routing rationale, the PRD anchor
  drift (`:205/:209` → live `:226/:230`), and the renderer-wrapper boundary reconciliation (hand-off to
  task-ux-404).
- No PRD edit required — D9 already specifies the sequence; the `stagingRootPath` marker field is the
  engineering realization of D9's cross-process staging removal. Optionally note the new channel name
  `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL` and renderer export `mithrilPartialSyncFinalizeChannel` for
  task-ux-404.

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-202-plan-review.md`
  (append-only).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-202-impl-review.md`
  (created at impl time).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-202-research.md` (durable
  research note).

## Final outcome

**Planning status: `approved` · Build status: `completed`.** Implemented exactly as planned (zero design
deviations); plan review `approved` (`task-ux-202-plan-review.md`), implementation review `approved`
(`task-ux-202-impl-review.md`). Verified green at finalize time (re-run by SCRIBE 2026-06-22):
`./node_modules/.bin/tsc --noEmit` → exit 0; the 4 target specs → **61/61 pass**.

### What was implemented

**New finalize IPC channel (`void → void`).**
- `source/common/ipc/api.ts:489-493` — `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL` const +
  `MithrilPartialSyncFinalizeRendererRequest = void` / `MithrilPartialSyncFinalizeMainResponse = void`,
  appended after the AVAILABILITY block.
- `source/main/ipc/mithrilPartialSyncChannel.ts` — `mithrilPartialSyncFinalizeChannel` `MainIpcChannel`
  constructed **LAST** (index 6, after availability at 5) `:61-64`; `onRequest` handler `:127-129` calling
  `controller.finalizePartialSync()`.
- `source/renderer/app/ipc/mithrilPartialSyncChannel.ts:58-61` — renderer wrapper export
  `mithrilPartialSyncFinalizeChannel` added here too (Decision (a) / task-ux-101 both-sides precedent;
  strictly additive, unused-yet — task-ux-404 imports it with no churn). **Reconciles** the JSON
  targetPaths conflict (renderer file omitted from task-ux-202, present in task-ux-404).

**Controller route (direct-to-service, no coordinator).**
- `source/main/mithril/MithrilController.ts:428-432` — `finalizePartialSync()` calls
  `this._partialSyncService.finalizeCompletedPartialSync()` **directly** (finalize does no node
  orchestration → bypasses the coordinator + handlers bag; Decision (b)). `_getPartialSyncDependencies()`
  unchanged.

**Service finalize method (PRD D9 step 4).**
- `source/main/mithril/MithrilPartialSyncService.ts:354-365` — `finalizeCompletedPartialSync()`. Ordered:
  read marker → resolve `marker?.stagingRootPath ?? this._getStagingRootPath()` → `_resetToIdleStatus()` →
  `fs.remove(stagingRoot)` → `clearMithrilPartialSyncMarker()` → `_clearRuntimeWorkState()`. Idempotent
  (safe from idle / double-dismiss). `readMithrilPartialSyncMarker` added to the marker import `:26-30`.
- The `_resetToIdleStatus()` re-arms the backend half of BUG1 (`isPartialSyncActive()` returns
  `status !== 'idle'`, `MithrilController.ts:194-199`).

**Marker `stagingRootPath` field (cross-session staging-path resolution, Decision (c)).**
- `source/main/mithril/mithrilPartialSyncMarker.ts:15` — added `stagingRootPath?: string` to the
  `MithrilPartialSyncMarker` type; `:25` extended `writeMithrilPartialSyncMarker` options; `:33-35` spread
  it (omitted-when-absent, mirroring `managedChainPath`). `readMithrilPartialSyncMarker` unchanged (passes
  the field through verbatim when valid).
- `MithrilPartialSyncService.ts:228,232` — both cutover writes (`cutover-in-progress`,
  `installed-awaiting-node-start`) now pass `stagingRootPath: this._getStagingRootPath()`, captured while
  `_stagingChainDir` is set so it resolves the task-ux-201 **colocated** root.

**node-start-verified activation + deferred clear (Option A, Decision (d)).**
- `source/main/mithril/mithrilPartialSyncNodeStartup.ts:170-184` — in `finalizeInstalledNodeStart`, after
  the `RUNNING` assertion, the marker **clear** was **replaced** with
  `writeMithrilPartialSyncMarker('node-start-verified', { managedChainPath: marker.managedChainPath, stagingRootPath: marker.stagingRootPath })`;
  the `completed` emission is preserved. The clear is **deferred** to the dismiss-driven service finalize.
  This makes the previously-dead C2 startup branch live. `writeMithrilPartialSyncMarker` added to the
  marker import `:13-16`.

**C2 reclaim (close-without-dismiss / crash, Decision (e)).**
- `mithrilPartialSyncNodeStartup.ts:1` — `import fs from 'fs-extra'` added (module had no `fs`).
- `:62-71` — the `node-start-verified` branch now, before clearing, does
  `if (marker.stagingRootPath) { await fs.remove(marker.stagingRootPath); }` then `clearMarker()` +
  `return false` (normal boot, not C1). Runs cross-process on a fresh boot
  (`MithrilStartupGate.ts:196`, `_stagingChainDir = null`) — uses the **exact persisted** path, never
  re-resolved.

### Tests added/extended (61 tests across 4 specs, all pass)
- `source/main/mithril/mithrilPartialSyncMarker.spec.ts` (**NEW**, 8 tests) — `stagingRootPath` write/read
  round-trip; omission-when-absent parity; `node-start-verified` carry-forward; corrupt-marker fallback has
  no `stagingRootPath`.
- `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` (**NEW**, 9 tests) — `finalizeInstalledNodeStart`
  stamps `node-start-verified` (does NOT clear) + emits `completed`; throws on non-RUNNING; early-return on
  wrong state; C2 branch removes `stagingRootPath` then clears + returns `false`; skips remove when absent;
  C2 does NOT emit `failed` / does NOT call `dialog.showMessageBox` (not C1). `setTimeout` patched via
  `jest.spyOn(global, 'setTimeout')` to resolve immediately.
- `source/main/mithril/MithrilPartialSyncService.spec.ts` (**EXTENDED**, 25→32) — `finalizeCompletedPartialSync`
  resets to idle + removes marker-persisted stagingRoot + clears once; falls back to `_getStagingRootPath()`
  when marker has no path (`/tmp/daedalus-state/mithril-partial-sync`); idempotent from idle (twice, no
  throw); both cutover writes include `stagingRootPath` (`/tmp/mithril-partial-sync`).
  `readMithrilPartialSyncMarker: jest.fn()` added to the module mock + `beforeEach` (the plan-review MINOR
  was addressed).
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts` (**EXTENDED**, 7→12) — `finalizePartialSync: jest.fn()`
  on the controller mock; `mockChannels[6]` delegates to `controller.finalizePartialSync()` + resolves
  `undefined`; finalize channel distinct from availability(5)/status(1). Existing `mockChannels[5]`
  assertions unaffected.

### Verification results
- `./node_modules/.bin/tsc --noEmit` → **exit 0** (clean; `yarn compile`'s `typedef:sass` hook is the
  Node-24 gotcha, not used here).
- `./node_modules/.bin/jest` on the 4 target specs → **61/61 pass, 0 failures** (impl: marker 8, nodeStartup
  9, service 32, channel 12). Code review independently re-ran broader `jest source/main/mithril
  source/main/ipc` → **150/150 pass** (no collateral regression).
- `./node_modules/.bin/eslint` on the 7 production files → **0 errors, 30 warnings** (all pre-existing
  `no-unused-vars` on untouched code; no new warning; the new `fs-extra` import is consumed in the C2 branch).
  Spec files: 0 errors, 0 warnings.

### Deviations
**None.** All 9 ordered mechanical steps implemented as specified. The only implementation-level choice not
spelled out in the plan was the test technique for the node-startup delay — `jest.spyOn(global, 'setTimeout')`
to resolve immediately rather than fake timers; the plan permitted "fake timers or a small mock", so this is
within scope.

### Hand-offs (not in scope here)
- **task-ux-404** — renderer `dismissCompletedOverlay` must invoke `mithrilPartialSyncFinalizeChannel.send()`
  (wrapper already exported); plus the renderer CTA / sync-status-after-cancel work.
- **task-ux-301** — `isActionBlocked` derived from `isWorking` vs `isActive` (renderer CTA re-arm half of
  BUG1) — not touched.
- **task-ux-503** — cross-cutting E2E/integration coverage of the full dismiss→reset→C2-reclaim loop (this
  task's tests are unit-level; no E2E added here).

### Code-review note worth carrying forward
The deferred-clear in-session overlay survival depends on `MithrilStartupGate._startupCheckDone` (`:213`)
short-circuiting `handleInterruptedRecovery` on the in-session `restartStartupFlow` re-entry (it is reset
only by `resetOnDirectoryChange`, NOT by `restartStartupFlow`). So the in-session re-entry does NOT
prematurely reclaim staging / clear the marker; the `completed` overlay stays visible until the explicit
dismiss IPC. Correct today; flagged in case startup-gate flow changes (potential task-ux-204/701 interaction).

## Status
- Planning status: `approved`
- Build status: `completed`
