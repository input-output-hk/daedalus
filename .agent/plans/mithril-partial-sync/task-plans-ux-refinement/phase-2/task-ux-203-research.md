# task-ux-203 — Research note (durable findings)

> Backend half of PRD **D5(f)** / research-19 **gap #39**: guarantee a cancel request always re-emits a
> status so the renderer never sticks on its optimistic `stopping-node` frame. This note captures the durable
> reasoning that is NOT obvious from the one-line diff: the channel-file no-change disposition (with the full
> status-push trace), the stopping-node defer decision, the `_updateStatus({})` re-emit semantics, the
> anchor drift found at planning/impl time, and the task-ux-404 hand-off.

- Sprint: Mithril Partial Sync UX Refinement — phase-2 (Backend UX-Enabling Correctness)
- Source file changed: `source/main/mithril/MithrilPartialSyncService.ts` (`cancel()` early-return) + its spec.
- Created at finalize time (2026-06-24) by the SCRIBE; line numbers re-verified against live code.

---

## 1. The seam and the fix (verified live)

`MithrilPartialSyncService.cancel()` (`MithrilPartialSyncService.ts:264`) has two guard branches:

1. **Node-stop-window early-return** (`:265-276`): `if (!this._activeWorkDir && !this._currentProcess)`.
   This is the multi-minute window where the node is being stopped/restarted and the service holds **no**
   active work refs. Before this task it ended in a bare `return;` that emitted **nothing** — so a Cancel
   click here produced zero status, and the renderer's optimistic `stopping-node` frame had nothing to
   reconcile against and looked dead (issue #10 comment, gap #39).
   **Fix:** the bare `return;` is now preceded by `this._updateStatus({});` (`:274`) — a verbatim re-emit of
   the TRUE current status.
2. **Post-cutover hard-rejection** (`:278-282`, drifted down 5 lines from the added block):
   `if (['installing','finalizing'].includes(this._status.status)) { throw ... }` — **LOCK #6**, left
   **byte-unchanged**. It is mutually exclusive with branch 1 (the throw requires `_activeWorkDir` set, which
   makes the early-return guard false), so the re-emit can never leak into the throw path. Test B is the guard.

### Why `_updateStatus({})` and not a fabricated `cancelled` / a new helper
- `_updateStatus(update)` (`:847-879`) with an empty partial sets `nextStatus = this._status.status`
  (`:848`), re-merges `this._status` over itself (no-op spread), and fires
  `this._statusEmitter.emit('status', { ...this._status })` (`:878`). The emitted object is the current
  status **verbatim**. This is the existing, tested re-emit primitive — reused per the orchestrator's
  reuse-over-abstraction preference; a dedicated `_emitCurrentStatus()` helper was rejected as unnecessary.
- **No fabricated `cancelled`/`failed`.** There is nothing active to cancel in the node-stop window.
  Asserting `status:'cancelled'` would also assert a recovery-action surface
  (`['retry','restart-normal','wipe-and-full-sync']`) that does not apply — a lie to the renderer and a
  **lock #2** violation. The truth (the existing status) is exactly what lets the renderer reconcile its
  optimistic overlay.
- **Benign `elapsedSeconds` edge (accepted).** `_updateStatus` only refreshes
  `transferProgress.elapsedSeconds` for statuses where `_shouldTrackElapsed` is true (`:881-884`), which
  **excludes** `idle`/`cancelled`/`failed`/`completed` — exactly the terminal-ish statuses present in the
  node-stop window. So in that window the re-emit is a **pure verbatim re-emit** (nothing refreshed). Even in
  the theoretical case where a tracking status were live, `elapsedSeconds` is true wall-clock elapsed, not a
  synthesized throughput/%/remaining-time number → **lock #8 intact** either way.

---

## 2. Channel-file disposition — `mithrilPartialSyncChannel.ts` does NOT change (durable decision)

`source/main/ipc/mithrilPartialSyncChannel.ts` is in the tasks-JSON `targetPaths` for task-ux-203, but the
verified disposition is **no change needed**. The reasoning is durable because it is the kind of thing a
future reader will re-question ("the JSON lists the channel — did we forget it?"). The answer is no: the
status **PUSH** that the renderer relies on is already fully wired and is **independent of the cancel
channel**. Full trace (file:line, re-verified live at finalize time):

1. `mithrilPartialSyncChannel.ts:100-102` — `controller.setPartialSyncStatusSender(async (status) => { await mithrilPartialSyncStatusChannel.send(status, window.webContents); })`
   installs the sender that pushes **any** status to the renderer over the dedicated **STATUS** channel.
2. `mithrilPartialSyncChannel.ts:104` — `controller.initialize()` runs the subscription wiring.
3. `MithrilController.ts:111-112` — `this._partialSyncService.onStatus((status) => { this.broadcastPartialSyncStatus(status)... })`
   subscribes the controller to **every** service status emission.
4. `MithrilController.ts:253-263` — `broadcastPartialSyncStatus(status)` sets
   `this._partialSyncStatus = status` (`:256`, keeping the `getPartialSyncStatus()` pull fresh too) and,
   when a sender is installed (`:260`), dispatches `this._partialSyncStatusSender?.(status)` (`:263`) → push.

So when `cancel()` calls `_updateStatus({})`, the EventEmitter fires
(`MithrilPartialSyncService.ts:878`) → controller `broadcastPartialSyncStatus` → `_partialSyncStatusSender`
pushes the snapshot over the STATUS channel to the renderer. The **cancel** channel itself
(`mithrilPartialSyncCancelChannel`, declared `:41`, handler `:115-117`:
`onRequest(async () => { await controller.cancelPartialSync(); })`) is `void → void` and is **not** a
carrier of status — adding anything to it would be redundant. The controller cancel route
(`MithrilController.ts:404-405` → `chainStorageCoordinator.cancelPartialSync(...)`) is likewise a thin
pass-through and carries no status.

**Disposition: no edit to `mithrilPartialSyncChannel.ts`; the service-level re-emit is sufficient.** This is
the JSON-`targetPaths` reconciliation. (Independently re-traced by both the plan critiquer and the code
reviewer; `git diff` on the channel file is empty.)

### Corollary for task-ux-404's pull
task-ux-404 will additionally call the renderer `syncStatus()` after any cancel; that pull resolves via
`getPartialSyncStatus()` (`MithrilController.ts:163-165`) → `_partialSyncStatus`, which
`broadcastPartialSyncStatus:256` refreshes with the **same** re-emitted snapshot. So **both** the push (this
task) and the pull (task-ux-404) land on a non-stale source — no coordination gap.

---

## 3. Optional `stopping-node` backend emit — DEFERRED (durable rationale)

The tasks-JSON implementationNote optionally allows the backend to emit `stopping-node` when it actually
stops the node (pairs with D8#19). **Deferred**, with evidence:

- **`stopping-node` is documented renderer-optimistic-only** (research-19 section 3). Making the backend
  author it is a separate, broader status-model change, not the cancel-resync fix this task scopes.
- **The service does NOT stop the node on the cancel path.** The coordinator's `cancelPartialSync`
  (`source/main/utils/chainStorageCoordinator.ts:274-277`) is a thin pass-through —
  `await dependencies.handlers.cancel();` — with **no `_ensureNodeStopped…` and no mutation lock**. Contrast
  `restartStartupFlowFromPartialSync`/`wipeAndFullSyncFromPartialSync`, which DO call
  `_ensureNodeStoppedForPartialSyncAction` (`:297`, `:325`). The node-stop that produces the multi-minute
  window is orchestrated **elsewhere** (the renderer's optimistic frame / startup-flow re-entry), **not**
  inside `MithrilPartialSyncService`. Emitting `stopping-node` from the service would mean inventing a
  transition the service does not own — larger than the smallest-truthful change and risks colliding with
  D8#19.
- **Not required for acceptance.** A cancel request producing *a* status emission (the re-emit of the true
  current status) fully satisfies criterion 1 ("the renderer can act on it / never sticks"). The
  truthful-`stopping-node` nicety is independent polish left to D8#19 / the backend-correctness track.

---

## 4. Anchor drift found (prefer live code; recorded for future readers)

| Anchor | Cited (JSON/PRD/plan) | Live (finalize time) | Note |
|--------|-----------------------|----------------------|------|
| `cancel()` early-return | `MithrilPartialSyncService.ts:241-247` (JSON/PRD) | `:265-276` | bare `return` was at `:270`; now followed by `_updateStatus({})` at `:274` |
| Post-cutover throw (lock #6) | `:249-253` (JSON/PRD) | `:278-282` | drifted down 5 lines after the added block; byte-unchanged content |
| `_updateStatus` / emit | `:842-874` / emit `:873` (plan, pre-edit) | `:847-879` / emit `:878` | shifted +5 by the added lines (expected) |
| `_shouldTrackElapsed` | `:876-880` (plan, pre-edit) | `:881-884` | shifted +5 |
| `get status()` / `onStatus` / `_statusEmitter` / `_status` | `:122-131` / `:100` / `:99` | `:122` / `:126` / `:100` / `:99` | accurate |
| Controller push wiring | `:111-118`, `:253-269`, `:256`, `:263` | `:111-112`, `:253-263`, `:256`, `:263` | accurate (block is slightly shorter live) |
| `getPartialSyncStatus()` | `:163-165` | `:163` | accurate |
| Controller cancel route / coordinator pass-through | route `:404-408`; coordinator `utils/chainStorageCoordinator.ts:274-278` | route `:404-405`; coordinator **`source/main/utils/chainStorageCoordinator.ts:274-277`** | **PATH drift:** the coordinator lives at `source/main/utils/`, NOT `source/main/mithril/utils/` as the plan/202-template cited. Same `handlers.cancel()` pass-through content. |

The drift is benign (additive lines + a path typo in the plan) and was correctly resolved in favor of live
code. Recorded so a future task that greps the plan's coordinator path does not chase
`source/main/mithril/utils/chainStorageCoordinator.ts` (which does not exist).

---

## 5. Tests (durable: what proves what)

Both new tests live in `source/main/mithril/MithrilPartialSyncService.spec.ts`, inserted after the existing
post-cutover-throw test (`:781-797`):

- **Test A — `re-emits the current status when cancel is requested during the node-stop window`**
  (`:799-825`). `_activeWorkDir`/`_currentProcess` both `null`, `_status.status = 'completed'`; subscribes via
  `service.onStatus`; `await service.cancel()` resolves; asserts **exactly one** emission, that it equals the
  TRUE current status (`objectContaining({ status:'completed', allowedRecoveryActions:[] })`, i.e. **no**
  fabricated `cancelled`), and `service.status.status === 'completed'`. This is the direct proof that the
  previously-silent early-return now emits (coverage report lists line `274` as covered).
- **Test B — `does not emit a status when post-cutover cancel hard-rejects (lock #6 regression guard)`**
  (`:827-849`). `_activeWorkDir` set + `status:'installing'` so the early-return guard is bypassed and the
  throw fires; asserts the rejection message is unchanged AND **zero** emissions on the throw path (proves the
  early-return re-emit did not leak into the throw branch).

`MithrilPartialSyncStatusSnapshot` was already imported in the spec; no new mocks needed (the early-return
path calls no marker/fs/runner functions before re-emitting — `_updateStatus` only mutates in-memory state
and emits).

---

## 6. Hand-offs

- **task-ux-404 (renderer half of D5(f) / gap #39).** Disable the Cancel button during `stopping-node` with a
  tooltip, and **always** call `syncStatus()` after any cancel. The pull lands on the same re-emitted truth
  via `getPartialSyncStatus()` → `_partialSyncStatus` (refreshed at `broadcastPartialSyncStatus:256` by this
  task's re-emit). gap #9 (post-cutover Cancel button visibility) is also renderer-side, NOT this task.
- **D7 / backend-correctness track.** True abort-during-stop (a cancel flag honored *before* the node-stop
  handler) remains deferred — out of scope here. This task only guarantees a resync target exists; it does
  not make the in-flight node-stop itself abortable.
- **D8#19 / status-model track.** Truthful backend-emitted `stopping-node` remains deferred (section 3).
- **task-ux-503 / cross-cutting coverage.** This task's tests are unit-level (service spec). Any
  integration/E2E coverage of the cancel-during-stop UX loop (renderer disables Cancel → optimistic frame →
  backend re-emit → renderer reconciles) belongs to the cross-cutting coverage task.

---

## 7. Locked invariants honored (summary)

- **#6** — post-cutover throw (`:278-282`) byte-unchanged; the re-emit is on the mutually-exclusive
  pre-cutover early-return; Test B guards it.
- **#2** — re-emit carries the existing `allowedRecoveryActions` verbatim; no recovery surface fabricated.
- **#8** — existing in-memory snapshot; no synthesized throughput/%/remaining-time; no raw mithril-client
  JSON routed; `elapsedSeconds` not even refreshed for the node-stop-window statuses.
- **#11** — only the partial-sync `cancel()` early-return path touched; bootstrap flow untouched.

No PRD edit required: D5(f) already specifies the always-resync behavior; this task is its backend half.
