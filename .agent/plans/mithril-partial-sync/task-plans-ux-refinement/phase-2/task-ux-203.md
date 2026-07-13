# task-ux-203 — Guarantee cancel always re-emits status (backend half of cancel-during-stop fix)

- Sprint: Mithril Partial Sync UX Refinement — phase-2 (Backend UX-Enabling Correctness)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `completed`
- Interaction mode: `autonomous`
- Priority: high · Estimated: 3h · Dependencies: none

## Why now
A cancel request must **always** result in a status emission so the renderer can never stick on its
optimistic `stopping-node` frame. Today `MithrilPartialSyncService.cancel()` **early-returns silently** when
both `_activeWorkDir` and `_currentProcess` are null — the multi-minute node-stop window — emitting
**nothing** (`MithrilPartialSyncService.ts:265-271`, the bare `return` at `:270`):

```ts
async cancel(): Promise<void> {
  if (!this._activeWorkDir && !this._currentProcess) {
    logger.info(
      'MithrilPartialSyncService: ignoring cancel request with no active partial sync',
      null
    );
    return;                       // <- silent: emits no status; renderer optimistic frame looks dead
  }
  ...
```

The reporter clicked Cancel during node-stop and nothing happened (issue #10 comment). Because the cancel
channel is `void → void` and carries no status, and the only path that pushes a fresh status to the renderer
is a service `onStatus` emission (traced below), a cancel that returns silently leaves the renderer with no
new status to land on — the optimistic frame never resolves.

This task is the **BACKEND HALF** of PRD **D5(f)** / research-19 **gap #39**: make `cancel()` re-emit the
**true current status** on the early-return path so the renderer resyncs. The **RENDERER HALF** (disable
Cancel during `stopping-node` with a tooltip + always call `syncStatus()` after any cancel) is
**task-ux-404** — NOT in scope here. True abort-during-stop (a cancel flag honored *before* the node-stop
handler) is deferred to the backend-correctness track (D7) — also NOT in scope.

## Interaction mode justification
`autonomous`: backend-only. No user-facing copy is authored (the tooltip/disable copy is task-ux-404). No
destructive or irreversible action — the change only **re-emits an existing in-memory status snapshot** over
the already-wired status push; it does not stop/start the node, touch the chain, mutate the marker, or fabricate
state. PRD D5(f) and gap #39 fully specify the behavior (emit on cancel so the UI never sticks), so there is
no open product decision. The smallest-truthful change is a one-line replacement of a bare `return` with a
re-emit of the current status.

## Scope
1. **`cancel()` early-return re-emit** — replace the silent `return` (`MithrilPartialSyncService.ts:270`)
   with a re-emit of the **current** status (`this._updateStatus({})`) immediately before returning, so the
   EventEmitter fires and the existing push pipeline delivers the true status to the renderer.
2. **Tests** — extend `source/main/mithril/MithrilPartialSyncService.spec.ts`:
   - cancel during the node-stop window (`_activeWorkDir` & `_currentProcess` both null) now **emits** a
     status the renderer can resync to (assert via `service.onStatus`), and the emitted status equals the
     current `service.status` (no fabricated `cancelled`/`failed`);
   - post-cutover cancel **still throws unchanged** (regression guard for lock #6).

## Non-goals
- **Do NOT** change the post-cutover cancel hard-rejection
  (`MithrilPartialSyncService.ts:273-277`) — **lock #6**, stays byte-unchanged.
- **Do NOT** fabricate a `cancelled` (or `failed`) status on the early-return path. There is nothing to
  cancel in that window — re-emit the **TRUE current status** so the renderer replaces its optimistic frame
  with the truth. (Fabricating `cancelled` would assert a recovery surface
  (`allowedRecoveryActions:['retry','restart-normal','wipe-and-full-sync']`) that does not apply when no sync
  is active — a lie to the renderer.)
- **Do NOT** synthesize any throughput / remaining-time / overall-% or route raw mithril-client JSON into the
  emission — **lock #8**. Re-emitting an existing snapshot is fine; no new numbers are invented.
- **Do NOT** implement the renderer disable-Cancel-during-stop tooltip or the always-`syncStatus()`-after-cancel
  store change — **task-ux-404**.
- **Do NOT** implement true abort-during-stop (a cancel flag the coordinator honors before the node-stop
  handler) — deferred to **D7** (backend-correctness track).
- **Do NOT** change `source/main/ipc/mithrilPartialSyncChannel.ts` (in the JSON targetPaths) — the
  service-level re-emit already reaches the renderer via the existing status-push wiring; see
  **PLANNING QUESTION (a)** below for the full trace. The file disposition is "verified no change needed".
- **Do NOT** alter `allowedRecoveryActions` semantics (lock #2): the re-emit carries the **existing**
  `allowedRecoveryActions` verbatim; no recovery surface is fabricated.

## Dependencies
None. This task touches only the `cancel()` early-return seam and its spec.

## Research / docs / workflows / skills consulted
- **PRD D5(f)** (`mithril-partial-sync-ux-refinement-prd.md:297-306`) — the decision this task's backend half
  implements: cancel during `stopping-node` is a silent no-op; resolution is disable+tooltip (renderer) AND
  always-resync (the backend must have something to resync to). D7 deferral of true abort-during-stop
  (`:304-306`).
- **Research-19 gap #39** (`research/19-ux-refinement-state-and-gaps.md:392`) — `service.cancel`
  early-returns while `_activeWorkDir`/`_currentProcess` are null; store never resyncs; button looks dead.
  Distinct from gap #9 (the post-cutover throw, renderer-side visibility — NOT this task).
- **Tasks JSON** task-ux-203 block (`mithril-partial-sync-ux-refinement-tasks.json`) — targetPaths,
  implementationNotes (re-emit on early-return; lock the post-cutover throw; optional stopping-node emit;
  renderer half is task-ux-404), testCases, acceptance.
- **Backend PRD** Boundary model (`mithril-partial-sync-prd.md`) — lock #6 (cancellation forbidden after
  cutover); lock #2 (boundary recovery backend-authoritative; render from `allowedRecoveryActions`).
- Live code: `MithrilPartialSyncService.ts` (`cancel`, `_updateStatus`, `onStatus`, `_statusEmitter`),
  `MithrilController.ts` (`onStatus` subscription → `broadcastPartialSyncStatus` → `_partialSyncStatusSender`),
  `mithrilPartialSyncChannel.ts` (cancel handler + status-sender wiring),
  `utils/chainStorageCoordinator.ts` (`cancelPartialSync` pass-through), and the existing
  `MithrilPartialSyncService.spec.ts` cancel tests.
- Format/rigor template: `task-ux-202.md` (phase-2). Project memory: type-check with
  `./node_modules/.bin/tsc --noEmit` (not `yarn compile` — its `typedef:sass` hook fails on Node 24).

## Verified seams (re-checked against live code at planning time; JSON/PRD anchors had drifted)

| What | Live location | Note |
|------|---------------|------|
| `cancel()` method | `MithrilPartialSyncService.ts:264` | the method to edit |
| Early-return (silent) — **the seam to fix** | `MithrilPartialSyncService.ts:265-271` (`if (!this._activeWorkDir && !this._currentProcess) { logger.info(...); return; }`) | **JSON/PRD said `:241-247` — DRIFTED.** Replace the bare `return` at `:270` with a re-emit |
| Post-cutover throw — **LOCK #6, byte-unchanged** | `MithrilPartialSyncService.ts:273-277` (`if (['installing','finalizing'].includes(this._status.status)) { throw ... }`) | **JSON/PRD said `:249-253` — DRIFTED.** Do NOT touch |
| `_updateStatus(update)` | `MithrilPartialSyncService.ts:842-874` | merges then emits `this._statusEmitter.emit('status', { ...this._status })` at `:873`. `_updateStatus({})` re-emits the current status verbatim (with one benign edge — see Decision (a)) |
| `_shouldTrackElapsed` | `MithrilPartialSyncService.ts:876-880` | excludes `idle`/`cancelled`/`failed`/`completed` → for those (the terminal-ish statuses present in the node-stop window) `_updateStatus({})` is a **pure verbatim re-emit** (no `elapsedSeconds` refresh) |
| `get status()` | `MithrilPartialSyncService.ts:122-124` | returns `{ ...this._status }` — what tests assert the emission equals |
| `onStatus()` / `_statusEmitter` | `MithrilPartialSyncService.ts:126-131`, field `:100` | tests subscribe via `service.onStatus(cb)` to capture emissions |
| `_status` field | `MithrilPartialSyncService.ts:99` | `{ ...DEFAULT_STATUS }` initially; mutated by `_updateStatus` |
| `logger` import | `MithrilPartialSyncService.ts:8` | already imported; the early-return keeps its `logger.info` |
| Controller `onStatus` subscription | `MithrilController.ts:111-118` (`this._partialSyncService.onStatus(status => this.broadcastPartialSyncStatus(status)...)`) | every service emit reaches `broadcastPartialSyncStatus` |
| `broadcastPartialSyncStatus` push | `MithrilController.ts:253-269` | sets `_partialSyncStatus = status` (`:256`) AND invokes `_partialSyncStatusSender?.(status)` (`:263`) |
| `_partialSyncStatusSender` install | `mithrilPartialSyncChannel.ts:100-102` | `controller.setPartialSyncStatusSender(async status => mithrilPartialSyncStatusChannel.send(status, window.webContents))` → pushes over the STATUS channel to the renderer |
| Cancel IPC handler (`void → void`) | `mithrilPartialSyncChannel.ts:115-117` | `mithrilPartialSyncCancelChannel.onRequest(async () => { await controller.cancelPartialSync(); })` — carries no status; needs no change (Decision (b)) |
| Controller cancel route | `MithrilController.ts:404-408` → `chainStorageCoordinator.cancelPartialSync(...)` | thin |
| Coordinator cancel pass-through | `utils/chainStorageCoordinator.ts:274-278` (`async cancelPartialSync(deps) { await deps.handlers.cancel(); }`) | **no mutation lock, no node-stop** — unlike restart/wipe (which call `_ensureNodeStoppedForPartialSyncAction`, `:297`/`:325`). Confirms the service is not the node-stop owner on cancel → supports deferring the stopping-node polish (Decision (c)) |
| `cancel → service.cancel()` handler | `MithrilController.ts:443` (`cancel: async () => this._partialSyncService.cancel()`) | the handler the coordinator invokes |
| `getPartialSyncStatus()` (the renderer `syncStatus()` pull) | `MithrilController.ts:163-165` | returns `_partialSyncStatus`, kept fresh by `broadcastPartialSyncStatus:256` — so task-ux-404's `syncStatus()` also lands on the re-emitted truth |
| Existing cancel specs | `MithrilPartialSyncService.spec.ts:781-797` (post-cutover throw), `:799-830` (pre-cutover cancel), `:832-862` (cleanup-failure) | extend here; the emission-capture pattern (`service.onStatus(cb)`) is used at `:538-540` |

## DESIGN DECISIONS (RESOLVED)

### (a) Core change: re-emit the current status via `_updateStatus({})` on the early-return path
**Chosen.** In `cancel()`, replace the silent `return` at `MithrilPartialSyncService.ts:270` with a re-emit
of the current status **before** returning:

```ts
async cancel(): Promise<void> {
  if (!this._activeWorkDir && !this._currentProcess) {
    logger.info(
      'MithrilPartialSyncService: ignoring cancel request with no active partial sync',
      null
    );
    // PRD D5(f) / gap #39: there is nothing to cancel in the node-stop window, but a cancel request
    // MUST always re-emit a status so the renderer never sticks on its optimistic stopping-node frame.
    // Re-emit the TRUE current status verbatim (do NOT fabricate `cancelled`). The existing push
    // pipeline (service.onStatus -> MithrilController.broadcastPartialSyncStatus ->
    // _partialSyncStatusSender) delivers it to the renderer; no channel change needed.
    this._updateStatus({});
    return;
  }
  ...
```

- **Why `_updateStatus({})` (reuse, not a new helper):** calling `_updateStatus` with an empty partial sets
  `nextStatus = this._status.status` (`:843`), re-merges `this._status` over itself (`:855-872` — a no-op
  spread), and emits `this._statusEmitter.emit('status', { ...this._status })` (`:873`). The emitted object
  is the current status verbatim. The orchestrator's preference is reuse over new abstractions, and this is
  the smallest truthful change.
- **The one benign edge (documented, accepted):** `_updateStatus({})` refreshes `transferProgress.elapsedSeconds`
  **only** for statuses where `_shouldTrackElapsed` is true (`:876-880` — i.e. NOT
  `idle`/`cancelled`/`failed`/`completed`). In the node-stop window the backend status is one of the
  terminal-ish statuses (`completed` after a verified cutover, or `cancelled`/`failed`/`idle`), for which
  `_shouldTrackElapsed` is **false** → the re-emit is a **pure verbatim re-emit** with no number refreshed.
  Even in the theoretical case where a tracking status is live, refreshing `elapsedSeconds` is truthful
  (real wall-clock elapsed), not a synthesized throughput/%/remaining-time number — so **lock #8 is honored**
  either way. A dedicated `_emitCurrentStatus()` helper was considered and **rejected** as unnecessary
  abstraction: `_updateStatus({})` is the existing, tested re-emit primitive.
- **Why not fabricate `cancelled`:** there is no active sync to cancel in this window, so asserting
  `status:'cancelled'` with a recovery-action surface would be a lie to the renderer (lock #2). Re-emitting
  the true status is what lets the renderer's optimistic `stopping-node` overlay reconcile to reality.

### (b) `source/main/ipc/mithrilPartialSyncChannel.ts` — VERIFIED NO CHANGE NEEDED
**Resolved: the channel file does not change.** It is in the JSON targetPaths, but the status PUSH that the
renderer relies on is already fully wired and is **independent of the cancel channel**. Trace (file:line):

1. `mithrilPartialSyncChannel.ts:100-102` — `controller.setPartialSyncStatusSender(async (status) => { await mithrilPartialSyncStatusChannel.send(status, window.webContents); })`
   installs a sender that pushes any status to the renderer over the **STATUS** channel.
2. `mithrilPartialSyncChannel.ts:104` — `controller.initialize()` runs the subscription wiring.
3. `MithrilController.ts:111-118` — `this._partialSyncService.onStatus((status) => this.broadcastPartialSyncStatus(status)...)`
   subscribes the controller to **every** service status emission.
4. `MithrilController.ts:253-269` — `broadcastPartialSyncStatus(status)` sets `this._partialSyncStatus = status`
   (`:256`, keeping the `getPartialSyncStatus()` pull fresh too) and invokes
   `this._partialSyncStatusSender?.(status)` (`:263`) → pushes to the renderer.

So when `cancel()` calls `_updateStatus({})`, the EventEmitter fires (`MithrilPartialSyncService.ts:873`) →
controller `broadcastPartialSyncStatus` → `_partialSyncStatusSender` pushes the snapshot over the STATUS
channel. The **cancel** channel itself (`mithrilPartialSyncCancelChannel`, `:41-44`/`:115-117`) is `void →
void` and is **not** the carrier of status — it never needs to change. Adding anything to the channel file
would be redundant. **Disposition: no change to `mithrilPartialSyncChannel.ts`; the service-level re-emit is
sufficient.** (Recorded as the JSON-targetPaths reconciliation in the research note.) This also means
task-ux-404's renderer `syncStatus()` (the pull via `getPartialSyncStatus()`, `MithrilController.ts:163-165`)
lands on the same re-emitted truth, since `broadcastPartialSyncStatus:256` refreshes `_partialSyncStatus`.

### (c) Optional stopping-node emit — DEFERRED
**Deferred (justified).** The JSON implementationNote optionally allows the backend to emit `stopping-node`
when it actually stops the node (pairs with D8#19). **Defer**, because:
- **`stopping-node` is documented renderer-optimistic-only** (research-19 section 3); making the backend
  author it is a separate, broader status-model change, not the cancel-resync fix this task scopes.
- **The service does not stop the node on the cancel path.** `cancelPartialSync` in the coordinator is a thin
  pass-through to `handlers.cancel()` with **no `_ensureNodeStopped…` and no mutation lock**
  (`utils/chainStorageCoordinator.ts:274-278`) — unlike restart/wipe (`:297`, `:325`). The node-stop that
  produces the multi-minute window is orchestrated **elsewhere** (the renderer's optimistic frame /
  startup-flow re-entry), **not** inside `MithrilPartialSyncService`. Emitting `stopping-node` from the
  service would mean inventing a transition the service does not own — larger than the smallest-truthful
  change and risks colliding with D8#19's status-model work.
- **It is not required** for the acceptance criteria: a cancel request producing *a* status emission (the
  re-emit of the true current status) fully satisfies "the renderer can act on it / never sticks." The
  truthful-stopping-node nicety is independent polish.

So this task ships only the early-return re-emit (Decision (a)); the stopping-node emit is left to D8#19 /
the backend-correctness track.

## Files expected to change (exact paths)
1. `source/main/mithril/MithrilPartialSyncService.ts` — replace the silent `return` in the `cancel()`
   early-return branch (`:270`) with `this._updateStatus({});` then `return;` (Decision (a)).
2. `source/main/mithril/MithrilPartialSyncService.spec.ts` — add the two tests (Step 2 / test plan below).

**Targets in the JSON that do NOT change (and why):**
- `source/main/ipc/mithrilPartialSyncChannel.ts` — **no change.** The status push is already wired and is
  independent of the cancel channel (Decision (b), full trace). The service-level re-emit reaches the
  renderer through the existing STATUS-channel sender.

## Implementation approach — ordered, mechanical steps

### Step 1 — `MithrilPartialSyncService.ts`: re-emit on the early-return path
In `cancel()` (`:264`), the early-return branch is exactly (`:265-271`):

```ts
    if (!this._activeWorkDir && !this._currentProcess) {
      logger.info(
        'MithrilPartialSyncService: ignoring cancel request with no active partial sync',
        null
      );
      return;
    }
```

Replace the bare `return;` with a current-status re-emit immediately before returning:

```ts
    if (!this._activeWorkDir && !this._currentProcess) {
      logger.info(
        'MithrilPartialSyncService: ignoring cancel request with no active partial sync',
        null
      );
      // PRD D5(f) / gap #39: nothing to cancel in the node-stop window, but a cancel request MUST
      // always re-emit a status so the renderer never sticks on its optimistic stopping-node frame.
      // Re-emit the TRUE current status verbatim (do NOT fabricate `cancelled`); the existing push
      // pipeline (service.onStatus -> broadcastPartialSyncStatus -> _partialSyncStatusSender) delivers it.
      this._updateStatus({});
      return;
    }
```

Do **NOT** modify the post-cutover throw block immediately below (`:273-277`) — **lock #6**, byte-unchanged.
Do **NOT** add any new field, helper, or fabricated status.

### Step 2 — `MithrilPartialSyncService.spec.ts`: add the two tests
Add both tests inside the existing top-level `describe('MithrilPartialSyncService', ...)` block (near the
other cancel tests at `:781-862`). They follow the existing pattern: construct the service, set `_status`
directly, subscribe with `service.onStatus`, call `service.cancel()`, assert.

**Test A — "re-emits the current status when cancel is requested during the node-stop window":**
```ts
it('re-emits the current status when cancel is requested during the node-stop window', async () => {
  const service = new MithrilPartialSyncService();
  // node-stop window: nothing active, both work refs null.
  service._activeWorkDir = null;
  service._currentProcess = null;
  service._status = {
    status: 'completed',
    allowedRecoveryActions: [],
    transferProgress: {},
    progressItems: [],
    error: null,
  };

  const emissions: Array<MithrilPartialSyncStatusSnapshot> = [];
  service.onStatus((update) => emissions.push(update));

  await expect(service.cancel()).resolves.toBeUndefined();

  // A status emission happened (renderer can resync) ...
  expect(emissions).toHaveLength(1);
  // ... and it is the TRUE current status, not a fabricated `cancelled`.
  expect(emissions[0]).toEqual(
    expect.objectContaining({ status: 'completed', allowedRecoveryActions: [] })
  );
  expect(service.status.status).toBe('completed');
});
```
- The key assertions: (1) at least one emission occurs on the early-return path (today there are **zero**),
  and (2) the emitted status equals the pre-cancel status (no fabricated `cancelled`/`failed`, no synthesized
  numbers). Using `status:'completed'` makes the verbatim-re-emit explicit (it is a non-tracking status, so
  `_shouldTrackElapsed` is false and nothing is refreshed). An optional second variant with `status:'idle'`
  (the bare default) can assert the same.

**Test B — "post-cutover cancel still hard-rejects unchanged (lock #6 regression guard):"** either reuse the
existing test at `:781-797` (it already asserts the throw) or add an explicit guard that asserts no emission
occurs on the throw path:
```ts
it('still hard-rejects cancellation after cutover (lock #6, unchanged)', async () => {
  const service = new MithrilPartialSyncService();
  service._activeWorkDir = '/tmp/daedalus-state/mithril-partial-sync/download';
  service._status = {
    status: 'installing',
    allowedRecoveryActions: [],
    transferProgress: {},
    progressItems: [],
    error: null,
  };
  const emissions: Array<MithrilPartialSyncStatusSnapshot> = [];
  service.onStatus((update) => emissions.push(update));

  await expect(service.cancel()).rejects.toThrow(
    'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
  );
  expect(emissions).toHaveLength(0); // the throw path emits nothing; behavior unchanged
});
```
(If keeping the spec minimal, the existing `:781-797` test already covers the throw; the new emission-guard
assertion above is the only addition strictly needed to prove the early-return re-emit did not leak into the
throw path. Note `installing` correctly exercises the post-cutover branch because `_activeWorkDir` is set, so
the early-return is not taken and the throw fires.)

`MithrilPartialSyncStatusSnapshot` is already imported in the spec (used at `:530`). No new mocks are needed
(the early-return path calls no marker/fs/runner functions before re-emitting — `_updateStatus` only mutates
in-memory state and emits).

### Step 3 — No other production changes
`source/main/ipc/mithrilPartialSyncChannel.ts`, `MithrilController.ts`, and the coordinator are unchanged;
the status push is already wired (Decision (b)). The behavior change lives entirely in the one-line `cancel()`
re-emit.

## Locked invariants this task honors (inline)
- **#6 Cancellation forbidden after cutover:** the post-cutover throw (`MithrilPartialSyncService.ts:273-277`)
  is **byte-unchanged**. The re-emit is added to the *pre-cutover* early-return branch above it, which only
  runs when both `_activeWorkDir` and `_currentProcess` are null — a state that can never coexist with the
  `installing`/`finalizing` throw (which requires `_activeWorkDir` set). Test B is the regression guard.
- **#2 Boundary recovery backend-authoritative; render strictly from `allowedRecoveryActions`:** the re-emit
  carries the **existing** `allowedRecoveryActions` verbatim. No recovery surface is fabricated; no
  `allowedRecoveryActions` semantics are altered on cancel.
- **#8 No synthetic throughput / remaining-time / overall-% over IPC; no raw mithril-client JSON in UI:** the
  re-emit is an existing in-memory snapshot. For the terminal-ish statuses present in the node-stop window
  `_shouldTrackElapsed` is false, so not even `elapsedSeconds` is refreshed; in any case no progress/%/throughput
  number is synthesized and no raw client JSON is routed.
- **#11 No empty-chain bootstrap regression:** this task touches only the partial-sync `cancel()` early-return
  path and its spec; the bootstrap flow and shared progress components are untouched.
- **Smallest-truthful-change:** one line of production code (replace `return;` with `this._updateStatus({}); return;`),
  reusing the existing re-emit primitive; no new channel, helper, field, or fabricated state.

## Acceptance criteria (carried verbatim from tasks JSON)
1. A cancel request always produces a status emission the renderer can act on.
2. Post-cutover cancel rejection is preserved.

## Verification plan (exact commands)
From repo root:
- `./node_modules/.bin/tsc --noEmit` — type-check (project memory: `yarn compile` fails only on the
  pre-existing `typedef:sass` hook under Node 24; tsc directly is the source of truth; allow a few minutes).
- `./node_modules/.bin/jest source/main/mithril/MithrilPartialSyncService.spec.ts` — the extended spec
  (new Test A + Test B/guard) plus all existing cancel tests pass.
- `./node_modules/.bin/eslint source/main/mithril/MithrilPartialSyncService.ts source/main/mithril/MithrilPartialSyncService.spec.ts`
  — clean on touched files (compare warning count to the pre-change baseline; no new warning).

### Test plan (map each tasks-JSON testCase to a concrete test)
1. **"Cancel during the node-stop window re-emits status (renderer can resync)"** → Test A
   (`MithrilPartialSyncService.spec.ts`): `_activeWorkDir`/`_currentProcess` null, `_status.status` set to a
   real value (`completed`); subscribe `service.onStatus`; `await service.cancel()`; assert **exactly one**
   emission and that it equals the current status (no fabricated `cancelled`). This is the direct proof that
   the previously-silent early-return now emits.
2. **"Post-cutover cancel still hard-rejects unchanged"** → Test B / the existing `:781-797` test: status
   `installing`, `_activeWorkDir` set; assert `service.cancel()` rejects with the unchanged message and that
   **no** emission occurred on the throw path (lock #6 regression guard).

## Risks / open questions
- **`_updateStatus({})` `elapsedSeconds` refresh (ACCEPTED, Decision (a)).** Only refreshes for tracking
  statuses (`_shouldTrackElapsed` false for `idle`/`cancelled`/`failed`/`completed`), which are the ones
  present in the node-stop window → pure verbatim re-emit there. Even if a tracking status were live, the
  refreshed value is true wall-clock elapsed, not a synthesized progress number → lock #8 intact.
- **Renderer pull vs push (RESOLVED, Decision (b)).** task-ux-404 will additionally call `syncStatus()` after
  cancel; that pull (`getPartialSyncStatus()` → `_partialSyncStatus`, refreshed at
  `broadcastPartialSyncStatus:256` by this same re-emit) lands on the same truth. Both the push (this task)
  and the pull (task-ux-404) now have a non-stale source. No coordination gap.
- **Stopping-node polish deferral (RESOLVED, Decision (c)).** Deferred to D8#19 / backend-correctness; the
  service does not own the node-stop on cancel (coordinator pass-through, no `_ensureNodeStopped…`), so a
  service-emitted `stopping-node` would be inventing an unowned transition.
- **Anchor drift (RECORDED).** JSON/PRD cite `MithrilPartialSyncService.ts:241-247` (early-return) and
  `:249-253` (throw); live code is `:265-271` and `:273-277`. Prefer live code; recorded in the research note.

## Required doc / research updates
- `task-ux-203-research.md` (this sprint): record the status-push trace proving the channel file needs no
  change (Decision (b), `mithrilPartialSyncChannel.ts:100-102` → `MithrilController.ts:111-118,253-269`); the
  stopping-node deferral rationale (coordinator cancel pass-through `chainStorageCoordinator.ts:274-278`, no
  node-stop); the `_updateStatus({})` re-emit semantics + the benign `elapsedSeconds` edge; and the
  JSON/PRD→live anchor drift (`:241-247`→`:265-271`, `:249-253`→`:273-277`).
- No PRD edit required — D5(f) already specifies the always-resync behavior; this task is its backend half.
  Note the hand-off to task-ux-404 (renderer disable-Cancel + always-`syncStatus()`).

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-203-plan-review.md`
  (append-only).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-203-impl-review.md`
  (created at impl time).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-203-research.md` (durable
  research note).

## Final outcome

**Planning status: `approved` · Build status: `completed`.** Implemented exactly as planned (zero design
deviations); plan review `approved` (`task-ux-203-plan-review.md`, Critiquer 2026-06-24T14:09:12Z),
implementation review `approved` (`task-ux-203-impl-review.md`, Code Review 2026-06-24T14:21:30Z). Verified
green at finalize time (re-run by SCRIBE 2026-06-24): `./node_modules/.bin/jest
source/main/mithril/MithrilPartialSyncService.spec.ts` → **38/38 pass** (was 36; +2 new).

### What was implemented

**Core change — re-emit the true current status on the cancel early-return (PRD D5(f) / gap #39).**
- `source/main/mithril/MithrilPartialSyncService.ts:265-276` — in `cancel()`, the node-stop-window
  early-return branch (`if (!this._activeWorkDir && !this._currentProcess)`) replaced its bare `return;` with
  `this._updateStatus({});` (`:274`) immediately before `return;` (plus a 4-line comment citing PRD D5(f) /
  gap #39 and naming the push pipeline). `_updateStatus({})` sets `nextStatus = this._status.status`
  (`:848`), re-merges `_status` over itself (no-op spread), and fires
  `this._statusEmitter.emit('status', { ...this._status })` (`:878`) — a **verbatim re-emit of the TRUE
  current status**. No fabricated `cancelled`/`failed`; no synthesized numbers. The renderer's optimistic
  `stopping-node` frame now has a status to reconcile against instead of sticking.
- **LOCK #6 byte-unchanged:** the post-cutover throw (`['installing','finalizing'] → throw`) sits at
  `:278-282` (drifted down 5 lines from the added block) and does not appear in `git diff`. The re-emit is on
  the mutually-exclusive pre-cutover branch.

**No channel change (Decision (b), verified).** `source/main/ipc/mithrilPartialSyncChannel.ts` is in the JSON
`targetPaths` but is **unchanged** (empty `git diff`). The status PUSH the renderer relies on is already
wired and is independent of the `void → void` cancel channel:
`controller.setPartialSyncStatusSender(...)` (`mithrilPartialSyncChannel.ts:100-102`) →
`this._partialSyncService.onStatus(...)` (`MithrilController.ts:111-112`) → `broadcastPartialSyncStatus` sets
`_partialSyncStatus = status` (`:256`) and invokes `_partialSyncStatusSender?.(status)` (`:263`). The
`cancel()` re-emit reaches the renderer through this existing STATUS-channel sender. Full trace is in the
research note section 2.

**Stopping-node backend emit DEFERRED (Decision (c)).** The service does not own the node-stop on cancel —
the coordinator's `cancelPartialSync` (`source/main/utils/chainStorageCoordinator.ts:274-277`) is a thin
`handlers.cancel()` pass-through with no `_ensureNodeStopped…` (contrast restart/wipe at `:297`/`:325`).
Emitting `stopping-node` from the service would invent an unowned transition; left to D8#19.

### Tests added (2 new; 38/38 pass)
- `source/main/mithril/MithrilPartialSyncService.spec.ts:799-825` — **Test A**
  `re-emits the current status when cancel is requested during the node-stop window`: `_activeWorkDir`/
  `_currentProcess` null, `_status.status='completed'`; subscribe `service.onStatus`; `await service.cancel()`
  resolves; assert **exactly one** emission equal to the true current status (`objectContaining({
  status:'completed', allowedRecoveryActions:[] })`, no fabricated `cancelled`) and `service.status.status ===
  'completed'`. Direct proof the previously-silent early-return now emits (coverage report lists line `274`
  covered).
- `source/main/mithril/MithrilPartialSyncService.spec.ts:827-849` — **Test B**
  `does not emit a status when post-cutover cancel hard-rejects (lock #6 regression guard)`: `_activeWorkDir`
  set + `status:'installing'`; assert `cancel()` rejects with the unchanged message AND **zero** emissions on
  the throw path (the re-emit did not leak into the throw branch).

### Verification results
- `./node_modules/.bin/tsc --noEmit` → **exit 0** (clean; `yarn compile`'s `typedef:sass` hook is the Node-24
  gotcha, not used).
- `./node_modules/.bin/jest source/main/mithril/MithrilPartialSyncService.spec.ts` → **38 passed, 38 total**,
  0 failures (impl + code-review + SCRIBE re-run all agree).
- `./node_modules/.bin/eslint` on the two touched files → **0 errors, 6 warnings** (all 6 pre-existing on
  untouched `MithrilPartialSyncService.ts` lines `:41/:127/:466/:1014`; none on the touched `cancel()` lines;
  spec file 0 warnings). No new warning.

### Deviations
**None.** Step 1 (one-line `this._updateStatus({});` re-emit, reusing the existing primitive — no new helper,
field, channel, or fabricated status), Step 2 (Test A + Test B), Step 3 (no other production changes)
implemented exactly per the approved plan. Decision (c) stopping-node emit deferred; locked invariants
#2/#6/#8/#11 honored.

### Anchor drift recorded (research note section 4)
JSON/PRD cite `MithrilPartialSyncService.ts:241-247`/`:249-253`; live is `:265-276`/`:278-282`. The plan/202
template cited the coordinator at `source/main/mithril/utils/chainStorageCoordinator.ts`; it actually lives at
**`source/main/utils/chainStorageCoordinator.ts`** (path drift, same `handlers.cancel()` content). Prefer live
code.

### Hand-offs (not in scope here)
- **task-ux-404** — renderer half: disable Cancel during `stopping-node` with a tooltip + always call
  `syncStatus()` after any cancel. The pull lands on the same re-emitted truth via `getPartialSyncStatus()` →
  `_partialSyncStatus` (refreshed at `broadcastPartialSyncStatus:256` by this re-emit). gap #9 (post-cutover
  Cancel visibility) is also renderer-side.
- **task-ux-503** — cross-cutting integration/E2E coverage of the cancel-during-stop UX loop (this task's
  tests are unit-level).
- **D7 / backend-correctness** — true abort-during-stop (cancel flag honored before the node-stop handler)
  remains deferred. **D8#19 / status-model** — truthful backend-emitted `stopping-node` remains deferred.

## Status
- Planning status: `approved`
- Build status: `completed`
