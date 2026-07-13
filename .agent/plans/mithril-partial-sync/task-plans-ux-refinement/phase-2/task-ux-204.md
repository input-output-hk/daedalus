# task-ux-204 — Native dialog is the single startup-interrupted recovery surface

- Sprint: Mithril Partial Sync UX Refinement — phase-2 (Backend UX-Enabling Correctness)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `completed`
- Interaction mode: `autonomous`
- Priority: high · Estimated: 2h · Dependencies: none (all satisfied)

## Why now
When startup detects an **unsafe interrupted cutover**, the recovery currently fires **two competing
surfaces** at once: the backend emits a React `failed` status (which mounts the in-window React overlay) AND
blocks on a native Electron `dialog.showMessageBox`. They have divergent button sets (the overlay renders
recovery CTAs from `allowedRecoveryActions:['wipe-and-full-sync']`; the native dialog offers "Wipe chain and
full Mithril sync" / "Quit"). This is research-19 **gap #7** and the cluster PRD **D5(a)** resolves.

PRD D5(a) makes the **native Electron dialog the single authoritative recovery surface** on the
startup-interrupted path, because startup-owned recovery **must not depend on diagnostics UI or a running
node** (locked safety boundary #2) and must work **before the renderer is ready** and be correctly blocking
with a Quit. The fix is to **remove the redundant React `failed` emission** on that one startup path. The
native dialog and the wipe/quit/generation logic stay byte-unchanged. **In-session failures are unaffected** —
they keep using the React overlay (the `startInstalledNode` catch path, untouched).

This is a backend/main-process correctness change with **no renderer edit** and **no new product decision**:
D5(a) fully specifies the behavior.

## Interaction mode justification
`autonomous`: backend/main-process only. **No user-facing copy is authored** — the native dialog copy already
exists and stays verbatim; no new strings. **No new product decision** — PRD D5(a) fully specifies the surface
consolidation. **Not destructive** — the change *removes* a redundant status emission; the chain wipe is
unchanged and already gated behind the user's explicit dialog choice (`response === 0`). The
generation-recheck, wipe, and marker-clear logic are untouched. The smallest-truthful change is deleting
exactly one `emitMithrilPartialSyncStatus({status:'failed',...})` call. No user stop is required.

## Scope
1. **Remove the redundant React `failed` emission** on the startup-interrupted unsafe-cutover path in
   `MithrilPartialSyncNodeStartup.handleInterruptedRecovery` — delete exactly the
   `await emitMithrilPartialSyncStatus({ status: 'failed', ... })` call
   (`source/main/mithril/mithrilPartialSyncNodeStartup.ts:77-87`), optionally replacing it with a one-line
   comment "native dialog is the single recovery surface (PRD D5a)". Everything else in the method stays.
2. **Tests** — extend `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`'s
   `handleInterruptedRecovery` describe block with the unsafe-cutover dialog-branch tests (marker state
   `cutover-in-progress`) that currently have **zero** coverage: prove the `failed` emission is gone while the
   native dialog is still shown, and that wipe/quit/generation behavior is preserved.

## Non-goals (explicit KEEP / DO-NOT-REGRESS list)
- **DO NOT remove the `emitMithrilPartialSyncStatus` import** (`:8`). It is used **three other times** and all
  three STAY:
  - **`:123`** — `startInstalledNode`, `status:'starting-node'` (the installed-node start announce). **KEEP.**
  - **`:139`** — `startInstalledNode` catch, `status:'failed'`, `stage:'starting-node'` — this is an
    **IN-SESSION failure** that drives the React overlay. **KEEP** (this is the "in-session failures
    unaffected" requirement).
  - **`:180`** — `finalizeInstalledNodeStart`, `status:'completed'` (success path, Boundary C2). **KEEP.**
- **DO NOT touch the native dialog** (`dialog.showMessageBox`, `:89-98`) — it is the single surface; copy and
  button set stay verbatim.
- **DO NOT touch the generation re-check / wipe / marker-clear / quit logic** (`:100-113`). No behavior change
  to wipe (`_wipeChainAndSnapshots`), `clearMithrilPartialSyncMarker`, or the `return true`/`return false`
  semantics.
- **DO NOT touch the no-marker (`:58-60`), node-start-verified C2 (`:62-71`), or installed-awaiting-node-start
  (`:73-75`) branches.**
- **DO NOT change `source/main/mithril/MithrilStartupGate.ts`** — disposition **verified no change needed**
  (see "MithrilStartupGate disposition" below; full evidence trace).
- **DO NOT regress the in-session React overlay failure path** (lock #11). Only the *startup-interrupted*
  (`handleInterruptedRecovery`) emission is removed; the `startInstalledNode` catch emission (`:139`) is
  untouched.
- **DO NOT regress the empty-chain Mithril bootstrap flow** (lock #11). This task touches only the partial-sync
  startup-interrupted path and its spec.
- **No new abstraction, channel, type, or copy.** Smallest truthful change.

## Dependencies
None. Task JSON lists `dependencies: []` (all satisfied). The task touches only the
`handleInterruptedRecovery` unsafe-cutover seam and its spec.

## Research / docs / workflows / skills consulted
- **PRD D5(a)** (`mithril-partial-sync-ux-refinement-prd.md:265-272`) — the decision this task implements:
  native dialog is the single authoritative recovery surface on the startup-interrupted path; remove the
  redundant React `failed` emission; in-session failures keep the React overlay. Implementation section
  (`prd.md:645-650`) — "drop the redundant React `failed` emission on the startup-interrupted path (native
  dialog only)".
- **Research-19 gap #7** (`research/19-ux-refinement-state-and-gaps.md:134`) — interrupted unsafe-cutover
  recovery uses BOTH a native Electron dialog AND the React overlay; `handleInterruptedRecovery` emits `failed`
  (overlay mounts) AND blocks on `dialog.showMessageBox` — two competing surfaces, divergent button sets.
  Cited anchors: `mithrilPartialSyncNodeStartup.ts:69-105`; `App.tsx:97-123`; `MithrilStartupGate.ts:194-205`.
- **Tasks JSON** task-ux-204 block (`mithril-partial-sync-ux-refinement-tasks.json`) — targetPaths
  (`mithrilPartialSyncNodeStartup.ts`, `MithrilStartupGate.ts`), implementationNotes (remove the redundant
  emission; keep the native dialog; honor startup-owned-recovery lock; do not touch the in-session path),
  testCases, acceptance.
- **Backend safety PRD** Boundary model (`mithril-partial-sync-prd.md`) — lock #2 (boundary recovery is
  backend-authoritative; the native startup dialog is the startup-owned recovery surface that must not depend
  on diagnostics UI or a running node and works before the renderer is ready); lock #11 (no empty-chain
  bootstrap regression; no in-session React overlay regression).
- Live code traced: `mithrilPartialSyncNodeStartup.ts` (the four `emitMithrilPartialSyncStatus` sites; the
  unsafe-cutover branch), `mithrilPartialSyncMarker.ts` (`MithrilPartialSyncMarkerState` union + the
  corrupt/unreadable-marker fallback to `cutover-in-progress`), `MithrilStartupGate.ts`
  (`ensureMithrilStartupGate` BLOCKED→`partialSyncCutoverUnsafe`, `onPartialSyncStatus`, `state` getter),
  `MithrilController.ts` (`broadcastPartialSyncStatus`, `getStartupGateState`, `isPartialSyncNodeStartBlocked`,
  `_partialSyncStatus`), `mithrilPartialSyncChannel.ts` (`emitMithrilPartialSyncStatus`,
  `getMithrilPartialSyncStatus`), `setup.ts` (`onCrashed` partial-sync gate),
  `mithril-partial-sync.types.ts` (`isMithrilPartialSyncBlockingNodeStart`, `isMithrilPartialSyncOverlayStatus`),
  `App.tsx:97`, `MithrilPartialSyncStore.ts` (`shouldShowOverlay`), and the existing spec.
- Format/rigor template: `task-ux-203.md` (phase-2, the completed sibling). Project memory: type-check with
  `./node_modules/.bin/tsc --noEmit` (not `yarn compile` — its `typedef:sass` hook fails on Node 24).

## Verified seams (re-checked against live code at planning time; JSON/PRD anchors had drifted)

| What | Live location | Note |
|------|---------------|------|
| `handleInterruptedRecovery(currentGeneration)` | `mithrilPartialSyncNodeStartup.ts:55` | the method to edit |
| no-marker branch | `:58-60` (`if (!marker) return false;`) | **KEEP unchanged** |
| node-start-verified (Boundary C2) | `:62-71` (`fs.remove(stagingRootPath)` + `clearMithrilPartialSyncMarker` + `return false`) | **KEEP unchanged** |
| installed-awaiting-node-start | `:73-75` (`return false`) | **KEEP unchanged** |
| **Redundant React `failed` emission — THE SEAM TO REMOVE** | **`:77-87`** (`await emitMithrilPartialSyncStatus({ status: 'failed', allowedRecoveryActions: ['wipe-and-full-sync'], transferProgress: {}, progressItems: [], error: { message: '...', stage: 'installing' } });`) | **JSON/PRD D5(a) cite `:69-90` — DRIFTED.** Live is **`:77-87`**. This is the **only** change. |
| Native dialog (the single surface) | **`:89-98`** (`dialog.showMessageBox(this._mainWindow, { type:'warning', buttons:['Wipe chain and full Mithril sync','Quit'], defaultId:0, cancelId:1, noLink:true, title:..., message:... })`) | **JSON/PRD said `:69-105`/`:89-105` region — live dialog is `:89-98`.** **KEEP byte-unchanged** |
| generation re-check | `:100-102` (`if (currentGeneration !== this._getGeneration()) return true;`) | **KEEP unchanged** |
| wipe + marker-clear (response 0) | `:104-111` (`_wipeChainAndSnapshots(...)` + `clearMithrilPartialSyncMarker()` + `return false`) | **KEEP unchanged** |
| quit / blocked (response 1) | `:113` (`return true`) | **KEEP unchanged** |
| `emitMithrilPartialSyncStatus` import | `:7-10` (named import from `../ipc/mithrilPartialSyncChannel`) | **MUST STAY** — used 3 more times (`:123`, `:139`, `:180`) |
| `:123` emission (`starting-node`) | `startInstalledNode`, `status:'starting-node'` | **KEEP** |
| `:139` emission (in-session `failed`) | `startInstalledNode` catch, `status:'failed'`, `stage:'starting-node'` | **KEEP** — in-session failure → React overlay (lock #11) |
| `:180` emission (`completed`) | `finalizeInstalledNodeStart`, `status:'completed'` | **KEEP** |
| Unsafe-cutover marker state | `mithrilPartialSyncMarker.ts:6-9` union; corrupt/unreadable fallback `:76-79` returns `state:'cutover-in-progress'` | the marker state that reaches the dialog branch is **`cutover-in-progress`** — use it in new tests |
| Gate BLOCKED→state transition | `MithrilStartupGate.ts:195-205` (`const blocked = await deps.partialSyncNodeStartup.handleInterruptedRecovery(...)`; `if (blocked) { this._transition('partialSyncCutoverUnsafe', 'partialSyncMarkerDetected'); return null; }`) | **JSON/research cite `:194-205` — live is `:195-205`.** The gate blocks via the BLOCKED boolean, **not** via the removed `failed` emission |
| Gate `onPartialSyncStatus` | `MithrilStartupGate.ts:157-167` | reacting to `failed` only set `partialSyncRunning` (the generic else-if); benign — no consumer (see disposition) |
| `broadcastPartialSyncStatus` | `MithrilController.ts:253-269` (sets `_partialSyncStatus = status` `:256`; calls `_startupGate.onPartialSyncStatus(status)` `:257`; pushes via `_partialSyncStatusSender` `:262-263`) | the pipeline the removed emission fed |
| `getStartupGateState()` | `MithrilController.ts:155-157` | **no live caller** (only the definition exists) — see disposition |
| `isPartialSyncNodeStartBlocked()` | `MithrilController.ts:188-192` → `isMithrilPartialSyncBlockingNodeStart(_partialSyncStatus.status)` | the only live reader of `_partialSyncStatus.status` for gating; `setup.ts:132` onCrashed |
| `isMithrilPartialSyncBlockingNodeStart` | `mithril-partial-sync.types.ts:106-118` | **`failed` is NOT in the blocking list** — proves the removed emission never gated node-start |
| Renderer overlay mount | `App.tsx:97` (`mithrilPartialSync.shouldShowOverlay`) → `MithrilPartialSyncStore.ts:97-100` (`hasDisplayStatus && !isCompletedOverlayDismissed`) → `isMithrilPartialSyncOverlayStatus(status)` (`failed` IS an overlay status) | the removed `failed` emission was exactly what mounted the overlay on this path; after removal the overlay never mounts |
| Existing `handleInterruptedRecovery` spec block | `mithrilPartialSyncNodeStartup.spec.ts:85-157` | **no test for the unsafe-cutover dialog branch** — added here |
| Spec mock harness | `dialog.showMessageBox` mocked `:11-15`; `emitMithrilPartialSyncStatus` mocked `:23-32`, captured `:46-51`; `dialog` captured `:53`; `makeInstance` `:62-73` | reuse as-is; no new mocks |

**Anchor drift recorded.** JSON description + implementationNotes and PRD D5(a) cite
`mithrilPartialSyncNodeStartup.ts:69-90` for the emission and `:69-105` for the path; LIVE code is **`:77-87`**
(emission) and **`:89-98`** (dialog). Research-19 gap #7 cites `:69-105`. The gate anchor `MithrilStartupGate.ts:194-205`
is live `:195-205`. Prefer live code.

## MithrilStartupGate disposition — VERIFIED NO CHANGE NEEDED (evidence)
`MithrilStartupGate.ts` is in the JSON `targetPaths`, but it **does not change**. The gate transitions to the
recovery state via the **BLOCKED boolean** returned by `handleInterruptedRecovery`, **not** via the removed
`failed` emission. Evidence trace (file:line):

1. **The gate's authoritative block is the BLOCKED boolean.** `ensureMithrilStartupGate` calls
   `const blocked = await deps.partialSyncNodeStartup.handleInterruptedRecovery(currentGeneration)`
   (`MithrilStartupGate.ts:195-198`); on `blocked === true` it does
   `this._transition('partialSyncCutoverUnsafe', 'partialSyncMarkerDetected'); return null;` (`:199-205`).
   `handleInterruptedRecovery` still returns `true` on Quit and on a mid-dialog generation change — **unchanged
   by this task**. So `partialSyncCutoverUnsafe` is still set, by the more-specific marker-detected path.
2. **The removed emission's only gate side-effect was benign and unconsumed.** The removed
   `emitMithrilPartialSyncStatus({status:'failed'})` reached the gate through
   `broadcastPartialSyncStatus` (`MithrilController.ts:253-269`) → `_startupGate.onPartialSyncStatus(status)`
   (`:257`). In `onPartialSyncStatus` (`:157-167`), `failed` is not `installing`/`starting-node`/`idle`, so it
   fell to the generic `else if (status.status !== 'idle')` → `_transition('partialSyncRunning',
   'partialSyncActive')`. That `partialSyncRunning` transition is **less specific** than the
   `partialSyncCutoverUnsafe` the BLOCKED path sets, and — critically — **nothing reads the gate `_state` on
   this path**: `getStartupGateState()` (`MithrilController.ts:155-157`) has **no live caller** (grep across
   `source/` returns only its definition). Losing the `partialSyncRunning` transition is therefore inert.
3. **Node-start gating does not depend on the removed `failed` emission.** The only live reader of
   `_partialSyncStatus.status` for gating is `isPartialSyncNodeStartBlocked()`
   (`MithrilController.ts:188-192` → `isMithrilPartialSyncBlockingNodeStart`, consumed at the `onCrashed`
   handler `setup.ts:132`). **`failed` is NOT in `isMithrilPartialSyncBlockingNodeStart`'s list**
   (`mithril-partial-sync.types.ts:106-118`), so the removed emission never blocked node-start. After removal,
   `_partialSyncStatus` simply stays at its default `idle` on this path (cleaner; still non-blocking). The
   startup block is enforced by the gate returning `null` (BLOCKED), which is preserved.
4. **No renderer dependency on the removed emission.** The renderer overlay mounts on
   `MithrilPartialSyncStore.shouldShowOverlay` (`App.tsx:97`), driven by `isMithrilPartialSyncOverlayStatus`
   over the `failed` status. The removed emission is **precisely** what mounted the overlay on this path — its
   removal is the intended effect (single surface). On the startup-interrupted path the renderer may not even
   be ready; the native dialog is the surface. No renderer state needs the emission *cleaned up* afterward:
   because the overlay never MOUNTS, there is no stale overlay to clear.

**Disposition: `MithrilStartupGate.ts` — verified no change needed** (mirrors task-ux-203's channel-file
disposition). No gate/controller/renderer/setup logic *requires* the removed emission. If implementation
uncovers a real dependency, surface it as a finding.

## Implementation approach — ordered, mechanical steps

### Step 1 — `mithrilPartialSyncNodeStartup.ts`: delete the redundant React `failed` emission
In `handleInterruptedRecovery` (`:55`), after the `installed-awaiting-node-start` branch (`:73-75`), the
unsafe-cutover path **currently** is exactly (`:77-98`):

```ts
    await emitMithrilPartialSyncStatus({
      status: 'failed',
      allowedRecoveryActions: ['wipe-and-full-sync'],
      transferProgress: {},
      progressItems: [],
      error: {
        message:
          'Daedalus detected an interrupted Mithril partial sync after live chain cutover. The installed chain data is not safe to start normally.',
        stage: 'installing',
      },
    });

    const { response } = await dialog.showMessageBox(this._mainWindow, {
      type: 'warning',
      buttons: ['Wipe chain and full Mithril sync', 'Quit'],
      defaultId: 0,
      cancelId: 1,
      noLink: true,
      title: 'Interrupted Mithril partial sync detected',
      message:
        'Daedalus found an interrupted Mithril partial sync after live chain replacement began. Normal startup is blocked until the chain data is wiped and a full Mithril sync can run again.',
    });
```

**Delete the entire `await emitMithrilPartialSyncStatus({ status: 'failed', ... });` call (`:77-87`)**, leaving
a one-line comment in its place. Result:

```ts
    // PRD D5(a) / gap #7: the native dialog below is the SINGLE authoritative startup-interrupted recovery
    // surface (startup-owned recovery must not depend on diagnostics UI or a running node, and works before
    // the renderer is ready). The redundant React `failed` emission was removed here to eliminate the
    // two-competing-surfaces problem. In-session failures keep the React overlay (startInstalledNode catch).

    const { response } = await dialog.showMessageBox(this._mainWindow, {
      type: 'warning',
      buttons: ['Wipe chain and full Mithril sync', 'Quit'],
      defaultId: 0,
      cancelId: 1,
      noLink: true,
      title: 'Interrupted Mithril partial sync detected',
      message:
        'Daedalus found an interrupted Mithril partial sync after live chain replacement began. Normal startup is blocked until the chain data is wiped and a full Mithril sync can run again.',
    });
```

- **Do NOT remove the `emitMithrilPartialSyncStatus` import** (`:8`) — still used at `:123`/`:139`/`:180`.
- **Do NOT touch** the dialog (`:89-98`), the generation re-check (`:100-102`), the wipe/marker-clear
  (`:104-111`), or the quit `return true` (`:113`).
- No new field, helper, channel, type, or copy.

### Step 2 — `mithrilPartialSyncNodeStartup.spec.ts`: add unsafe-cutover dialog-branch tests
Add inside the existing `describe('handleInterruptedRecovery', ...)` block (`:85-157`), reusing the existing
mocks (`dialog.showMessageBox` `:11-15`/`:53`; `emitMithrilPartialSyncStatus` captured `:46-51`; `makeInstance`
`:62-73`; `fsMock.remove` `:55`). Marker state for every test: **`cutover-in-progress`**. Mock the dialog
result per-test via `(dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: <n> })`.

**Test 1 — single surface (the core proof; inverts the C2 test at `:129-143`):**
```ts
it('unsafe cutover: shows ONLY the native dialog and does NOT emit a failed status (single surface, PRD D5a)', async () => {
  readMithrilPartialSyncMarker.mockResolvedValue({
    state: 'cutover-in-progress',
    updatedAt: '2026-06-01T00:00:00.000Z',
  });
  (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 1 }); // Quit
  const { instance } = makeInstance();

  await instance.handleInterruptedRecovery(0);

  // The native dialog IS the recovery surface ...
  expect(dialog.showMessageBox).toHaveBeenCalledTimes(1);
  // ... and the redundant React `failed` emission is GONE.
  expect(emitMithrilPartialSyncStatus).not.toHaveBeenCalledWith(
    expect.objectContaining({ status: 'failed' })
  );
});
```

**Test 2 — Wipe (response 0): wipes + clears marker + returns false:**
```ts
it('unsafe cutover, Wipe (response 0): wipes chain + snapshots, clears the marker, returns false', async () => {
  readMithrilPartialSyncMarker.mockResolvedValue({
    state: 'cutover-in-progress',
    updatedAt: '2026-06-01T00:00:00.000Z',
  });
  (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 0 });
  const { instance, wipeChainAndSnapshots } = makeInstance();

  const result = await instance.handleInterruptedRecovery(0);

  expect(result).toBe(false);
  expect(wipeChainAndSnapshots).toHaveBeenCalledTimes(1);
  expect(clearMithrilPartialSyncMarker).toHaveBeenCalledTimes(1);
  expect(emitMithrilPartialSyncStatus).not.toHaveBeenCalledWith(
    expect.objectContaining({ status: 'failed' })
  );
});
```

**Test 3 — Quit (response 1): blocked, no wipe:**
```ts
it('unsafe cutover, Quit (response 1): returns true (blocked) without wiping or clearing the marker', async () => {
  readMithrilPartialSyncMarker.mockResolvedValue({
    state: 'cutover-in-progress',
    updatedAt: '2026-06-01T00:00:00.000Z',
  });
  (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 1 });
  const { instance, wipeChainAndSnapshots } = makeInstance();

  const result = await instance.handleInterruptedRecovery(0);

  expect(result).toBe(true);
  expect(wipeChainAndSnapshots).not.toHaveBeenCalled();
  expect(clearMithrilPartialSyncMarker).not.toHaveBeenCalled();
});
```

**Test 4 (optional) — generation changed mid-dialog: blocked, no wipe:**
```ts
it('unsafe cutover, generation changed during the dialog: returns true without wiping', async () => {
  readMithrilPartialSyncMarker.mockResolvedValue({
    state: 'cutover-in-progress',
    updatedAt: '2026-06-01T00:00:00.000Z',
  });
  (dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: 0 }); // user picked Wipe...
  const { instance, getGeneration, wipeChainAndSnapshots } = makeInstance();
  getGeneration.mockReturnValueOnce(0).mockReturnValue(1); // ...but generation moved on

  const result = await instance.handleInterruptedRecovery(0);

  expect(result).toBe(true); // stale generation short-circuits before the wipe
  expect(wipeChainAndSnapshots).not.toHaveBeenCalled();
});
```
> Note: `handleInterruptedRecovery(0)` reads `readMithrilPartialSyncMarker` first (no generation read), then
> `dialog.showMessageBox`, then the generation re-check `currentGeneration !== this._getGeneration()`. The
> mock above returns `0` once (irrelevant here, since the marker read doesn't call `getGeneration`) and `1`
> thereafter, so the re-check at `:100` sees `0 !== 1` → returns `true` before the wipe. If the one-shot
> sequencing proves brittle in the harness, set `getGeneration.mockReturnValue(1)` and call
> `instance.handleInterruptedRecovery(0)` — the same `0 !== 1` re-check fires.

**The in-session-overlay regression guard already exists and stays:** the existing C2 test
(`:129-143`) asserts that on `node-start-verified` neither `failed` is emitted nor the dialog shown; the
`startInstalledNode` catch emission (`:139`, the in-session `failed`) is covered by its own surface and is not
touched by this task. No new mock is required — the spec already mocks `electron.dialog`,
`emitMithrilPartialSyncStatus`, the marker functions, and `fs-extra`.

### Step 3 — No other production changes
`MithrilStartupGate.ts` is unchanged (verified-no-change disposition). No controller, channel, renderer, or
`setup.ts` edit. The behavior change lives entirely in the single deleted emission.

## Locked invariants this task honors (inline)
- **#2 Boundary recovery is BACKEND-AUTHORITATIVE; render strictly from `allowedRecoveryActions`; the native
  startup dialog is the startup-owned recovery surface (must not depend on diagnostics UI or a running node;
  works before the renderer is ready).** This task **consolidates onto** that backend-owned native dialog by
  removing the competing React surface. The dialog, its blocking behavior, and its Quit option are
  byte-unchanged. The wipe stays gated behind the user's explicit dialog choice.
- **#11 No empty-chain bootstrap regression; no in-session React overlay regression.** Only the
  *startup-interrupted* (`handleInterruptedRecovery`) emission is removed. The in-session failure path
  (`startInstalledNode` catch, `:138-152`, emission at `:139`) is untouched and keeps driving the React
  overlay. The bootstrap flow and shared components are untouched.
- **Smallest-truthful change.** Delete exactly the one `emitMithrilPartialSyncStatus({status:'failed',...})`
  call (`:77-87`), leaving a one-line comment. The import stays (3 other uses). No new abstraction, channel,
  type, or copy. No behavior change to wipe/quit/generation logic.

## Acceptance criteria (carried verbatim from tasks JSON)
1. Only one surface (native dialog) handles startup-interrupted unsafe-cutover recovery.
2. In-session failure overlay behavior is unchanged.

### Test plan (map each tasks-JSON testCase to a concrete test)
1. **"Startup-interrupted unsafe cutover shows only the native dialog (no React overlay)"** → **Test 1**
   (`mithrilPartialSyncNodeStartup.spec.ts`): marker `cutover-in-progress`; assert
   `dialog.showMessageBox` called exactly once AND `emitMithrilPartialSyncStatus` **not** called with
   `status:'failed'`. (No React overlay can mount because `failed` is never emitted — `App.tsx:97` →
   `shouldShowOverlay` driven by `isMithrilPartialSyncOverlayStatus(failed)`.) Tests 2–4 lock the
   wipe/quit/generation behavior unchanged.
2. **"In-session failure still shows the React overlay"** → guarded by the **unchanged** `startInstalledNode`
   catch emission (`:139`, `status:'failed'`, `stage:'starting-node'`) — not touched by this task, so its
   overlay behavior is preserved by construction. The existing C2 test (`:129-143`) plus the unchanged catch
   path establish the in-session surface is unaffected.

## Verification plan (exact commands)
From repo root:
- `./node_modules/.bin/tsc --noEmit` — type-check (project memory: `yarn compile`'s `typedef:sass` hook fails
  under Node 24; use `tsc` directly; allow a few minutes). Confirms the `emitMithrilPartialSyncStatus` import
  is still used (no "unused import" error) and the deletion type-checks.
- `./node_modules/.bin/jest source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` — the extended spec
  (new unsafe-cutover tests + all existing `handleInterruptedRecovery`/`finalizeInstalledNodeStart` tests)
  pass.
- `./node_modules/.bin/eslint source/main/mithril/mithrilPartialSyncNodeStartup.ts source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`
  — compare warning count to the **pre-change baseline: 2 warnings** (both pre-existing on
  `mithrilPartialSyncNodeStartup.ts:21-22`, the `WipeChainAndSnapshots` type's unused `reason`/`nodeState`
  params; spec file 0 warnings). Introduce **no new warning**.

## Risks / open questions
- **Gate disposition (RESOLVED, evidence above).** `MithrilStartupGate.ts` needs no change: the block is the
  BLOCKED boolean (`:195-205`), `getStartupGateState()` has no live caller, and `failed` is not in
  `isMithrilPartialSyncBlockingNodeStart` — so the removed emission never gated anything. If the implementer
  finds a real dependency, that is a finding to surface.
- **Import retention (RESOLVED).** `emitMithrilPartialSyncStatus` import stays — 3 other live uses
  (`:123`/`:139`/`:180`). `tsc --noEmit` is the guard against an accidental unused-import error.
- **`_partialSyncStatus` stays `idle` on this path after removal (ACCEPTED).** Cleaner and still non-blocking;
  the startup block is enforced by the gate returning `null`. No consumer depends on it being `failed` here.
- **No stale overlay to clear (RESOLVED).** Because `failed` is never emitted, the overlay never mounts
  (`App.tsx:97`); there is nothing to clean up afterward.
- **Anchor drift (RECORDED).** JSON/PRD D5(a) cite `mithrilPartialSyncNodeStartup.ts:69-90`; research-19 gap #7
  cites `:69-105`; live is `:77-87` (emission) / `:89-98` (dialog). Gate `:194-205` → live `:195-205`. Prefer
  live code.

## Required doc / research updates
- `task-ux-204-research.md` (this sprint, impl-time deliverable): record the gate disposition evidence trace
  (BLOCKED boolean `MithrilStartupGate.ts:195-205`; `onPartialSyncStatus` `failed`→`partialSyncRunning` benign,
  `getStartupGateState` unconsumed; `isMithrilPartialSyncBlockingNodeStart` excludes `failed`); the
  renderer-overlay mount trace (`App.tsx:97` → `shouldShowOverlay` → `isMithrilPartialSyncOverlayStatus`); the
  three KEPT emissions (`:123`/`:139`/`:180`); and the JSON/PRD/research→live anchor drift
  (`:69-90`/`:69-105` → `:77-87`/`:89-98`; gate `:194-205` → `:195-205`).
- No PRD edit required — D5(a) already specifies the single-surface consolidation; this task is its
  implementation. (Note the sibling D5 items: D5(b) stage-aware copy, D5(c) no-Cancel-after-cutover, D5(f)
  cancel-resync — separate tasks, not in scope here.)

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-204-plan-review.md`
  (append-only).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-204-impl-review.md`
  (created at impl time).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-2/task-ux-204-research.md` (durable
  research note, impl-time).

## Final outcome

**Planning status: `approved` · Build status: `completed`.** Implemented exactly as planned (zero design
deviations); plan review `approved` (`task-ux-204-plan-review.md`, Critiquer 2026-06-24T14:52:03Z),
implementation review `approved` (`task-ux-204-impl-review.md`, Code Review 2026-06-24T14:56:53Z). Verified
green at finalize time (re-run by SCRIBE 2026-06-24): `./node_modules/.bin/jest
source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` → **13/13 pass** (was 9; +4 new unsafe-cutover
dialog-branch tests); `./node_modules/.bin/tsc --noEmit` → **exit 0**; eslint on the two touched files →
**0 errors, 2 warnings** (both pre-existing, baseline-matching).

### What was implemented

**Core change — remove the redundant React `failed` emission on the startup-interrupted unsafe-cutover path
(PRD D5(a) / gap #7).**
- `source/main/mithril/mithrilPartialSyncNodeStartup.ts` — in `handleInterruptedRecovery` (`:55`), the
  unsafe-cutover path's `await emitMithrilPartialSyncStatus({ status: 'failed', allowedRecoveryActions:
  ['wipe-and-full-sync'], transferProgress: {}, progressItems: [], error: { message: '...interrupted Mithril
  partial sync after live chain cutover...', stage: 'installing' } });` call (formerly `:77-87`) was **deleted**
  and replaced with a 4-line comment (`:77-80`) citing PRD D5(a) / gap #7 ("the native dialog below is the
  SINGLE authoritative startup-interrupted recovery surface ... In-session failures keep the React overlay").
  The native `dialog.showMessageBox` (now `:82-91`), the generation re-check
  (`currentGeneration !== this._getGeneration()`, `:93-95`), the wipe `_wipeChainAndSnapshots` +
  `clearMithrilPartialSyncMarker` on `response === 0` (`:97-104`), and the Quit `return true` (`:106`) are
  **byte-unchanged**. The no-marker (`:58-60`), node-start-verified C2 (`:62-71`), and
  installed-awaiting-node-start (`:73-75`) branches are untouched.
- **The three OTHER emissions and the import STAY (the emissions nuance).** `emitMithrilPartialSyncStatus` is
  imported at `:8` and still used **three** times after the deletion (line numbers shifted up 6 by the removed
  block): `:116` (`startInstalledNode`, `status:'starting-node'` — KEEP), `:132` (`startInstalledNode` catch,
  `status:'failed'`, `stage:'starting-node'` — the **IN-SESSION** failure that drives the React overlay, KEEP)
  and `:173` (`finalizeInstalledNodeStart`, `status:'completed'`, Boundary C2 — KEEP). The import was therefore
  **retained**; `tsc --noEmit` (exit 0) is the guard against an accidental unused-import error. Only the
  startup-interrupted `failed` emission was removed.

**MithrilStartupGate disposition — verified no change needed (no production edit).**
`source/main/mithril/MithrilStartupGate.ts` is in the JSON `targetPaths` but is **unchanged** (empty
`git diff`). The gate transitions to the recovery state via the **BLOCKED boolean** returned by
`handleInterruptedRecovery` (`MithrilStartupGate.ts:195-205`:
`const blocked = await deps.partialSyncNodeStartup.handleInterruptedRecovery(currentGeneration); if (blocked) {
this._transition('partialSyncCutoverUnsafe', 'partialSyncMarkerDetected'); return null; }`), **not** via the
removed `failed` emission — and `handleInterruptedRecovery` still returns `true` on Quit / mid-dialog
generation change (byte-unchanged), so `partialSyncCutoverUnsafe` is still set by the more-specific
marker-detected path. The removed emission's only gate side-effect (`onPartialSyncStatus` `:157-167` →
`failed` falling to the generic `else if (status !== 'idle')` → `partialSyncRunning`) was both less specific
**and unconsumed**: `getStartupGateState()` (`MithrilController.ts:155-157`) has no live caller. Node-start
gating (`isPartialSyncNodeStartBlocked` → `isMithrilPartialSyncBlockingNodeStart`,
`mithril-partial-sync.types.ts:106-118`) **excludes `failed`**, so the removed emission never gated node-start;
after removal `_partialSyncStatus` simply stays at its default `idle` on this path (cleaner, still
non-blocking). No real dependency on the removed emission was found in the gate, controller, channel,
renderer, or `setup.ts` — no finding to surface. Full trace in `task-ux-204-research.md` section 2.

**No stale overlay to clear (renderer).** The overlay mounts on `MithrilPartialSyncStore.shouldShowOverlay`
(`App.tsx:97`), driven by `isMithrilPartialSyncOverlayStatus(failed)`. The removed emission was *exactly* what
mounted the competing overlay on this startup path; with it gone the overlay never MOUNTS, so there is nothing
to clean up afterward.

### Tests added (4 new; 13/13 pass)
All four appended inside the existing `describe('handleInterruptedRecovery', ...)` block, marker state
`cutover-in-progress` for all four, reusing the existing mocks (`dialog.showMessageBox`,
`emitMithrilPartialSyncStatus`, the marker functions, `makeInstance`) — no new mocks.
- **Test 1** `unsafe cutover: shows ONLY the native dialog and does NOT emit a failed status (single surface,
  PRD D5a)` — dialog `{ response: 1 }`; asserts `dialog.showMessageBox` called **exactly once** AND
  `emitMithrilPartialSyncStatus` **not** called with `objectContaining({ status: 'failed' })`. The core
  single-surface proof (inverts the existing C2 test). Because `failed` is never emitted, no React overlay can
  mount (`App.tsx:97` → `shouldShowOverlay` → `isMithrilPartialSyncOverlayStatus(failed)`).
- **Test 2** `unsafe cutover, Wipe (response 0): wipes chain + snapshots, clears the marker, returns false` —
  dialog `{ response: 0 }`; asserts `result === false`, `wipeChainAndSnapshots` called once,
  `clearMithrilPartialSyncMarker` called once, still no `failed` emission.
- **Test 3** `unsafe cutover, Quit (response 1): returns true (blocked) without wiping or clearing the marker`
  — dialog `{ response: 1 }`; asserts `result === true`, no wipe, no marker clear.
- **Test 4** `unsafe cutover, generation changed during the dialog: returns true without wiping` — dialog
  `{ response: 0 }` but `getGeneration.mockReturnValue(1)` so the `:93` re-check sees `0 !== 1` and
  short-circuits before the wipe; asserts `result === true` and no wipe. Used the plan's documented simpler
  `mockReturnValue(1)` form (not the one-shot `mockReturnValueOnce(0).mockReturnValue(1)`, which the plan
  critiquer flagged as failing because the marker read does not call `getGeneration`).

The in-session-overlay path is preserved **by construction**: the `startInstalledNode` catch emission (`:132`,
the in-session `failed`) is untouched, so its overlay behavior is unaffected.

### Verification results (SCRIBE re-run, 2026-06-24)
- `./node_modules/.bin/jest source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` → **13 passed, 13
  total**, 0 failures (impl + code-review + SCRIBE all agree; was 9 before, +4 new).
- `./node_modules/.bin/tsc --noEmit` → **exit 0** (clean; `tsc` directly per project memory — `yarn compile`'s
  `typedef:sass` hook fails on Node 24). Confirms the retained `emitMithrilPartialSyncStatus` import produced
  no unused-import error and the deletion type-checks.
- `./node_modules/.bin/eslint source/main/mithril/mithrilPartialSyncNodeStartup.ts
  source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` → **0 errors, 2 warnings**, exit 0. Both warnings
  are pre-existing on `mithrilPartialSyncNodeStartup.ts:21-22` (the `WipeChainAndSnapshots` type's unused
  `reason`/`nodeState` params, outside the edited hunk), identical to the pre-change baseline (spec file 0
  warnings). **No new warning introduced.**

### Deviations
**None.** Step 1 (delete the single `failed` emission, leave the one-line PRD D5(a) comment, retain the
import), Step 2 (the four unsafe-cutover tests, using the plan's documented simpler
`getGeneration.mockReturnValue(1)` fallback for Test 4), Step 3 (no other production changes —
`MithrilStartupGate.ts` and all controllers/channels/renderer/`setup.ts` untouched) implemented exactly per
the approved plan. Locked invariants #2 (consolidate onto the backend-owned native dialog; dialog/blocking/Quit
byte-unchanged; wipe stays gated behind the user's dialog choice) and #11 (no in-session-overlay regression; no
empty-chain bootstrap regression) honored.

### Anchor drift recorded (research note section 4)
JSON description + PRD D5(a) cite `mithrilPartialSyncNodeStartup.ts:69-90` for the emission; research-19 gap #7
cites `:69-105`. Live (pre-edit) was `:77-87` (emission) / `:89-98` (dialog); the gate anchor `:194-205` is
live `:195-205`. Preferred live code throughout. Post-edit: the dialog sits at `:82-91`, the three KEPT
emissions at `:116` / `:132` / `:173` (shifted up 6 lines by the deletion).

### Hand-offs (not in scope here)
- **task-ux-403** — in-session error copy (D5(b) stage-aware copy / the React overlay's failure messaging). The
  in-session `failed` emission this task deliberately preserved (`:132`, `stage:'starting-node'`) is the surface
  task-ux-403 refines; this task only consolidated the *startup-interrupted* path onto the native dialog.
- **task-ux-503** — cross-cutting native-dialog-single-surface coverage (integration/E2E that the
  startup-interrupted recovery presents only the native dialog and never the React overlay). This task's tests
  are unit-level (the node-startup spec).
- Sibling D5 items remain separate tasks, NOT in scope here: D5(b) stage-aware copy, D5(c) no-Cancel-after-
  cutover, D5(f) cancel-resync (task-ux-203 backend half / task-ux-404 renderer half).

## Status
- Planning status: `approved`
- Build status: `completed`
