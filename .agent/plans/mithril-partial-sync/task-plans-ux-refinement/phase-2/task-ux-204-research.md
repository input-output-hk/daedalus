# task-ux-204 — Research note (durable findings)

> PRD **D5(a)** / research-19 **gap #7**: make the native Electron dialog the SINGLE authoritative
> startup-interrupted recovery surface by removing the redundant React `failed` emission on that one path,
> eliminating the two-competing-surfaces problem. In-session failures are unaffected (they keep the React
> overlay). This note captures the durable reasoning that is NOT obvious from the one-deletion diff: the
> three-emissions nuance (why the import stays), the `MithrilStartupGate.ts` "no change needed" disposition
> with its consumer trace, the JSON/PRD/research→live anchor drift, how the tests prove single-surface
> behavior, and the locked invariants honored.

- Sprint: Mithril Partial Sync UX Refinement — phase-2 (Backend UX-Enabling Correctness)
- Source file changed: `source/main/mithril/mithrilPartialSyncNodeStartup.ts`
  (`handleInterruptedRecovery`, unsafe-cutover path) + its spec.
- Created at finalize time (2026-06-24) by the SCRIBE; line numbers re-verified against live code.

---

## 1. The seam and the fix (verified live)

`MithrilPartialSyncNodeStartup.handleInterruptedRecovery(currentGeneration)`
(`mithrilPartialSyncNodeStartup.ts:55`) walks the recovery marker through four branches:

1. **no-marker** (`:58-60`): `if (!marker) return false;` — nothing to recover. **KEEP.**
2. **node-start-verified / Boundary C2** (`:62-71`): a prior run already proved one successful node start on
   the installed DB; reclaim leftover staging (`fs.remove(marker.stagingRootPath)`), clear the marker, resume
   a normal boot (`return false`). **KEEP.**
3. **installed-awaiting-node-start** (`:73-75`): `return false` (B/C1 hand-off). **KEEP.**
4. **UNSAFE interrupted cutover** (the else — reached when the marker is `cutover-in-progress`): this is the
   path that fired **two competing surfaces**.

Before this task, branch 4 did BOTH:
- emitted a React `failed` status (formerly `:77-87`) — which mounts the in-window React overlay with recovery
  CTAs derived from `allowedRecoveryActions:['wipe-and-full-sync']`, AND
- blocked on the native `dialog.showMessageBox` (formerly `:89-98`) — with a divergent button set
  (`['Wipe chain and full Mithril sync','Quit']`).

**Fix (the only change):** delete the `await emitMithrilPartialSyncStatus({ status: 'failed', ... });` call
and replace it with a 4-line comment (`:77-80`) citing PRD D5(a) / gap #7. Everything else in branch 4 is
**byte-unchanged**: the native `dialog.showMessageBox` (now `:82-91`), the generation re-check
(`currentGeneration !== this._getGeneration()`, `:93-95`), the wipe (`_wipeChainAndSnapshots` +
`clearMithrilPartialSyncMarker`) on `response === 0` (`:97-104`), and the Quit `return true` (`:106`).

### Why the native dialog is the right single surface (lock #2)
Startup-owned recovery **must not depend on diagnostics UI or a running node** and must work **before the
renderer is ready**, be correctly **blocking**, and include **Quit**. The native Electron dialog satisfies all
of that; the React overlay cannot (it needs a mounted renderer). On the startup-interrupted path the renderer
may not even exist yet — so consolidating onto the native dialog is the only correct single surface, and the
removed emission was the redundant one.

---

## 2. The three-emissions nuance — why the import STAYS (durable)

`emitMithrilPartialSyncStatus` is imported at `mithrilPartialSyncNodeStartup.ts:8`. A naive reading of "remove
the failed emission" might delete the import — that would be **wrong** and would break the build. The function
is used **four** times in this file; only ONE is removed:

| Emission | Live line (post-edit) | Status | Disposition |
|----------|-----------------------|--------|-------------|
| startup-interrupted unsafe cutover | (was `:77`) | `failed`, `stage:'installing'` | **REMOVED** (this task) |
| `startInstalledNode` announce | `:116` | `starting-node` | **KEEP** |
| `startInstalledNode` catch | `:132` | `failed`, `stage:'starting-node'` | **KEEP** — this is the **IN-SESSION** failure that drives the React overlay (lock #11) |
| `finalizeInstalledNodeStart` | `:173` | `completed` (Boundary C2 success) | **KEEP** |

(The KEPT emissions shifted **up 6 lines** after the removed block; pre-edit they were `:123`/`:139`/`:180`.)

So the import is **retained**, and `./node_modules/.bin/tsc --noEmit` (exit 0) is the concrete guard against an
accidental unused-import error. The critical correctness point: the `:132` catch emission is the *in-session*
`failed` surface — removing it would regress the in-session React overlay (lock #11). This task removes ONLY
the *startup-interrupted* `failed` emission; the two `failed` emissions are on mutually distinct paths
(startup recovery vs. in-session node-start failure) with distinct `stage` values (`installing` vs.
`starting-node`).

---

## 3. `MithrilStartupGate.ts` disposition — VERIFIED NO CHANGE NEEDED (durable, with consumer trace)

`source/main/mithril/MithrilStartupGate.ts` is in the tasks-JSON `targetPaths` for task-ux-204, but the
verified disposition is **no change needed** (mirrors task-ux-203's channel-file disposition). This is the kind
of thing a future reader will re-question ("the JSON lists the gate — did we forget it?"). The answer is no.
Full evidence trace (file:line, re-verified live):

1. **The gate's authoritative block is the BLOCKED boolean, NOT the removed emission.**
   `ensureMithrilStartupGate` calls
   `const blocked = await deps.partialSyncNodeStartup.handleInterruptedRecovery(currentGeneration)`
   (`MithrilStartupGate.ts:195-198`); on `blocked === true` it does
   `this._transition('partialSyncCutoverUnsafe', 'partialSyncMarkerDetected'); return null;` (`:199-205`).
   `handleInterruptedRecovery` STILL returns `true` on Quit and on a mid-dialog generation change — both
   **byte-unchanged** by this task. So `partialSyncCutoverUnsafe` is still set, by the more-specific
   marker-detected path. The startup block (the gate returning `null`) is fully preserved.

2. **The removed emission's only gate side-effect was benign AND unconsumed.** The removed
   `emitMithrilPartialSyncStatus({status:'failed'})` reached the gate via `broadcastPartialSyncStatus`
   (`MithrilController.ts:257`) → `_startupGate.onPartialSyncStatus(status)`. In `onPartialSyncStatus`
   (`:157-167`), `failed` is not `installing`/`starting-node`/`idle`, so it fell to the generic
   `else if (status.status !== 'idle')` → `_transition('partialSyncRunning', 'partialSyncActive')`. That
   transition is **less specific** than the `partialSyncCutoverUnsafe` the BLOCKED path sets, and — critically
   — **nothing reads the gate `_state` on this path**: `getStartupGateState()`
   (`MithrilController.ts:155-157`) has **no live caller** (grep across `source/`, `tests/`, `storybook/`
   returns only its definition). Losing the `partialSyncRunning` transition is therefore inert.

3. **Node-start gating does not depend on the removed `failed` emission.** The only live reader of
   `_partialSyncStatus.status` for gating is `isPartialSyncNodeStartBlocked()`
   (`MithrilController.ts:188-192` → `isMithrilPartialSyncBlockingNodeStart`, consumed at the `onCrashed`
   handler `setup.ts:132`). **`failed` is NOT in `isMithrilPartialSyncBlockingNodeStart`'s list**
   (`mithril-partial-sync.types.ts:106-118`), so the removed emission never blocked node-start. After removal,
   `_partialSyncStatus` simply stays at its default `idle` on this path — cleaner, still non-blocking.

4. **No renderer dependency / no stale overlay to clear.** The renderer overlay mounts on
   `MithrilPartialSyncStore.shouldShowOverlay` (`App.tsx:97`, live path `source/renderer/app/App.tsx` — the
   plan's `containers/App.tsx` was a cosmetic path slip noted by the critiquer), driven by
   `isMithrilPartialSyncOverlayStatus` over the status; `failed` IS an overlay status
   (`MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES`, `mithril-partial-sync.types.ts:79-84`). The removed emission was
   **precisely** what mounted the competing overlay on this path — its removal is the intended single-surface
   effect. Because the overlay never MOUNTS afterward, there is no stale overlay state to clean up.

**Disposition: `MithrilStartupGate.ts` — verified no change needed** (empty `git diff`). No
gate/controller/channel/renderer/setup logic *requires* the removed emission. Independently re-traced by both
the plan critiquer and the code reviewer.

---

## 4. Anchor drift found (prefer live code; recorded for future readers)

| Anchor | Cited (JSON/PRD/research) | Live pre-edit | Live post-edit | Note |
|--------|---------------------------|---------------|----------------|------|
| startup-interrupted `failed` emission (the seam to remove) | `:69-90` (JSON desc + PRD D5(a) `prd:265-272`); `:69-105` (research-19 gap #7 `:134`) | `:77-87` | deleted; comment at `:77-80` | the only change |
| native dialog (the single surface) | within `:69-105` (research) | `:89-98` | `:82-91` | KEEP byte-unchanged |
| generation re-check / wipe+clear / quit | within `:100-113` | `:100-102` / `:104-111` / `:113` | `:93-95` / `:97-104` / `:106` | KEEP byte-unchanged |
| three KEPT emissions | — | `:123` / `:139` / `:180` | `:116` / `:132` / `:173` | shifted up 6 by the deletion |
| gate BLOCKED→state transition | `MithrilStartupGate.ts:194-205` (JSON/research) | `:195-205` | `:195-205` | unchanged; off-by-one anchor |

Live code was preferred throughout (per the orchestration rule). The drift is benign — the emission/dialog
anchors had drifted ~8 lines and the gate anchor was off by one; recorded so a future task that greps the
JSON/PRD `:69-90` does not chase a stale region.

---

## 5. Tests (durable: what proves what)

All four new tests live in `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`, inside the existing
`describe('handleInterruptedRecovery', ...)` block, after the `installed-awaiting-node-start` test. Marker
state for every one is **`cutover-in-progress`** (a valid `MithrilPartialSyncMarkerState`,
`mithrilPartialSyncMarker.ts:6-9`; also the corrupt/unreadable-marker fallback `:76-79`), which is neither
`node-start-verified` nor `installed-awaiting-node-start`, so it reaches branch 4. They reuse the existing mock
harness (`dialog.showMessageBox`, `emitMithrilPartialSyncStatus`, the marker functions, `makeInstance`); no new
mocks. Dialog result is set per-test via `(dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: n })`.

- **Test 1 — single surface (the core proof; inverts the existing C2 test).** Dialog `{ response: 1 }`.
  Asserts `dialog.showMessageBox` called **exactly once** AND `emitMithrilPartialSyncStatus` **not** called
  with `objectContaining({ status: 'failed' })`. This is the direct single-surface proof: the native dialog IS
  the surface, the React `failed` emission is GONE. And because `failed` is never emitted, **no React overlay
  can mount** (`App.tsx:97` → `shouldShowOverlay` → `isMithrilPartialSyncOverlayStatus(failed)`) — so the
  two-competing-surfaces condition is eliminated by construction, not merely visually.
- **Test 2 — Wipe (`response 0`).** Asserts `result === false`, `wipeChainAndSnapshots` called once,
  `clearMithrilPartialSyncMarker` called once, still no `failed` emission. Locks the wipe path unchanged.
- **Test 3 — Quit (`response 1`).** Asserts `result === true` (blocked), no wipe, no marker clear. Locks the
  blocked path unchanged.
- **Test 4 — generation changed mid-dialog.** Dialog `{ response: 0 }` (user picked Wipe) but
  `getGeneration.mockReturnValue(1)` so the `:93` re-check sees `0 !== 1` and short-circuits; asserts
  `result === true` and no wipe. Uses the simpler `mockReturnValue(1)` form: `handleInterruptedRecovery` calls
  `this._getGeneration()` exactly once (the re-check; the marker read does NOT read generation), so the plan's
  alternative one-shot `mockReturnValueOnce(0).mockReturnValue(1)` would FAIL (the single call consumes the
  `0`, re-check sees `0 !== 0` false → wipes → returns false). The critiquer caught this; the simpler form is
  correct and less brittle.

**In-session overlay preserved by construction:** the `startInstalledNode` catch emission (`:132`, the
in-session `failed`, `stage:'starting-node'`) is untouched, so its overlay behavior is unaffected — no new test
needed; the existing C2 test (`does NOT emit failed status and does NOT call dialog.showMessageBox`) plus the
unchanged catch path establish the in-session surface is unaffected.

---

## 6. Hand-offs

- **task-ux-403 — in-session error copy (D5(b) stage-aware copy).** The in-session `failed` emission this task
  deliberately preserved (`:132`, `stage:'starting-node'`) is the React-overlay surface whose copy task-ux-403
  refines. This task only consolidated the *startup-interrupted* path onto the native dialog; it does not touch
  the in-session overlay's messaging.
- **task-ux-503 — cross-cutting native-dialog-single-surface coverage.** This task's tests are unit-level (the
  node-startup spec). Integration/E2E coverage that the startup-interrupted recovery presents only the native
  dialog and never the React overlay belongs to the cross-cutting coverage task.
- **Sibling D5 items (separate tasks, NOT in scope here):** D5(b) stage-aware copy (task-ux-403), D5(c)
  no-Cancel-after-cutover, D5(f) cancel-resync (task-ux-203 backend half / task-ux-404 renderer half).

---

## 7. Locked invariants honored (summary)

- **#2 — Boundary recovery is BACKEND-AUTHORITATIVE; the native startup dialog is the startup-owned recovery
  surface (must not depend on diagnostics UI or a running node; works before the renderer is ready; blocking;
  includes Quit).** This task CONSOLIDATES onto that backend-owned native dialog by removing the competing
  React surface. The dialog, its button set, blocking behavior, and Quit option are byte-unchanged; the wipe
  stays gated behind the user's explicit dialog choice (`response === 0`).
- **#11 — No empty-chain bootstrap regression; no in-session React overlay regression.** Only the
  *startup-interrupted* (`handleInterruptedRecovery`) emission is removed. The in-session `startInstalledNode`
  catch emission (`:132`) → React overlay is untouched. Bootstrap flow and shared components untouched.
- **Smallest-truthful change.** Exactly one production emission deleted (import retained — 3 other uses), a
  one-line comment left in its place, four tests added, `MithrilStartupGate.ts` unchanged (evidence-backed
  disposition). No new abstraction, channel, type, or copy; no behavior change to wipe/quit/generation logic.

No PRD edit required: D5(a) already specifies the single-surface consolidation; this task is its
implementation.
