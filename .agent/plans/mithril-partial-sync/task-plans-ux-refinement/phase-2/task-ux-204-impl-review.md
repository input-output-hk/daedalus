# task-ux-204 — Implementation review transcript (append-only)

Append entries at the END only. Never rewrite history. `Implementation:` entries from the implementer;
`Code Review:` entries from the reviewer. Each entry: speaker label, ISO-8601 UTC `Timestamp:`, and an
outcome. Code Review entries END with `Decision: approved` or `Decision: requires_changes`.

---

## Implementation:

**Timestamp:** 2026-06-24T14:54:23Z

### Files changed

1. `source/main/mithril/mithrilPartialSyncNodeStartup.ts` (`handleInterruptedRecovery`, unsafe-cutover path,
   formerly `:77-87`) — **Removed the redundant React `failed` emission**: deleted the entire
   `await emitMithrilPartialSyncStatus({ status: 'failed', allowedRecoveryActions: ['wipe-and-full-sync'],
   transferProgress: {}, progressItems: [], error: { message: '...interrupted Mithril partial sync after live
   chain cutover...', stage: 'installing' } });` call and replaced it with a 4-line comment citing PRD D5(a) /
   gap #7 ("native dialog below is the SINGLE authoritative startup-interrupted recovery surface ... In-session
   failures keep the React overlay"). The native `dialog.showMessageBox` (now `:82-91`), the generation re-check
   (`currentGeneration !== this._getGeneration()`), the wipe (`_wipeChainAndSnapshots`) + `clearMithrilPartialSyncMarker`
   on `response === 0`, and the `return true` (Quit / blocked) are **byte-unchanged**. The
   `emitMithrilPartialSyncStatus` import (`:8`) is **retained** — still used 3 times (`:117` starting-node,
   `:133` in-session failed, `:174` completed; line numbers shifted up by 6 after the deletion). The three KEPT
   emissions and the no-marker / node-start-verified C2 / installed-awaiting-node-start branches are untouched.

2. `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` (EXTENDED — four new tests appended inside the
   existing `describe('handleInterruptedRecovery', ...)` block, after the `installed-awaiting-node-start` test;
   marker state `cutover-in-progress` for all four; reused existing mocks — `dialog.showMessageBox`,
   `emitMithrilPartialSyncStatus`, the marker functions, `makeInstance`; no new mocks) —
   - **Test 1** `unsafe cutover: shows ONLY the native dialog and does NOT emit a failed status (single
     surface, PRD D5a)`: dialog mocked `{ response: 1 }`; asserts `dialog.showMessageBox` called **exactly once**
     AND `emitMithrilPartialSyncStatus` **not** called with `objectContaining({ status: 'failed' })`. This is the
     core single-surface proof (inverts the existing C2 test).
   - **Test 2** `unsafe cutover, Wipe (response 0): wipes chain + snapshots, clears the marker, returns false`:
     dialog `{ response: 0 }`; asserts `result === false`, `wipeChainAndSnapshots` called once,
     `clearMithrilPartialSyncMarker` called once, and still no `failed` emission.
   - **Test 3** `unsafe cutover, Quit (response 1): returns true (blocked) without wiping or clearing the
     marker`: dialog `{ response: 1 }`; asserts `result === true`, `wipeChainAndSnapshots` not called,
     `clearMithrilPartialSyncMarker` not called.
   - **Test 4** `unsafe cutover, generation changed during the dialog: returns true without wiping`: dialog
     `{ response: 0 }` (user picked Wipe) but `getGeneration.mockReturnValue(1)` so the re-check sees `0 !== 1`;
     asserts `result === true` and no wipe (stale-generation short-circuit). Used the plan's fallback
     `getGeneration.mockReturnValue(1)` form (not the one-shot `mockReturnValueOnce(0).mockReturnValue(1)`), since
     the marker read does not call `getGeneration` and the re-check is the only `getGeneration` consumer on this
     path — both forms drive the same `0 !== 1` short-circuit; the simple form is less brittle.

### Targets in JSON that did NOT change (verified, per approved plan)

- `source/main/mithril/MithrilStartupGate.ts` — **no change. Disposition: verified no change needed.** The gate
  transitions to the recovery state via the **BLOCKED boolean** returned by `handleInterruptedRecovery`
  (`MithrilStartupGate.ts:195-205`: `const blocked = await deps.partialSyncNodeStartup.handleInterruptedRecovery(...)`
  → `if (blocked) { this._transition('partialSyncCutoverUnsafe', 'partialSyncMarkerDetected'); return null; }`),
  **not** via the removed `failed` emission. `handleInterruptedRecovery` still returns `true` on Quit and on a
  mid-dialog generation change (both byte-unchanged by this task), so `partialSyncCutoverUnsafe` is still set by
  the more-specific marker-detected path. The removed emission's only gate side-effect — falling through
  `onPartialSyncStatus` (`:157-167`) to the generic `else if (status.status !== 'idle')` →
  `_transition('partialSyncRunning', 'partialSyncActive')` — was both **less specific** and **unconsumed**:
  `getStartupGateState()` (`MithrilController.ts:155-157`) has no live caller. Node-start gating
  (`isPartialSyncNodeStartBlocked` → `isMithrilPartialSyncBlockingNodeStart`,
  `mithril-partial-sync.types.ts:106-118`) **excludes `failed`**, so the removed emission never gated node-start;
  after removal `_partialSyncStatus` simply stays at its default `idle` (cleaner, still non-blocking). No real
  dependency on the removed emission was found in the gate, controller, channel, renderer, or `setup.ts`. No
  finding to surface.

- Renderer: no stale overlay to clear. The overlay mounts on `MithrilPartialSyncStore.shouldShowOverlay`
  (`App.tsx:97`), driven by `isMithrilPartialSyncOverlayStatus(failed)`. The removed emission was exactly what
  mounted the overlay on this startup path; with it gone the overlay never MOUNTS, so there is nothing to clean
  up afterward.

### Verification

- **`./node_modules/.bin/tsc --noEmit`**: exit code **0** (clean; ran `tsc` directly per project memory —
  `yarn compile`'s `typedef:sass` hook fails on Node 24). Confirms the retained `emitMithrilPartialSyncStatus`
  import produced no unused-import error and the deletion type-checks.
- **`./node_modules/.bin/jest source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`**: exit code **0** —
  **13 passed, 13 total** (was 9; +4 new unsafe-cutover tests). 0 failures, 0 skipped. The
  `handleInterruptedRecovery` block now has 9 tests (was 5), including the 4 new dialog-branch tests; the 4
  `finalizeInstalledNodeStart` tests still pass unchanged.
- **`./node_modules/.bin/eslint source/main/mithril/mithrilPartialSyncNodeStartup.ts
  source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`**: exit code **0** — **0 errors, 2 warnings**.
  Both warnings are pre-existing on `mithrilPartialSyncNodeStartup.ts:21-22` (the `WipeChainAndSnapshots` type's
  unused `reason`/`nodeState` params), identical to the pre-change baseline (captured before editing: 2 warnings
  on the `.ts`, 0 on the spec). **No new warning introduced.**

### Acceptance criteria coverage (carried from tasks JSON)

1. **Only one surface (native dialog) handles startup-interrupted unsafe-cutover recovery.** The redundant
   `emitMithrilPartialSyncStatus({ status:'failed', ... })` is deleted; only `dialog.showMessageBox` remains on
   that path. Proven by Test 1 (dialog called exactly once AND no `failed` emission) — and because `failed` is
   never emitted, no React overlay can mount (`App.tsx:97` → `shouldShowOverlay` →
   `isMithrilPartialSyncOverlayStatus(failed)`). Tests 2–4 lock the wipe/quit/generation behavior unchanged. ✓
2. **In-session failure overlay behavior is unchanged.** The `startInstalledNode` catch emission
   (`status:'failed'`, `stage:'starting-node'`) is untouched and keeps driving the React overlay; only the
   startup-interrupted (`handleInterruptedRecovery`) emission was removed. The existing C2 test plus the
   unchanged catch path establish the in-session surface is unaffected. ✓

### Deviations from plan

None. Implemented exactly per the approved plan: Step 1 (delete the single `failed` emission, leave the
one-line PRD D5(a) comment, retain the import), Step 2 (the four unsafe-cutover tests including the optional
generation-change test, using the plan's documented simpler `getGeneration.mockReturnValue(1)` fallback form),
Step 3 (no other production changes — `MithrilStartupGate.ts` and all controllers/channels/renderer/`setup.ts`
untouched). Locked invariants #2 (consolidate onto the backend-owned native dialog; dialog/blocking/Quit
byte-unchanged) and #11 (no in-session-overlay regression; no empty-chain bootstrap regression) honored.

### Anchor drift recorded

JSON description + PRD D5(a) cite `mithrilPartialSyncNodeStartup.ts:69-90` for the emission; research-19 gap #7
cites `:69-105`. Live (pre-edit) was `:77-87` (emission) / `:89-98` (dialog); the gate anchor `:194-205` is live
`:195-205`. Preferred live code throughout (per the orchestration rule). Post-edit the dialog sits at `:82-91`
and the three KEPT emissions at `:117` / `:133` / `:174` (shifted up 6 lines by the deletion).

---

## Code Review:

**Timestamp:** 2026-06-24T14:56:53Z

Iteration 1 — one broad pass over the working-tree diff vs the approved plan
(`task-ux-204.md`). Independent verification, re-running every check (not trusting reported numbers).

### What I verified (file:line, live code)

- **The one-line change is exactly the seam, and ONLY the seam.** `git diff` on
  `mithrilPartialSyncNodeStartup.ts` shows a single hunk: the `await emitMithrilPartialSyncStatus({ status:
  'failed', allowedRecoveryActions: ['wipe-and-full-sync'], transferProgress: {}, progressItems: [], error: {
  message: '...interrupted Mithril partial sync after live chain cutover...', stage: 'installing' } });` call is
  deleted and replaced by a 4-line PRD-D5(a)/gap-#7 comment (`mithrilPartialSyncNodeStartup.ts:77-80`). Nothing
  else in the hunk. The native `dialog.showMessageBox` (`:82-91`), the generation re-check (`:93-95`,
  `currentGeneration !== this._getGeneration()`), the wipe `_wipeChainAndSnapshots` + `clearMithrilPartialSyncMarker`
  on `response === 0` (`:97-104`), and the Quit `return true` (`:106`) are **byte-unchanged** (confirmed by
  reading live `:55-107`). The no-marker (`:58-60`), node-start-verified C2 (`:62-71`), and
  installed-awaiting-node-start (`:73-75`) branches are untouched.

- **Import retained and still live (lock against unused-import / accidental over-deletion).** `grep` shows
  `emitMithrilPartialSyncStatus` imported at `:8` and used exactly 3x: `:116` (`starting-node`, KEEP), `:132`
  (in-session `failed`, `stage:'starting-node'`, KEEP), `:173` (`completed`, KEEP). The startup-interrupted
  `failed` emission is the only one gone. `tsc --noEmit` (exit 0) is the guard — no unused-import error.

- **In-session React overlay path preserved (lock #11).** The `startInstalledNode` catch emission (`:132-143`,
  `status:'failed'`, `stage:'starting-node'`) is untouched — in-session failures still drive the overlay.
  Empty-chain bootstrap flow not touched (change is confined to the partial-sync startup-interrupted seam).

- **Locked invariant #2 (backend/startup-owned single surface).** The native `dialog.showMessageBox` is the
  sole remaining recovery surface on this path; copy, button set (`['Wipe chain and full Mithril sync','Quit']`),
  `defaultId/cancelId/noLink`, and blocking behavior are byte-unchanged. No new abstraction, channel, type, or
  copy introduced anywhere in the diff.

- **MithrilStartupGate disposition independently re-confirmed (verified no change needed).** Live
  `MithrilStartupGate.ts:195-205`: `const blocked = await deps.partialSyncNodeStartup.handleInterruptedRecovery(
  currentGeneration); if (blocked) { this._transition('partialSyncCutoverUnsafe', 'partialSyncMarkerDetected');
  return null; }` — the `partialSyncCutoverUnsafe` transition fires off the BLOCKED boolean, NOT the removed
  emission. `handleInterruptedRecovery` still returns `true` on Quit/generation-change (byte-unchanged). On the
  unsafe path the gate returns `null` at `:205` and never reaches the later `isMithrilPartialSyncBlockingNodeStart(
  partialSyncStatus)` check (`:488`); and that check reads `_controller.getPartialSyncStatus().status` (`:457`),
  which after removal stays `idle` — and `failed` was never in the blocking list anyway
  (`mithril-partial-sync.types.ts:106-118`, confirmed `failed` absent). `getStartupGateState()` has **no live
  caller**: `grep` across `source/`, `tests/`, `storybook/` returns only its definition (`MithrilController.ts:155`).
  Marker state `cutover-in-progress` confirmed as a valid `MithrilPartialSyncMarkerState`
  (`mithrilPartialSyncMarker.ts:6-9`) and the corrupt/unreadable-marker fallback (`:77`). No dependency on the
  removed emission found in gate/controller/channel/renderer/setup.

- **Tests genuinely prove the contract and use a valid state.** Read `spec.ts:158-224`: all 4 new tests use
  marker state `cutover-in-progress`. Test 1 asserts `dialog.showMessageBox` called exactly once AND
  `emitMithrilPartialSyncStatus` NOT called with `objectContaining({ status: 'failed' })` — the core
  single-surface proof, correctly inverting the C2 test (`:129-143`). Tests 2/3 lock Wipe (returns false, wipe +
  clear, no `failed`) and Quit (returns true, no wipe/clear). Test 4 locks the generation-change short-circuit.
  Mocks reuse the existing harness; no new mocks.

### Independent verification results (re-run from repo root)

- `./node_modules/.bin/jest source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` → **13 passed / 13 total**
  (+4 unsafe-cutover), exit 0. All 4 new tests green.
- `./node_modules/.bin/tsc --noEmit` → **exit 0** (clean). Confirms retained import is used; deletion type-checks.
- `./node_modules/.bin/eslint` on the two touched files → **0 errors, 2 warnings**, exit 0. Both warnings are
  pre-existing on `mithrilPartialSyncNodeStartup.ts:21-22` (the `WipeChainAndSnapshots` type's unused
  `reason`/`nodeState` params — outside the edited hunk). **No new warning** — matches the plan's baseline of 2.

### Doc drift / scope notes

- Implementation matches the approved plan with no deviations. Anchor drift correctly recorded (JSON/PRD `:69-90`,
  research `:69-105` vs live `:77-87` emission / `:89-98` dialog; gate `:194-205` vs live `:195-205`).
- The diff also contains two non-production files (`.agent/.../prompt-ux-refinement.md`, `.gitignore`) that were
  already modified (`M`) in the pre-task git status — orchestrator/env files, not task-relevant production code;
  out of scope for this task's commit and not a blocker. (Commit-scope hygiene is the scribe stage's concern.)

### Blockers

None.

Decision: approved
