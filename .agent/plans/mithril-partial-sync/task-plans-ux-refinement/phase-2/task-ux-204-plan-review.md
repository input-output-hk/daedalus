# task-ux-204 — Plan review transcript (append-only)

Append entries at the END only. Never rewrite history. Each entry: speaker label, ISO-8601 UTC
`Timestamp:`, and an outcome. Critiquer/Code-Review entries END with `Decision: approved` or
`Decision: requires_changes`.

---

Planner:
Timestamp: 2026-06-24T14:48:38Z

Produced the canonical plan for task-ux-204 (Native dialog is the single startup-interrupted recovery surface
— PRD D5(a) / research-19 gap #7). Planning status `in_review`, build status `in_progress`. Interaction mode
`autonomous` (backend/main-process only; no user-facing copy authored — the native dialog copy already exists
and stays; no new product decision — D5(a) fully specifies; not destructive — removes a redundant emission, the
wipe is unchanged and already gated behind the user's dialog choice). No open user question.

CORE CHANGE (smallest truthful): in `MithrilPartialSyncNodeStartup.handleInterruptedRecovery`, delete exactly
the redundant React `failed` emission on the unsafe-cutover path — the
`await emitMithrilPartialSyncStatus({ status: 'failed', allowedRecoveryActions: ['wipe-and-full-sync'],
transferProgress: {}, progressItems: [], error: { message: '...', stage: 'installing' } });` call at
`mithrilPartialSyncNodeStartup.ts:77-87` — replacing it with a one-line comment. The native
`dialog.showMessageBox` (`:89-98`) becomes the single authoritative recovery surface, byte-unchanged, along
with the generation re-check / wipe / marker-clear / quit logic (`:100-113`).

ANCHOR DRIFT CORRECTED (prefer live code): JSON description + implementationNotes and PRD D5(a)
(`prd:265-272`) cite `mithrilPartialSyncNodeStartup.ts:69-90` for the emission; research-19 gap #7
(`research-19:134`) cites `:69-105` for the path. LIVE code is `:77-87` (the `failed` emission to remove) and
`:89-98` (the native dialog to keep). The gate anchor cited as `MithrilStartupGate.ts:194-205` is live
`:195-205`. All other CONTEXT anchors verified accurate: method at `:55`; no-marker `:58-60`; C2 `:62-71`;
installed-awaiting `:73-75`; the three KEPT emissions at `:123`/`:139`/`:180`; the import at `:7-10`; marker
union `mithrilPartialSyncMarker.ts:6-9` + corrupt-fallback `:76-79` → `cutover-in-progress`; spec mocks
(`dialog` `:11-15`/`:53`, `emit` `:23-32`/`:46-51`, `makeInstance` `:62-73`), existing `handleInterruptedRecovery`
describe `:85-157` with NO unsafe-cutover test.

IMPORT RETENTION — RESOLVED: the `emitMithrilPartialSyncStatus` import at `:8` MUST STAY. It has three other
live uses, all KEPT: `:123` (`starting-node`), `:139` (in-session `failed`, stage `starting-node` — the React
overlay path that must NOT regress), `:180` (`completed`). `tsc --noEmit` is the guard against an accidental
unused-import error. Only the `:77-87` startup-interrupted emission is removed.

MITHRILSTARTUPGATE.TS DISPOSITION — VERIFIED NO CHANGE NEEDED (evidence-backed, mirrors task-ux-203's
channel-file disposition). Four-point trace:
(1) The gate's authoritative block is the BLOCKED boolean: `ensureMithrilStartupGate`
    (`MithrilStartupGate.ts:195-198`) calls `handleInterruptedRecovery`, and on `blocked === true` does
    `_transition('partialSyncCutoverUnsafe','partialSyncMarkerDetected'); return null;` (`:199-205`).
    `handleInterruptedRecovery` still returns `true` on Quit / mid-dialog generation change (unchanged), so
    `partialSyncCutoverUnsafe` is still set — by the more-specific marker-detected path, not the removed
    emission.
(2) The removed emission's only gate side-effect was benign and unconsumed: it reached the gate via
    `broadcastPartialSyncStatus` (`MithrilController.ts:257`) → `onPartialSyncStatus` (`:157-167`), where
    `failed` fell to the generic `else if (status !== 'idle')` → `partialSyncRunning`. That state is less
    specific than `partialSyncCutoverUnsafe` AND nothing reads gate `_state` on this path —
    `getStartupGateState()` (`MithrilController.ts:155-157`) has NO live caller (grep across `source/` returns
    only its definition). Inert.
(3) Node-start gating does not depend on the removed `failed` emission: the only live reader of
    `_partialSyncStatus.status` for gating is `isPartialSyncNodeStartBlocked()` (`MithrilController.ts:188-192`
    → `isMithrilPartialSyncBlockingNodeStart`, consumed at `setup.ts:132` onCrashed), and `failed` is NOT in
    `isMithrilPartialSyncBlockingNodeStart`'s list (`mithril-partial-sync.types.ts:106-118`). After removal
    `_partialSyncStatus` stays `idle` on this path (cleaner; still non-blocking). The startup block is the
    gate returning `null`, preserved.
(4) No renderer dependency / no stale overlay: the overlay mounts on `shouldShowOverlay` (`App.tsx:97` →
    `MithrilPartialSyncStore.ts:97-100` → `isMithrilPartialSyncOverlayStatus(failed)`), so the removed emission
    was EXACTLY what mounted the competing overlay — its removal is the intended single-surface effect. Because
    the overlay never mounts, there is nothing to clean up afterward.
Disposition recorded as "verified no change needed"; if impl uncovers a real dependency, surface it as a
finding.

LOCKED INVARIANTS honored inline: #2 (consolidates onto the backend-owned native dialog — startup-owned
recovery that must not depend on diagnostics UI or a running node, works before renderer ready, blocking, with
Quit — all byte-unchanged; wipe stays gated behind the user's dialog choice). #11 (only the
startup-interrupted emission removed; in-session `startInstalledNode` catch emission `:139` → React overlay
untouched; empty-chain bootstrap untouched). Smallest-truthful (delete one emission, keep the import, no new
abstraction/channel/type/copy, no wipe/quit/generation behavior change).

TESTS (in `mithrilPartialSyncNodeStartup.spec.ts`, marker state `cutover-in-progress`, reusing existing mocks;
dialog result via `(dialog.showMessageBox as jest.Mock).mockResolvedValue({ response: n })`): Test 1 — single
surface: dialog called once AND `emit` NOT called with `status:'failed'` (inverts the C2 test `:129-143`); Test
2 — Wipe (response 0): wipe + clear marker + returns false; Test 3 — Quit (response 1): returns true, no
wipe/clear; Test 4 (optional) — generation changed mid-dialog: returns true, no wipe. In-session overlay is
preserved by construction (the `:139` catch emission is untouched). The two tasks-JSON testCases map 1:1 to the
acceptance criteria.

ACCEPTANCE (verbatim from JSON): (1) Only one surface (native dialog) handles startup-interrupted
unsafe-cutover recovery; (2) In-session failure overlay behavior is unchanged.

VERIFICATION: `./node_modules/.bin/tsc --noEmit` (also confirms import still used); `./node_modules/.bin/jest
source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`; `./node_modules/.bin/eslint` on the two touched
files — pre-change baseline is 2 warnings (both pre-existing on `mithrilPartialSyncNodeStartup.ts:21-22`,
unused `reason`/`nodeState` of the `WipeChainAndSnapshots` type; spec 0); introduce no new warning. Baseline
captured at planning time.

Smallest-truthful-change confirmed: one deleted production emission (import retained), four added tests,
`MithrilStartupGate.ts` unchanged (evidence-backed), no renderer/controller/setup edit, no new
abstraction/copy. No open question for the user. Both files written: `task-ux-204.md` and this review log.

---

Critiquer:
Timestamp: 2026-06-24T14:52:03Z

One broad pass, verified empirically against the LIVE repo and by prototyping the change (then reverting —
`git status` confirms source tree byte-identical; only the two plan docs remain).

VERIFIED CORRECT (read each file/line):
- The seam to remove is `mithrilPartialSyncNodeStartup.ts:77-87` (the `failed` emission); the native dialog is
  `:89-98`; generation re-check `:100-102`; wipe+clear `:104-111`; quit `return true` `:113`. All match live
  exactly. The three KEPT emissions are live: `:123` (`starting-node`), `:139` (in-session `failed`, stage
  `starting-node`), `:180` (`completed`). The plan's KEEP list and "smallest-truthful (one deletion)" are
  correct.
- Import retention: confirmed by `tsc --noEmit` on the prototyped change (exit 0, no unused-import error).
  The `emitMithrilPartialSyncStatus` import MUST and does stay (3 other uses).
- In-session overlay path preserved: the `startInstalledNode` catch emission (`:139`) is untouched. Lock #11
  honored.
- Marker state for tests: `cutover-in-progress` is a valid `MithrilPartialSyncMarkerState`
  (`mithrilPartialSyncMarker.ts:6-9`) and the corrupt/unreadable-marker fallback (`:76-79`) returns it. Reaches
  the dialog branch (it is neither `node-start-verified` nor `installed-awaiting-node-start`). Correct.
- MithrilStartupGate disposition (VERIFIED NO CHANGE NEEDED) — independently re-traced and CONFIRMED on all 4
  points: (1) gate blocks via the BLOCKED boolean `ensureMithrilStartupGate:195-205` →
  `partialSyncCutoverUnsafe`, unchanged by this task; (2) the removed emission's only gate effect was
  `onPartialSyncStatus:157-167` `failed`→generic `else if (status !== 'idle')`→`partialSyncRunning`, and
  `getStartupGateState` (`MithrilController.ts:155`) has NO caller (grep across `source/` returns only its def)
  — inert; (3) `isMithrilPartialSyncBlockingNodeStart` (`mithril-partial-sync.types.ts:106-118`) does NOT list
  `failed`, and it is the only gating reader of `_partialSyncStatus.status` (`isPartialSyncNodeStartBlocked`
  `MithrilController.ts:188-192`, consumed `setup.ts:132` onCrashed) — the removed emission never gated
  node-start; (4) `failed` IS in `MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES`
  (`mithril-partial-sync.types.ts:79-84`, includes the terminal statuses incl. `failed`), so the removed
  emission was EXACTLY what mounted the competing overlay (`App.tsx:97` `shouldShowOverlay` →
  `MithrilPartialSyncStore.ts:98-99` `hasDisplayStatus` → `isMithrilPartialSyncOverlayStatus`). No consumer
  requires the removed emission. Disposition stands.
- Anchor drift recorded correctly (JSON/PRD `:69-90`/`:69-105` → live `:77-87`/`:89-98`; gate `:194-205` → live
  `:195-205`). PRD D5(a) text (`prd:265-272`) matches the plan's reading. Lint baseline confirmed: exactly 2
  pre-existing warnings on `mithrilPartialSyncNodeStartup.ts:21-22` (`reason`/`nodeState`), spec 0 — change
  touches neither line.
- Prototyped the deletion + all four tests: with the comment in place, `tsc --noEmit` exit 0, eslint unchanged
  (2/0), and Tests 1–3 (single-surface, Wipe, Quit) PASS as written. The core change is sound.

ONE PLAN DEFECT (minor; not a blocker — a smaller model can still ship from the doc):
- Test 4 (optional, generation-changed) as the plan's PRIMARY fixture
  `getGeneration.mockReturnValueOnce(0).mockReturnValue(1)` FAILS: `handleInterruptedRecovery` calls
  `this._getGeneration()` exactly once (the `:100` re-check; the marker read does NOT read generation), so
  `mockReturnValueOnce(0)` is consumed by that single call → re-check sees `0 !== 0` false → wipes → returns
  false, so `expect(result).toBe(true)` fails. The plan's OWN documented fallback
  (`getGeneration.mockReturnValue(1)`) works — I verified all 13 tests pass with it. Recommend the implementer
  use `getGeneration.mockReturnValue(1)` for Test 4 (or drop Test 4, which is flagged optional and not an
  acceptance criterion). The load-bearing Tests 1–3 are unaffected.
- Cosmetic: plan cites `source/renderer/app/containers/App.tsx:97`; the live file is `source/renderer/app/App.tsx:97`
  (line and behavior correct, path off by `containers/`). Non-load-bearing for this backend-only task.

These do not change the scope, the disposition, or any acceptance criterion. The plan is correct, minimal, and
small-model-implementable. Approving with the Test-4-fixture note (the plan already documents the working
fallback inline).

Decision: approved

---
