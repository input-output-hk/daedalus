# task-ux-202 — Plan review transcript (append-only)

Append entries at the END only. Never rewrite history. Each entry: speaker label, ISO-8601 UTC
`Timestamp:`, and an outcome. Critiquer entries END with `Decision: approved` or
`Decision: requires_changes`.

---

Planner:
Timestamp: 2026-06-22T15:48:45Z

Produced the canonical plan for task-ux-202 (Success-path finalization / Boundary C2), planning status
`in_review`, build status `in_progress`. Implements PRD D9 (`...-ux-refinement-prd.md:402-485`), fixes
BUG1, BUG2, gap #41 (staging-leftover), and gap #44 (dead `node-start-verified`).

Names chosen:
- Channel: `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL` with `MithrilPartialSyncFinalizeRendererRequest = void`
  / `MithrilPartialSyncFinalizeMainResponse = void` (mirrors the cancel channel's `void → void` shape).
- Main + renderer wrappers: `mithrilPartialSyncFinalizeChannel` (added on BOTH sides, per the task-ux-101
  availability precedent — reconciles the JSON targetPaths omission of the renderer file vs task-ux-404's
  inclusion; strictly additive, unused-yet export).
- Controller method: `finalizePartialSync()` routed **direct-to-service** (NOT via chainStorageCoordinator)
  because finalize does no node orchestration and runs only from a terminal state.
- Service method: `finalizeCompletedPartialSync()` — idempotent `_resetToIdleStatus()` +
  `fs.remove(stagingRoot)` + `clearMithrilPartialSyncMarker()` + `_clearRuntimeWorkState()`.

Key RESOLVED decisions:
- (c) Cross-session staging path: extend the marker with `stagingRootPath?: string`; persist the RESOLVED
  colocated root at the two cutover writes (while `_stagingChainDir` is set) and carry it into
  `node-start-verified`. The C2 reclaim `fs.remove(marker.stagingRootPath)` (fresh process, no service
  instance, `_stagingChainDir` null — re-resolution would hit the wrong volume). The dismiss finalize uses
  `marker.stagingRootPath ?? _getStagingRootPath()`.
- (d) `node-start-verified` (Option A): in `finalizeInstalledNodeStart`, replace the marker clear at
  `mithrilPartialSyncNodeStartup.ts:165` with `writeMithrilPartialSyncMarker('node-start-verified', …)`,
  keep the `completed` emission, defer the clear to dismiss. Activates the dead `:60-63` C2 branch.
- (e) Extend the `:60-63` C2 branch with `fs.remove(marker.stagingRootPath)` (guarded) before clearing;
  add `import fs from 'fs-extra'` (not currently imported in node-startup).
- (f) Finalize is idempotent (safe from idle / double-dismiss) — each step is a no-op-safe operation.

In-session vs cross-session — VERIFIED in-session for the happy path: post-cutover node restart runs via
`restartStartupFlow` → `handleCheckDiskSpace(false)` (`index.ts:250-252`) with NO `app.relaunch`, hitting
the same `MithrilController`/`MithrilPartialSyncService` singletons (`MithrilStartupGate.ts:503`). The
C2-reclaim branch (`MithrilStartupGate.ts:196`) IS cross-process — hence the marker-persisted path is
necessary there and defense-in-depth for dismiss.

Verified anchors / drift recorded:
- PRD's `MithrilPartialSyncService.ts:205/:209` cutover marker writes are DRIFTED → live `:226/:230`.
- `finalizeInstalledNodeStart` clear at `:165` matches live.
- `_clearRuntimeWorkState()` (`:576-582`) does NOT clear `_stagingChainDir` (sticky from task-ux-201) — so
  the in-session dismiss finalize still resolves the colocated path.
- Mock channels are index-by-construction-order (`mithrilPartialSyncChannel.spec.ts`): availability is
  `mockChannels[5]`; finalize appends as `mockChannels[6]`, so existing index assertions are unaffected.
- `idle` is NOT an overlay status (`mithril-partial-sync.types.ts:79-96`), confirming why resetting to idle
  at verified success would drop the overlay (lock #16 / D9).

Test list: new `mithrilPartialSyncMarker.spec.ts` (stagingRootPath round-trip), new
`mithrilPartialSyncNodeStartup.spec.ts` (verified-stamp-not-clear + completed emit; C2 reclaim+normal-boot;
C2-not-C1), extended `MithrilPartialSyncService.spec.ts` (finalize reset/remove/clear-once + idempotent +
cutover-write persistence), extended `mithrilPartialSyncChannel.spec.ts` (finalize handler at index 6
delegates to `controller.finalizePartialSync`). Each tasks-JSON testCase mapped 1:1 in the plan.

Conflicts vs JSON/PRD: (1) PRD line anchors drifted (recorded). (2) Renderer file omitted from task-ux-202
targetPaths but present in task-ux-404 — resolved by adding the renderer wrapper here (both-sides
precedent), recorded as a hand-off. (3) No other conflicts; D9 fully specifies the sequence.

Smallest-truthful-change confirmed: no coordinator change, no MithrilStartupGate change, no renderer
store/overlay logic (task-ux-404/301), no cutover/merge re-architecture (task-ux-701).

---

Critiquer:
Timestamp: 2026-06-22T15:52:49Z

One broad adversarial pass over the canonical plan, independently re-verifying every load-bearing anchor
and design claim against LIVE code (not trusting the plan). The plan is unusually accurate — the bulk of
its line anchors and its in-session/cross-session reasoning hold up under refutation. The locked safety
invariants are all honored (verified below). Findings are minor and confined to test-setup precision.

VERIFIED LOAD-BEARING ANCHORS (all correct against live code):
- Marker `mithrilPartialSyncMarker.ts`: state union `:6-9`, type `:11-15`, `writeMithrilPartialSyncMarker`
  `:22-39` (options `{ managedChainPath? }`, spread pattern), `readMithrilPartialSyncMarker` `:41-76`. No
  `stagingRootPath` present today → additive change confirmed; only two write callers exist
  (`MithrilPartialSyncService.ts:226,:230`), so the optional-field type change breaks nothing.
- Cutover writes live at `MithrilPartialSyncService.ts:226` (`'cutover-in-progress'`) / `:230`
  (`'installed-awaiting-node-start'`) — both pass only `managedChainPath` today (PRD's `:205/:209`
  correctly recorded as DRIFTED).
- `mithrilPartialSyncNodeStartup.ts`: C2 branch `:60-63` is live-and-dead-as-claimed; `finalizeInstalledNodeStart`
  clears at `:165`; module has NO `fs` import (grep confirms) → `import fs from 'fs-extra'` needed as stated.
  Marker read at `:148` is `installed-awaiting-node-start` (asserted `:150`) so it WILL carry the new
  `stagingRootPath` forward into the `node-start-verified` write — the carry-forward is sound.
- Service: cancel guard `:270-274` (lock #6) present + untouched; `_resetToIdleStatus` `:563-574`;
  `_clearRuntimeWorkState` `:576-582` does NOT clear `_stagingChainDir` (sticky from task-ux-201 confirmed);
  `_getStagingRootPath` `:588-598`; `_cleanupPartialSyncArtifacts` `:558-561`; `finalizeWipeAndFullSync`
  `:345-349`; marker import `:26-29` lacks `readMithrilPartialSyncMarker` (must be added, as planned);
  `fs`/`path` already imported `:1-2`.
- Controller: `_partialSyncService` `:84`; cancel/restart/wipe `:404-426` (all via coordinator);
  `_getPartialSyncDependencies` `:428-446`; `isPartialSyncActive` `:194-199`. CONFIRMED the controller
  subscribes to `_partialSyncService.onStatus` (`:111-118`) → `broadcastPartialSyncStatus` sets
  `_partialSyncStatus` (`:256`), so finalize's `_resetToIdleStatus()` genuinely re-arms the BUG1 reader
  cross-object. Direct-to-service routing (Decision (b)) is justified: finalize does no node orchestration.
- IPC: api.ts AVAILABILITY block `:483-487` (append point); cancel `void→void` `:468-471`; main channel
  imports `:3-24`, cancel const `:38-41`, availability const `:53-56`, handler bag `:101-118`; renderer
  imports `:2-23`, consts `:25-53`. Five existing channels confirmed; finalize as the 6th is consistent.
- Types: `completed` valid `:3-15`; overlay statuses `:79-96` EXCLUDE `idle` → resetting to idle at verified
  success WOULD drop the overlay (lock #16 / D9 honored by deferring reset to dismiss). Confirmed.
- StartupGate: `handleInterruptedRecovery` `:196` (fresh-boot, cross-process), `startInstalledNode` `:482`,
  `finalizeInstalledNodeStart` `:503` (in-session after `cardanoNode.start()`). index.ts `:250-252`
  `restartStartupFlow → handleCheckDiskSpace(false)`; ZERO `relaunch` in index.ts or chainStorageCoordinator
  → the in-session-happy-path / cross-process-C2 split is VERIFIED. The marker-persisted `stagingRootPath`
  is genuinely NECESSARY for C2 (no service instance / `_stagingChainDir` null on fresh boot) and
  defense-in-depth for dismiss. Decision (c) holds.
- Test infra: `mithrilPartialSyncChannel.spec.ts` builds `mockChannels` by construction order via the
  `MainIpcChannel` mock push (`:35-44`); availability = `mockChannels[5]` (asserted `:182,:196,:198`);
  appending finalize LAST → `mockChannels[6]`, existing index assertions unaffected. The controller mock
  (`:19-33`) lacks `finalizePartialSync` → must be added, as planned. `mithrilPartialSyncNodeStartup.spec.ts`
  and `mithrilPartialSyncMarker.spec.ts` do NOT exist yet (both NEW, as stated).

LOCKED-INVARIANT SAFETY: all honored.
- #1 staged-only: finalize + C2 reclaim only `fs.remove` the scratch staging root (never the live chain;
  the installed chain already passed a proven node start). #6 post-cutover cancel rejection untouched.
  #9/#16 as amended by D9: keeps the `completed` emission, does NOT reset to idle at verified success,
  defers reset+remove+clear to the dismiss-driven finalize → overlay stays visible until dismiss. #11
  bootstrap path untouched. Renderer halves correctly deferred to task-ux-404/301.
- D9 COVERAGE: ordered sequence (1) RUNNING → (2) stamp `node-start-verified` → (3) emit `completed` →
  (4) dismiss-finalize `_resetToIdleStatus + fs.remove(stagingRoot) + clearMarker`; node-start-verified
  activation (Option A); C2 close-without-dismiss reclaim — ALL present. All 5 tasks-JSON acceptance
  criteria and all 5 testCases are mapped 1:1.

BLOCKERS / FINDINGS:

[MINOR] source/main/mithril/MithrilPartialSyncService.spec.ts:6-9 — the existing
`jest.mock('./mithrilPartialSyncMarker', () => ({ clearMithrilPartialSyncMarker, writeMithrilPartialSyncMarker }))`
factory does NOT export `readMithrilPartialSyncMarker`. The new `finalizeCompletedPartialSync()` calls
`readMithrilPartialSyncMarker()`, so without adding it to this factory the finalize tests throw
`readMithrilPartialSyncMarker is not a function` at runtime. The plan's test plan (testCase 2/3) says to
"mock `readMithrilPartialSyncMarker`" but never explicitly instructs editing the factory object at `:6-9`
— a small model is likely to write `require(...).readMithrilPartialSyncMarker.mockResolvedValue(...)` and
hit a TypeError. Suggested fix: add an explicit step "add `readMithrilPartialSyncMarker: jest.fn()` to the
`./mithrilPartialSyncMarker` mock factory at `MithrilPartialSyncService.spec.ts:6-9`, and capture it as
`readMithrilPartialSyncMarkerMock` (mirroring `writeMithrilPartialSyncMarkerMock` at `:77-79`)."

[MINOR] task-ux-202.md:514-520 (testCase 3a) — "marker absent (`readMithrilPartialSyncMarker` → `null`)":
correct, but note `readMithrilPartialSyncMarker` returns `null` ONLY when the file does not exist
(`mithrilPartialSyncMarker.ts:46-47`); on a corrupt/unparseable marker it returns the non-null
`{ state:'cutover-in-progress', updatedAt:'' }` fallback (`:72-75`). The double-dismiss-after-clear case IS
file-absent (the first finalize cleared it) so `null` is right; the plan's Risks note (`:546-550`) already
captures the corrupt-fallback distinction. No code bug — the `marker?.stagingRootPath ?? _getStagingRootPath()`
expression handles both shapes. Suggested fix: none required; optionally add one assertion that a corrupt
(non-null) marker with no `stagingRootPath` still falls back to `_getStagingRootPath()` for completeness.

[MINOR] task-ux-202.md:343-350 / Step 8 imports — the plan converts the service marker import to a
3-line named-import block adding `readMithrilPartialSyncMarker`, and adds `writeMithrilPartialSyncMarker`
to the node-startup marker import. Both are correct, but the implementer must keep the imports alphabetized
the way the codebase does NOT enforce — verify no `import/order` lint regression on the two touched import
blocks (the plan's eslint gate at `:491` covers this; just ensure the baseline-warning comparison is run).
Suggested fix: keep the explicit "compare warning count to pre-change baseline" note already in the plan; no
change needed beyond honoring it.

Smallest-truthful-change: confirmed. No unnecessary abstraction. Direct-to-service finalize (no coordinator,
no handlers-bag entry) is the minimal wiring; the marker `stagingRootPath` field is the minimal durable
carrier for cross-process C2. The renderer-wrapper-added-here decision (Decision (a)) is justified by the
task-ux-101 both-sides precedent and is strictly additive (unused-yet export) — acceptable convergence, not
over-engineering. No hidden manual checkpoints (verification is `tsc --noEmit` + jest + scoped eslint).

The two MINOR test-setup items are mechanical and do not change the design; they are implementation-time
notes the implementer (and impl critique) should catch. They do not rise to requires_changes because the
plan's verification gate (`jest ...MithrilPartialSyncService.spec.ts`) will fail loudly and immediately if
the `readMithrilPartialSyncMarker` mock is missing, surfacing the fix in-loop. Recommend the Planner fold
the explicit `readMithrilPartialSyncMarker: jest.fn()` factory-edit step into the test plan before impl, but
this is not blocking.

Decision: approved
