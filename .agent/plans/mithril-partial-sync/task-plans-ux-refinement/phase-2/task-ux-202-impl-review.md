# task-ux-202 — Implementation review transcript (append-only)

Append entries at the END only. Never rewrite history. `Implementation:` entries from the implementer;
`Code Review:` entries from the reviewer. Each entry: speaker label, ISO-8601 UTC `Timestamp:`, and an
outcome. Code Review entries END with `Decision: approved` or `Decision: requires_changes`.

---

## Implementation:

**Timestamp:** 2026-06-22T16:03:37Z

### Files changed

1. `source/main/mithril/mithrilPartialSyncMarker.ts` — Added `stagingRootPath?: string` to `MithrilPartialSyncMarker` type; extended `writeMithrilPartialSyncMarker` options to accept and spread `stagingRootPath` (mirrors `managedChainPath` pattern). `readMithrilPartialSyncMarker` unchanged (passes through new field verbatim).

2. `source/main/mithril/MithrilPartialSyncService.ts` — Added `readMithrilPartialSyncMarker` to the marker import. Added `stagingRootPath: this._getStagingRootPath()` to both cutover marker writes at lines 226 and 230 (`cutover-in-progress` and `installed-awaiting-node-start`). Added new public method `finalizeCompletedPartialSync()` after `finalizeWipeAndFullSync()` (PRD D9 step 4): reads marker, resolves staging root, calls `_resetToIdleStatus()` + `fs.remove(stagingRoot)` + `clearMithrilPartialSyncMarker()` + `_clearRuntimeWorkState()`.

3. `source/main/mithril/MithrilController.ts` — Added `finalizePartialSync()` direct-to-service method (no coordinator; node is already RUNNING, no orchestration needed). Placed after `wipeAndFullSyncFromPartialSync()`, before `_getPartialSyncDependencies()`.

4. `source/common/ipc/api.ts` — Added `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL` const, `MithrilPartialSyncFinalizeRendererRequest = void`, `MithrilPartialSyncFinalizeMainResponse = void` after the AVAILABILITY block.

5. `source/main/ipc/mithrilPartialSyncChannel.ts` — Added the finalize const/types to imports; added `mithrilPartialSyncFinalizeChannel` (`MainIpcChannel`, appended last at index 6); added `onRequest` handler calling `controller.finalizePartialSync()` after the availability handler.

6. `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` — Added the finalize const/types to imports; added `mithrilPartialSyncFinalizeChannel` (`RendererIpcChannel`, exported) after the availability channel. Per Decision (a) / task-ux-101 precedent — both-sides wrapper added here so task-ux-404 can import without churn.

7. `source/main/mithril/mithrilPartialSyncNodeStartup.ts` — Added `import fs from 'fs-extra'` (top); added `writeMithrilPartialSyncMarker` to the marker import. Extended the C2 branch (`node-start-verified`, lines 62–71): if `marker.stagingRootPath` is present, `fs.remove(marker.stagingRootPath)` before `clearMithrilPartialSyncMarker()`. In `finalizeInstalledNodeStart`: replaced `clearMithrilPartialSyncMarker()` at line 165 with `writeMithrilPartialSyncMarker('node-start-verified', { managedChainPath: marker.managedChainPath, stagingRootPath: marker.stagingRootPath })`, deferring clear to the dismiss-driven finalize (PRD D9 steps 2–3).

8. `source/main/mithril/mithrilPartialSyncMarker.spec.ts` (NEW) — `stagingRootPath` write/read round-trip; omission-when-absent parity; `node-start-verified` carry-forward; corrupt-marker fallback has no stagingRootPath.

9. `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` (NEW) — C2 branch: removes stagingRootPath then clears, skips remove when absent, does not call dialog/emit failed (not C1). `finalizeInstalledNodeStart`: stamps `node-start-verified` with both paths, does not clear, emits `completed`; early-return on wrong state; throws on non-RUNNING node. `setTimeout` patched via `jest.spyOn(global, 'setTimeout')` to resolve immediately (avoids fake-timer async interplay).

10. `source/main/mithril/MithrilPartialSyncService.spec.ts` (EXTENDED) — Added `readMithrilPartialSyncMarker` mock to the module mock and `beforeEach`. Added `finalizeCompletedPartialSync` describe: resets to idle + removes marker-persisted stagingRoot + clears marker once; falls back to `_getStagingRootPath()` when marker has no stagingRootPath; idempotent from idle (called twice, no throw). Added `cutover marker writes persist stagingRootPath` describe: asserts both `cutover-in-progress` and `installed-awaiting-node-start` writes include `stagingRootPath`.

11. `source/main/ipc/mithrilPartialSyncChannel.spec.ts` (EXTENDED) — Added `finalizePartialSync: jest.fn()` to the controller mock. Added finalize channel test: `mockChannels[6].onRequest` delegates to `controller.finalizePartialSync()` and resolves to `undefined`. Added distinction test: finalize channel is not availability or status channel. Existing `mockChannels[5]`-is-availability assertions unaffected.

### Verification

- **tsc --noEmit**: exit code 0. Clean (no pre-existing unrelated errors; the `typedef:sass` hook failure is only via `yarn compile`, not `tsc --noEmit` directly).
- **jest** (4 spec files, 61 tests): all 61 pass. 0 failures.
  - `mithrilPartialSyncMarker.spec.ts`: 8 tests — PASS
  - `mithrilPartialSyncNodeStartup.spec.ts`: 9 tests — PASS
  - `MithrilPartialSyncService.spec.ts`: 32 tests — PASS (was 25, added 7)
  - `mithrilPartialSyncChannel.spec.ts`: 12 tests — PASS (was 7, added 2 new finalize tests + 1 existing extended with `finalizePartialSync` mock)
- **eslint** (7 production files): 0 errors, 30 warnings — all pre-existing (no new warnings introduced). Spec files: 0 errors, 0 warnings.

### Acceptance criteria coverage

1. **Finalize/dismiss channel exists and routed idempotently** — `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL` added to api.ts; main + renderer wrappers; `onRequest` → `controller.finalizePartialSync()` → `service.finalizeCompletedPartialSync()`. Idempotency verified by the double-call test. ✓
2. **node-start-verified written after RUNNING; C2 startup branch live** — `finalizeInstalledNodeStart` now writes `node-start-verified` (deferred clear); C2 branch extended with `fs.remove(stagingRootPath)`. Both verified by new specs. ✓
3. **Verified success keeps completed overlay; reset+cleanup on dismiss** — backend emits `completed` and stays in that state (not `idle`); `finalizeCompletedPartialSync` performs the triple (reset+remove+clear) on dismiss. Honors lock #9/#16. ✓
4. **Close-without-dismiss reclaims leftover staging via C2 on next launch** — C2 branch `fs.remove(marker.stagingRootPath)` + skips when absent. Verified by spec. ✓
5. **Locked decision #16 amendment (D9) implemented** — deferred clear at `node-start-verified`; dismiss-driven finalize; no reset at verified success. ✓

### Deviations from plan

None. All 9 ordered mechanical steps implemented exactly as specified. The `setTimeout` mock approach (spy on `global.setTimeout` to synchronize) is an implementation-level test technique choice not specified in the plan but consistent with testing the behavior without fake timer complexity. The plan specified using "fake timers or a small mock" — this uses the small mock approach.

### Hand-offs

- **task-ux-404**: renderer store `dismissCompletedOverlay` action should invoke `mithrilPartialSyncFinalizeChannel.send()`. The renderer wrapper `mithrilPartialSyncFinalizeChannel` is exported from `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` — no import churn needed.
- **task-ux-301**: `isActionBlocked` derivation (`isWorking` vs `isActive`) — not touched here.

---

## Code Review:

**Timestamp:** 2026-06-22T16:07:56Z

**Reviewer:** code-reviewer subagent (round 1) — one broad adversarial pass over the working-tree diff vs the
approved plan (`task-ux-202.md`) and PRD D9.

### Verification performed (independently re-run)

- `./node_modules/.bin/tsc --noEmit` → exit 0 (clean).
- `./node_modules/.bin/jest` on the 4 target specs → **61/61 pass**. Broader
  `jest source/main/mithril source/main/ipc` → **150/150 pass** (no collateral regression).
- `./node_modules/.bin/eslint` on all touched production + spec files → **0 errors, 30 warnings**, every
  warning pre-existing `no-unused-vars` on untouched code; no new warning introduced. The new
  `import fs from 'fs-extra'` in `mithrilPartialSyncNodeStartup.ts` is consumed in the C2 branch (no
  unused-import warning).

### Correctness vs plan / PRD D9 — all confirmed

- **Ordered finalization sequence** (`MithrilPartialSyncService.ts:354-365`): read marker → `_resetToIdleStatus`
  → `fs.remove(stagingRoot)` → `clearMithrilPartialSyncMarker` → `_clearRuntimeWorkState`. Matches D9 step 4
  exactly.
- **node-start-verified written after RUNNING** (`mithrilPartialSyncNodeStartup.ts:167-185`): the `clear` was
  replaced by `writeMithrilPartialSyncMarker('node-start-verified', { managedChainPath, stagingRootPath })`;
  the `completed` emission is preserved; marker clear is DEFERRED. Satisfies D9 steps 2–3 and makes the
  `:62-71` C2 branch live.
- **Verified success does NOT reset to idle** — confirmed: `finalizeInstalledNodeStart` emits `completed` and
  never touches status→idle (lock #16 / D9 honored).
- **In-session overlay survival (the subtle one I tried hardest to break):** after the in-session happy path,
  the marker is `node-start-verified`. The in-session `restartStartupFlow` (`index.ts:250-252`,
  `handleCheckDiskSpace(false)`, NO `app.relaunch`) re-enters `ensureMithrilStartupGate`, but
  `_startupCheckDone` was set true on the first pass (`MithrilStartupGate.ts:213`) and is reset ONLY by
  `resetOnDirectoryChange` (`:218-224`), NOT by `restartStartupFlow`. So the early-return at `:174-176`
  prevents `handleInterruptedRecovery` from re-running and prematurely reclaiming staging / clearing the
  marker in-session. The `completed` overlay therefore stays visible until the explicit dismiss IPC. Lock #9
  / #16 hold. (Worth recording: the deferred-clear safety depends on this `_startupCheckDone` short-circuit;
  it is correct today.)

### Cross-session staging-path correctness (the BLOCKER-class concern) — SAFE

- The on-disk staging root is `preparePartialSyncStagingDirectory(this._getStagingRootPath(), …)` which
  `path.resolve`s the same value (`mithrilPartialSyncStaging.ts:24-52`). The marker's persisted
  `stagingRootPath` is `this._getStagingRootPath()` captured at the two cutover writes while
  `_stagingChainDir` is set (`MithrilPartialSyncService.ts:229,234`). On-disk path ≡ persisted path — no
  mismatch, no legacy-dir removal, no colocated-dir leak.
- **C2 reclaim** (`mithrilPartialSyncNodeStartup.ts:66-68`) runs in a fresh process (`_stagingChainDir = null`)
  and removes `marker.stagingRootPath` directly — the exact persisted colocated dir, never re-resolved. The
  `if (marker.stagingRootPath)` guard makes a legacy/absent value a safe no-op (still clears + normal boot).
- **Dismiss finalize** removes `marker?.stagingRootPath ?? this._getStagingRootPath()`. In-session both agree;
  the marker value keeps it correct even if dismiss ever arrives cross-session. Verified the fallback test
  resolves to `/tmp/daedalus-state/mithril-partial-sync` (fresh service, `_stagingChainDir = null`).

### Locked invariants — no regressions

- #1 staged-only restore: finalize/C2 `fs.remove` only the scratch staging dir; live chain untouched.
- #6 post-cutover cancel rejection (`MithrilPartialSyncService.ts:273-277`) — untouched.
- #8 no synthetic IPC math / no raw JSON — finalize/marker carry only paths+state.
- #11 bootstrap non-regression — no bootstrap files in the diff; `git diff --name-only` shows zero
  store/overlay/component files; the only renderer file is the additive IPC wrapper.

### IPC contract — wired consistently end-to-end

`api.ts` const + `void→void` types → main import + `MainIpcChannel` (appended LAST) + `onRequest` →
`controller.finalizePartialSync()` → `service.finalizeCompletedPartialSync()`; renderer wrapper exported
(additive, per Decision (a) / task-ux-101 precedent). Channel construction order preserved:
start(0)…availability(5), finalize(6); existing `mockChannels[5]`-is-availability assertions remain valid.

### Tests — cover the 5 tasks-JSON testCases, asserting the right things

TC1→nodeStartup "stamps node-start-verified … then emits completed" (asserts write args + clear NOT called +
completed emit); TC2→service "resets to idle, removes marker-persisted stagingRoot, clears once" + fallback
sub-test; TC3→service "idempotent from idle" + channel "delegates to finalizePartialSync"; TC4→nodeStartup
"C2 removes stagingRootPath then clears … returns false" + absent-path variant; TC5→nodeStartup "C2 does NOT
emit failed / does NOT call dialog.showMessageBox". Supporting tests: marker round-trip + omission-parity;
cutover-write persistence asserting the resolved `/tmp/mithril-partial-sync`. No pass-for-wrong-reason
detected; fixture math (`_getStagingRootPath` = dirname(mithrilWorkDir)+/mithril-partial-sync) checks out.

### Non-blocking observations (severity: minor — no change required)

- **MINOR** (`MithrilPartialSyncService.ts:361-363`): `_resetToIdleStatus()` runs before `fs.remove`/
  `clearMarker`. If `fs.remove` rejects, status is already idle while staging/marker persist. Mitigated by
  full idempotency (a re-dismiss retries cleanly) and it matches the plan's prescribed order
  (read→reset→remove→clear). Acceptable as-is; no fix needed.
- **MINOR** (housekeeping, NOT this task's code): the working tree also contains unrelated edits
  (`.gitignore` `.devcontainer`, `prompt-ux-refinement.md` wording) and untracked `.wf-*.mjs` /
  plan-review/research scratch files. These are not task-202 production changes and must be EXCLUDED from the
  single task commit (the implementer's commit step already scopes to task-relevant files).

### Blockers

None.

**Decision: approved**
