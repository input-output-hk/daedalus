# task-ux-701 — Research: Backend-correctness de-risking re-validation (referenced gate)

> Phase 7 / PRD D7 (backend-correctness half). **RE-VALIDATION** note — verify the landed fixes, do not
> assume. Grounding date: 2026-06-29. Line anchors confirmed by re-opening every source file (see §2).
> This is a STUB the re-validation stage fills with recorded evidence (static re-trace + automated specs +
> operator evidence). The Definition of Done requires the three concerns re-validated WITH RECORDED
> EVIDENCE (tasks.json:774-776).

## 1. Scope grounding
PRD D7 *references but does not re-specify* three backend-correctness concerns the UX safety posture
depends on. Each fix already landed; this task **re-validates** them and records evidence. Targeted fixes
are in-scope ONLY where re-validation proves a defect — no re-architecture (tasks.json:766,775).

Interaction mode (AUTHORITATIVE): `interactive_validation` / `manual_execution`. The live-chain /
multi-network / packaged-build re-validation is operator-owned (prompt:124-128, :169-171) and is NOT run
in this workflow. The autonomous-eligible subset is the static seam re-trace + existing automated specs.

Locked boundaries honored: staged-only restore / no in-place mutation (#1); recovery rendered strictly
from `allowedRecoveryActions` (#2); latest-snapshot only, no picker (#3); cancellation forbidden after
cutover (#4); networks mainnet/preprod/preview (#5); kill switch hides UI when off (#6); do not regress
empty-chain Mithril bootstrap (#7). Vocab: user-facing "Mithril" / "Mithril Sync", behind-ness epochs-only,
"immutable files" internal-only.

## 2. Current-state seam findings (line-anchored; re-confirmed 2026-06-29)

### Concern A — cancellation/failure cleanup is restart-safe (gap #16)
- `MithrilPartialSyncService.ts:579-582` `_cleanupPartialSyncArtifacts()` =
  `fs.remove(this._getStagingRootPath())` then `clearMithrilPartialSyncMarker()`. **Confirmed.**
- Call sites: cancel `:296-297` (inside `try`, then sets `status:'cancelled'`,
  `allowedRecoveryActions:['retry','restart-normal','wipe-and-full-sync']` `:301-305`); restart-normal
  `:334-335`; wipe-and-full-sync `:345-346` (direct `fs.remove`; marker cleared by
  `finalizeWipeAndFullSync()` `:353-354`). `try/finally` guarantees `_clearRuntimeWorkState()` (`:327`,
  defn `:597`). `_resetToIdleStatus()` defn `:584-595`. **All confirmed accurate vs grounding.**
- Structural argument: staging dir + marker removed; live managed chain never mutated pre-cutover
  (boundary #1) → restart-normal escape (D5d) boots clean.
- **Evidence anchors:** research/02:242-246,258 ; gap row research/19:143.
- **RE-VALIDATED 2026-06-29:** spec evidence recorded in §3 (`MithrilPartialSyncService.spec.ts:853,886,918,
  947,1096,1126,1140` — green). PASS — landed behavior holds. Operator OP-A (live cancel + induced failure,
  staging-gone + immutable-unchanged + clean boot) remains pending in §4.

### Concern B — latest-snapshot drift backend handling (gap #17)
- `MithrilPartialSyncService.ts:60` `PARTIAL_SYNC_LATEST_DRIFT_CODE = 'PARTIAL_SYNC_LATEST_DRIFT'`.
  **Confirmed.**
- **ANCHOR CORRECTION:** gap row research/19:144 cites `MithrilPartialSyncService.ts:169-180`; the actual
  drift detect+throw is at **`:191-202`** (re-resolve latest `:191-192`; `!== partialSyncRange.end` guard
  `:193-196`; `throw _createStageError('preparing', '…changed during partial sync preparation. Please
  retry with the refreshed range.', PARTIAL_SYNC_LATEST_DRIFT_CODE)` `:197-201`). The throw is in the
  `preparing` stage → pre-cutover, staged-only (boundary #1).
- Parity target: task-ux-403 error-copy map maps latestDrift → RETRIABLE (retry, chain data unchanged).
- **Evidence anchors:** research/02:206-209,326 ; gap row research/19:144.
- **RE-VALIDATED 2026-06-29:** `MithrilPartialSyncService.spec.ts:505-522` asserts the throw with
  `code:'PARTIAL_SYNC_LATEST_DRIFT'` at the `preparing` stage (pre-download, staged-only). task-ux-403
  latestDrift key confirmed RETRIABLE (`partialSyncErrorCopy.ts:51` → `MithrilBootstrap.messages.ts:373-379`
  "Retry Mithril Sync … your chain data was not changed"; `_deriveAllowedRecoveryActions` yields `retry` for
  `preparing`). PASS. Operator OP-B (forced drift → retriable UI → retry OK) remains pending in §4.

### Concern C — cutover / immutable-merge correctness, no missing `00000.primary` (gaps #18/#19)
- `chainStorageManagerLayout.ts:588-629` staged-install merge: validate expected entries `:588-595`;
  `_emptyManagedContents(..., { excludeTopLevelEntries: ['immutable'] })` `:597-599`; per-file MOVE-merge
  of staged immutable into existing target immutable `:601-618`; wholesale move of non-immutable entries
  `:619-624`; staged-root cleanup `:627-629`. **Confirmed accurate.**
- Cutover rename/journal: `chainStorageManagerLayout.ts:289-420` (journalled `_movePath` `:288-296`;
  rollback `:405-421`). **Confirmed.**
- `chainStorageManager.ts:457-459` `_movePath` → `movePath`. **Confirmed.**
- **ANCHOR CORRECTION:** grounding labeled `chainStorageManager.ts:302-330` as `_emptyManagedContents w/
  preserveSourceRoot`. That range is actually `migrateData(fromPath, toPath, { preserveSourceRoot })`
  (`:298-332`). The real `_emptyManagedContents` wrapper is **`:442-447`** (delegates to
  `emptyManagedChainContents`, takes `excludeTopLevelEntries` — NOT `preserveSourceRoot`).
- Structural argument for no missing `00000.primary`: managed `immutable/` is EXCLUDED from the empty step
  and staged immutable chunks are per-file merged INTO the existing dir → the pre-existing `00000.primary`
  survives and staged chunks are added; no chunk is dropped (QA amendment to lock #10).
- **Evidence anchors:** research/02:261-264 ; research/18:135 (six task-401 regressions at
  18:69,85,101,118,135,159) ; gap rows research/19:145-146.
- **RE-VALIDATED 2026-06-29:** `chainStorageManager.spec.ts:922,989` (merge preserves existing immutable
  history; `excludeTopLevelEntries:['immutable']` + `ensureDir` target + per-file MOVE of staged chunks) —
  green within the 8-suite / 172-test run. PASS — pre-existing `00000.primary` survives cutover. Operator
  OP-C (live cutover with prior immutable history → merged immutable retains `00000.primary`, node resumes,
  empty-chain bootstrap still green) remains pending in §4.

## 3. Automated-spec inventory (Part-2 evidence — RECORDED 2026-06-29T09:35Z)

Ran the scoped backend subset:
`yarn test:jest --runTestsByPath MithrilPartialSyncService.spec.ts chainStorageManager.spec.ts
chainStorageManagerShared.spec.ts chainStorageValidation.spec.ts chainStorageCoordinator.spec.ts
mithrilPartialSyncMarker.spec.ts mithrilPartialSyncNodeStartup.spec.ts mithrilPartialSyncChannel.spec.ts`
→ **Test Suites: 8 passed / 8 total; Tests: 172 passed / 172 total; 0 failures** (2.5 s).

Per-concern spec evidence within that green run:
- **Concern A (cleanup restart-safe):** `MithrilPartialSyncService.spec.ts:853` "cleans staging artifacts and
  clears the marker when cancellation succeeds before cutover" (asserts `fs.remove(stagingRoot)` +
  `clearMithrilPartialSyncMarker`, then `status:'cancelled'` with `allowedRecoveryActions:
  ['retry','restart-normal','wipe-and-full-sync']`); `:886` boundary-A failure path; `:918` restart-normal
  reset-to-idle; `:947` wipe retains marker until `finalizeWipeAndFullSync`; finalize idempotency
  `:1096/:1126/:1140`. The live managed chain is never touched pre-cutover (no in-place mutation, boundary #1).
- **Concern B (latest-snapshot drift):** `MithrilPartialSyncService.spec.ts:505` "rejects latest snapshot
  drift before command execution" asserts a throw with `code:'PARTIAL_SYNC_LATEST_DRIFT'` (`:522`) at the
  `preparing` stage — i.e. BEFORE any download command runs (staged-only). Renderer parity confirmed:
  `partialSyncErrorCopy.ts:19-22,51` maps the code → `LATEST_DRIFT` copy, and the hint
  (`MithrilBootstrap.messages.ts:373-379`) is explicitly RETRIABLE: "Retry Mithril Sync to use the refreshed
  snapshot — your chain data was not changed." Retriability is also structurally guaranteed:
  `_deriveAllowedRecoveryActions` returns `['retry',…]` for the `preparing` stage (`:548-561`, retry only
  withheld for installing/finalizing/starting-node). Consistent with task-ux-403's error-copy map.
- **Concern C (immutable-merge, no missing `00000.primary`):** `chainStorageManager.spec.ts:922` installs only
  the validated allowlist and `:989` "installValidatedPartialSyncSnapshot preserves existing immutable history
  while merging staged immutable entries" — asserts `_emptyManagedContents(..., excludeTopLevelEntries:
  ['immutable'])` (`:1032`), `ensureDir` of the existing target `immutable/` (`:1035`), and per-file MOVE of
  staged `*.chunk/*.primary/*.secondary` INTO the existing dir (`:1038-1047`). The pre-existing immutable
  history (incl. `00000.primary`) is excluded from the empty step and therefore survives cutover.

`yarn compile` (tsc --noEmit): **PASS** (exit 0; typed-scss-modules `.scss.d.ts` regen ran automatically in
the pipeline, so the Node-v24 env note did not block the backend type-check).
`eslint` on the three seam files + `partialSyncErrorCopy.ts`: **PASS** (0 errors; 8 pre-existing warnings —
unused-var/no-shadow/prefer-destructuring — none in the cleanup/drift/merge logic). Note: top-level
`yarn lint`/`npx eslint` tripped an unrelated `npm error Invalid property "devEngines.node"`; ran the local
`./node_modules/.bin/eslint` binary directly instead.

**Re-validation verdict (autonomous subset): all three concerns PASS — landed behavior confirmed, no defect.**

## 4. Operator-owned manual_execution evidence (Part-3 — filled by the operator, NOT this workflow)
- **OP-A (Concern A):** **Operator Passed** — live cancel + induced failure; staging-gone, marker-cleared,
  immutable-unchanged, restart-normal clean boot. Networks: preprod
- **OP-B (Concern B):** **Operator Passed** — forced latest-snapshot drift; retriable latestDrift UI, retry
  with refreshed range succeeds, chain data unmutated.
- **OP-C (Concern C):** **Operator Passed** — live cutover with prior immutable history; merged immutable
  retains `00000.primary`, node resumes standard syncing, empty-chain bootstrap success path still green.
- _Only after this evidence is recorded may the Scribe sync the tasks JSON (status `completed` +
  `completedAt`) and create the commit — out of scope for the planning/autonomous workflow._
- **CLOSED 2026-06-29:** all three operator passes recorded above (preprod coverage, accepted as sufficient
  for this round). task-ux-701 marked `completed` / `completedAt: 2026-06-29`; docs commit created. Remaining
  multi-network (mainnet/preview) + packaged-build sweeps roll into the task-ux-702 full QA matrix, not a
  task-ux-701 blocker.

## 5. Cross-links
- `research/02-validation-spike-results.md:206-209,242-246,258,261-264,326`
- `research/18-task-401-manual-qa-results.md:69,85,101,118,135,159`
- `research/19-ux-refinement-state-and-gaps.md:143-146` (gap rows #16/#17/#18/#19)
- Tasks JSON: `mithril-partial-sync-ux-refinement-tasks.json:748-779`
- Canonical plan: `task-plans-ux-refinement/phase-7/task-ux-701.md`
