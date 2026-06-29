# task-ux-701 — Implementation Review Log (append-only)

> Append-only. Each entry: ISO-8601 UTC `Timestamp:`, a speaker label
> (Implementation | Code-Review), and a `Decision: approved|requires_changes` line.
> (Planner phase complete — see the `## Planner` entry in `task-ux-701.md`. No implementation entries yet.)
>
> NOTE (interaction mode `interactive_validation` / `manual_execution`): the autonomous-eligible subset is
> the static seam re-trace + the existing automated backend specs. The live-chain / multi-network /
> packaged-build re-validation (OP-A/B/C across mainnet/preprod/preview) is OPERATOR-OWNED and is NOT run
> in this workflow. Do NOT mark the task `completed`, add `completedAt`, or commit from this stage — that
> happens LATER, only after recorded operator evidence.

---

## Implementation

Timestamp: 2026-06-29T09:35:49Z
Speaker: Implementation

Executed the autonomous-eligible subset of task-ux-701 (static seam re-trace + existing automated backend
specs). The live-chain / multi-network / packaged-build re-validation (OP-A/B/C) stays operator-owned and was
NOT run here.

Part 1 — static seam re-trace against current source (re-confirmed all three landed fixes hold):
- Concern A (cleanup restart-safe): `MithrilPartialSyncService.ts:579-582` `_cleanupPartialSyncArtifacts` =
  `fs.remove(_getStagingRootPath())` + `clearMithrilPartialSyncMarker()`; call sites cancel `:297`,
  restart-normal `:335`, wipe `:346` (+ marker via `finalizeWipeAndFullSync` `:354`); `try/finally` →
  `_clearRuntimeWorkState` `:597`. Live managed chain untouched pre-cutover (boundary #1). **PASS.**
- Concern B (latest-snapshot drift): `:60` code const; re-resolve `:191-192`, `!== partialSyncRange.end`
  guard `:193-196`, `throw _createStageError('preparing', …, PARTIAL_SYNC_LATEST_DRIFT_CODE)` `:197-201`.
  Surfaces RETRIABLE (renderer `partialSyncErrorCopy.ts:51`, message `:373-379`; `retry` allowed for
  `preparing` via `:548-561`), consistent with task-ux-403. **PASS.**
- Concern C (immutable-merge, no missing `00000.primary`): `chainStorageManagerLayout.ts:588-595` validate
  allowlist; `:597-599` empty managed EXCLUDING `immutable`; `:601-625` per-file MOVE-merge of staged
  immutable into the existing `immutable/` (ensureDir `:608`); cutover journal/rename `:289-318`. Existing
  `00000.primary` is excluded from the empty step → survives. **PASS.**

Part 2 — automated checks:
- `yarn test:jest` (scoped backend subset, 8 specs): **8 suites / 172 tests passed, 0 failures.** Per-concern
  asserting specs cited in `task-ux-701-research.md` §3 (cancel-cleanup `:853`; drift `:505`; immutable-merge
  `:989`).
- `yarn compile` (tsc --noEmit): **PASS** (exit 0; scss `.d.ts` regen ran in-pipeline).
- `eslint` on the seam files: **PASS** (0 errors; 8 pre-existing warnings, none in the cleanup/drift/merge
  logic). Top-level `yarn lint`/`npx` hit an unrelated `devEngines.node` npm error → ran local eslint binary.

No concrete defect proven → **no source/ changes made** (per the targeted-fix-only boundary). Did NOT set the
tasks JSON to `completed`, did NOT add `completedAt`, did NOT commit — gated on operator OP-A/B/C evidence.

Decision: approved (autonomous subset only; operator manual_execution still required before sprint DoD)

---

## Scribe

Timestamp: 2026-06-29T10:02:00Z
Speaker: Scribe

Operator manual_execution evidence recorded by the user in `task-ux-701-research.md` §4: OP-A, OP-B, and
OP-C all **Operator Passed**. Coverage was **preprod** this round, which the user deemed sufficient for this
round of testing (mainnet/preview + packaged-build coverage remain part of the task-ux-702 full QA matrix,
not a blocker for closing task-ux-701). All three referenced backend-correctness concerns (#16, #17, #18/#19)
are now re-validated WITH RECORDED EVIDENCE (static re-trace + green automated specs + operator pass),
satisfying the task acceptance (tasks.json:774-776). No defect was proven → no source/ changes.

Closed out: tasks JSON `status` → `completed`, `completedAt: 2026-06-29`. One docs commit created for the
phase-7 doc set + JSON sync. This unblocks `task-ux-702` (the full manual QA matrix + rollout-readiness gate),
which is itself `manual_execution` / `interactive_decision` and operator/user-owned.

Decision: approved
