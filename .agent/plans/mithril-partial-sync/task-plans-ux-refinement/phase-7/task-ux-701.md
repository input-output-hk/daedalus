# task-ux-701 — Backend-correctness de-risking re-validation (referenced gate)

> Phase 7 / PRD D7 (backend-correctness half). **RE-VALIDATION** task, not a re-architecture.
> Status: pending → planned → **completed (2026-06-29)**. Planner sign-off: see the `## Planner` entry at the
> foot of this doc; Critiquer approved in `-plan-review.md`; re-validation + operator evidence + Scribe sign-off
> in `-impl-review.md` and `-research.md` §4.

## Task id + title
- **Id:** task-ux-701
- **Title:** Backend-correctness de-risking re-validation (referenced gate)

## Why now
The UX safety posture shipped in phases 2–6 leans on three backend guarantees that this PRD *references*
but does not *re-specify*. Each fix already landed (gap #16 cleanup, gap #17 drift code, gaps #18/#19
immutable-merge in task-401). Before the phase-7 operator rollout decision (task-ux-702) we must
**re-validate, not assume** that those landed fixes still hold against the live code seams, and record
the evidence in the research brain. If — and only if — re-validation proves a concrete defect, land a
**targeted** fix (no re-architecture). This is the backend-correctness half of PRD D7.

## Interaction mode (AUTHORITATIVE)
`interactive_validation` / `manual_execution`. Per prompt:124-128 and prompt:169-171, phase-7 tasks are
**operator-owned and must never be relabeled autonomous**. The live-chain / multi-network /
packaged-build re-validation requires operator-run validation against real chain state and is
`manual_execution`. This workflow performs ONLY the **autonomous-eligible subset**: doc scaffolding,
static seam re-trace against live source, and running the existing automated backend specs. It MUST NOT
set the task JSON to `completed`, add `completedAt`, or create a commit — those happen LATER, only after
operator evidence (out of scope here).

## Scope
- Re-trace the three backend-correctness seams (A/B/C below) against live source and confirm the landed
  fixes are present and structurally correct.
- Run the existing automated backend specs that cover these seams (see Verification plan) and record
  results as evidence.
- Author the operator-run `manual_execution` checklist (live-chain, multi-network, packaged-build) for
  task-ux-702 to execute, with explicit pass/fail anchors.
- Record all findings + evidence in `task-ux-701-research.md`, cross-linked to research/02, /18, /19.
- IF a concrete defect is proven by re-validation: land a **targeted** fix (tasks.json:766,775).

## Non-goals (locked)
- **No re-architecture.** Targeted fixes ONLY where re-validation proves a defect (tasks.json:766,775).
- No source/ edits at the planning stage (this doc). No tasks-JSON status change. No commit.
- No new IPC channel, no threshold logic, no renderer-computed behind-ness.
- No snapshot-selection UI / storage-location picker (locked boundary #5).
- Do not re-derive behind-ness epochs FIGURE logic (owned by task-ux-304).

## Dependencies
`task-ux-201`, `task-ux-202`, `task-ux-203`, `task-ux-204` (all complete). The drift UX this re-validates
against was shipped by task-ux-403 (error-copy map); the immutable-merge fix landed in the original
task-401 sprint.

## Research / docs / workflows / skills consulted
- PRD / prompt: `prompt-ux-refinement.md:67-73` (canonical doc contents), `:93-114` (locked boundaries),
  `:124-128` + `:169-171` (phase-7 operator ownership), `:179-183` (DoD).
- Tasks JSON entry: `mithril-partial-sync-ux-refinement-tasks.json:748-779`.
- Research brain: `research/02-validation-spike-results.md`, `research/18-task-401-manual-qa-results.md`,
  `research/19-ux-refinement-state-and-gaps.md` (gap rows #16, #17, #18, #19 at lines 143-146).
- Workflows: `.agent/workflows/electron.md` (main-process), `.agent/workflows/test.md` (Jest specs),
  `.agent/system/architecture.md`.

## Files expected to change
- **This stage (docs only):** the four phase-7 docs in this directory + `research/` note.
- **Targeted-fix path (ONLY if a defect is proven):** one or more of
  `source/main/mithril/MithrilPartialSyncService.ts`, `source/main/utils/chainStorageManager.ts`,
  `source/main/utils/chainStorageManagerLayout.ts` (the JSON `targetPaths`), plus the matching
  `*.spec.ts`. Default expectation: **no source change** (re-validation confirms the landed fixes).

---

## Locked safety boundaries (inline — honor while reasoning; never silently break)
From prompt:93-114 and the task grounding:
1. **Staged-only restore; no in-place mutation of the live chain.** Download/verify happen entirely in
   the staging dir; the live managed chain is touched only at the locked cutover.
2. **Recovery is backend-authoritative.** Render recovery strictly from `allowedRecoveryActions`; never
   infer from status names. (Concern A asserts the post-cancel `allowedRecoveryActions` set is correct.)
3. **Latest snapshot only; no snapshot picker, no storage-location picker.** (Concern B is the
   latest-snapshot drift guard.)
4. **Cancellation forbidden after cutover.** Cancel/fail must be reachable ONLY pre-cutover; after
   cutover the only recovery is wipe-and-full-sync.
5. **Supported networks:** mainnet, preprod, preview (operator re-validation must cover all three).
6. **Kill switch `mithrilPartialSyncEnabled`** hides all partial-sync UI when off.
7. **Do not regress the empty-chain Mithril bootstrap flow.** Cutover/merge changes (Concern C) share
   `chainStorageManager*`; the bootstrap success path must remain green.
- **Vocab guardrail:** user-facing term is "Mithril" / "Mithril Sync", never "partial sync" (internal
  only); behind-ness shown to users is epochs-only; "immutable files" is an internal unit only.

---

## The three backend-correctness concerns (re-validate; verify the landed fix, do not assume)

### Concern A — cancellation/failure cleanup is restart-safe (gap #16)
**Seam (live source, re-confirmed):**
- `source/main/mithril/MithrilPartialSyncService.ts:579-582` — `_cleanupPartialSyncArtifacts()`:
  `await fs.remove(this._getStagingRootPath()); await clearMithrilPartialSyncMarker();`
- Cleanup call sites (re-confirmed): **cancel** `:296-297`; **restart-normal** `:334-335`;
  **wipe-and-full-sync** `:345-346` (direct `fs.remove(this._getStagingRootPath())` — marker is cleared
  separately by `finalizeWipeAndFullSync()` at `:353-354`); `_resetToIdleStatus()` definition `:584-595`.
- Post-cancel `allowedRecoveryActions` set at `:301-305` = `['retry','restart-normal','wipe-and-full-sync']`
  (boundary #2 — must match the documented pre-cutover boundary).

**Re-validate:** a cancel/fail mid-download removes the staging dir AND clears the marker, while leaving
the live managed chain untouched (staged-only, locked boundary #1). D5d's restart-normal escape then
boots cleanly. The `try/finally` at `:286-328` guarantees `_clearRuntimeWorkState()` (`:597`) runs even
when cleanup throws.

**Research evidence anchors:** `research/02-validation-spike-results.md:242-246,258`; gap row
`research/19-ux-refinement-state-and-gaps.md:143`.

### Concern B — latest-snapshot drift backend handling (gap #17)
**Seam (live source, re-confirmed + corrected):**
- `source/main/mithril/MithrilPartialSyncService.ts:60` —
  `const PARTIAL_SYNC_LATEST_DRIFT_CODE = 'PARTIAL_SYNC_LATEST_DRIFT';`
- **Throw site — CORRECTED:** the drift detection + throw is at `:191-202` (re-resolve latest snapshot at
  `:191-192`; `latestCertifiedImmutableNumber !== partialSyncRange.end` guard at `:193-196`; throw
  `_createStageError('preparing', '…changed during partial sync preparation. Please retry…',
  PARTIAL_SYNC_LATEST_DRIFT_CODE)` at `:197-201`). The gap row cites `:169-180`; the actual throw has
  drifted to **`:191-202`** — use the corrected anchor.

**Re-validate:** backend drift behavior emits `PARTIAL_SYNC_LATEST_DRIFT` and the message instructs a
retry, matching the **RETRIABLE** UX shipped in task-ux-403's error-copy map (latestDrift → retry,
chain data unchanged). Confirm the throw fires *before* any live-chain mutation (it is in the
`preparing` stage, staged-only — boundary #1 holds).

**Research evidence anchors:** `research/02-validation-spike-results.md:206-209,326`; gap row
`research/19-ux-refinement-state-and-gaps.md:144`.

### Concern C — cutover / immutable-merge correctness, no missing `00000.primary` (gaps #18/#19)
**Seam (live source, re-confirmed + corrected):**
- `source/main/utils/chainStorageManagerLayout.ts:588-629` — staged-install merge:
  - validates the staged top-level entries match `expectedTopLevelEntries` `:588-595` (throws if not);
  - empties managed contents **EXCLUDING `immutable`** via
    `ctx._emptyManagedContents(resolvedManagedChainPath, { excludeTopLevelEntries: ['immutable'] })`
    `:597-599`;
  - for each expected entry: when `immutable`, ensures the existing target `immutable/` and **per-file
    MOVES** staged immutable entries into it (preserving prior immutable history) `:601-618`; otherwise
    moves the entry wholesale `:619-624`; final staged-root cleanup `:627-629`.
- Cutover rename/journal: `source/main/utils/chainStorageManagerLayout.ts:289-420` (journalled
  `_movePath` migration at `:288-296`; rollback path at `:405-421`).
- `source/main/utils/chainStorageManager.ts:457-459` — `_movePath` → `movePath(this, …)`.
- **CORRECTION:** the grounding labeled `chainStorageManager.ts:302-330` as
  "`_emptyManagedContents` w/ `preserveSourceRoot`". That range is actually
  **`migrateData(fromPath, toPath, { preserveSourceRoot })`** (`:298-332`). The real
  `_emptyManagedContents` wrapper is at **`:442-447`** (delegates to `emptyManagedChainContents`) and
  takes `excludeTopLevelEntries`, not `preserveSourceRoot`. Use the corrected anchors:
  `_emptyManagedContents` = `chainStorageManager.ts:442-447`; `migrateData`/`preserveSourceRoot` =
  `chainStorageManager.ts:298-332`.

**Re-validate:** cutover **preserves/merges** the existing `immutable/` directory (does not wipe it) and
`00000.primary` is present post-cutover — i.e. the merge does not drop the first immutable chunk
(QA amendment to lock #10; gaps #18/#19).

**Research evidence anchors:** `research/02-validation-spike-results.md:261-264`;
`research/18-task-401-manual-qa-results.md:135` (six task-401 regressions at 18:69,85,101,118,135,159);
gap rows `research/19-ux-refinement-state-and-gaps.md:145-146`.

---

## Implementation approach — ordered re-validation checklist (small-model-implementable)

### Part 1 — Static seam re-trace (autonomous-eligible; this workflow)
1. **A.1** Open `MithrilPartialSyncService.ts` and confirm `_cleanupPartialSyncArtifacts()` (`:579-582`)
   does `fs.remove(stagingRoot)` THEN `clearMithrilPartialSyncMarker()`. Record the exact two lines.
2. **A.2** Confirm the three cleanup call sites: cancel `:297`, restart-normal `:335`, wipe `:346`
   (direct `fs.remove`). Confirm `try/finally` guarantees `_clearRuntimeWorkState()` (`:327`/`:597`).
3. **A.3** Confirm the post-cancel `allowedRecoveryActions` (`:301-305`) equals the documented
   pre-cutover boundary set and is NOT inferred from a status name (boundary #2).
4. **B.1** Confirm the drift constant (`:60`) and the corrected throw site (`:191-202`); confirm it is in
   the `preparing` stage (pre-cutover, staged-only) and the message says "retry".
5. **B.2** Cross-check the `PARTIAL_SYNC_LATEST_DRIFT` → retriable mapping in the task-ux-403 error-copy
   map (locate the latestDrift key) and confirm parity (RETRIABLE, chain data unchanged).
6. **C.1** Open `chainStorageManagerLayout.ts:588-629`; confirm validation (`:588-595`), empty-excluding-
   immutable (`:597-599`), per-file immutable MOVE-merge (`:601-618`), non-immutable wholesale move
   (`:619-624`), staged-root cleanup (`:627-629`).
7. **C.2** Confirm `_emptyManagedContents` (`chainStorageManager.ts:442-447`) honors
   `excludeTopLevelEntries`, and `_movePath` (`:457-459`) is a single move primitive. Confirm the journal
   cutover (`layout:289-420`) is restart-safe (journal written before/after each move, rollback present).
8. **C.3** Reason about `00000.primary`: since managed `immutable/` is EXCLUDED from the empty step and
   staged immutable entries are per-file merged INTO it, the pre-existing `00000.primary` survives and any
   staged chunk is added — i.e. no chunk is dropped. Record this as the structural argument.

### Part 2 — Automated specs (autonomous-eligible; this workflow)
9. Run the existing backend specs that cover these seams (see Verification plan). Capture pass/fail +
   counts as evidence in `task-ux-701-research.md`. These cover the cleanup, drift, and merge logic at the
   unit level; they substitute for live-chain coverage of the *code paths* (not the *live behavior*).

### Part 3 — Operator-owned `manual_execution` items (NOT runnable here; author for task-ux-702)
These require real chain state / packaged build / multiple networks and are operator-owned (prompt:169-171):
10. **OP-A (Concern A, live):** Start Mithril Sync mid-download on a real node, cancel; assert the staging
    dir is gone, the marker is cleared, the live managed `immutable/` is byte-identical to pre-run, and
    restart-normal boots the node cleanly. Repeat for an induced mid-download *failure*.
11. **OP-B (Concern B, live):** Force a latest-snapshot drift during preparation (or simulate via a
    staged snapshot bump); assert the UI shows the RETRIABLE latestDrift error, retry succeeds with the
    refreshed range, and chain data was not mutated.
12. **OP-C (Concern C, live):** Complete a real cutover on a node that already has immutable history;
    assert the merged `immutable/` retains prior chunks INCLUDING `00000.primary`, the node starts and
    resumes standard syncing, and the empty-chain Mithril **bootstrap** success path still works
    (boundary #7). Run OP-A/B/C on **mainnet, preprod, preview**.
13. Record operator evidence (logs, dir listings, screenshots) back into `task-ux-701-research.md`; only
    THEN may the Scribe sync the tasks JSON and commit (out of scope for this workflow).

### Targeted-fix gate
14. IF and only IF any of steps 1-12 proves a **concrete defect**: land the **smallest targeted fix** in
    the relevant `source/main/...` file + a regression spec; re-run Part 2. No re-architecture
    (tasks.json:766,775). Default expectation: no source change.

## Acceptance criteria (verbatim from tasks.json:774-776)
1. The three referenced backend-correctness concerns are re-validated with recorded evidence.
2. Any defect found is fixed with targeted changes (no re-architecture).
3. Findings are recorded in the research brain.

(Sprint DoD additionally requires: the four phase-7 docs + both review logs + the research note. JSON
sync + commit happen LATER, only after operator evidence — out of scope here.)

## Verification plan (build/verify commands — CLAUDE.md)
1. `yarn compile` — `tsc --noEmit` (backend is plain TS; no scss/.d.ts dependency for these files).
2. `yarn lint`.
3. `yarn test:jest` — full Jest, or scope to the seam specs for a fast loop:
   - `source/main/mithril/MithrilPartialSyncService.spec.ts` (Concern A cleanup + Concern B drift),
   - `source/main/mithril/mithrilPartialSyncMarker.spec.ts`,
   - `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`,
   - `source/main/utils/chainStorageManager.spec.ts`,
   - `source/main/utils/chainStorageManagerShared.spec.ts`,
   - `source/main/utils/chainStorageValidation.spec.ts`,
   - `source/main/utils/chainStorageCoordinator.spec.ts`,
   - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`.
4. `yarn check:all` — full gate (best-effort).
> ENV NOTE: the Node-v24 typed-scss-modules / identity-obj-proxy sidecar is **renderer/scss-only** and
> IRRELEVANT to this backend re-validation; mention it only if a renderer spec is actually touched.

## Risks / open questions
- **Coverage gap:** unit specs exercise the *code paths*, not real chain state. The live behavior of
  Concerns A/B/C can only be fully closed by the operator (Part 3). This workflow must not claim the task
  `completed` on unit evidence alone.
- **Anchor drift:** the gap-row anchors predate later edits; this plan already corrected the Concern B
  throw site (→`:191-202`) and the Concern C `_emptyManagedContents` location (→`:442-447`). Re-confirm
  before any fix.
- **Bootstrap regression risk (boundary #7):** any Concern-C targeted fix touches shared
  `chainStorageManager*`; re-run the bootstrap install specs.

## Required doc / research updates
- `task-ux-701-research.md` — record the static re-trace findings, the Part-2 spec results, and (later)
  the operator evidence; cross-link research/02:242-246,258 / 206-209,326 / 261-264, research/18:135, and
  research/19:143-146.
- Tasks JSON sync + `completedAt` + commit: LATER, only after operator evidence (out of scope here).

## Review-log paths
- Plan review: `task-ux-701-plan-review.md` (Critiquer appends next).
- Implementation review: `task-ux-701-impl-review.md` (re-validation/Implementation appends next).

## Planning status / build status
- Planning status: `approved` (Planner approved below; Critiquer approved in `-plan-review.md`).
- Build status: `completed` (2026-06-29) — static re-trace + automated specs (autonomous subset) PASS, and
  operator `manual_execution` OP-A/B/C all Operator Passed on preprod (accepted as sufficient for this round;
  see `-research.md` §4). No defect proven → no source/ changes. JSON synced to `completed`; docs commit made.

---

## Planner

Timestamp: 2026-06-29T14:30:00Z
Speaker: Planner (backend re-validation architect)

Summary:
Produced the canonical re-validation plan for task-ux-701, the backend-correctness half of PRD D7. Held
the AUTHORITATIVE interaction mode `interactive_validation` / `manual_execution` — phase-7 is
operator-owned and must never be relabeled autonomous (prompt:124-128, :169-171). Scoped this workflow to
the autonomous-eligible subset (doc scaffolding, static seam re-trace, existing automated specs) and
fenced off the live-chain / multi-network / packaged-build re-validation as operator-owned (OP-A/B/C),
plus the JSON-sync + commit, which happen LATER only after operator evidence.

Re-opened every source seam in the grounding and corrected two drifted anchors:
- **Concern B throw site:** the gap row cites `MithrilPartialSyncService.ts:169-180`; the actual
  `PARTIAL_SYNC_LATEST_DRIFT` throw has drifted to **`:191-202`** (constant still at `:60`). Plan uses the
  corrected anchor.
- **Concern C `_emptyManagedContents`:** the grounding labeled `chainStorageManager.ts:302-330` as
  `_emptyManagedContents w/ preserveSourceRoot`; that range is actually `migrateData(...)`. The real
  `_emptyManagedContents` wrapper is **`:442-447`** (takes `excludeTopLevelEntries`); `_movePath` is
  **`:457-459`**. Plan uses the corrected anchors.
Concerns A's anchors (cleanup `:579-582`; call sites cancel `:297`, restart-normal `:335`, wipe `:346`;
`_resetToIdleStatus` `:584`) and Concern C's layout merge (`:588-629`, journal `:289-420`) were confirmed
accurate as written.

Wrote the ordered re-validation checklist (Part 1 static re-trace, Part 2 automated specs, Part 3
operator-owned manual_execution OP-A/B/C across mainnet/preprod/preview), carried the locked invariants
inline (staged-only #1, allowedRecoveryActions-authoritative #2, latest-snapshot-only #3,
no-cancel-after-cutover #4, bootstrap-no-regress #7), and pinned the acceptance criteria verbatim from
tasks.json:774-776. Targeted fixes are gated on a proven defect only (no re-architecture, tasks.json:766).

Wrote: task-ux-701.md (this canonical doc), task-ux-701-plan-review.md (awaiting Critiquer),
task-ux-701-impl-review.md (awaiting Implementation), task-ux-701-research.md (evidence stub cross-linked
to research/02, /18, /19).

Decision: approved
