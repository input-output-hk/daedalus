# task-ux-702 — Full manual QA matrix and rollout-readiness decision (deployment gate)

> Phase 7 / PRD D7 (QA-gate half) + the PRD **Rollout / Kill Switch** section. This is the **deployment
> gate**, not a feature task. Status: pending → planned → **stays non-completed by this workflow**
> (the gate is held OPEN pending the operator's separately-supplied manual assessment). Planner sign-off:
> see the `## Planner` entry at the foot of this doc; Critiquer gates it in `-plan-review.md`; the
> operator-run matrix + recovery evidence + ship decision land in `-impl-review.md` / `-research.md` later.

> **CLOSEOUT SCOPE AMENDMENT (2026-06-29):** Per operator direction, the live QA matrix for this final
> closeout is scoped to **preprod / Linux only**. mainnet + preview and Windows + macOS are intentionally
> out of scope for this gate run and stay kill-switch-gated (advisory remains `keep guarded by default`,
> see the decision framework). The matrix/dimension/cell-count references below were updated to this
> scope; the broader axes are retained only as the not-yet-met `enabled by default` criteria. The original
> full-matrix `## Planner` sign-off at the foot is preserved verbatim as historical record.

> **GATE CLOSED (2026-07-02):** The operator's manual assessment is complete. The preprod/Linux
> closeout matrix, the six individual task-401 regression re-tests, and the guard-off /
> startup-owned-recovery / bootstrap re-checks are all PASS (`-research.md` §2-§3); the kill-switch
> decision is locked to `keep guarded by default` (§4); the rollback checklist is confirmed (§5); and
> the ship decision is **APPROVED TO MERGE** (§6). task-ux-702 is `completed` (completedAt 2026-07-02)
> in tasks.json. PR creation follows this closeout.

## Task id + title
- **Id:** task-ux-702
- **Title:** Full manual QA matrix and rollout-readiness decision (deployment gate)

## Why now
All three dependencies are complete as of 2026-06-29: automated coverage is green (task-ux-503,
tasks.json:665-666), the user-facing Mithril Sync vocabulary + epochs-only behind-ness shipped
(task-ux-601, :709-710), and the backend-correctness re-validation gate closed (task-ux-701, :752-753).
With the autonomous + automated half proven, the remaining barrier to merge/ship is the **human-driven
deployment gate**: the full manual QA matrix across real networks, custom storage, recovery paths, and
packaged builds, plus the six task-401 regressions re-tested individually (gap #18). This is the QA-gate
half of PRD D7 (`prd.md:344-373`) and the PRD **Rollout / Kill Switch** section (`prd.md:925-931`). This
branch **does not merge to master / ship** until this matrix passes (`prd.md:369-370,922-923`).

## Interaction mode (AUTHORITATIVE)
`manual_execution` / **operator-owned**. Per tasks.json:794-795 ("requires human-driven validation on
supported networks, custom storage, packaged builds, and recovery paths; do not relabel autonomous") and
the phase-7 rule (operator-owned, never relabeled autonomous), the live multi-network / custom-storage /
packaged-build matrix and the six task-401 regression re-tests are **OPERATOR-OWNED and are NOT executed
in this workflow.**

This workflow performs ONLY the **autonomous-eligible subset**: authoring this canonical plan and
**scaffolding the deliverables** —
1. the QA-results matrix template (every cell empty/`PENDING`, ready for operator entry),
2. the kill-switch production-default + network-scoping **decision framework** (criteria + an *advisory*
   recommendation — NOT a final locked decision, because the PRD explicitly **defers** the production
   default to the readiness gate, `prd.md:929,939`),
3. the rollback **checklist** (structure + items),
4. the research-note skeleton with operator sections fenced **PENDING OPERATOR EXECUTION**.

**This workflow MUST NOT** set the tasks JSON to `completed`, add `completedAt`, or claim the gate
passed. The deployment gate stays **OPEN** until the operator's manual assessment is brought in
separately. The Document stage keeps the JSON status non-completed.

> NOTE: a separate file `task-ux-702-manual-assessment.md` exists in this directory; it is supplementary
> operator material the user will bring in later. This plan does **not** open, read, or rely on it — its
> existence is noted only.

## Scope
- Author the canonical plan (this doc) to the small-model-implementable bar.
- Scaffold the **QA-results matrix** enumerating every row the operator must fill (closeout scope:
  **preprod** × (default + custom chain storage) × {success, cancel, retry, restart-normal,
  wipe-and-full-sync} × **Linux** packaged build), plus startup-owned unsafe-recovery where safely
  exercisable.
- Scaffold the **six task-401 regression** re-test rows individually (gap #18), each anchored to
  research/18 and tagged with its landed root-cause fix file.
- Author the kill-switch **production-default + network-scoping decision framework** (advisory).
- Author the **rollback checklist** for release review.
- Author the research-note skeleton with operator sections fenced **PENDING OPERATOR EXECUTION**.
- Record acceptance criteria verbatim (JSON :806-811) and the verification plan.

## Non-goals (locked)
- **No live QA execution in this workflow.** The multi-network / custom-storage / packaged-build matrix
  and the six 401 regression re-tests are operator-owned and not run here.
- **Do NOT mark task-ux-702 completed; do NOT set `completedAt`.** No ship/merge decision is finalized
  by this workflow; the gate stays open.
- **Do NOT lock the production default** — the PRD defers it (`prd.md:929,939`); this plan supplies an
  *advisory* recommendation pending operator/user sign-off only.
- No source/ edits (this is a QA + decision gate, not a feature change). The only source path that may
  move is a defect found *during operator QA*, which is recorded and handled as a follow-up, not here.
- No re-architecture; no new IPC channel; no renderer-computed behind-ness; no snapshot/storage-location
  picker (locked boundary below).
- **Never open `task-ux-702-manual-assessment.md`.**

## Dependencies
`task-ux-503` (automated coverage green; :665-666), `task-ux-601` (Mithril Sync vocabulary + epochs-only;
:709-710), `task-ux-701` (backend-correctness re-validation closed; :752-753) — ALL complete 2026-06-29.
702 is the next unblocked task.

## Research / docs / workflows / skills consulted
- Tasks JSON entry: `mithril-partial-sync-ux-refinement-tasks.json:782-812`.
- PRD: D7 QA-gate half `prd.md:344-373` (esp. :369-370 full-matrix gate); the deployment-gate testing
  bullet `prd.md:920-923`; **Rollout / Kill Switch** `prd.md:925-931`; Open Questions `prd.md:933-939`
  (production default / network-scoping deferred); coverage map rows `prd.md:945-962` (gaps #3, #16-#19).
- Research brain: `research/18-task-401-manual-qa-results.md` (the six regressions at :69,85,101,118,135,159;
  Decision Rules :17-25; Required Scenario Coverage :27-37; Run Template :39-54; Rollback checklist
  confirmation :165-169; Final Recommendation :171-178); `research/19-ux-refinement-state-and-gaps.md`
  (gap rows #16-#19 at :143-146, gap #3 QA-matrix-deferred); `research/02-validation-spike-results.md`.
- Precedent: task-ux-701 docs in this directory (mirrored section shape + operator-owned split).
- Workflows: `.agent/workflows/test.md`, `.agent/workflows/build.md`, `.agent/system/architecture.md`.

## Files expected to change
- **This stage (docs only):** the four phase-7 docs for 702 in this directory —
  `task-ux-702.md` (this canonical doc), `task-ux-702-plan-review.md` (Planner appends; Critiquer next),
  `task-ux-702-impl-review.md` (operator/Scribe append later), `task-ux-702-research.md` (matrix +
  decision framework + rollback checklist skeleton, operator sections fenced PENDING).
- **PRD:** `prd.md` — record the finalized production-default / network-scoping decision and the rollback
  checklist outcome in the Rollout / Open Questions sections **only after operator sign-off** (not in this
  workflow).
- **Tasks JSON:** `mithril-partial-sync-ux-refinement-tasks.json` — status flip to `completed` +
  `completedAt` happen LATER, only after the operator matrix passes and the ship decision is recorded
  (out of scope here).
- **No `source/` changes** unless operator QA proves a concrete defect (handled as a targeted follow-up).

---

## Locked safety boundaries (inline — honor while reasoning; never silently break)
From the PRD locks, task-ux-701, and the grounding:
1. **Kill switch `mithrilPartialSyncEnabled` hides ALL partial-sync UI when off** — diagnostics entry,
   recommendation prompt, confirmation modal, and overlay all disappear (`prd.md:927`, gap #1 / D3).
2. **Startup-owned native recovery dialog remains** for already-interrupted unsafe-cutover installs even
   when the diagnostics entry is disabled (`prd.md:930-931`).
3. **Staged-only restore; no in-place mutation of the live chain** before the locked cutover.
4. **Cancellation forbidden after cutover.** Cancel/fail reachable ONLY pre-cutover; post-cutover the only
   recovery is wipe-and-full-sync.
5. **Latest snapshot only; no snapshot picker, no user storage-location picker.**
6. **Supported networks: mainnet, preprod, preview** (the feature's full support). This closeout's live
   matrix is scoped to **preprod** per operator direction; mainnet + preview stay kill-switch-gated until covered.
7. **Do not regress the empty-chain Mithril bootstrap flow** (shares `chainStorageManager*`).
8. **Vocab guardrail:** user-facing term is "Mithril Sync" vs "standard sync", **never** "partial sync"
   in user copy; behind-ness shown to users is **epochs-only** (Cardano-native); "immutable files" is an
   internal unit only.

---

## QA matrix dimensions (operator fills pass/fail per cell)

**Axes (closeout scope; from tasks.json:784, prd.md:920-923):**
- Networks: `preprod` (closeout scope). The feature also supports `mainnet` + `preview` (boundary #6),
  but those are out of scope for this gate run and stay kill-switch-gated.
- Chain storage: `default managed` + `custom path`.
- Recovery / scenario paths: `success`, `cancel`, `retry`, `restart-normal`, `wipe-and-full-sync`
  (+ `startup-owned unsafe-recovery` where safely exercisable).
- Packaged builds: `Linux` (closeout scope; release-equivalent packaged build, not `yarn dev`).
  `Windows` + `macOS` are out of scope for this run.

Closeout cross-product = 1 network × 2 storage × 5 scenarios × 1 OS = **10 cells**, plus the
startup-owned unsafe-recovery pass on Linux. Per tasks.json:796 the operator records
**environment, platform, network, chain-storage target, and observed behavior** for every pass, using the
research/18 Run Template (:39-54) shape.

**Guard-off check (boundary #1/#2):** for at least one Linux cell, also verify with
`mithrilPartialSyncEnabled = false` that ALL partial-sync UI is hidden, the backend rejects a
diagnostics-launched start and restart-normal, while wipe-and-full-sync and the startup-owned recovery
dialog still behave as designed (research/18:23,37).

## The six task-401 regressions — re-test individually (gap #18)
Per tasks.json:797 and gap #18 (`research/19:145`), re-test each of the six individually, NOT via a single
combined pass. Each is anchored to research/18 and tagged with the landed root-cause fix to confirm:

| # | research/18 anchor | Regression | Landed fix to confirm |
|---|--------------------|------------|-----------------------|
| R1 | `research/18:69` | cancel → wipe-and-full-sync failed: recovery paths asserted a pre-stopped node | `source/main/utils/chainStorageCoordinator.ts` (+ `.spec.ts`) reuses the automatic partial-sync node-stop handler |
| R2 | `research/18:85` | `cardano-node` restarted mid-partial-sync after a background disk-space poll | `source/main/utils/handleDiskSpace.ts` guards on `isMithrilPartialSyncBlockingNodeStart(...)` (+ `handleDiskSpace.spec.ts`) |
| R3 | `research/18:101` | overlay stuck at `Finalizing`: disk-space guard blocked the marker-driven startup handoff | `handleDiskSpace.ts` runs `startNodeAfterPartialSyncInstall(...)` ahead of the blocking guard (+ `.spec.ts`) |
| R4 | `research/18:118` | `starting-node` never invoked `cardanoNode.start()`: status broadcast awaited renderer ack | `source/main/ipc/mithrilPartialSyncChannel.ts` makes status delivery fire-and-forget (+ `.spec.ts`) |
| R5 | `research/18:135` | node crash `FsResourceDoesNotExist` `00000.primary`: immutable prefix wiped at cutover | `source/main/utils/chainStorageManagerLayout.ts` preserves + merges existing `immutable/` (+ `chainStorageManager.spec.ts`) |
| R6 | `research/18:159` | representative retry / restart-normal / wipe / startup-owned recovery re-validation deferred after the latest fixes | exercise each recovery path individually (not via the single combined pass at `research/18:151`) and confirm clean behavior |

---

## Implementation approach — ordered checklist (small-model-implementable)

### Part 1 — Autonomous-eligible scaffolding (THIS workflow)
1. **Create the canonical plan** = this doc (`task-ux-702.md`) with every section above. (DONE when this
   file exists with the QA axes, the six-regression table, the acceptance criteria verbatim, and the
   operator-owned split clearly fenced.)
2. **Scaffold `task-ux-702-research.md`** with:
   - **§1 Scope grounding** — restate the gate, interaction mode (operator-owned), and the locked
     boundaries 1-8 above.
   - **§2 QA-results matrix template** — a fillable table whose rows enumerate the 10-cell closeout
     cross-product (preprod × Storage × Scenario × Linux), each cell defaulting to `PENDING`, plus the
     Linux guard-off row. Carry the research/18 Run Template fields (env, platform, network, chain-storage, build id,
     guard setting, scenario, expected, actual, pass/fail/blocked, logs/evidence, notes).
   - **§3 Six task-401 regression rows** — the table above, one fillable row per regression, each
     `PENDING` with its research/18 anchor + fix file.
   - **§4 Kill-switch decision framework** — the criteria + advisory recommendation (see below), marked
     **advisory, pending operator/user sign-off**.
   - **§5 Rollback checklist** — the checklist structure below, each item `PENDING` operator confirmation.
   - **§6 Operator-owned execution log** — fenced **PENDING OPERATOR EXECUTION**, empty for operator entry.
   - **§7 Cross-links** — research/02, /18, /19; PRD D7 + Rollout; tasks.json:782-812.
3. **Create `task-ux-702-plan-review.md`** (append-only header note mirroring the 701 plan-review header)
   and append the **Planner** entry (label, real ISO-8601 UTC timestamp via `date -u`, summary,
   `Decision: approved`).
4. **Create `task-ux-702-impl-review.md`** as an append-only transcript stub (header note only) awaiting
   the operator/Implementation + Scribe entries; do not pre-fill outcomes.
5. **Leave the tasks JSON status `pending`/non-completed.** Do NOT set `completedAt`. Do NOT commit a ship
   decision. (The Document stage keeps the gate OPEN.)

### Part 2 — OPERATOR-OWNED manual execution (NOT runnable here; for the operator)
> These require real chain state, custom storage, packaged builds, and multiple networks. They are
> operator-owned (tasks.json:794-795) and are executed OUTSIDE this workflow. The operator fills the
> §2/§3 templates and the §6 log.
6. **OP-MATRIX:** Execute the 10-cell closeout matrix — for {Network = preprod} ×
   {Storage ∈ default, custom} × {Scenario ∈ success, cancel, retry, restart-normal, wipe-and-full-sync}
   × {OS = Linux} on a **release-equivalent packaged build**, record env/platform/network/
   chain-storage/observed-behavior and pass/fail/blocked per cell (research/18:39-54).
7. **OP-GUARDOFF:** On Linux, run the `mithrilPartialSyncEnabled = false` check — all partial-sync UI hidden;
   diagnostics-launched start + restart-normal rejected by the backend; wipe-and-full-sync + startup-owned
   recovery dialog still work (research/18:23,37; boundaries #1/#2).
8. **OP-REGRESSIONS:** Re-test R1-R6 individually (gap #18) using the §3 rows; confirm each landed fix
   holds against live behavior, not just unit specs.
9. **OP-BOOTSTRAP:** Confirm the empty-chain Mithril **bootstrap** success path still works on preprod
   (boundary #7 — shared `chainStorageManager*`).
10. **OP-DECISION:** From the matrix + regression results, finalize (a) the production default + any
    network-scoping of `mithrilPartialSyncEnabled` (Open Question, `prd.md:939`), (b) confirm the rollback
    checklist, and (c) record a clear **merge/ship readiness decision**. Then the Scribe records evidence
    in §6, syncs the PRD Rollout/Open-Questions sections and the tasks JSON (status + `completedAt`), and
    creates the single commit — all OUT OF SCOPE for this planning workflow.

### Defect gate (operator)
11. IF operator QA proves a concrete defect: open a **targeted** follow-up fix (no re-architecture),
    record it, and re-run the affected matrix cells. The gate does not pass until the matrix is green.

---

## Kill-switch production-default + network-scoping decision framework (ADVISORY — pending sign-off)

**Why advisory, not locked:** the PRD explicitly **defers** the production default and network-scoping to
this readiness gate (`prd.md:929,939`); the branch enables the switch only for testing (`prd.md:928`).
This workflow therefore supplies criteria + a recommendation, and leaves the final lock to the operator/
user after the matrix runs (HARD CONSTRAINT 2).

**Decision criteria (from research/18:17-25 Decision Rules):**
- **`enabled by default`** requires ALL of: release-equivalent packaged-build evidence for every release OS
  in scope; success-path coverage for mainnet, preprod, preview across default + custom storage;
  representative recovery-path evidence; and explicit guard-off evidence (UI hidden + backend rejects
  diagnostics-launched start/restart-normal while wipe + startup-owned recovery still work).
- **`keep guarded by default`** = the truthful outcome when QA is otherwise acceptable but packaged-build
  evidence or matrix coverage is incomplete.
- **`hold for follow-up fixes`** = when any blocking safety/correctness issue remains unresolved.

**Network-scoping options:** (i) single global default; (ii) staged — enable on testnets (preprod/preview)
first, keep `mainnet` guarded until mainnet packaged-build + full-matrix evidence exists; (iii) all-or-
nothing held until the complete full matrix passes.

**Advisory recommendation (NOT a lock):** this closeout's matrix is scoped to **preprod / Linux** (see
"QA matrix dimensions"); broad coverage — mainnet + preview and Windows/macOS release-equivalent packaged
builds — therefore remains **outstanding**, so the `enabled by default` criteria are not met by this run.
Accordingly, recommend **`keep guarded by default`** (switch ships OFF, flippable via launcher config
without an app release per `prd.md:929`), with staged network-scoping (option ii) as the path to
`enabled by default` once mainnet packaged-build evidence lands. The kill switch remains the rollout lever
regardless. *Final default + scoping pending operator/user sign-off.*

## Rollback checklist (structure — operator confirms each at release review)
From research/18:165-169 and `prd.md:925-931`:
- [ ] **PENDING** — `mithrilPartialSyncEnabled = false` hides ALL partial-sync UI (diagnostics entry,
      recommendation prompt, confirmation modal, overlay) (boundary #1).
- [ ] **PENDING** — with the guard off, the backend **rejects** a diagnostics-launched partial-sync start
      and `restart-normal`.
- [ ] **PENDING** — the **startup-owned native recovery dialog remains available** for any already-
      interrupted unsafe-cutover install even when the diagnostics entry is disabled (boundary #2).
- [ ] **PENDING** — **wipe-and-full-sync** remains available where designed.
- [ ] **PENDING** — the empty-chain **Mithril bootstrap** path remains unaffected (boundary #7).
- [ ] **PENDING** — the switch is **flippable via launcher config without an app release** (`prd.md:929`).

## Acceptance criteria (verbatim from tasks.json:806-811)
1. Manual QA results are captured in repo-local planning artifacts.
2. The production default / network-scoping decision for the kill switch is documented.
3. A rollback checklist is written for release review.
4. A clear merge/ship readiness decision is recorded.

> NOTE: criteria (1), (2-decision lock), and (4) require **operator-owned** live execution + sign-off and
> are completed OUTSIDE this workflow. This workflow delivers the artifacts/templates/criteria/checklist
> that those criteria are recorded into, and holds the gate OPEN (status non-completed).

## Verification plan (this workflow = docs; operator = live)
**This workflow (docs only):**
1. Confirm the four phase-7 702 docs exist with the required sections (matrix template enumerates all
   axes; six-regression rows present with anchors; acceptance criteria verbatim; operator sections fenced
   PENDING; rollback checklist + decision framework present).
2. No `yarn` build/test is required for doc scaffolding; the automated/backend coverage was already
   proven green by task-ux-503 / task-ux-701 (do not re-claim it as 702 evidence).

**Operator (live, OUT OF SCOPE here):**
3. Release-equivalent packaged build on Linux (`.agent/workflows/build.md`,
   `yarn package`).
4. The 10-cell preprod/Linux matrix + Linux guard-off check + six individual regression re-tests +
   empty-chain bootstrap confirmation, recorded per the research/18 Run Template.

## Risks / open questions
- **Scoped coverage:** this closeout runs the 10-cell preprod/Linux matrix only; mainnet + preview and
  Windows/macOS remain uncovered and kill-switch-gated, which is why the advisory stays `keep guarded by
  default` (research/18 Decision Rules).
- **Production default is an Open Question** (`prd.md:939`): this plan recommends `keep guarded` advisory
  only; the lock is the operator's.
- **Defect risk during live QA:** any concrete defect → targeted follow-up fix (no re-architecture), then
  re-run affected cells.
- **Do not over-claim:** unit/automated evidence from 503/701 covers code paths, not live multi-network /
  packaged behavior — this gate exists precisely to close that.

## Required doc / research updates
- `task-ux-702-research.md` — the matrix template, six-regression rows, decision framework, rollback
  checklist, and the fenced operator log (this workflow); operator evidence + final decision (later).
- `prd.md` Rollout / Open Questions — record the finalized production default / network-scoping + rollback
  outcome **after** operator sign-off (not here).
- Tasks JSON — status `completed` + `completedAt`: LATER, only after the matrix passes and the ship
  decision is recorded (out of scope here).

## Review-log paths
- Plan review: `task-ux-702-plan-review.md` (Planner appended; Critiquer appends next).
- Implementation review: `task-ux-702-impl-review.md` (operator/Implementation + Scribe append later).

## Planning status / build status
- Planning status: `approved` (Planner approved; Critiquer approved in `-plan-review.md`).
- Implementation status: `scaffolding complete` — autonomous-eligible deliverables authored and
  code-reviewed (pass 1 of max 3 — no blockers; Decision: approved in `-impl-review.md`).
- Build status: **deployment gate CLOSED (2026-07-02).** The QA-results matrix (10 cells,
  preprod/Linux scope), the six task-401 regression re-tests (R1-R6), the guard-off /
  startup-owned-recovery / bootstrap re-checks, the locked kill-switch decision
  (`keep guarded by default`), the confirmed rollback checklist (6 items), and the
  **APPROVED TO MERGE** ship decision are recorded with operator sign-off in
  `task-ux-702-research.md` §2-§6.
- Tasks JSON sync: task-ux-702 is `"completed"` with `"completedAt": "2026-07-02"` (ledger version
  1.11.0). The close-out entry is appended to `-impl-review.md`.

---

## Planner

Timestamp: 2026-06-29T15:27:23Z
Speaker: Planner (deployment-gate / QA-matrix architect)

Summary:
Authored the canonical plan for task-ux-702, the deployment gate implementing the QA-gate half of PRD D7
(`prd.md:344-373`) and the Rollout / Kill Switch section (`prd.md:925-931`). Held the AUTHORITATIVE
interaction mode `manual_execution` / operator-owned (tasks.json:794-795): the live 3-network ×
(default + custom storage) × 5 recovery-path × 3 packaged-build matrix (90 cells), the per-OS guard-off
check, and the six task-401 regression re-tests are operator-owned and NOT executed in this workflow.

Scoped this workflow to the autonomous-eligible subset: this plan + the deliverable scaffolding — the
QA-results matrix template (every cell PENDING), the six-regression table anchored to
research/18:69,85,101,118,135,159 with each landed root-cause fix file tagged, the kill-switch
production-default + network-scoping DECISION FRAMEWORK (criteria from research/18:17-25 + an *advisory*
`keep guarded by default` recommendation, deliberately NOT locked because PRD defers the default at
`prd.md:929,939`), the rollback checklist (research/18:165-169 + `prd.md:925-931`), and the research-note
skeleton with operator sections fenced PENDING OPERATOR EXECUTION.

Carried the locked invariants inline: kill switch hides ALL partial-sync UI when off (boundary #1);
startup-owned native recovery dialog persists for interrupted unsafe-cutover installs (#2); staged-only /
no-cancel-after-cutover (#3/#4); latest-snapshot-only / no picker (#5); networks mainnet/preprod/preview
(#6); no bootstrap regression (#7); and the vocab guardrail — "Mithril Sync" vs "standard sync", never
"partial sync" in user copy, behind-ness epochs-only (#8). Pinned the acceptance criteria verbatim from
tasks.json:806-811. Explicitly held the gate OPEN: task-ux-702 is NOT marked completed and no `completedAt`
is set by this workflow; the JSON-sync + ship decision + commit happen LATER only after the operator's
manual assessment. Did not open, read, or rely on `task-ux-702-manual-assessment.md` (existence noted only).

Wrote: task-ux-702.md (this canonical doc); will also scaffold task-ux-702-research.md,
task-ux-702-impl-review.md, and append this entry to task-ux-702-plan-review.md.

Decision: approved
