# task-ux-702 — Plan Review Log (append-only)

> Append-only chronological transcript. Each entry carries a speaker label
> (Planner | Critiquer), an ISO-8601 UTC `Timestamp:`, and an outcome; Critiquer entries end with
> `Decision: approved` or `Decision: requires_changes`. Append at end-of-file only; never rewrite history.
>
> The Planner self-asserts readiness below; the Critiquer gates it next. The operator-owned manual QA
> matrix, the six task-401 regression re-tests, and the final ship decision are executed OUTSIDE this
> workflow — this log governs only the planning + scaffolding stage. The supplementary
> `task-ux-702-manual-assessment.md` is NOT consulted by this workflow.

---

## Planner

Timestamp: 2026-06-29T15:27:23Z
Speaker: Planner (deployment-gate / QA-matrix architect)

Summary:
Produced the canonical plan `task-ux-702.md` for the deployment gate — the QA-gate half of PRD D7
(`prd.md:344-373`) and the Rollout / Kill Switch section (`prd.md:925-931`). Held the AUTHORITATIVE
interaction mode `manual_execution` / operator-owned (tasks.json:794-795): the live 3-network ×
(default + custom storage) × 5 recovery-path × 3 packaged-build matrix (90 cells), the per-OS guard-off
check, the six task-401 regression re-tests (gap #18, research/18:69,85,101,118,135,159), the empty-chain
bootstrap re-check, and the final ship decision are operator-owned and NOT executed in this workflow.

Scoped this workflow to the autonomous-eligible subset and fenced the operator-owned items in a separate
Part 2 of the implementation checklist:
- QA-results matrix template enumerating all axes (every cell PENDING), with the research/18 Run-Template
  fields per cell.
- Six-regression table, one fillable row each, anchored to research/18 with the landed root-cause fix file
  tagged (chainStorageCoordinator, handleDiskSpace×2, mithrilPartialSyncChannel, chainStorageManagerLayout,
  plus the deferred representative-recovery re-validation).
- Kill-switch production-default + network-scoping DECISION FRAMEWORK: criteria from research/18:17-25 +
  an *advisory* `keep guarded by default` recommendation with staged network-scoping as the path to
  `enabled by default` — deliberately NOT locked, because the PRD defers the production default
  (`prd.md:929,939`).
- Rollback checklist (research/18:165-169 + `prd.md:925-931`).
- Research-note skeleton with operator sections fenced PENDING OPERATOR EXECUTION.

Carried the locked invariants inline (kill switch hides ALL partial-sync UI when off; startup-owned native
recovery dialog persists for interrupted unsafe-cutover installs; staged-only / no-cancel-after-cutover;
latest-snapshot-only / no picker; networks mainnet/preprod/preview; no bootstrap regression; vocab
"Mithril Sync" vs "standard sync", never "partial sync" in user copy, behind-ness epochs-only). Pinned the
acceptance criteria verbatim from tasks.json:806-811. Held the gate OPEN: task-ux-702 is NOT marked
completed, no `completedAt` is set, and no ship decision/commit is made by this workflow — those land later
only after the operator's manual assessment. Did not open or rely on `task-ux-702-manual-assessment.md`
(existence noted only).

Decision: approved

---

## Critiquer

Timestamp: 2026-06-29T15:31:02Z
Speaker: Adversarial Critiquer (plan review, pass 1 of max 3)

Summary:
Reviewed `task-ux-702.md` against the tasks.json entry (:782-812), PRD D7 (`prd.md:344-373`), the
deployment-gate testing bullet (`prd.md:920-923`), Rollout / Kill Switch (`prd.md:925-931`), Open
Questions (`prd.md:933-939`), the coverage map (`prd.md:945-962`), research/18 (Decision Rules :17-25;
Required Scenario Coverage :27-37; Run Template :39-54; six regressions :69,85,101,118,135,159; Rollup
:153-169; Final Recommendation :171-178), research/19 (gap rows #16-#19 :143-146), and the 701 precedent.
Did NOT open `task-ux-702-manual-assessment.md` (existence noted only).

Verified:
- QA matrix coverage is complete: 3 networks (mainnet/preprod/preview) x (default + custom storage) x 5
  recovery paths (success/cancel/retry/restart-normal/wipe-and-full-sync) x 3 packaged builds
  (Windows/macOS/Linux) = 90 cells, plus the per-OS guard-off check and startup-owned unsafe-recovery
  pass. Matches tasks.json:784 and prd.md:920-923 exactly.
- The six task-401 regressions are re-tested INDIVIDUALLY (gap #18), each row anchored to the correct
  research/18 line (R1 :69, R2 :85, R3 :101, R4 :118, R5 :135, R6 :159) and tagged with the landed
  root-cause fix file. Anchor mapping is correct against the source runs.
- Interaction mode is AUTHORITATIVE `manual_execution` / operator-owned and is NOT relabeled autonomous;
  the autonomous-eligible scaffolding subset (Part 1) is cleanly separated from operator-owned live
  execution (Part 2), consistent with tasks.json:794-795 and the phase-7 rule.
- Kill-switch production-default + network-scoping decision framework is present with criteria sourced
  from research/18:17-25 and an explicitly *advisory* `keep guarded by default` recommendation, correctly
  NOT locked because the PRD defers the production default (`prd.md:929,939`).
- Rollback checklist present and grounded (research/18:165-169 + prd.md:925-931), every item PENDING
  operator confirmation.
- Acceptance criteria reproduced verbatim from tasks.json:806-811 (exact match on all four).
- Locked invariants 1-8 carried inline and consistent with PRD/701 (kill switch hides ALL partial-sync
  UI when off; startup-owned native recovery dialog persists; staged-only / no-cancel-after-cutover;
  latest-snapshot-only / no picker; networks mainnet/preprod/preview; no bootstrap regression; vocab
  "Mithril Sync" vs "standard sync", never "partial sync" in user copy, behind-ness epochs-only).
- Gate is correctly held OPEN: no JSON status flip to completed, no `completedAt`, no ship decision/commit
  by this workflow.
- Part 1 scaffolding steps (1-5) are concrete and small-model-executable; no hidden manual checkpoints
  are smuggled into the autonomous subset.

Consolidated blockers: none.

Non-blocking notes:
- N1. The advisory `keep guarded by default` recommendation diverges from research/18:171-174, whose
  task-401 Final Recommendation is `enabled by default` (an explicit user decision for that narrow
  Linux/preview/`yarn dev` checkpoint with broader coverage deferred). The plan's posture is the more
  conservative and correct one for the FULL deployment gate, and is directly supported by the Decision
  Rule at research/18:24 ("keep guarded by default is the truthful outcome when ... packaged-build
  evidence or matrix coverage is incomplete"). Optional: add a one-line note in the decision-framework
  section explicitly acknowledging this divergence from the task-401 narrow-checkpoint recommendation so
  a later reader does not read it as a contradiction. Not required for approval.
- N2. The cross-product is stated as 90 cells "plus" the per-OS guard-off and startup-owned unsafe-recovery
  passes, so the true cell count exceeds 90; the plan already says this, just keep the research-note matrix
  template consistent (90 core rows + the extra guard-off/unsafe-recovery rows) when scaffolding §2.

Decision: approved
