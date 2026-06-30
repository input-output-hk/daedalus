# task-ux-702 — Implementation Review Log (append-only)

> Append-only. Each entry: ISO-8601 UTC `Timestamp:`, a speaker label
> (Implementation | Code-Review | Scribe), and a `Decision: approved|requires_changes` line.
> (Planner phase complete — see the `## Planner` entry in `task-ux-702.md` and the Critiquer approval in
> `task-ux-702-plan-review.md`.)
>
> NOTE (interaction mode `manual_execution` / operator-owned): the autonomous-eligible subset of this
> workflow is scaffolding the deliverable artifacts only. The live 3-network × (default + custom storage) ×
> 5 recovery-path × 3 packaged-build matrix (90 cells), the per-OS guard-off check, the six task-401
> regression re-tests (gap #18), the empty-chain bootstrap re-check, and the final ship/merge readiness
> decision are ALL OPERATOR-OWNED and are NOT run in this workflow. Do NOT mark the task `completed`, add
> `completedAt`, or commit from this stage — that happens LATER, only after the operator's manual
> assessment is recorded.

---

## Implementation

Timestamp: 2026-06-29T15:32:45Z
Speaker: Implementation

Executed the autonomous-eligible subset of task-ux-702 (scaffolding the deliverable artifacts). The live
multi-network / custom-storage / packaged-build QA matrix, the per-OS guard-off checks, and the six
task-401 regression re-tests remain operator-owned and were NOT run here.

### What was scaffolded

**`task-ux-702-research.md`** (created) — the primary scaffolding artifact, containing:
- §1 Scope grounding: gate context, AUTHORITATIVE interaction mode, and the eight locked safety boundaries
  carried verbatim from the canonical plan (kill switch hides ALL partial-sync UI when off; startup-owned
  native recovery dialog persists for interrupted unsafe-cutover installs; staged-only / no-cancel-after-
  cutover; latest-snapshot-only / no picker; networks mainnet/preprod/preview; no bootstrap regression;
  vocab "Mithril Sync" vs "standard sync", never "partial sync" in user copy, behind-ness epochs-only).
- §2 QA-results matrix template: the full 90-cell cross-product enumerated in six network×storage
  sub-tables (mainnet/preprod/preview × default/custom) each with 5 scenario rows × 3 OS columns, every
  cell defaulting to `—` (PENDING). Plus three supplementary tables: per-OS guard-off check (§2b),
  startup-owned unsafe-recovery pass (§2c), and empty-chain Mithril bootstrap re-check (§2d). Each table
  carries operator-fill instructions referencing the research/18:39-54 Run Template fields.
- §3 Six task-401 regression re-test rows: one fillable row per regression (R1-R6), each `PENDING` with
  its research/18 anchor (`:69,:85,:101,:118,:135,:159`) and tagged with the landed root-cause fix file
  (`chainStorageCoordinator.ts`, `handleDiskSpace.ts` ×2, `mithrilPartialSyncChannel.ts`,
  `chainStorageManagerLayout.ts`, plus the deferred representative-recovery re-validation). Operator
  instruction to test INDIVIDUALLY per gap #18 / tasks.json:797.
- §4 Kill-switch production-default + network-scoping decision framework: decision criteria sourced from
  research/18:17-25, three network-scoping options (global / staged testnets-first / all-or-nothing), an
  explicitly *advisory* `keep guarded by default` recommendation with staged scoping (option ii) as the
  path to `enabled by default`, and the Critiquer-requested divergence note acknowledging that research/18
  :171-174 recommended `enabled by default` for the narrower task-401 checkpoint. `FINAL DECISION:
  PENDING OPERATOR/USER SIGN-OFF` line present with a fill-in block.
- §5 Rollback checklist: six `[ ] PENDING` items grounded in research/18:165-169 and `prd.md:925-931`,
  each with a build-ID + evidence slot for the operator. Items cover: ALL partial-sync UI hidden (guard
  off), backend rejects diagnostics-launched start + restart-normal, startup-owned recovery dialog remains,
  wipe-and-full-sync remains available, empty-chain bootstrap unaffected, switch flippable via launcher
  config without an app release.
- §6 Operator-owned execution log: fenced PENDING OPERATOR EXECUTION with overall verdict tallies,
  defect-recording block, and merge/ship readiness decision fill-in.
- §7 Cross-links: research/02, /18, /19; tasks.json:782-812; PRD D7 + Rollout + Open Questions; canonical
  plan + review logs.

**`task-ux-702-impl-review.md`** (this file, created) — append-only transcript stub with the header note
stating interaction mode + operator-owned scope and this Implementation entry.

### What was explicitly NOT run (operator-owned)
- Live QA on any of the 90 matrix cells (mainnet/preprod/preview × default/custom storage × 5 scenarios ×
  Windows/macOS/Linux packaged builds).
- Per-OS guard-off checks (mithrilPartialSyncEnabled = false).
- Six task-401 regression re-tests (R1-R6) against live behavior.
- Empty-chain Mithril bootstrap re-check on any network.
- Final production-default + network-scoping decision (advisory recommendation authored; final lock is
  operator/user sign-off).
- Any source/ changes (none required; no defect proven by autonomous subset).

### Automated checks
No `yarn` build/test run was required for doc scaffolding. The automated/backend coverage was already
proven green by task-ux-503 / task-ux-701 (not re-claimed as task-ux-702 evidence per the verification
plan in the canonical doc).

### Deviations from the plan
None. All five Part 1 checklist steps are complete:
1. Canonical plan `task-ux-702.md` already existed (authored in the planning stage).
2. `task-ux-702-research.md` scaffolded with all required sections (§1-§7).
3. `task-ux-702-plan-review.md` already existed with Planner + Critiquer entries (created in planning stage).
4. `task-ux-702-impl-review.md` (this file) created with header + this Implementation entry.
5. Tasks JSON status left `pending` / non-completed; no `completedAt` set; no ship decision made; gate held OPEN.

Decision: approved (autonomous scaffolding subset only; operator manual_execution still required before sprint DoD)

---

## Code Review

Timestamp: 2026-06-29T15:36:19Z
Speaker: Code-Review (pass 1 of max 3)

Summary:
One broad pass over the scaffolded deliverables (`task-ux-702.md`, `task-ux-702-research.md`,
`task-ux-702-plan-review.md`, this `task-ux-702-impl-review.md`) against the approved canonical plan and
the Critiquer approval. Did NOT open `task-ux-702-manual-assessment.md` (existence noted in git status
only). Spot-checked cited anchors against source: research/18:17-25 Decision Rules, research/18:69/:85
regression notes, prd.md:925-931 Rollout/Kill Switch, prd.md:939 Open Question — all match the wording the
deliverables attribute to them; no fabricated citations.

Verified:
- **Deliverables match the approved plan.** The research note's §1-§7 map exactly to Part 1 step 2 of the
  canonical plan (`task-ux-702.md:162-175`): scope grounding + boundaries 1-8, QA matrix template,
  six-regression rows, decision framework, rollback checklist, fenced operator log, cross-links.
- **QA matrix complete.** §2a enumerates the full 90-cell cross-product as six network×storage sub-tables
  (mainnet/preprod/preview × default/custom), each 5 scenarios (success/cancel/retry/restart-normal/
  wipe-and-full-sync) × 3 OS (Windows/macOS/Linux) = 90 cells, every cell `—` (pending). §2b per-OS
  guard-off, §2c startup-owned unsafe-recovery, §2d empty-chain bootstrap re-check all present and PENDING.
  Honors Critiquer N2 (90 core rows + extra tables). Matches tasks.json:784 / prd.md:920-923.
- **Six task-401 regressions individually.** §3 has one fillable row per R1-R6, each anchored to the
  correct research/18 line (:69,:85,:101,:118,:135,:159) and tagged with the landed root-cause fix file;
  operator instruction to test individually per gap #18 / tasks.json:797 is present, with the R6
  not-a-single-fix caveat called out.
- **Cells correctly fenced PENDING.** Every results cell defaults to pending; each section carries a
  `PENDING OPERATOR EXECUTION` fence; §6 operator log fully fenced; no result, build ID, verdict, or
  timestamp is fabricated.
- **Decision framework present** with criteria from research/18:17-25, three network-scoping options, an
  explicitly *advisory* `keep guarded by default` recommendation, the Critiquer-N1 divergence note vs
  research/18:171-174, and a `FINAL DECISION: PENDING OPERATOR/USER SIGN-OFF` fill-in block — correctly
  NOT locked because the PRD defers the default (prd.md:929,939).
- **Rollback checklist complete** (§5): six `[ ] PENDING` items grounded in research/18:165-169 +
  prd.md:925-931, honoring locked invariants — guard-off hides ALL partial-sync UI (#1), backend rejects
  diagnostics-launched start + restart-normal (#1), startup-owned recovery dialog remains (#2), wipe
  remains, empty-chain bootstrap unaffected (#7), switch flippable via launcher config without a release.
- **Research note mirrors 701 structure** with correct cross-links (§7 → research/02, /18, /19; PRD D7 +
  Rollout + Open Questions; canonical plan + both review logs).
- **Interaction mode correct:** `manual_execution` / operator-owned throughout, never relabeled
  autonomous; Part 1 autonomous subset cleanly separated from Part 2 operator-owned execution.
- **Gate held OPEN:** tasks.json:785 status is still `pending`; no `completedAt`; no ship/merge decision
  recorded; no commit from this stage.
- **No source-code changes.** `git status` shows only the new phase-7 docs (plus a pre-existing `.gitignore`
  edit and unrelated `assets/` + `scheduled_tasks.lock` untracked entries); no `source/` paths touched.
- **Append-only logs intact:** plan-review (Planner + Critiquer) and this impl-review (Implementation +
  this entry) only append; prior entries unmodified.
- **Acceptance criteria verbatim:** `task-ux-702.md:251-254` reproduces tasks.json:806-811 exactly (all
  four lines).

Consolidated blockers: none.

Non-blocking notes:
- NB1. §2d allows the bootstrap re-check OS column to be "(any)"; the operator should still record the
  actual OS/build ID per the Run Template when filling it, for parity with §2a. Cosmetic only.
- NB2. The startup-owned unsafe-recovery pass (§2c) is qualified "where safely exercisable"; if it cannot
  be exercised on a given OS, the operator should mark it BLOCKED with rationale rather than leaving it
  ambiguous. Documentation nicety, not a defect.

Decision: approved

---

## Scribe

Timestamp: 2026-06-29T15:37:30Z
Speaker: Scribe (documentation finalization)

Summary of durable findings recorded:

**Research note (`task-ux-702-research.md`) confirmed complete and accurate.**
All sections are present with correct content: §1 scope grounding + eight locked safety boundaries;
§2 QA-results matrix template (90 cells across 6 network×storage sub-tables, all cells defaulting to `—`
PENDING, plus per-OS guard-off §2b, startup-owned unsafe-recovery §2c, empty-chain bootstrap §2d);
§3 six task-401 regression re-test rows (R1-R6), each anchored to the correct research/18 line
(`:69,:85,:101,:118,:135,:159`) and tagged with the landed root-cause fix file; §4 kill-switch
production-default + network-scoping decision framework (advisory `keep guarded by default`; staged
testnets-first as recommended path; final lock PENDING OPERATOR/USER SIGN-OFF); §5 rollback checklist
(six `[ ] PENDING` items grounded in research/18:165-169 and prd.md:925-931); §6 operator-owned
execution log (fully fenced PENDING OPERATOR EXECUTION); §7 cross-links to research/02, /18, /19,
tasks.json:782-812, PRD D7 + Rollout + Open Questions, canonical plan, and both review logs.

**All operator evidence sections are correctly fenced PENDING OPERATOR EXECUTION** — no fabricated
results, build IDs, verdicts, or timestamps appear anywhere in the scaffolded deliverables.

**Canonical plan footer (`task-ux-702.md` `## Planning status / build status`) updated** to reflect:
plan approved; scaffolding complete and code-reviewed (pass 1 of max 3, no blockers, Decision: approved);
deployment gate OPEN pending operator manual assessment; `task-ux-702-manual-assessment.md` existence
noted (NOT opened, read, or relied upon per constraint #4).

**Tasks JSON sync decision:** The file `mithril-partial-sync-ux-refinement-tasks.json` contains only
two status values — `"completed"` and `"pending"`. No non-completed in-progress enum value exists.
Therefore task-ux-702 status is LEFT AS `"pending"` in the JSON. The scaffolding-done / gate-open
state is recorded in the canonical plan footer and in this Scribe entry only. No new enum value was
invented.

**No commit and no completion happen in this workflow.** The deployment gate remains OPEN. The operator
manual assessment has not yet been executed; it will be brought in separately (via
`task-ux-702-manual-assessment.md` and recorded in this log and in `-research.md` §6). Only after
the operator matrix, regression re-tests, guard-off checks, and ship decision are recorded does this
task reach completion and get committed.

Decision: approved
