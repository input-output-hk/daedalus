# Task task-002: Lock the restore and install strategy from spike evidence

## Task ID and Title

- ID: `task-002`
- Title: `Lock the restore and install strategy from spike evidence`

## Why Chosen Now

`task-001` is complete and removed the main unknowns around partial restore CLI behavior. `task-002` is the next critical gate because later backend, IPC, and renderer work should not proceed until the restore boundary, install boundary, and top-level chain-directory handling rules are explicitly locked from recorded evidence instead of being inferred during implementation.

## Interaction Mode

- Mode: `autonomous`

This is a planning and documentation-hardening task. The required evidence already exists in repo-local research, and the task is to turn that evidence into the approved restore/install strategy and the follow-on doc updates that later implementation tasks should follow.

## Scope

- Lock the approved partial-sync restore strategy from `task-001` evidence.
- State whether staged restore or direct in-place restore is approved.
- Define the allowed handling rules for top-level DB entries in the managed chain target: `clean`, `immutable`, `ledger`, `lsm`, `protocolMagicId`, and `volatile`.
- Identify the minimum PRD, task-graph, and research updates required so later tasks inherit the same safety assumptions.
- Preserve existing fixed decisions for diagnostics-launched partial sync: manual entry, automatic node stop before work, automatic node restart after success, supported networks `mainnet`/`preprod`/`preview`, and no regression of empty-chain Mithril bootstrap.

## Non-Goals

- Implementing partial sync code, IPC channels, stores, or UI.
- Reopening `task-001` evidence collection unless a contradiction is found.
- Fully specifying immutable-range derivation or corrupted-layout preconditions; that belongs to `task-003`.
- Fully specifying failure branching, rollout kill switch, or restart-normal eligibility rules; that belongs to `task-004`.
- Designing speculative merge machinery for future restore modes not supported by the spike.

## Relevant Dependencies

- Required completed dependency: `task-001`
- Direct downstream tasks affected by this decision:
  - `task-003` define immutable range derivation and chain-layout preconditions
  - `task-004` define failure containment, kill-switch, and rollback rules
  - `task-100`, `task-102`, `task-200`, `task-203`, and `task-204`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- Workflows:
  - `.agent/workflows/test.md`
  - `.agent/workflows/update-doc.md`
- Skills:
  - `understand` loaded first per instruction; live repo findings for this plan were then verified directly against current files

## Live Repo Findings Verified For Planning

- `flake.lock` still pins Mithril to revision `2940975d3f9020d975b4bc62d75a646aa353c03a`.
- `flake.nix` still points Mithril to branch `sl/fix-mismatch-rust-versions`, with the lockfile revision remaining the planning source of truth.
- `nix/internal/common.nix` and `nix/internal/cardano-bridge.nix` still package `mithril-client` for supported platforms.
- `source/main/mithril/mithrilCommandRunner.ts` still resolves the bundled `mithril-client` via `DAEDALUS_INSTALL_DIRECTORY` when present.
- `source/main/mithril/mithrilNetworkConfig.ts` still supports only `mainnet`, `preprod`, and `preview`.
- `source/main/utils/chainStorageCoordinator.ts` still treats bootstrap as an empty-chain-only path and enforces a stopped-node precondition for managed chain mutations.
- `source/main/cardano/setup.ts` still suppresses generic cardano-node restart behavior while Mithril bootstrap is active.
- `source/main/mithril/MithrilBootstrapService.ts` still performs ancillary-enabled restore followed by explicit local LSM conversion and install.
- `source/main/utils/handleDiskSpace.ts` still owns the empty-chain Mithril startup gate and node-start handoff, so the partial-sync strategy must not weaken this existing bootstrap path.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-002.md`

## Implementation Approach

1. Lock the approved restore boundary from spike evidence.
   - Approve staged restore only.
   - Treat direct in-place partial restore into the live managed chain target as unapproved because `task-001` proved only scratch-target `--allow-override`, not safe mutation of a populated Daedalus chain target.
   - Keep the staging area under Daedalus-managed Mithril work space, separate from the live chain root.

2. Lock the approved download and conversion assumptions.
   - Partial sync uses `latest` snapshot metadata internally.
   - Ancillary-enabled partial restore remains required for the intended fast catch-up path.
   - Ancillary-enabled partial restore is still conversion-required because the spike showed `ledger/` is restored but `lsm/` is not created automatically, and the CLI emitted explicit converter commands.
   - Range choice must be tied to current snapshot metadata closely enough to tolerate latest-snapshot drift, but the exact derivation algorithm is deferred to `task-003`.

3. Lock one explicit managed-target cutover rule.
   - Do not point Mithril restore at the live managed chain target.
   - Do not treat `--allow-override` as approval for live-target mutation.
   - After staged restore, verification, and required LSM conversion succeed, Daedalus cuts over by emptying the managed chain target and reinstalling only a fixed allowlist of validated staged entries.
   - The allowlist is `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`.
   - No live top-level entry is merged or preserved across cutover. Existing live `volatile/` is discarded as part of the cutover and must be recreated by cardano-node after restart instead of being merged with staged data.
   - Any unexpected staged top-level entry, including `volatile/`, blocks install instead of being copied through implicitly.
   - Treat cancellation or interrupted staged restore as cleanup-required and not normal-restart-safe on that partially restored target.

4. Lock top-level DB handling rules at the level the evidence supports.
   - `clean`: required staged metadata; replace from validated staged output only.
   - `immutable`: replace from validated staged output only; never direct live restore target.
   - `ledger`: replace from validated staged output only after successful restore and required conversion.
   - `lsm`: must be produced by Daedalus-owned conversion before cutover, then replaced from validated staged output only.
   - `protocolMagicId`: replace from validated staged output together with the restored DB set.
   - `volatile`: never merged, never installed from current staged output, and never preserved from the live target across cutover.
   - `task-003` may still define start preconditions and `task-203` may still define exact implementation mechanics, but neither task may reopen this cutover rule or broaden the allowlist.

5. Update planning artifacts before implementation starts.
   - Update the PRD restore-strategy section so it explicitly names staged restore plus validated staged install as the approved path, not just the current assumption.
   - Update the tasks graph where wording still leaves room for direct in-place restore, unspecified merge behavior, or an incomplete top-level artifact list.
   - Add a required strategy decision addendum to `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` so the executed spike evidence and the approved interpretation stay linked in the repo.
   - Add one rejected alternative: direct live-target restore via `--allow-override`, rejected because the spike did not prove safety for populated Daedalus chain storage and cancellation leaves partial artifacts behind.

## Acceptance Criteria

- The plan names staged restore plus validated staged install as the approved strategy.
- The plan explicitly rejects direct in-place restore into the live managed chain target.
- The plan names one explicit cutover rule: empty the managed target and reinstall only allowlisted staged entries `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`.
- The plan documents evidence-backed handling rules for `clean`, `immutable`, `ledger`, `lsm`, `protocolMagicId`, and `volatile`.
- The plan preserves existing fixed decisions: manual diagnostics entry, supported networks `mainnet`/`preprod`/`preview`, automatic node stop before partial sync, automatic node restart after success, and no regression of empty-chain Mithril bootstrap.
- The plan identifies the minimum PRD, tasks, and research updates required before implementation tasks continue.
- The plan records at least one rejected alternative and why it was rejected.

## Verification Plan

- Verify that the approved strategy is consistent with `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`:
  - staged scratch download area is safe
  - ancillary restore is conversion-required
  - `--allow-override` proved only scratch-target overwrite
  - cancellation leaves partial artifacts and is not restart-safe
  - staged artifact layout includes `protocolMagicId` and excludes `volatile`
- Verify against live repo files that the locked strategy does not conflict with current Daedalus bootstrap architecture:
  - `source/main/utils/handleDiskSpace.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/cardano/setup.ts`
  - `source/main/mithril/MithrilBootstrapService.ts`
  - `source/main/utils/chainStorageManager.ts`
  - `source/main/utils/chainStorageManagerLayout.ts`
- Verify that follow-on doc updates stay minimal and do not pre-decide work reserved for `task-004`.

## Risks and Open Questions

- Latest-snapshot drift means later implementation must bind range derivation tightly to current snapshot metadata or tolerate retry on drift.
- The current shared installer empties the managed target and moves every staged top-level entry it finds, so `task-203` must narrow that behavior to the locked allowlist instead of reusing generic move-all semantics unchanged.
- Restart-normal eligibility after later install-stage failures remains open and belongs to `task-004`.

## Required Docs, Tracking, and Research Updates

- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` to change restore-strategy wording from assumption to approved staged strategy and to record the rejected direct-override alternative.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` where later task wording still implies that direct in-place restore, unspecified live merge behavior, or incomplete top-level artifact handling remains acceptable.
- Add a short required strategy decision addendum to `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` to make the handoff from spike evidence to approved strategy explicit.
- Do not create broader architecture or workflow docs for this task.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-002-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-002-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- The staged-only restore strategy is now locked across the canonical task plan, PRD, task graph, and spike-results research note.
- The approved managed-target cutover rule is explicit: empty the managed chain target and reinstall only validated staged entries from the fixed allowlist `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`.
- Direct live-target restore via `--allow-override` is now documented as rejected, and live `volatile/` is explicitly discarded rather than merged or preserved across cutover.
- Downstream task wording now reflects that `task-203` must enforce the allowlist cutover instead of an open-ended install-or-merge strategy.

## Final Outcome

- `task-002` completed as an `autonomous` documentation-hardening gate.
- Acceptance criteria satisfied:
  - PRD now names the selected staged-only install strategy and exact cutover rule
  - top-level artifact handling rules are documented for `clean`, `immutable`, `ledger`, `lsm`, `protocolMagicId`, and `volatile`
  - the spike-results note now contains the required strategy decision addendum
  - later task wording changed where assumptions needed to match the locked cutover rule
- Review outcome:
  - planning loop converged after one critique pass and one final planner revision
  - implementation review approved on iteration 1 with no blocking findings
- Durable design decision carried forward:
  - later implementation tasks must preserve the staged-only allowlist cutover unless new spike evidence explicitly reopens that decision

## Self-Review

- Scope creep check: kept range-derivation details, failure-option safety rules, and implementation mechanics out of this task plan.
- Docs/tests check: included the required PRD/tasks/research updates and a verification pass against both spike notes and live files; no build/test execution is needed for the planning loop itself.
- Consistency check: the locked strategy matches the fixed decisions, the spike conclusions, and the existing empty-chain bootstrap architecture.
