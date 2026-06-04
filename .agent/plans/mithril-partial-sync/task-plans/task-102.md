# Task task-102: Add ChainStorageCoordinator Partial Sync APIs And Operation Locking

## Task

- Task ID: `task-102`
- Title: `Add chainStorageCoordinator partial sync APIs and operation locking`

## Why This Task Now

- `task-100` and `task-101` are complete, so the shared partial-sync contracts and dedicated IPC seams now exist.
- The next smallest backend step is to give the main process one coordinator-owned place to enforce node-stopped, managed-layout, recovery-fallback, and operation-serialization rules before any partial-sync service work begins.
- `task-200` and `task-103` depend on this seam; without it, later service and node-lifecycle work would have to duplicate chain-storage safety checks.

## Interaction Mode

- `autonomous`

## Scope

- Extend `ChainStorageCoordinator` with explicit partial-sync lifecycle entry points that remain separate from `startBootstrap()`.
- Limit task-102 to `startPartialSync`, `cancelPartialSync`, mutual-exclusion state or locking, and the minimal delegate-registration/context seam that later partial-sync service work needs.
- Serialize partial-sync start with existing coordinator mutations and Mithril bootstrap work.
- Enforce strict node-stopped, managed-layout, and workdir resolution preconditions in the coordinator before later partial-sync service execution begins.
- Fail closed for partial sync when managed-layout normalization reports `isRecoveryFallback`.
- Pass preflight context through a partial-sync-specific delegate seam that includes `layoutResult` and resolved Mithril workdir, without implying reuse of bootstrap service state.

## Non-Goals

- Implementing the partial-sync engine, latest snapshot lookup, immutable range derivation, download flow, or cutover logic; those belong to `task-200` through `task-204`.
- Reusing `startBootstrap()` for partial sync or weakening its empty-chain invariant.
- Adding recovery-action coordinator APIs for restart-normal or wipe-and-full-sync; those defer to `task-204` unless later implementation proves one concrete coordinator-owned chain-storage mutation is required sooner.
- Adding renderer stores, diagnostics UI, or startup restart-suppression changes; those belong to later tasks.
- Reopening the staged-only restore strategy, range-derivation rules, or boundary-dependent recovery rules already locked by `task-002` through `task-004`.
- Broad refactors of `ChainStorageManager`, `handleDiskSpace`, or bootstrap IPC behavior beyond the narrow coordinator seam this task needs.

## Relevant Dependencies

- Required completed dependencies:
  - `task-100`
  - `task-101`
- Design inputs verified from completed planning tasks:
  - `task-002`
  - `task-003`
  - `task-004`
- This task directly unblocks:
  - `task-103`
  - `task-200`
  - `task-204`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril-partial-sync/research/03-task-100-shared-contract-notes.md`
- `.agent/plans/mithril-partial-sync/research/04-task-101-ipc-wrapper-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-client-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-process-output.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- `.agent/plans/mithril/chain-storage-pr-review-fixes.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand`

## Live Repo Findings Verified For Planning

- `source/main/utils/chainStorageCoordinator.ts` already owns the serialized mutation queue, strict `STOPPED` node guard, workdir syncing through `resolveMithrilWorkDir()`, and bootstrap-specific overlap protection via `_bootstrapInProgress`.
- `startBootstrap()` already proves the intended coordinator pattern for Mithril-managed work: acquire the coordinator lock, enforce layout and node-state preconditions once, set an in-progress flag, then delegate outside the lock while clearing the flag in `finally`.
- `cancelBootstrap()` intentionally bypasses the mutation queue and delegates directly to the active service, which is useful precedent if partial-sync cancel needs to be prompt rather than queued behind unrelated filesystem work.
- `source/main/utils/chainStorageCoordinator.spec.ts` already covers the coordinator seam directly and is the right place to lock new partial-sync overlap and precondition behavior.
- `source/main/ipc/mithrilPartialSyncChannel.ts` currently exposes dedicated channels but still rejects all actions as not implemented, so `task-102` does not need to force full IPC behavior if the coordinator seam alone is sufficient.
- `source/main/cardano/setup.ts` currently suppresses generic crash restart only for bootstrap via `isMithrilBootstrapNodeStartBlocked()`, confirming that node lifecycle suppression is still a later task and should not be folded into this one.
- `task-003` and the PRD now require partial sync to fail closed on `ManagedChainLayoutResult.isRecoveryFallback`, while the existing coordinator `ensureManagedChainLayout()` helper does not yet enforce that rule for any caller.
- No partial-sync service exists in `source/main/mithril/` yet, so this task should add only a narrow partial-sync-specific delegate registration or callback seam rather than inventing orchestration logic prematurely.
- `resolveMithrilWorkDir()` already exists on `ChainStorageManager`, so task-102 can resolve a partial-sync-specific workdir during preflight and pass it as explicit context instead of mutating shared bootstrap-service state via `_syncMithrilWorkDir()`.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-102.md`
- `source/main/utils/chainStorageCoordinator.ts`
- `source/main/utils/chainStorageCoordinator.spec.ts`

## Implementation Approach

1. Add explicit coordinator-owned partial-sync operation state.
    - Introduce a partial-sync-in-progress guard that is separate from `_bootstrapInProgress` so bootstrap and partial sync remain distinct operations.
    - Keep one shared coordinator rule: partial sync cannot overlap with bootstrap or with serialized chain-storage mutations that change the managed target.
    - Prefer a small shared assertion helper for Mithril-managed exclusivity instead of scattering separate ad hoc checks across each method.

2. Add explicit partial-sync coordinator entry points instead of reusing bootstrap APIs.
    - Add a `startPartialSync(...)` method that is distinct from `startBootstrap(...)` because the invariants differ.
    - Add `cancelPartialSync()` as the only other explicit lifecycle method in this task.
    - Defer recovery-action coordinator APIs to `task-204` unless later implementation proves one concrete coordinator-owned chain-storage mutation is required sooner.
    - Keep these methods thin: coordinator owns preconditions, mutual exclusion, and delegation only, while later service tasks own the actual partial-sync workflow.

3. Enforce partial-sync preconditions in one place before delegation.
    - `startPartialSync(...)` should require `cardano-node` state to be strictly `STOPPED`, matching the existing coordinator guard.
    - It should call `ensureManagedChainLayout(...)` and resolve the Mithril workdir through `ChainStorageManager`, but it should pass those values forward as explicit partial-sync preflight context rather than mutating bootstrap-service state via `_syncMithrilWorkDir()`.
    - It should fail closed if `layoutResult.isRecoveryFallback` is true.
    - It should build one partial-sync-specific preflight context object for delegation, at minimum containing `layoutResult` and resolved Mithril workdir.
    - It must not apply bootstrap's empty-chain guard, because partial sync is defined for non-empty managed chain data.

4. Keep the service seam minimal and future-proof.
    - Because `task-200` creates the real `MithrilPartialSyncService`, `task-102` should not build placeholder orchestration logic into the coordinator.
    - The coordinator should instead delegate through a narrow partial-sync-specific registration seam, such as a registered handler or callback that accepts the preflight context object.
    - The delegate contract should be backend-owned and payload-light; renderer-owned range or snapshot inputs must not enter this layer.
    - This delegate seam should be the only new context handoff task-200 depends on: `layoutResult` plus resolved Mithril workdir, not shared bootstrap service state.

5. Keep bootstrap behavior intact while broadening overlap protection.
    - `startBootstrap()` should continue to enforce its empty-chain invariant.
    - Existing directory-change and wipe guards should continue rejecting mutations during bootstrap.
    - Extend those same overlap protections to partial sync without renaming bootstrap concepts into a vague shared state machine.

6. Add focused coordinator tests rather than broad integration coverage in this task.
    - Extend `source/main/utils/chainStorageCoordinator.spec.ts` with mocked partial-sync delegates and lock-order assertions.
    - Test only the coordinator contract: no snapshot resolution, no Mithril CLI behavior, no IPC behavior, and no node restart suppression.

## Acceptance Criteria

- `ChainStorageCoordinator` exposes explicit partial-sync lifecycle APIs rather than forcing callers through `startBootstrap()`.
- Partial sync cannot overlap with existing chain-storage mutations or Mithril bootstrap work.
- `startPartialSync(...)` enforces strict node-stopped and managed-layout preconditions before any downstream service execution begins.
- `startPartialSync(...)` fails closed when `ensureManagedChainLayout(...)` reports `isRecoveryFallback`.
- `startPartialSync(...)` resolves partial-sync preflight context and passes it through a partial-sync-specific delegate seam that includes `layoutResult` and resolved Mithril workdir.
- Task-102 does not introduce restart-normal or wipe-full-sync coordinator APIs unless one concrete coordinator-owned mutation is proven necessary during implementation.
- Bootstrap-specific invariants, especially the non-empty-chain rejection in `startBootstrap(...)`, remain unchanged.
- Coordinator tests cover the new overlap and precondition rules closely enough to catch regression in mutual exclusion or fallback rejection.

## Verification Plan

- Run targeted Jest coverage for `source/main/utils/chainStorageCoordinator.spec.ts`.
- Verify these coordinator behaviors directly in tests:
  - partial-sync start rejects when node state is `running` or `stopping`
  - partial-sync start rejects on `isRecoveryFallback: true`
  - partial-sync start resolves preflight context and passes `layoutResult` plus resolved Mithril workdir to the registered partial-sync delegate
  - partial sync and bootstrap are mutually exclusive in both directions
  - chain-storage mutations remain blocked while partial sync is in progress
  - `cancelPartialSync()` delegates without depending on unrelated coordinator mutations
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` remains blocked by the known unrelated Trezor typing failures, record that and rely on focused Jest plus live diff inspection instead.

## Risks And Open Questions

- The largest design choice in this task is the coordinator-to-service seam because the real partial-sync service does not exist yet. The plan should prefer a tiny delegate interface over creating a fake service class early.
- `cancelPartialSync()` is the one place where serialization and responsiveness pull in opposite directions. The preferred answer is to keep cancel prompt like bootstrap cancel unless the implementation must mutate chain storage inside the coordinator.
- The exact registration shape of the partial-sync delegate can still be chosen during implementation, but the context boundary is now fixed: explicit preflight context with `layoutResult` and resolved Mithril workdir, not implicit bootstrap workdir reuse.
- If task-200 later proves it needs a richer preflight context object from the coordinator, that object should stay narrow and derived from already validated chain-storage state rather than exposing renderer-facing concepts.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc.
- Preserve append-only planning and implementation review logs for this task.
- Mark `task-102` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review are approved.
- Add a short research note only if implementation lands a durable new coordinator-to-service seam decision that later tasks must preserve.
- No `.agent/system/` doc update is required unless task-102 ends up changing the public IPC behavior in the same patch.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-102-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-102-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Approved Plan

- Keep `task-102` narrowly coordinator-focused.
- Add explicit `startPartialSync`, `cancelPartialSync`, a separate partial-sync in-progress guard, and the minimal partial-sync delegate-registration seam instead of reusing bootstrap entry points.
- Enforce strict `STOPPED`, managed-layout, explicit workdir resolution, and `isRecoveryFallback` rejection in the coordinator before later service work begins.
- Pass `layoutResult` plus resolved Mithril workdir through explicit partial-sync preflight context rather than mutating shared bootstrap-service state.
- Serialize partial sync with bootstrap and chain-storage mutations, while preserving bootstrap's existing empty-chain semantics.
- Defer recovery-action coordinator APIs to `task-204`.
- Defer actual Mithril partial-sync orchestration and node restart suppression to later tasks.

## Final Outcome

- Implemented the approved coordinator-only partial-sync seam in `source/main/utils/chainStorageCoordinator.ts`.
- Added exported `PartialSyncPreflightContext`, delegate registration via `setPartialSyncHandlers(...)`, explicit `startPartialSync(...)` and `cancelPartialSync()` APIs, and separate partial-sync in-progress tracking.
- Kept the workdir boundary partial-sync-specific by resolving `layoutResult` plus Mithril workdir and handing them to the registered partial-sync delegate instead of mutating `MithrilBootstrapService` state.
- Extended the coordinator lock so partial sync is mutually exclusive with bootstrap and chain-storage mutations in both directions while preserving bootstrap's existing non-empty managed-chain guard.
- Fixed the implementation-review regression by enforcing the strict `STOPPED` guard before any partial-sync layout normalization begins.
- Focused Jest verification passed in `source/main/utils/chainStorageCoordinator.spec.ts` with 31 tests covering preflight context handoff, strict node-state rejection before layout/workdir work, recovery-fallback rejection, symmetric bootstrap/partial-sync exclusion, prompt cancel delegation, and mutation blocking.
- Ad hoc file-list `tsc` invocation was not a truthful focused verification signal in this repo because it pulled in unrelated repo-wide TypeScript/decorator configuration failures outside task-102 touched files.
- Implementation review approved in `.agent/plans/mithril-partial-sync/task-plans/task-102-impl-review.md`.

## Self-Review

- Scope-creep check: kept range derivation, service orchestration, startup suppression, and renderer work out of `task-102`.
- Required-fields check: included task ID/title, why now, interaction mode, scope, non-goals, dependencies, research consulted, docs/workflows/skills consulted, expected files, implementation approach, acceptance criteria, verification plan, risks/open questions, docs/tracking updates, review-log paths, planning status, and build status.
- Consistency check: matched the live coordinator pattern, the staged-only restore decision, the fail-closed recovery-fallback rule, and the known compile/test constraints.
