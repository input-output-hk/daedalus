# Task 003: Define Immutable Range Derivation And Chain-Layout Preconditions

## Task

- Task ID: `task-003`
- Title: `Define immutable range derivation and chain-layout preconditions`

## Why This Task Now

- `task-001` and `task-002` already locked the staged-only restore boundary.
- `task-003` is the next backend design gate needed before later service work can safely derive a partial immutable range or decide when partial sync must be blocked.

## Interaction Mode

- `autonomous`

## Scope

- Define the backend source of truth for the current local immutable position.
- Define the immutable range derivation rule against the latest Mithril snapshot metadata.
- Define the chain-layout, readability, and node-state preconditions that must pass before partial sync starts.
- Keep the design anchored to existing chain-storage seams rather than inventing a second filesystem model.

## Non-Goals

- Implement the diagnostics entry point, IPC, or renderer behavior.
- Reopen the staged-only cutover rule from `task-002`.
- Use renderer sync percentage, slot lag, or recommendation copy in backend range calculation.
- Define rollback and failure-option policy that belongs to `task-004`.

## Relevant Dependencies

- Formal dependency: `task-001`
- Explicit safety-gate dependency: completed `task-002`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/test.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/update-doc.md`
- `understand`

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-003.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-003-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-003-impl-review.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`

## Live Repo Findings

- `source/main/utils/chainStorageManagerLayout.ts` already owns managed-layout normalization, fallback, and recovery behavior.
- `source/main/utils/chainStoragePathResolver.ts` resolves the active managed chain target from the live `stateDir/chain` entry point.
- `source/main/utils/chainStorageCoordinator.ts` already serializes chain mutations and enforces stopped-node preconditions around chain-storage work.
- `source/main/mithril/MithrilBootstrapService.ts` remains full-bootstrap-only; there is not yet a partial-sync-specific immutable range helper.
- `ensureManagedChainLayout()` can recover by falling back to default storage, which is acceptable for startup but unsafe for partial sync if used silently as the basis for range derivation.
- `ManagedChainLayoutResult.isRecoveryFallback` is the concrete live seam that later backend work must inspect and fail closed on before deriving any immutable range.

## Implementation Approach

- Keep this task docs/spec-first. Do not add product code unless later review proves a tiny helper is required immediately.
- Record one backend-only source-of-truth rule: derive local immutable position from the highest parseable immutable file number present in the resolved managed chain `immutable/` directory.
- Record one backend-only range rule: resolve latest certified Mithril immutable position from latest snapshot metadata, then request `start = localImmutable + 1` and `end = latestCertifiedImmutable`.
- Record that partial sync must not start when `localImmutable >= latestCertifiedImmutable` because no certified missing immutable range exists.
- Record that snapshot lookup and range calculation must stay close together in the backend so latest-snapshot drift is handled safely.
- Record one drift rule for later implementation: re-resolve `latest` immediately before command issuance, or fail safely on range drift and treat it as a bounded retriable preflight failure.
- Record that chain ownership and path resolution must reuse `ensureManagedChainLayout()`, `resolveChainStoragePath()`, and coordinator serialization instead of ad hoc filesystem heuristics.
- Record that `volatile/` is never an input to immutable range derivation.

## Preconditions To Start Partial Sync

- Coordinator serialization is held so partial sync cannot overlap chain-storage mutations or other Mithril work.
- `cardano-node` is stopped.
- Managed-layout normalization succeeds.
- Managed-layout normalization does not report recovery fallback, and later implementation must fail closed on `ManagedChainLayoutResult.isRecoveryFallback` before reading local immutable state.
- The resolved managed chain target is readable as a Daedalus-managed chain root.
- The resolved managed chain target contains a readable `immutable/` directory.
- The resolved managed chain target contains a readable `protocolMagicId`.
- The `immutable/` path is a readable directory.
- The `protocolMagicId` path is a readable file.
- At least one parseable immutable file number exists in `immutable/`.
- Latest Mithril snapshot metadata can be resolved for the active supported network.

## Blocking Conditions

- Managed-layout normalization falls back to default storage.
- The managed chain target is missing `immutable/`.
- The managed chain target has no parseable immutable file number.
- The `immutable/` directory is unreadable or not a directory.
- The `protocolMagicId` file is unreadable or not a file.
- Immutable filenames cannot be parsed into a trustworthy highest local immutable number.
- Latest snapshot metadata cannot be resolved truthfully.

## Acceptance Criteria

- The canonical plan names the backend source of truth for current local immutable position.
- The canonical plan states the immutable range derivation rule as `local + 1` through latest certified immutable.
- The canonical plan explicitly keeps renderer sync percentage out of backend range calculation.
- The canonical plan lists the chain-layout and node-state preconditions that must pass before partial sync starts.
- The canonical plan states only the concrete managed-root, `immutable/`, `protocolMagicId`, and parseable-filename checks that block partial sync instead of allowing heuristic fallback.
- The canonical plan makes `isRecoveryFallback` a fail-closed blocker for later implementation and downstream docs.
- The task stays within docs/spec scope and does not introduce unrelated implementation work.

## Verification Plan

- Verify the plan against live repo seams in:
  - `source/main/utils/chainStorageManager.ts`
  - `source/main/utils/chainStorageManagerLayout.ts`
  - `source/main/utils/chainStoragePathResolver.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/mithril/MithrilBootstrapService.ts`
- Verify consistency with `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`, especially latest-snapshot drift and staged-only restore assumptions.
- Verify the plan preserves the chain-storage hardening rule that `stateDir/chain` remains the authoritative entry point.
- Verify the plan propagates the `ManagedChainLayoutResult.isRecoveryFallback` fail-closed rule into the PRD, task graph, and research update.
- Verify the task remains docs/spec-first and does not pre-decide rollback policy reserved for `task-004`.

## Risks And Open Questions

- The repo does not yet contain a reusable immutable-file parser, so later implementation must keep that helper narrow and colocated with current chain-storage utilities.
- Latest-snapshot drift can invalidate a stale `end` selection, so later implementation must re-resolve `latest` immediately before command issuance or fail safely with a bounded retriable preflight error.
- The exact immutable filename completeness heuristic still needs live Cardano DB confirmation during implementation, but that does not change the higher-level source-of-truth rule.

## Required Docs, Tracking, And Research Updates

- Add the canonical task plan doc and both task-specific review logs.
- Tighten the PRD so backend-only immutable range derivation, latest-snapshot drift handling, and the `isRecoveryFallback` fail-closed rule are explicit.
- Update the tasks graph so `task-003` explicitly depends on `task-002` and later wording does not imply renderer-driven thresholds or heuristic filesystem inference.
- Add a short research addendum to preserve the durable interpretation that latest-snapshot drift and layout fallback must block unsafe range derivation.

## Review Logs

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-003-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-003-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Outcome

- Completed. `task-003` now locks the backend-only immutable range derivation and preflight contract into the project source of truth: later implementation must derive the local immutable position from the highest parseable immutable filename in the resolved managed chain `immutable/` directory, treat `ManagedChainLayoutResult.isRecoveryFallback` as a fail-closed blocker, require concrete readable `immutable/` and `protocolMagicId` checks, and handle latest-snapshot drift as a bounded backend preflight concern.
- The final approved implementation remained docs/spec-first. No product code changed in this task.
