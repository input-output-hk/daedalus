# Task 102 Coordinator Seam Notes

## Status

- State: complete
- Task: `task-102`
- Purpose: record the durable coordinator boundary decisions for diagnostics-launched Mithril partial sync before the real service lands

## Durable Decisions

- `ChainStorageCoordinator` now owns a distinct partial-sync seam instead of reusing `startBootstrap()`.
- The coordinator exports `PartialSyncPreflightContext` with exactly two required inputs for the next service layer:
  - `layoutResult`
  - `mithrilWorkDir`
- Partial sync delegate registration currently happens through `setPartialSyncHandlers(...)`, which keeps `task-102` coordinator-only while giving `task-200` a narrow backend-owned handoff point.
- The coordinator must resolve partial-sync workdir context explicitly and pass it to the delegate; it must not reuse bootstrap-only `_syncMithrilWorkDir()` or mutate `MithrilBootstrapService` state for partial sync.

## Safety Rules Locked By This Task

- `startPartialSync(...)` must enforce the strict `STOPPED` node-state guard before any layout normalization or fallback work begins.
- `startPartialSync(...)` must fail closed when `ensureManagedChainLayout(...)` would report `isRecoveryFallback: true`.
- Partial sync is mutually exclusive with:
  - Mithril bootstrap
  - serialized chain-storage mutations coordinated by `ChainStorageCoordinator`
- The exclusion is symmetric: bootstrap must also reject while partial sync is active.
- `cancelPartialSync()` stays prompt and delegates directly through the registered handler rather than being queued behind unrelated mutation-lock work.

## Scope Boundary Preserved

- Task-102 stopped at coordinator APIs, preflight context preparation, operation locking, and focused coordinator tests.
- It did not add:
  - real partial-sync service orchestration
  - IPC handler delegation changes
  - node restart suppression hooks
  - recovery-action coordinator APIs

## Verification Notes

- Focused Jest coverage passed in `source/main/utils/chainStorageCoordinator.spec.ts`.
- Added coverage locks:
  - explicit preflight context handoff
  - strict non-stopped rejection before layout/workdir calls
  - `isRecoveryFallback` rejection
  - symmetric bootstrap/partial-sync mutual exclusion
  - prompt cancel delegation
  - chain-storage mutation blocking while partial sync is active
- A direct file-list `tsc` invocation was not a truthful focused signal in this repo because it pulled in unrelated repo-wide TypeScript/decorator configuration failures outside task-102 touched files.

## Next Task Handoff

- `task-200` should register the real partial-sync service through the coordinator's partial-sync handler seam instead of widening bootstrap service responsibilities.
- `task-103` should consume the new active partial-sync state boundary for node lifecycle suppression without inventing renderer-owned state.
