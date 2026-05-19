# Task 100 Shared Contract Notes

## Status

- State: complete
- Task: `task-100`
- Purpose: record the durable contract decisions from the first shared partial-sync type layer implementation

## Durable Decisions

- Partial sync shared contracts now live in `source/common/types/mithril-partial-sync.types.ts` instead of broadening `source/common/types/mithril-bootstrap.types.ts`.
- Bootstrap-specific lifecycle unions remain bootstrap-only so startup bootstrap and diagnostics partial sync do not silently share incompatible status semantics.
- `MithrilPartialSyncStatusUpdate.allowedRecoveryActions` is required on every update.
- Producers must emit `allowedRecoveryActions: []` whenever no recovery action is currently safe.
- This explicit empty-array rule is important because the existing Mithril renderer store pattern is merge-based and optional fields otherwise leave stale values behind.

## Shared Lifecycle Groupings

- Partial-sync terminal statuses are `completed`, `failed`, and `cancelled`.
- Partial-sync restore-complete statuses are `completed` and `starting-node`.
- Partial-sync node-start-blocking statuses stop at `starting-node`; `completed` is terminal success and must not continue suppressing normal node-start behavior.

## Scope Boundary Preserved

- Task-100 intentionally stopped at the shared contract layer.
- It did not add IPC channels, bootstrap contract widening, coordinator wiring, or renderer consumers.
- `ChainStorageConfig` and `ChainStorageValidation` remained in `mithril-bootstrap.types.ts` because they are still imported broadly and moving them here would have widened task scope without helping partial-sync safety.

## Verification Notes

- Repo-wide `yarn compile` is currently blocked by unrelated pre-existing Trezor typing failures in:
  - `source/main/ipc/getHardwareWalletChannel.ts`
  - `source/main/trezor/manifest.ts`
- Task-100 verification therefore used targeted typechecking for:
  - `source/common/types/mithril-bootstrap.types.ts`
  - `source/common/types/mithril-partial-sync.types.ts`
- Later tasks that begin consuming the new helpers should add focused coverage so lifecycle grouping drift is caught closer to usage.
