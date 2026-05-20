# Task 200 Partial Sync Service Skeleton Notes

## Status

- State: complete
- Task: `task-200`
- Purpose: record the durable service, latest-resolution, and logging seams established before range derivation and restore/install work land

## Durable Decisions

- `source/main/mithril/MithrilPartialSyncService.ts` is now the dedicated backend service boundary for diagnostics-launched partial sync; bootstrap and partial sync remain separate classes.
- The service owns partial-sync runtime state directly:
  - cached status
  - status emitter
  - current child-process reference
  - dedicated log path
- Partial-sync logging now uses `mithril-partial-sync.log` through a minimal `logFileName` seam in `mithrilCommandRunner.ts`; bootstrap logging remains on `mithril-bootstrap.log`.
- `source/main/ipc/mithrilPartialSyncChannel.ts` now registers the service once through `chainStorageCoordinator.setPartialSyncHandlers(...)` and delegates start/cancel requests through the existing coordinator seam.

## Latest Snapshot Resolution Rule

- Task-200 intentionally kept latest-snapshot metadata backend-owned and internal to the service; no shared contract expansion was needed yet.
- The implemented latest lookup path is bounded and live-supported rather than optimistic:
  - first try `cardano-db snapshot show latest --json`
  - if that fails or yields no usable metadata, fall back to `cardano-db snapshot list --json`
  - choose the newest usable item by `createdAt`
- If no usable latest metadata can be resolved, the service fails in `preparing` with a truthful partial-sync log path instead of guessing or proceeding.

## Skeleton Boundary Preserved

- Task-200 stops after latest resolution with the explicit preparing-stage error code `PARTIAL_SYNC_NOT_READY`.
- This is intentional boundary preservation, not a partial happy path:
  - no immutable range derivation yet
  - no staging directory prep yet
  - no download/verification yet
  - no LSM conversion or cutover yet
  - no recovery-action implementation yet

## Verification Notes

- Focused Jest coverage passed for:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/mithril/mithrilCommandRunner.spec.ts`
- The tests lock:
  - latest lookup fallback from `show latest` to snapshot list
  - preparing-stage bounded failure with the partial-sync log path
  - distinct partial-sync log-file usage
  - coordinator delegation for start/cancel IPC requests

## Next Task Handoff

- `task-201` should extend `MithrilPartialSyncService` in place for immutable range derivation, preflight checks, and staging preparation instead of creating a second backend entrypoint.
- When later tasks start using `runBinary(...)` for partial sync, add focused coverage for the custom log filename on that path too.
- If shared snapshot-normalization or JSON-parse helpers become worth deduplicating later, keep that extraction small and avoid blurring bootstrap and partial-sync safety semantics.
