# Task 201 Range And Staging Notes

## Status

- State: complete
- Task: `task-201`
- Purpose: record the durable range-derivation, preflight, and staging seams established before partial restore download work lands

## Durable Decisions

- `source/main/mithril/MithrilPartialSyncService.ts` remains the sole backend boundary for task-201 preflight work; no coordinator, IPC, or shared-contract widening was needed.
- Latest certified immutable position stays backend-local and is extracted from raw Mithril snapshot payloads instead of widening `MithrilSnapshotItem`.
- The service now supports both currently relevant latest-metadata shapes:
  - `snapshot show latest --json` with `beacon.immutable_file_number`
  - snapshot-list fallback items with `cardano_db_beacon.immutable_file_number`
- Local immutable position is derived only from the highest parseable numeric stem in the managed-chain `immutable/` directory.
- The local preflight gate is concrete and fail-closed:
  - managed chain path must be a directory
  - `immutable/` must be a readable directory
  - `protocolMagicId` must be a readable file
  - at least one parseable immutable filename must exist
  - local immutable must be strictly behind the latest certified immutable number

## Staging Rule Locked By Implementation

- Task-201 prepares staged partial-sync work under `stateDirectoryPath/mithril-partial-sync/download`.
- The staging root is cleaned before reuse so interrupted earlier attempts do not leak partial artifacts into later runs.
- The service rejects any computed staging root that resolves inside `layoutResult.managedChainPath`.
- This keeps the staged-only safety rule from the spike enforceable in code before task-202 starts writing partial restore output.

## Runtime Boundary Preserved

- Task-201 still terminates with the bounded preparing-stage `PARTIAL_SYNC_NOT_READY` failure after successful latest resolution, local range derivation, and staging preparation.
- Coordinator-owned partial-sync active-state semantics therefore remain unchanged until task-202 extends the live operation duration.

## Verification Evidence

- Focused Jest coverage passed in `source/main/mithril/MithrilPartialSyncService.spec.ts`.
- The spec now locks:
  - dual latest-metadata normalization paths
  - unreadable `immutable/` and `protocolMagicId` fail-closed behavior
  - no-certified-range rejection
  - staging cleanup
  - staging-path rejection inside the managed chain subtree

## Next Task Handoff

- `task-202` should reuse the staged download parent prepared here instead of resolving a new destination heuristically.
- Keep latest resolution and partial-range derivation adjacent to command issuance so latest-snapshot drift remains a bounded retriable concern instead of reusing stale metadata.
- If task-202 needs explicit staged-path state beyond `_activeWorkDir`, add it as narrow service-local state rather than widening shared contracts.
