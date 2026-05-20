# Task 103 Node Lifecycle Seam Notes

## Status

- State: complete
- Task: `task-103`
- Purpose: record the durable node lifecycle suppression seam for diagnostics-launched Mithril partial sync

## Durable Decisions

- The authoritative partial-sync active-state source for node lifecycle suppression is the coordinator-owned `_partialSyncInProgress` boundary exposed through `ChainStorageCoordinator.isPartialSyncInProgress()`.
- `source/main/ipc/mithrilPartialSyncChannel.ts` now owns a tiny bootstrap-style provider/accessor seam for partial-sync activity, but cached partial-sync status remains separate from that safety seam.
- `source/main/index.ts` wires the partial-sync active-state provider directly from `chainStorageCoordinator.isPartialSyncInProgress()` so crash-restart suppression stays fully main-process-owned.
- `source/main/cardano/setup.ts` now suppresses generic automatic crash restart while partial sync is active and logs the current partial-sync status distinctly from bootstrap suppression.

## Safety Boundary Preserved

- Restart suppression does not derive activity from renderer state or cached partial-sync status.
- Existing Mithril bootstrap restart suppression remains unchanged and separate.
- The task does not add service orchestration, recovery-action branching, renderer state, or shared IPC contract changes.

## Verification Notes

- Focused Jest coverage passed in:
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
  - `source/main/cardano/setup.spec.ts`
- The repo-local `understand` knowledge graph was unavailable during implementation, so live file reads and focused tests were the truthful verification source.

## Next Task Handoff

- `task-200` and later backend tasks should keep the coordinator-owned partial-sync active boundary as the source of truth for lifecycle suppression unless a later approved plan explicitly changes that contract.
- Later partial-sync status emission may become richer, but it must not silently replace the coordinator-backed safety seam with renderer-visible cached state.
