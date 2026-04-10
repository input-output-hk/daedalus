# Implementation Ready: Chain Subdirectory Safety

## Status

This document records the approved pre-implementation plan and design handoff for the chain-storage-subdirectory-safety feature. It should be treated as the durable implementation checkpoint for coding and for final documentation closure.

## Problem Statement

When a user selects a custom chain storage directory today, Daedalus uses that directory directly as both the `stateDir/chain` symlink target and the Mithril work directory. As a result, wipe and install operations can delete or repurpose unrelated user files located under the selected directory. The approved fix introduces a managed `chain` subdirectory safety pattern so users select a parent directory while Daedalus manages only `<parent>/chain` for blockchain data.

Current baseline behavior is visible in `source/main/utils/chainStorageManager.ts:43-111`, `source/main/utils/chainStoragePathResolver.ts:33-79`, `source/main/utils/handleDiskSpace.ts:113-118`, `source/main/ipc/mithrilBootstrapChannel.ts:109-162`, and `source/main/mithril/MithrilBootstrapService.ts:283-290` plus `source/main/mithril/MithrilBootstrapService.ts:485-501` and `source/main/mithril/MithrilBootstrapService.ts:757-807`.

## Approved Plan

### 1. Define storage model and on-disk schema

Scope:

- Define the managed chain-subdirectory layout so the user selection is treated as a parent directory.
- Keep `customPath` in `chain-storage-config.json` as the user-selected parent directory.
- Derive the managed chain path as `<customPath>/chain` at runtime instead of persisting that child path as `customPath`.

Acceptance criteria:

- `customPath` persists the user-selected parent directory.
- The derived managed chain path is `<customPath>/chain`.

### 2. Create a process-wide chain-storage coordinator with a serialized mutation lock

Scope:

- Replace duplicated manager and Mithril service ownership with a single process-wide coordination point.
- Ensure all storage mutations are serialized through one lock or equivalent mutation queue.
- Provide one authoritative source for resolved parent path, managed chain path, and Mithril work directory.

Acceptance criteria:

- Startup flow and Mithril IPC flow both use the same coordinator instance.
- Concurrent storage mutations are prevented by serialized execution.
- Consumers read the same derived paths and config state.

### 3a. Define legacy-layout detection and normalization rules

Scope:

- Detect current installations where the selected custom directory is itself the active chain root.
- Distinguish between default layout, legacy direct-target custom layout, already-managed `<parent>/chain` layout, and broken-link cases.
- Normalize path interpretation before migration or destructive operations run.

Acceptance criteria:

- Legacy direct-target installs are detected reliably.
- Already-managed layouts are recognized without re-migrating them.
- Broken or inconsistent layouts are surfaced to recovery logic instead of being silently wiped.

### 3b. Add migration preflight and journaling

Scope:

- Verify space and destination preconditions before any migration begins.
- Create a migration journal with explicit states for start, progress, cutover, rollback, and completion.
- Refuse to begin migration unless the node is in a safe state for file moves.

Acceptance criteria:

- Migration preflight verifies capacity and destination suitability.
- Journal state is persisted before destructive work starts.
- Migration cannot begin unless `cardano-node` is in `STOPPED` state.

### 3c. Execute migration with atomic cutover and rollback

Scope:

- Migrate legacy layouts entry by entry into the managed `chain` subdirectory.
- Perform a final atomic cutover for the visible `stateDir/chain` entry point.
- Roll back to the prior stable state if migration fails before completion.

Acceptance criteria:

- Legacy data is moved into the managed chain subdirectory without mixing unrelated parent files into the chain directory.
- The final cutover leaves `stateDir/chain` pointing at the managed chain subdirectory.
- Failure before completion leaves the app in a recoverable, journaled state.

### 3d. Recover interrupted migration and adopt pre-existing managed paths

Scope:

- Resume or roll back from interrupted journal states after restart.
- Adopt pre-existing managed `<parent>/chain` layouts when they already match the managed-subdirectory model.
- Avoid destructive reinitialization when a valid managed path already exists.

Acceptance criteria:

- Restart after an interrupted migration follows journal recovery logic.
- Pre-existing valid managed layouts are adopted without duplicate migration.
- Recovery logic is idempotent across repeated restarts.

### 4. Resolve Mithril `_workDir` conflicts

Scope:

- Place Mithril `_workDir` inside the managed chain subdirectory.
- Keep Mithril staging, install, and cleanup targets co-located inside the managed chain subdirectory.
- Remove any remaining behavior that treats the selected parent root as a Mithril working area.

Acceptance criteria:

- Mithril staging targets the managed `chain` subdirectory only.
- Snapshot installation targets the managed `chain` subdirectory only.
- Cleanup routines operate only inside the managed `chain` subdirectory and do not empty the selected parent directory.

### 5. Gate startup and emptiness checks through the coordinator

Scope:

- Move startup emptiness checks and bootstrap decisions onto the shared coordinator.
- Base empty-chain detection on the managed chain subdirectory rather than legacy assumptions.
- Ensure preflight, migration, and path verification occur before destructive startup behavior.

Acceptance criteria:

- Startup uses the coordinated managed-chain path for emptiness checks.
- Migration or recovery runs before any destructive wipe or bootstrap install path.
- Startup behavior is consistent between default storage and custom parent-directory storage.

### 6. Make wipe semantics explicit per caller

Scope:

- Separate "unlink entry point", "remove managed directory", and "empty managed contents" operations.
- Require callers to choose the exact destructive behavior they need.
- Prevent ambiguous wipes that silently follow symlinks or junctions into user-selected parents.

Acceptance criteria:

- Each destructive caller declares whether it is unlinking, removing, or emptying.
- No caller relies on implicit symlink-following behavior for destructive operations.
- Wipe helpers operate only on managed targets defined by the coordinator.

### 7. Update validation rules for parent-directory selection

Scope:

- Validate the user-selected parent directory rather than the current direct chain root model.
- Detect conflicts where `<parent>/chain` already exists as a file.
- Preserve disk-space and inside-state-dir protections while applying them to the parent-directory model.

Acceptance criteria:

- Validation evaluates the parent selection and derived managed child together.
- A conflicting `<parent>/chain` file produces a dedicated invalid result.
- Validation still blocks unwritable, missing, inside-state-dir, and insufficient-space cases where applicable.

### 8. Preserve the renderer and IPC contract shape

Scope:

- Keep the renderer-driven storage-selection flow IPC-based.
- Avoid layout or structural IPC churn beyond the approved additions needed for the new validation state.
- Preserve current page flow while updating the meaning of the selected directory.

Acceptance criteria:

- Renderer continues to obtain config and validation via IPC.
- Existing selection flow remains intact from the user perspective.
- Any new contract surface is limited to the approved validation/status additions.

### 9. Sweep remaining hardcoded `stateDir/chain` assumptions

Scope:

- Review all startup, Mithril, selfnode, and utility paths that currently hardcode `stateDir/chain`.
- Replace unsafe assumptions with coordinator-derived paths and explicit wipe semantics.
- Include both bootstrap-time and post-bootstrap call sites.

Acceptance criteria:

- Hardcoded `stateDir/chain` assumptions are removed or intentionally retained only where they remain safe entry-point references.
- The sweep explicitly covers `MithrilBootstrapService._installSnapshot()` and `MithrilBootstrapService.wipeChainAndSnapshots()`.
- Selfnode and startup utilities remain compatible with the managed-subdirectory model.

### 10. Expand automated test coverage

Scope:

- Add unit and integration coverage for validation, migration, recovery, Mithril install semantics, and coordinator serialization.
- Cover platform-specific behavior such as Windows junction handling.
- Verify interrupted-migration recovery.

Acceptance criteria:

- Tests cover both default layout and custom parent-directory layout.
- Migration, recovery, and destructive-operation semantics are exercised with failure scenarios.
- Windows junction and broken-symlink cases are covered where practical.

### 11. Update documentation

Scope:

- Update technical and UX documentation to describe the new parent-directory model.
- Preserve research findings and implementation-ready decisions as durable artifacts.
- Ensure final docs explain migration and validation behavior clearly.

Acceptance criteria:

- Final docs describe the managed `chain` subdirectory model accurately.
- Migration, recovery, validation, and Mithril path semantics are documented.
- This pre-implementation checkpoint remains consistent with final implementation docs.

## Key Design Decisions

- `customPath` in configuration represents the user-selected parent directory.
- Actual chain data lives at `<parent>/chain`.
- The managed child path is derived at runtime from `customPath` and is never persisted as the semantic equivalent of the current direct-target `customPath`.
- Mithril `_workDir` is placed inside the managed chain subdirectory so staging, install, and cleanup stay co-located there.
- All consumers share one `ChainStorageManager`-style singleton or equivalent coordinator with serialized mutations.
- Migration uses journaling for crash recovery.
- Lock and log files remain at `stateDir/Logs`.

## Approved Design

### UI scope

- No layout or structural UI changes are needed.
- The approved UI change is a copy and validation addendum layered onto the existing picker flow.
- The new help text belongs below `.storageInputWrapper` and above `.resetActionRow` when `effectiveSelection.path != null`.
- The help text uses the `.storageSubtext` class and the DOM id `chain-storage-help-text`.

Existing picker structure is in `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx:281-338`, existing subtext styling is in `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.scss:114`, and current validation messaging is derived in `source/renderer/app/components/chain-storage/chainStorageUtils.ts:22-41` and `source/renderer/app/components/chain-storage/ChainStorage.messages.ts:74-103`.

### Decision table

| Condition | Message | Renders In | isValid |
| --- | --- | --- | --- |
| `chain` does not exist | `subdirectoryCreationNotice` | `.storageSubtext` paragraph | `true` |
| `chain` exists as directory | `subdirectoryWarningExists` | `.storageSubtext` paragraph | `true` |
| `chain` exists as file | `subdirectoryErrorConflict` | `.validationMessage` box | `false` |

### Validation and accessibility rules

- Add one new validation reason: `path-is-file` with `isValid: false`.
- Add the i18n keys `subdirectoryCreationNotice`, `subdirectoryWarningExists`, and `subdirectoryErrorConflict`.
- Migration UX remains silent and fast. No progress UI is added for migration.
- `aria-describedby` must compose the existing validation message id with `chain-storage-help-text` using a space-separated value when both are present.

## Advisory Notes From Auditors

- Task 9 must explicitly cover the hardcoded `stateDirectoryPath` usage in `MithrilBootstrapService._installSnapshot()` and `MithrilBootstrapService.wipeChainAndSnapshots()`.
- Task 3c migration must not begin unless `cardano-node` is in `STOPPED` state.
- The implementation should add a `chainSubdirectoryStatus` field to the `ChainStorageValidation` type so the renderer can distinguish the approved help-text and conflict cases cleanly.

## Edge Cases To Cover

- Legacy normalization for direct-target custom layouts.
- Mixed user files in the selected parent directory.
- Pre-existing `chain` directory under the selected parent.
- Reset-to-default behavior after migration.
- Switching between custom storage locations.
- Mithril `workDir` placement inside the managed chain subdirectory.
- Snapshot install behavior when the managed chain subdirectory already exists.
- Wipe behavior reuse across startup and Mithril call sites.
- Selfnode compatibility.
- Windows junction behavior.
- Cross-device migration.
- Broken symlink or junction verification.
- Interrupted migration recovery.
- Stability of lock and log placement under `stateDir/Logs`.

## Implementation Notes For Final Documentation Closure

- Final technical documentation should reuse this checkpoint together with the research-findings artifact.
- Final docs should describe the parent-directory selection model, migration behavior, Mithril co-location within the managed chain subdirectory, and explicit destructive semantics in terms of the implemented coordinator and storage model.