# Mithril Partial Sync From Diagnostics PRD

## Overview

Add a user-initiated Mithril partial sync flow to Daedalus so a user who judges that their node is far behind the network tip can request a faster catch-up path from `DaedalusDiagnostics`. The feature will stop `cardano-node`, restore the latest certified Mithril Cardano DB range needed to catch up, convert any restored ledger snapshot to Daedalus's LSM-compatible format, install the validated result safely, and restart the node automatically.

## Problem Statement

Daedalus currently uses Mithril only when chain storage is empty at startup. That solves the fresh-install and wiped-chain case, but it does not help users who already have chain data and later fall significantly behind the tip. Those users currently rely on normal node catch-up and replay, which can be much slower than a certified partial restore if Mithril can safely supply the missing Cardano DB range.

The current system is insufficient for this recovery use case because:

- existing Mithril orchestration is startup-only and assumes an empty managed chain target
- current UI entry points do not expose Mithril as an in-app recovery action
- Daedalus now relies on the LSM ledger backend, so restored ledger state must be validated against a stricter runtime storage expectation
- failure handling must protect live chain data, not just disposable first-run bootstrap artifacts

This work matters because it turns Mithril from a first-run accelerator into a targeted recovery tool for existing installations while preserving Daedalus's chain-storage safety guarantees.

## Goals

- Add a manual Mithril partial sync action to `DaedalusDiagnostics`
- Reuse existing sync data so users can decide for themselves when they are too far behind the tip
- Stop and restart `cardano-node` automatically around the partial sync operation
- Validate a safe partial restore strategy before implementing the feature on live chain data
- Preserve Daedalus's LSM ledger compatibility after partial restore
- Provide clear failure recovery actions: retry partial sync, restart normally, or wipe and perform full Mithril sync
- Support the same networks as current Mithril bootstrap: `mainnet`, `preprod`, and `preview`

## Non-Goals

- Auto-triggering partial sync based on sync percentage, slot lag, or time-behind heuristics
- Adding snapshot selection to the partial sync flow; partial sync always uses `latest`
- Replacing the existing empty-chain startup bootstrap flow
- Changing Daedalus's chain-storage directory selection UX as part of this feature
- Generalizing all Mithril bootstrap and partial sync state into a single implementation before the validation spike proves the live-data strategy

## Inputs And Source Material

- `.agent/system/architecture.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/plans/readme.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `source/main/mithril/MithrilBootstrapService.ts`
- `source/main/utils/chainStorageCoordinator.ts`
- `source/main/utils/handleDiskSpace.ts`
- `source/main/cardano/setup.ts`
- `source/common/ipc/api.ts`
- `source/common/types/mithril-bootstrap.types.ts`
- `source/renderer/app/stores/NetworkStatusStore.ts`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`

## Locked Planning Decisions

- Entry point is `DaedalusDiagnostics`
- UI exposes a manual button plus conditional recommendation text; no automatic trigger logic
- The user decides when they are too far behind based on surfaced sync data
- Partial sync stops and restarts `cardano-node` automatically
- Failure actions must include:
  - retry partial sync
  - restart normally on the existing DB
  - wipe chain data and perform full Mithril sync
- Supported networks are `mainnet`, `preprod`, and `preview`
- The flow opens with a small confirmation modal before background work begins
- Partial sync always uses the latest Mithril snapshot
- On success, Daedalus returns to normal app flow and existing sync/loading behavior resumes naturally
- A validation spike is mandatory before implementation touches the live chain restore path

## Requirements

### Functional Requirements

- [ ] Add a Mithril partial sync action to `DaedalusDiagnostics`
- [ ] Show recommendation copy near the action using existing sync state already surfaced in diagnostics
- [ ] Open a confirmation modal that explains automatic node stop/restart and failure recovery options
- [ ] Add a dedicated main-process partial sync orchestration path that is separate from the current empty-chain bootstrap entrypoint
- [ ] Use the latest Mithril snapshot only; do not add snapshot selection UI to partial sync
- [ ] Derive the partial immutable range from local chain state and the latest certified Mithril artifact, not from renderer sync percentage alone
- [ ] Stop `cardano-node` automatically before running Mithril partial sync
- [ ] Restore verified Cardano DB data safely for the missing immutable range
- [ ] Convert restored ledger state to a Daedalus-compatible LSM-backed layout before restarting the node
- [ ] Restart `cardano-node` automatically after successful partial sync
- [ ] Surface progress, cancellation, error details, and support log location during the operation
- [ ] Provide failure actions for retry, normal restart, and wipe-plus-full-Mithril-sync
- [ ] Preserve chain-storage safety for default and custom managed chain targets
- [ ] Support `mainnet`, `preprod`, and `preview`

### Non-Functional Requirements

- Live chain data safety must take precedence over restore speed; the implementation must not mutate the live chain path unsafely before the validation spike proves the strategy
- Partial sync orchestration must remain serialized with other chain-storage mutations and Mithril operations
- The feature must preserve existing cardano-node restart suppression semantics while Mithril-managed work is active
- Logs and status updates must remain sufficiently detailed for support diagnostics
- The UI must remain accessible and consistent with existing Mithril overlay patterns
- The implementation must remain compatible with custom chain-storage redirection managed by `ChainStorageManager`
- The rollout must include a fast disable path if post-implementation QA uncovers safety issues

## Technical Design

### Components Affected

- `source/main/mithril/`: new partial sync orchestration service and any reusable Mithril helpers
- `source/main/ipc/`: new partial sync IPC handler channel(s)
- `source/main/utils/chainStorageCoordinator.ts`: partial sync coordinator APIs and serialization rules
- `source/main/utils/chainStorageManager.ts` and related layout helpers: validated staged cutover behavior for the locked top-level install allowlist
- `source/main/cardano/setup.ts`: suppress generic crash-restart behavior while partial sync is active
- `source/common/ipc/api.ts`: new IPC channel contracts
- `source/common/types/`: shared partial sync status and error contracts, or a safe generalization of existing Mithril contracts
- `source/renderer/app/ipc/`: partial sync renderer channel wrappers
- `source/renderer/app/stores/`: new partial sync store and store registration
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`: recommendation copy and action button
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`: pass partial sync actions and store state into the diagnostics dialog
- `source/renderer/app/components/` and app-shell wiring: confirmation modal plus progress/error overlay reuse
- `tests/` and `*.spec.ts[x]`: main-process, IPC, store, and renderer verification

### Data / IPC / API Changes

Add a dedicated partial sync IPC surface instead of overloading bootstrap channels.

Recommended channels:

- `MITHRIL_PARTIAL_SYNC_START_CHANNEL`
- `MITHRIL_PARTIAL_SYNC_STATUS_CHANNEL`
- `MITHRIL_PARTIAL_SYNC_CANCEL_CHANNEL`
- `MITHRIL_PARTIAL_SYNC_RESTART_NORMAL_CHANNEL`
- `MITHRIL_PARTIAL_SYNC_WIPE_AND_FULL_SYNC_CHANNEL`

Recommended shared data contracts:

- `MithrilPartialSyncStatus`
  - `idle`
  - `confirming`
  - `stopping-node`
  - `preparing`
  - `downloading`
  - `verifying`
  - `converting`
  - `installing`
  - `finalizing`
  - `starting-node`
  - `completed`
  - `failed`
  - `cancelled`
- `MithrilPartialSyncStatusUpdate`
  - `status`
  - `error`
  - `filesDownloaded`
  - `filesTotal`
  - `ancillaryBytesDownloaded`
  - `ancillaryBytesTotal`
  - `progressItems`
  - `elapsedSeconds`
  - `logPath`
- `MithrilPartialSyncFailureAction`
  - `retry`
  - `restart-normal`
  - `wipe-and-full-sync`

The partial sync start request should not require user-supplied snapshot metadata or thresholds. It should resolve `latest` internally and derive the immutable file range from current local chain state plus Mithril artifact metadata.

### UI / Store / Process Changes

#### Diagnostics entry point

- Add recommendation copy to `DaedalusDiagnostics` near existing sync data and restart controls
- Add a manual `Mithril Partial Sync` button
- The button should be disabled while any Mithril-managed operation is already active

#### Confirmation modal

- Open a small modal before any node lifecycle change occurs
- Explain:
  - Daedalus will stop the node automatically
  - Daedalus will download verified data from Mithril
  - Daedalus will restart the node automatically on success
  - If the attempt fails, the user can retry, restart normally, or wipe and do full Mithril sync

#### Store design

- Add `MithrilPartialSyncStore` instead of forcing the current bootstrap store to carry both flows immediately
- Register the store in `source/renderer/app/stores/index.ts`
- Add app-shell or dialog wiring so the partial sync progress/error overlay can appear outside the startup-only loading route

#### Progress and error UI reuse

- Reuse existing Mithril progress and error components where practical
- Do not reuse bootstrap-only views such as:
  - storage location picker
  - snapshot selection UI
  - startup accept/decline copy

### Restore Strategy

The restore strategy is locked from spike evidence.

Approved strategy:

1. Resolve the managed chain target and ensure the node is stopped
2. Determine the local immutable position and the latest Mithril certified immutable position
3. Download the missing range into a staging area under the resolved Mithril work directory
4. Verify the restored range and ancillary data
5. Convert restored ledger state to an LSM-compatible layout
6. Cut over by emptying the managed chain target and reinstalling only validated staged entries from the fixed allowlist: `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`
7. Restart the node only after install succeeds and cleanup completes

Locked cutover rules:

- Partial restore never targets the live managed chain directory directly.
- `--allow-override` is accepted only for Daedalus-controlled staging directories, not as proof that live-target mutation is safe.
- No live top-level entry is merged or preserved across cutover.
- Existing live `volatile/` is discarded during cutover and must be recreated by cardano-node after restart rather than merged with staged state.
- Any unexpected staged top-level entry, including `volatile/`, is a validation failure and blocks install.

Rejected alternative:

- Direct in-place restore into the populated managed chain target via `--allow-override` is rejected because the spike proved only scratch-target overwrite semantics, while cancellation leaves partial artifacts and did not prove live-chain safety.

### Validation Spike Scope

Before implementation, confirm the following against the Mithril version Daedalus ships or the exact version it will ship for this feature:

1. Exact `mithril-client cardano-db download` semantics for partial restore with `--start`, `--end`, `--include-ancillary`, and `--allow-override`
2. Whether partial restore can safely target an existing DB layout without wiping unrelated state
3. Whether ancillary download is required for meaningful catch-up speed and compatible restart behavior
4. Whether restored ancillary ledger state still needs the same LSM conversion path Daedalus uses for full bootstrap
5. Which top-level DB artifacts can be installed from staging and which must never be merged through implicitly:
   - `clean`
   - `immutable`
   - `ledger`
   - `lsm`
   - `protocolMagicId`
   - `volatile`
6. Whether cancellation or restore failure leaves the destination usable for a normal restart without rollback

The spike is a gate, not optional polish. The PRD, tasks graph, and spike-results research note must be updated with the selected strategy before code implementation proceeds past the service skeleton and contract work.

## Implementation Strategy

1. Run the validation spike and record the exact safe restore/install strategy for live Daedalus chain data, including the fixed cutover allowlist.
2. Add shared types, IPC contracts, and coordinator APIs for a separate partial sync path.
3. Implement backend orchestration for latest-snapshot selection, immutable-range derivation, staging, verification, LSM conversion, and the locked staged cutover rule.
4. Add renderer store wiring, diagnostics CTA, confirmation modal, and progress/error overlay integration.
5. Add automated tests across main-process orchestration, IPC, store, and renderer flows.
6. Run supported-network manual QA and finalize rollout and rollback posture based on those results.

## Testing Strategy

Validation should combine automated coverage with late-phase manual QA.

- **Validation spike**
  - Confirm restore semantics, artifact layout, LSM conversion needs, and failure containment before implementation finalization
- **Jest / main-process tests**
  - Service orchestration
  - coordinator guards and mutation serialization
  - node stop/start handoff
  - cancellation and failure branching
  - install allowlist and cutover preconditions
- **Jest / renderer tests**
  - diagnostics CTA visibility and disabled states
  - confirmation modal behavior
  - partial sync store status transitions
  - progress and error action rendering
- **IPC tests**
  - channel contract round-trips for start, cancel, restart-normal, and wipe-full-sync actions
- **Storybook / visual coverage**
  - confirmation modal
  - progress overlay states
  - error states for each failure option
- **Manual QA**
  - supported networks: `mainnet`, `preprod`, `preview`
  - default chain storage and custom chain storage targets
  - success path, cancellation path, retry path, restart-normal path, wipe-and-full-sync path
  - Windows, macOS, and Linux verification after automated coverage is complete

## Rollout / Migration / Rollback

- No persisted data migration is required beyond any shared status-store additions.
- Because the feature operates on live chain data, rollout should include a fast disable path such as a temporary launcher or environment kill switch until QA confidence is high.
- Rollback strategy:
  - disable the diagnostics entry point and partial sync IPC path
  - leave existing full bootstrap behavior unchanged
  - preserve the ability to wipe and perform full Mithril sync if partial sync proves unsafe in the field
- The existing startup bootstrap flow remains the fallback path for empty-chain recovery and must not regress.

## Open Questions

- Does the shipped Mithril binary expose all needed partial restore semantics consistently across supported platforms?
- Is a dedicated shared `mithril-partial-sync.types.ts` cleaner than broadening `mithril-bootstrap.types.ts`, or does safe reuse reduce duplication without obscuring separate invariants?
- Is a lightweight feature flag sufficient for rollout control, or should launcher config own the primary kill switch?

---

**Status:** Draft
**Date:** 2026-05-18
**Author:** OpenCode
