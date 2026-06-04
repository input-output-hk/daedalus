# Mithril Partial Sync Research

## Summary

This note captures the repo-local findings that shape the Mithril partial sync PRD.

## Existing Daedalus Mithril Flow

- `source/main/utils/handleDiskSpace.ts` is the startup gate that decides whether to launch the existing Mithril bootstrap flow.
- `source/main/utils/chainStorageCoordinator.ts` only allows `startBootstrap()` on an empty managed chain unless `wipeChain` is explicitly set.
- `source/main/mithril/MithrilBootstrapService.ts` currently orchestrates:
  - snapshot discovery
  - full snapshot download
  - verification progress parsing
  - ledger conversion to the LSM-compatible on-disk format
  - installation into the resolved managed chain path
- `source/main/cardano/setup.ts` already suppresses generic cardano-node automatic restart behavior while Mithril bootstrap is active.

## Existing Renderer Surfaces

- `source/renderer/app/stores/NetworkStatusStore.ts` already exposes the sync data a user needs to judge whether their node is too far behind:
  - `syncProgress`
  - `localTip`
  - `networkTip`
  - `isNodeInSync`
  - `isNodeSyncing`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` and `source/renderer/app/components/status/DaedalusDiagnostics.tsx` are the best fit for a manual recovery entry point because they already surface sync data and a node restart affordance.
- The current Mithril renderer flow is startup-only and lives under:
  - `source/renderer/app/stores/MithrilBootstrapStore.ts`
  - `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx`
  - `source/renderer/app/components/loading/mithril-bootstrap/`

## Mithril Client Findings

### Bootstrap docs

- Mithril bootstrap docs describe full restore via `mithril-client cardano-db download --include-ancillary <digest>`.
- After ancillary restore, Mithril warns that the ledger snapshot is in `InMemory` format and recommends conversion before starting cardano-node.
- Daedalus already accounts for this by converting the restored ledger state to its expected on-disk format before handoff.

### Partial restore support

- Mithril client and library docs for Cardano database v2 support partial restoration by immutable range.
- The CLI source shows `cardano-db download` accepts:
  - `--start`
  - `--end`
  - `--include-ancillary`
  - `--allow-override`
- The library example and CLI internals treat partial restore as a valid first-class operation, but the Daedalus repo does not yet validate how that interacts with an already populated managed chain target.

## Key Technical Risks

1. Safe application of partial restore to an existing Daedalus chain DB is not yet validated.
2. The exact install strategy is undecided until a validation spike confirms whether live in-place mutation is safe.
3. LSM compatibility after partial restore must be revalidated, especially when ancillary data is downloaded and an existing `lsm/` directory already exists.
4. The "restart normally on existing DB" fallback is only safe if failed partial sync does not leave the live chain directory in an unknown mixed state.

## Planning Implications

- Partial sync should not be implemented as a thin extension of `startBootstrap()`.
- A separate service path, or a very strongly separated mode within Mithril orchestration, is warranted.
- A validation spike must come before feature implementation.
- The safest default plan is staged restore plus validated install/merge, not direct mutation of the live chain directory.
