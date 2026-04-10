# Research Findings: Chain Subdirectory Safety

## Purpose

This document records the approved pre-implementation research for the chain-storage-subdirectory-safety feature. It is intended to preserve the current-state findings that motivated the storage-layout safety change before any implementation work begins.

## Architecture Summary

- `stateDir/chain` is the stable app-visible entry point. When custom chain storage is configured, the app redirects that path through a symlink on Unix-like systems or a junction on Windows to the user-selected location rather than changing downstream consumers to a new root path directly. See `source/main/utils/chainStorageManager.ts:43-111`.
- `cardano-node` is still launched with only `stateDir`. The custom storage layout is therefore mediated through `stateDir/chain`, not through a separate node argument for chain storage. See `source/main/cardano/setup.ts:54-86` and `source/main/cardano/CardanoNode.ts:385-392`.
- There are currently two independent `ChainStorageManager` plus `MithrilBootstrapService` pairings, one in startup disk-space handling and one in the Mithril IPC channel layer. See `source/main/utils/handleDiskSpace.ts:113-118` and `source/main/ipc/mithrilBootstrapChannel.ts:109-162`.
- The renderer does not read `chain-storage-config.json` directly. Renderer code requests storage information and validation over IPC. See `source/main/ipc/chainStorageChannel.ts:51-59`, `source/renderer/app/ipc/chainStorageChannel.ts:1-28`, and `source/renderer/app/stores/MithrilBootstrapStore.ts:245-270`.
- `MithrilBootstrapService` currently mixes hardcoded `stateDirectoryPath` usage with an instance-specific `_workDir`, which makes storage semantics inconsistent once custom chain storage is enabled. See `source/main/mithril/MithrilBootstrapService.ts:88-94`, `source/main/mithril/MithrilBootstrapService.ts:283-290`, `source/main/mithril/MithrilBootstrapService.ts:485-501`, and `source/main/mithril/MithrilBootstrapService.ts:757-807`.

## Key Findings

### F1. `setDirectory()` points `stateDir/chain` at the raw selected path

`ChainStorageManager.setDirectory()` validates the selected directory, resolves it, removes the existing `stateDir/chain`, ensures the resolved target exists, creates a symlink or junction from `stateDir/chain` to that resolved target, and writes `customPath` as the user-selected path to `chain-storage-config.json`. This means the selected directory becomes the live chain root today rather than a parent that contains a managed child directory. See `source/main/utils/chainStorageManager.ts:43-111`.

### F2. `wipeChainAndSnapshots()` empties `stateDir/chain` via hardcoded `stateDirectoryPath`

`MithrilBootstrapService.wipeChainAndSnapshots()` computes `chainDir` as `path.join(stateDirectoryPath, 'chain')` and calls `fs.emptyDir(chainDir)` before snapshot cleanup. If `stateDir/chain` is a symlink or junction to a custom location, this destructive wipe follows that link into the custom root. See `source/main/mithril/MithrilBootstrapService.ts:283-290`.

### F3. `_cleanupSnapshotArtifacts()` removes `_workDir/data` and `_workDir/db`

Snapshot artifact cleanup targets `path.join(this._workDir, 'data')` and, unless `preserveDb` is set, `path.join(this._workDir, 'db')`. When `_workDir` resolves to the user-selected custom root, Mithril cleanup operates directly under that custom location. See `source/main/mithril/MithrilBootstrapService.ts:485-501`.

### F4. `_installSnapshot()` empties the resolved chain target during normal bootstrap

`_installSnapshot()` resolves `stateDir/chain`, detects when it is a symlink, calls `fs.emptyDir(resolvedChainDir)`, and then moves snapshot contents into that resolved target. Under the current layout, normal snapshot installation can therefore empty the custom target directory itself. See `source/main/mithril/MithrilBootstrapService.ts:757-807`.

### F5. `MithrilBootstrapService` mixes hardcoded `stateDirectoryPath` with instance `_workDir`

The constructor defaults `_workDir` from `stateDirectoryPath`, but later methods do not use a single path model consistently. `wipeChainAndSnapshots()` and `_getBootstrapLogPath()` derive locations from hardcoded `stateDirectoryPath`, while `_cleanupSnapshotArtifacts()`, `_runCommand()`, `_resolveDbDirectory()`, and parts of bootstrap flow use `_workDir`. That split is the core implementation inconsistency that the safety change must resolve. See `source/main/mithril/MithrilBootstrapService.ts:88-94`, `source/main/mithril/MithrilBootstrapService.ts:283-290`, `source/main/mithril/MithrilBootstrapService.ts:402-404`, `source/main/mithril/MithrilBootstrapService.ts:485-501`, `source/main/mithril/MithrilBootstrapService.ts:507-516`, `source/main/mithril/MithrilBootstrapService.ts:736-755`, and `source/main/mithril/MithrilBootstrapService.ts:757-807`.

### F6. Cardano startup still uses `stateDir` only

`setupCardanoNode()` passes `stateDir` from launcher configuration into `CardanoNode`, and the node launcher path continues to operate from that state directory. The startup path does not pass a custom chain root separately, so storage indirection must remain compatible with `stateDir/chain`. See `source/main/cardano/setup.ts:54-86` and `source/main/cardano/CardanoNode.ts:385-392`.

### F7. Two independent `ChainStorageManager` plus `MithrilBootstrapService` pairs exist

Startup disk-space handling constructs its own `ChainStorageManager` and `MithrilBootstrapService`, then synchronizes the Mithril work directory from the manager. The Mithril IPC channel layer creates another separate pair and repeats the same work-dir synchronization pattern. This duplication creates coordination risk for mutations and path resolution. See `source/main/utils/handleDiskSpace.ts:113-118` and `source/main/ipc/mithrilBootstrapChannel.ts:109-162`.

### F8. No non-main-process code reads `chain-storage-config.json`

The JSON filename is declared and read in main-process utilities (`ChainStorageManager` and the path resolver helpers), while renderer code consumes storage configuration through `getChainStorageDirectoryChannel` and validation through `validateChainStorageDirectoryChannel`. The renderer store never touches the JSON file directly. See `source/main/utils/chainStorageManager.ts:21`, `source/main/utils/chainStorageManager.ts:290-328`, `source/main/utils/chainStoragePathResolver.ts:7-79`, `source/main/ipc/chainStorageChannel.ts:51-59`, `source/renderer/app/ipc/chainStorageChannel.ts:1-28`, and `source/renderer/app/stores/MithrilBootstrapStore.ts:245-270`.

### F9. Selfnode startup removes `stateDir/chain`

The selfnode config path unconditionally computes `chainDir` as `path.join(stateDir, 'chain')` and removes it with `fs.remove(chainDir)`. Any storage-layout change must preserve safe behavior for this path too. See `source/main/cardano/utils.ts:155-159`.

## Risk Areas

- Direct symlink target risk: the current custom selection becomes the live target of `stateDir/chain`, so unrelated user files may sit in the same root as managed blockchain data.
- Destructive wipe follows the symlink or junction: startup and Mithril wipes act on `stateDir/chain` and can traverse into the custom target.
- Cleanup under custom root: Mithril artifact cleanup removes `_workDir/data` and `_workDir/db` under the resolved custom root.
- Install path empties resolved target: normal snapshot installation empties the resolved chain target when `stateDir/chain` is a symlink or junction.
- Startup wipe reuses the same path assumptions: both standard startup and Mithril flows reuse `stateDir/chain` semantics.
- No migration on reset: current config and symlink behavior do not provide a safe managed-subdirectory migration path.
- Validation scope is the selected root, not a managed child subtree: validation today accepts the selected directory itself rather than validating a parent directory that will own `<parent>/chain`.

## Key Files

### Main process

- `source/main/utils/chainStorageManager.ts`
- `source/main/utils/chainStoragePathResolver.ts`
- `source/main/utils/chainStorageValidation.ts`
- `source/main/utils/handleDiskSpace.ts`
- `source/main/mithril/MithrilBootstrapService.ts`
- `source/main/ipc/chainStorageChannel.ts`
- `source/main/ipc/mithrilBootstrapChannel.ts`
- `source/main/cardano/setup.ts`
- `source/main/cardano/CardanoNode.ts`
- `source/main/cardano/CardanoWalletLauncher.ts`
- `source/main/cardano/utils.ts`

### Renderer and shared contract

- `source/renderer/app/stores/MithrilBootstrapStore.ts`
- `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx`
- `source/renderer/app/components/chain-storage/chainStorageUtils.ts`
- `source/renderer/app/ipc/chainStorageChannel.ts`
- `source/common/types/mithril-bootstrap.types.ts`

## Research Conclusion

The current architecture treats the user-selected custom path as the chain root itself. Because multiple destructive flows operate either through `stateDir/chain` or through a Mithril `_workDir` that can resolve to that same custom root, the present layout can wipe or repurpose directories that were never meant to be fully managed by Daedalus. The approved fix should therefore introduce a managed `chain` subdirectory beneath the user-selected parent, centralize path ownership, and make wipe semantics explicit per caller.