# Mithril bootstrap client notes

## Sources
- https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node
- https://mithril.network/doc/manual/getting-started/network-configurations

## CLI behavior reminders
- `mithril-client cardano-db snapshot list --json` returns a JSON array of snapshot metadata.
- `mithril-client cardano-db snapshot show <digest> --json` returns a JSON object with details.
- `mithril-client cardano-db snapshot download --include-ancillary <digest> --json` emits JSON progress lines on stdout when `--json` is used.
- After restore, conversion to LMDB flavor uses:
  `mithril-client tools utxo-hd snapshot-converter --db-directory <db> --cardano-node-version <nodeVersion> --utxo-hd-flavor LMDB --commit`.

## Network config values
- Mainnet: `https://aggregator.release-mainnet.api.mithril.network/aggregator`
- Preprod: `https://aggregator.release-preprod.api.mithril.network/aggregator`
- Preview: `https://aggregator.pre-release-preview.api.mithril.network/aggregator`
- Genesis/ancillary verification keys are hosted under the same `mithril-infra/configuration/<network>` path.

## Notes
- `GENESIS_VERIFICATION_KEY` and `ANCILLARY_VERIFICATION_KEY` in Mithril docs are shown as URLs; the CLI accepts URLs and fetches keys when those env vars are set.
- `--wipe-chain` flow now supports LauncherConfig (`wipeChain`), `DAEDALUS_WIPE_CHAIN=true`, or CLI `--wipe-chain` in that precedence order; `wipeChain` defaults to false in the launcher config.
- Preview env got blocked by Nix flakes treating `nixpkgs` as non-flake. For manual testing, we temporarily set `flake.nix` inputs:
  - `nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin"` and `nixpkgs.flake = true`
  - `mithril.flake = true` (required because `nix/internal/common.nix` expects `mithrilFlake.packages`)
- With nixpkgs 23.11, build failed due to `pkgconfig` rename; fix by replacing `pkgconfig` with `pkg-config` in:
  - `nix/devshells.nix`
  - `nix/internal/x86_64-linux.nix`
  - `nix/internal/x86_64-windows.nix`
  - `nix/internal/any-darwin.nix`
- Nix dev shell failed copying empty bin globs in `nix/internal/cardano-bridge.nix`. Fix by adding `|| true` to the `cp` lines for:
  - `${cardano-wallet}`, `${cardano-address}`, `${cardano-launcher}`, `${cardano-node}`, `${cardano-cli}`, `${mithril-client}`
- Electron dev on Linux may fail with SUID sandbox error; use `ELECTRON_DISABLE_SANDBOX=1 yarn start:dev` (optionally `--no-sandbox`).
- `launcherConfig.wipeChain` default in nix launcher config overrides env; remove default or ensure precedence favors env/argv.
- Mithril `cardano-db` subcommands differ by client version; detect whether `download` is under `cardano-db` or `cardano-db snapshot`.
- Verification keys are fetched from mithril-infra URLs and must be converted from JSON byte arrays into hex strings for `mithril-client` env vars.
- Snapshot download progress includes `files_downloaded/files_total` and `seconds_elapsed/seconds_left` fields.
- Mithril manual Step 4 shows `cardano-db download` already checks disk info, fetches the certificate, validates the certificate chain, downloads/unpacks the DB, computes the Cardano DB message, and verifies the signature before returning success.
- Shared IPC type now carries optional `filesDownloaded/filesTotal` on `MithrilBootstrapStatusUpdate` and optional `error.stage` (`download|verify|convert|node-start`) on `MithrilBootstrapError` for staged UX without breaking existing producers.
- `parseMithrilProgressUpdate` now preserves raw `files_downloaded/files_total` as `filesDownloaded/filesTotal` while still deriving percentage progress when totals are valid.
- In `MithrilBootstrapService`, download progress status updates now include `filesDownloaded/filesTotal`, and stage-aware errors are thrown/normalized so `startBootstrap` catch handling does not lose `error.stage` metadata.
- `handleDiskSpace` now emits Mithril startup failures with `error.stage = 'node-start'` when cardano-node fails to start after bootstrap completion.
- `MithrilBootstrapService.spec.ts` now mocks `../config`, `../environment`, and `../utils/logging` so service-level tests run outside nix-shell while verifying file-count and staged-error behavior.
- Shared chain-storage contracts now live in `mithril-bootstrap.types.ts`: `ChainStorageConfig` now carries `customPath`, `defaultPath`, `availableSpaceBytes`, `requiredSpaceBytes`, and optional `setAt`; `ChainStorageValidation` still carries `isValid`, `path`, and optional `resolvedPath/space metrics/reason/message` with canonical reasons (`not-writable`, `insufficient-space`, `inside-state-dir`, `path-not-found`, `unknown`).
- Added shared IPC channels in `common/ipc/api.ts`: `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL` and `GET_CHAIN_STORAGE_DIRECTORY_CHANNEL`, plus renderer/main channel wrappers.
- `ChainStorageManager` now backs chain-storage IPC validation/config reads: validates existence, directory type, writability, free space (`DISK_SPACE_REQUIRED`), rejects targets inside `stateDir`, and `getConfig()` returns default-path + disk-space metadata even when no custom path is configured.
- `ChainStorageManager.setDirectory()` is now a pure repoint for empty/default chain storage: it validates the requested target, recreates `{stateDir}/chain` as a symlink/junction, and persists config timestamp without migrating existing chain contents into the new location.
- `ChainStorageManager.migrateData()` moves directory entries with `fs.move(..., { overwrite: true })` and falls back to `copy + remove` on `EXDEV`, then removes the source directory after successful entry migration.
- `SET_CHAIN_STORAGE_DIRECTORY_CHANNEL` now executes `setDirectory()` instead of validate-only behavior, and unit coverage lives in `source/main/utils/chainStorageManager.spec.ts`.
- `setDirectory(null)` now routes to `resetToDefault()`: removes any chain symlink, recreates the default `{stateDir}/chain` directory, removes `chain-storage-config.json`, and does not migrate data back from previously selected custom locations.
- `verifySymlink()` now compares configured custom path to `{stateDir}/chain` symlink target and returns validation diagnostics for missing/mismatched/broken targets.
- Added `VALIDATE_CHAIN_STORAGE_DIRECTORY_CHANNEL` so UI can request chain-directory validation without invoking `setDirectory()` migration side effects.
- Selecting the default path explicitly is treated as a valid reset-to-default choice, and validation also succeeds when the Daedalus state directory has not been created yet by checking the future parent path for disk space.
- If the default local `{stateDir}/chain` directory already contains data, switching to a custom location is rejected instead of moving or implicitly deleting that data.
- `handleDiskSpace` now resolves the chain storage real path before disk-space checks, so free-space polling follows custom chain symlink targets instead of always using `stateDir`.
- `mithrilBootstrapChannel` now syncs `MithrilBootstrapService` workDir from `ChainStorageManager.resolveMithrilWorkDir()` before start/cancel operations.
- `MithrilBootstrapService._installSnapshot()` now compares resolved paths and preserves symlinked `{stateDir}/chain` targets by moving DB contents into the resolved target directory instead of deleting the symlink.
- The current Daedalus progress mapping squeezes Mithril CLI download progress into 10-90%, then performs `_installSnapshot()` as the real post-download local move/copy into `chain`; that local install work is the likely cause of the visible 90% plateau, especially with custom chain storage targets.
- Task 021a implementation now emits explicit `installing` and `finalizing` statuses after download completion and clears `filesDownloaded/filesTotal/elapsedSeconds/remainingSeconds` when network transfer ends so renderer progress metadata cannot linger into local post-download work.
- Follow-up task-024a should collapse the visible Mithril flow to `preparing -> downloading -> finalizing`, with finalizing covering install plus cleanup/handoff and no separate verify step in the progress UX.
- Follow-up task-024b should rename install-related post-download states/messages to `unpacking` where the service still distinguishes that work internally.
- Follow-up task-024c should remove raw `currentStep` display transport and let renderer components derive localized step labels directly from `status`.
- Added focused install-path tests in `source/main/mithril/MithrilBootstrapService.install.spec.ts` and expanded `ChainStorageManager.spec.ts` with path/workDir resolution coverage.
- `ChainStorageManager.setDirectory()` now snapshots previous chain/config state and performs rollback on failure by restoring the previous symlink/default-dir shape plus persisted config, without attempting to migrate data out of the newly targeted location.
- Review follow-up captured as low-priority task: `handleDiskSpace.ts` has legacy lint debt (`@ts-ignore`, broad `any`, and a shadowed `path` parameter) worth cleanup in a dedicated safe refactor pass.
- Startup path now invokes `ChainStorageManager.verifySymlink()` during main IPC chain-storage initialization and logs warnings when the configured custom target is missing or mismatched.
- `handleDiskSpace.ts` now has typed disk-report `Promise.race` handling and logger calls that satisfy the current lint rules without behavior changes.
- Added `source/main/ipc/chainStorageChannel.spec.ts` with startup verification tests that mock `ChainStorageManager` and assert warning logs for invalid and rejected verification paths.
- After bootstrap, move the snapshot database from `stateDir/db` into `stateDir/chain` so cardano-node uses it.
- Cardano node should auto-start after Mithril bootstrap completion (don’t wait for disk check polling).
- Added `parseMithrilProgressLine` helper to keep progress parsing isolated from `MithrilBootstrapService` so Jest tests can run without requiring nix-shell.
- Added a Mithril retry path that can optionally wipe `stateDir/chain` and Mithril snapshot artifacts when the node fails to start after bootstrap; the renderer sends `wipeChain` on retry to trigger the wipe before re-downloading.
- Added persistent Mithril decision listeners in main so decline-after-failure reliably wipes chain/snapshot artifacts and starts the node.
