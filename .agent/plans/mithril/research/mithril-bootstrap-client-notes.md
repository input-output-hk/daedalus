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
- Mithril decision overlay debugging notes:
  - `handleDiskSpace` originally gated on `_startupTries === 0`. Removing that condition allows decision status to emit when node is STOPPED.
- `launcherConfig.wipeChain` default in nix launcher config overrides env; remove default or ensure precedence favors env/argv.
  - Renderer store should call `syncStatus()` on setup to pick up cached status if broadcast was missed.
- Decision status must be cached in main (IPC module) so `status` request returns the decision update.
- Overlay was hidden while onboarding (`app.isSetupPage`); removing that gate allows decision screen to show during setup.
- If the user declines Mithril, emit an idle status to clear the Mithril overlay before starting node sync.
- Onboarding pages were hidden when node was stopped; adjust Root to render profile setup routes even in node-stopped state.
- If Mithril UI strings log missing i18n ids, run `yarn i18n:manage` to update `translations/messages.json`, `translations/en-US.json`, `translations/ja-JP.json`.
- Mithril `cardano-db` subcommands differ by client version; detect whether `download` is under `cardano-db` or `cardano-db snapshot`.
- Verification keys are fetched from mithril-infra URLs and must be converted from JSON byte arrays into hex strings for `mithril-client` env vars.
- Snapshot download progress includes `files_downloaded/files_total` and `seconds_elapsed/seconds_left` fields.
- Shared IPC type now carries optional `filesDownloaded/filesTotal` on `MithrilBootstrapStatusUpdate` and optional `error.stage` (`download|verify|convert|node-start`) on `MithrilBootstrapError` for staged UX without breaking existing producers.
- `parseMithrilProgressUpdate` now preserves raw `files_downloaded/files_total` as `filesDownloaded/filesTotal` while still deriving percentage progress when totals are valid.
- In `MithrilBootstrapService`, download progress status updates now include `filesDownloaded/filesTotal`, and stage-aware errors are thrown/normalized so `startBootstrap` catch handling does not lose `error.stage` metadata.
- `handleDiskSpace` now emits Mithril startup failures with `error.stage = 'node-start'` when cardano-node fails to start after bootstrap completion.
- `MithrilBootstrapService.spec.ts` now mocks `../config`, `../environment`, and `../utils/logging` so service-level tests run outside nix-shell while verifying file-count and staged-error behavior.
- `MithrilBootstrapStore` should assign optional progress fields using property presence checks (`'field' in update`) rather than nullish coalescing so explicit backend resets like `filesDownloaded: undefined` are not dropped.
- Store-side download metadata can be derived cheaply from existing payloads: `bytesDownloaded ~= snapshot.size * (filesDownloaded / filesTotal)` and `throughputBps = bytesDownloaded / elapsedSeconds`, with file counts clamped to `[0, filesTotal]` to avoid overshoot.
- `total_db_size_uncompressed` is the size field used for snapshot detail size.
- After bootstrap, move the snapshot database from `stateDir/db` into `stateDir/chain` so cardano-node uses it.
- Cardano node should auto-start after Mithril bootstrap completion (don’t wait for disk check polling).
- Snapshot selection UI now surfaces metadata (digest, size, created, node version). Created timestamps are displayed using local time formatting and fallback to raw strings when parsing fails.
- Added `parseMithrilProgressLine` helper to keep progress parsing isolated from `MithrilBootstrapService` so Jest tests can run without requiring nix-shell.
- Manual QA checklist lives in `.agent/plans/mithril/bootstrap-cardano-node.md` under the Testing Strategy section.
- Added a Mithril retry path that can optionally wipe `stateDir/chain` and Mithril snapshot artifacts when the node fails to start after bootstrap; the renderer sends `wipeChain` on retry to trigger the wipe before re-downloading.
- Added persistent Mithril decision listeners in main so decline-after-failure reliably wipes chain/snapshot artifacts and starts the node.
