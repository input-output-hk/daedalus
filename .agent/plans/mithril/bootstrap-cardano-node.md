# Mithril Snapshot Bootstrapping

## Overview
Improve the first-run experience by offering Mithril snapshot bootstrapping when the Cardano DB is empty. This reduces initial sync time from days to minutes by downloading and verifying a certified Cardano DB snapshot, then launching `cardano-node` against the restored DB.

## Requirements
- [x] Detect empty `stateDir/chain` on startup before `cardano-node` launches.
- [x] Interrupt automatic node startup and present a user choice: "Use Mithril Fast Sync" or "Sync from Genesis".
- [x] Sequence the Mithril decision after onboarding agreements (Profile Configuration, Terms of Service, Analytics). Acceptance state lives in `ProfileStore` (`areTermsOfUseAccepted`, `analyticsAcceptanceStatus`, `isInitialScreen`, `isCurrentLocaleSet`) with setup routing derived in `AppStore.isSetupPage`. Show the decision immediately on startup when `isSetupPage` is false; otherwise wait until onboarding completes.
- [x] For supported networks (Mainnet, Preprod, Preview), run Mithril snapshot restoration using hardcoded aggregator endpoints and verification keys.
- [x] Allow the user to select which snapshot to restore (latest by default, but list available snapshots).
- [x] Automate ledger snapshot conversion using `mithril-client tools utxo-hd snapshot-converter --commit` after restoration.
- [x] Provide a progress UI with explicit steps and error states.
- [x] If the user declines, continue with standard genesis sync. If Mithril fails, wipe `stateDir/chain`, any downloaded snapshot data, present error to the user and re-prompt.
- [x] Provide a developer/test flag to launch Daedalus with wiped chain + Mithril snapshot artifacts for consistent Mithril flow testing.
- [x] On startup, detect an incomplete Mithril restore (e.g., marker/lock file or partial snapshot artifacts), wipe `stateDir/chain`, and re-prompt the user to either retry Mithril from scratch or sync from genesis.

## Technical Design

### Components Affected
- **Nix / Build**
  - `flake.nix`: Add Mithril repo as an input for stable tag pinning.
  - `nix/internal/common.nix`: Include the `mithril-client` binary in `cardano-bridge` bundling per OS.
- **Main Process**
  - `source/main/utils/handleDiskSpace.ts`: Add a pre-start check for empty chain and a Mithril decision gate before calling `cardanoNode.start()`.
  - `source/main/mithril/MithrilBootstrapService.ts` (new): Manages `mithril-client` lifecycle, progress parsing, cancellation, and conversion.
  - `source/main/ipc/mithrilBootstrapChannel.ts` (new): IPC endpoints for bootstrapping.
- **Renderer Process**
  - `source/renderer/app/stores/MithrilBootstrapStore.ts` (new): MobX state for bootstrap progress and UI decisions.
  - `source/renderer/app/components/loading/mithril-bootstrap/` (new): Prompt + progress screen.
  - `source/renderer/app/containers/loading/LoadingPage.tsx` (existing): Integrate Mithril bootstrap state into initial flow.

### Store Changes
- `MithrilBootstrapStore` (new)
  - `status`: `idle | decision | preparing | downloading | verifying | converting | completed | failed | cancelled`
  - `progress`: `number` (0-100, step-weighted)
  - `currentStep`: `string`
  - `snapshot`: `{ digest, size, nodeVersion, createdAt } | null`
  - `error`: `{ message, code, logPath } | null`

### IPC Changes
- `MITHRIL_BOOTSTRAP_DECISION_CHANNEL`: Renderer -> Main (accept/decline).
- `MITHRIL_BOOTSTRAP_START_CHANNEL`: Renderer -> Main (start snapshot restore).
- `MITHRIL_BOOTSTRAP_STATUS_CHANNEL`: Main -> Renderer (progress + status updates).
- `MITHRIL_BOOTSTRAP_CANCEL_CHANNEL`: Renderer -> Main (cancel and cleanup).

### CLI Flow
1. `mithril-client cardano-db snapshot list --json` (populate snapshot selection list).
2. `mithril-client cardano-db snapshot show <digest> --json` (optional details).
3. `mithril-client cardano-db download --include-ancillary <digest> --json`.
4. `mithril-client tools utxo-hd snapshot-converter --db-directory <db> --cardano-node-version <nodeVersion> --utxo-hd-flavor LMDB --commit`.

### Parsing Requirements
- Snapshot list: parse JSON items into `{ digest, createdAt, size, cardanoNodeVersion, network }`.
- Download progress: parse `--json` progress events from stdout and map to step progress (download, unpack, verify).
- Conversion: parse converter stdout/stderr; treat non-zero exit as failure and surface the error.
- Persist last Mithril logs in `stateDir/Logs/mithril-bootstrap.log` for support.
- Incomplete restore marker: create `stateDir/Logs/mithril-bootstrap.lock` at bootstrap start. On success, delete the lock and cleanup downloaded snapshot artifacts. On failure, cleanup downloaded snapshot artifacts and wipe `stateDir/chain`. The lock is left behind on failure to ensure that a restart re-prompts for Mithril, but it MUST be cleared if the user subsequently chooses "Sync from genesis" or cancels the operation.

### Test/Developer Workflow
Add a dev/test-only switch with:
- CLI: `--wipe-chain`
- Env: `DAEDALUS_WIPE_CHAIN=true`
- Launcher config: `wipeChain?: boolean` in `LauncherConfig`

Resolution precedence: `launcherConfig.wipeChain ?? (process.env.DAEDALUS_WIPE_CHAIN === 'true') ?? argv.includes('--wipe-chain')`.

Launcher wiring note: extend `nix/launcher-config.nix` (and any launcher config generator) to pass `wipeChain` into `LauncherConfig` for dev/test builds.
This is now wired in `nix/internal/launcher-config.nix` with a default `wipeChain = false`.

Behavior:
- Stops `cardano-node` if running.
- Deletes `stateDir/chain` and any Mithril snapshot artifacts in `stateDir/data` and `stateDir/db`.
- Starts the app normally, triggering the Mithril flow.

### Network Configuration (Hardcoded)
Use official Mithril network configuration values. Keep these in a single map keyed by Daedalus network.

- Mainnet
  - Aggregator: `https://aggregator.release-mainnet.api.mithril.network/aggregator`
  - Genesis key: `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey`
  - Ancillary key: `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey`
- Preprod
  - Aggregator: `https://aggregator.release-preprod.api.mithril.network/aggregator`
  - Genesis key: `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey`
  - Ancillary key: `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey`
- Preview
  - Aggregator: `https://aggregator.pre-release-preview.api.mithril.network/aggregator`
  - Genesis key: `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey`
  - Ancillary key: `https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey`

## Implementation Steps

1. **Bundle `mithril-client`**
   - Add Mithril input in `flake.nix`.
   - Extend `nix/internal/common.nix` to include `mithril-client` binary in the installer bundle.
2. **Create bootstrap service**
   - Implement `MithrilBootstrapService` with:
     - Snapshot discovery and selection list (from JSON list).
     - Snapshot details fetch (optional).
     - Download and verification (JSON progress parsing).
     - Conversion step (LMDB flavor, committed).
     - Error propagation.
3. **IPC plumbing**
   - Add IPC channels in `source/common/ipc/api.ts`.
   - Implement main/renderer channel handlers.
4. **Startup gating**
   - Update `handleDiskSpace.ts` to pause `cardanoNode.start()` until the Mithril decision is resolved.
   - Add dev/test wipe flag handling before the empty DB check.
   - Detect incomplete Mithril restore; always wipe `stateDir/chain` and re-run the decision prompt.
   - Expose a "pending Mithril decision" state to the renderer and trigger the prompt from `source/renderer/app/containers/loading/LoadingPage.tsx` once `stores.app.isSetupPage` is false (agreements already accepted).
5. **UI + Store**
   - Build `MithrilBootstrapScreen` with a multi-step progress bar, cancel button, and snapshot selection UI.
   - Wire into loading flow via `MithrilBootstrapStore`.
6. **Error handling + fallback**
   - Retry flow on failure.
   - Fall back to standard sync if Mithril is unavailable or fails.

## Testing Strategy
- **Manual**: Fresh install on Windows/macOS/Linux with empty DB; validate end-to-end bootstrap.
- **Unit**: Mock `mithril-client` output parsing and progress events.
- **Integration**: Ensure IPC round-trip and safe fallback when Mithril errors.

### Manual QA Checklist

#### Common (all OS)
1. Start from an empty chain:
   - Fresh install or remove `stateDir/chain`.
   - Or launch with `--wipe-chain` / `DAEDALUS_WIPE_CHAIN=true`.
2. Complete onboarding (Profile, Terms, Analytics) and confirm the Mithril decision prompt appears once onboarding ends.
3. Choose **Use Mithril Fast Sync** and verify:
   - Snapshot list loads and selecting a snapshot updates metadata.
   - Progress steps move through download → verify → convert.
   - `stateDir/Logs/mithril-bootstrap.log` updates during the process.
4. On success:
   - `stateDir/Logs/mithril-bootstrap.lock` is removed.
   - The app continues to node startup without re-prompting.
5. Choose **Sync from Genesis** and verify:
   - The prompt closes.
   - Standard node sync begins.
6. Cancellation path:
   - Start Mithril, hit cancel, ensure cleanup happens, the lock file is removed, and the prompt returns.
7. Failure path (simulate by breaking network):
   - Mithril fails, error is shown, `stateDir/chain` is wiped, lock remains (to force re-prompt on restart).
   - From error screen, choose **Sync from Genesis**: verify the lock file is removed and node starts.
   - Restarting Daedalus before making a choice: verify it re-prompts for Mithril decision.

#### Windows
1. Verify `mithril-client` is present in the installed bundle (same directory as `cardano-node`).
2. Run the Common checklist and confirm no Windows Defender or permissions blocks the download.

#### macOS
1. Verify `mithril-client` exists inside the app bundle (`Daedalus.app/Contents/Resources`).
2. Run the Common checklist; confirm the app is not blocked by Gatekeeper.

#### Linux
1. Verify `mithril-client` exists in the installed bundle directory and is executable.
2. Run the Common checklist; confirm required permissions for the data directory.

## Rollout Plan
- Enable by default for Mainnet, Preprod, Preview.
- Keep a feature flag to disable Mithril via environment variable if needed.

## Miscellaneous
- Persist Mithril logs for support diagnostics.
- If conversion succeeds but the node still fails to start, surface the error and ask the user if they want to wipe the DB and retry Mithril bootstrap.

## References
- Mithril bootstrapping guide: [Bootstrap a Cardano node](https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node)
- Mithril network configurations: [Network configurations](https://mithril.network/doc/manual/getting-started/network-configurations)
- Mithril client library docs: [Mithril client library](https://mithril.network/doc/manual/develop/nodes/mithril-client-library)
- Mithril GitHub repository: [input-output-hk/mithril](https://github.com/input-output-hk/mithril)

---

**Status:** 🚧 In Progress  
**Date:** 2026-02-16  
**Author:** Antigravity AI  
**Notes:**
[2026-02-16] Added Mithril flake input.
[2026-02-16] Bundled mithril-client in cardano-bridge.
[2026-02-16] Defined Mithril bootstrap IPC channel/types.
[2026-02-16] Implemented main-process MithrilBootstrapService with progress parsing and lock cleanup.
[2026-02-16] Wired Mithril bootstrap IPC handlers and decision waiters.
[2026-02-16] Gated cardano-node startup on Mithril decision and handled lock cleanup on restart.
[2026-02-16] Added wipe-chain support (LauncherConfig + env/argv) for deterministic Mithril tests.
[2026-02-17] Centralized chain/snapshot wipe in MithrilBootstrapService and aligned flags to `wipeChain` naming.
[2026-02-16] Added renderer MithrilBootstrapStore and IPC channels.
[2026-02-16] Added Mithril bootstrap decision/progress UI overlay.
[2026-02-16] Gated Mithril overlay display until onboarding completes (AppStore.isSetupPage false).
[2026-02-16] Added snapshot selection metadata panel (digest, size, created, node version).
[2026-02-16] Added Mithril progress parsing helper and Jest unit coverage for progress line parsing.
[2026-02-16] Documented manual QA checklist for Mithril bootstrap across OSes.
[2026-02-16] Emitted Mithril idle status on decline to ensure fallback sync UI clears promptly.
[2026-02-16] Fixed onboarding rendering during node-stopped state so Mithril prompt can appear after setup.
[2026-02-17] Added snapshot details resolution for "latest", size parsing, and selection updates in the Mithril UI.
[2026-02-17] Added elapsed/remaining time display and file-based progress parsing for Mithril downloads.
[2026-02-17] Normalized mithril-client download commands and verification key handling for preview/mainnet/preprod.
[2026-02-17] Install Mithril snapshot into stateDir/chain before starting cardano-node.
[2026-02-17] Start cardano-node automatically after Mithril bootstrap completes.
[2026-02-17] Added node-start failure retry UI with optional DB wipe and wired Mithril bootstrap to wipe chain on retry when requested.
[2026-02-17] Fixed decline-after-failure flow so "Sync from genesis" always wipes chain artifacts and starts the node; added persistent decision listener in main.
[2026-02-24] Fixed issue where `mithril-bootstrap.lock` caused unintended chain wipes on next launch after a failure + genesis sync choice. Exposed `clearLockFile` and ensured it's called on decline and cancel.
