# Mithril Snapshot Bootstrapping

## Overview
Improve the first-run experience by offering Mithril snapshot bootstrapping when the Cardano DB is empty. This reduces initial sync time from days to minutes by downloading and verifying a certified Cardano DB snapshot, then launching `cardano-node` against the restored DB.

## Requirements
- [ ] Detect empty `stateDir/chain` on startup before `cardano-node` launches.
- [ ] Interrupt automatic node startup and present a user choice: "Use Mithril Fast Sync" or "Sync from Genesis".
- [ ] Sequence the Mithril decision after onboarding agreements (Profile Configuration, Terms of Service, Analytics). Acceptance state lives in `ProfileStore` (`areTermsOfUseAccepted`, `analyticsAcceptanceStatus`, `isInitialScreen`, `isCurrentLocaleSet`) with setup routing derived in `AppStore.isSetupPage`. Show the decision immediately on startup when `isSetupPage` is false; otherwise wait until onboarding completes.
- [ ] For supported networks (Mainnet, Preprod, Preview), run Mithril snapshot restoration using hardcoded aggregator endpoints and verification keys.
- [ ] Allow the user to select which snapshot to restore (latest by default, but list available snapshots).
- [ ] Automate ledger snapshot conversion using `mithril-client tools utxo-hd snapshot-converter --commit` after restoration.
- [ ] Provide a progress UI with explicit steps and error states.
- [ ] If the user declines, continue with standard genesis sync. If Mithril fails, wipe `stateDir/chain`, any downloaded snapshot data, present error to the user and re-prompt.
- [ ] Provide a developer/test flag to launch Daedalus with a wiped DB for consistent Mithril flow testing.
- [ ] On startup, detect an incomplete Mithril restore (e.g., marker/lock file or partial snapshot artifacts), wipe `stateDir/chain`, and re-prompt the user to either retry Mithril from scratch or sync from genesis.

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
- Incomplete restore marker: create `stateDir/Logs/mithril-bootstrap.lock` at bootstrap start. On success, delete the lock and cleanup downloaded snapshot artifacts. On failure, cleanup downloaded snapshot artifacts, wipe `stateDir/chain`, and leave the lock so the next startup re-prompts.

### Test/Developer Workflow
No existing CLI flag or environment variable for wiping the DB was found in the codebase. Add a dev/test-only switch with:
- CLI: `--wipe-db`
- Env: `DAEDALUS_WIPE_DB=true`
- Launcher config: `wipeDb?: boolean` in `LauncherConfig`

Resolution precedence: `launcherConfig.wipeDb ?? (process.env.DAEDALUS_WIPE_DB === 'true') ?? argv.includes('--wipe-db')`.

Launcher wiring note: extend `nix/launcher-config.nix` (and any launcher config generator) to pass `wipeDb` into `LauncherConfig` for dev/test builds.
This is now wired in `nix/internal/launcher-config.nix` with a default `wipeDb = false`.

Behavior:
- Stops `cardano-node` if running.
- Deletes `stateDir/chain`.
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
[2026-02-16] Added wipe-db support (LauncherConfig + env/argv) for deterministic Mithril tests.
[2026-02-16] Added renderer MithrilBootstrapStore and IPC channels.
[2026-02-16] Added Mithril bootstrap decision/progress UI overlay.
