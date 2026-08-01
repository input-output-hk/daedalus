# 01 — TypeScript Mithril Code Survey

> Research notes from surveying the existing Mithril TypeScript implementation before moving to Rust.

## Files Surveyed

- `source/main/mithril/MithrilPartialSyncService.ts` — ~800-line state machine
- `source/main/mithril/mithrilCommandRunner.ts` — mithril-client and snapshot-converter subprocess spawning
- `source/main/mithril/mithrilSnapshotConverter.ts` — snapshot-converter wrapper
- `source/main/mithril/mithrilPartialSyncStaging.ts` — staging directory management
- `source/main/mithril/mithrilPartialSyncMarker.ts` — marker file read/write
- `source/main/mithril/mithrilNetworkConfig.ts` — aggregator URLs, verification key fetching
- `source/main/mithril/mithrilProgressItems.ts` — progress step label helpers
- `source/main/mithril/mithrilErrors.ts` — error code constants
- `source/main/mithril/mithrilSnapshotMetadata.ts` — snapshot metadata normalization
- `source/main/mithril/MithrilController.ts` — IPC bridge / orchestration layer
- `source/main/ipc/mithrilPartialSyncChannel.ts` — 7 Electron IPC channels

---

## CLI Commands

### mithril-client — snapshot metadata (behind-ness probe)

```
mithril-client \
  --origin-tag DAEDALUS \
  --json \
  cardano-db snapshot show latest
```

Response JSON includes `beacon.immutable_file_number` — the latest certified snapshot's immutable file count. Compare against local count (files in `<stateDir>/chain/immutable/` with `.chunk` extension).

### mithril-client — download

```
mithril-client \
  --origin-tag DAEDALUS \
  --json \
  cardano-db download latest \
  --download-dir <stagingDir> \
  --start <startImmutable> \
  --end <endImmutable> \
  --include-ancillary \
  --allow-override
```

Env vars required:
- `AGGREGATOR_ENDPOINT` — per-network aggregator URL
- `GENESIS_VERIFICATION_KEY` — fetched from GitHub
- `ANCILLARY_VERIFICATION_KEY` — fetched from GitHub (mainnet only)

### snapshot-converter

```
snapshot-converter \
  --input-mem <stagingLedgerDir/<slot>> \
  --output-lsm-snapshot <stagingLedgerDir/<slot>> \
  --output-lsm-database <chain_path>/lsm \
  --config <configFile>
```

Note: current TypeScript implementation parses no structured progress from snapshot-converter stdout. It only waits for exit code.

---

## State Machine Phases

```
idle
  → preparing         (synchronous; set at start of startPartialSync)
  → downloading       (mithril-client running)
  → verifying         (mithril-client verification step)
  → converting        (snapshot-converter running)
  → installing        (atomic rename in progress)
  → finalizing        (node started, waiting for wallet_ready)
  → starting-node     (node/wallet start triggered)
  → completed         (wallet_ready received)

cancel branch:
  any phase → cancelling → cancelled

error branch:
  any phase → failed
```

---

## Marker File

Path: `<stateDir>/Logs/mithril-partial-sync.lock`

JSON format:
```json
{ "state": "cutover-in-progress" }
{ "state": "installed-awaiting-node-start" }
{ "state": "node-start-verified" }
```

Transitions:
- Write `cutover-in-progress` before atomic rename from staging to chain path.
- Write `installed-awaiting-node-start` after rename succeeds.
- Write `node-start-verified` after wallet_ready fires (inside `finalizeInstalledNodeStart`).

On startup: if marker exists and is `installed-awaiting-node-start`, skip download and start node/wallet directly (`handleStoppedNodeStartup` → `startInstalledNode`).

---

## Staging Directory

Path: `<parent of managedChainPath>/mithril-partial-sync/download/db/`

Post-download validation checks for:
- `clean` file
- `immutable/` directory
- `ledger/` directory
- `protocolMagicId` file

Staging is removed on error or cancellation.

---

## Behind-ness Probe

1. Fetch latest snapshot metadata: `mithril-client --json cardano-db snapshot show latest`
2. Parse `beacon.immutable_file_number` from response.
3. Count local immutable files in `<stateDir>/chain/immutable/` (any file, not just `.chunk`).
4. If `certified - local < threshold` (default 20), skip Mithril.
5. The threshold 20 corresponds to roughly one epoch of immutable files.

---

## Network Config

Per-network aggregator URLs (hardcoded in `mithrilNetworkConfig.ts`):

| Network | Aggregator URL |
|---------|---------------|
| mainnet | `https://aggregator.release-mainnet.api.mithril.network/aggregator` |
| preprod | `https://aggregator.release-preprod.api.mithril.network/aggregator` |
| preview | `https://aggregator.release-preview.api.mithril.network/aggregator` |
| sanchonet | `https://aggregator.release-sanchonet.api.mithril.network/aggregator` |

Verification keys are fetched at startup from GitHub:
```
https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-infra/configuration/<network>/genesis.vkey
```

The TypeScript layer fetches these once and passes them into the service via `MithrilNetworkConfig`. In the Rust approach, TypeScript still resolves them at startup and passes them in the WatchdogConfig, avoiding the need for `reqwest` in the watchdog binary.

---

## IPC Channels (current — to be deleted)

7 Electron channels in `source/main/ipc/mithrilPartialSyncChannel.ts`:

1. `mithrilSyncStart` — trigger sync
2. `mithrilSyncCancel` — cancel sync
3. `mithrilSyncStatus` — poll status (returns current phase + progress)
4. `mithrilCheckAvailability` — check if Mithril is available for this network
5. `mithrilRestartNormalStart` — abandon Mithril, start node normally
6. `mithrilWipeAndFullSync` — wipe Mithril data and restart
7. `mithrilFinalize` — trigger finalization after installed-awaiting-node-start

These are replaced by 3 channels:
1. `mithrilStart` — trigger sync
2. `mithrilCancel` — cancel sync
3. `mithrilEvent` (push) — receive status/progress from watchdog

---

## Progress Object Format (mithril-client stdout JSON lines)

```json
{
  "files_downloaded": 1234,
  "files_total": 5678,
  "bytes_downloaded": 1073741824,
  "bytes_total": 10737418240,
  "seconds_elapsed": 42.5,
  "step_num": 2,
  "total_steps": 4
}
```

Step numbers map to phases:
- step 1: downloading
- step 2: verifying
- step 3+: post-processing (TypeScript maps these to `converting`/`installing`)

---

## Key Observations for Rust Implementation

1. **No concurrent Mithril + node/wallet runs**: the TypeScript layer stops cardano-node before starting Mithril. The Rust watchdog should do the same: accept `start_mithril` only when node/wallet are not running, or stop them first.
2. **Installer resume path is critical**: `installed-awaiting-node-start` marker must be checked at watchdog startup (before waiting for `start_mithril` command), because Daedalus may have been closed mid-sync.
3. **Cancellation must clean staging**: a partial download in staging must be removed on cancel to avoid disk space accumulation.
4. **Rate-limiting progress**: the TypeScript implementation throttles updates before sending to renderer. The Rust layer should do the same to avoid flooding the stdout channel.
5. **snapshot-converter slot argument**: the input path requires the ledger slot directory, which must be discovered by listing `<staging>/ledger/` after download. This is not a fixed path.
