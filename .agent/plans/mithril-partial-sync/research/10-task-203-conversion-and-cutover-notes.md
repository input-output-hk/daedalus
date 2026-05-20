# Task-203 Conversion And Cutover Notes

- Purpose: record the durable backend decisions and safety constraints landed by `task-203` for Mithril partial sync staged conversion, validated live cutover, and startup-owned interruption handling.
- Date: 2026-05-20

## Durable Findings

- Partial sync now reuses the bootstrap LSM conversion choreography through `source/main/mithril/mithrilSnapshotConverter.ts` instead of duplicating the slot-selection and temporary input move logic in two services.
- Validated staged cutover is enforced through a dedicated chain-storage seam, `ChainStorageManager.installValidatedPartialSyncSnapshot(...)`, rather than the broader `installSnapshot(...)` helper. The partial-sync cutover contract remains the fixed allowlist `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId` only.
- Unexpected staged top-level entries, including `volatile`, are rejected before live mutation and are also rejected again at the chain-storage install seam. This double validation is intentional defense-in-depth, but later tasks should keep the two allowlist sites aligned if the contract ever changes.
- Live `volatile/` is discarded during successful cutover because the managed target is emptied before the validated allowlist is installed.

## Persisted Marker Rules

- `source/main/mithril/mithrilPartialSyncMarker.ts` now persists a partial-sync marker in `stateDir/Logs/mithril-partial-sync.lock`.
- The current implementation uses these marker states in practice:
  - `cutover-in-progress`: Boundary B, live chain replacement has started and startup must block normal node start behind wipe-full-sync recovery only.
  - `installed-awaiting-node-start`: Boundary C1, validated staged DB is installed but the first proven node-start handoff has not yet succeeded.
- Successful first-start proof currently clears the marker rather than writing a durable `node-start-verified` state. The `node-start-verified` enum value is still present but unused; later work should either adopt it or remove it.

## Startup-Owned Recovery Constraints

- `source/main/utils/handleDiskSpace.ts` now owns the minimal startup recovery seam for interrupted unsafe partial-sync installs.
- On startup:
  - `cutover-in-progress` blocks normal startup and presents a native wipe-or-quit prompt.
  - `installed-awaiting-node-start` is allowed one truthful first-start proof attempt.
  - If that first-start proof fails, fallback normal startup is suppressed and the partial-sync status remains wipe-only.
- This startup seam is intentionally main-process-local for task-203. Diagnostics-launched retry/restart-normal/wipe action wiring remains task-204 work.

## Verification Evidence

- Focused Jest suite passed:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageManager.spec.ts`
  - `source/main/utils/handleDiskSpace.spec.ts`
  - `source/main/mithril/MithrilBootstrapService.spec.ts`
  - `source/main/mithril/MithrilBootstrapService.install.spec.ts`
- `yarn compile` still reports unrelated pre-existing failures in:
  - `source/main/cardano/setup.spec.ts`
  - `source/main/ipc/getHardwareWalletChannel.ts`
  - `source/main/trezor/manifest.ts`

## Follow-Ups

- `task-204` must replace the remaining startup source-string regression assertion with stronger behavioral coverage once the broader recovery matrix lands.
- `task-204` should also decide whether to keep clearing the marker on first successful start or to adopt the dormant `node-start-verified` marker state explicitly.
