# Watchdog Architecture — Planning Brief

## Origin

This plan was assembled retroactively (2026-07-30) after the implementation was complete, to capture the architectural motivation and decisions for future reference.

## Problems Identified

### 1. Process management in Node.js was fragile

The Electron main process directly spawned `cardano-node` and `cardano-wallet` as Node.js child processes. All restart logic, signal handling, and shutdown sequencing lived in `CardanoNode.ts`. On Windows, the process signal surface (`SIGTERM`, `SIGKILL`) differs significantly from Unix, and Named Pipe socket path handling was handled inconsistently — both `--socket-path` and `--node-socket` received file paths instead of `\\.\pipe\cardano-node.socket` on Windows.

### 2. No circuit breaker on wallet restarts

The TypeScript state machine would restart the wallet indefinitely on failure with no attempt limit. A permanently broken wallet would spin forever, consuming resources and obscuring the real error.

### 3. Log rotation was not working

`pipe_to_file` in the original Rust watchdog appended without bound. The TypeScript side had `mkLogFile`/`rfs` streams created for log rotation but they were dead competing file handles in the watchdog path — the watchdog wrote to one handle and TypeScript's rotation stream never received any data.

### 4. Block sync progress parsing was fragile and architecturally wrong

The block sync progress pipeline worked like this:
- `handleCheckBlockReplayProgress.ts` tailed the log file from disk after the watchdog wrote it
- `blockSyncProgressHelpers.ts` parsed line patterns from the tail output
- `get-block-sync-progress.ts` sent results over a dedicated IPC push channel to the renderer

Problems:
- The tail reader competed with the watchdog's file handle, causing missed lines during rotation
- Log rotation renamed files, breaking the tail path
- The push channel was a separate IPC contract from all other metrics
- Parsing happened after a file round-trip instead of from the live stream

### 5. Restart race on wallet_unrecoverable

When the watchdog emitted `wallet_unrecoverable`, TypeScript immediately rejected the startup promise. The `.catch()` in `CardanoNode.ts` then restarted the whole stack. But the watchdog was still alive — it needed up to 30 seconds to shut down cardano-node and release the chain DB lock file. The new watchdog process could start and try to acquire the same lock before the old node had released it.

## Goal

Move all platform-native process management concerns into a cross-platform Rust binary. Electron becomes a thin consumer of structured events, not an active process supervisor. The IPC boundary between Electron and the watchdog is the only integration surface.
