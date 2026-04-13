# SOP: Mithril Bootstrap Lock Handling

## Overview
This SOP explains the purpose and management of the `mithril-bootstrap.lock` file, which prevents the application from starting with a corrupted or incomplete Mithril snapshot.

## Purpose
The lock file acts as a safety mechanism. If Daedalus crashes or is killed during a Mithril bootstrap, the partial data in `stateDir/chain` or its resolved custom chain-storage target is not valid. The lock file ensures that on the next launch, this partial data is wiped before any sync begins.

## Location
`stateDir/Logs/mithril-bootstrap.lock`

The lock file always lives in `stateDir/Logs` even when chain data is redirected through custom chain storage.

## Lifecycle

1. **Creation**: Written at the start of `MithrilBootstrapService.startBootstrap()`.
2. **Success**: Removed by `MithrilBootstrapService.clearLockFile()` at the end of a successful bootstrap.
3. **Cancellation**: Removed by `MithrilBootstrapService.cancel()` to allow normal startup/genesis sync.
4. **Failure**: Left on disk intentionally to trigger a wipe on next startup (forcing the user to re-choose a sync method).
5. **Post-Failure Choice**: Must be explicitly cleared if the user chooses "Sync from genesis" after a failure.

## Management via Code

### Service Methods
- Use `mithrilBootstrapService.clearLockFile()` to remove the lock manually.
- Calling `mithrilBootstrapService.wipeChainAndSnapshots()` also removes the lock automatically as part of the total reset.

### Startup Gate
The `ensureMithrilStartupGate()` function in `source/main/utils/handleDiskSpace.ts` checks for this lock. If found:
- It calls `wipeChainAndSnapshots()` (which clears the lock and wipes the active chain target, including a redirected custom location when configured).
- It re-prompts the user for a Mithril decision.

## Common Pitfalls

### ⚠️ Lock file causes repeated wipes
**Problem:** If the lock file is not cleared when a user switches to "Sync from genesis" after a Mithril failure, every subsequent launch will see the lock, assume an incomplete bootstrap, and wipe the active chain data again, including a redirected custom chain target.  
**Solution:** Ensure the lock is cleared in all "decline" and "cancel" paths. This is handled by having `wipeChainAndSnapshots()` and `cancel()` call `clearLockFile()`.

## Verification
1. Start a Mithril bootstrap and force-kill the application.
2. Restart: Verify that Daedalus wipes the chain and prompts for Mithril again.
3. Choose "Sync from genesis": Verify the lock file is gone and syncing begins.
4. Restart again: Verify it does NOT wipe the chain this time.
5. If custom chain storage is configured, repeat the same flow and verify the redirected target is wiped/restored consistently.

---
**Created:** 2026-02-24  
**Author:** opencode
