# Chain Storage Review — Fix Plan

> Branch: `fix/chain-storage-external-data-deletion`
>
> Generated: 2026-04-14

---

## Overview

This plan originally addressed 15 findings from the review of the chain storage path handling and validation changes. Those items are now complete. The document now also tracks the next follow-up findings from the latest review pass so future work has one place to continue from.

---

## Completed Items (2026-04-15)

The original review items are complete and verified in the current working tree.

- **Fix 1 + Fix 5 completed**: nested managed-child selections are rejected again, including chain-suffixed descendants, and the `is-managed-child` validation reason is now emitted by real code paths.
- **Fix 2 completed**: coordinator mutations now accept node state and block destructive directory changes when the node is active.
- **Fix 3 completed**: `createSelfnodeConfig` now validates the config path before mutating chain storage.
- **Fix 4 completed**: Mithril status listeners now run independently of renderer delivery failures.
- **Fix 6 completed**: `returnToStorageLocation` now has a re-entry guard and safe cleanup flow.
- **Fix 7 completed**: rollback now restores broken symlink targets using `linkTargetPath` fallback.
- **Fix 8 completed**: Mithril completion handoff now checks directory-change generation after async boundaries.
- **Fix 9 completed**: apply and reset failures in the storage picker are now surfaced instead of escaping the click handler.
- **Fix 10 completed**: invalid selections are now retained in picker candidate state instead of being discarded immediately.
- **Fix 11 completed**: `preparing` and `cancelled` now clear transient Mithril state consistently.
- **Fix 12 completed**: channel setup now has idempotent guards.
- **Fix 13 completed**: `returnToStorageLocation` now catches IPC rejection and exits to a safe state.
- **Fix 14 completed**: symlink aliases to the default chain path are treated as reset-to-default.
- **Fix 15 completed**: picker coverage was expanded around invalid path handling and apply/reset error paths.
- **Verification completed**: all 10 affected suites passed during implementation review, covering 180 tests.

---

## Fix 1 — Reject nested managed-child path selections

| Field | Value |
|-------|-------|
| **Severity** | High |
| **Effort** | Small (30–60 min) |
| **Files** | `source/main/utils/chainStorageValidation.ts` (L121–L146), `source/main/utils/chainStorageValidation.spec.ts` |

### Problem

`validateChainStorageDirectory` only canonicalizes a direct selection of a folder named `chain` (the managed child). A descendant like `/mnt/external/chain/db` passes validation, and `setDirectory` then creates a nested `chain/` inside existing chain data, orphaning the real database.

### Root Cause

Lines 121–126 only check `path.basename(resolvedPath) === CHAIN_DIRECTORY_NAME`. There is no check for deeper descendants within an existing managed chain root.

### Fix Steps

1. In `chainStorageValidation.ts`, after resolving the path (around L110), resolve the current managed chain path from the config.
2. Add a guard: if the resolved selected path is *inside* the current managed chain path (use the existing `isSubPath` / `isPathWithin` helper), reject with reason `'is-managed-child'` — unless the resolved path equals the managed chain parent exactly (the existing canonicalization case).
3. Add tests in `chainStorageValidation.spec.ts`:
   - Select `/mnt/external/chain/db` where `/mnt/external/chain` is the active managed chain → expect rejection `is-managed-child`.
   - Select `/mnt/external/chain` (exact managed child) → expect canonicalization to `/mnt/external` (existing behavior).
   - Select `/mnt/external/chain/deeply/nested` → expect rejection.

### Acceptance Criteria

- Selecting any path that resolves *within* the current managed chain directory is rejected.
- The exact managed child still canonicalizes to its parent (existing behavior preserved).
- All new and existing validation tests pass.

---

## Fix 2 — Guard destructive chain-storage mutations with node-stopped check

| Field | Value |
|-------|-------|
| **Severity** | High |
| **Effort** | Medium (1–2 hrs) |
| **Files** | `source/main/utils/chainStorageCoordinator.ts` (L44, L66), `source/main/utils/chainStorageManager.ts` (L221, L246), `source/main/utils/chainStorageManagerShared.ts` (L304, L337), `source/main/utils/chainStorageCoordinator.spec.ts` |

### Problem

`prepareForLocationChange`, `setDirectory`, `unlinkChainEntryPoint`, `removeManagedDirectory`, and `resetToDefault` all remove or repoint the live chain entry point, but none of them assert that `cardano-node` is stopped. If any code path reaches these while the node is running, they can delete or repoint the database underneath the running process.

### Root Cause

The coordinator's `_assertBootstrapMutationAllowed` only checks `_bootstrapInProgress`. There is no `assertNodeStopped` guard for destructive operations.

### Fix Steps

1. Add a `_nodeStateProvider` to `ChainStorageCoordinator` (similar to `setMithrilBootstrapNodeStateProvider` in the channel layer). Accept it via constructor or setter.
2. Create a private `_assertNodeStopped(action: string)` method on the coordinator that reads the current node state and throws if the node is in `RUNNING`, `STARTING`, or `SYNCING` states.
3. Call `_assertNodeStopped` at the top of:
   - `prepareForLocationChange`
   - `setDirectory`
   - `wipeChainAndSnapshots` (already partially guarded)
4. Wire the node state provider in `source/main/index.ts` where the coordinator is initialized.
5. Add test cases in `chainStorageCoordinator.spec.ts`:
   - `prepareForLocationChange` throws when node state is `RUNNING`.
   - `setDirectory` throws when node state is `RUNNING`.
   - Both succeed when node state is `STOPPED`.

### Acceptance Criteria

- All destructive coordinator methods throw when the node is running.
- Existing tests continue to pass (they use stopped/null state).
- New node-running rejection tests pass.

---

## Fix 3 — Validate inputs before mutating chain storage in `createSelfnodeConfig`

| Field | Value |
|-------|-------|
| **Severity** | High |
| **Effort** | Small (15–30 min) |
| **Files** | `source/main/cardano/utils.ts` (L140–L144), `source/main/cardano/utils.spec.ts` |

### Problem

`createSelfnodeConfig` calls `unlinkChainEntryPoint()` and `resetToDefault()` before checking if the config file exists. If the config path is invalid, the function already mutated chain storage and then throws.

### Root Cause

The config file existence check (`fs.pathExists(configFilePath)`) is on L144, after the chain-storage mutations on L140–L141.

### Fix Steps

1. Move the config file existence check and its throw **before** the chain storage mutations:
   ```ts
   const configFileExists = await fs.pathExists(configFilePath);
   if (!configFileExists) {
     throw new Error('No config file found');
   }
   // THEN do chain storage reset
   const chainStorageManager = new ChainStorageManager(stateDir);
   await chainStorageManager.unlinkChainEntryPoint();
   await chainStorageManager.resetToDefault();
   ```
2. Update the spec: the existing test that asserts early failure should also verify that `unlinkChainEntryPoint` and `resetToDefault` were **not** called.

### Acceptance Criteria

- An invalid config path throws without touching chain storage.
- The spec explicitly asserts no chain-storage mutation on early failure.

---

## Fix 4 — Decouple status listeners from renderer delivery in broadcast

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Small (15–30 min) |
| **Files** | `source/main/ipc/mithrilBootstrapChannel.ts` (L85–L92), `source/main/ipc/mithrilBootstrapChannel.spec.ts` |

### Problem

`broadcastMithrilBootstrapStatus` awaits `sendStatusUpdate` before running `statusListeners`. If the renderer `webContents` is destroyed, the send rejects and the `statusListeners` (which drive the startup handoff) are silently skipped.

### Root Cause

Sequential `await` followed by listener invocation on L89–L92 — a rejected send prevents listener execution.

### Fix Steps

1. Restructure `broadcastMithrilBootstrapStatus` so that `statusListeners` are invoked **before** or **independently of** the renderer send:
   ```ts
   const broadcastMithrilBootstrapStatus = async (status) => {
     lastStatus = status;
     statusListeners.forEach((listener) => listener(status));
     try {
       if (sendStatusUpdate) await sendStatusUpdate(status);
     } catch (error) {
       logger.warn('Failed to send Mithril status to renderer', { error });
     }
   };
   ```
2. Add a test case: mock `sendStatusUpdate` to reject, assert that `onMithrilBootstrapStatus` handlers still fire.

### Acceptance Criteria

- Status listeners always fire regardless of renderer availability.
- A destroyed renderer logs a warning instead of silently breaking the startup flow.

---

## Fix 5 — Remove or enforce `is-managed-child` rejection reason

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Small (15 min) |
| **Files** | `source/common/types/mithril-bootstrap.types.ts` (L99), `source/main/utils/chainStorageValidation.ts` |

### Problem

The `ChainStorageValidationReason` type still includes `'is-managed-child'`, but the backend never emits it after the validation refactor. The contract and implementation have drifted.

### Root Cause

The type was kept for backwards compatibility but enforcement was removed.

### Fix Steps

- **If Fix 1 is implemented** (recommended), emit `'is-managed-child'` from the new nested-descendant guard. No type change needed.
- **If Fix 1 is deferred**, remove `'is-managed-child'` from the `ChainStorageValidationReason` union type and verify no renderer code depends on it being emitted.

### Acceptance Criteria

- Every value in the `ChainStorageValidationReason` union type is emitted by at least one code path, or the dead variant is removed.

---

## Fix 6 — Serialize `returnToStorageLocation` to prevent double-invocation race

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Small (15–30 min) |
| **Files** | `source/renderer/app/stores/MithrilBootstrapStore.ts` (L377–L405), `source/renderer/app/stores/MithrilBootstrapStore.spec.ts` |

### Problem

`returnToStorageLocation` is `async` but has no re-entry guard. A double-click triggers two concurrent IPC requests. The first response restores the draft path; the second falls through the `null` branch and clears `pendingChainPath`.

### Root Cause

No in-flight flag and no stale-response detection.

### Fix Steps

1. Add a `_returnToStorageInFlight` flag (or reuse `isApplyingStorageLocation`).
2. Set it `true` before the `await`, return early if already `true`.
3. Set it `false` in a `finally` block.
4. Add a test: call `returnToStorageLocation` twice concurrently, assert the second call is a no-op and `pendingChainPath` preserves the correct value.

### Acceptance Criteria

- Only one IPC request is made per user action.
- The test verifies concurrent invocations don't corrupt state.

---

## Fix 7 — Fix rollback to restore broken symlinks

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Small (30 min) |
| **Files** | `source/main/utils/chainStorageManagerShared.ts` (L273–L290), `source/main/utils/chainStorageManager.spec.ts` |

### Problem

`rollbackSetDirectory` only uses `previousState.resolvedPath`. If the pre-existing entry point was a broken symlink, `resolvedPath` is `undefined` because `realpath` failed. Rollback then does nothing, leaving `stateDir/chain` removed.

### Root Cause

The symlink rollback branch (L280) checks `previousState.resolvedPath` but ignores `previousState.linkTargetPath`.

### Fix Steps

1. In `rollbackSetDirectory`, symlink case: use `previousState.linkTargetPath` as fallback when `resolvedPath` is `undefined`:
   ```ts
   case 'symlink': {
     const rollbackTarget = previousState.resolvedPath ?? previousState.linkTargetPath;
     if (rollbackTarget) {
       await fs.ensureDir(rollbackTarget);
       await ctx._replaceCustomChainEntryPoint(rollbackTarget);
     }
     break;
   }
   ```
   Note: `ensureDir` on a broken target path may fail — consider wrapping that in a try/catch and only restoring the symlink itself if the target directory can't be created.
2. Add a test: start with a broken symlink (`chain → /nonexistent`), attempt `setDirectory` that fails mid-operation, verify rollback restores the original broken symlink link.

### Acceptance Criteria

- Rollback restores the original link target even when the target doesn't exist.
- New test covers broken-symlink rollback.

---

## Fix 8 — Check directory-change generation in bootstrap handoff

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Small (30 min) |
| **Files** | `source/main/utils/handleDiskSpace.ts` (L219–L240), `source/main/utils/handleDiskSpace.spec.ts` |

### Problem

`startNodeAfterMithrilCompletion` doesn't check `directoryChangeGeneration` after its awaits (unlike `runHandleCheckDiskSpace`). A storage change during the handoff window starts the node against the old path.

### Root Cause

The generation-check pattern was applied to the main disk-space loop but not to the bootstrap completion handoff.

### Fix Steps

1. Capture `const gen = directoryChangeGeneration` at the start of `startNodeAfterMithrilCompletion`.
2. After each `await` (start, delay, idle), check generation:
   ```ts
   if (directoryChangeGeneration !== gen) {
     logger.info('[MITHRIL] Aborting node start — directory changed during handoff');
     return;
   }
   ```
3. Add a test: trigger `startNodeAfterMithrilCompletion`, change the directory during the 6s delay, assert the function aborts instead of emitting idle.

### Acceptance Criteria

- A directory change during the bootstrap-to-node handoff aborts the handoff cleanly.
- New test covers this scenario.

---

## Fix 9 — Handle apply/reset failures in `ChainStorageLocationPicker`

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Small (30 min) |
| **Files** | `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx` (L291–L320), `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.spec.tsx` |

### Problem

If `onSetChainStorageDirectory` or `onResetChainStorageDirectory` rejects at the IPC/filesystem layer, the promise escapes the click handler. The spinner clears in `finally` but no error is surfaced.

### Root Cause

The `handleContinue` try block has no `catch` for IPC rejections.

### Fix Steps

1. Add a `catch` block in `handleContinue` that:
   - Sets a local error state (`setSelectionValidation` with an `isValid: false` error message).
   - Preserves the attempted path/candidate so the user can retry.
   - Logs the error.
2. Add test cases:
   - Mock `onSetChainStorageDirectory` to reject → assert error message is displayed and candidate is preserved.
   - Mock `onResetChainStorageDirectory` to reject → assert error message is displayed.

### Acceptance Criteria

- A failed apply/reset shows a user-visible error message.
- The candidate path is preserved so the user can retry or modify.

---

## Fix 10 — Display rejected path in picker after validation failure

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Small (30 min) |
| **Files** | `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx` (L194–L264), `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.spec.tsx` |

### Problem

When directory validation fails, the component records the error but doesn't store the rejected path. The input displays the old committed path while the error message references the new rejected selection.

### Root Cause

Line ~262: `setCandidate` is only called inside `if (validation.isValid)`.

### Fix Steps

1. In the `handleChooseDirectory` function, after receiving an invalid validation result, store the rejected path in local state so it displays in the input:
   ```ts
   if (validation.isValid) {
     setCandidate({ path: validation.path ?? selectedPath, validation });
   } else {
     setCandidate({ path: selectedPath, validation });
   }
   ```
   This means the `effectiveSelection` will show the rejected path alongside the error message.
2. Ensure the "Continue" button remains disabled when the candidate has an invalid validation.
3. Add a test: select an invalid directory → assert the input shows the rejected path, the error message is visible, and Continue is disabled.

### Acceptance Criteria

- The displayed path matches the path referenced in the error message.
- Continue is disabled when the candidate is invalid.

---

## Fix 11 — Emit full transient-state reset on `preparing` and `cancelled`

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (15–30 min) |
| **Files** | `source/main/mithril/MithrilBootstrapService.ts` (L169–L174, L281–L295), `source/main/mithril/MithrilBootstrapService.spec.ts` |

### Problem

The `preparing` status update doesn't clear `snapshot`. The `cancelled` status doesn't clear `elapsedSeconds`, counters, or error metadata. A subsequent run can inherit stale display data.

### Fix Steps

1. In the `preparing` update (L172), add `snapshot: null`.
2. In the `cancelled` update (L287), add the full reset:
   ```ts
   this._updateStatus({
     status: 'cancelled',
     snapshot: null,
     error: null,
     filesDownloaded: undefined,
     filesTotal: undefined,
     ancillaryBytesDownloaded: undefined,
     ancillaryBytesTotal: undefined,
     elapsedSeconds: undefined,
     progressItems: [],
   });
   ```
3. Add spec assertions:
   - After a `completed` → new `preparing` transition, assert `snapshot` is `null`.
   - After `cancel()`, assert all transient fields are cleared.

### Acceptance Criteria

- No stale data carries across bootstrap attempts or cancellations.

---

## Fix 12 — Make channel setup functions idempotent

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (15 min) |
| **Files** | `source/main/ipc/mithrilBootstrapChannel.ts` (L184–L202), `source/main/ipc/chainStorageChannel.ts` (L41) |

### Problem

Repeated calls to `handleMithrilBootstrapRequests` or `handleChainStorageRequests` add duplicate `ipcMain` listeners and status subscriptions.

### Fix Steps

1. Add a module-level `let initialized = false` guard to each setup function.
2. Return early if already initialized:
   ```ts
   if (initialized) return;
   initialized = true;
   ```
3. Optionally add a test that calls setup twice and asserts the handler count doesn't double.

### Acceptance Criteria

- Double-initialization is a no-op.
- No duplicate IPC listeners or status subscriptions.

---

## Fix 13 — Handle async rejection on "Change location" return

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (15 min) |
| **Files** | `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx` (L102–L103), `source/renderer/app/stores/MithrilBootstrapStore.ts` (L377) |

### Problem

If the `prepareChainStorageLocationChange` IPC request fails in `returnToStorageLocation`, the rejection is unhandled — the user gets no feedback.

### Fix Steps

1. Wrap the `await` in `returnToStorageLocation` with a try/catch:
   ```ts
   try {
     const cleanupValidation = await prepareChainStorageLocationChangeChannel.request();
     // existing runInAction...
   } catch (error) {
     logger.error('Failed to prepare for location change', { error });
     runInAction('return to chain storage location picker — error', () => {
       this.storageLocationConfirmed = false;
       this.isApplyingStorageLocation = false;
     });
   }
   ```
2. The user is returned to the picker in a safe default state even on failure.

### Acceptance Criteria

- A failed IPC request logs the error and returns the user to the picker without a broken state.

---

## Fix 14 — Handle symlink aliases to the default chain path

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (15 min) |
| **Files** | `source/main/utils/chainStorageValidation.ts` (L121–L132), `source/main/utils/chainStorageValidation.spec.ts` |

### Problem

The default-path fast path only checks raw path equality before `realpath`. A symlink that resolves to `stateDir/chain` skips that branch and then fails the `inside-state-dir` check.

### Fix Steps

1. After resolving the selected path, add a comparison against the resolved default chain path:
   ```ts
   const resolvedDefaultChainPath = await fs.realpath(
     path.join(stateDir, CHAIN_DIRECTORY_NAME)
   ).catch(() => path.join(stateDir, CHAIN_DIRECTORY_NAME));

   if (resolvedPath === resolvedDefaultChainPath || resolvedValidationPath === resolvedDefaultChainPath) {
     // Treat as default selection
     return { ...defaultValidation, isValid: true, reason: null };
   }
   ```
2. Add a test: create a symlink `/tmp/alias → stateDir/chain`, select `/tmp/alias` → expect it is treated as a reset-to-default.

### Acceptance Criteria

- Symlink aliases to the default chain path are treated as reset-to-default.
- No `inside-state-dir` false positive.

---

## Fix 15 — Add missing picker test coverage

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (30 min) |
| **Files** | `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.spec.tsx` |

### Problem

The picker spec doesn't test IPC rejection during apply/reset (Fix 9) or invalid-path display after validation failure (Fix 10).

### Fix Steps

These tests should be added as part of Fixes 9 and 10. If those fixes are deferred:

1. Add a test: mock `onSetChainStorageDirectory` to reject → assert the component doesn't crash and shows a fallback state.
2. Add a test: select an invalid path → assert the rejected path is displayed (or document the current behavior as intentional).

### Acceptance Criteria

- Test coverage for both error paths exists.

---

## Execution Order

Recommended implementation sequence based on dependencies and severity:

1. **Fix 3** (no dependencies, small, high severity)
2. **Fix 1 + Fix 5** (Fix 5 depends on Fix 1's decision)
3. **Fix 2** (independent, high severity)
4. **Fix 4** (independent, quick)
5. **Fix 7** (independent)
6. **Fix 8** (independent)
7. **Fix 6** (independent)
8. **Fix 9 + Fix 10 + Fix 15** (UI fixes, do together)
9. **Fix 11** (independent)
10. **Fix 12** (independent)
11. **Fix 13** (independent)
12. **Fix 14** (independent)

---

## Follow-up Findings (2026-04-15)

These are the next issues identified after the first review batch was implemented.

---

## Completed Follow-up Items (2026-04-15)

The follow-up review items (Fixes 16–20) are complete and verified in the current working tree. All 211 affected tests pass.

- **Fix 16 completed**: Both `_assertNodeStopped` implementations in the coordinator and layout helpers now only allow `CardanoNodeStates.STOPPED`. The `STOPPING` state is rejected because the underlying `cardano-node` process may still have the database open. A direct `_assertNodeStopped` guard was also added to `wipeChainAndSnapshots` in the coordinator. Test coverage proves `STOPPING` is rejected for `prepareForLocationChange`, `setDirectory`, and `wipeChainAndSnapshots`.
- **Fix 17 completed**: `handleMithrilBootstrapRequests` now always rebinds `sendStatusUpdate` to the latest `BrowserWindow` before checking the idempotent guard. IPC request listeners and the `service.onStatus` subscription remain inside the guard to prevent duplication. Test coverage verifies that calling setup twice with different windows sends status to the latest window without duplicating request handlers.
- **Fix 18 completed**: The picker now shows the raw rejected path for invalid draft candidates instead of passing it through `getManagedChainDisplayPath` (which appends `/chain`). The `isContinueDisabled` logic also includes a `hasInvalidCandidate` check so Continue is disabled when an invalid candidate is active. Test coverage verifies raw path display for invalid selections and managed-chain display for valid committed selections.
- **Fix 19 completed**: The hardening summary's stale-window edge case is marked as fixed with an explanation that Daedalus recreates the main window after renderer failure via `RendererErrorHandler`. Node-state guard text updated to reflect `STOPPED`-only policy. Rejected-path preservation text updated to mention bypassing managed-chain formatting.
- **Fix 20 completed**: Changelog entry added for 2026-04-15 describing all 5 follow-up fixes with references to the hardening summary.

---

## Fix 16 — Disallow destructive storage mutations while cardano-node is still stopping

| Field | Value |
|-------|-------|
| **Severity** | High |
| **Effort** | Small (30–60 min) |
| **Files** | `source/main/utils/chainStorageCoordinator.ts`, `source/main/utils/chainStorageManagerLayout.ts`, `source/main/cardano/CardanoNode.ts`, `source/main/utils/chainStorageCoordinator.spec.ts` |

### Problem

The new node-state guard still treats `STOPPING` as safe for destructive storage mutations. In practice, `cardano-node` enters `STOPPING` before the main process finishes waiting for the node process to exit. That leaves a window where chain storage can still be unlinked, repointed, or migrated while the process may still have the database open.

### Root Cause

Both `_assertNodeStopped` implementations allow `CardanoNodeStates.STOPPING`, but that state does not guarantee the underlying process has fully exited.

### Fix Steps

1. Tighten the node-state guard in both coordinator and layout helpers so only `STOPPED` is treated as safe for destructive storage work.
2. Re-check all guard call sites to make sure they still pass the correct node state into:
  - `prepareForLocationChange`
  - `setDirectory`
  - `wipeChainAndSnapshots`
  - layout recovery and migration helpers
3. Add coverage proving `STOPPING` is rejected the same way as `RUNNING`.

### Acceptance Criteria

- Destructive chain-storage mutations are blocked unless node state is strictly `STOPPED`.
- Tests cover `RUNNING`, `STOPPING`, and `STOPPED` cases.

---

## Fix 17 — Rebind Mithril status delivery when the main window is recreated

| Field | Value |
|-------|-------|
| **Severity** | Medium |
| **Effort** | Medium (1–2 hrs) |
| **Files** | `source/main/ipc/mithrilBootstrapChannel.ts`, `source/main/ipc/mithrilBootstrapChannel.spec.ts`, `source/main/windows/main.ts`, `source/main/utils/rendererErrorHandler.ts` |

### Problem

The new initialization guard in `handleMithrilBootstrapRequests` prevents duplicate setup, but it also prevents `sendStatusUpdate` from being rebound to a replacement `BrowserWindow`. Daedalus recreates the main window after renderer failure, so Mithril status updates can keep targeting stale `webContents` after recovery.

### Root Cause

The one-time initialization flag is currently coupled to both request-listener setup and active-window status delivery binding.

### Fix Steps

1. Split one-time request registration from per-window status binding.
2. Keep IPC listener setup idempotent, but always refresh `sendStatusUpdate` when a new window is passed in.
3. Ensure any existing `onStatus` subscription does not get duplicated when rebinding.
4. Add a test that calls `handleMithrilBootstrapRequests` twice with different windows and verifies status updates go to the latest window.

### Acceptance Criteria

- Repeated setup with a replacement window does not duplicate request handlers.
- Mithril status sends always target the most recent live window.

---

## Fix 18 — Preserve the exact rejected path in picker display for invalid selections

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (30–60 min) |
| **Files** | `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx`, `source/renderer/app/components/chain-storage/chainStorageUtils.ts`, `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.spec.tsx` |

### Problem

The picker now stores the rejected path after validation failure, but the display helper still appends the managed `chain` suffix to every non-null path. That means some invalid selections are still shown in a transformed form instead of exactly as the user selected them.

### Root Cause

`getManagedChainDisplayPath` applies managed-path formatting without considering whether the current selection is valid, committed, or a rejected draft.

### Fix Steps

1. Separate committed managed-path display from invalid draft-path display.
2. Keep the managed `.../chain` display for valid committed selections.
3. Show the raw rejected path when validation failed and the picker is intentionally preserving that user attempt.
4. Add tests for at least:
  - invalid managed-child selection
  - invalid descendant selection
  - valid committed custom selection still displaying managed chain path

### Acceptance Criteria

- Invalid selections display exactly the path the user chose.
- Valid committed selections still display the managed chain path.

---

## Fix 19 — Correct the hardening summary's stale-window risk framing

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (15 min) |
| **Files** | `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`, `source/main/windows/main.ts`, `source/main/utils/rendererErrorHandler.ts` |

### Problem

The hardening summary currently describes the stale `webContents` issue as low risk because Daedalus uses a single window. In practice, the main window can be recreated in the same process lifetime after renderer failure, so the documented risk is understated.

### Root Cause

The documentation simplifies the app model in a way that hides a real in-process recovery path.

### Fix Steps

1. Update the summary language to explain that Daedalus is single-window in normal operation but still recreates the main window after renderer failure.
2. Reframe the stale-window issue as a real recovery-path bug rather than a mostly theoretical edge.

### Acceptance Criteria

- The hardening summary accurately describes the risk and recovery path.

---

## Fix 20 — Align documentation wording with the current rejected-path behavior

| Field | Value |
|-------|-------|
| **Severity** | Low |
| **Effort** | Small (15 min) |
| **Files** | `.agent/plans/mithril/mithril-snapshot-ux-changelog.md`, `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`, `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx`, `source/renderer/app/components/chain-storage/chainStorageUtils.ts` |

### Problem

The changelog and hardening summary currently describe the rejected-path picker fix as if the UI now always shows users exactly what they attempted. The implementation is better than before, but still rewrites some invalid paths through the managed-path display helper.

### Root Cause

The documentation reflects the intended UX end state, not the exact current behavior.

### Fix Steps

1. If Fix 18 is implemented first, update the docs to describe the fully corrected behavior.
2. If Fix 18 is not yet implemented, tone down the wording so the docs describe the current partial preservation accurately.
3. Keep changelog and hardening summary aligned so future review does not assume the UX issue is already fully closed.

### Acceptance Criteria

- Documentation matches the actual runtime behavior.
- Future reviewers can tell whether exact rejected-path display is fully fixed or still pending.

---

## Next Execution Order

Recommended implementation sequence for the follow-up findings:

1. **Fix 16** (highest runtime risk, small)
2. **Fix 17** (real recovery-path regression, medium)
3. **Fix 18** (finishes the picker UX hardening)
4. **Fix 19 + Fix 20** (documentation alignment after code behavior is settled)
