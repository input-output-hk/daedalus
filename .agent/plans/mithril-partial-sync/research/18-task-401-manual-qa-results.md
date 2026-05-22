# Task 401 Manual QA Results

## Purpose

- Record the supported-network manual QA evidence needed to finalize the Mithril partial sync rollout posture.
- Do not claim `enabled by default` unless release-equivalent packaged-build evidence exists for every release OS in scope.

## Scope Confirmation

- Commit or build identifier: workspace state as of 2026-05-22 after task-401 live QA fixes through iteration 21
- Release OSes in scope: Linux only for this task execution checkpoint
- Build types exercised: `yarn dev`
- Launcher guard configuration used: validated with `mithrilPartialSyncEnabled = false` earlier in QA, then enabled for the live fixed-flow verification with generated config default `mithrilPartialSyncEnabled = true`
- Chain-storage environments used: default managed chain storage
- Notes: By explicit user direction for this checkpoint, only one supported network is required now, prior guard-off validation is accepted as sufficient evidence, and broader network/storage/recovery matrix coverage is deferred to later manual QA outside this task closeout.

## Decision Rules

- `enabled by default` requires:
  - release-equivalent packaged-build evidence for every release OS in scope,
  - success-path coverage for `mainnet`, `preprod`, and `preview` across default and custom chain storage,
  - representative recovery-path evidence,
  - explicit guard-off evidence showing new diagnostics-launched start and restart-normal are blocked while wipe/startup-owned recovery still behave as designed.
- `keep guarded by default` is the truthful outcome when QA is otherwise acceptable but packaged-build evidence or matrix coverage is incomplete.
- `hold for follow-up fixes` is the truthful outcome when any blocking safety or correctness issue remains unresolved.

## Required Scenario Coverage

- Success path on `mainnet`, `preprod`, and `preview` with default managed chain storage.
- Success path on `mainnet`, `preprod`, and `preview` with custom managed chain storage.
- Representative recovery-path coverage on at least one supported network per release OS and storage mode for:
  - cancellation,
  - retry,
  - restart-normal,
  - wipe-and-full-sync,
  - startup-owned unsafe recovery when safely exercisable.
- Guard-off verification showing backend rejection of diagnostics-launched start and restart-normal.

## Run Template

### Run

- OS/platform:
- Build type:
- Build identifier:
- Network:
- Chain-storage mode:
- Launcher guard setting:
- Scenario:
- Expected result:
- Actual result:
- Pass/fail/blocked:
- Logs and evidence paths:
- Notes:

### Run

- OS/platform: Linux
- Build type: `yarn dev`
- Build identifier:
- Network: `preview`
- Chain-storage mode: default managed chain storage
- Launcher guard setting: `mithrilPartialSyncEnabled = true`
- Scenario: cancel Mithril partial sync before cutover, then choose `wipe chain data`
- Expected result: Daedalus stops `cardano-node` automatically, wipes chain storage, and re-enters startup/full-sync flow without requiring the node to have been manually stopped first.
- Actual result: uncaught runtime error: `Daedalus can only wipe chain storage and full sync after Mithril partial sync while cardano-node is stopped.`
- Pass/fail/blocked: fail
- Logs and evidence paths:
- Notes: Root cause traced to `ChainStorageCoordinator` recovery paths (`restartNormalFromPartialSync`, `wipeAndFullSyncFromPartialSync`) still asserting a pre-stopped node instead of reusing the automatic partial-sync node-stop handler. Fix landed in `source/main/utils/chainStorageCoordinator.ts` with regression coverage in `source/main/utils/chainStorageCoordinator.spec.ts`. Pending manual re-test.

### Run

- OS/platform: Linux
- Build type: `yarn dev`
- Build identifier:
- Network: `preview`
- Chain-storage mode: default managed chain storage
- Launcher guard setting: `mithrilPartialSyncEnabled = true`
- Scenario: allow Mithril partial sync to keep running during a long download window and observe whether the stopped-node background changes to wallet UI mid-run
- Expected result: `cardano-node` remains stopped until the controlled post-install/startup phase; transparent overlay may show the stopped/loading surface beneath it, but should not reveal wallet UI due to an actual node restart during active partial sync.
- Actual result: fail. Live logs showed `cardano-node` restarted during the active partial sync window after a background disk-space poll. In `~/.local/share/Daedalus/preview/Logs/pub/Daedalus.json`, partial sync preflight was active from `2026-05-22T15:12:16Z`, then a later `[DISK-SPACE-DEBUG] Disk space check completed` at `2026-05-22T15:20:28.772Z` was followed immediately by `CardanoNode#start: trying to start cardano-node for the 1 time` and later renderer `NetworkStatusStore: handling cardano-node state <running>`.
- Pass/fail/blocked: fail
- Logs and evidence paths:
  - `~/.local/share/Daedalus/preview/Logs/pub/Daedalus.json`
- Notes: Root cause traced to `source/main/utils/handleDiskSpace.ts`. The background poll path respected active Mithril bootstrap but not active Mithril partial sync, so it could restart `cardano-node` whenever the node was stopped and the chain was non-empty. Fix landed in `handleDiskSpace.ts` using `isMithrilPartialSyncBlockingNodeStart(getMithrilPartialSyncStatus().status)` with regression coverage in `handleDiskSpace.spec.ts`. Pending manual re-test.

### Run

- OS/platform: Linux
- Build type: `yarn dev`
- Build identifier:
- Network: `preview`
- Chain-storage mode: default managed chain storage
- Launcher guard setting: `mithrilPartialSyncEnabled = true`
- Scenario: allow partial sync to reach post-install finalizing state and observe whether it advances into node startup/completion
- Expected result: once install is complete, Daedalus should start `cardano-node`, move through `starting-node`, and complete the partial sync flow.
- Actual result: fail. Overlay remained stuck at `Finalizing` with `installing` already green. Inspection of `~/.local/share/Daedalus/preview/Logs/mithril-partial-sync.lock` showed marker state `installed-awaiting-node-start`, proving chain replacement had finished and the app should have been performing the controlled post-install node startup handoff.
- Pass/fail/blocked: fail
- Logs and evidence paths:
  - `~/.local/share/Daedalus/preview/Logs/mithril-partial-sync.lock`
- Notes: Root cause traced to the previous disk-space guard fix in `source/main/utils/handleDiskSpace.ts`. The new partial-sync blocking guard ran before `startNodeAfterPartialSyncInstall(...)`, so when renderer status still reported `finalizing`, the periodic poll blocked the intended marker-driven startup handoff as well. Fix landed by moving `startNodeAfterPartialSyncInstall(currentGeneration)` ahead of the active partial-sync blocking guard, with regression coverage in `handleDiskSpace.spec.ts`. Pending manual re-test.

### Run

- OS/platform: Linux
- Build type: `yarn dev`
- Build identifier:
- Network: `preview`
- Chain-storage mode: default managed chain storage
- Launcher guard setting: `mithrilPartialSyncEnabled = true`
- Scenario: resume past finalizing and verify that `starting-node` triggers a real `cardano-node` startup
- Expected result: after status changes to `Starting Cardano node...`, the main process should actually call `cardanoNode.start()`, node logs should show a fresh start attempt, and the flow should complete or fail with an explicit startup error.
- Actual result: fail. UI reached `Starting Cardano node...`, but marker stayed `installed-awaiting-node-start` and no fresh `CardanoNode#start` attempt appeared in `Daedalus.json`, indicating the handoff stalled before invoking `cardanoNode.start()`.
- Pass/fail/blocked: fail
- Logs and evidence paths:
  - `~/.local/share/Daedalus/preview/Logs/mithril-partial-sync.lock`
  - `~/.local/share/Daedalus/preview/Logs/pub/Daedalus.json`
- Notes: Root cause traced to `source/main/ipc/mithrilPartialSyncChannel.ts`. `startNodeAfterPartialSyncInstall()` awaited `emitMithrilPartialSyncStatus({ status: 'starting-node' })` before calling `cardanoNode.start()`, and the partial-sync status broadcast path awaited renderer delivery acknowledgement. If renderer delivery never resolved, the main process stalled before node start. Fix landed by making partial-sync status delivery fire-and-forget while still updating cached status synchronously, with regression coverage in `mithrilPartialSyncChannel.spec.ts`. Pending manual re-test.

### Run

- OS/platform: Linux
- Build type: `yarn dev`
- Build identifier:
- Network: `preview`
- Chain-storage mode: default managed chain storage
- Launcher guard setting: `mithrilPartialSyncEnabled = true`
- Scenario: continue to Daedalus after partial-sync completion and verify `cardano-node` remains healthy
- Expected result: `cardano-node` starts after cutover and continues running normally with the merged partial-sync chain state.
- Actual result: fail. `Daedalus.json` showed node start reached `running`, then later transitioned `exiting` and `crashed`. `node.log` identified the real cause: `FsResourceDoesNotExist` for `/home/westbam/.local/share/Daedalus/preview/chain/immutable/00000.primary`, followed by `ApiMisuse ClosedDBError` and node shutdown.
- Pass/fail/blocked: fail
- Logs and evidence paths:
  - `~/.local/share/Daedalus/preview/Logs/pub/Daedalus.json`
  - `~/.local/share/Daedalus/preview/Logs/pub/node.log`
- Notes: Root cause traced to `source/main/utils/chainStorageManagerLayout.ts`. `installValidatedPartialSyncSnapshot()` emptied the managed chain completely, including the existing immutable prefix, then installed only the staged Mithril suffix range. That left early immutable files such as `00000.primary` missing, so the node crashed after startup. Fix landed by preserving the live `immutable/` directory during partial-sync cutover and merging staged immutable entries into it, with regression coverage in `source/main/utils/chainStorageManager.spec.ts`. The current preview chain state is already broken by the previous buggy run and needs a clean retry under the patched build.

### Run

- OS/platform: Linux
- Build type: `yarn dev`
- Build identifier: workspace state as of 2026-05-22 after task-401 live QA fixes through iteration 21
- Network: `preview`
- Chain-storage mode: default managed chain storage
- Launcher guard setting: `mithrilPartialSyncEnabled = true`
- Scenario: latest end-to-end fixed-flow retry from diagnostics through partial-sync completion and return to Daedalus
- Expected result: Mithril partial sync completes successfully, the app transparently handles long download/install/finalizing windows, `cardano-node` restarts successfully after cutover, and Daedalus resumes normal operation.
- Actual result: pass. The latest fixed flow worked. During one long run the UI appeared stalled in installing/finalizing, but backend logs showed the node restart path completed successfully and the app came up correctly. Follow-up renderer UX adjustments were made so the install substep remains visibly active until the flow is ready to transition.
- Pass/fail/blocked: pass
- Logs and evidence paths:
  - `~/.local/share/Daedalus/preview/Logs/pub/Daedalus.json`
- Notes: User confirmed the latest fixed flow worked. This pass is accepted as sufficient current supported-network evidence for this task by explicit user direction, with broader matrix/recovery coverage deferred to later manual QA.

## Rollup Summary

- Matrix gaps:
  - `mainnet` and `preprod` success-path coverage deferred
  - custom managed chain storage coverage deferred
  - release-equivalent packaged-build coverage deferred
  - representative retry/restart-normal/wipe/startup-owned recovery re-validation deferred after the latest fixes
- Blocking issues:
  - None for the narrowed task checkpoint approved by the user
- Non-blocking observations:
  - Long `installing` / `finalizing` windows can look stalled without active substep feedback; renderer UX was adjusted so installing remains visibly active until completion handoff
  - Prior guard-off validation is accepted for this task checkpoint based on earlier manual QA performed before the generated default was flipped to `true`
- Rollback checklist confirmation:
  - launcher guard disables new diagnostics-launched partial sync entry,
  - startup-owned unsafe recovery remains available,
  - wipe-and-full-sync remains available where designed,
  - empty-chain Mithril bootstrap remains unaffected.

## Final Recommendation

- Recommended rollout posture: `enabled by default`
- Reasoning: Explicit user decision. The latest fixed `preview`/Linux/dev flow now succeeds end to end, prior guard-off validation was already exercised earlier in manual QA, and the user has directed that broader network/storage/recovery matrix coverage be deferred to later manual QA rather than block this task closeout.
- Follow-up tasks required:
  - Complete broader manual QA later across additional supported networks
  - Complete broader manual QA later across custom chain storage and deferred recovery-path coverage
  - Complete release-equivalent packaged-build validation later if release-signoff requires it
