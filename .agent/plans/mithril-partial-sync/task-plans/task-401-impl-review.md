Implementation: Iteration 1
Timestamp: 2026-05-21T21:27:33Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Prepared the repo-local manual QA evidence template at `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.
- Updated the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-401.md` to mark build status as `in_progress` and record that the task is now waiting on manual execution evidence.
- Re-verified the core rollout seams against live files before the handoff:
  - backend guard remains in `source/main/utils/chainStorageCoordinator.ts`,
  - diagnostics CTA and confirmation entry remain in `source/renderer/app/components/status/DaedalusDiagnostics.tsx`,
  - renderer recovery affordances still come from backend-owned `allowedRecoveryActions` in `source/renderer/app/stores/MithrilPartialSyncStore.ts`.

Files touched:
- `.agent/plans/mithril-partial-sync/task-plans/task-401.md`
- `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`

Verification run:
- Read required docs/workflows: `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`.
- Loaded `understand` guidance, then verified live guard/CTA/recovery claims with repo reads and targeted content searches.
- Confirmed no existing implementation review log existed before appending this entry.

Deviations from approved plan:
- None. This is the planned documentation-preparation and user handoff phase for a `manual_execution` task.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- `task-401` is a manual-execution task. Truthful completion depends on supported-network, release-representative QA that cannot be executed by the agent in this environment.

Exact manual steps:
1. Confirm the release OS matrix and packaged builds in scope for this rollout decision.
2. For each release OS in scope, run at least one release-equivalent packaged build.
3. Execute success-path partial sync runs on `mainnet`, `preprod`, and `preview` for both default managed chain storage and custom managed chain storage.
4. On at least one representative supported network per release OS and storage mode, exercise recovery scenarios as safely possible: cancellation, retry, restart-normal, wipe-and-full-sync, and startup-owned unsafe recovery if feasible.
5. Validate both launcher-guard states:
   - guard on: diagnostics confirmation and partial-sync flow can start normally,
   - guard off: diagnostics CTA may still render, but backend start and `restart-normal` are rejected while wipe/startup-owned recovery remain available where designed.
6. Record each run in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md` using the provided template.
7. Add log paths, screenshots, and short notes for any failures, blocked runs, or unexpected recovery behavior.
8. State the final rollout recommendation in that same file: `enabled by default`, `keep guarded by default`, or `hold for follow-up fixes`.

Expected results:
- The results file contains enough evidence to prove one truthful rollout posture.
- Any `enabled by default` recommendation is backed by release-equivalent packaged-build evidence for every release OS in scope.
- Guard-off behavior shows new diagnostics-launched start/restart-normal are blocked without regressing startup-owned unsafe recovery.

What output or decision the user should return:
- Return after the manual QA runs are complete with:
  - confirmation that `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md` has been filled in,
  - any additional notes if a run was blocked or if follow-up fixes are needed,
  - the intended final rollout posture if it is not already explicit in the results file.

Whether work is blocked or can continue in parallel:
- Blocked on user or operator manual QA evidence. No truthful review or completion step can proceed until that evidence exists.

Outcome: Manual QA handoff prepared; waiting for user execution evidence

Implementation: Iteration 2
Timestamp: 2026-05-21T21:38:20Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Flipped the generated launcher-config default to enable Mithril partial sync by default by setting `mithrilPartialSyncEnabled = true` in `nix/internal/launcher-config.nix`.
- Kept the implementation deliberately narrow: this changes the generated launcher config default used by the dev shell and packaged launcher-config generation, without changing the backend guard semantics themselves.

Files touched:
- `nix/internal/launcher-config.nix`

Verification run:
- Re-read the existing implementation review log before continuing.
- Verified the live launcher-config generation seam in `nix/internal/launcher-config.nix` and `nix/devshells.nix`.
- Verified the resulting diff sets `mithrilPartialSyncEnabled = true` in the generated launcher config.

Deviations from approved plan:
- This is a user-directed rollout-posture change before manual QA evidence is complete. It intentionally makes the generated default opt-in state easier for QA, while the task remains blocked on truthful manual validation and final signoff.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The config default change is in place, but manual QA still must be executed and recorded to complete `task-401` truthfully.

Exact manual steps:
1. Exit the current dev shell if it is still running.
2. Re-enter the target Nix shell, for example `yarn nix:mainnet`, `yarn nix:preprod`, or `yarn nix:preview`.
3. Run `yarn dev` again so the regenerated `LAUNCHER_CONFIG` picks up the new default.
4. Confirm Mithril partial sync now starts from diagnostics without the previous launcher-config-disabled error.
5. Continue the supported-network QA runs and record them in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- `yarn dev` uses a generated launcher config with `mithrilPartialSyncEnabled: true`.
- The previous `Mithril partial sync is disabled by launcher configuration.` runtime error no longer occurs when starting partial sync from diagnostics.

What output or decision the user should return:
- Return with:
  - whether the relaunch cleared the launcher-config-disabled error,
  - any new issue encountered,
  - and, when available, the completed manual QA results file or a summary of the recorded outcomes.

Whether work is blocked or can continue in parallel:
- Blocked on user relaunch + manual QA evidence.

Outcome: Default launcher-config generation updated for QA convenience; waiting for user relaunch and manual evidence

Implementation: Iteration 3
Timestamp: 2026-05-21T21:41:25Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Fixed a live orchestration bug where diagnostics-launched Mithril partial sync incorrectly required `cardano-node` to already be stopped instead of stopping it automatically first.
- Added a dedicated coordinator stop seam in `source/main/utils/chainStorageCoordinator.ts` via `setPartialSyncNodeStopHandler(...)` and `_ensureNodeStoppedForPartialSync(...)`.
- Updated `startPartialSync(...)` to stop the node first when current state is `running` or `stopping`, then continue preflight using `stopped` state.
- Wired the real stop behavior in `source/main/index.ts` so partial sync start now calls `cardanoNode.stop()` before restore work begins.
- Updated focused Jest coverage in `source/main/utils/chainStorageCoordinator.spec.ts` for the new running/stopping-node start path and adjusted the IPC channel test double for the new coordinator seam.

Files touched:
- `source/main/utils/chainStorageCoordinator.ts`
- `source/main/index.ts`
- `source/main/utils/chainStorageCoordinator.spec.ts`
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts`

Verification run:
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts`
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts`

Deviations from approved plan:
- None in product intent. This fixes repo behavior to match the locked decision that `cardano-node` stops automatically before partial sync starts.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The implementation bug is fixed locally, but the manual QA run must be retried in your environment to verify the end-to-end start path.

Exact manual steps:
1. If `yarn dev` is still running, restart it so the rebuilt main-process bundle is loaded.
2. Re-open diagnostics and trigger Mithril partial sync again.
3. Confirm the previous error `Daedalus can only start Mithril partial sync while cardano-node is stopped.` no longer appears.
4. If the flow proceeds, continue the planned QA scenarios and record the results in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.
5. If a new error appears, return the exact text and stack.

Expected results:
- Diagnostics-launched partial sync stops `cardano-node` automatically before preflight proceeds.
- The prior stopped-node assertion no longer blocks the start path.

What output or decision the user should return:
- Return with whether the retry succeeded, or paste the next exact error if another issue appears.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Automatic node-stop bug fixed; waiting for user retry and continued QA evidence

Implementation: Iteration 4
Timestamp: 2026-05-21T22:07:12Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Fixed the renderer handoff bug where diagnostics confirmation could remain visible after `cardano-node` stopped, making Mithril partial sync appear stuck before `preparing`.
- Updated `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` so `stopping-node` is now treated as a real overlay-backed progress state.
- Updated `source/renderer/app/stores/MithrilPartialSyncStore.ts` so `stopping-node` counts as a display status and immediately drives the global overlay.
- Updated focused renderer tests to match the intended handoff behavior in:
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts`
  - `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Files touched:
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Verification run:
- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `yarn test:jest source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts`
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Deviations from approved plan:
- None. This aligns the renderer with the existing partial-sync start semantics after the node-stop orchestration fix.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The local code is fixed, but the partial-sync start path must be retried in your running app to confirm the overlay handoff works end to end.

Exact manual steps:
1. Restart `yarn dev` so the renderer and main-process bundles both refresh.
2. Open diagnostics and trigger Mithril partial sync again.
3. Confirm that after clicking `Start Mithril partial sync`, the diagnostics confirmation view is replaced by the Mithril partial sync overlay instead of remaining stuck on the confirmation screen.
4. If the overlay appears and progress advances, continue QA and record the run in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.
5. If another issue appears, return the exact behavior and any error text.

Expected results:
- The confirmation view hands off immediately to the global overlay during `stopping-node`.
- Partial sync no longer looks stalled on the confirmation page once the node is stopped.

What output or decision the user should return:
- Return with whether the overlay now appears and whether partial sync proceeds, or describe the next failure precisely.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Renderer handoff fixed for stopping-node overlay takeover; waiting for user retry and continued QA evidence

Implementation: Iteration 5
Timestamp: 2026-05-21T22:18:50Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Fixed the partial-sync progress UX by enabling the existing combined download progress bar in `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` instead of suppressing it.
- Fixed the cancelled dead-end by updating `source/main/mithril/MithrilPartialSyncService.ts` so successful pre-cutover cancellation exposes backend-owned recovery actions: `retry`, `restart-normal`, and `wipe-and-full-sync`.
- Updated focused tests to lock both behaviors in:
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`

Files touched:
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`

Verification run:
- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts`

Deviations from approved plan:
- None. These are direct QA fixes for the selected task.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The fixes are local and tested, but the running app must be retried to verify the real UX in your environment.

Exact manual steps:
1. Restart `yarn dev`.
2. Start Mithril partial sync again from diagnostics.
3. Confirm the overlay now shows visible combined download progress while the download is running.
4. Cancel again before cutover and confirm the cancelled screen now offers recovery actions instead of trapping you.
5. Record the observed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- The partial-sync overlay shows a download progress bar/details while downloading.
- The cancelled terminal state shows actionable buttons so you can leave that state via retry, restart normally, or wipe/full-sync.

What output or decision the user should return:
- Return with whether both UX issues are fixed, or describe any remaining issue precisely.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Download progress surfaced and cancelled-state recovery actions restored; waiting for user retry and continued QA evidence

Implementation: Iteration 6
Timestamp: 2026-05-21T22:26:24Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Fixed the remaining partial-sync progress-bar bug in `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`.
- Root cause: the renderer only showed the combined download progress bar when the active downloading item used bootstrap substep id `step-3`, but partial sync publishes a top-level active item with id `downloading`.
- Updated the bar-render condition so the combined progress bar also appears when partial sync is in top-level `downloading` state and download metrics are present.
- Added focused regression coverage in `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx` for the partial-sync top-level downloading case without bootstrap substeps.

Files touched:
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx`

Verification run:
- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx`

Deviations from approved plan:
- None. This is a direct QA-driven renderer fix.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The final progress-bar renderer fix is local and tested, but needs a live retry in your app session.

Exact manual steps:
1. Restart `yarn dev`.
2. Start Mithril partial sync again from diagnostics.
3. Confirm the downloading screen now shows the combined progress bar/details while the download is active.
4. Continue QA and record the result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- The downloading overlay shows a visible combined progress bar during partial sync, even though partial sync uses top-level `downloading` progress items rather than bootstrap substeps.

What output or decision the user should return:
- Return with whether the progress bar is now visible in the live app, or describe any remaining mismatch precisely.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Partial-sync top-level downloading progress bar path fixed; waiting for user retry and continued QA evidence

Implementation: Iteration 7
Timestamp: 2026-05-21T22:31:04Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Fixed incorrect partial-sync download metric formatting in `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`.
- Root cause: partial sync `filesDownloaded/filesTotal` represent snapshot file counts from Mithril JSON progress output, but the renderer was formatting those counts as byte sizes. That produced misleading values like `25.4 KB total`.
- Updated the combined progress detail formatter so snapshot progress now renders as numeric file counts while ancillary fast-sync progress still renders as byte sizes.
- Added focused regression coverage in `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx` to lock that mixed-format behavior.

Files touched:
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx`

Verification run:
- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx`

Deviations from approved plan:
- None. This is a direct QA-driven correctness fix.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The metric-formatting fix is local and tested, but the live app needs one more retry to confirm the displayed text now matches real Mithril progress semantics.

Exact manual steps:
1. Restart `yarn dev`.
2. Start Mithril partial sync again on preview.
3. Confirm the overlay shows snapshot progress as file counts rather than byte sizes, while fast-sync ancillary transfer remains byte-sized.
4. Record the confirmed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- The previous misleading `25.4 KB`-style snapshot total is replaced by a file-count total.
- Fast-sync ancillary transfer still shows byte-sized values.

What output or decision the user should return:
- Return with whether the metric display now looks truthful, or paste the remaining mismatch.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Partial-sync snapshot metrics corrected from byte-size formatting to file-count formatting; waiting for user retry and continued QA evidence

Implementation: Iteration 8
Timestamp: 2026-05-21T22:41:39Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Investigated the live preview verification failure `Mithril partial sync staged output has an invalid clean entry.` against the actual staged filesystem and the partial-sync log.
- Confirmed the download and Mithril verification completed successfully; the failure was in Daedalus post-download validation, not in the Mithril transfer itself.
- Root cause: `source/main/mithril/MithrilPartialSyncService.ts` hard-coded `download/db/clean` as a required directory, but the real staged output produced by Mithril on preview is an empty file named `clean`.
- Updated `_validateStagedDownloadOutput(...)` to require `clean` as a file instead of a directory.
- Updated `source/main/mithril/MithrilPartialSyncService.spec.ts` fixtures so staged validation matches the live on-disk shape.

Live evidence consulted:
- `/home/westbam/.local/share/Daedalus/preview/Logs/mithril-partial-sync.log`
- `/home/westbam/.local/share/Daedalus/preview/mithril-partial-sync/download/db`
- `/home/westbam/.local/share/Daedalus/preview/mithril-partial-sync/download/db/clean`
- `/home/westbam/.local/share/Daedalus/preview/mithril-partial-sync/download/db/immutable`
- `/home/westbam/.local/share/Daedalus/preview/mithril-partial-sync/download/db/ledger`

Files touched:
- `source/main/mithril/MithrilPartialSyncService.ts`
- `source/main/mithril/MithrilPartialSyncService.spec.ts`

Verification run:
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts`

Deviations from approved plan:
- None. This is a direct QA-discovered correctness fix in the selected task.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The validation fix is local and tested, but the full partial-sync path must be retried in your environment to confirm preview now proceeds past verifying.

Exact manual steps:
1. Restart `yarn dev`.
2. Start Mithril partial sync again on preview.
3. Let it proceed through downloading and verifying.
4. Confirm whether it now advances past the previous `invalid clean entry` failure.
5. Record the result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- The previous preview verification failure caused by `clean` entry type mismatch no longer occurs.
- Partial sync proceeds into the later conversion/installing/finalizing phases, or surfaces a different concrete issue if another live compatibility problem remains.

What output or decision the user should return:
- Return with whether preview now gets past the former verification failure, or paste the next exact failure if one appears.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Live preview staged-layout validation corrected for `clean` file output; waiting for user retry and continued QA evidence

Implementation: Iteration 9
Timestamp: 2026-05-22T11:31:17Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Investigated the new renderer runtime overlay failure that showed only `[object Object]` when starting Mithril partial sync.
- Root cause: `source/renderer/app/stores/MithrilPartialSyncStore.ts` optimistically seeded `stopping-node`, then awaited `mithrilPartialSyncStartChannel.request()`. When main rejected with a plain IPC object, that object could escape uncaught into webpack-dev-server before the store finished syncing backend status.
- Updated `startPartialSync()` so it captures raw start-request rejections, always refreshes backend status, and only throws a real generic `Error` if status is still stuck in the optimistic pending state after sync.
- Added focused regression coverage in `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` to ensure raw IPC object rejections are absorbed once backend status reports a concrete failure state.

Files touched:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Verification run:
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Deviations from approved plan:
- None. This is a direct QA/dev-loop reliability fix for the selected task.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The renderer-side uncaught-object fix is local and tested, but you need to retry the start flow in the live dev app to confirm webpack overlay no longer masks the actual backend state.

Exact manual steps:
1. Restart `yarn dev`.
2. Start Mithril partial sync again.
3. Confirm the webpack overlay no longer shows raw `[object Object]`.
4. If start still fails, note the actual in-app Mithril state or error card that appears instead.
5. Record the observed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- The dev overlay no longer crashes with `[object Object]` for Mithril partial sync start failures.
- The app instead settles into the backend-reported Mithril status or failure UI.

What output or decision the user should return:
- Return with whether the raw `[object Object]` overlay is gone, and what the app now shows instead.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Renderer start-path now absorbs raw IPC object rejections and syncs into backend-reported Mithril status; waiting for user retry and continued QA evidence

Implementation: Iteration 10
Timestamp: 2026-05-22T11:35:44Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Investigated why webpack-dev-server still showed `[object Object]` after the store-side rejection handling fix.
- Root cause: the diagnostics confirmation button invoked `onStartMithrilPartialSync()` without handling its returned promise, so any rejected async start still surfaced as an unhandled promise rejection at the React click-handler boundary before the store-side status sync could mask it.
- Updated `source/renderer/app/components/status/DaedalusDiagnostics.tsx` so the confirmation button wraps `onStartMithrilPartialSync()` in `Promise.resolve(...).catch(...)` and logs the rejection instead of leaking it to the dev overlay.
- Added focused regression coverage in `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` to ensure rejected start promises from the confirmation action are caught locally.

Files touched:
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`

Verification run:
- `yarn test:jest source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Deviations from approved plan:
- None. This is a direct QA/dev-loop reliability fix for the selected task.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The second renderer unhandled-promise path is fixed locally and tested, but you need to retry in the live dev app to confirm webpack overlay no longer shows raw `[object Object]`.

Exact manual steps:
1. Restart `yarn dev`.
2. Start Mithril partial sync again from diagnostics.
3. Confirm webpack overlay no longer appears with `[object Object]`.
4. If start still fails, note the actual app-level Mithril state or error card that appears instead.
5. Record the observed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- The confirmation click path no longer leaks rejected Mithril start promises to webpack-dev-server.
- The app settles into backend-reported Mithril status or failure UI instead of showing the raw dev overlay.

What output or decision the user should return:
- Return with whether the `[object Object]` overlay is now gone, and what app state appears instead.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Diagnostics confirmation click now catches rejected Mithril start promises locally; waiting for user retry and continued QA evidence

Implementation: Iteration 11
Timestamp: 2026-05-22T11:44:37Z

Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

Changes made:
- Investigated the full dev log for the persistent `[object Object]` webpack overlay.
- Confirmed the overlay was no longer coming from Mithril start IPC directly. The live log showed the real trigger during partial sync startup was renderer wallet polling: `AdaApi::getWallets error` fired exactly while partial sync intentionally stopped `cardano-wallet` and `cardano-node`.
- Root cause: `source/renderer/app/stores/WalletsStore.ts` `refreshWalletsData()` awaited `this.walletsRequest.execute().promise` without a local `catch`, so the expected wallet API failure during backend shutdown escaped as an unhandled rejection and surfaced as webpack-dev-server `[object Object]`.
- Updated `refreshWalletsData()` to catch wallet-refresh failures, log them as expected refresh failures, and return early instead of leaking a runtime rejection while partial sync is taking control of the backend.
- Also cleaned up incidental renderer issues introduced during prior debugging so focused renderer tests remain green.

Files touched:
- `source/renderer/app/stores/WalletsStore.ts`
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`

Verification run:
- `yarn test:jest source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Live evidence consulted:
- The provided `yarn dev` main/renderer log, especially:
  - `[MITHRIL] Stopping cardano-node before partial sync start`
  - `wallet: Service.stop: trying to stop cardano-wallet`
  - `AdaApi::getWallets error`
  - clean Mithril preflight / download command startup immediately after backend stop

Deviations from approved plan:
- None. This is a direct QA/dev-loop reliability fix for the selected task.

User interaction is now required:
- yes

User Handoff:
Why user interaction is required now:
- The log-verified wallet-refresh rejection path is fixed locally, but you need to retry in the live dev app to confirm the webpack overlay is finally gone during the intentional wallet/node shutdown window.

Exact manual steps:
1. Restart `yarn dev`.
2. Start Mithril partial sync again from diagnostics.
3. Watch the moment when partial sync stops `cardano-wallet` and `cardano-node`.
4. Confirm webpack overlay no longer appears with `[object Object]` during that shutdown/startup handoff.
5. If the app still fails, capture the new exact visible app state or error text.
6. Record the observed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

Expected results:
- Wallet polling failures during the intentional backend shutdown no longer surface as uncaught runtime overlay errors.
- The app continues into the Mithril partial sync flow instead of showing webpack `[object Object]`.

What output or decision the user should return:
- Return with whether the `[object Object]` overlay is now gone, and what the app does next.

Whether work is blocked or can continue in parallel:
- Blocked on user retry and QA evidence.

Outcome: Unhandled wallet-refresh rejection during intentional backend shutdown is now guarded; waiting for user retry and continued QA evidence

Implementation: Iteration 13
Timestamp: 2026-05-22T14:49:51Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- Investigated the newest live log and confirmed the remaining renderer-side failure still correlates with wallet shutdown during partial sync start, with `AdaApi::getWallets error` emitted after the intentional wallet/node stop begins.
- Traced additional direct wallet-loading paths and found page-navigation analytics remained a likely unhandled promise source: `TrackedRoute` calls `analytics.sendPageNavigationEvent(pageTitle)` fire-and-forget, while `MatomoClient.sendPageNavigationEvent()` awaits `getCustomDimensions()`, which had historically called `adaApi.getWallets()`.
- Updated `source/renderer/app/analytics/MatomoAnalyticsTracker.ts` so both `sendPageNavigationEvent(...)` and `sendEvent(...)` catch and log analytics client promise failures instead of letting them surface as unhandled renderer rejections.
- Retained the previously added guards in `WalletsStore.refreshWalletsData()`, `WalletsStore._pollRefresh()`, and `getCustomDimensions()` so explicit wallet refreshes, polling refreshes, and analytics dimension collection all degrade safely during intentional backend shutdown.
Files touched:

- `source/renderer/app/analytics/MatomoAnalyticsTracker.ts`
- `source/renderer/app/analytics/getCustomDimensions.ts`
- `source/renderer/app/stores/WalletsStore.ts`
Verification run:

- `yarn test:jest source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts`
Deviations from approved plan:

- None. This remains QA-driven shutdown/error-path hardening for the selected task.

User interaction is now required:
- yes

User Handoff:
- Why user interaction is required now: the remaining likely unhandled analytics promise path is now guarded, but the live app must be retried to see whether the webpack overlay is finally gone during the shutdown handoff.
- Exact manual steps:
  1. Restart `yarn dev`.
  2. Start Mithril partial sync again.
  3. Watch the shutdown handoff when `cardano-wallet` and `cardano-node` stop.
  4. Confirm whether the webpack overlay still appears.
  5. If it still appears, capture the exact renderer-side stack from devtools so the final remaining caller can be identified directly instead of inferred from main logs.
  6. Record the observed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.
- Expected results: wallet refresh and analytics promise failures during intentional backend shutdown no longer surface as uncaught runtime overlay errors, and the app continues into the Mithril partial sync flow.

- What output or decision the user should return: whether the overlay is now gone; if not, the exact devtools renderer stack for the remaining uncaught promise.

- Whether work is blocked or can continue in parallel: blocked on user retry and QA evidence.

Outcome: Additional analytics promise rejection path is now guarded; waiting for user retry and, if needed, renderer devtools stack evidence

Implementation: Iteration 14
Timestamp: 2026-05-22T14:54:07Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User confirmed the webpack runtime overlay is now gone; Mithril partial sync proceeds into downloading.
- The remaining signal from devtools is handled renderer logging only: `AdaApi::getWallets error` with an empty object payload during the intentional wallet shutdown window.
- Investigated `ApiError` logging behavior and confirmed this shutdown-time wallet fetch now appears to be expected, handled noise rather than an uncaught runtime failure.
- Updated `source/renderer/app/api/api.ts` so `AdaApi::getWallets` logs this opaque empty-object shutdown case at `warn` instead of `error`, reducing false-positive noise while preserving real error logging for actionable wallet failures.
Files touched:

- `source/renderer/app/api/api.ts`
- `source/renderer/app/analytics/MatomoAnalyticsTracker.ts`
- `source/renderer/app/analytics/getCustomDimensions.ts`
- `source/renderer/app/stores/WalletsStore.ts`
Verification run:

- `yarn test:jest source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts`
Deviations from approved plan:

- None. This closes out dev-only shutdown-noise cleanup around the selected task.

User interaction is now required:
- yes

User Handoff:
- Why user interaction is required now: the uncaught renderer overlay problem is resolved, so manual QA can continue on the actual partial sync flow.
- Exact manual steps:
  1. Continue the preview partial sync run.
  2. Confirm whether it completes successfully or fails in a later stage.
  3. If it fails, capture the exact in-app Mithril error card and any related log path.
  4. Record the observed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.
- Expected results: the run continues without webpack overlay interruption, and subsequent QA findings now reflect real Mithril partial sync behavior rather than renderer-side shutdown noise.

- What output or decision the user should return: whether the preview run now completes, or the next real Mithril-stage failure if it does not.

- Whether work is blocked or can continue in parallel: blocked on user-run QA evidence.

Outcome: Runtime overlay issue resolved; wallet shutdown fetch noise downgraded; waiting for continued manual QA evidence from the live partial sync run

Implementation: Iteration 15
Timestamp: 2026-05-22T15:03:00Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User found another live QA bug after cancelling partial sync and selecting `wipe chain data`: recovery failed with `Daedalus can only wipe chain storage and full sync after Mithril partial sync while cardano-node is stopped.`
- Root cause: `startPartialSync` already used the dedicated coordinator node-stop handler, but `restartNormalFromPartialSync` and `wipeAndFullSyncFromPartialSync` still hard-required an already-stopped node state instead of reusing the same automatic stop orchestration.
- Updated `ChainStorageCoordinator` so partial-sync recovery actions now share the same automatic node-stop helper as partial-sync start.
- `wipeAndFullSyncFromPartialSync` now also threads the normalized post-stop node state through the managed-layout recheck before wiping chain data.
- Added focused coordinator regression tests covering `restart-normal` and `wipe-and-full-sync` recovery while the node is still running.

Files touched:
- `source/main/utils/chainStorageCoordinator.ts`
- `source/main/utils/chainStorageCoordinator.spec.ts`

Verification run:
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts`
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts source/main/mithril/MithrilPartialSyncService.spec.ts`

Deviations from approved plan:

- None. This is a direct bug fix discovered during planned manual QA.

User interaction is now required:

- yes

User Handoff:
- Why user interaction is required now: this bug only manifests in the live recovery flow after cancelling partial sync, so the patched behavior must be re-checked manually in preview.
- Exact manual steps:
  1. Restart `yarn dev` if needed so the patched main-process coordinator is loaded.
  2. Start Mithril partial sync from diagnostics.
  3. Cancel partial sync before cutover.
  4. Choose `wipe chain data`.
  5. Confirm that Daedalus now stops the node automatically, wipes chain data, and re-enters normal full-sync startup instead of throwing the stopped-node error.
  6. Record the observed result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

- Expected results: no uncaught runtime error; wipe-and-full-sync recovery should behave like partial sync start and handle node shutdown automatically.

- What output or decision the user should return: whether the recovery now succeeds, or the next exact failure if it still does not.

- Whether work is blocked or can continue in parallel: blocked on user-run QA evidence.

Outcome: Fixed partial-sync recovery orchestration so restart/wipe paths auto-stop the node; awaiting live QA confirmation

Implementation: Iteration 16
Timestamp: 2026-05-22T15:31:00Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User reported that during long-running Mithril partial sync download the transparent overlay background changed from the `cardano node stopped` loading state to wallet UI, raising suspicion that `cardano-node` had restarted mid-partial-sync.
- Investigation confirmed this was a real restart risk, not just a renderer illusion.
- Live evidence in `~/.local/share/Daedalus/preview/Logs/pub/Daedalus.json` showed:
  - partial sync preflight began at `2026-05-22T15:12:16Z`,
  - then a later disk-space poll at `2026-05-22T15:20:28Z`,
  - followed immediately by `CardanoNode#start: trying to start cardano-node for the 1 time` and `NetworkStatusStore: handling cardano-node state <running>` while the partial sync run was still active.
- Root cause: `handleDiskSpace.ts` had a background polling branch that restarted `cardano-node` whenever the node was stopped and the managed chain was non-empty. That branch already respected active Mithril bootstrap, but it did not guard against active Mithril partial sync states.
- Fixed `handleDiskSpace.ts` to read `getMithrilPartialSyncStatus()` and suppress startup whenever `isMithrilPartialSyncBlockingNodeStart(status)` is true.
- Added regression coverage to ensure the periodic disk-space check does not restart `cardano-node` during partial sync `downloading`.
- Updated the disk-space spec defaults so unrelated startup tests continue using an idle partial-sync mock state.

Files touched:
- `source/main/utils/handleDiskSpace.ts`
- `source/main/utils/handleDiskSpace.spec.ts`

Verification run:
- `yarn test:jest source/main/utils/handleDiskSpace.spec.ts`
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts source/main/mithril/MithrilPartialSyncService.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts`

Deviations from approved plan:

- None. This is a direct correctness fix found during supported-network manual QA.

User interaction is now required:

- yes

User Handoff:
- Why user interaction is required now: this issue manifested only during a long-running live preview partial sync, so the fix must be validated in the actual runtime flow.
- Exact manual steps:
  1. Restart `yarn dev` so the patched disk-space handler is loaded.
  2. Start Mithril partial sync on `preview` again.
  3. Let it run past the previous background-restart window.
  4. Confirm the background no longer transitions into wallet UI because `cardano-node` restarted mid-download.
  5. If possible, confirm via logs that no `CardanoNode#start` occurs until the controlled post-install partial-sync startup phase.
  6. Record the result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

- Expected results: the transparent overlay may still show the stopped-node loading surface beneath it, but it should not switch to wallet UI due to a real node restart during active partial sync download/verify/convert/install/finalize phases.

- What output or decision the user should return: whether the background remains on the stopped/loading surface through active partial sync, and whether the run now completes or fails later for a different reason.

- Whether work is blocked or can continue in parallel: blocked on live QA confirmation.

Outcome: Fixed disk-space poll so it cannot restart `cardano-node` during active Mithril partial sync; awaiting runtime confirmation

Implementation: Iteration 17
Timestamp: 2026-05-22T16:03:00Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User then reported a later live-run hang: the overlay showed `Finalizing` with `installing` already green, and the screen remained stuck instead of completing.
- Live inspection of `~/.local/share/Daedalus/preview/Logs/mithril-partial-sync.lock` showed the marker state had already advanced to `installed-awaiting-node-start`, which means chain replacement was done and Daedalus should have been starting `cardano-node`.
- Root cause: the previous disk-space guard fix correctly blocked unintended mid-download restarts, but the guard also ran before `startNodeAfterPartialSyncInstall(...)`. When the renderer still reported partial-sync `finalizing`, the periodic poll refused all starts, including the intended post-install handoff.
- Fixed `handleDiskSpace.ts` by checking `startNodeAfterPartialSyncInstall(currentGeneration)` before applying the active partial-sync blocking guard.
- This preserves the restart block for active pre-cutover stages while allowing the marker-driven `installed-awaiting-node-start` recovery/startup handoff to proceed.
- Added deterministic regression coverage proving that `installed-awaiting-node-start` still attempts the partial-sync startup handoff even when renderer status remains `finalizing`.

Files touched:
- `source/main/utils/handleDiskSpace.ts`
- `source/main/utils/handleDiskSpace.spec.ts`

Verification run:
- `yarn test:jest source/main/utils/handleDiskSpace.spec.ts`
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts source/main/mithril/MithrilPartialSyncService.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts`

Deviations from approved plan:

- None. This is a follow-up correctness fix from the same supported-network QA run.

User interaction is now required:

- yes

User Handoff:
- Why user interaction is required now: the currently stuck preview run needs to be resumed/restarted under the patched main-process logic, and the handoff must be rechecked live.
- Exact manual steps:
  1. Restart `yarn dev` so the patched disk-space handler is loaded.
  2. Let Daedalus resume startup with the existing `installed-awaiting-node-start` marker, or re-run partial sync if needed.
  3. Confirm the stuck `Finalizing` screen now advances into `starting-node` and then completes.
  4. Record the result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

- Expected results: once the patched app is running, a post-install partial-sync handoff is allowed to start `cardano-node` even if the overlay still reports `Finalizing` while polling catches up.

- What output or decision the user should return: whether the resumed/retried run now transitions from `Finalizing` to `starting-node`/success, or the next exact failure if not.

- Whether work is blocked or can continue in parallel: blocked on live QA confirmation.

Outcome: Fixed stuck finalizing state by allowing intended node startup after `installed-awaiting-node-start`; awaiting live confirmation

Implementation: Iteration 18
Timestamp: 2026-05-22T19:02:00Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User then reported a new live-run hang at `Starting Cardano node...`.
- Live inspection showed:
  - the marker remained `installed-awaiting-node-start`,
  - no fresh `CardanoNode#start` attempt was logged in `Daedalus.json`,
  - the renderer nevertheless displayed the cached `starting-node` partial-sync status.
- Root cause: `startNodeAfterPartialSyncInstall()` emitted `starting-node` by awaiting `emitMithrilPartialSyncStatus(...)` before calling `cardanoNode.start()`. In the main IPC implementation, partial-sync status broadcast awaited renderer delivery acknowledgement. If renderer delivery never resolved, the main process could stall before actually calling `cardanoNode.start()`.
- Fixed `source/main/ipc/mithrilPartialSyncChannel.ts` so partial-sync status delivery is non-blocking:
  - `lastStatus` is still updated synchronously,
  - in-process listeners still run synchronously,
  - renderer delivery is now fire-and-forget with warning logging on rejection,
  - main-process orchestration no longer waits on renderer acknowledgement before continuing.
- Added regression coverage proving `emitMithrilPartialSyncStatus(...)` still resolves when renderer delivery never resolves.

Files touched:
- `source/main/ipc/mithrilPartialSyncChannel.ts`
- `source/main/ipc/mithrilPartialSyncChannel.spec.ts`

Verification run:
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- `yarn test:jest source/main/utils/handleDiskSpace.spec.ts source/main/utils/chainStorageCoordinator.spec.ts source/main/mithril/MithrilPartialSyncService.spec.ts`

Deviations from approved plan:

- None. This is a direct follow-up fix from the same supported-network QA sequence.

User interaction is now required:

- yes

User Handoff:
- Why user interaction is required now: the currently stuck live run needs to be resumed under the patched non-blocking IPC behavior.
- Exact manual steps:
  1. Restart `yarn dev` so the updated main-process partial-sync IPC channel is loaded.
  2. Let Daedalus resume from the existing `installed-awaiting-node-start` marker.
  3. Confirm a fresh `cardano-node` start actually occurs and the run advances beyond `Starting Cardano node...`.
  4. Record the outcome in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

- Expected results: `starting-node` status is shown, but it no longer blocks the main process from actually invoking `cardanoNode.start()`.

- What output or decision the user should return: whether the resumed run now reaches node startup/completion, or the next exact failure if not.

- Whether work is blocked or can continue in parallel: blocked on live QA confirmation.

Outcome: Fixed renderer-acknowledgement IPC stall so partial-sync startup handoff can proceed to `cardanoNode.start()`; awaiting live confirmation

Implementation: Iteration 19
Timestamp: 2026-05-22T19:03:00Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- While in the same preview/dev session, user requested reduction of repeated Trezor transport-error log noise:
  - `[TREZOR-CONNECT] Received TRANSPORT_EVENT: transport-error`
  - payload `{ apiType: 'usb', error: 'Network request failed' }`
- Investigation traced this to the unconditional `logger.info(...)` in `source/main/ipc/getHardwareWalletChannel.ts` for every `TRANSPORT.ERROR` event.
- Adjusted that handler so the known no-device polling case (`apiType === 'usb'` and `error === 'Network request failed'`) logs at `debug` instead of `info`.
- Other Trezor transport errors still log at `info`, so actionable hardware-wallet issues remain visible.

Files touched:
- `source/main/ipc/getHardwareWalletChannel.ts`

Verification run:
- `yarn test:jest source/main/ipc/mithrilPartialSyncChannel.spec.ts source/main/utils/handleDiskSpace.spec.ts`
- `yarn test:jest source/main/utils/chainStorageCoordinator.spec.ts source/main/mithril/MithrilPartialSyncService.spec.ts`

Deviations from approved plan:

- Minor opportunistic cleanup requested by user during active task work; no functional rollout deviation.

User interaction is now required:

- yes

User Handoff:
- Why user interaction is required now: both the partial-sync startup handoff fix and the Trezor log-noise reduction require a restarted dev session to take effect.
- Exact manual steps:
  1. Restart `yarn dev`.
  2. Let Daedalus resume from the existing partial-sync marker if still present.
  3. Confirm the app moves beyond `Starting Cardano node...`.
  4. Confirm the repeated Trezor `transport-error` no-device messages no longer appear at `info` severity in the dev console/log stream.

- Expected results: quieter hardware-wallet logs in no-device conditions and a real resumed node startup attempt after partial-sync install.

- What output or decision the user should return: whether node startup now completes and whether the Trezor noise is acceptably reduced.

- Whether work is blocked or can continue in parallel: blocked on live QA confirmation.

Outcome: Reduced no-device Trezor transport log noise while keeping real transport issues visible; waiting for combined live re-test

Implementation: Iteration 20
Timestamp: 2026-05-22T19:08:00Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User reported that after clicking `continue to Daedalus`, a `cardano-node crashed` message appeared.
- Live logs showed this was a real app-side regression in the post-partial-sync chain install, not a generic node instability issue.
- Evidence:
  - partial-sync marker file was already gone, so partial sync had completed its cutover flow.
  - `Daedalus.json` showed node start succeeded to `running`, then later transitioned `exiting -> crashed`.
  - `node.log` showed the actual failure:
    - `FsResourceDoesNotExist` for `/home/westbam/.local/share/Daedalus/preview/chain/immutable/00000.primary`
    - followed by `ApiMisuse ClosedDBError` and node shutdown.
- Root cause: `installValidatedPartialSyncSnapshot()` wiped the entire managed chain contents, including the pre-existing `immutable/` history, then installed only the staged Mithril suffix range. That left early immutable files such as `00000.primary` missing.
- Fixed `source/main/utils/chainStorageManagerLayout.ts` so partial-sync install now preserves the existing live `immutable/` directory and merges staged immutable entries into it, while still replacing the other validated top-level entries (`clean`, `ledger`, `lsm`, `protocolMagicId`).
- Added regression coverage in `source/main/utils/chainStorageManager.spec.ts` proving validated partial-sync install preserves existing immutable history while merging the staged immutable suffix.

Files touched:
- `source/main/utils/chainStorageManagerLayout.ts`
- `source/main/utils/chainStorageManager.spec.ts`

Verification run:
- `yarn test:jest source/main/utils/chainStorageManager.spec.ts`
- `yarn test:jest source/main/mithril/MithrilPartialSyncService.spec.ts source/main/utils/handleDiskSpace.spec.ts source/main/ipc/mithrilPartialSyncChannel.spec.ts`

Deviations from approved plan:

- None. This is a direct correctness fix from supported-network manual QA.

User interaction is now required:

- yes

User Handoff:
- Why user interaction is required now: the current preview chain directory is already in a broken post-cutover state from the previous buggy install, so it needs a clean retry under the patched install logic.
- Exact manual steps:
  1. Restart `yarn dev`.
  2. Use the partial-sync recovery path that wipes chain data and performs a clean full Mithril/bootstrap retry, or otherwise clear the broken preview chain state before retrying partial sync.
  3. Re-run Mithril partial sync under the patched build.
  4. Confirm that after `continue to Daedalus`, `cardano-node` starts and stays running instead of crashing on missing immutable files.
  5. Record the result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

- Expected results: post-install chain contents preserve the old immutable prefix and add the downloaded immutable suffix, so `cardano-node` no longer crashes on missing `00000.primary`.

- What output or decision the user should return: whether the clean re-run now reaches stable node startup, or the next exact failure if it does not.

- Whether work is blocked or can continue in parallel: blocked on live QA confirmation.

Outcome: Fixed partial-sync cutover so immutable history is preserved during staged install; previous preview run remains broken and requires a clean retry

Implementation: Iteration 21
Timestamp: 2026-05-22T19:18:00Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User reported that the app looked stuck during/after install because the `installing` substep already showed as complete before the flow was actually ready to show `Continue to Daedalus`.
- Follow-up inspection showed that, in at least one later run, the backend had actually restarted `cardano-node` successfully; the bigger UX problem was that the step indicator dropped the live install activity too early, making long installs look finished/stuck.
- Root cause: when the top-level status advanced to `finalizing`, the renderer continued to treat the `install-snapshot` substep as completed based on the last backend progress item, so the user no longer saw an active spinner even though final post-install work was still ongoing.
- Updated `MithrilStepIndicator.tsx` so when top-level status remains `finalizing`, the `install-snapshot` substep is kept active (spinner visible) until the flow advances to `starting-node` or `completed`.
- Added regression coverage in `MithrilStepIndicator.spec.tsx` proving the install substep stays active while top-level status is `finalizing`.

Files touched:
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx`

Verification run:
- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx`
- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Deviations from approved plan:

- None. UX clarification fix directly requested during manual QA.

User interaction is now required:

- yes

User Handoff:
- Why user interaction is required now: the new behavior must be confirmed in the live partial-sync flow.
- Exact manual steps:
  1. Re-run partial sync in preview.
  2. Let it reach the long install/finalizing window.
  3. Confirm the install row keeps its active spinner/active styling until the flow is actually ready to transition to `Starting Cardano node...` or `Continue to Daedalus`.
  4. Record the result in `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`.

- Expected results: users no longer see a misleading completed install checkmark while the app is still legitimately working in the post-install finalizing phase.

- What output or decision the user should return: whether the install phase now communicates activity clearly enough, plus whether the overall run still completes.

- Whether work is blocked or can continue in parallel: blocked on live QA confirmation.

Outcome: Installing substep now stays visibly active through long finalizing windows instead of looking prematurely complete

Implementation: Iteration 22
Timestamp: 2026-05-22T19:48:24Z
Task: `task-401` - `Run supported-network manual QA and finalize rollout decision`

- User confirmed the latest fixed flow worked end to end.
- User also explicitly directed the remaining scope for this task checkpoint:
  - accept the earlier guard-off validation already performed before the generated default was flipped to `true`,
  - skip broader supported-network matrix coverage for now,
  - skip broader recovery-path re-validation for now,
  - treat one supported-network run as sufficient for this checkpoint,
  - record rollout posture as `enabled by default`.
- Updated the repo-local QA results document to:
  - fill in scope confirmation,
  - add the passing latest-flow run,
  - document the deferred matrix/recovery/build coverage explicitly,
  - record the final recommendation exactly as directed by the user.
- Updated the canonical task plan status to `completed` and recorded the narrowed current outcome.
- Updated the Mithril partial-sync task graph so `task-401` is now marked `completed`, with notes capturing both the completed live QA work and the explicitly deferred follow-up QA scope.

Files touched:
- `.agent/plans/mithril-partial-sync/research/18-task-401-manual-qa-results.md`
- `.agent/plans/mithril-partial-sync/task-plans/task-401.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`

Verification run:
- Artifact-only update; relied on the already-recorded live QA evidence and focused automated verification from the preceding implementation iterations.

Deviations from approved plan:

- Yes. The original task plan required broader supported-network/platform/storage/recovery evidence before truthful rollout closure. Final closeout scope was explicitly narrowed by user instruction for this checkpoint, and the deferred coverage is documented in the QA results and task-tracking artifacts.

User interaction is now required:

- no

Outcome: Task artifacts updated to reflect the passing latest fixed flow, explicit deferred-coverage limits, and user-directed `enabled by default` rollout decision
