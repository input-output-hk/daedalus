# Task-204 Cancellation And Recovery Notes

- Purpose: record the durable backend decisions landed by `task-204` for cancellation cleanup, boundary-owned recovery actions, startup handoff reuse, and the LauncherConfig rollout guard.
- Date: 2026-05-20

## Durable Findings

- Boundary A cancellation now removes `stateDir/mithril-partial-sync/` staging artifacts before reporting `cancelled`, and it also clears `stateDir/Logs/mithril-partial-sync.lock` so the untouched live DB can start normally again.
- If Boundary A cleanup itself fails, the backend now reports a terminal `failed` state with `retry`, `restart-normal`, and `wipe-and-full-sync` still available, preserving the task-004 rule that the live DB remains restart-safe before cutover.
- Diagnostics-launched `restart-normal` and `wipe-and-full-sync` now flow through `ChainStorageCoordinator` and re-enter the startup-owned `handleCheckDiskSpace` closure that `source/main/index.ts` gets from `handleDiskSpace(...)`. This keeps generation checks, marker safety, and empty-chain Mithril bootstrap decisions in one startup-owned seam.
- `wipe-and-full-sync` intentionally keeps the partial-sync marker in place until the actual chain wipe has completed successfully. Marker clearing now happens only in a final post-wipe step so a mid-recovery crash cannot strand an unsafe DB without startup blocking.

## Boundary Mapping

- Boundary A terminal failures expose `retry`, `restart-normal`, and `wipe-and-full-sync`.
- Boundary B / C1 / install-stage failures remain wipe-only.
- Boundary C2 remains the approved same-session in-memory contract only. Task-204 did not adopt the dormant durable `node-start-verified` marker.
- Backend start gating now enforces that `startPartialSync()` can only be reused as retry when the current terminal state explicitly allows `retry`; wipe-only boundaries cannot re-enter through the normal start channel.

## Rollout Guard

- `LauncherConfig.mithrilPartialSyncEnabled` is now the backend rollout guard.
- When the flag is not `true`, the main process rejects diagnostics-launched partial sync start and restart-normal requests.
- The guard deliberately does not block startup-owned unsafe-install recovery or diagnostics-triggered `wipe-and-full-sync`, matching the task-004 rollback contract.

## Verification Evidence

- Focused Jest suite passed:
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageCoordinator.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
- `yarn compile` still reports unrelated pre-existing failures in:
  - `source/main/cardano/setup.spec.ts`
  - `source/main/ipc/getHardwareWalletChannel.ts`
  - `source/main/trezor/manifest.ts`

## Follow-Ups

- Renderer tasks should consume only backend-exposed `allowedRecoveryActions`; they should not infer retry or restart availability from visible status text.
- Later rollout work should document the expected launcher default for `mithrilPartialSyncEnabled` once manual QA approves field enablement.
