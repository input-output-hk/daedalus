# Task-300 Renderer Store And App Wiring Notes

- Purpose: record the durable renderer decisions from `task-300` for Mithril partial sync state ownership and lifecycle-safe status syncing.
- Date: 2026-05-20

## Durable Findings

- `task-300` adds `source/renderer/app/stores/MithrilPartialSyncStore.ts` as a dedicated renderer MobX store instead of widening `MithrilBootstrapStore`.
- The store is registered in `source/renderer/app/stores/index.ts` as `mithrilPartialSync`, so later diagnostics, confirmation, and progress tasks can consume one stable injected seam.
- Renderer recovery availability stays backend-owned. The store persists `allowedRecoveryActions` exactly as received from `MithrilPartialSyncStatusUpdate` and derives convenience booleans from that array instead of inferring action safety from visible status text.
- Partial-sync status updates intentionally do not use `mithrilPartialSyncStatusChannel.onReceive(...)` in this task. Renderer IPC still exposes no unsubscribe path, and `window.daedalus.reset()` recreates stores, so a permanent store-local broadcast listener would accumulate across resets.
- The approved lifecycle-safe fallback is serialized cached-status polling:
  - `syncStatus()` is single-flight so overlapping `request()` calls cannot race on the shared IPC response channel.
  - polling runs only while the backend reports an active restore status
  - terminal and idle states clear the polling interval during normal updates and store teardown
  - action methods (`startPartialSync`, `cancelPartialSync`, `restartNormally`, `wipeAndFullSync`) explicitly refresh cached status after their payload-free IPC request resolves

## Scope Boundary Preserved

- `App.tsx` and diagnostics modal ownership stay unchanged in `task-300`.
- This task does not define the diagnostics-to-confirmation-to-progress handoff contract and does not add a second modal or overlay owner.
- Diagnostics CTA placement, confirmation UX, and progress/error surface reuse remain later work in `task-301` through `task-303`.

## Verification Evidence

- Focused Jest suite passed:
  - `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- The spec locks:
  - cached status sync during setup
  - explicit field clearing
  - backend-driven recovery action replacement
  - payload-free action delegation plus post-action refresh
  - active-only polling and teardown cleanup
  - reset-safe polling recreation without `onReceive()` listener accumulation
  - single-flight status requests on the shared IPC response channel
- `yarn compile` still reports unrelated pre-existing failures in:
  - `source/main/cardano/setup.spec.ts`
  - `source/main/ipc/getHardwareWalletChannel.ts`
  - `source/main/trezor/manifest.ts`

## Follow-Ups

- Later renderer tasks can build confirmation/progress UI on top of `stores.mithrilPartialSync` without reopening backend action semantics.
- If a future task needs push-driven status instead of polling, it must introduce a lifecycle owner that survives store recreation or adds truthful unsubscribe support before switching away from the polling fallback.
