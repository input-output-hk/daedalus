# Task task-300: Create MithrilPartialSyncStore And App-Level Wiring

## Task

- Task ID: `task-300`
- Title: `Create MithrilPartialSyncStore and app-level wiring`

## Why This Task Now

- `task-101` and `task-204` are complete, so the renderer now has stable partial-sync IPC wrappers plus backend-owned start, cancel, restart-normal, wipe-and-full-sync, and `allowedRecoveryActions` semantics to consume.
- Phase 3 still cannot start truthfully without a dedicated renderer state seam. The repo has no `MithrilPartialSyncStore` and no store registration, so later diagnostics CTA, confirmation, and overlay tasks would otherwise have nowhere stable to read or write partial-sync state.
- Critique against the first planning pass confirmed two boundaries that must stay out of this task unless re-opened explicitly:
  - renderer IPC listener lifecycle is not safe to hand-wave because `window.daedalus.reset()` recreates stores while the current IPC helper exposes no unsubscribe path
  - modal or overlay ownership is still ambiguous because diagnostics already lives inside `ReactModal`, so `App.tsx` host work would risk stacked-modal behavior before later tasks define the handoff contract
- `task-300` is therefore the next smallest truthful task on the critical path only if it is simplified to store creation, registration, and lifecycle-safe status-sync strategy, leaving modal or overlay ownership to `task-302` and `task-303`.
- The canonical task plan doc for this task did not exist yet under `.agent/plans/mithril-partial-sync/task-plans/`, so this planning pass also closes that tracking gap.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Required manual validation before implementation can proceed: none.
- Required evidence back from the user during implementation: none.
- Manual QA will still exist later in `task-401`, but this task does not require user-driven execution to implement the renderer seam.

## Scope

- Add a dedicated `MithrilPartialSyncStore` under `source/renderer/app/stores/` instead of widening `MithrilBootstrapStore`.
- Register that store in `source/renderer/app/stores/index.ts` so renderer code can access partial-sync state through the normal injected stores map.
- Make the store own renderer-side partial-sync state needed by later UX tasks:
  - current backend status
  - backend-exposed `allowedRecoveryActions`
  - progress metadata and error/log-path state
  - only the minimal renderer-owned state that is unambiguously store-local without deciding modal ownership yet
- Define a lifecycle-safe renderer status-sync approach that does not rely on a permanent store-local `onReceive()` listener surviving `window.daedalus.reset()` store recreation.
- Keep the implementation small enough that `task-301`, `task-302`, and `task-303` can build on it without reopening store shape or listener-lifecycle assumptions.
- Add focused renderer tests for the new store, especially around duplicate-listener prevention and store recreation behavior.

## Non-Goals

- Diagnostics recommendation copy and CTA placement in `DaedalusDiagnostics`; that belongs to `task-301`.
- The user-facing confirmation modal copy and confirm-before-start behavior; that belongs to `task-302`.
- Full progress/error overlay reuse and step mapping onto existing Mithril loading components; that belongs to `task-303`.
- Defining or implementing the final modal or overlay owner for partial sync. This task must not decide between diagnostics-dialog-local ownership and a global app-shell host.
- `App.tsx` wiring or any second modal layer while diagnostics is still the active `ReactModal` owner.
- New shared contracts, IPC channel names, or backend safety decisions; those are already locked by `task-100` through `task-204`.
- Snapshot selection, chain-storage picker behavior, or any bootstrap-only renderer flow.
- i18n, theme, and Storybook coverage beyond the absolute minimum needed to keep this task compiling; those remain `task-304` work.

## Relevant Dependencies

- Required completed dependencies:
  - `task-101`
  - `task-204`
- Supporting completed prerequisites used directly by this plan:
  - `task-100`
  - `task-102`
  - `task-103`
  - `task-200`
  - `task-201`
  - `task-202`
  - `task-203`
- This task directly unblocks:
  - `task-301`
  - `task-302`
  - `task-303`
  - renderer portions of `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/04-task-101-ipc-wrapper-notes.md`
- `.agent/plans/mithril-partial-sync/research/11-task-204-cancellation-and-recovery-notes.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ux-notes.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `understand` skill guidance was loaded for repository-understanding workflow expectations, then all material planning claims were verified against live files before inclusion.

## Live Repo Findings Verified For Planning

- `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` already exposes the five payload-free renderer IPC wrappers required for this task: status, start, cancel, restart-normal, and wipe-and-full-sync.
- `source/common/types/mithril-partial-sync.types.ts` already gives the renderer the durable backend contract it must follow: status, `allowedRecoveryActions`, progress metadata, error shape, and the blocking/terminal helpers. The renderer should consume those fields directly instead of inferring recovery availability from status text.
- `source/renderer/app/stores/index.ts` currently registers `MithrilBootstrapStore` but has no `mithrilPartialSync` entry in either `storeClasses`, `StoresMap`, or `setUpStores(...)`.
- `source/renderer/app/containers/loading/LoadingPage.tsx` is explicitly startup-only. Its Mithril branch is tied to bootstrap statuses and would be the wrong seam for diagnostics-launched partial sync.
- `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx` and `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx` prove the existing renderer patterns worth reusing selectively later: a store-backed container that maps backend status into localized progress/error views. They also show why task-300 should not overload bootstrap state now: bootstrap includes storage-location and snapshot-selection branches that partial sync must not inherit.
- `source/renderer/app/index.tsx` exposes `window.daedalus.reset()` and recreates stores by calling `setUpStores(...)` again. `source/renderer/app/stores/lib/Store.ts` only tears down MobX reactions, not arbitrary IPC subscriptions. That means a store-local `mithrilPartialSyncStatusChannel.onReceive(...)` registration would stack across resets unless task-300 introduces a different lifecycle-safe strategy.
- `source/common/ipc/lib/IpcChannel.ts` exposes `onReceive()` registration but no unsubscribe helper. Critique was correct: task-300 cannot assume a setup-only listener on the store is safe across store recreation.
- `source/renderer/app/App.tsx` already mounts `DaedalusDiagnosticsDialog` as a global dialog, `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` itself wraps diagnostics in `ReactModal`, and `source/renderer/app/components/status/DaedalusDiagnostics.tsx` still does direct `ReactModal__Content` focus management. That makes modal ownership ambiguous today and confirms task-300 should not add `App.tsx` host or overlay ownership before later tasks lock the handoff model.
- There is no existing `.understand-anything/knowledge-graph.json` in the repo, so live-file verification was the authoritative source for task-specific findings after consulting the `understand` skill workflow.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-300.md`
- `source/renderer/app/stores/MithrilPartialSyncStore.ts`
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `source/renderer/app/stores/index.ts`
- `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` only if a tiny typing/export adjustment is needed during implementation
- `source/renderer/app/index.tsx` only if the chosen lifecycle-safe sync strategy needs a narrow existing-app bootstrap/reset seam rather than a store-local permanent listener
- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`
- `.agent/system/architecture.md` because this task adds a durable renderer MobX store and, if needed, a lifecycle-safe renderer status-sync seam that later tasks will rely on

## Final Outcome

- Implemented `source/renderer/app/stores/MithrilPartialSyncStore.ts` as the dedicated renderer seam for diagnostics-launched partial sync.
- Registered the store in `source/renderer/app/stores/index.ts` as `mithrilPartialSync`.
- Kept renderer recovery semantics backend-owned by exposing `allowedRecoveryActions` directly and deriving convenience booleans from that source of truth.
- Rejected renderer-side `onReceive()` subscriptions for this task because the IPC transport still has no unsubscribe path across `window.daedalus.reset()`.
- Landed a lifecycle-safe fallback based on:
  - single-flight cached status requests on the shared IPC response channel
  - immediate working-state seeding on long-running `startPartialSync()`
  - active-only polling while backend work is in progress
  - queued catch-up syncs behind already in-flight status requests
  - teardown and reset suppression for both queued and direct post-action status refreshes
- Left modal ownership, diagnostics CTA copy, confirmation UX, and progress or error overlay composition to later phase-3 tasks.
- Added focused renderer Jest coverage in `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` for cached status sync, explicit field clearing, long-running start behavior, stale setup responses, queued catch-up syncs, single-flight transport safety, teardown races, and reset-time no-op behavior.
- Updated `.agent/system/architecture.md` and added `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md` as the required durable follow-ups.

## Implementation Approach

1. Add a separate partial-sync store rather than widening bootstrap state.
   - Create `MithrilPartialSyncStore` as a sibling to `MithrilBootstrapStore`.
   - Keep bootstrap and partial-sync renderer invariants separately understandable.
   - Mirror only the small patterns that still fit: status syncing, observable progress/error state, and focused computed helpers.

2. Keep the store contract backend-driven.
   - Store `allowedRecoveryActions` exactly as received from backend status updates.
   - Expose small computed helpers such as `canRetry`, `canRestartNormally`, `canWipeAndFullSync`, `isActive`, `isWorking`, and `isTerminal`, but derive them from backend-provided actions and status rather than new renderer heuristics.
   - Preserve explicit field-presence handling for optional progress fields so backend resets like `filesDownloaded: undefined` and `error: null` clear stale renderer state correctly, following the durable bootstrap-store lesson.

3. Make status-sync lifecycle-safe before adding any long-lived listener.
   - Do not approve a store-local permanent `mithrilPartialSyncStatusChannel.onReceive(...)` registration unless implementation also proves teardown safety across `window.daedalus.reset()`.
   - Prefer the smallest truthful strategy that avoids listener accumulation, even if that means task-300 stops at cached `request()`-based sync plus action/result state and leaves push wiring to the task that can own a longer-lived listener safely.
   - If implementation does require push updates now, the listener owner must be a seam whose lifecycle survives store recreation or can be re-bound idempotently without stacking. That ownership must be made explicit in code and tests during implementation, not assumed.
   - Keep raw backend `logPath` data in the store; any `file:///` conversion remains later UI-layer work.

4. Register the store through the normal app store map.
   - Add `mithrilPartialSync` to `storeClasses`, `StoresMap`, and `setUpStores(...)`.
   - Keep setup consistent with other renderer stores where truthful, but do not force the bootstrap-store listener model onto partial sync now.
   - Fetch the current cached status explicitly and keep any remaining lifecycle-sensitive wiring minimal and auditable.

5. Do not decide modal or overlay ownership in this task.
   - Leave `App.tsx` unchanged unless implementation discovers a narrowly-scoped lifecycle seam there that is required only for duplicate-listener safety and does not create a new modal host.
   - Keep diagnostics-to-confirmation-to-progress ownership for later tasks, where the app can define one modal stack and one focus-management rule intentionally.
   - `task-300` should not invent final CTA copy, bootstrap-style decision views, or full overlay step mapping before those tasks.

6. Add focused tests now instead of waiting for phase-end coverage.
   - Add a store spec that locks status syncing, field clearing, backend-driven recovery-action exposure, and action delegation for start/cancel/restart-normal/wipe-and-full-sync.
   - Add an explicit lifecycle test that covers store recreation or `window.daedalus.reset()` semantics for whichever listener strategy remains in scope.
   - If any non-store listener owner is introduced, test that it remains idempotent and does not duplicate updates after reset.

## Acceptance Criteria

- A dedicated `MithrilPartialSyncStore` exists under `source/renderer/app/stores/`.
- The store can request partial-sync `start`, `cancel`, `restart-normal`, and `wipe-and-full-sync` through the existing renderer IPC wrappers.
- The store syncs truthful backend status and keeps progress/error/log-path state correct when fields are explicitly cleared.
- The store surfaces backend-owned `allowedRecoveryActions` directly and derives any convenience booleans from that backend-owned source of truth.
- The store is registered in the app store map and available through injected `stores` as `mithrilPartialSync`.
- The task does not rely on a store-local permanent IPC listener model that would duplicate updates across `window.daedalus.reset()` store recreation.
- The task does not overload `MithrilBootstrapStore` or force diagnostics-launched partial sync through bootstrap-only storage-picker or snapshot-selection branches.
- The task does not introduce a second modal or overlay owner before later tasks define the diagnostics-to-partial-sync handoff contract.
- Focused renderer tests exist for the new store and for duplicate-listener prevention across any lifecycle-sensitive wiring left in scope.

## Verification Plan

- Run targeted Jest coverage for:
  - `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
  - any touched renderer spec that verifies lifecycle-safe wiring if implementation introduces a non-store listener owner
- Verify these behaviors directly:
  - the store requests the latest partial-sync status from backend and updates observables truthfully
  - explicit backend field resets clear stale renderer state instead of merge-retaining prior values
  - `allowedRecoveryActions` fully replaces prior recovery-action state on every update
  - `start`, `cancel`, `restart-normal`, and `wipe-and-full-sync` delegate through the renderer IPC wrappers with payload-free requests
  - store recreation via `window.daedalus.reset()` does not create duplicate partial-sync status handlers or duplicated state updates
  - any remaining lifecycle-sensitive listener owner is idempotent across reset/rebinding
  - bootstrap-only Mithril flows still remain startup-scoped and unaffected
- Attempt truthful TypeScript verification after edits.
- If repo-wide `yarn compile` is still blocked by unrelated pre-existing repo issues, record that and rely on focused Jest plus live diff inspection.

## Risks And Open Questions

- The main risk is accidental scope creep into `task-301` through `task-303`, especially by trying to finish CTA copy, modal ownership, or full overlay rendering before the store and lifecycle seams are stabilized.
- A second risk is reusing too much of `MithrilBootstrapStore` or `MithrilBootstrap` too early, which would leak bootstrap-only concepts like storage selection or snapshot choice into diagnostics-launched partial sync.
- Because renderer IPC channel helpers do not expose unsubscribe support, any push-status wiring needs extra scrutiny. The safe default for this task is to avoid a permanent store-local listener unless implementation proves a different lifecycle owner.
- Modal and focus ownership intentionally remain open until the later UX tasks define one handoff contract around the existing diagnostics `ReactModal` usage.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-300.md`.
- Preserve append-only planning and implementation review logs for this task.
- Add `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md` capturing the final store shape, backend-driven recovery-action rule on the renderer side, and the chosen lifecycle-safe status-sync seam.
- Update `.agent/system/architecture.md` after implementation because this task adds a new MobX store and may add a durable renderer lifecycle seam that later tasks depend on.
- Mark `task-300` completed in `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-300-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-300-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Verification

- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` passed with 12 focused assertions covering the task-300 lifecycle and transport hazards.
- `yarn compile` was attempted during implementation and still failed only on unrelated pre-existing errors in:
  - `source/main/cardano/setup.spec.ts`
  - `source/main/ipc/getHardwareWalletChannel.ts`
  - `source/main/trezor/manifest.ts`
- Implementation review reached `Decision: approved` in the latest `Code Review` entry of `.agent/plans/mithril-partial-sync/task-plans/task-300-impl-review.md`.

## Final Review Outcome

- Planning review outcome: approved after the final planner revision narrowed task scope away from modal ownership and unsafe store-local listeners.
- Implementation review outcome: approved after the store was hardened for long-running start, single-flight status transport, queued catch-up syncs, stale setup responses, and teardown/reset races.

## Self-Review

- Scope-creep check: keeps diagnostics CTA, confirmation copy, modal ownership, progress/error overlay reuse, and i18n/theme/storybook work in later tasks.
- Workflow freshness check: frontend, IPC, test, and doc-update guidance were used only where they materially affect this renderer/store task.
- Missing-tests/docs/tracking check: includes store/lifecycle verification plus required research and architecture updates for the new durable store seam.
- Consistency check: plan matches the PRD, the task graph, backend-owned recovery semantics from `task-204`, and the live renderer reality that store recreation and diagnostics modal ownership both need stricter boundaries than the first draft assumed.
