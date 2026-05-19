Planner: Iteration 1
Timestamp: 2026-05-19T18:29:41Z

- Task: `task-101` — `Define partial sync IPC channels and add main/renderer wrappers`
- Canonical plan doc: `.agent/plans/mithril-partial-sync/task-plans/task-101.md`
- Summary:
  - Added the canonical task plan for the dedicated partial-sync IPC layer.
  - Locked the task scope to `source/common/ipc/api.ts`, a new `source/main/ipc/mithrilPartialSyncChannel.ts`, a new `source/renderer/app/ipc/mithrilPartialSyncChannel.ts`, `source/main/ipc/index.ts` registration, and focused IPC wrapper tests.
  - Kept coordinator/service orchestration, node lifecycle suppression, renderer store work, and diagnostics UI out of scope for later tasks.
- Key planning decisions:
  - Use five dedicated channels: start, status, cancel, restart-normal, and wipe-and-full-sync.
  - Keep all requests payload-free (`void`) so snapshot/range/recovery safety stays backend-owned.
  - Reuse the existing Mithril bootstrap IPC shape: cached status + initial `request()` + push updates, not a new conversation channel.
  - Initialize idle partial-sync status with `allowedRecoveryActions: []` to match the task-100 contract and avoid stale merge-based UI state later.
  - Register the new handler via `source/main/ipc/index.ts` and preserve idempotent setup / window-rebind behavior.
- Verification expectations:
  - Focus on a new `source/main/ipc/mithrilPartialSyncChannel.spec.ts` modeled on the bootstrap IPC spec.
  - Use targeted typecheck / Jest verification if repo-wide `yarn compile` remains blocked by the known unrelated Trezor typing failures.
- Notes:
  - The plan calls out `.agent/system/api-endpoints.md` as a likely documentation follow-up once the channels are added in code.
  - No plan review log was written in this iteration.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-05-19T18:32:15Z

Blocking findings:
1. The plan does not fail closed on unimplemented action channels, and its current wording makes a false-success stub too likely. `task-101.md` says the new main wrapper may use placeholder coordinator methods or "exported stubs" if the real methods do not exist yet, while the tests should verify that each action channel registers a handler (`.agent/plans/mithril-partial-sync/task-plans/task-101.md:114-115,128-135`). In the live repo, `MainIpcChannel` handlers are real request endpoints, and the bootstrap file only registers channels once it can delegate to concrete behavior (`source/main/ipc/mithrilBootstrapChannel.ts:214-261`). If task-101 registers `start`, `cancel`, `restart-normal`, or `wipe-and-full-sync` with no-op success stubs, renderer callers will get a successful IPC round-trip for behavior that does not exist yet. The plan needs an explicit rule: until task-102/task-200 provide real coordinator/service methods, action handlers must reject with a deliberate "not implemented" error, not succeed.
2. "Mirror the bootstrap structure" is currently too loose and risks copying two bootstrap-only behaviors that violate the partial-sync contract and task boundary. The bootstrap channel mutates setup state via `chainStorageCoordinator.syncMithrilWorkDir()` during IPC registration (`source/main/ipc/mithrilBootstrapChannel.ts:200-204`) and exposes a merge-style `setMithrilBootstrapStatus(update: Partial<...>)` helper (`source/main/ipc/mithrilBootstrapChannel.ts:131-139`). For partial sync, setup-time chain-storage/workdir coordination belongs to task-102, not task-101, and a merge helper directly conflicts with the task-100 contract that `allowedRecoveryActions` must be present on every update to avoid stale renderer state (`.agent/plans/mithril-partial-sync/research/03-task-100-shared-contract-notes.md:13-15`). The plan must explicitly forbid copying both patterns into `mithrilPartialSyncChannel.ts`.
3. The documentation requirement is treated as optional even though the repo workflow makes it mandatory for IPC changes. The task plan says the broader IPC/system doc update is out of scope unless review proves it is expected, then later says `.agent/system/api-endpoints.md` "should eventually" or "during implementation or immediately after" be updated (`.agent/plans/mithril-partial-sync/task-plans/task-101.md:31,162,169`). The documented workflow says IPC channel changes require updating `.agent/system/api-endpoints.md` (`.agent/workflows/update-doc.md:12-17,47,168-176`). The live API doc already inventories Electron IPC channels and is missing Mithril channel coverage entirely (`.agent/system/api-endpoints.md:351-472`). If task-101 adds channels in code, the plan needs to require the doc update in the same task, not as a maybe-later follow-up.

Non-blocking observations:
1. The simplest safe version of task-101 is narrower than the current wording: define the five contracts in `source/common/ipc/api.ts`, add thin renderer wrappers, add a main partial-sync channel module with cached idle status and broadcast plumbing, and make action handlers explicitly reject until coordinator/service methods land. That still gives later tasks stable IPC seams without pretending orchestration exists.
2. Do not cargo-cult bootstrap-only helpers into this task. Partial sync has no decision waiter flow, so bootstrap-style decision/reset tests are not relevant just because `mithrilBootstrapChannel.spec.ts` is the closest example.
3. `isMithrilPartialSyncNodeStartBlocked()` and any `set...NodeStateProvider()` hook should stay out of task-101 unless they are strictly required for compilation. The live startup-suppression seam is consumed from `source/main/cardano/setup.ts` and wired in `source/main/index.ts`; that is task-103 territory, not needed to truthfully add IPC wrappers.

Approval bar:
1. Amend the plan so every unimplemented action channel is specified to reject, not no-op succeed, until task-102/task-200 provide concrete delegates.
2. Explicitly exclude bootstrap-only setup side effects and `Partial<>` merge helpers from the partial-sync channel implementation; require full-status updates that always include `allowedRecoveryActions`.
3. Make `.agent/system/api-endpoints.md` an in-scope required update for task-101 if the IPC channels are added in code.
4. Tighten the test plan so it verifies idle status shape, best-effort broadcast, window rebinding, and explicit rejection semantics for unimplemented actions rather than "expected delegation seam or exported stub."

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-19T18:34:07Z

- Task: `task-101` — `Define partial sync IPC channels and add main/renderer wrappers`
- Canonical plan doc: `.agent/plans/mithril-partial-sync/task-plans/task-101.md`
- Summary:
  - Revised the canonical plan to address all critique blockers while keeping task-101 narrowly scoped to IPC contracts, wrappers, registration, focused tests, and the required IPC documentation update.
  - Kept coordinator/service orchestration, startup suppression, renderer store work, and diagnostics UI out of scope for later tasks.
- Changes made to the plan:
  - Added an explicit fail-closed rule: until `task-102` or `task-200` provides real backend delegates, every partial-sync action channel (`start`, `cancel`, `restart-normal`, `wipe-and-full-sync`) must reject with a deliberate not-implemented error instead of returning a successful no-op response.
  - Explicitly forbade copying bootstrap-only setup side effects from `source/main/ipc/mithrilBootstrapChannel.ts`, especially `chainStorageCoordinator.syncMithrilWorkDir()` during IPC registration.
  - Explicitly forbade copying the bootstrap `Partial<>` merge helper pattern; partial-sync status updates must always replace the full cached status object and always include `allowedRecoveryActions`, including `[]`.
  - Tightened the expected helper surface so task-101 stays at the wrapper seam and does not pull in `isMithrilPartialSyncNodeStartBlocked()` or other task-103-oriented startup hooks.
  - Made `.agent/system/api-endpoints.md` an in-scope required update if the new IPC channels are added in code, per the documentation workflow.
  - Tightened the test plan to verify:
    - truthful idle status shape with `allowedRecoveryActions: []`
    - best-effort status broadcast when renderer delivery rejects
    - window rebinding without duplicate handler registration
    - explicit rejection semantics for unimplemented action channels
    - absence of `Partial<>` merge-style status retention behavior
- Key planning decisions now locked:
  - Use five dedicated channels: start, status, cancel, restart-normal, and wipe-and-full-sync.
  - Keep all renderer requests payload-free (`void`) so snapshot selection, range derivation, and recovery safety remain backend-owned.
  - Reuse the existing request-plus-push status transport shape from bootstrap, but not its bootstrap-only side effects or merge helpers.
  - Require full-status partial-sync updates carrying `allowedRecoveryActions` every time.
  - Require the same-task `.agent/system/api-endpoints.md` update when the channels are introduced in code.
- Verification expectations:
  - Focus on a new `source/main/ipc/mithrilPartialSyncChannel.spec.ts` at the wrapper level only.
  - Use targeted Jest and truthful typecheck/compile verification, noting the known unrelated repo-wide Trezor compile failures if they still block full `yarn compile`.

Outcome: Canonical plan updated and aligned with the critique approval bar

