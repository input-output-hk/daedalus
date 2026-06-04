Planner: Iteration 1
Timestamp: 2026-05-20T19:12:36Z
- Reviewed required planning inputs in order: `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/workflows/frontend.md`, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`, relevant Mithril partial-sync research notes, and historical Mithril bootstrap UI research relevant to renderer/store/app wiring.
- Consulted the `understand` skill guidance for repository-understanding workflow expectations, then verified all material planning claims directly against live repo files because there is no existing `.understand-anything/knowledge-graph.json` in this workspace.
- Verified live renderer state before planning:
  - `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` already exposes payload-free renderer wrappers for status, start, cancel, restart-normal, and wipe-and-full-sync.
  - `source/common/types/mithril-partial-sync.types.ts` already defines the renderer-facing source of truth for status, progress metadata, error shape, and backend-owned `allowedRecoveryActions`.
  - `source/renderer/app/stores/index.ts` has no `mithrilPartialSync` registration yet.
  - `source/renderer/app/App.tsx` has no partial-sync app-level host.
  - `source/renderer/app/containers/loading/LoadingPage.tsx` is startup/bootstrap-only and is not the right seam for diagnostics-launched partial sync.
  - `source/renderer/app/stores/MithrilBootstrapStore.ts` and `source/renderer/app/containers/loading/MithrilBootstrapPage.tsx` show the reusable pattern to follow selectively, but also confirm bootstrap-specific state must not be overloaded for partial sync.
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` and `source/renderer/app/components/status/DaedalusDiagnostics.tsx` currently have no partial-sync CTA or modal props, so task-300 should stop at store/app-shell wiring and leave diagnostics UX to tasks 301-303.
  - `source/common/ipc/lib/IpcChannel.ts` exposes `onReceive()` registration without unsubscribe helpers, so the new store should use the normal setup-once store lifecycle rather than inventing listener teardown in this task.
- Classified interaction mode as `autonomous`.
  - Required user input before implementation: none.
  - Required manual validation before implementation: none.
  - Required evidence back from the user during implementation: none.
  - Manual QA remains later `task-401` work, not a blocker for this task.
- Wrote the canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-300.md`.
- Planned `task-300` as the smallest truthful renderer seam:
  - add a dedicated `MithrilPartialSyncStore`
  - register it in `source/renderer/app/stores/index.ts`
  - keep store state backend-driven for status, error, progress metadata, and `allowedRecoveryActions`
  - let the store also own the minimal renderer-only confirmation/overlay visibility flags needed by later diagnostics UX tasks
  - add an app-level host seam in `source/renderer/app/App.tsx` so diagnostics-launched partial sync can render outside startup loading routes
  - keep CTA copy, confirmation content, and full progress/error overlay reuse out of scope for `task-301` through `task-303`
- Expected implementation files captured in the plan:
  - `source/renderer/app/stores/MithrilPartialSyncStore.ts`
  - `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
  - `source/renderer/app/stores/index.ts`
  - `source/renderer/app/App.tsx`
  - `source/renderer/app/ipc/mithrilPartialSyncChannel.ts` only if a tiny typing/export adjustment is required
  - focused renderer spec files only if the chosen app-host seam needs them
  - `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`
  - `.agent/system/architecture.md` after implementation because this task adds a durable store and app-shell seam
- Verification plan captured in the canonical doc:
  - targeted Jest for `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
  - focused app/render tests only if `App.tsx` gains meaningful host logic
  - verify cached-status sync, push-update handling, explicit field clearing, full replacement of `allowedRecoveryActions`, payload-free action delegation, and app-level host reachability outside `LoadingPage`
  - attempt truthful TypeScript verification and record any unrelated pre-existing compile blockers if repo-wide compile is still not clean
- Self-review completed:
  - scope creep avoided: no diagnostics CTA, confirmation copy, overlay mapping, or i18n/theme/storybook work pulled into task-300
  - workflow guidance remains current and matched to the renderer/store scope
  - required tests, docs, research, review-log paths, planning status, and build status are all present in the canonical plan doc
  - plan is consistent with the PRD, task graph, backend-owned recovery semantics from `task-204`, and the live renderer state
Outcome: Approved creation of canonical plan `.agent/plans/mithril-partial-sync/task-plans/task-300.md` with `autonomous` execution, dedicated `MithrilPartialSyncStore` scope, app-level mounting in `App.tsx`, explicit non-goals for tasks 301-303, focused verification, and required research/architecture follow-ups.

Critiquer: Iteration 1
Timestamp: 2026-05-20T19:15:20Z
Blocking findings:
- `.agent/plans/mithril-partial-sync/task-plans/task-300.md:93-94, 120-128, 173-174` bakes in a false lifecycle assumption: it says the store should register `mithrilPartialSyncStatusChannel.onReceive(...)` once in `setup()` and not invent teardown because the store is effectively long-lived. Live repo says otherwise. `source/renderer/app/index.tsx:57-60` can recreate stores via `window.daedalus.reset()`, `source/renderer/app/stores/lib/Store.ts:37-39` only stops MobX reactions, and `source/common/ipc/lib/IpcChannel.ts:152-167` exposes no unsubscribe path. If task-300 follows the current plan, partial-sync status listeners will accumulate across store resets and duplicate updates. The plan needs a lifecycle-safe listener strategy before implementation is approved.
- `.agent/plans/mithril-partial-sync/task-plans/task-300.md:31-33, 120-136, 149-150` over-commits to store-owned confirmation/overlay booleans plus an `App.tsx` host without first locking who owns modal handoff from diagnostics. In the live app, `source/renderer/app/App.tsx:84-95` already mounts `DaedalusDiagnosticsDialog`, that dialog is itself a `ReactModal` (`source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx:83-129`), and the diagnostics content still does direct modal focus work (`source/renderer/app/components/status/DaedalusDiagnostics.tsx:896-899`). Adding a second app-level partial-sync modal seam now, while leaving close/handoff semantics for tasks 302-303, risks stacked modals and focus bugs. Either de-scope task-300 to store registration/status sync only, or explicitly lock a single modal owner and handoff rule now.

Non-blocking observations:
- The backend-driven `allowedRecoveryActions` rule is correct and matches live contracts in `source/common/types/mithril-partial-sync.types.ts:39-50` plus task-204 research. Keep that.
- The architecture-doc update is only clearly required if task-300 really adds a durable app-shell orchestration seam. If the task is simplified to store-only, that doc update can move to the task that actually lands the host.
- The verification plan should add one explicit lifecycle test around store recreation or `window.daedalus.reset()` if any renderer-side long-lived IPC listener remains in scope.
- Keep `logPath` as raw backend data in the store. Windows-safe `file:///` conversion belongs in the eventual UI/open-link layer, consistent with prior Mithril UX research.

Approval bar:
- Revise the plan so task-300 no longer relies on a non-tear-downable store listener model.
- Either remove `App.tsx`/overlay-visibility work from task-300, or define the diagnostics-to-global-modal ownership contract concretely enough that tasks 302-303 are not forced to reopen it.
- Strengthen verification to cover duplicate-listener prevention across store recreation and, if `App.tsx` stays in scope, prove diagnostics and partial-sync UI cannot be active as competing modal layers.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-20T19:17:26Z
- Re-read the required planning inputs in order: `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/workflows/frontend.md`, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`, the current canonical plan at `.agent/plans/mithril-partial-sync/task-plans/task-300.md`, the planning review log at `.agent/plans/mithril-partial-sync/task-plans/task-300-plan-review.md`, and the relevant Mithril research notes already referenced by the task.
- Verified the critique claims directly against live repo files before revising the plan:
  - `source/renderer/app/index.tsx:57-60` proves `window.daedalus.reset()` recreates stores via `setUpStores(...)`.
  - `source/renderer/app/stores/lib/Store.ts:37-39` proves store teardown only stops MobX reactions and does not remove arbitrary IPC listeners.
  - `source/common/ipc/lib/IpcChannel.ts:152-167` proves renderer `onReceive()` has no unsubscribe/remove-listener path.
  - `source/renderer/app/App.tsx:84-95` proves diagnostics is already mounted as a global dialog.
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx:83-129` proves diagnostics itself is a `ReactModal`.
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx:894-912` proves diagnostics still does direct modal-content focus work, so stacked modal ownership is not safe to leave ambiguous.
- Confirmed the blocker: the previous plan’s “setup-once store listener” assumption was false. A store-local permanent `mithrilPartialSyncStatusChannel.onReceive(...)` would stack across `window.daedalus.reset()` store recreation unless task-300 also introduced a lifecycle owner with explicit idempotence or teardown.
- Updated the canonical plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-300.md`.
- Revised task-300 scope to the final approved shape:
  - keep `MithrilPartialSyncStore` creation and store registration in scope
  - keep backend-driven renderer state in scope: status, `allowedRecoveryActions`, progress metadata, error, and raw `logPath`
  - require a lifecycle-safe status-sync strategy
  - remove any assumption that a permanent store-local IPC listener is acceptable without additional lifecycle proof
  - de-scope `App.tsx` host or overlay ownership from task-300 unless implementation needs a narrowly-scoped lifecycle seam that does not create a second modal host
- Locked the listener/lifecycle rule in the plan:
  - task-300 must not rely on a store-local permanent listener unless implementation proves teardown safety across reset
  - the preferred truthful fallback is cached `request()`-based sync if push wiring cannot be made lifecycle-safe within this task
  - if push wiring remains in scope, its owner must be explicit, survive store recreation or rebind idempotently, and be verified by tests
- Simplified modal ownership to avoid stacked-modal ambiguity:
  - task-300 no longer commits to `App.tsx` overlay ownership
  - diagnostics-to-confirmation-to-progress modal ownership is now explicitly deferred to `task-302` and `task-303`
  - the plan now states that task-300 must not introduce a second modal or overlay owner before that handoff contract is defined
- Tightened expected file set and doc follow-ups:
  - removed `source/renderer/app/App.tsx` from the expected core changes
  - added `source/renderer/app/index.tsx` only as a conditional seam if implementation truly needs a reset-safe listener owner there
  - kept `.agent/system/architecture.md` as a conditional-but-expected follow-up because this task definitely adds a new MobX store and may add a durable renderer lifecycle seam
- Strengthened verification requirements:
  - added an explicit lifecycle test for store recreation or `window.daedalus.reset()`
  - added explicit proof that no duplicate partial-sync status handlers or duplicate state updates appear after reset
  - added explicit verification that any non-store listener owner is idempotent across reset/rebinding
  - removed the earlier app-host reachability check because app-shell/modal ownership is no longer in task-300 scope
- Preserved the correct parts of the original plan:
  - backend-owned `allowedRecoveryActions` remains the renderer source of truth
  - bootstrap store and bootstrap-only UI remain out of scope
  - confirmation copy, CTA placement, and overlay reuse remain in tasks 301-303
  - raw backend `logPath` stays raw in the store; Windows-safe link conversion remains later UI-layer work
- Self-review after revision:
  - scope creep removed: no premature `App.tsx` host or modal ownership remains
  - stale lifecycle assumption removed completely
  - verification now covers the exact reset/listener hazard the critique identified
  - canonical task plan remains iteration-stable and consistent with live repo behavior plus the PRD/task graph
Outcome: Revised `task-300.md` is now the approved final implementation plan, with task-300 narrowed to dedicated store creation, registration, backend-driven state handling, and lifecycle-safe status-sync verification, while modal/overlay ownership is intentionally deferred to the later UX tasks.

