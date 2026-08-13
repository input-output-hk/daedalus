Planner: Iteration 1
Timestamp: 2026-08-13T21:07:19Z

Plan: Created canonical draft `.agent/plans/dapp-browser-cip30/task-plans/task-101.md` with autonomous interaction mode. The plan covers exact trusted WebContents/main-frame/document/origin authentication, destroyed and stale frame rejection, generation-safe lifecycle, caller-targeted responses, request correlation and concurrency, listener-before-send, concrete compatibility, fail-closed registration, task-102 scope boundaries, focused unit/runtime tests, documentation, research, and tracking.

Consulted: Required docs and workflows, task/PRD constraints, threat-model research, the `understand` skill, live source/tests, Electron 41 declarations, and focused git history.

Self-review: No unresolved scope, security-boundary, wire-contract, test, or documentation omissions found; focused Prettier and `git diff --check` passed.

Outcome: Canonical plan drafted with planning status `draft` and build status `in_progress`, ready for critique.

Critiquer: Iteration 1
Timestamp: 2026-08-13T21:10:12Z

Review: The plan is well researched and preserves the task-101/task-102 boundary, but lifecycle defects prevent approval.

Blocking findings:
- Capturing `webContents.mainFrame` before `loadURL` risks binding the initial `about:blank` frame. Define two-stage authority: bind WebContents/generation before setup while rejecting requests, activate the exact frame/document only after canonical commit/load, revoke before navigation, and safely reactivate canonical reloads. Test about:blank rejection, initial activation, reload replacement, stale-frame rejection, and noncanonical navigation.
- Suppressing stale responses leaves request promises/listeners pending. Add deterministic cancellation and exact listener cleanup for navigation, destruction, replacement, send failure, and response/cancellation races, without an arbitrary timeout.
- Re-running `ipcApi(window)` on recreation can duplicate process-global listeners whose dynamic authority all accepts the replacement window, causing duplicate privileged execution. Define process-once registration with updated dependencies or disposable registration, and prove exactly one handler invocation/response after repeated replacement.

Non-blocking observations:
- State that malformed or unauthenticated requests without a valid correlation ID are silently dropped; authenticated valid requests retain existing error semantics.
- Describe renderer-side response authentication narrowly because `IpcRendererEvent.sender` is not analogous to main-side frame authority.
- Reconcile task-100 as a tracker dependency or document why it remains only an implementation relationship.
- Make the Electron runtime fixture unconditional.
- Preserve one authority, one private envelope, no generic registry/invoke migration/guest abstraction/task-102 sweep.

Approval bar:
- Revise for two-stage authority, deterministic pending-request cancellation/cleanup, and process-safe recreation registration, with focused unit and Electron 41 evidence including exactly-once execution after recreation.
- Retain caller-frame targeting, revalidation, malformed/spoof handling, correlation/concurrency, concrete compatibility, task-102 ownership, and fail-closed production gating.

Outcome: Comprehensive first-pass critique completed with a consolidated blocker list.
Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-13T21:17:35Z

Plan: Revised the canonical task-101 plan with two-stage inactive/active generation binding, revoke-before-navigation, canonical reload activation, deterministic race-safe cancellation and exact cleanup without timeout, and process-once wrapper registration with current-window rebinding/disposal.

Blockers resolved:
- Initial `about:blank` remains unauthorized; only the exact canonical completed current frame activates.
- Navigation, destruction, replacement, send failure, and response/cancellation races settle pending requests and remove exact listeners once.
- Repeated window recreation installs no duplicate wrapper listeners and must produce exactly one invocation/response.

Clarity incorporated: Malformed or unauthenticated requests are silently dropped, renderer authentication is narrowly scoped, `test:trusted-ipc` is unconditional, and task-100 becomes a deliberate tracker dependency.

Self-review: Acceptance criteria, tests, files, risks, verification, tracking, and scope are consistent. Task-102 retains raw-listener migration ownership. Planning status remains `draft`; focused Prettier and `git diff --check` passed.

Outcome: Canonical plan revised for the focused approval check with every iteration-1 blocker resolved.

Critiquer: Iteration 2
Timestamp: 2026-08-13T21:18:35Z

Review: The revised canonical plan resolves the complete iteration-1 blocker set without broadening task-101.

Blocking findings:
- None.

Non-blocking observations:
- The live tracker still lists only `task-001`; the plan coherently assigns adding completed `task-100` to implementation synchronization.
- Malformed requests are silently dropped, renderer authentication is narrowly described, and the Electron fixture is unconditional.

Approval bar:
- Met. Two-stage authority rejects `about:blank`, revokes before navigation, and activates only the canonical loaded current frame. Generation-scoped cancellation covers navigation, destruction, detachment, replacement, send failure, and races with exactly-once cleanup. Process-once/disposable registration and current-window rebinding prevent duplicate execution after recreation. Required unit/runtime evidence, documentation, tracker lifecycle, task-102 boundaries, and production gating are coherent.

Outcome: Revised plan approved for implementation.
Decision: approved
