Planner: Iteration 1
Timestamp: 2026-08-18T01:14:37Z

Plan: Created canonical draft `.agent/plans/dapp-browser-cip30/task-plans/task-102.md` with autonomous interaction mode and no human checkpoint. The plan covers an exhaustive machine-checkable privileged IPC manifest, production source drift checks, migration of raw close/resize listeners onto task-101 authenticated wrappers, recreation-safe window targeting, hostile known-channel Electron evidence, and required documentation, research, and tracking updates.

Consulted: Required architecture, PRD/task dependencies, task-102 research evidence, Electron/IPC/test/documentation workflows, task-101 canonical plan and reviews, the task-plan policy, the `understand` skill, live main/window/preload/shared/renderer IPC surfaces, tests, and focused git history.

Self-review: Scope remains limited to the privileged IPC audit and two proven raw migrations. Stale workflow guidance and documentation conflicts are recorded; manifest parity, tests, docs, security boundaries, lifecycle behavior, and deliberate close/resize private-wire migration are covered without guest, backend, packaging, or hardware-certification scope. Focused Prettier and `git diff --check` passed.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-18T01:17:37Z

Review: The plan is comprehensive and correctly preserves the task-101 boundary, but two knowable audit and lifecycle gaps prevent approval.

Blocking findings:
- The proposed source audit is not exhaustive over Electron 41 ingress APIs. It names only `ipcMain.on/once/handle/handleOnce`, `WebContents.ipc` listeners, and renderer `send/invoke/sendSync`. It would miss equivalent EventEmitter registrations such as `addListener`, `prependListener`, and `prependOnceListener`, renderer `postMessage`, aliases/destructuring of Electron objects or methods, and any applicable `WebContents` IPC events or `MessagePortMain` receive path. Define the audited Electron 41 receiver/caller API set from live declarations and require symbol-aware detection so spelling or alias changes cannot bypass the empty production allowlists.
- The plan inventories temporary main-side response listeners but does not require lifecycle-safe ownership of every production main-originated wrapper promise. Live calls in `source/main/cardano/setup.ts`, menus, hardware callbacks, download paths, and other senders often invoke async `MainIpcChannel.send()` without awaiting or handling rejection. Task-101 invalidates these requests on trusted-window navigation, destruction, or replacement, so recreation can produce unhandled rejections even though spoofed responses are rejected. Audit every main-originated `send/request` response surface, establish a minimal consistent settlement policy for awaited and intentional fire-and-forget calls, and test navigation/destruction/replacement with zero unhandled rejection, leaked listener, stale delivery, or alternate-window targeting.

Non-blocking observations:
- The manifest must distinguish constructor ownership, handler-registration ownership, and direction because hardware channels are created through `createChannels(MainIpcChannel)` and several channels have both renderer-to-main handlers and main-originated request/response traffic.
- Define “production source” precisely, excluding colocated `*.spec.ts` files and fixture roots without allowing exclusions for ordinary `source/main` or renderer paths.
- For close/resize, preserve the existing synchronous store action behavior while explicitly consuming adapter promises; close must reply before scheduled teardown and resize cancellation must not become an unhandled rejection.
- The manifest-driven sentinel fixture is appropriate and avoids executing real filesystem, TLS, update, Mithril, or hardware logic. Its results should identify every logical channel and relevant private endpoint, while transport-generic assertions can remain shared to avoid unnecessary per-channel implementation duplication.
- Packaging claims, guest-disable gates, privacy requirements, documentation synchronization, task ownership, and autonomous interaction classification are otherwise coherent.

Approval bar:
- Expand the static audit to cover all equivalent Electron 41 renderer-to-main registration and caller APIs with symbol/alias-aware detection and empty production bypass allowlists.
- Add complete lifecycle ownership and verification for every main-originated wrapper response promise, including recreation, navigation, destruction, cancellation, cleanup, and current-window targeting.
- Retain the manifest parity checks, close/resize atomic migration, every-known-channel hostile probes, trusted positive controls, privacy-safe evidence, separate guest boundary, and all downstream package/sandbox/release gates.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-18T01:23:22Z

Plan: Revised the canonical task-102 plan with declaration-derived Electron 41 ingress/caller coverage, symbol-aware auditing, exhaustive main-originated wrapper promise ownership, and lifecycle-safe current-window targeting.

Blockers resolved:
- Covers global, WebContents-, frame-, and service-worker-scoped IPC; EventEmitter-equivalent registrations; WebContents IPC events; renderer `postMessage`; and MessagePortMain paths, including aliases, destructuring, binding, and re-exports. Production receiver and caller bypass allowlists are empty.
- Audits all 26 live main-originated wrapper calls, distinguishes awaited from intentional fire-and-forget ownership, and requires navigation/destruction/replacement cancellation, exact cleanup, latest-window targeting, and zero unhandled rejection.

Clarifications incorporated: The manifest separates constructor, registration, and caller ownership; production roots exclude only tests/fixtures; close/resize store actions remain synchronous while consuming promises; hostile fixture results identify every logical channel and private endpoint.

Self-review: The revision uses existing wrappers and current-window seams without adding a runtime registry, timeout, compatibility route, guest work, or release claims. Prettier and `git diff --check` passed.

Outcome: Plan revised and ready for focused approval check

Critiquer: Iteration 2
Timestamp: 2026-08-18T01:25:10Z

Review: The revision resolves both original blockers and incorporates the relevant clarifications without harmful scope growth. Live Electron 41.3.0 and Node EventEmitter declarations support the expanded global, scoped, frame, service-worker, WebContents-event, renderer `postMessage`, and `MessagePortMain` coverage. Live source also confirms the three raw close/resize registrations, the two direct renderer callers, and 26 main-originated wrapper calls addressed by the lifecycle ownership requirements.

Blocking findings:
- None.

Non-blocking observations:
- Constructor, registration, caller, direction, and settlement ownership are now distinct in the manifest.
- Production roots, test-only exclusions, synchronous store behavior, close-before-teardown ordering, private-endpoint evidence, and current-window rebinding are sufficiently explicit.
- The plan remains bounded to evidence, close/resize migration, and lifecycle-safe ownership, with guest and release work still downstream.

Approval bar:
- Met. Both original blockers and all relevant iteration-1 clarifications are resolved.

Decision: approved
