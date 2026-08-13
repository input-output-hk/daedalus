# Task task-100: Harden trusted-window navigation and external URLs

## Task

- Task ID: `task-100`
- Title: `Harden trusted-window navigation and external URLs`
- Phase: `phase-1` (`Electron And IPC Security Foundation`)
- Priority: `critical`

## Why This Task Now

- `task-001` is complete and establishes that the existing Node-enabled renderer is privileged legacy UI that must never navigate to remote content.
- This is the first unblocked phase-1 hardening task and is a direct dependency of `task-105`, which will later create the separately sandboxed dApp guest.
- Live startup currently creates and starts loading the trusted window before registering a global popup policy; that policy also sends every popup target to `shell.openExternal` without validation. The renderer shell IPC similarly accepts arbitrary strings.
- Hardening this existing boundary now reduces current exposure without waiting for the broader sender/frame authentication work owned by `task-101` and `task-102`.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs: none. The task graph, PRD, accepted threat model, and live application URLs provide the required policy decisions.
- Required manual test steps: none for task completion. Focused unit tests plus the agent-executable pinned-Electron runtime fixture, TypeScript compilation, lint/format checks, and a production main build provide the required evidence for this bounded change.
- Required evidence from the user: none. Packaged hostile-renderer, OS sandbox, and external-audit evidence remain downstream gates and must not be claimed here.
- Implementation can proceed immediately and can be completed without pausing for user interaction.

## Scope

- Register the application-wide popup deny policy before any application `WebContents` can be created.
- Remove automatic popup-to-shell behavior. A popup request is denied regardless of target and does not create a window or invoke `shell.openExternal`.
- Install the trusted main window's navigation policy immediately after `BrowserWindow` construction and before its first `loadURL` call. Enforce the canonical trusted document on renderer-initiated main-frame navigation and redirects, and deny every subframe document navigation so the privileged window hosts no remote frame content.
- Permit only the trusted renderer document appropriate to the current environment: the exact local development renderer document in development and the normalized packaged renderer file in production/test. Deny malformed, unexpected, and remote document navigation without relying on URL prefixes.
- Parse renderer-requested external URLs, require HTTPS, reject malformed values and embedded credentials, and await `shell.openExternal` so failures are returned through the existing IPC response path.
- Convert external-URL and local-directory IPC listener setup from import-time side effects to explicit process-scoped registration before main-window creation, outside the recreatable `ipcApi(window)` path.
- Add focused tests plus a narrow Electron 41 runtime fixture for the event matrix, redirect/hash/popup behavior, denied-load recovery isolation, URL policy, awaited shell failures, renderer rejection consumption, and process-scoped listener ownership.

## Non-Goals

- Do not create a dApp guest, guest preload, session policy, destination-bound network policy, route lease, or CIP-30 broker. Tasks `104` through `106-a` own those surfaces.
- Do not migrate the trusted renderer to context isolation or remove Node integration in this task.
- Do not implement privileged IPC sender/main-frame authentication or refactor the shared request/response protocol. Tasks `101` and `102` own that work. Denying trusted-window subframe documents here is containment, not authorization of any frame.
- Do not add popup URL allowlists or route popup targets through the external-URL IPC. Popup requests remain side-effect-free and denied; trusted UI must explicitly use the existing external-link action.
- Do not restrict `shell.openPath` to URL semantics. It remains a local-directory operation, but its listener becomes explicitly registered and its asynchronous error remains awaited.
- Do not sweep static, main-owned menu URLs or unrelated `shell.openExternal` call sites; their inputs are not renderer-controlled and are outside this task's stated target.
- Do not add a generalized URL-policy framework for the future hostile guest. Guest origin and connection-destination policy requires stricter, separate logic under `source/main/dapp/`.
- Do not enable `nodeIntegrationInSubFrames` or load the privileged preload in subframes. Electron 41 defaults that option to false; task-100 nevertheless denies all subframe document navigation rather than treating lack of inherited Node/preload authority as permission to host remote content.
- Do not edit either review log.

## Dependencies

- `task-001`: completed. Its accepted ADR requires a trusted navigation lock, no automatic unsafe external opening, and continued separation between the privileged renderer and hostile remote content.
- `task-101`: independent pending follow-up for exact trusted sender and main-frame authentication. Task-100 must not imply that existing shell IPC is safe for a guest before task-101/102 complete.
- `task-102`: later audits and migrates every privileged IPC handler; task-100 provides explicit registration for the two shell handlers but does not perform that audit.
- `task-105`: depends on task-100 and must retain a separate hostile-guest popup/navigation/session policy rather than reusing the trusted renderer's local-document allow rule.
- No dependency on the pending Linux sandbox tasks is required because this task does not create or enable remote guest content.

## Research Consulted

- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
  - Confirms the late popup registration, unsafe popup-to-shell behavior, unvalidated external-URL IPC, privileged trusted renderer, and assignment of trusted navigation hardening to task-100.
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md` and `06-linux-system-package-decision.md` were checked for coupling. They do not change this task's implementation; production guest launch and packaged sandbox proof remain separate pending gates.
- No standards, backend, exact-CBOR, hardware, or connector-contract research applies to this Electron boundary task.

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/dapp-browser-cip30/prompt.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `.agent/workflows/electron.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `understand` was loaded before repository exploration. No existing `.understand-anything` graph or metadata was present; generating a full project graph would be disproportionate and would create unrelated artifacts, so all material architecture and call-path findings were verified directly against live source and focused git history.
- No frontend, IPC-contract, e2e-Cucumber, i18n, Storybook, Cardano, CBOR, hardware, or operator skill is required for this bounded main-process policy task.

## Verified Live Findings

- `source/main/windows/main.ts` creates a trusted `BrowserWindow` with `nodeIntegration: true`, `contextIsolation: false`, and the privileged preload. It invokes `loadURL` without first installing a top-level navigation guard.
- The trusted initial document is `http://127.0.0.1:8080` in development and `file://${__dirname}/../renderer/index.html` otherwise. Renderer application routing is hash-based, so locking document navigation does not require allowing arbitrary web origins.
- `source/main/index.ts` registers `app.on('web-contents-created', ...)` only after `createMainWindow()`, node setup, menus, IPC setup, and disk-space work. Its handler denies the popup but first invokes `shell.openExternal(details.url)` without parsing, scheme restriction, awaiting, or error handling.
- `source/main/ipc/open-external-url.ts` creates a singleton channel and calls `onReceive` at module import. It passes the renderer string directly to `shell.openExternal`; because Electron's current API returns a promise, the truthy ternary immediately resolves instead of awaiting shell success or failure.
- `source/main/ipc/open-local-directory.ts` also registers at import time. It already awaits `shell.openPath` and turns Electron's nonempty error string into a rejected response.
- `source/main/ipc/index.ts` imports both already-active channels and references them as no-op expressions, making listener ownership and initialization order implicit.
- Renderer external-link calls are centralized through `AppStore.openExternalLink` and `openExternalUrlChannel`. The application has many legitimate HTTPS destinations, so a static hostname allowlist would exceed the task's requirements and risk regressions; a parsed HTTPS policy is the smallest truthful boundary.
- There are no focused tests for trusted-window navigation, popup handling, or these shell IPC modules. Existing Jest configuration discovers colocated `*.spec.ts` files under `source/` and existing main-process tests demonstrate that style.
- Electron 41 typings document the exact boundary: `will-navigate` and `will-frame-navigate` do not fire for `webContents.loadURL`, history APIs, or same-document hash changes; `will-redirect` is separately cancellable; `will-frame-navigate` identifies main versus subframe attempts. `nodeIntegrationInSubFrames` defaults to false and is not enabled in the live window.
- The live `did-fail-load` listener forwards every failure with the wrong callback shape to a singleton `RendererErrorHandler`; its retry calls `createMainWindow()` without the required locale/bounds arguments. A prevented navigation can report `ERR_ABORTED` (`-3`) and must not enter that path. Only non-aborted main-frame load failures should remain eligible for existing recovery, with a correctly closed-over recreation callback.
- `ipcApi(window)` is inside `createMainWindow`, so shell listener registration there is not process-safe under the intended recovery path. Process-owned shell listeners must be registered once from startup before the first window, not made part of window recreation.
- `AppStore.openExternalLink` currently discards the channel promise. Once main truthfully rejects validation or shell failures, this caller must consume the rejection to avoid an unhandled renderer promise while preserving the channel's rejecting contract for awaited callers.
- No existing release-equivalent test covers this event matrix. The current Cucumber/Spectron suite launches the full built app but is backend-heavy; a standalone local Electron fixture using the pinned Electron binary, local file/HTTP fixtures, and the production policy seam is the smallest viable runtime evidence.
- Git status also contains Orchestrator-owned task review artifacts. They remain untouched by Planner.

## Expected Files

- `source/main/index.ts`
  - Install the application-wide popup deny policy synchronously before the ready lifecycle can create application `WebContents`; remove late unsafe popup handling and the now-unused `shell` import.
- `source/main/windows/main.ts`
  - Define the trusted renderer target once, install navigation enforcement before first load, and load that same target.
- `source/main/windows/navigationPolicy.ts` or an equivalently small main-window policy module
  - Hold testable trusted-document URL comparison and popup/navigation registration logic if extraction materially simplifies tests; avoid a broad reusable guest policy abstraction.
- `source/main/windows/navigationPolicy.spec.ts`
  - Focused canonical comparison, event routing, subframe denial, redirect denial, and aborted-load classification tests.
- `source/main/ipc/open-external-url.ts`
  - Export explicit registration and a small parsed-HTTPS validator/normalizer; await shell completion.
- `source/main/ipc/open-external-url.spec.ts`
  - Validate accepted normalized HTTPS URLs, malformed/non-HTTPS/credential rejection, no shell side effect on rejection, awaited success/failure, and no listener registration merely from import.
- `source/main/ipc/open-local-directory.ts`
  - Replace import-time listener setup with explicit registration while retaining awaited `openPath` error handling.
- `source/main/ipc/open-local-directory.spec.ts`
  - Prove explicit registration and propagation of Electron's asynchronous error string if this can be covered without duplicating shared IPC-wrapper tests.
- `source/main/ipc/index.ts`
  - No longer owns the two process-scoped shell listeners; retain all genuinely window-scoped initialization.
- `source/main/ipc/registerShellIpc.ts` or an equivalently small startup module
  - Explicitly register external-URL and local-directory listeners once before main-window creation. A module-local installed guard makes accidental repeated startup invocation a no-op and is covered by tests.
- `source/renderer/app/stores/AppStore.ts`
  - Consume the intentionally fire-and-forget external-link rejection without logging submitted data or exposing an unhandled promise.
- `source/renderer/app/stores/AppStore.spec.ts` or the nearest focused existing store spec
  - Prove external-link rejection is consumed and does not become an unhandled rejection.
- `tests/security/trusted-window/`
  - Narrow standalone Electron 41 fixture and local pages/redirect server covering actual navigation, redirect, hash, popup, and recovery behavior without starting cardano-node/cardano-wallet.
- `package.json`
  - Add one explicit command for the narrow trusted-window Electron runtime fixture if a direct invocation cannot remain self-documenting without it.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - Include task-100 current-boundary wording in the implementation-review diff, without claiming sender authentication, guest safety, or task completion.
- `.agent/system/architecture.md`
  - Include the planned/current trusted-window boundary update in the implementation-review diff, distinguishing this live document control from remaining IPC and sandbox gates.
- `.agent/system/api-endpoints.md`
  - Include the runtime string, parsed credential-free HTTPS, canonicalization, privacy-safe awaited failure, and renderer rejection-consumption contract in the implementation-review diff.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - In the reviewed implementation diff, reconcile task-100 acceptance with explicit main-frame and deny-all-subframe policy while leaving status pending. Mark task-100 complete and add completion metadata only after implementation review approval.
- `.agent/plans/dapp-browser-cip30/task-plans/task-100.md`
  - Lifecycle, verification evidence, research disposition, and final outcome updates.

The exact unit-test split may be reduced if one focused spec covers both shell handlers without obscuring behavior. No shared IPC type, dependency, lockfile, translation, Storybook, Cucumber, Nix, or sibling-backend change is expected.

## Implementation Approach

1. Install popup denial before application WebContents creation.
   - Move registration out of `onAppReady` and execute it synchronously in main startup before the ready handler can create the first `BrowserWindow`.
   - On every `web-contents-created`, install `setWindowOpenHandler(() => ({ action: 'deny' }))` immediately.
   - Do not call the shell, log the full requested URL, create another window, or conditionally allow a target. This policy also provides a safe default for future WebContents, while task-105 must still install its guest-specific controls.

2. Lock every trusted-window document navigation before initial load, with an explicit Electron 41 event matrix.
   - Compute one canonical trusted document URL once: `new URL('http://127.0.0.1:8080/')` in development and `pathToFileURL(path.resolve(__dirname, '../renderer/index.html'))` otherwise. Compare parsed, normalized components with fragments removed; for development require exact protocol, hostname, port, pathname, empty credentials, and no unexpected search, and for packaged mode require the exact normalized file URL. No prefix or starts-with comparison is allowed.
   - Install policy immediately after `new BrowserWindow(...)`, before IPC initialization and before the sole application-owned initial `loadURL(trustedUrl.href)`.
   - Initial/programmatic loads: Electron does not emit `will-navigate`/`will-frame-navigate` for `webContents.loadURL`, `back`, or similar APIs. The initial load is safe because application code receives only the policy-produced canonical URL. This task adds no later programmatic navigation API; any future call must pass the same policy validator before calling Electron.
   - Renderer/user main-frame navigation: handle `will-navigate` and fail closed unless `details.url` is the canonical trusted document after fragment removal. Hash-only and History API changes do not emit this event and remain allowed as same-document routing; verify their resulting origin/path stays unchanged through `did-navigate-in-page` evidence rather than pretending that event is cancellable.
   - Server redirects: independently handle `will-redirect`; deny a main-frame redirect unless its destination is the canonical trusted document, including redirect-to-remote and redirect-to-lookalike cases. Record policy denial only as a boolean/counter local to the WebContents, never the destination value.
   - Subframes: handle `will-frame-navigate` and prevent every event where `isMainFrame === false`, regardless of scheme or origin. For `isMainFrame === true`, apply the same canonical check (or delegate to the one main-frame handler without double side effects). The window does not enable `nodeIntegrationInSubFrames`, so Electron does not inherit Node/preload authority into iframes; deny-all additionally satisfies the tracker statement that this privileged renderer hosts no remote content.
   - Keep this policy specific to the trusted window. It is not guest origin, session, egress, or IPC authorization policy.

3. Keep policy-denied loads out of renderer recovery.
   - Consume the full Electron 41 `did-fail-load` callback and ignore subframe failures plus `errorCode === -3` (`ERR_ABORTED`) before invoking `RendererErrorHandler`. This ensures prevented main-frame/subframe navigation and ordinary superseded/cancelled loads cannot create a replacement window or duplicate listeners.
   - Preserve the pre-existing recovery behavior only for non-aborted main-frame load failures. Pass `RendererErrorHandler.setup` a closure that captures the current locale and bounds provider instead of the bare parameterized `createMainWindow` function, removing the immediate missing-argument crash without redesigning global window/cardano lifecycle in this task.
   - Unit-test classification and bound retry arguments, and use the runtime fixture to assert a denied navigation produces no recovery callback, no replacement WebContents, and no listener count increase.

4. Make shell IPC initialization explicit and process-scoped.
   - Change each shell module from an exported, import-time active singleton into an exported registration function.
   - Invoke both through one process-scoped startup registration before `createMainWindow`, outside `ipcApi(window)`. Use a minimal module-local `installed` guard so an accidental repeat is a no-op; listener handlers capture no `BrowserWindow`, so recreation requires no rebind.
   - Preserve the existing channel names and renderer contract; do not refactor the shared IPC wrapper ahead of task-101.
   - Prove imports alone register nothing, first startup registration installs exactly one listener per shell channel, and repeated registration/window recreation does not change listener counts.

5. Enforce and await a privacy-safe external URL contract.
   - Runtime-check `typeof value === 'string'` before parsing with the platform `URL` parser.
   - Require `https:` and a valid nonempty host, reject embedded username/password credentials, and pass the parser's canonical serialized URL to `shell.openExternal`.
   - Throw the same fixed validation error, such as `External URL is not allowed`, before shell invocation for non-string, malformed, disallowed-scheme, credential, or missing-host input. The error and all logs must omit the submitted value, URL components, query, credentials, parser error, and shell error detail.
   - Await `shell.openExternal`. Catch its rejection and return a separate fixed `Unable to open external URL` error through the existing rejecting channel response; do not log the raw rejection because OS errors can repeat the URL. Tests use secret-bearing values/errors and assert those secrets are absent from responses and captured logs.
   - Do not add a hostname allowlist because legitimate destinations are numerous and dynamic, and the selected task requires approved HTTPS scheme parsing rather than a product-domain registry.

6. Consume renderer rejection while retaining truthful channel semantics.
   - Keep `RendererIpcChannel.send` rejecting for validation and shell failures so direct awaited callers receive truthful failure.
   - `AppStore.openExternalLink` is intentionally `void`/fire-and-forget UI behavior. Attach a terminal rejection handler at that call site and do not rethrow or log the error; this consumes the promise without exposing potentially sensitive URL/shell details or creating an unhandled rejection. Add a focused store test with a rejected channel promise.
   - Do not add a new toast, translated copy, return type, analytics event, or renderer-wide error abstraction for this security hardening task.

7. Retain local-directory semantics while removing hidden setup.
   - Register the local-directory receive handler explicitly.
   - Continue awaiting `shell.openPath` and reject when Electron returns a nonempty error string.
   - Do not reinterpret filesystem paths as URLs or add speculative path authorization that belongs to the later privileged IPC audit.

8. Add focused unit and Electron runtime regression tests.
   - Exercise exact trusted target and hash behavior versus HTTPS, alternate local file, custom-scheme, malformed, origin-prefix, port, and credential/navigation bypass cases.
   - Prove popup attempts are always denied and never invoke the shell.
   - Unit tests prove policy registration precedes `loadURL`, redirect and subframe denial call `preventDefault`, hashes compare as the same document, `ERR_ABORTED`/subframe failures bypass recovery, non-aborted main-frame failures retain recovery, and process shell registration is single-install.
   - Exercise non-string and secret-bearing external values, HTTPS normalization, no shell side effect on rejection, generic privacy-safe errors/logs, and deferred/rejected shell promises to prove completion is awaited.
   - Add a standalone fixture launched by `node_modules/.bin/electron` against a tiny main script and local pages/server. It imports the production policy seam, creates only a test `BrowserWindow`, and reports machine-readable assertions before quitting. It must observe actual Electron 41 behavior for: initial canonical programmatic load; renderer-initiated remote main-frame navigation denied; local 30x redirect to remote denied by `will-redirect`; hash/History routing remains in the same loaded document; `window.open` creates no child and invokes no shell; subframe navigation is denied; and denied/`ERR_ABORTED` loads do not invoke recovery, create replacement WebContents, or increase shell/policy listener counts. Use bounded timeouts and fail nonzero; no Cardano backend, packaged sandbox, or full app boot is involved.

9. Include documentation in the reviewed implementation diff; defer only completion bookkeeping.
   - Update PRD current-baseline wording, architecture, and API docs in the same diff submitted to implementation review. Describe the implemented main-document allow policy, deny-all subframe/popup policy, parsed HTTPS external-link contract, privacy-safe failure behavior, and pending sender/frame authentication without claiming task completion before approval.
   - Reconcile the task-100 tracker acceptance text in that reviewed diff to say the privileged trusted window cannot host remote top-level or subframe documents, while leave `status: pending`, `completedAt`, and completion notes unchanged.
   - Only after implementation review approval mark task-100 complete, add completion metadata, and finalize this canonical outcome. Those lifecycle-only changes are then checked for consistency and are not substantive unreviewed design/code changes.
   - Record `no new research` in the final canonical outcome unless implementation reveals a durable policy constraint or Electron behavior not already captured in research `01`; if it does, add the next numbered focused research note rather than bloating the PRD.

## Acceptance Criteria

- The global popup handler is registered before any application `WebContents` is created, denies every popup, creates no window, invokes no shell method, and does not log the full target URL.
- The trusted main window installs policy before its first `loadURL`; the only application-owned programmatic load receives the same canonical URL produced by that policy. Renderer/user main-frame navigation is checked by `will-navigate`, server redirects by `will-redirect`, and all subframe document navigation by `will-frame-navigate`.
- The privileged trusted window cannot host remote content in either its top-level document or subframes. It denies remote HTTP(S), another local file, custom schemes, malformed input, credentials, and origin/path lookalikes using parsed canonical comparison; `nodeIntegrationInSubFrames` remains disabled.
- The expected development renderer document and normalized packaged renderer file remain loadable. Same-document hash/History routing remains functional and is explicitly recognized as non-cancellable by the navigation events rather than claimed as event-allowed.
- Policy-denied, cancelled, subframe, and `ERR_ABORTED` load failures do not enter renderer recovery, create a replacement window/WebContents, or duplicate listeners. Non-aborted main-frame failures retain the existing recovery behavior with valid bound arguments.
- External URL IPC runtime-checks strings, accepts parsed credential-free HTTPS URLs with a valid host, rejects all other values before side effects, and sends only the canonical serialization to `shell.openExternal`.
- Validation and shell failures use fixed privacy-safe errors and logs containing none of the submitted URL/query/credentials or raw shell error. `shell.openExternal` is awaited; its rejection remains truthful to awaited channel callers and is consumed by `AppStore.openExternalLink` without an unhandled renderer rejection.
- External-URL and local-directory imports do not register IPC listeners. Their process-scoped listeners are explicitly installed once before the first main window and remain single-installed across repeated registration or window recreation.
- Local-directory opening continues to await `shell.openPath` and rejects its nonempty error response.
- Focused automated tests cover navigation/redirect/subframe/popup bypasses, aborted-load recovery isolation, external URL runtime validation/privacy/awaiting, renderer rejection consumption, process listener ownership, and shell failure propagation.
- A narrow test using the pinned Electron 41 runtime proves initial load, renderer navigation, redirect, hash routing, popup/subframe denial, and no denied-load recovery or listener/window duplication. It does not claim packaged guest or OS sandbox evidence.
- No guest window, remote content, new privileged channel, shared IPC protocol refactor, renderer migration, or sandbox/release-gate claim is introduced.
- PRD, architecture/API documentation, canonical plan, and task tracking accurately distinguish completed task-100 controls from pending task-101/102 and guest/sandbox work.

## Verification

- Run focused Jest specs for the trusted navigation/popup policy and shell IPC handlers, using the repository's `yarn test:jest <paths>` form or direct equivalent.
- Run the dedicated pinned-Electron trusted-window runtime command/fixture and retain its machine-readable pass/fail output for all required event-matrix cases. This is local Electron runtime evidence, not a mocked Jest claim and not packaged hostile-guest evidence.
- Run `yarn compile` to validate Electron and event typings, including the current Electron 41 navigation event API.
- Run focused ESLint and Prettier checks for touched TypeScript and Markdown files; use direct touched-file commands if repository-wide scripts include unrelated pre-existing failures, and record that distinction.
- Run `yarn build:main` to verify startup imports, webpack bundling, and explicit IPC registration in the production main bundle.
- Inspect the focused diff and run `git diff --check`.
- Parse `dapp-browser-cip30-tasks.json` after lifecycle synchronization and verify no unrelated task status, dependencies, or critical-path data changed.
- Re-read the latest matching planning and implementation review-log entries during later lifecycle work while leaving review logs append-only and Orchestrator-owned.
- Verify documentation statements directly against final live files: event matrix, main/subframe policy, denied-load recovery isolation, process listener ownership, trusted target construction, shell side-effect/privacy behavior, renderer rejection consumption, and remaining pending security gates.
- No Cucumber E2E, packaged Electron sandbox proof, configured-network test, wallet data, hardware device, external audit, or user manual evidence is required for this task. Those tests would not add truthful evidence for this bounded policy implementation.

## Risks And Open Questions

- Electron event semantics are explicit: `will-navigate`/`will-frame-navigate` omit programmatic and same-document navigations, `will-redirect` is separately cancellable, and `did-navigate-in-page` is observational. Unit tests protect policy logic; the narrow runtime fixture protects actual Electron 41 event behavior.
- Initial-load boundary: application-owned `loadURL` must use the same canonical target that the policy trusts so construction and enforcement cannot drift.
- Subframe scope: task-100 denies every trusted-window subframe document navigation. This is intentionally stricter than depending on Electron's default `nodeIntegrationInSubFrames: false`, because the tracker says the privileged renderer cannot host remote content. It still does not implement the future hostile guest's frame identity, session, or network egress policy; task-105 and `106-a` retain that ownership.
- Development compatibility: only the existing loopback renderer origin is required. Do not allow arbitrary loopback ports, host aliases, or remote development hosts without a separately reviewed concrete need.
- Existing IPC wrapper limitations: explicit registration removes hidden import effects but does not authenticate sender/frame or fix response races. Documentation and tests must not overstate this handler as guest-safe before tasks 101/102.
- Error privacy: popup targets, rejected external values, and OS shell errors may contain sensitive query/credential data. New errors/logs are fixed messages and tests assert secret absence; the fire-and-forget renderer consumer emits no error log.
- Recovery ownership: live recovery has broader lifecycle debt independently of navigation policy. Repair only argument binding and failure filtering needed to ensure a denied load cannot trigger recreation or crash; record any remaining non-aborted recovery ownership issue for its proper owner rather than redesigning lifecycle here.
- Runtime-fixture portability: use only local file/loopback resources, bounded waits, the repository-pinned Electron binary, and machine-readable exit status. If the environment cannot launch Electron due to missing display/system libraries, task completion is blocked rather than replacing this evidence with mocks; the available workspace is expected to support the existing Electron E2E toolchain, so no user checkpoint is planned.
- Testability versus abstraction: one small policy module is justified if it avoids loading the entire main entry point in Jest. Do not introduce factories, registries, policy classes, or guest-oriented generality solely for tests.
- Static menu shell calls use trusted constants and are excluded. If implementation inspection finds renderer-controlled data reaching another direct shell call, record it for task-102 rather than silently widening this task unless it defeats task-100 acceptance.

## Docs, Tracking, And Research Updates

- Include `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md` in the implementation-review diff to remove stale task-100 baseline claims while retaining all remaining release gates and pending status.
- Include `.agent/system/architecture.md` in that reviewed diff, stating that trusted main-frame canonical navigation plus deny-all subframe/popup policy are implemented while sender/frame IPC authentication and guest containment remain planned.
- Include `.agent/system/api-endpoints.md` in that reviewed diff with runtime-string, parsed credential-free HTTPS, canonical serialization, privacy-safe awaited-failure, and renderer-consumption behavior for `OPEN_EXTERNAL_URL_CHANNEL`; local-directory semantics are unchanged except process listener lifecycle.
- Include the task-100 acceptance wording reconciliation in `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json` during implementation review while status remains pending. Only after approval mark task-100 complete with truthful notes, preserving unrelated graph state.
- Update this canonical plan with final planning/build lifecycle, verification evidence, and final outcome.
- Research disposition: no new research is expected because research `01` already captures the durable baseline and ownership. Add a focused research note only for a genuinely new Electron constraint or security decision, and otherwise record `no new research` in the final outcome.
- No workflow, plans index, dependency lockfile, translation, Storybook, Nix, or backend documentation update is expected. `package.json` changes only if needed to expose the dedicated runtime-fixture command.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-100-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-100-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope creep: limited changes to trusted-window document containment, global popup denial, denied-load recovery filtering/argument ownership, process-owned shell setup, the renderer-requested external URL boundary and its existing fire-and-forget caller, focused tests, and required documentation/tracking. Guest policy, sender authentication, shared IPC refactoring, renderer migration, and unrelated shell calls remain with their owning tasks.
- Complete Electron matrix: initial/programmatic load is policy-generated and not event-covered; renderer/user main-frame navigation uses `will-navigate`; redirects use `will-redirect`; same-document hash/History routing is non-cancellable and runtime-observed; every subframe document attempt is denied through `will-frame-navigate`.
- Denial/recovery: `ERR_ABORTED`, cancelled, and subframe failures cannot reach recovery; runtime evidence proves no replacement WebContents or listener increase, while the existing non-aborted main-frame retry gets correctly bound arguments without a lifecycle redesign.
- Listener ownership: shell listeners are process-scoped before window creation, import-side-effect-free, and guarded single-install; they are not registered in recreatable `ipcApi(window)`.
- Runtime validation/privacy: non-string input, malformed/scheme/credential rejection, canonical serialization, generic validation/shell errors, no submitted/raw error logging, and secret-absence assertions are explicit.
- Renderer rejection: the existing `void` AppStore action consumes terminal rejection without rethrow/logging, while direct channel callers retain a rejecting promise.
- Viable runtime evidence: a local standalone fixture launches the pinned Electron binary and production policy seam without Cardano services, covering the exact event/order/recovery matrix that Jest mocks cannot prove.
- Docs in reviewed diff: PRD, architecture, API docs, and tracker acceptance reconciliation are reviewed with code; only task completion metadata and final canonical outcome wait for approval.
- Top/subframe consistency: deny-all subframes fulfills the tracker and fixed remote-content rule. Electron's default no-Node/no-preload subframe inheritance is documented but not used to permit remote iframes; guest frame/session/egress policy remains downstream.
- Missing tests/docs: included adversarial URLs, redirect/subframe/hash behavior, registration order, no popup shell side effect, denied-load isolation, import/single-registration, asynchronous shell success/failure, privacy assertions, renderer promise consumption, pinned Electron runtime, compilation/build, PRD/architecture/API docs, tracker parsing, and diff checks.
- Trust-boundary drift: the trusted renderer remains privileged legacy UI; task-100 prevents remote top-level and subframe documents but does not call it sandboxed or guest-safe. Remote content remains restricted to the future separate guest, and popup requests never become an automatic external-opening capability.
- Smallest truthful solution: preserve channel names and renderer callers, use built-in URL parsing, retain the existing IPC wrapper until task-101, and extract at most one small policy module for testability. No allowlist registry, policy class, new IPC contract, or future guest machinery is proposed.
- Inconsistencies: the plan follows live Electron code rather than the stale workflow example that shows a context-isolated main window. It preserves the PRD's current Node-enabled baseline and production-guest-disabled posture.
- Interaction mode: `autonomous` is truthful because all required runtime and static evidence can be produced in this workspace; no packaged sandbox, physical hardware, configured-network, or external-review checkpoint belongs to task-100.
- Repository understanding: `understand` consultation is recorded, and direct reads/history verified every material claim because no knowledge graph existed.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Completed and approved in implementation review iteration 2.
- Final implementation locks trusted main-frame navigation and redirects to the canonical local document, denies every subframe and popup, keeps aborted/subframe failures out of renderer recovery, and explicitly registers process-scoped shell IPC once.
- External opening runtime-validates credential-free HTTPS, canonicalizes the URL, awaits shell success/failure with fixed privacy-safe errors, and consumes fire-and-forget renderer rejection.
- Verification passed: 5 focused Jest suites with 22 tests; `yarn compile`; focused ESLint with zero errors; focused Prettier; `yarn build:main`; `git diff --check`; and `CHROME_DEVEL_SANDBOX=/opt/google/chrome/chrome-sandbox yarn test:trusted-window` with all 12 bounded runtime assertions true. The helper was verified root-owned mode `4755`; the committed command contains no sandbox-disabling switch. This is trusted-window runtime evidence, not packaged guest sandbox certification.
- Documentation synchronized: PRD current baseline and IPC-hardening section, architecture, API endpoint contract, and task tracker acceptance/completion metadata.
- Research outcome: no new research. The durable policy decisions were already captured in research `01`; the host-specific SUID helper was only execution evidence for this local fixture and does not alter accepted packaging or sandbox design.
- User handoff: none. No user feedback or manual evidence was required.
- Final review: `Decision: approved` in `.agent/plans/dapp-browser-cip30/task-plans/task-100-impl-review.md`.
- Final task commit: `f21bed26fd5fa23b8e8932136659316216dee388` (`fix(dapps): task-100 harden trusted navigation`).
