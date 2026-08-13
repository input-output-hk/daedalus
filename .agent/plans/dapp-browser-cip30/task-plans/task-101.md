# Task task-101: Add trusted renderer sender and frame authentication

## Task

- Task ID: `task-101`
- Title: `Add trusted renderer sender and frame authentication`
- Phase: `phase-1` (`Electron And IPC Security Foundation`)
- Priority: `critical`

## Why This Task Now

- `task-001` established the hostile-renderer boundary and `task-100` now prevents the privileged trusted window from hosting remote top-level or subframe documents.
- The remaining legacy wrapper protocol discards Electron sender/frame identity, replies through `event.sender.send`, shares one uncorrelated response channel per logical channel, and installs `IpcChannel` response listeners after sending.
- `task-101` must provide the authenticated, correlated wrapper foundation before `task-102` can audit and migrate every privileged listener. Production guest creation remains blocked until that migration is complete.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs: none. The accepted PRD, trusted-window URL policy, Electron 41 event/frame APIs, and current call sites determine the bounded design.
- Required manual test steps: none. Focused Jest tests, an agent-executable pinned-Electron runtime fixture, TypeScript, lint/format checks, and the main/renderer builds provide the required evidence.
- Required evidence from the user: none. Packaged hostile-renderer proof, complete privileged-handler migration, guest sandbox evidence, hardware certification, and external audit remain downstream gates.
- Implementation can proceed immediately without pausing for user interaction.

## Scope

- Preserve Electron IPC events through the common channel and conversation request/response paths so main-side wrappers can authorize the caller before invoking a handler and can revalidate the target before releasing a response.
- Add one main-owned trusted-renderer authority whose initial, cleared, and bound-but-inactive states reject all requests, and whose active state identifies the current trusted main `WebContents`, exact committed/loaded main frame, and canonical trusted document/origin.
- Bind only the new `WebContents` and generation before IPC setup/load, revoke active authority before every non-same-document main-frame navigation, activate only after a canonical main-frame commit/load, and safely replace/reactivate generations during window recreation or canonical reload.
- Make all `MainIpcChannel` and `MainIpcConversation` receive/request registrations use that authority by default, while retaining explicit injectable seams for focused tests and only the concrete legacy call shapes still used in the repository.
- Correlate every `IpcChannel` request and terminal response, install filtered response listeners before sending, authenticate response events, remove listeners on exactly-once settlement/cancellation, and route replies to the exact authenticated caller/frame.
- Bring `IpcConversation` onto the same event-authentication, caller-targeting, deterministic lifecycle-cancellation, and response-authentication rules while retaining its already-correlated listener-before-send behavior.
- Register wrapper-backed main handlers once per process across main-window recreation; update the current-window dependency separately rather than accumulating listeners that all authorize the replacement window.
- Add focused common/main/renderer wrapper tests and narrow Electron runtime evidence for real Electron 41 sender-frame semantics and rejection cases.

## Non-Goals

- Do not inventory, rewrite, or individually migrate every direct `ipcMain`, `WebContents.ipc`, preload, menu, hardware, filesystem, store, TLS, update, or window-control listener. `task-102` owns the complete machine-checkable inventory and migration.
- Do not claim privileged IPC is fully inaccessible to a future guest until task-102 has migrated and tested all listeners outside the core wrappers.
- Do not create the dApp guest, guest preload, scoped gateway, session/origin generation, route lease, CIP-30 broker, capability model, or guest request schemas.
- Do not use these legacy wrappers as the future guest protocol; the PRD requires a separate schema-validated gateway.
- Do not migrate the trusted renderer away from Node integration or its privileged preload.
- Do not change logical channel names, application request/response value types, handler business logic, or error payload semantics except for the private correlation envelope needed by the wrappers.
- Do not adopt `ipcMain.handle`/`ipcRenderer.invoke`, `MessageChannel`, a generic policy registry, or speculative guest abstractions. The smallest safe change repairs the existing wrappers.
- Do not edit either review log.

## Dependencies And Ownership

- `task-001`: completed; supplies the accepted authority invariant that existing IPC accepts only the exact trusted main `WebContents` and main frame.
- `task-100`: completed; supplies `getTrustedRendererUrl`, `isTrustedDocumentUrl`, and the trusted-window navigation event seams. Task-101 deliberately adds it to task-101's tracker dependencies because activation/revocation reuses that implemented canonical-document lifecycle rather than merely relating to it informally.
- `task-102`: consumes this task's authenticated wrapper seam and owns the exhaustive privileged-listener inventory, direct-listener migration, import-time registration cleanup, and hostile known-channel probes.
- `task-105` and later guest tasks remain blocked by task-102 and other phase-1 gates. Completing task-101 alone must not enable production guest launch.
- No sibling `cardano-wallet`, Cardano protocol, hardware, Nix/package, translation, Storybook, or Cucumber dependency is involved.

## Research Consulted

- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
  - Confirms that the wrappers discard required event identity, `IpcChannel` has a shared response race and listener-after-send ordering, and tasks 101/102 own remediation.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`, especially Current Baseline, ADR-001, Boundary And Authority Invariants, Existing IPC Hardening, Main/Electron Tests, and Security Review Gates.
  - Requires exact trusted main `WebContents` plus main frame, rejection without privileged side effects, correlation/concurrency safety, and a separate guest gateway.
- No standards, exact-CBOR, backend, network-egress, packaging, or hardware research changes this wrapper-only design.

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/electron.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/dapp-browser-cip30/prompt.md` for canonical-plan fields, interaction classification, review-log ownership, task boundaries, and convergence policy.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json` task-101 and task-102 boundaries.
- The `understand` repository-understanding skill was loaded first. No `.understand-anything` graph or metadata exists, and generating a full graph for this mature repository would create disproportionate unrelated artifacts; material findings were therefore verified against live files, Electron 41 declarations, call sites, tests, and focused git history.
- No frontend UI, e2e-Cucumber, Storybook, i18n, theme, hardware, Cardano CLI, CBOR, or operator skill applies.

## Verified Live Findings

- `IpcChannel` uses fixed `-broadcast`, `-request`, and `-response` channels. Both send paths call `sender.send` before `receiver.once`, so a synchronous response can be lost. Concurrent requests share one response channel and `once` allows the first response to settle/remove the wrong listener.
- `IpcChannel.onReceive` and `onRequest` pass only the decoded message to application handlers and respond with `event.sender.send`, which targets the sender `WebContents` rather than necessarily the originating frame.
- `IpcConversation` generates a UUID, installs its listener before sending, filters by that UUID, and removes its listener after settlement. It still discards request events, does not authenticate request or response events, and responds through `event.sender.send` rather than the original frame.
- `MainIpcChannel` and `MainIpcConversation` only default the receiver to global `ipcMain`; neither has trusted-window authority. Renderer wrappers default to `global.ipcRenderer` and preserve the current no-argument application call shapes.
- Electron 41 `IpcMainEvent.senderFrame` can be `null` after navigation/destruction. `WebContents.mainFrame`, `WebFrameMain.url`, `origin`, `detached`, `isDestroyed()`, and `IpcMainEvent.reply()` provide the needed exact identity, lifecycle, document/origin, and caller-frame response seams.
- Electron documents `event.reply` as the method that guarantees a response reaches the process and frame that sent the original message. `event.sender.send` is therefore not sufficient for task-101 responses.
- `source/main/windows/main.ts` creates the current trusted window, derives the canonical trusted URL, initializes `ipcApi(window)` before loading, and has an intentional recovery/recreation path. A trusted authority must exist before process listeners register, remain fail-closed until binding, bind immediately after `BrowserWindow` construction, and avoid an old window's late destruction clearing a newer binding.
- `WebContents.mainFrame` exists while the initial document is still `about:blank`; capturing it at construction would authorize the wrong document/frame lifetime. Electron 41 emits `did-start-navigation` before replacement and exposes main-frame completion IDs through `did-frame-navigate`/`did-frame-finish-load`, allowing revoke-before-navigation and post-load resolution through `WebFrameMain.fromId` plus `webContents.mainFrame` identity.
- Process-scoped shell wrappers are registered before the main window exists. This concrete caller requires registration against an initially unbound authority rather than throwing merely because no window has yet been created; invocation must still fail closed until binding.
- Existing main and renderer application handlers accept only the message argument. Adding the event as an optional second handler argument preserves these concrete callers while enabling future audited handlers to inspect already-authenticated context if needed.
- Main-to-renderer notifications pass both `BrowserWindow` and `WebContents`-like senders in live code. The sender abstraction must retain both concrete shapes or normalize `BrowserWindow.webContents` without broad application call-site migration.
- One `MainIpcConversation`/`RendererIpcConversation` pair backs `electron-store`; it is a privileged concrete caller and must receive the same authentication and correlation guarantees without changing its application payload.
- No dedicated wrapper specs currently exist. Nearby main IPC specs mock wrappers rather than testing their security protocol, and the task-100 Electron fixture proves the workspace can exercise pinned Electron 41 behavior with bounded local-only assertions.
- `createMainWindow` currently invokes `ipcApi(window)` on every construction, while most registrations are process-global and several handlers capture the supplied window. Recreating a window can therefore accumulate wrapper listeners; a single-install registration with a separately updated current-window provider is required, following the already-live Mithril pattern of one-time handlers plus a rebound sender.
- Focused history shows these wrappers are old and have no recent protocol redesign; task-100 changed trusted-window lifecycle but intentionally deferred sender/frame authentication.

## Expected Files

- `source/common/ipc/lib/IpcChannel.ts`
  - Add the private request ID envelope, listener-before-send filtered response handling, event-preserving handlers, response-event validation, cleanup, and caller-targeted response dispatch.
- `source/common/ipc/lib/IpcConversation.ts`
  - Apply the same event/response validation and caller-targeted dispatch while retaining its correlated request protocol.
- `source/common/ipc/lib/IpcChannel.spec.ts`
- `source/common/ipc/lib/IpcConversation.spec.ts`
  - Cover ordering, concurrency, spoofed/mismatched responses, cleanup, handler suppression, caller-targeted replies, and errors.
- `source/main/ipc/lib/trustedRendererIpcAuthority.ts` or an equivalently small module
  - Own unbound, bound-inactive, and active generation state; exact sender/frame/document/origin validation; generation cancellation subscriptions; and pending caller cancellation.
- `source/main/ipc/lib/trustedRendererIpcAuthority.spec.ts`
  - Cover exact identity and all fail-closed lifecycle/origin cases.
- `source/main/ipc/lib/MainIpcChannel.ts`
- `source/main/ipc/lib/MainIpcConversation.ts`
  - Supply trusted request and response authentication by default and preserve only verified sender/receiver compatibility seams.
- `source/renderer/app/ipc/lib/RendererIpcChannel.ts`
- `source/renderer/app/ipc/lib/RendererIpcConversation.ts`
  - Supply renderer-side response-event checks and retain current default globals/call shapes.
- `source/main/windows/main.ts`
  - Bind each new `WebContents` generation before setup/load, install revoke/activate lifecycle hooks, update the current main-window dependency, and invalidate only matching generations.
- `source/main/ipc/index.ts` and the small set of wrapper-registration modules that currently capture `BrowserWindow` (file dialogs, app update, Mithril bootstrap/partial sync, download manager, and hardware setup), or an equivalently narrower split discovered during implementation
  - Make wrapper-backed registration process-once and resolve the current live window at invocation/send time. Preserve task-102 ownership of the exhaustive raw-listener authority audit.
- `tests/security/trusted-ipc/` and `package.json`
  - Add an unconditional bounded, local-only pinned-Electron command/fixture proving actual `senderFrame`, origin, caller-frame reply, two-stage activation, cancellation/cleanup, recreation exactly-once behavior, and adversarial cases beyond mocks.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/system/architecture.md`
- `.agent/system/api-endpoints.md`
- `.agent/workflows/ipc.md`
  - Document the implemented wrapper contract and retain the explicit caveat that task-102 must migrate/audit all privileged listeners before guest launch.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - Reconcile task-101 implementation evidence while preserving task-102 ownership; completion metadata is added only after implementation review approval.
- `.agent/plans/dapp-browser-cip30/task-plans/task-101.md`
  - Track lifecycle, evidence, research disposition, and final outcome.

No shared public `api.ts` channel names/types, handler business logic, raw-listener migration, lockfile, dependency, backend, Nix, translation, Storybook, or Cucumber file is expected to change. The exact test-file split may be reduced if equally focused coverage remains clear.

## Smallest Implementation Approach

1. Define one internal correlated request/terminal envelope without changing application payloads.
   - Generate a UUID before each `send`/`request` and send `{ requestId, message }` internally, or an equivalent tuple with the same runtime validation.
   - Return the same request ID in exactly one terminal envelope: success, existing application rejection, or a fixed internal cancellation. Listeners use `on`, ignore wrong IDs and unauthenticated response events, and share one idempotent `settle` function that removes exactly their own response listener and lifecycle-cancellation subscription before resolving/rejecting.
   - Install the response listener and cancellation subscription before `sender.send`. If setup or send throws, invoke the same settlement/cleanup path. Preserve success versus application rejection semantics; expose cancellation only as a fixed wrapper-internal error without payload data and do not add payload logging.
   - Do not add an elapsed-time timeout. Cancellation is triggered only by concrete local lifecycle signals or an authenticated correlated cancellation from the responder.
   - Use the same protocol for both legacy channel directions. Do not expose IDs to application handlers or change common IPC API value types.

2. Preserve and authenticate request events before any business handler runs.
   - Change common handler signatures to receive `(message, event)` while remaining source-compatible with current one-argument callbacks.
   - Accept small request- and response-authentication callbacks in the common primitive. Main wrappers provide strict trusted authority; renderer wrappers provide their concrete expected IPC event-sender identity check.
   - Silently drop malformed or unauthenticated requests, whether or not attacker-controlled data resembles a correlation ID, because there is no authenticated reply target. For a well-formed correlated request from the authenticated caller, retain existing application success/error semantics; malformed/unauthenticated responses are ignored without settling.

3. Reply only to the authenticated caller and revalidate before result release.
   - Capture the authenticated request frame/context and generation, register that request in the authority's pending set, then invoke the handler only after initial validation. Revalidate the same active generation/frame/document before sending success or application failure.
   - On main-side incoming events, use `event.reply` (or the captured authenticated `WebFrameMain.send` equivalent where testability requires it), never global window lookup or `event.sender.send`.
   - On navigation, detachment, destruction, replacement, or authority invalidation, atomically mark each matching pending request cancelled before attempting a caller-targeted cancellation response. This suppresses later handler completion. If the frame is already gone, the send may fail but the pending main entry is still deterministically removed and no alternate renderer is targeted.
   - The sender's response/cancellation race uses the same idempotent settlement gate: the first authenticated matching terminal event or local lifecycle cancellation wins; every later event is ignored and listener/subscription removal occurs exactly once.
   - Renderer-side replies to main-owned requests may retain the generic sender path only where Electron provides no frame-target distinction and the response event is authenticated/correlated; document and test that concrete compatibility case.

4. Add a fail-closed two-stage trusted-renderer authority with generation-safe lifecycle.
   - Instantiate one process-owned authority before process-scoped wrapper registration. Unbound/cleared authority always rejects.
   - Stage 1, bind: immediately after constructing each `BrowserWindow`, atomically replace the generation with `{ webContents, trustedRendererUrl, activeFrame: null }`, before IPC setup and `loadURL`. Reject `about:blank` and every request while inactive; replacement first cancels/revokes the old generation and cannot be undone by an old destruction callback.
   - Revoke synchronously on `did-start-navigation` for every non-same-document main-frame navigation, including canonical reload, before the old document can continue as active. Also revoke on render-process loss, frame/WebContents destruction, and close. Same-document hash/history transitions remain active only while `isTrustedDocumentUrl` continues to pass.
   - Stage 2, activate: only after canonical main-frame commit/load (`did-frame-finish-load`, cross-checked with the preceding canonical `did-frame-navigate`, or an equally strict Electron 41 seam) resolve the reported process/routing IDs with `WebFrameMain.fromId`; require that exact live object to equal the bound `webContents.mainFrame`, and then atomically set it active for the same generation. A canonical reload therefore revokes the old frame and activates only the newly loaded canonical frame; a noncanonical, failed, stale, subframe, or old-generation event cannot activate.
   - Derive expected origin from the same canonical URL and reuse `isTrustedDocumentUrl` for exact document comparison including allowed same-document hashes. For each main request require: active matching generation; exact `event.sender === webContents`; neither sender nor frame destroyed; non-null `senderFrame`; exact identity with both active frame and current `webContents.mainFrame`; frame not detached; exact canonical frame URL; and expected serialized origin (`http://127.0.0.1:8080` in development or `null` for the packaged file document).
   - Never substitute `senderFrame.top`, process ID, URL alone, `BrowserWindow.fromWebContents`, or sender equality alone for exact frame identity.
   - Every revoke/replacement emits one generation cancellation signal and drains that generation's pending registry. Destruction/close callbacks clear only if their captured generation is still current, so late events from an old window cannot cancel or unauthorize the replacement.

5. Wire strict defaults and process-once registration without widening task-101 into task-102.
   - `MainIpcChannel` and `MainIpcConversation` use the authority for every request entering main and every response entering main for main-originated request cycles.
   - Keep receiver injection for tests and existing sender forms (`BrowserWindow` and `WebContents`) only where live call sites prove the need. Normalize the send target locally rather than migrating dozens of notification call sites.
   - Split recreatable window binding from wrapper-backed process registration. `ipcApi` (or a small registrar it delegates to) installs each wrapper handler once; each new window only updates one process-owned current-window provider used by handlers/senders that concretely need `BrowserWindow`. Do not retain the first window in those closures. Repeated calls are no-ops for listener installation but update current dependencies.
   - Prefer the existing one-time-guard/rebound-dependency pattern already used by Mithril. Registration methods return exact disposers where practical for tests/shutdown, but do not add a generic registry. Tests reset through explicit disposers or isolated modules, never `removeAllListeners` on shared channels.
   - Do not otherwise audit or migrate every direct `ipcMain.on` call in this task. Wrapper-backed handlers become protected and single-installed; task-102 must inventory the whole repository, verify each direct channel's authority, migrate it, and remove any remaining compatibility seam that lacks a concrete caller.

6. Add focused protocol, authority, and real-Electron tests.
   - Common tests use deterministic fake sender/receiver/event objects to prove listener registration precedes send, synchronous response is received, two concurrent requests settle only from their own IDs even out of order, spoofed sender or wrong ID is ignored, listeners/subscriptions are removed exactly once, send failure cleans up, cancellation rejects, and response/cancellation/send-failure races settle once.
   - Main authority tests reject unbound/cleared/bound-inactive authority and initial `about:blank`; prove initial canonical activation; reject wrong `WebContents`, null frame, subframe, wrong main-frame object, wrong canonical URL/origin, detached/destroyed frame or sender, stale generation, and post-handler navigation/destruction; prove revoke before canonical reload, new-frame reactivation, old-frame rejection, and no activation after noncanonical/failed/stale events.
   - Response tests prove main-originated requests ignore responses from wrong `WebContents`, subframes, stale/destroyed frames, wrong origins, and wrong IDs; incoming replies use the caller-specific reply seam and are never sent via another/global renderer target.
   - Conversation tests prove equivalent request and response authentication plus existing concurrent correlation and listener-before-send behavior.
   - Renderer wrapper response authentication is deliberately narrow: require the expected `ipcRenderer` event emitter identity supplied by the concrete receiver plus a valid matching terminal envelope. Do not claim `IpcRendererEvent.sender` supplies main-side `WebContents`/frame/origin authority. Tests prove matching responses/cancellations settle, wrong emitter/wrong ID does not, and no caller receives another request's terminal event.
   - Registration tests invoke the recreation path repeatedly and prove each wrapper request has exactly one listener, handler invocation, and caller-targeted response while window-dependent operations use only the latest live window. Disposing test registrations removes exactly the installed functions.
   - The unconditional `test:trusted-ipc` pinned-Electron fixture creates a trusted local main frame, an untrusted second `WebContents`, and a subframe-capable page isolated from production navigation policy. It exercises real `senderFrame`, canonical URL/origin, initial `about:blank` rejection, initial activation, revoke-before-reload, canonical new-frame activation, stale/noncanonical rejection, caller-targeted reply/cancellation, pending navigation/destruction/replacement cancellation cleanup, response/cancellation races, concurrent out-of-order replies, and exactly one execution/response after repeated window recreation. It loads no remote resource, starts no Cardano service, has a hard deadline/nonzero failure exit, and must pass for acceptance rather than being optional or replaced by mocks.

7. Synchronize governing documentation in the reviewed implementation diff.
   - Update PRD, architecture, API docs, and IPC workflow to describe the correlation envelope, listener ordering, exact sender/main-frame/document/origin authority, caller-targeted response, destroyed/stale suppression, and current task-102 gap.
   - Keep task-101 tracker status pending during implementation review. After approval, add truthful completion metadata without changing task-102's scope or production guest gates.
   - Record `no new research` in the final canonical outcome unless implementation reveals a durable Electron/wrapper constraint not already captured by research `01`; update that focused research note only if needed.

## Acceptance Criteria

- Every common wrapper receive/request path retains the Electron event and permits authentication before application handler execution.
- Main wrapper registrations fail closed while trusted authority is unbound, cleared, or bound-inactive and accept only the exact live trusted main `WebContents`, its activated current main frame, canonical trusted document URL, and expected origin.
- Initial `about:blank`, wrong `WebContents`, null frame, subframe, lookalike/stale frame, wrong URL/origin, detached frame, destroyed frame/sender, and stale/replaced trusted-window generation are rejected without invoking the privileged handler.
- Stage-1 generation/WebContents binding occurs before IPC setup/load; every non-same-document main-frame navigation revokes before replacement; only canonical committed/loaded current main frames activate. Canonical reload safely activates its new frame, noncanonical/stale events cannot activate, replacement is atomic, and late old-window events cannot affect a newer generation.
- Handler completion is revalidated; navigation, destruction, detachment, authority replacement, and send failure deterministically cancel matching pending requests and remove response/cancellation listeners. The first response/cancellation race outcome wins exactly once without an arbitrary timeout.
- Responses are sent only through the authenticated caller-specific frame/reply target, never through global window lookup or an unrelated renderer target.
- Every `IpcChannel` request has a unique request ID, registers its filtered response listener and lifecycle cancellation before send, ignores wrong-ID and unauthenticated responses, cleans up exactly its own functions, and safely supports synchronous and concurrent out-of-order terminal responses.
- `IpcConversation` retains correlation/listener-before-send and gains equivalent request authentication, narrowly scoped renderer response authentication, caller-targeting, lifecycle cancellation/revalidation, race-safe settlement, and cleanup behavior.
- Wrapper-backed process listeners are installed once across repeated main-window recreation. Window-dependent handlers/senders resolve only the current live window; repeated recreation yields exactly one privileged handler invocation and response per request, with exact disposers available for tests where needed.
- Existing application payload types, logical channel names, one-argument handlers, no-argument renderer requests, and concrete `BrowserWindow`/`WebContents` main send callers continue to compile and behave; no hypothetical compatibility path is added.
- Malformed or unauthenticated uncorrelated requests are silently dropped; authenticated well-correlated requests retain existing application success/error payload semantics, and no rejected input is echoed or logged.
- Focused wrapper, authority, and registration tests cover two-stage activation/reload, request sender authentication, response sender authentication, exact caller response delivery/cancellation, destroyed/stale lifecycle, correlation/concurrency, ordering, exact cleanup, send/response/cancellation races, malformed envelopes, and exactly-once recreation behavior.
- The unconditional bounded local Electron 41 fixture verifies real sender-frame/origin/caller-reply behavior, two-stage activation/reload, deterministic pending cancellation/cleanup, and exactly-once repeated recreation without claiming packaged sandbox or complete privileged-handler migration.
- Documentation and tracking distinguish the authenticated wrapper foundation from task-102's remaining direct-listener inventory/migration. No guest protocol, wallet wire contract, handler business logic, or production guest enablement is introduced.

## Verification

- Run focused Jest specs for `IpcChannel`, `IpcConversation`, both main wrappers/authority, both renderer wrappers, and any touched trusted-window lifecycle seam.
- Run the dedicated `test:trusted-ipc` pinned-Electron fixture unconditionally with a hard deadline and retain machine-readable results for initial inactive state, canonical activation/reload, exact trusted frame, wrong window/subframe/origin, caller-targeted reply/cancellation, concurrency/races, pending cleanup, navigation/destruction/replacement, and exactly-once recreation assertions. Failure blocks acceptance; mocks cannot substitute for it.
- Run `yarn compile` to validate the changed common/Electron event and sender/frame types.
- Run focused ESLint and Prettier checks for touched TypeScript, JavaScript, JSON, and Markdown files; distinguish unrelated repository-wide failures if present.
- Run `yarn build:main` and `yarn build:renderer` because the private protocol and wrapper defaults cross both bundles.
- Run relevant existing shell IPC, navigation-policy, Mithril IPC, chain-storage, app-update, file-dialog, download/hardware initialization, and renderer/store specs as regression coverage for concrete wrapper callers and current-window rebinding.
- Run `git diff --check`, inspect the focused diff for payload/channel-name drift, and parse the task JSON after lifecycle updates.
- Search live source again for wrapper and direct IPC registrations to confirm the task-101 diff did not silently claim or perform task-102's complete migration.
- Re-read the latest matching review-log entries during later lifecycle work; leave both logs append-only and Orchestrator-owned.
- No full Cucumber E2E, configured Cardano network, wallet data, physical hardware, packaged guest sandbox, external audit, or user manual evidence is required for this bounded wrapper foundation.

## Risks And Open Questions

- Frame lifecycle is inherently asynchronous. Stage-1 binding authorizes nothing; canonical completion activates one exact frame, revoke begins at non-same-document main-frame navigation start, and mandatory pre-response revalidation prevents stale result release. If the caller disappears, its pending entry and sender listener are cancelled/cleaned without targeting another renderer.
- File documents have serialized origin `null`; origin equality alone is insufficient. Exact bound `WebContents`, exact main-frame identity, and canonical file URL remain mandatory alongside origin.
- Same-document hash routing changes `frame.url`; reuse of `isTrustedDocumentUrl` intentionally ignores only the fragment, matching task-100. Search, path, host, port, credentials, and scheme remain exact.
- Process-scoped shell listeners predate the window. Registration may occur while authority is unbound, but invocation cannot pass until the canonical frame is active. Throwing at registration would break the concrete task-100 lifecycle without improving security.
- A persistent listener must ignore wrong-ID/spoofed responses rather than use `once`; otherwise an attacker or concurrent response can consume the legitimate listener. Tests must also prove ignored events do not leak or settle another promise.
- There is no timeout in the existing wrapper contract. Do not add one. Deterministic authority-generation cancellation plus authenticated correlated cancellation and the sender's local destruction/replacement subscription cover all locally knowable terminal paths; one idempotent settlement gate removes exact listeners/subscriptions on response, cancellation, send failure, or races. A surviving active peer that never responds remains the unchanged application contract rather than being assigned an arbitrary deadline.
- Electron completion ordering must not authorize `about:blank` or a merely started/committed-but-not-canonical document. Activation uses canonical main-frame completion IDs resolved to the current `WebFrameMain`; runtime evidence fixes the precise Electron 41 event seam if mocks disagree.
- Process-once registration is required only for wrapper-backed registrations reached through recreatable `ipcApi(window)`. Moving or authenticating every raw `ipcMain` listener remains task-102; however, any wrapper registration touched here must stop capturing a stale window and must prove exact disposal/single installation.
- Main-to-renderer pushes can be followed by renderer responses and currently accept both `BrowserWindow` and `WebContents` send targets. Preserve those proven forms locally; do not introduce generic duck-typed targets beyond the existing send contract.
- Wrapper strict defaults protect wrapper-backed main handlers, but direct raw IPC remains. Documentation and release gates must continue to state that task-102 is required before any guest launch.
- The IPC workflow currently presents generic payload logging/broadcast examples and omits sender/frame authority. Update only guidance contradicted by the implemented security contract; do not rewrite unrelated workflow sections.
- Runtime Electron tests may need a display or configured Chromium sandbox helper as the existing trusted-window fixture does. The current workspace has already produced bounded Electron 41 evidence for task-100, so this remains agent-executable; if the environment unexpectedly cannot launch Electron, implementation is blocked and cannot claim acceptance from mocks alone.

## Docs, Tracking, And Research Updates

- Update `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md` Current Baseline and Existing IPC Hardening after implementation, stating precisely what wrapper-backed IPC now enforces and what task-102 still leaves pending.
- Update `.agent/system/architecture.md` with trusted-renderer authority binding/lifecycle, correlated caller-targeted wrappers, and the remaining production guest gate.
- Update `.agent/system/api-endpoints.md` with the common private request-ID/authenticated-response transport contract; application channel names and payload tables remain unchanged.
- Update `.agent/workflows/ipc.md` examples and best practices to require authenticated main registration, event retention, caller-targeted replies, correlation, listener-before-send, and no sensitive payload logging.
- During the reviewed implementation diff, add completed `task-100` to task-101's dependency list because its canonical navigation lifecycle is now a direct prerequisite; keep task-101 pending until approval. After approval add completion date/notes and preserve task-102 acceptance/dependencies.
- Update this canonical plan with final evidence, review decision, research disposition, and outcome.
- Research disposition: no new research is expected. Amend research `01` only if implementation establishes a durable constraint beyond its existing wrapper baseline.
- No plans index, dependency manifest beyond the required `test:trusted-ipc` script, lockfile, translation, Storybook, backend, hardware, Nix, or package documentation update is expected.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-101-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-101-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope: the plan repairs common/main/renderer wrapper mechanics and trusted-window binding only. It leaves complete listener inventory/migration to task-102 and all guest/broker work downstream.
- Security boundary: two-stage bound-inactive/active generations reject initial `about:blank`, revoke before non-same-document navigation, activate only a canonical committed/loaded current frame, safely reactivate reloads, and retain exact live WebContents/frame/URL/origin checks plus generation-safe replacement and pre-release revalidation.
- Response safety: responses use the authenticated caller frame/reply seam, main responses authenticate exact authority while renderer authentication is narrowly emitter/correlation based, never select a global renderer, and send correlated cancellation or clean locally when stale/destroyed.
- Correlation/concurrency: UUID request IDs, filtered listeners plus lifecycle subscriptions before send, wrong-ID/spoof ignore, one idempotent settlement gate, exact cleanup, synchronous response, send failure, cancellation races, and out-of-order concurrency tests are included without timeout.
- Compatibility: preserved only live one-argument handlers, no-argument requests, default renderer globals, injected test receivers, and main `BrowserWindow`/`WebContents` send targets. No old uncorrelated wire compatibility or hypothetical external consumer is retained.
- Fail-closed lifecycle: process listeners may register before a window but cannot authorize while unbound/inactive; stage-1 binding occurs before setup/load; lifecycle revocation drains pending work; stale destruction cannot clear or cancel replacement authority.
- Process safety: wrapper registrations install once, current-window dependencies rebind, exact disposers support isolation, and repeated recreation proves one invocation/response rather than relying on dynamic authority to mask duplicate listeners.
- Missing tests: unit and unconditional real-Electron coverage include `about:blank`, initial activation, reload replacement, noncanonical/stale activation, wrong sender/frame/origin, null/destroyed/detached frames, mid-flight navigation/destruction/replacement, caller targeting/cancellation, response spoofing, malformed silent drops, exact cleanup/races, ordering/concurrency, and exactly-once concrete regression callers.
- Docs/tracking: PRD, architecture, API docs, IPC workflow, tracker, canonical outcome, and research disposition are assigned without prematurely claiming task-102 or release-gate completion.
- Wire drift: logical channel names and application payload types remain stable; only a private symmetric request-ID envelope changes. Both bundles and concrete callers are regression-tested.
- Smallest design: one two-stage authority, one private terminal envelope, repaired existing primitives, and a mutable current-window provider are sufficient. No invoke migration, generic registry/policy framework, guest abstraction, timeout framework, or task-102 handler sweep is proposed.
- Interaction mode: `autonomous` is truthful because no configured network, package, physical device, partner, audit, or user-only checkpoint belongs to task-101.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Planning and implementation review are approved. Wrapper correlation,
  trusted-frame authority, lifecycle cancellation, process-once hardware channel
  construction/current-window rebinding, and focused unit/Electron evidence are
  implemented in the live diff.
- No user interaction is required. Task-102 still owns raw-listener migration,
  and production guest launch remains disabled.
- Electron 41 runtime evidence confirms packaged `file:` frame origins serialize
  as `file://`; this durable implementation constraint is recorded in the
  implementation review log and research `01`.
- Final verification: 9 focused Jest suites with 22 tests, `yarn compile`,
  `yarn build:main`, focused ESLint/Prettier, `git diff --check`, task JSON parse,
  and the bounded Electron 41 fixture passed. The fixture used the verified
  root-owned Chromium helper and no sandbox-disabling flag.
- Final review: `Decision: approved` in
  `.agent/plans/dapp-browser-cip30/task-plans/task-101-impl-review.md`.
