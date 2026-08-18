# Task task-102: Audit and migrate every privileged IPC handler

## Task

- Task ID: `task-102`
- Title: `Audit and migrate every privileged IPC handler`
- Phase: `phase-1` (`Electron And IPC Security Foundation`)
- Priority: `critical`

## Why This Task Now

- `task-101` is complete and supplies strict wrapper defaults for exact active trusted `WebContents`, main-frame, canonical-document, and origin authentication, correlated caller-frame responses, lifecycle cancellation, and current-window rebinding.
- The accepted hostile-renderer boundary still blocks production guest creation until every main-process IPC ingress is inventoried and either uses that authenticated wrapper path or an explicitly equivalent policy.
- Live source still contains direct `ipcMain` registrations for `resize-window` and `close-window`. The resize listener is also registered twice through `source/main/windows/main.ts` and `source/main/ipc/resize-window.ts`, and each main-window recreation can add more raw listeners that capture stale windows.
- All other located production privileged handlers use `MainIpcChannel` or `MainIpcConversation`, but that inherited protection is not yet backed by an exhaustive, machine-checkable inventory or a hostile-known-channel probe. Task-102 closes that evidence gap before task-105 can create a guest and task-107 can build the packaged hostile suite.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs: none. The PRD fixes the trust decision, and the live registration/call sites determine the migration.
- Required manual test steps: none. Static inventory checks, focused Jest, TypeScript/build checks, and the existing bounded Electron 41 fixture are agent-executable in this workspace.
- Required evidence from the user: none. Packaged guest sandbox proof, configured-network wallet testing, physical hardware certification, and external audit remain downstream gates.
- Human checkpoint: none for task-102. Implementation can proceed without pausing for user interaction.

## Scope

- Inventory every production main-process IPC receive surface across all of `source/main`, not only `source/main/ipc`: global, WebContents-scoped, frame-scoped, service-worker-scoped, WebContents IPC events, transferred `MessagePortMain` receive paths, wrapper construction/handler registration, and temporary main-side response listeners, together with their shared/renderer contracts.
- Derive the Electron ingress/caller set from the installed Electron 41.3.0 and Node EventEmitter declarations. Audit `ipcMain`, `WebContents.ipc`, `WebFrameMain.ipc`, and `ServiceWorkerMain.ipc` registrations through `on`, `once`, `addListener`, inherited `prependListener`/`prependOnceListener`, `handle`, and `handleOnce`; `WebContents` `ipc-message`/`ipc-message-sync` listeners through every EventEmitter registration equivalent; `MessagePortMain` `message` listeners through every equivalent; and renderer-to-main `ipcRenderer.send`, `sendSync`, `invoke`, and `postMessage` callers/port transfers.
- Record a machine-checkable manifest for each logical privileged channel with its transport kind, direction, constructor owner, handler-registration owner, main-originated caller owner when present, privileged capability category, response-promise settlement policy, and exact trusted-renderer authority policy.
- Add an automated source audit that fails when a production raw Electron IPC listener is introduced, when a wrapper-backed main channel is missing or duplicated in the manifest, when a manifest row no longer resolves to a live shared contract/registration, or when a direct renderer caller bypasses the approved adapter.
- Migrate the raw close/resize controls atomically on both sides to the task-101 authenticated channel wrapper, preserving their current logical base names and application behavior while changing the private transport to the correlated wrapper envelope.
- Make close/resize registration recreation-safe and current-window-safe. A close response must settle to the authenticated caller before window teardown is scheduled so authority invalidation does not create an unhandled cancellation race.
- Audit every production main-originated `MainIpcChannel.send/request` and `MainIpcConversation.send/request` call, including cardano state/TLS, menus, download progress, app update, block replay/disk status, Mithril, and hardware callbacks. Give every returned promise explicit awaited or intentional-fire-and-forget ownership, route long-lived producers through the rebound current trusted-window sender, and consume lifecycle cancellation without an unhandled rejection or sensitive error logging.
- Extend the bounded Electron security fixture to send well-formed traffic for every inventoried privileged channel from an untrusted `WebContents`, a subframe, a wrong-origin/stale trusted document, and after lifecycle invalidation, and prove no sentinel privileged handler executes and no spoofed response settles main-owned work.
- Retain focused trusted-frame success coverage so a blanket deny or stale manifest cannot satisfy the hostile tests.
- Classify raw IPC used only inside security fixtures as test-only harness transport, keep it out of production manifests, and ensure static production checks cannot accidentally exempt equivalent source paths.
- Synchronize IPC architecture, workflow, PRD baseline, research evidence, and task tracking after reviewed implementation.

## Non-Goals

- Do not create or enable the dApp guest, guest preload, guest session, scoped gateway, CIP-30 broker, route lease, grants, or wallet APIs. Tasks 104-106 own those surfaces after this gate.
- Do not expose the legacy privileged preload or any existing channel to a guest. `source/main/preload.ts` remains trusted-main-only; task-104 creates a separate least-authority preload.
- Do not migrate the trusted main renderer away from `nodeIntegration: true`, `contextIsolation: false`, or raw `global.ipcRenderer` exposure. This task removes reachable unauthenticated main handlers rather than redesigning the legacy renderer.
- Do not replace the task-101 wrappers with `ipcMain.handle`, a generic policy engine, a new IPC framework, runtime schema infrastructure, or the future guest protocol.
- Do not rewrite filesystem, TLS, store, logging, update, download, Mithril, cardano, shell, or hardware business logic already protected by strict wrapper defaults unless the exhaustive audit finds a concrete authority bypass.
- Do not change application payload types, error semantics, or established logical channel values except to add typed close/resize contracts for their existing string values and move those two private transports onto wrapper envelopes.
- Do not treat main-to-renderer menu/UI notifications, Electron window lifecycle events, or test-fixture-only probes as unauthenticated privileged renderer-to-main handlers; classify them explicitly rather than broadening the migration.
- Do not claim packaged sandbox, hostile remote-content, network egress, wallet, or physical hardware release evidence.
- Do not write either review log.

## Dependencies And Ownership

- `task-001`: completed; establishes that existing privileged IPC accepts only the exact trusted main `WebContents` and main frame, with no side effect on rejection.
- `task-100`: completed transitively through task-101; supplies the canonical trusted-document navigation lifecycle used by IPC authority.
- `task-101`: completed direct dependency; supplies authenticated/correlated `MainIpcChannel` and `MainIpcConversation`, two-stage trusted authority, lifecycle cancellation, caller-frame replies, and current-window sender seams. Task-102 reuses and audits these controls rather than redesigning them.
- `task-105`: remains blocked on task-102, task-103, and task-104. No guest manager may be enabled merely because this audit passes.
- `task-107`: consumes task-102's inventory and hostile-known-channel evidence in the later packaged hostile-renderer suite.
- No sibling `cardano-wallet`, standards, exact-CBOR, network-egress, Nix/package, translation, Storybook, or physical hardware dependency is involved.

## Research Consulted

- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
  - Establishes that the legacy renderer/preload is privileged, existing IPC cannot be a guest protocol, task-101 hardened wrappers, and task-102 still owns exhaustive raw-listener migration.
- `.agent/plans/dapp-browser-cip30/research/06-linux-system-package-decision.md`, relevant non-negotiables
  - Confirms production guest launch remains disabled until privileged IPC authentication and the independent packaged `.deb`/`.rpm` sandbox gates are complete.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`, especially ADR-001, Protected Assets, Boundary And Authority Invariants, Threat Traceability And Release Gates, Trust Boundaries, Existing IPC Hardening, and Phase 1
  - Fixes exact trusted sender/main-frame authentication, rejection before privileged side effects, a separate guest gateway, and completion of task-102 before guest creation.
- No backend, wire-standard, exact-CBOR, hardware-capability, or connection-bound egress research changes this bounded trusted-IPC audit.

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`, task-102 and dependencies task-101/task-100/task-001, plus downstream task-105/task-107 gates
- `.agent/workflows/electron.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/dapp-browser-cip30/prompt.md` for canonical-plan fields, interaction classification, smallest-truthful-solution policy, research disposition, and review-log ownership
- Task-101's canonical plan and both reviews for the inherited wrapper authority, private envelope, lifecycle, response targeting, process registration, and verified Electron 41 file-origin behavior
- The `understand` repository-understanding skill was loaded first. Running its full graph generator would violate this planning turn's single-file write restriction and create disproportionate unrelated artifacts, so all material findings were verified directly against live source, tests, contracts, installed Electron 41.3.0/Node EventEmitter declarations, and focused git history.
- No e2e-Cucumber, Storybook, i18n, theme, hardware-operator, Cardano CLI, CBOR, or protocol skill applies.

## Verified Live Findings

- Electron 41.3.0 live declarations expose `IpcMain`/scoped `IpcMain` registration through `on`, `once`, `addListener`, inherited EventEmitter `prependListener`/`prependOnceListener`, `handle`, and `handleOnce`; renderer callers through `send`, `sendSync`, `invoke`, and `postMessage`; WebContents `ipc-message` and `ipc-message-sync`; and transferred `MessagePortMain` `message` events. `WebContents.ipc`, `WebFrameMain.ipc`, and experimental `ServiceWorkerMain.ipc` are `IpcMain`-like receive surfaces. `sendToHost` targets a webview host renderer rather than main and is explicitly non-applicable to this renderer-to-main audit.
- Symbol-aware inspection of current production code finds only raw `ipcMain.on` for `resize-window` and `close-window` in `source/main/windows/main.ts` plus a second `resize-window` registration in `source/main/ipc/resize-window.ts`. It finds no production scoped-IpcMain, WebContents IPC-event, service-worker IPC, or MessagePortMain receive path; those zero-result API classes remain guarded by the audit rather than omitted.
- The raw controls check only `event.sender === window.webContents`; they do not enforce main-frame identity, active canonical document/origin, or task-101 lifecycle authority. Because `ipcApi(window)` and the inline listeners run on recreation, old closures/listeners can accumulate.
- `WindowStore` is the only production renderer caller that directly invokes `ipcRenderer.send`; it sends the two raw control names. All located renderer IPC modules otherwise construct `RendererIpcChannel` or `RendererIpcConversation`.
- Production main handlers are distributed across `source/main/index.ts`, `source/main/cardano/setup.ts`, `source/main/utils/handleDiskSpace.ts`, and `source/main/ipc/**`. Limiting the audit to the IPC directory would miss real registration owners.
- Located wrapper-backed main channel/conversation instances inherit task-101's strict `authorizeTrustedRenderer`, `isTrustedRendererEvent`, and lifecycle invalidation defaults. Main responses use the correlated private envelope and exact caller reply seam.
- Live source contains 26 main-originated wrapper call expressions: 2 cardano state/TLS, 8 menu/UI, 6 download-event, 1 block-replay, 2 disk-space, 1 app-update, 2 Mithril status, and 4 hardware notification sends. Four disk/Mithril sends are awaited; the other 22 discard the promise and can reject unhandled when task-101 cancels on navigation, destruction, or replacement. No production main-originated `MainIpcConversation` call is currently present, but the manifest/audit covers that API symmetrically.
- Hardware already uses a rebound `createCurrentWindowSender`, and Mithril replaces its status-sender closure on each `ipcApi(window)` call. Cardano, download, block-replay, disk-space, menu, and app-update producers still have call paths that capture a construction-time window or discard the response promise; the smallest consistent fix is to reuse one process-owned rebound current-window sender and make settlement explicit at each audited call site, not redesign wrapper transport.
- `source/common/ipc/api.ts` defines the shared logical contracts for current wrapper channels but has no typed constants/contracts for the raw close/resize names.
- `source/main/preload.ts` intentionally exposes raw `ipcRenderer` plus HTTP(S), environment, paths/config, OS, and logging globals to the trusted legacy renderer. Security comes from denying every unauthenticated main ingress and never reusing this preload for a guest, not from claiming this preload is least-authority.
- The existing `tests/security/trusted-ipc` fixture uses raw `ipcMain`, `WebContents.ipc`, and `ipcRenderer` only as local test-control/probe transport. It proves actual wrappers, trusted sender frame, HTTP and packaged `file://` origins, initial inactive state, correlation, and recreation, but does not enumerate production channel names or exercise hostile `WebContents`/subframe attempts across the complete manifest.
- Test-only raw IPC also appears in the trusted-window fixture and Mithril E2E mocks; those are not production authority surfaces and need explicit scanner boundaries rather than deletion.
- Task-101 made wrapper re-registration replace the prior exact listener and added a rebound hardware current-window sender. The close/resize migration should reuse those seams; no second registry or generic authorization layer is needed.
- Current documentation conflicts with live/security truth: the Electron workflow shows `contextIsolation: true`, `nodeIntegration: false`, and a `contextBridge` preload; architecture says the preload limits exposed APIs; and generic broadcast guidance sends to all windows. The live trusted main uses the opposite legacy settings/raw globals, and privileged responses must never select all/global renderers.

## Expected Files

- `source/common/ipc/api.ts`
  - Add typed close/resize request and response contracts using the existing logical string values.
- `source/main/ipc/windowControlChannels.ts` (or one equivalently small existing module)
  - Register authenticated close and resize handlers against the current main window; settle close before scheduled teardown.
- `source/main/ipc/resize-window.ts`
  - Delete the duplicate raw listener after the consolidated authenticated migration.
- `source/main/ipc/index.ts`
  - Register/rebind the authenticated window-control channels with the same recreatable window path as other wrapper handlers.
- `source/main/windows/main.ts`
  - Remove direct `ipcMain` import/listeners and leave window control registration to the authenticated IPC module.
- `source/renderer/app/ipc/windowControlChannels.ts`
  - Add renderer adapters for the two shared contracts.
- `source/renderer/app/stores/WindowStore.ts`
  - Replace direct raw `ipcRenderer.send` calls with the adapters while preserving action behavior and handling close lifecycle settlement safely.
- `source/main/ipc/privilegedIpcManifest.ts` (name may vary narrowly)
  - Hold the typed machine-readable inventory of live logical channels, transport/direction, constructor/registration/caller owners, capability category, settlement policy, and authority policy without introducing runtime dispatch.
- `source/main/ipc/privilegedIpcManifest.spec.ts`
  - Parse/audit all production `source/main` and relevant renderer source, enforce manifest-to-registration/contract parity, reject raw listener/caller bypasses, and preserve explicit test-only exclusions.
- Focused window-control main/renderer tests, colocated with the new modules or existing store tests
  - Cover trusted execution, wrong/stale window suppression through inherited authority, current-window rebinding, exactly-once registration, payload forwarding, response-before-close scheduling, synchronous store actions with explicitly consumed promises, and no unhandled cancellation.
- `source/main/ipc/lib/currentWindowSender.ts` and focused spec, plus the live main-originated caller files listed in Verified Live Findings
  - Reuse the existing small sender seam as the single process-owned current trusted-window send target; classify and explicitly settle every wrapper response promise. No business handler rewrite is expected.
- `tests/security/trusted-ipc/main.js`, `tests/security/trusted-ipc/preload.js`, and its local fixture document only as needed
  - Add manifest-driven hostile known-channel, subframe/wrong-origin/stale lifecycle, spoofed-response, zero-side-effect, and trusted-positive evidence using actual task-101 wrappers/authority.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/system/architecture.md`
- `.agent/system/api-endpoints.md`
- `.agent/workflows/electron.md`
- `.agent/workflows/ipc.md`
  - Document the completed exhaustive authority rule/inventory and correct stale privileged-renderer, preload, broadcast, and raw-listener guidance.
- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
  - Record the durable final inventory/migration evidence and remaining separate-guest boundary.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - Add truthful completion evidence only after implementation review approval; preserve all downstream guest and release gates.
- `.agent/plans/dapp-browser-cip30/task-plans/task-102.md`
  - Track reviewed lifecycle, verification evidence, research disposition, and final outcome.

`source/main/index.ts`, all non-test production files under `source/main/**` and `source/renderer/**`, shared wrapper primitives, preload, and shared contracts are audit inputs. Main-originated caller files change only as needed for promise ownership/current-window targeting; wrapper primitives change only if a small typed cancellation discriminator is needed to distinguish expected lifecycle cancellation without string matching. `package.json` need not change because `test:trusted-ipc` already provides the bounded Electron command. No lockfile, backend, Nix, translation, Storybook, or Cucumber file is expected to change.

## Smallest Implementation Approach

1. Freeze the live inventory before migration.
   - Define production inputs exactly as non-test `.ts`, `.tsx`, and `.js` compilation units under `source/main/**` and `source/renderer/**`; exclude only filename-classified `*.spec.*`/`*.test.*`, generated build output, and fixture roots outside `source`. There is no ordinary source-directory or per-file bypass allowlist.
   - Load the repository TypeScript program and resolve aliases, destructuring, property extraction, re-exports, and bound method references back to their Electron 41 symbols/types. Do not match identifier spelling. Derive and freeze the audited declaration set: `IpcMain` and scoped `IpcMain` `on`/`once`/`addListener`/inherited `prependListener`/`prependOnceListener`/`handle`/`handleOnce`; `IpcMainServiceWorker` equivalents; `WebContents` `ipc-message`/`ipc-message-sync` registration through all five EventEmitter methods; `MessagePortMain` `message` registration through all five; and renderer `IpcRenderer.send`/`sendSync`/`invoke`/`postMessage` including transferred ports. Explicitly classify `sendToHost` as renderer-to-webview-host, not main ingress.
   - Walk those production inputs to identify every direct/scoped Electron receiver, wrapper construction, persistent `onReceive`/`onRequest` registration, main-originated `send`/`request`, response listener, MessageChannel/port creation or acquisition, and renderer caller. This must detect `const { on } = ipcMain`, `const send = ipcRenderer.send.bind(ipcRenderer)`, aliases, and equivalent imported/re-exported symbols.
   - Inspect shared channel constants and renderer adapters/callers to classify each logical channel as renderer-to-main, main-to-renderer with authenticated response, or bidirectional; separately record constructor owner, handler-registration owner, and each main-originated caller owner because `createChannels(MainIpcChannel)` and hardware/bidirectional channels do not collapse to one site. Categorize capability as store, TLS/cardano lifecycle, shell, filesystem/dialog/export, logs, update/download, window/system, Mithril/storage, hardware, or UI notification.
   - Keep one typed static manifest used only for audit/evidence. Do not route production messages through it or add a generic policy registry.

2. Make drift fail in tests.
   - Add one focused audit spec that derives production listener/constructor/registration/caller sites and compares them to the manifest. Every main-side receive surface must be an authenticated task-101 wrapper row or fail; every main-originated response promise must have manifest ownership; production raw-receiver and direct-renderer-caller bypass allowlists are both empty.
   - Fail on any symbol-resolved Electron API above, including scoped/frame/service-worker IPC, WebContents IPC events, `postMessage`/ports, MessagePortMain message listeners, aliases, destructuring, bound references, and EventEmitter-equivalent registrations. Add scanner self-tests with small in-memory TypeScript fixtures for each direct and aliased spelling so a future syntactic rewrite cannot evade the check.
   - Require each manifest row to resolve to a real shared contract, constructor owner, handler-registration owner where applicable, renderer adapter, and every main caller; reject duplicate logical ownership and classify authenticated temporary response listeners separately from persistent renderer-to-main handlers.
   - The approved renderer adapters and common wrapper internals are recognized by resolved wrapper symbols/manifest ownership, not pathname exemptions. Test/security raw transport is separately classified because it lies outside the exact production roots; production files cannot be allowlisted.
   - Avoid brittle line-number snapshots and generated source copies. Assert symbols, semantic call/constructor shapes, stable owner paths, logical names, and declaration-set completeness against the installed Electron/Node types.

3. Give every main-originated wrapper promise lifecycle-safe ownership.
   - Enumerate all live `MainIpcChannel`/`MainIpcConversation` `send` and `request` call sites, including calls reached through typed parameters or `createChannels`; record whether the response is semantically awaited or the call is an intentional notification whose acknowledgment is fire-and-forget. An unused bare promise is forbidden.
   - Reuse `createCurrentWindowSender` as one process-owned sender rebound by every `ipcApi(window)` call. Long-lived cardano, menu, download, app-update, block-replay/disk, Mithril, and hardware producers resolve that sender at send time; a callback retained from an old generation must target only the current live trusted window or settle locally if none exists, never the old or an arbitrary window.
   - At fire-and-forget sites use an explicit `void` plus terminal rejection handler. Treat fixed task-101 lifecycle cancellation/current-window-unavailable outcomes as expected completion; handle any other rejection without logging payloads, renderer-supplied errors, URLs, or wallet data. At awaited sites catch the same lifecycle outcomes at the owning operation boundary while preserving existing non-lifecycle error/ordering semantics. Add a small typed cancellation predicate only if the existing fixed error cannot be handled safely without string matching; do not add a promise registry or timeout.
   - Test response, navigation, destruction, replacement, and send-failure races for both ownership modes. Assert exactly-once settlement, zero `unhandledRejection`, exact response-listener/invalidation-subscription cleanup, no stale delivery, and no alternate/global-window targeting.

4. Migrate only the two live raw controls.
   - Add `CLOSE_WINDOW_CHANNEL = 'close-window'` and `RESIZE_WINDOW_CHANNEL = 'resize-window'` plus exact request/void response types to the shared API.
   - Create one main and one renderer channel module using existing wrappers. Register close/resize through `ipcApi(window)` so re-registration replaces the old wrapper listener and binds the latest window.
   - Remove both inline raw listeners and delete the duplicate resize module. The wrapper's private suffix/envelope changes atomically on main and renderer while the logical base values and application payloads remain stable; no compatibility listener remains because there is no external consumer and leaving it would preserve the vulnerability.
   - Resize applies only to the current live bound window. Close returns successfully first and schedules `window.close()` on the next turn; tests prove exactly one close and no stale-window action or dangling rejection.

5. Add hostile known-channel evidence without executing real privileged business logic.
   - Extend the local Electron fixture to consume the manifest and register sentinel handlers through the actual main wrapper kind for all privileged ingress names in an isolated process.
   - From a separate untrusted `WebContents`, a subframe, a wrong-origin/stale generation, and after navigation/destruction/replacement, send structurally valid request/response envelopes on every derived wire endpoint. Identify each logical channel and relevant private endpoint in machine-readable results while sharing transport-generic assertions. Assert zero sentinel invocations, no main-owned request settles from a spoof, no leaked response listener, no unhandled rejection, and no response is redirected to a trusted/global window.
   - Exercise representative trusted main-frame requests, including window-control unit seams, to prove the fixture is not passing through absent listeners or universal denial. Keep the fixture local-only, bounded by a hard deadline, free of Cardano services/remote resources/sensitive logging, and explicit that it is not packaged sandbox proof.

6. Re-run the complete audit after edits and synchronize truth.
   - Confirm all Electron 41 declaration-derived ingress/caller classes are audited, there are no production raw receivers or direct renderer bypasses, every live wrapper construction/registration/caller has exactly one manifest classification, every outgoing promise has explicit ownership, and the inherited task-101 authority remains unchanged.
   - Update PRD/system/workflow docs to distinguish the legacy privileged preload from the future guest preload, prohibit all-window privileged responses and import-time/raw registration, and point to the checked inventory.
   - Record the final audit as durable research, then update tracker completion only after approved implementation review. Do not weaken task-103/104/105/107 or production launch gates.

## Acceptance Criteria

- A machine-checkable manifest accounts for every production main-process IPC receive/response surface and main-originated wrapper call across the exact production roots, with logical channel, transport/direction, constructor owner, registration owner, caller owner, capability category, response settlement, and `exact-active-trusted-main-frame` policy.
- The audit resolves Electron symbols through aliases, destructuring, binding, and re-exports and fails for every Electron 41 declaration-derived raw receiver: global/WebContents/frame/service-worker `IpcMain` `on`/`once`/`addListener`/`prependListener`/`prependOnceListener`/`handle`/`handleOnce`; WebContents `ipc-message`/`ipc-message-sync` equivalents; MessagePortMain `message` equivalents; or an unaccounted port-transfer path. Production raw-receiver allowlist is empty.
- The audit fails for an unmanifested/duplicate wrapper construction, handler registration, main caller, missing shared contract/owner, or renderer `ipcRenderer.send/sendSync/invoke/postMessage` bypass, including aliased forms. The production direct-caller allowlist is empty; `sendToHost` is declaration-backed and documented as non-main transport, not silently ignored.
- `resize-window` and `close-window` use shared typed constants and the authenticated task-101 wrappers end-to-end. Their old raw listeners are removed, the duplicate resize module is deleted, and no backward-compatibility listener remains.
- Wrong `WebContents`, subframe/null/detached/stale frame, wrong document/origin, inactive/replaced generation, and post-navigation/destruction traffic cannot resize/close a window or invoke any filesystem, electron-store, TLS/cardano, shell, logging, update/download, Mithril/storage, or hardware handler.
- Window recreation leaves exactly one close and one resize handler targeting only the current live window. Store actions remain synchronous at their public seam while explicitly consuming adapter promises. A close response settles before scheduled teardown, and resize/close paths produce no unhandled rejection, stale-window action, duplicate effect, or global response.
- Every production main-originated wrapper response promise is manifest-owned and either awaited with lifecycle cancellation handled at its operation boundary or intentionally fire-and-forget with a terminal rejection handler. Navigation, destruction, replacement, unavailable-current-window, send failure, and response/cancellation races leave zero unhandled rejections, leaked response listeners/subscriptions, stale deliveries, or alternate-window responses.
- Long-lived cardano, menu, download, app-update, block-replay/disk, Mithril, and hardware callbacks use the process-owned rebound current-window sender. A callback retained across recreation sends only to the latest live trusted window; no current window settles locally without transmission.
- Every pre-existing privileged wrapper handler remains behind task-101 strict defaults; the implementation introduces no per-handler opt-out, sender-only fallback, origin-only fallback, global renderer target, or unauthenticated compatibility route.
- The bounded Electron 41 fixture probes every manifest channel's relevant wire endpoint with well-formed hostile traffic from untrusted window/subframe/wrong-origin/stale lifecycle contexts and proves zero privileged sentinel effects and spoofed-response rejection. Matching trusted-frame positive probes pass.
- Test-only raw fixture transport is explicitly isolated from production inventory and cannot authorize a production exclusion.
- Shared logical channel values and existing application payload/error contracts remain stable except for the deliberate private close/resize wrapper-envelope migration, which changes main and renderer atomically and is regression-tested.
- Documentation accurately states that the current main preload is broad and trusted-only, all production privileged IPC is exact-frame authenticated, a guest receives none of it, and guest launch remains disabled pending all other PRD gates.
- No guest/broker, backend, package/sandbox, network policy, wallet protocol, physical hardware, or production enablement work is introduced.

## Verification

- Run the focused privileged manifest/source-audit Jest spec and prove it observes the exact non-test production roots, shared contracts, renderer caller surfaces, and installed Electron/Node declarations rather than a handpicked IPC directory. Run scanner fixture cases for each API family, EventEmitter equivalent, alias, destructure, bound reference, re-export, scoped IPC object, WebContents IPC event, and MessagePort transfer/listener.
- Run focused Jest for new window-control main/renderer modules and any touched `WindowStore` seam, including synchronous action behavior, current-window replacement, duplicate registration, exact payload, response-before-close scheduling, stale authority, send/cancellation races, cleanup, and zero unhandled rejection.
- Run focused main-originated-send ownership tests over representative awaited and fire-and-forget sites plus every long-lived producer category. Retain callbacks across navigation, destruction, and replacement; assert latest-window-only transmission, local no-window settlement, preserved non-lifecycle semantics, exact listener/subscription cleanup, and a process-level `unhandledRejection` sentinel remaining empty.
- Re-run task-101 wrapper/authority/current-window suites because task-102 relies on their exact authentication and lifecycle guarantees.
- Run the existing `test:trusted-ipc` Electron 41 fixture with the verified Chromium helper and hard deadline. Require machine-readable results for manifest completeness, every-known-channel hostile rejection, wrong window/subframe/origin/stale lifecycle, spoofed response, caller targeting, trusted positive control, and zero privileged effects. No sandbox-disabling flag is permitted.
- Run relevant existing shell, electron-store, cardano/TLS, logs, file dialogs/exports, update/download, Mithril/chain-storage, window, and hardware initialization/rebinding specs as category regression coverage without physical devices or configured networks.
- Run `yarn compile`, `yarn build:main`, and `yarn build:renderer` because shared contracts and both adapters change.
- Run focused ESLint and Prettier checks over touched source, tests, JSON, and Markdown; run `git diff --check` and parse the task JSON after lifecycle updates.
- Repeat independent symbol-aware/source searches for every declaration-derived Electron receiver/caller API, wrapper constructors/registrations/main callers, preload exposure, MessagePort paths, and direct renderer sends; compare to the manifest and inspect the diff for channel/payload drift, bare promises, stale-window capture, or accidental exemptions.
- Re-read the latest matching review-log entries before lifecycle/final-evidence updates; both logs remain append-only and Orchestrator-owned.
- No Cucumber E2E, Cardano node/wallet, configured network, wallet data, physical hardware, packaged guest sandbox, external audit, or user manual evidence is required for this task.

## Risks And Open Questions

- Static source inventories can become regex theater. Use TypeScript syntax traversal or an equivalently semantic repository-local check for call/new expressions, and pair it with live Electron hostile probes. Do not rely on a documentation table alone.
- Method-name AST matching is also bypassable. The checker must use the TypeScript checker to follow import aliases, destructuring, binding, re-exports, receiver types, and inherited EventEmitter members back to installed Electron/Node declarations; scanner fixtures lock every covered API family. If a construct cannot be resolved confidently, fail the audit rather than place it on an allowlist.
- Electron exposes more renderer-to-main paths than global `ipcMain`: WebContents- and WebFrameMain-scoped IPC, service-worker IPC, WebContents IPC events, and transferred MessagePortMain messages. None is live in current production, but an empty finding is an asserted invariant, not grounds to remove that API class from the checker.
- Wrapper construction is not identical to privileged handler registration: some channels are main-originated notifications with temporary authenticated response listeners. The manifest and audit must classify direction instead of falsely requiring every constructor to have a persistent renderer-to-main handler.
- Main-originated notifications are request/response at the wrapper transport level even when application code ignores the acknowledgment. Explicit fire-and-forget ownership must consume cancellation; awaited ownership must handle expected lifecycle cancellation without converting unrelated failures. Do not solve this with `process.on('unhandledRejection')`, blanket global suppression, timeout, or a second pending-request registry.
- Rebinding the target is distinct from settling its promise. Long-lived producers must use the current-window sender at execution time, and every resulting promise still needs local ownership. A destroyed old window must never cause fallback to `BrowserWindow.getAllWindows()` or another renderer.
- Importing the complete production main graph into a fixture could start services or perform filesystem/update/hardware work. Use manifest names with actual wrappers and sentinel handlers in an isolated fixture, while static parity proves those names cover production owners; never invoke real business handlers with hostile test payloads.
- `close-window` destroys the authority it uses. Scheduling close only after the correlated success reply is emitted avoids deterministic cancellation winning first; focused tests must fix this ordering rather than ignoring a rejected promise.
- The trusted preload remains broad. This is acceptable only because task-100 locks its document and this task ensures no other renderer/frame can invoke main privileges. Documentation must not describe it as minimal or reusable.
- The task-101 wrappers retain fallback `event.sender.send` behavior in their common primitives for non-main test/renderer compatibility, while main authenticated events use `event.reply`. Task-102 should verify main paths, not widen scope into another wrapper redesign without a demonstrated bypass.
- Test fixtures legitimately use raw IPC control channels. They are outside the exact `source/main`/`source/renderer` production roots; within those roots only filename-classified specs/tests are excluded. No ordinary production path, source subdirectory, API spelling, or symbol can be allowlisted.
- Electron workflow text and architecture security wording are stale relative to live settings. Correct the contradictory sections in this task, but do not opportunistically rewrite unrelated Electron development guidance.
- No unresolved product/security decision or human checkpoint exists. If implementation discovers an unwrapped handler requiring a weaker policy or wire compatibility for an external consumer, record the conflict and return it for plan review rather than silently exempting it.

## Conflicts Recorded

- `.agent/workflows/electron.md` shows a context-isolated, Node-disabled main window and `contextBridge` preload, but live `source/main/windows/main.ts` and `source/main/preload.ts` use the privileged legacy model. Live code plus the accepted PRD govern; implementation must correct the workflow rather than plan against its stale example.
- `.agent/system/architecture.md` says the preload limits exposed APIs, while live preload exposure is broad. The fixed trust decision is exact trusted document/frame plus authenticated main ingress and a separate future guest preload, not a claim that the legacy preload is least-authority.
- `.agent/workflows/ipc.md` still includes generic all-window broadcast guidance. It may describe nonprivileged notifications only; privileged request responses and authority-bearing notifications must target the authenticated/current trusted window.
- Task-102's tracker target paths broadly include `source/main/index.ts` and `source/main/cardano/`. Live inspection finds wrapper registrations there but no direct Electron IPC listener. They remain mandatory audit inputs, not forced edit targets; edits occur only if the machine audit finds concrete drift.
- Task-101's final review accepts focused Electron evidence plus Jest lifecycle coverage, while task-102 additionally requires every-known-channel hostile probing. Task-102 extends that evidence without reopening the approved wrapper design.

## Docs, Tracking, And Research Updates

- Update the PRD Current Baseline/Existing IPC Hardening language from “direct listeners pending task-102” to the exact reviewed manifest/migration result while retaining every independent production guest release gate.
- Update `.agent/system/architecture.md` and `.agent/system/api-endpoints.md` with the exhaustive privileged-channel authority rule, inventory location, close/resize migration, direct-listener prohibition, and truthful legacy preload boundary.
- Update `.agent/workflows/electron.md` and `.agent/workflows/ipc.md` so examples require explicit authenticated initialization, exact trusted-frame policy, caller/current-window targeting, no all-window privileged response, no import-time/raw privileged listener, and no sensitive payload logging.
- Document the declaration-derived prohibited raw API set, symbol-aware alias/destructure coverage, exact production roots, explicit main-originated promise ownership, and current-window rebinding rule so future contributors do not treat `postMessage`, scoped IPC, EventEmitter aliases, or discarded wrapper promises as exceptions.
- Update research `01` with the durable final inventory, hostile-known-channel evidence, test-only raw-listener classification, and remaining separate guest/preload gate. If implementation finds nothing beyond those planned findings, record `no additional research beyond the task-102 audit evidence` in the final canonical outcome.
- Keep task-102 pending during implementation review. After approval, add truthful completion date/notes to the task JSON without changing task-105/task-107 dependencies or claiming sandbox/guest release readiness.
- Update this canonical plan with approved planning/build lifecycle, exact verification evidence, review result, research disposition, and final outcome.
- No plans index, public API endpoint beyond the two existing internal control names, lockfile, backend pin, Nix/package, translation, Storybook, or Cucumber tracking update is expected.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-102-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-102-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope creep: the plan adds one evidence-only manifest/audit, migrates only the two proven raw controls, reuses task-101 wrappers/fixture/current-window seams, and leaves every guest, protocol, backend, package, and hardware-certification concern downstream.
- Stale workflow guidance: the plan records and assigns corrections for the context-isolation/Node/preload and global-broadcast contradictions instead of treating them as live architecture.
- Missing manifests/tests/docs: manifest-to-live-registration parity, production raw/direct-call scans, category regressions, hostile every-known-channel Electron evidence, positive controls, PRD/system/workflow/research/tracker updates, and canonical lifecycle updates are all assigned.
- Security drift: every production ingress must use exact active trusted WebContents/main frame/document/origin authority with pre-handler rejection, pre-release revalidation, lifecycle cancellation, and caller/current-window targeting. No sender-only or raw compatibility exception remains.
- Electron API completeness: the declaration-derived global/scoped/frame/service-worker, WebContents IPC-event, MessagePortMain, EventEmitter-equivalent, postMessage/port-transfer, alias/destructure/bind/re-export coverage is explicit, scanner-tested, and guarded by empty production receiver/caller bypass allowlists.
- Wire drift: close/resize retain exact logical base names and payloads but deliberately move atomically to the existing private correlated transport; all other channel names/application payloads remain unchanged and both bundles are verified.
- Registration/lifecycle: the plan removes duplicate/stale raw listeners, covers main-window recreation, and explicitly orders close response before teardown.
- Outgoing lifecycle: all located main-originated cardano, menu, download, app-update, block/disk, Mithril, and hardware sends are manifest-owned, explicitly awaited or safely fire-and-forget, rebound to the current window where long-lived, and tested for cancellation, exact cleanup, latest-window-only targeting, and zero unhandled rejection.
- Inventory consistency: constructor, registration, and caller ownership are distinct; main-originated temporary response listeners and test-only raw probes are classified rather than omitted; `createChannels(MainIpcChannel)`, bidirectional hardware channels, `source/main/index.ts`, cardano setup, utilities, windows, IPC, preload, shared contracts, renderer adapters, tests, and docs are all included in the audit boundary.
- Elegance: no generic runtime registry, invoke migration, generated source mirror, new dependency, or per-capability policy layer is proposed. The typed manifest is evidence-only and the test derives live parity.
- Interaction mode: `autonomous` is truthful because all acceptance evidence is local and agent-executable; no hidden packaged, configured-network, physical-device, partner, audit, or user-owned checkpoint is included.
- Internal consistency: scope, expected files, approach, acceptance, verification, risks, conflicts, and updates all preserve task-101 as the wrapper foundation and task-102 as the final privileged-listener audit, while production guest launch remains disabled.
- Blocker resolution: the plan now derives exhaustive ingress/caller coverage from live Electron 41/Node declarations with symbol-aware evasion tests and empty production bypass allowlists; it also inventories every live main-originated wrapper promise and specifies minimal awaited/fire-and-forget ownership, rebound current-window targeting, cancellation/cleanup/race tests, and zero unhandled rejection.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Planning and implementation review are approved. The 77-channel manifest,
  authenticated window controls, current-window promise ownership,
  declaration-derived symbol-aware audit, hostile Electron fixture, and security
  documentation are implemented in the live diff.
- Final verification: 14 focused Jest suites with 50 tests, `yarn compile`, the
  bounded Electron 41 fixture covering all 77 channels, focused
  ESLint/Prettier, and `git diff --check` passed. Earlier production main and
  renderer builds also passed.
- Final review: `Decision: approved` in
  `.agent/plans/dapp-browser-cip30/task-plans/task-102-impl-review.md` after the
  user-authorized sixth iteration.
- No additional research beyond the task-102 audit evidence was required.
  Production guest launch remains disabled by the independent sandbox, guest
  preload, wallet/protocol, hardware, audit, packaging, and release gates.
