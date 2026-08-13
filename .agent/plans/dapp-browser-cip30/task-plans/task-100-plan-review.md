Planner: Iteration 1
Timestamp: 2026-08-13T13:55:18Z

Created canonical plan `.agent/plans/dapp-browser-cip30/task-plans/task-100.md`.

Interaction mode: `autonomous`. No user input, manual testing, or external evidence is required. Implementation can proceed immediately.

Plan summary:
- Register popup denial before any application `WebContents` creation, with no automatic shell side effect.
- Lock trusted-window top-level navigation before its first `loadURL`.
- Accept only parsed, credential-free HTTPS external URLs and await `shell.openExternal`.
- Replace import-time external-URL and local-directory IPC listener side effects with explicit initialization.
- Add focused tests for navigation bypasses, popup denial, URL validation, awaited shell failures, and listener registration.
- Preserve task boundaries: sender/frame authentication, guest policy, renderer migration, and sandbox proof remain downstream work.
- Synchronize PRD, architecture, API documentation, tracking, and canonical outcome only after implementation review approval.

Repository understanding: loaded `understand` before exploration. No existing knowledge graph was available, so all material findings were verified against live source and focused git history.

Self-review found no scope creep, hidden manual checkpoint, missing test/documentation requirement, trust-boundary drift, or conflict with the hostile-content constraints. Both review logs remain untouched. `git diff --check` and touched-file Prettier validation passed.

Outcome: Canonical task-100 plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-13T14:00:40Z

Blocking findings:
- The navigation design is not Electron-event complete. Electron 41 documents that `will-navigate` does not fire for programmatic `loadURL`, history navigation, or same-document hash navigation. Server redirects have their own cancellable `will-redirect` event. The plan must define the exact event matrix instead of describing hashes as allowed by a `will-navigate` check: trusted initial `loadURL` uses the same canonical target, renderer-initiated main-frame document navigation is checked in `will-navigate`, redirects are independently checked in `will-redirect`, and hash routing is recognized as non-cancellable same-document navigation. Add redirect-to-remote tests and avoid relying on string-prefix comparison.
- Denied navigation interacts dangerously with the existing load-error recovery path. `source/main/windows/main.ts` sends every `did-fail-load` to `RendererErrorHandler`, whose retry invokes `createMainWindow()` without its required arguments. A prevented navigation may surface as an aborted load, turning a blocked remote-navigation attempt into window recreation, duplicate IPC setup, or a crash. The plan must explicitly prevent policy-denied/`ERR_ABORTED` navigation from entering recovery and test that denial creates no replacement window or listener duplication.
- The proposed explicit IPC registration assumes `ipcApi(window)` runs only once and explicitly rejects production idempotency, but live source contains an intended window-recreation path. Moving the shell handlers from import-time registration into `ipcApi(window)` can therefore install duplicate listeners. Define concrete listener ownership: either register process-scoped shell channels once outside per-window setup, or make registration safely single-install with testable behavior. Do not leave this to Jest module-registry resets.
- External-URL error behavior is incomplete. Require a runtime string check before parsing, generic privacy-safe validation errors that never contain the submitted URL/query/credentials, no rejected-value logging, canonical serialization, and privacy-safe handling of `shell.openExternal` rejection. Tests must assert sensitive input is absent from errors and logs. The current renderer caller discards the returned promise, so the plan must also define how rejection is consumed without producing an unhandled renderer rejection while still preserving truthful failure semantics.
- The verification plan cannot claim Electron 41 event/order behavior from pure policy mocks, compilation, or `build:main`. Add a narrow Electron runtime test covering initial trusted load, remote renderer navigation, remote redirect, hash routing, popup denial, and denied-navigation recovery behavior, or identify an existing release-equivalent test that proves those behaviors. Keep packaged hostile-guest and sandbox evidence downstream.
- Documentation sequencing is internally inconsistent. PRD, architecture, and API documentation are acceptance criteria, but the plan defers them until implementation-review approval. That would make the reviewed diff incomplete and create unreviewed post-approval changes. Include documentation and non-completion lifecycle evidence in the implementation-review diff; only the final tracker completion status and final outcome should wait for approval.
- The task tracker says the trusted renderer cannot navigate to remote content, while the canonical plan silently narrows this to top-level navigation. Reconcile the scope explicitly. If task-100 intentionally protects only the privileged top-level document, state that consistently in the tracker/PRD and document why remote subframes do not inherit the privileged preload/Node surface; otherwise enforce and test the required frame policy with `will-frame-navigate`.

Non-blocking observations:
- A hostname allowlist is unnecessary for this bounded trusted-UI external-link action. Parsed, credential-free HTTPS plus later sender/frame authentication is the simpler division of responsibility.
- Construct the packaged URL with `pathToFileURL` and compare normalized URL components rather than retaining the current interpolated `file://${...}` form, especially for Windows paths and escaped characters.
- The global popup deny default is appropriate, but task-105 should retain an independently tested guest policy rather than treating this handler as its complete popup boundary.
- Preserve the current distinction that task-100 hardens an existing privileged channel but does not make it guest-safe before tasks 101 and 102.
- The proposed small policy module is justified for testing; broader policy classes, registries, or guest-oriented abstractions are not.

Approval bar:
- Revise the canonical plan to specify complete Electron navigation/redirect/hash event handling and fail-closed canonical URL comparison.
- Resolve denied-navigation interaction with `did-fail-load` recovery and repeated IPC initialization.
- Add privacy-safe external URL validation/error handling and explicit renderer-side rejection consumption.
- Require viable runtime Electron evidence for event ordering and bypass cases.
- Reconcile top-frame scope and move documentation updates into the reviewed implementation set.
- Preserve the narrow task boundary and avoid introducing guest/session abstractions or sender-authentication work owned downstream.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-13T14:06:40Z

Revised the canonical task-100 plan to resolve every iteration-1 blocker:

- Defined the Electron 41 matrix for programmatic loads, `will-navigate`, `will-redirect`, `will-frame-navigate`, and non-cancellable hash/History navigation.
- Added canonical URL construction with `pathToFileURL` and component-based comparison.
- Denied every trusted-window subframe document navigation while preserving downstream guest frame/session ownership.
- Excluded subframe and `ERR_ABORTED` failures from renderer recovery and bound valid arguments for retained main-frame recovery.
- Made shell IPC listeners process-scoped, explicit, import-side-effect-free, and idempotent across window recreation.
- Added runtime string validation, canonical HTTPS serialization, fixed privacy-safe errors, secret-absence assertions, and no rejected-value logging.
- Required `AppStore.openExternalLink` to consume fire-and-forget rejection while preserving rejecting channel semantics.
- Added a narrow pinned-Electron runtime fixture covering initial load, remote navigation, redirects, hash routing, popups, subframes, recovery isolation, and listener/window duplication.
- Moved PRD, architecture, API documentation, and tracker acceptance reconciliation into the implementation-review diff; only completion metadata remains post-approval.
- Self-reviewed the complete blocker list and verified the canonical plan with `git diff --check` and focused Prettier validation.

Outcome: Canonical task-100 plan revised and ready for focused Critiquer iteration-2 approval check

Critiquer: Iteration 2
Timestamp: 2026-08-13T14:08:24Z

Focused convergence check against the complete iteration-1 blocker list found that the revised canonical plan:

- Defines the complete Electron 41 navigation matrix, canonical URL comparison, redirect handling, hash/History behavior, and deny-all subframe policy.
- Prevents aborted, cancelled, and subframe load failures from entering renderer recovery while preserving correctly bound recovery for genuine main-frame failures.
- Makes shell IPC registration explicit, process-scoped, import-side-effect-free, and idempotent across window recreation.
- Specifies runtime type validation, credential-free HTTPS canonicalization, privacy-safe errors and logging, awaited shell failures, and renderer-side rejection consumption.
- Requires viable pinned-Electron runtime evidence for navigation, redirects, hashes, popups, subframes, recovery isolation, and listener/window duplication.
- Includes PRD, architecture, API documentation, and tracker acceptance reconciliation in the implementation-review diff while deferring only completion bookkeeping.
- Preserves the narrow task boundary without introducing guest/session policy, sender authentication, or generalized policy infrastructure.

The proposed implementation remains proportionate: one small trusted-window policy seam, one process-scoped shell registration seam, focused tests, and no speculative framework. The `autonomous` classification remains truthful because all required evidence is agent-executable in the workspace and packaged sandbox, hardware, configured-network, external-audit, and user-manual evidence are explicitly excluded as downstream gates.

Blocking findings:
- None. Every iteration-1 blocker is resolved.

Non-blocking observations:
- None requiring plan changes. Implementation review should verify that the runtime fixture exercises real Electron events rather than reproducing policy logic in mocks.

Approval bar:
- Met. The revised plan is complete, narrowly scoped, testable, privacy-safe, and consistent with the tracker, PRD, accepted threat model, live Electron boundary, and downstream task ownership.

Decision: approved

