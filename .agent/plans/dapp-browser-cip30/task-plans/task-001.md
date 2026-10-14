# Task task-001: Write the hostile-renderer threat model and architecture ADR

## Task

- Task ID: `task-001`
- Title: `Write the hostile-renderer threat model and architecture ADR`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`

## Why This Task Now

- This is the first phase-0 security gate and has no task dependencies.
- Every guest or connector implementation task depends on a stable statement of the hostile-renderer boundary, authority model, approval-byte binding, accepted availability exposure, and release gates.
- The live application still has privileged-renderer and sandbox debt that must be named accurately before later tasks add a remote guest.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs: none. The PRD and task tracker already freeze the product and architecture decisions needed by this task.
- Required manual test steps: none for this documentation task.
- Required evidence from the user: none. Packaged sandbox proof, physical hardware certification, and internal/external audit evidence remain later task gates and must not be claimed as task-001 evidence.
- Implementation can proceed without user interaction and can be completed with repository inspection and documentation verification.

## Scope

- Add one explicit hostile-renderer threat-model and Architecture Decision Record section to the dApp browser PRD.
- Name protected assets, attacker capabilities and trust assumptions, all relevant trust boundaries, security invariants, principal abuse cases, consequences, and residual risks.
- Lock the separately managed sandboxed `BrowserWindow`, fresh nonpersistent session, dedicated least-authority preload, and main-owned capability broker architecture.
- Explain how route, wallet, network, origin, frame, document generation, session, extension, and grant state compose into authority.
- Explain how immutable broker-owned bytes and correlated trusted consent bind review, signing, data signing, and submission to the exact authorized request.
- Record product limits and distinguish ordinary within-limit availability failures from release-blocking confidentiality and integrity failures.
- Make the architecture documentation describe both the current unsafe baseline and the planned fail-closed target without implying that the target is already implemented.
- Record the live-baseline and threat-control traceability findings as the first research note for this plan workspace.
- Synchronize task tracking after the documentation is reviewed and truthfully complete.

## Non-Goals

- Do not implement a guest window, preload, broker, route lease, grant repository, IPC hardening, network egress policy, sandbox packaging changes, wallet API, signing, or UI.
- Do not freeze CIP wire schemas or fixtures owned by `task-002`.
- Do not resolve sandbox packaging, connection-bound DNS enforcement, backend, exact-CBOR, or hardware unknowns owned by later phase-0 tasks.
- Do not migrate the existing trusted renderer to context isolation or remove Node integration; record that work as privileged legacy debt and require navigation/IPC hardening before guest enablement.
- Do not weaken or reopen the PRD's frozen hostile-renderer decisions.
- Do not edit either review log during planning.

## Dependencies

- Task-graph dependencies: none.
- Governing inputs:
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - frozen decisions in `.agent/plans/dapp-browser-cip30/prompt.md`
- Downstream work remains blocked by this task, including privileged IPC hardening, sandbox validation, guest creation, broker implementation, and connector exposure.

## Research Consulted

- No files exist yet under `.agent/plans/dapp-browser-cip30/research/`; there was no prior task-specific research to consult.
- The PRD's current baseline, technical design, testing strategy, security review gates, risks, accepted risks, and evidence gates were used as the accepted design source.

## Docs, Workflows, And Skills Consulted

- Docs and planning anchors:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
  - `.agent/plans/readme.md`
  - `.agent/plans/dapp-browser-cip30/prompt.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- Workflows:
  - `.agent/workflows/update-doc.md`
  - `.agent/workflows/electron.md`
  - `.agent/workflows/ipc.md`
  - `.agent/workflows/test.md`
- Skill:
  - `understand` guidance was loaded before live-file verification. No existing `.understand-anything` graph was available, so material architecture claims were verified directly against live source files as required by the skill guidance.

## Live Repo Findings Verified For Planning

- `source/main/windows/main.ts` creates the trusted renderer with `nodeIntegration: true`, `contextIsolation: false`, and the existing privileged preload; it is unsuitable as a remote-content host.
- `source/main/preload.ts` exposes raw `ipcRenderer`, Node HTTP(S), environment/configuration values, paths, OS data, and logging to the trusted renderer and must never be reused by a dApp guest.
- `source/common/ipc/lib/IpcChannel.ts` and `IpcConversation.ts`, plus their main wrappers, pass only decoded messages into handlers and do not retain enough Electron sender/frame identity for hostile-renderer authorization. `IpcChannel` also uses shared response channels with listener registration after send.
- `source/main/index.ts` registers the global `web-contents-created` popup handler after `createMainWindow()` has created the trusted window and initiated `loadURL`, and only after substantial additional startup work; the handler forwards requested URLs to `shell.openExternal` without a sufficient scheme/origin policy. The source does not prove that registration occurs only after the page has finished loading.
- `source/main/ipc/open-external-url.ts` passes renderer-supplied values directly to `shell.openExternal` and does not perform the PRD-required parsed HTTPS policy.
- `source/main/webpack.config.js` and `nix/internal/x86_64-linux.nix` currently launch Electron with `--disable-setuid-sandbox --no-sandbox`.
- There is no live `source/main/dapp/`, dedicated dApp preload, nonpersistent guest session, route lease, or main-process CIP-30 broker. Documentation must label those as the decided target architecture, not current behavior.

## Files Expected To Change

- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/system/architecture.md`
- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `.agent/plans/dapp-browser-cip30/task-plans/task-001.md` for approved-plan lifecycle and final outcome updates

No code, package manifest, test source, or review-log change is planned.

## Implementation Approach

1. Add a dedicated threat-model and ADR section to the PRD without duplicating every later technical-design detail.
   - Mark the ADR `Accepted` and task-scoped to the connector architecture.
   - State context, decision, rejected alternatives, consequences, follow-up gates, and the fact that current code has not implemented the target yet.
   - Reject hosting remote content in the trusted renderer, `<webview>`/iframe reuse, reuse of the privileged preload or generic IPC, renderer-owned authority or persistence, persistent/reused guest sessions, and an external-browser connector.

2. Define protected assets and attacker scope explicitly.
   - Assets include funds and signing authority; exact transaction/message/submission bytes and resulting witnesses; wallet addresses, UTxOs, balances, public keys and xpubs; wallet/network identity and grants; passphrases; cardano-wallet mutual-TLS material; filesystem, shell, logs, updates, electron-store, hardware and other privileged IPC; catalog and policy integrity; and privacy-sensitive origins and associations.
   - Model a hostile dApp as controlling its top-level document, scripts, workers, subframes, navigation attempts, malformed/concurrent requests, storage attempts, redirects, resource hosts, and DNS/network behavior, including a catalogued dApp.
   - Assume the hostile page may fully compromise its guest renderer. Do not assume compromise of Electron main, the trusted local UI, cardano-wallet, cardano-node, the OS, or hardware firmware; instead list those as trusted components/dependencies with separate release, audit, and capability gates.

3. Inventory every trust boundary and authority transition in one compact matrix.
   - Remote network and DNS/connection destination to the guest session.
   - Hostile top frame, subframes/workers, isolated preload world, Electron guest `WebContents`, and main process.
   - Dedicated guest gateway versus existing privileged IPC and the trusted main frame.
   - Main broker to trusted renderer approval/execution IPC.
   - Trusted executor to cardano-wallet over mutual TLS, cardano-wallet to cardano-node/network, and hardware service to the physical device.
   - Main-owned grant/collateral persistence, memory-only capabilities/requests/witness staging, guest nonpersistent storage, logs/analytics/crash reports, and OS/package sandbox boundary.
   - For each boundary, name the untrusted input, authoritative principal, required validation, and fail-closed outcome.

4. Freeze route, origin, session, and lifecycle invariants.
   - Authority binds exact guest `WebContents`, top-level frame, canonical origin, document generation, fresh session, route-selected eligible wallet, monotonic route epoch, network ID/magic/genesis, negotiated extensions, scopes, and main-issued connection/request identifiers.
   - Invalid routes never use wallet fallback; navigation/reload, origin or dApp switch, route/wallet/network changes, guest failure/close, trusted renderer reload, and revocation invalidate live authority before stale result release.
   - Guest recreation, default-deny permissions, nonpersistent storage, connection-time HTTPS/WSS destination policy, disabled bypass transports, and production DevTools denial are security invariants rather than compatibility preferences.
   - Require policy enforcement against each actual connection destination for the initial top-level navigation, every redirect, every subresource, and every WSS connection, including DNS answer changes/rebinding and IPv4, IPv6, and IPv4-mapped IPv6 forms. DNS preflight alone is insufficient. WebRTC/data channels, STUN/TURN, WebTransport, QUIC, and every other non-proxied or unaudited transport remain disabled; if the initial Diagnostics destination cannot be connection-bound, production Diagnostics launch remains disabled.
   - Authorized submission continuation is the explicit exception: after confirmation it continues against the fixed wallet/network while stale guest result delivery remains suppressed.

5. Freeze trusted-consent and exact-byte binding.
   - Main owns immutable request bytes and method arguments from validation through result release.
   - Trusted UI receives broker-authoritative review data keyed by a main-issued request ID; its response contains decision plus request identity only and cannot replace bytes.
   - Connection and elevated disclosure consent occur in trusted UI; every signing, data-signing, and submission call receives fresh consent, and signing never waives submission consent.
   - For signing, distinguish the exact body bytes that VKey witnesses cryptographically sign from every byte, commitment, context item, and effect that trusted review must authorize. The immutable approval record binds the exact body, existing witness set, outer `isValid`, auxiliary data, script data and redeemers, datums, native/Plutus/reference scripts, broker-authenticated resolved-context snapshot/digest, and complete decoded semantic effects and commitment checks; missing, unknown, unsupported, mismatched, or changed material fails closed before signing or release.
   - Build or merge the final submission transaction before consent. Submission approval separately binds the exact immutable final envelope, including its body, final witness set, outer `isValid`, auxiliary data, script data, datums, and scripts; no component may be replaced or assembled after approval. Main verifies signer body hash, newly returned witnesses/COSE, final-envelope identity, and submission result before release.
   - Guest hiding/disabling, five-minute inactivity rejection, lifecycle cancellation, stale-result suppression, and privacy-safe error/log handling are included in the invariant set.

6. Separate release-blocking security failures from bounded availability behavior.
   - Record 64 KiB request-CBOR/payload, 50-item CIP-103 batch, 100-entry page, and five-minute consent-inactivity limits, with typed rejection before side effects.
   - Treat crashes, slowness, queue pressure, or rejection for valid within-limit requests as ordinary robustness/availability defects unless they cross authority, leak data, change approved bytes, produce unauthorized side effects, or bypass fail-closed behavior.
   - Treat any guest access to privileged capability, sender/origin/route confusion, stale authority, byte/review mismatch, unverified signer result, persistence leak, private-network/transport bypass, or unsandboxed production guest as release-blocking confidentiality/integrity failure.
   - Preserve the PRD's intrinsic platform/backend/device limits and accepted risks without relabeling them as solved.

7. Tie threats to verification and release gates.
   - Map each threat class first to its phase-0 evidence owner, then through the existing phase-1 through phase-9 task families and PRD security-review gates rather than inventing implementation in task-001.
   - Include phase-0 owner/evidence rows for `task-003` (reviewed cardano-wallet backend contract, consistency guarantees, owner/reviewer, migration/rollback and pin gate), `task-004` (exact-CBOR/body/output extraction and supported-era evidence), `task-005-a` (Linux package contract/matrix), `task-005-b` (installed-package Chromium sandbox proof), and `task-006` (Ledger/Trezor library, model, firmware, field, message-signing, and returned-hash capability matrices). Historical `task-005` now preserves the cancelled portable spike. Mark these as later evidence gates, not task-001 evidence.
   - Require privileged IPC sender/main-frame authentication, trusted navigation lock, packaged OS sandbox proof, connection-bound HTTPS/WSS egress with bypass transports disabled, full-ledger semantic review, exact-byte signer/result verification, pending-submission fault tests, privacy inspection, physical hardware certification, internal review, external audit, current Electron/Chromium review, and release-candidate change control.
   - State that production guest launch remains disabled until every applicable gate has evidence.

8. Update system architecture and durable research without creating competing sources of truth.
   - Add a concise `Planned dApp Browser Security Boundary` section to `.agent/system/architecture.md` that links to the PRD threat model/ADR, depicts the decided process flow, distinguishes current baseline from target architecture, and lists non-negotiable ownership boundaries.
   - Create `research/01-hostile-renderer-threat-model-traceability.md` with live baseline evidence, threat/control/gate traceability, and any implementation gotchas discovered. Keep normative decisions in the PRD/architecture docs and mark the research note as supporting evidence.
   - After review approval, update `task-001` in the tasks JSON to `completed`, add truthful completion metadata/notes, and update tracker metadata without changing dependencies or unrelated tasks.

## Acceptance Criteria

- The PRD contains an accepted ADR that locks a separate sandboxed `BrowserWindow`, fresh random nonpersistent session, dedicated least-authority preload, and main-owned capability broker; rejected less-safe alternatives and consequences are explicit.
- The threat model names every process, top frame/subframe/worker, preload and IPC transition, backend/node boundary, hardware-device boundary, persistence class, OS sandbox, and external network/DNS destination boundary.
- Protected assets, attacker capabilities, trust assumptions, security invariants, abuse cases, mitigations, accepted risks, and fail-closed outcomes are explicit.
- Route-selected wallet, canonical origin, guest sender/frame, document generation, session, route epoch, network genesis, extensions, scopes, and request identity are all required authority inputs; invalid wallet routes never fall back.
- Trusted approval is correlated to immutable main-owned bytes, review data cannot replace those bytes, signing and submission consent remain separate, and signer/submission results are verified before release.
- Signing approval explicitly distinguishes the exact body bytes cryptographically signed from the complete review binding: exact body, existing witness set, outer `isValid`, auxiliary data, script data/redeemers, datums, native/Plutus/reference scripts, authenticated resolved-context snapshot/digest, complete semantic effects, and validated commitments; absent, unsupported, mismatched, or mutated material fails closed.
- Submission consent binds the exact immutable final transaction envelope after witness assembly, including body, final witness set, `isValid`, auxiliary data, script data, datums, and scripts; nothing is substituted or assembled after approval.
- HTTPS/WSS policy applies at connection time to the actual destination of initial navigation, redirects, subresources, and WSS across DNS changes/rebinding and IPv4/IPv6 forms; DNS preflight alone is insufficient, bypass transports remain disabled, and production Diagnostics launch remains disabled unless its initial destination is connection-bound.
- The four documented product limits and before-side-effect rejection posture are explicit, and within-limit ordinary availability exposure is distinguished from release-blocking confidentiality/integrity failures.
- The trusted Node-enabled renderer is documented as privileged legacy debt that never hosts remote content; planned controls are not misrepresented as live.
- Security release gates and later evidence owners are traceable, and production guest launch remains blocked until their evidence exists.
- Threat traceability names the phase-0 evidence owners for backend consistency (`task-003`), exact-CBOR/era coverage (`task-004`), the Linux package contract (`task-005-a`), installed-package sandbox certification (`task-005-b`), and hardware capability matrices (`task-006`), then maps controls through phases 1-9 without claiming those artifacts as task-001 evidence. Historical `task-005` remains cancelled portable evidence.
- `.agent/system/architecture.md`, the PRD, supporting research, and task tracking agree without weakening any frozen decision.

## Verification Plan

- Re-read the complete added PRD and architecture sections against the task acceptance criteria and frozen decisions in `prompt.md`.
- Use a boundary checklist to confirm explicit coverage of process, frame, IPC, backend, node, device, persistence, sandbox/package, and network/DNS/transport boundaries.
- Use an invariant checklist to confirm sender/frame/origin/generation/session/route/wallet/network/extension/scope/request binding, revocation, signing approval over the exact body plus existing witnesses/`isValid`/auxiliary and script material/datums/scripts/resolved context/complete effects, exact-final-envelope submission approval, submission continuation, and stale-result suppression.
- Use a connection-destination checklist to confirm actual-destination enforcement for initial navigation, redirects, subresources, and WSS; DNS change/rebinding and IPv4/IPv6 forms; rejection of DNS-preflight-only designs; disabled bypass transports; and fail-closed production Diagnostics when its initial connection cannot be bound.
- Check the threat/control/owner matrix names phase-0 evidence from tasks 003-006 before phase-1 through phase-9 controls, while clearly identifying that evidence as downstream and not produced by task-001.
- Check exact agreement of the four product limits and the distinction between ordinary availability and release-blocking confidentiality/integrity risk across PRD, architecture, research, and tracker completion notes.
- Validate that planned components are labeled `planned` and live baseline statements still match direct source reads.
- Parse `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json` after editing and inspect the focused diff for unrelated task, dependency, or metadata changes.
- Check internal Markdown links and referenced paths exist.
- Run focused formatting checks appropriate to changed files if available; no application build, Jest, Cucumber, packaged Electron, hardware, or manual QA run is required because this task changes documentation/tracking only.
- Compare the final diff with the latest matching planning and implementation review-log entries during later review, while leaving both review logs untouched by Planner.

## Risks And Open Questions

- Duplication risk: the PRD already contains detailed controls. Keep the new threat model as the canonical threat/asset/boundary/ADR synthesis and link to detailed sections rather than copying them wholesale.
- Status-drift risk: architecture documentation could accidentally describe planned controls as implemented. Use explicit `Current baseline` and `Accepted target` labels.
- Trust-boundary drift risk: do not assign authority for guest identity, request bytes, grants, or result validation to the renderer merely because existing wallet API execution currently lives there.
- Availability ambiguity: ordinary within-limit robustness does not excuse confidentiality/integrity failure or side effects after validation failure.
- Open implementation questions listed in the PRD remain evidence gates for later tasks; this task records ownership and fail-closed posture but does not resolve them.

## Required Docs, Tracking, And Research Updates

- Update `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md` with the threat model and accepted ADR.
- Update `.agent/system/architecture.md` with a concise planned dApp security architecture and current-baseline warning.
- Create `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md` as supporting evidence and downstream gate traceability.
- Update `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json` only after implementation review confirms acceptance; preserve unrelated task state and graph structure.
- Update this canonical plan with approved planning/build status and final outcome during later lifecycle steps.
- No `.agent/plans/readme.md`, workflow, API endpoint, code, or manifest update is expected because the plan is already indexed and task-001 introduces no live channel or runtime behavior.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-001-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-001-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope creep: limited task-001 to threat-model/ADR documentation, supporting evidence, and required synchronization; all runtime implementation and unresolved spikes remain assigned to later tasks.
- Stale workflows: treated workflow code examples as guidance only and used live Electron, IPC, popup, and Linux launcher files as current truth.
- Missing docs/verification: included PRD, system architecture, research, tracker, link/path, JSON-parse, consistency, and focused-diff checks; correctly excluded application and manual security evidence from this doc-only task.
- Trust-boundary drift: retained main ownership of guest lifecycle, authority, immutable bytes, grants, approval correlation, and result validation; renderer remains presentation/execution under authenticated, broker-issued requests.
- Inconsistencies: preserved separate signing/submission consent, authorized-submission continuation, stale-result suppression, exact-byte plus complete-semantic review, all four product limits, and fail-closed release gates.
- Critique resolution: made the signing versus reviewed-material distinction explicit, bound submission to the final envelope, elevated actual connection-destination and Diagnostics fail-closed policy to acceptance level, added phase-0 evidence owners, and corrected the popup-registration baseline to the event order the live source proves.
- Interaction mode: `autonomous` is truthful because all decisions are frozen and no user/manual evidence is needed to complete this documentation gate.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Completed. Planning and implementation reviews are approved. The accepted ADR, threat model, architecture boundary summary, and supporting live-baseline/traceability research are synchronized with task tracking. Verification: direct live-source inspection, focused boundary/invariant checks, `git diff --check`, and touched-file Prettier checks passed. No new research beyond `research/01-hostile-renderer-threat-model-traceability.md` was required.
