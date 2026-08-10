Planner: Iteration 1
Timestamp: 2026-08-10T18:38:18Z

Created canonical plan `.agent/plans/dapp-browser-cip30/task-plans/task-001.md`.

Interaction mode: `autonomous`. No user input, manual testing, or external evidence is required; later sandbox, hardware, and audit gates remain explicitly out of scope.

Plan summary:
- Add the hostile-renderer threat model and accepted separate-`BrowserWindow`/main-broker ADR to the PRD.
- Document assets, attacker capabilities, every process/frame/IPC/backend/device/persistence/network boundary, authority invariants, approval-byte binding, product limits, accepted risks, and release gates.
- Update system architecture while distinguishing the unsafe live baseline from the planned target.
- Create a supporting threat-control traceability research note.
- Synchronize task tracking only after review confirms completion.
- Make no runtime, test, manifest, or review-log changes.

Live verification confirmed the privileged renderer, broad preload surface, insufficient IPC sender/frame authority, unsafe external URL handling, sandbox-disabling Linux flags, and absence of planned dApp isolation components.

Self-review found no scope creep, hidden manual checkpoint, missing documentation/verification requirement, trust-boundary drift, or frozen-decision inconsistency. Both review logs remain untouched.

Outcome: Canonical task-001 plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-10T18:40:46Z

Blocking findings:
- The exact-byte approval contract is underspecified in the implementation steps and acceptance criteria. Require signing review to bind the exact body, existing witness set, outer `isValid`, auxiliary data, script data, datums, scripts, resolved context, and complete semantic effects; require submission consent to bind the exact final envelope. Explicitly distinguish bytes signed from all bytes and commitments that must be reviewed.
- Network containment lacks an acceptance-level statement that policy applies to actual connection destinations across initial navigation, redirects, subresources, WSS, DNS changes/rebinding, and IPv4/IPv6 forms. State that DNS preflight alone is insufficient, bypass transports remain disabled, and Diagnostics launch remains disabled when its initial destination cannot be connection-bound.
- Threat-to-owner traceability incorrectly starts at phase 1 in step 7. It must include the phase-0 validation owners for backend consistency, exact-CBOR coverage, Linux sandbox packaging, and hardware capability matrices, then map through phases 1-9.
- The live-baseline claim that popup policy is installed “only after the main window has loaded” is stronger than the source proves. `source/main/index.ts` registers it after `createMainWindow()` has created the window and initiated `loadURL`, following substantial startup work. Use that precise statement so the research note does not convert a timing risk into an unsupported event-order claim.

Non-blocking observations:
- The autonomous interaction mode is truthful because packaged sandbox proof, hardware certification, audits, and rollout evidence remain explicitly assigned to later tasks.
- Scope is appropriately documentation-only and avoids implementing speculative broker, network, backend, hardware, or packaging machinery.
- PRD, architecture, supporting research, canonical-plan lifecycle, and tracker synchronization are the correct documentation surfaces; no API-endpoint or application-test update is needed without runtime changes.
- Sender/main-frame authority, stale-result suppression, authorized-submission continuation, privacy restrictions, bounded availability treatment, and current-versus-planned architecture labeling are otherwise covered comprehensively.
- The three-document synthesis is justified if the PRD remains normative, architecture stays concise, and research contains evidence and traceability rather than duplicated requirements.

Approval bar:
- Add acceptance-level exact-byte and connection-destination invariants, include phase-0 evidence owners in traceability, and correct the popup-registration baseline wording. Preserve the current narrow scope and verification posture.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-10T18:42:41Z

Revised the canonical `task-001` plan to resolve all Critiquer blockers:

- Signing approval now binds the exact body, existing witness set, `isValid`, auxiliary/script data, datums, scripts, resolved context, commitments, and complete semantic effects while distinguishing body bytes actually signed.
- Submission approval now binds the immutable final envelope after witness assembly.
- Connection-time policy now covers actual destinations for initial navigation, redirects, subresources, WSS, rebinding, and IPv4/IPv6 forms; DNS preflight is insufficient, bypass transports stay disabled, and unenforceable Diagnostics launch stays disabled.
- Traceability now identifies phase-0 evidence owners for backend consistency, exact-CBOR coverage, Linux sandbox packaging, and hardware capability matrices.
- Popup baseline wording now precisely reflects registration after window creation and `loadURL` initiation without claiming page-load completion.
- Acceptance and verification criteria mirror each strengthened invariant. Review logs remain unchanged.

Outcome: Canonical task-001 plan revised and ready for focused approval review

Critiquer: Iteration 2
Timestamp: 2026-08-10T18:44:05Z

Blocking findings:
- None. All Iteration-1 blockers are resolved without introducing a new blocking defect.

Non-blocking observations:
- Signing review now distinguishes cryptographically signed body bytes from the complete immutable review binding, and submission consent binds the final assembled envelope.
- Network containment now covers actual connection destinations, rebinding, redirects, subresources, WSS, and address forms, with fail-closed Diagnostics behavior.
- Traceability includes phase-0 evidence owners before phases 1-9.
- Popup-registration wording now matches the live source precisely.

Approval bar:
- Met. The revised plan preserves the narrow documentation-only scope and verification posture while resolving every original blocker.

Decision: approved

