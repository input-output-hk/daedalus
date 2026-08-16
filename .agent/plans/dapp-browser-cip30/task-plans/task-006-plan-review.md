Planner: Iteration 1
Timestamp: 2026-08-14T20:20:07Z

Plan: Created the canonical `task-006` plan at `.agent/plans/dapp-browser-cip30/task-plans/task-006.md` with planning status `draft` and build status `in_progress`.

Interaction mode: Classified as `autonomous`. Task-006 requires no user inputs, manual tests, or physical-device evidence because it freezes a static capability contract. Implementation can proceed immediately. Physical Ledger/Trezor model, app, firmware, cancellation, display, returned-hash, and signature certification remains a mandatory downstream `task-607` checkpoint.

Scope: Freezes versioned Ledger 7.1.4, candidate Ledger 8.0.0, and Trezor Connect 9.7.2 library matrices; transaction-field, CIP-8/CIP-95, response-proof, error, cancellation, and model/version contracts; a bounded Ledger dependency recommendation; deterministic later-certification cases; and a privacy-safe evidence schema. Production dependency changes, hardware services, adapters, IPC, physical execution, and product enablement remain out of scope.

Live findings: Verified exact package and lock identities, Ledger 7.1.4’s app-major-7 cap and field/message surface, Trezor Connect 9.7.2’s static transaction/message surface, reduced current Daedalus adapters, absent hardware CIP-8 IPC and returned-proof verification, and the Ledger-only scope of `yarn test:hardware-wallets`. Identified stale workflow claims concerning Safe-model support, Trezor address verification, initialization, and Ledger/Trezor test coverage.

Verification: The plan requires deterministic installed/candidate package probes, generated matrix coverage checks, focused Jest, TypeScript, formatting, JSON validation, task-002/task-004 regression tests, privacy and fail-closed assertions, dependency-change exclusion, and `git diff --check`. The canonical plan passes `git diff --check`; neither review log was modified.

Self-review: Confirmed no scope creep into phase-6 implementation or physical certification, explicit static-versus-physical evidence dimensions, complete expected manifests/tests/docs, hardware fail-closed boundaries, preserved task-002 wire contracts and task-004 exact-CBOR/Dijkstra decisions, corrected stale workflow/dependency claims, mandatory downstream human certification, and disabled production guest launch.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-14T20:23:50Z

Outcome: The plan has the correct phase-0 boundary and appropriately keeps physical-device work in task-607, but several contracts remain underspecified or internally inconsistent. These are currently knowable and must be fixed before implementation begins.

Blocking findings:
1. Critical: Exact-byte capability is not fully tied to task-004’s accepted encoding families. The matrix must classify each applicable exact-body representation by reproducibility, deterministic host-side reconstruction/hash comparison, required physical returned-hash evidence, or pre-device rejection.
2. Critical: The evidence-status model mixes representability, evidence, implementation, and enablement. Replace it with orthogonal validated dimensions, define the fail-closed predicate over their intersection, and reject contradictory combinations.
3. Critical: Downstream task wording creates certification-before-certification semantics. Rewrite affected phase-6 contracts so tasks 600-606 consume only the frozen static contract and task-607 alone promotes exact artifact/model/app-or-firmware rows after physical evidence with complete evidence inputs.
4. High: Freeze operation-specific typed failure predicates for transaction and message operations across malformed input, unsupported shapes, incompatibility, wrong app, disconnects, refusal, host/vendor cancellation, malformed response, proof mismatch, and stale completion.
5. High: Make candidate dependency identity complete and immutable whenever runtime probes are claimed. Bind Trezor behavioral transitives/configuration assets, the direct-versus-Connect transport split, and exact Ledger deep imports.
6. High: Explicitly handle Trezor Connect 9.7.2 CIP-8 output incompatibility: vendor COSE bytes cannot pass through; require independently validated raw material, exact Daedalus reconstruction, task-002 fixture equivalence, and separate model/firmware identity-return capability rows.
7. High: Normalize task-607 evidence to enums and bounded numerics; prohibit screenshots, raw payloads, personal/device data, sensitive paths, raw transactions, and arbitrary errors; use opaque identities and digest/access-policy binding for external raw evidence.
8. Medium: Correct the workflow’s Trezor popup claim and sensitive `devicePath` logging recommendation in addition to the other identified stale claims.

Non-blocking observations:
- `autonomous` is truthful for this static contract task; physical evidence remains task-607-owned.
- The no-production-change boundary and reuse of task-002/task-004 fixtures are sound.
- `Build Status: in_progress` should be reconciled with the stated no-implementation-yet outcome or explicitly documented as the orchestration convention.

Approval bar:
- Resolve all eight blocking findings while keeping task-006 autonomous, static-only, dependency-neutral, and fail-closed.
- Preserve task-002 wire bytes and task-004 exact-CBOR decisions without reinterpretation.
- Focused tests must enforce matrix completeness, impossible-state rejection, immutable artifact identity, exact fixture references, typed errors, Trezor COSE non-pass-through, privacy restrictions, downstream ownership, and no static promotion to physical certification or product enablement.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-14T20:28:01Z

Plan: Revised `.agent/plans/dapp-browser-cip30/task-plans/task-006.md` with planning status `in_review` and required build status `in_progress`, explicitly defined as orchestration state rather than implementation evidence.

Scope preserved: Task-006 remains autonomous, static-only, dependency-neutral, and non-certifying. Tasks 600-606 consume frozen contracts while remaining product-disabled; task-607 alone records reviewed physical-certification results, and later release policy owns product enablement.

Blockers resolved: Added exact task-004 encoding-family dispositions and hash gates; six orthogonal capability dimensions with impossible-state rejection; complete phase-6 tracker rewrite requirements; exact transaction/data-signing error mappings; immutable Ledger candidate/deep-import and Trezor transitive/config identities; mandatory Trezor raw-material validation and Daedalus COSE reconstruction; normalized privacy-safe task-607 evidence; and workflow corrections for `popup: false` and sensitive `devicePath` logging.

Verification: Focused tests now enforce exact-body coverage, deterministic hash reproduction or pre-device rejection, orthogonal state invariants, immutable dependency/config identity, operation-specific errors, Trezor COSE non-pass-through, privacy restrictions, task-607 evidence completeness, and phase-6 ownership. `git diff --check` passes, and neither review log was modified.

Self-review: Confirmed all Critiquer blockers are addressed without production adapters, dependency updates, behavioral mocks, physical execution, or standards drift. Task-002 wire contracts and task-004 exact-CBOR decisions remain normative; unresolved capabilities fail closed; no static evidence can promote physical certification or product enablement.

Outcome: Plan revised and ready for focused approval check

Critiquer: Iteration 2
Timestamp: 2026-08-14T20:29:11Z

Outcome: Six original blockers are resolved, but blockers 4 and 6 remain partially unresolved due to two contract errors introduced in the revision.

Blocking findings:
1. High, blocker 6: The Trezor COSE rule incorrectly requires physical payment, stake, and DRep results to have “byte equality with task-002 fixtures” (`task-006.md:138`, `task-006.md:178`). Real devices use different keys and signatures, so their complete COSE bytes cannot equal the deterministic task-002 golden. Require exact byte equality only for synthetic inputs using the frozen fixture material. Physical cases must instead prove exact frozen encoding rules, original payload preservation, expected address/key-hash identity, public-key association, and signature verification without vendor COSE pass-through.
2. High, blocker 4: The error table does not explicitly distinguish the 64 KiB task-002 product limit from a smaller vendor protocol/device payload limit. Product-limit violations map to `APIError.InvalidRequest`, while a valid within-product-limit request exceeding a hardware protocol capability must map to operation-specific `DataSignError.ProofGeneration` or `TxSignError.ProofGeneration`. Add this predicate explicitly so task-604/605 cannot reinterpret it.

Non-blocking observations:
- Blockers 1, 2, 3, 5, 7, and 8 are resolved without broadening task-006 into implementation or physical certification.
- Exact task-004 representation dispositions, orthogonal dimensions, impossible-state checks, immutable Ledger/Trezor dependency identities, phase-6 ownership, privacy normalization, and workflow corrections now satisfy the recorded approval bar.
- The `in_progress` lifecycle value is now explicitly documented as orchestration state rather than implementation evidence.

Approval bar:
- Correct the two remaining blocker-derived contracts: scope task-002 byte equality to matching synthetic fixture material while defining cryptographic/structural physical conformance, and freeze the distinct product-limit versus hardware-intrinsic-limit error mappings.
- Preserve all other revised boundaries and focused verification requirements unchanged.

Decision: requires_changes

Planner: Iteration 3
Timestamp: 2026-08-14T22:50:37Z

Plan: Applied the user-authorized narrow correction to `.agent/plans/dapp-browser-cip30/task-plans/task-006.md` without changing either review log or any other revised contract.

COSE correction: Exact task-002 golden-byte equality now applies only to synthetic cases using the exact frozen fixture payload, key, identity, and signature material. Physical payment, stake, and DRep cases instead require frozen encoding conformance, original-payload preservation, expected address/key-hash identity, public-key association, signature verification, valid local COSE reconstruction, and no vendor COSE pass-through.

Limit correction: The task-002 product boundary accepts 65,536 decoded bytes and rejects 65,537 with `APIError.InvalidRequest`. Requests within that product limit but beyond a smaller vendor protocol/device capability map to `TxSignError.ProofGeneration` or `DataSignError.ProofGeneration`. Tasks 604 and 605 are explicitly prohibited from reinterpreting either boundary.

Verification: Focused verification now covers synthetic golden equality, physical cross-key structural/cryptographic conformance, vendor COSE non-pass-through, the 65,536/65,537 product boundary, and synthetic smaller vendor limits. `git diff --check` passes.

Self-review: Confirmed fixture-byte equality is no longer required across different physical keys/signatures, and product-limit failures cannot be conflated with intrinsic hardware-capability failures. All other revised scope, ownership, privacy, evidence, and fail-closed contracts remain unchanged.

Outcome: Authorized correction applied and ready for focused approval check

Critiquer: Iteration 3
Timestamp: 2026-08-14T22:51:06Z

Outcome: Both iteration-2 contracts are corrected, and all previously approved boundaries remain preserved.

Blocking findings:
- None.

Non-blocking observations:
- Task-002 golden-byte equality is now limited to synthetic cases using the exact frozen fixture material. Physical cases correctly require encoding, payload, identity, public-key association, signature, and local COSE verification without cross-key byte equality or vendor COSE pass-through.
- The 65,536-byte product boundary now maps violations to `APIError.InvalidRequest`, while smaller intrinsic hardware limits for otherwise valid requests map to operation-specific `TxSignError.ProofGeneration` or `DataSignError.ProofGeneration`.
- Task-006 remains autonomous, static-only, dependency-neutral, non-certifying, and fail-closed. Existing exact-byte, evidence, privacy, downstream ownership, and task-607 boundaries are unchanged.

Approval bar:
- Met. Both remaining contracts are exact and executable, task-002/task-004 decisions remain normative, and no static evidence can promote physical certification or product enablement.

Decision: approved
