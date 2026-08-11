Planner: Iteration 1
Timestamp: 2026-08-11T13:00:33Z

Plan scope: Validate the pinned cardano-wallet revision separately from sibling HEAD; freeze exact capability, coherent transaction-context, signing-reuse, CIP-8/CIP-95, pending-submission, migration/rollback, and future pin gates; produce evidence and documentation only while phase 2 owns backend implementation and task-209 owns the pin; preserve the truthfully completed task-002 tracker state.

Interaction mode: manual_execution. Agent-executable contract research can proceed, but completion requires a named cardano-wallet work owner, authorized upstream reviewer, durable review URL, and explicit contract acceptance evidence.

Key evidence and verification: The plan records the lossy wallet TxOut, recent-era full-output query, incomplete signing API, proxy submission without pending state, wallet submission crash window, migration limitations, and maintenance-only upstream policy. It requires pin-specific and sibling comparisons, field-by-field contract tracing, tracker parsing, focused formatting and whitespace checks, and durable upstream acceptance.

Files expected: research/03-cardano-wallet-backend-contract.md, the project PRD, tracker, and canonical plan. No sibling source, Daedalus source, manifest, lockfile, pin, or review-log content changes are planned.

Self-review: No scope creep, hidden human checkpoint, security or wire-contract drift, missing documentation or verification category, or cross-repository sequencing inconsistency was identified. Focused Prettier, JSON parsing, and whitespace checks passed while drafting; review logs remained untouched.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-11T13:05:03Z

The documentation-only direction and manual checkpoint are appropriate. Live evidence supports the pinned revision, sibling baseline, lossy TxOut, limited full-output query, full-transaction signing response, submission crash window, untracked proxy submission, and exclusion of Electron, frontend, and hardware work.

Outcome: The plan needs one revision because material contract and task-authority ambiguities could permit completion without an implementable, testable backend agreement.

Blocking findings:

1. Mandatory tracker and orchestration reconciliation: task-003 currently requires a sibling branch or PR, reviewed pin-eligible commit, migration or rollback evidence, and pin gate, while the plan intentionally produces only a design-review record. Make synchronization of task description, acceptance criteria, target paths, and completion notes mandatory; distinguish phase-0 design review from phase-2 commits and task-209 pinning; record an explicit validation-only exception to cross-repository commit wording. Do not complete task-003 while governing requirements still demand unproduced artifacts.

2. Implementable coherent-context protocol: specify exact node acquisition point, wallet checkpoint point or generation, pending generation, bounded retries, and fail-closed mismatch. Define provenance and precedence for node UTxOs, wallet state, pending outputs, and earlier request outputs; all missing, spent, rolled-back, duplicate, self, forward, conflict, and unsupported-era cases; full-output recovery despite lossy persistence; and an independently recomputable binding over ordered exact bodies and authoritative context. Select stateless recomputation or bounded memory-only lifetime with restart and expiry semantics.

3. Definitive signing and CIP-8/CIP-95 ownership: evaluate both live V1 and V2 signing paths for exact body and existing-witness preservation, enumerate mutable witness classes, and define current-batch and partialSign input. Restrict accepted deltas to independently verified new VKey witnesses after envelope/body validation. Select one exact COSE ownership boundary rather than leaving raw proof versus complete structure open; backend authenticates ownership/key selection and Daedalus verifies frozen COSE bytes, association, payload, and signature.

4. Precise durable submission state machine: replace atomic-with-broadcast wording with write-ahead states and exact-byte identity, authorization and attempt status, durable commit point, transitions, failure and retry rules, expiry, confirmation, rollback, and crash recovery. Cover concurrent replay, same hash with different bytes, wallet relevance, normal/collateral accounting, never-broadcast records, terminal rejection, CIP-103 attempt-all ordering, and cardano-wallet-only reconciliation without duplicate broadcasts.

5. Complete capability, error, and privacy contracts: require an exact endpoint and strict schema, unique capabilities/revisions, build/API relationship, malformed/duplicate/contradictory/partial/old responses, network and era availability, and startup versus invocation checks. Add an exhaustive backend-to-frozen-public error matrix for malformed bytes, network/era/field failures, stale context, proof/passphrase/node/persistence/replay/submission failures, with no sensitive details in info, HTTP bodies, traces, or transaction logs.

6. Assigned verification obligations: require a requirement-to-test matrix mapped to tasks 200-209 and named sibling test layers. Include pin-versus-sibling goldens, snapshot races/retries, full-output fidelity, adversarial batch ownership/dependencies, V1/V2 body/witness differential tests, CIP-8/CIP-95 vectors, capability downgrade/malformed cases, crash injection around every state transition, concurrent replay, confirmation/expiry/rollback, pinned-schema migration, backup restore, and old-pin rollback behavior. Upstream must explicitly accept this evidence plan, not only acknowledge the design.

Non-blocking observations:

- manual_execution is truthful and correctly blocks completion before durable upstream acceptance.
- Pinned and sibling revisions are correctly separated; record relevant drift rather than assuming it immaterial.
- Keeping Haskell, Daedalus clients, pin changes, Electron, frontend, and hardware outside task-003 is appropriately narrow.
- Reuse signing and submission seams only after evidence proves required semantics.
- Deferring live API docs until implementation avoids presenting a proposal as shipped behavior.

Approval bar:

Revise once to mandate tracker/orchestration reconciliation, concrete context and binding semantics, definitive signing and CIP-8 ownership, a complete durable submission state machine, exact capability/error/privacy contracts, and an upstream-accepted requirement-to-test matrix while preserving documentation-only scope and the manual checkpoint.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-11T13:11:08Z

Revised the canonical plan to resolve all six blockers:

- Made tracker reconciliation mandatory across task-003 description, paths, notes, acceptance, and completion metadata, with a validation-only exception that preserves phase-2 candidate-commit, migration, integration, and pin sequencing.
- Fixed context semantics with exact W/G/P points and generations, three bounded attempts, source precedence, conflict handling, full-output recovery, an independently recomputable binding, stateless authenticated tokens, expiry, and restart behavior.
- Required decisive V1/V2 signing differential evidence, VKey-only accepted deltas, exact current-batch and partialSign inputs, and backend-produced complete CIP-8/CIP-95 COSE with independent Daedalus verification.
- Replaced atomic-broadcast wording with a write-ahead submission state machine covering durable identity, transitions, input claims, crashes, unknown outcomes, replay, concurrency, expiry, rollback, and CIP-103 ordering.
- Selected an exact strict capability endpoint/schema with startup and invocation checks, exhaustive backend-to-public error mapping, and explicit privacy/logging prohibitions.
- Added a requirement-to-test matrix for tasks 200-206, 208, and 209 with named sibling and Daedalus test layers, explicit task-207 absence, and mandatory upstream acceptance of the evidence plan.

The manual_execution checkpoint and documentation-only scope remain unchanged. Focused Prettier and tracker JSON validation passed; review logs were not modified by Planner.

Outcome: Plan revised and ready for focused approval check

Critiquer: Iteration 2
Timestamp: 2026-08-11T13:12:25Z

The revision resolves the complete iteration-1 blocker list without expanding task-003 beyond documentation and reviewed contract evidence.

Outcome: Focused approval passed. The plan mandates tracker reconciliation and a validation-only cross-repository exception; defines exact W/G/P context acquisition, retries, provenance, digest, stateless token, and restart semantics; requires decisive V1/V2 differential evidence and backend-owned complete COSE production; specifies a write-ahead submission state machine; fixes capability, error, and privacy contracts; and assigns an upstream-accepted test matrix across tasks 200-209.

Blocking findings:

None.

Non-blocking observations:

- The summary scope retains older before-or-atomically-with-broadcast wording while authoritative implementation and acceptance sections use write-ahead persistence. Normalize this during PRD and final task synchronization.
- The live tracker remains unreconciled because that is implementation work; the plan correctly makes it a completion gate.
- manual_execution, the upstream evidence checkpoint, and no-sibling-source-change scope remain truthful.

Approval bar:

Met. Implementation must execute mandatory tracker/PRD reconciliation, produce the exact reviewed research contract and evidence matrix, and keep task-003 pending until substantive upstream acceptance is returned.

Decision: approved

