Planner: Iteration 1
Timestamp: 2026-08-19T17:09:11Z

Plan: Created canonical plan `.agent/plans/dapp-browser-cip30/task-plans/task-201.md` for exact-point `W/G/P` capture, canonical full-ledger UTxO CBOR, protocol context, pending provenance, digest/token binding, and cross-repository delivery gates.

Interaction mode: `interactive_validation`. Implementation can proceed, but exact-candidate Conway local-cluster evidence is required; the known sibling Nix-store blocker may require operator execution.

Scope: Reuses existing LSQ, wallet DB, ledger serialization, task-200 API/error seams, and test infrastructure. Excludes ownership/batch analysis, signing, durable submission, Daedalus runtime clients, capability activation, and pinning.

Delivery: Plans a non-amended cardano-wallet child commit, memory-only state with no migration, temporary Daedalus candidate builds, internal combined review, unchanged tracked pin, and consolidated upstream review/activation in task-209.

Verification: Direct Prettier and `git diff --check` passed. Both review logs remain untouched.

Self-review: Checked scope creep, test/documentation coverage, backend trust authority, wire-contract fidelity, privacy, rollback, interaction classification, and task-202/task-209 boundaries.

Outcome: Canonical plan drafted with planning status `in_review` and build status `in_progress`, ready for critique.

Critiquer: Iteration 1
Timestamp: 2026-08-19T17:13:38Z

Blocking findings:
1. The HTTP contract is not implementation-ready. `task-201.md:98-101` and `:136-142` name strict types but never freeze the complete request/response schema, byte wrappers, cardinality rules, or exact representation of chain point, records, protocol parameters, pending entries, digest, and token. Task-209 cannot retroactively choose the representation after this route, Swagger, clients, and goldens ship. Add the exact revision-1 schema to this plan and align research 03 before implementation.
2. Requested input roles remain caller-authoritative. The route accepts normal/collateral/reference outpoint lists, but the plan never requires parsing each supplied transaction and proving those lists exactly match its body. Checking only duplicate or “incompatible” claims permits omitted or relabeled inputs. Task-201 must derive and validate all three input sets from the ordered transaction CBOR; task-202 may still own ownership, witness requirements, earlier-output resolution, and conflict graphs.
3. The `W/G/P` mechanism is not concrete or demonstrably atomic. Live `DBLayer.atomically` is an existential database action, currently `SqlPersistT IO`, not STM, while `task-201.md:108-109` requires memory-only counters changed atomically with persistent wallet mutations. The plan also does not enumerate the mutation boundaries that advance `G` and `P` or prevent bypasses. Specify the actual DB-layer decoration/state mechanism, read/update ordering, rollback behavior, process-reset behavior, and tests proving no old-state/new-generation or new-state/old-generation snapshot can pass.
4. Pending provenance is underspecified. Current pending `TxCBOR` preserves raw transaction bytes, but the plan does not map existing submission states to revision-1 pending-state codes, define the exact available-wallet-UTxO overlay, or explain how normal/collateral claims and expiry populate record `0x07`. Task-208’s future states cannot be implied now. Freeze the task-201 subset and make unsupported or unavailable state fail closed.
5. Output-byte authority is contradictory. Research 03 binds `exact_ledger_txout_cbor` and treats unequal authoritative bytes as conflict, while `task-201.md:117`, `:171-172`, and `:245` choose canonical ledger reserialization that may change a pending output’s original encoding. Select one rule, define whether equality is byte or semantic/canonical equality, add a noncanonical pending-output vector, and update research 03 consistently. Full datum, inline datum, reference-script, multi-asset, and protocol-parameter bytes must have pinned-ledger and cross-language goldens.
6. Error coverage is incomplete at the real Servant boundary. `withWorkerCtx`, JSON decoding, wallet deletion, route mismatch, exact-point acquisition failure, unsupported era, malformed CBOR, and internal exceptions can currently escape through ordinary backend errors rather than the frozen dApp pairs. Define the exhaustive mapping and prove status, tag, fixed `info`, and body redaction for every route path, including malformed transport input and missing/deleted wallets. Logging tests must inspect actual traces, not only encoded error values.
7. The local-cluster checkpoint is both over-scoped and incorrectly human-dependent. The sibling Nix shell currently fails on another missing store source, but that ephemeral failure does not justify a baked-in operator handoff or waiting state. Keep implementation autonomous: repair/recreate the environment where feasible and report a real infrastructure blocker only if still present. Use deterministic network/DB tests for generation races, pruning, and three-attempt exhaustion; require one focused real HTTP/mTLS local-cluster scenario for exact-point success and observable acquisition failure. Do not manufacture flaky chain races or accept compilation as runtime proof.
8. Cross-repository review ordering is internally inconsistent. `task-201.md:187-188` asks Reviewer to inspect fixture/docs and canonical outcome, then updates research, PRD, tracker, and plan only after approval. The final reviewed state would therefore differ from the approved state. Define one candidate range and Daedalus evidence revision, apply review fixes as follow-up commits, update durable records truthfully, and have Reviewer examine the complete final range before completion. Task-209 must retain sole ownership of upstream PR, activation, aggregate integration, migration review, and pinning.
9. Migration and rollback depend on resolving finding 3. “No schema migration” is valid only if generations, secrets, tokens, and context remain genuinely process-memory-only and no DB representation changes. The plan must name the proof that candidate-created databases are byte/schema compatible with the old pin and state that any persisted generation or pending field forces replanning rather than silently expanding task-201.
10. The plan changes the accepted meaning of “volatile delta” to node provenance at `W` without first synchronizing the governing contract. Either amend research 03/PRD before implementation or retain a distinct defined field. Do not let implementation invent this interpretation and document it afterward.

Non-blocking observations:
1. The reuse-first LSQ approach, single additive route, unavailable capability endpoint, unchanged tracked pin, and exclusion of task-202 ownership/signing work are appropriately minimal.
2. `task-201.md:270-271` says build status is `in_progress` while implementation has not started; use `not_started` or the repository’s truthful equivalent.
3. The command in `task-201.md:227` should remain illustrative until the registered scenario title exists.
4. Deferring `.agent/system/api-endpoints.md` until task-209 activation is reasonable.

Approval bar:
1. Freeze the exact route schema and derive all input roles from transaction bytes.
2. Provide an implementable, atomic `W/G/P` design against the live DB layer.
3. Resolve pending-state, output-byte, protocol-byte, digest/token, and error contracts with executable goldens.
4. Replace the assumed human checkpoint with autonomous execution plus truthful infrastructure-blocker handling and a smaller deterministic test split.
5. Correct final cross-repository review, rollback, tracking, and research-update sequencing.
6. Non-blocking observations need not trigger another iteration once the blocking findings are resolved.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-19T17:27:03Z

Revision: Updated only the canonical task-201 plan and completed final self-review against all iteration-1 blockers.

Contract: Froze revision-1 schemas, representations, cardinalities, transaction-derived input roles, pending subset, volatile delta, original/canonical CBOR authority, records, digest/token, and exhaustive redacted route errors.

Consistency: Specified a concrete `SqlPersistT IO`-compatible atomic `ContextClockRegistry`, exact mutation ordering/boundaries, rollback/restart tests, autonomous HTTP/mTLS validation, no-schema/old-pin proof, final-state review sequencing, and preserved task-202/task-209 ownership.

Verification: Direct Prettier and whitespace checks pass. Review logs and all other files remain untouched.

Outcome: Canonical plan revised for focused iteration-2 approval with build status `in_progress`.

Critiquer: Iteration 2
Timestamp: 2026-08-19T17:29:36Z

Blocking findings:
1. The pending-overlay subset remains internally contradictory. `task-201.md:187` defines `produced_wallet_outputs` as the change from checkpoint membership to current `availableUTxO`, while `task-201.md:214` says that same set includes wallet-known pending-produced additions. Live `availableUTxO` only removes pending normal/collateral inputs from checkpoint UTxO and never adds pending outputs (`Cardano/Wallet/Primitive/Model.hs:460-478`); pending change is handled separately by `totalUTxO`. Therefore the response output union, `produced_wallet_outputs`, and exact pending membership cannot be implemented unambiguously. Freeze one explicit set equation: either preserve live `availableUTxO` with no produced additions, or define a new task-201 overlay that subtracts pending normal/collateral inputs and adds wallet-known pending outputs, then align output cardinality, recovery requirements, records, and fixtures.

Non-blocking observations:
1. The revision resolves the other iteration-1 blockers: closed schema and transaction-derived roles; concrete gated `SqlPersistT IO`-compatible `W/G/P`; pending-state mapping; source/canonical byte authority and fixtures; route-scoped redacted errors; autonomous deterministic/local-cluster evidence; final-state review sequencing; no-schema rollback proof; and a distinct volatile-delta projection synchronized before implementation.
2. `task-201.md:238` should acknowledge that a client disconnect after WAI receives the complete request can also prevent an HTTP body; this transport fact does not weaken normalization for responses that can be delivered.
3. The already-known `build status: in_progress` wording remains a truthful-status nit and does not affect approval once the blocker is resolved.

Approval bar:
1. Replace the contradictory pending-membership wording with one exact set equation and propagate it consistently through `outputs`, `pending_overlay`, record cardinality, and executable fixtures. No other iteration-1 finding needs reopening.

Decision: requires_changes

Planner: Iteration 3
Timestamp: 2026-08-19T17:36:03Z

Authorization: User-authorized narrow correction after the normal two-pass planning guard.

Revision: Corrected only the canonical task-201 plan pending-overlay contract.

Decision: Preserved live `availableUTxO` semantics: `A = C - (N union K)`, `S = C - A`, `O = A union Q`, `spent_wallet_inputs=S`, and `produced_wallet_outputs=[]`.

Consistency: Propagated the equation through outputs, wallet membership, recovery, records, cardinalities, acceptance criteria, and executable fixtures. Added the adjacent post-request client-disconnect clarification without reopening approved design.

Evidence: Verified sibling `availableUTxO` removes pending normal and collateral inputs without adding pending outputs; `totalUTxO` separately adds pending change and retains collateral-only inputs. Prettier and whitespace checks pass.

Outcome: The iteration-2 pending-overlay blocker is resolved with no remaining canonical-plan contradiction.

Critiquer: Iteration 3
Timestamp: 2026-08-19T17:37:15Z

Authorization: User-authorized narrow approval check after the normal two-pass planning guard.

Blocking findings:
None. The iteration-2 pending-overlay blocker is resolved.

Non-blocking observations:
None.

Approval bar:
Satisfied. The plan consistently defines `A = C - (N union K)`, `S = C - A`, `Q` from request inputs, and `O = A union Q`. It propagates these equations through `outputs` and `0x01` cardinality `|O|`, `spent_wallet_inputs=S`, `produced_wallet_outputs=[]`, pending `0x07` records, recovery requirements, acceptance criteria, and executable fixtures. This matches live sibling semantics: `availableUTxO` removes pending normal and collateral inputs without adding pending outputs, while `totalUTxO` separately adds pending change.

Decision: approved
