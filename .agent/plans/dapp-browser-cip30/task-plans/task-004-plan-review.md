Planner: Iteration 1
Timestamp: 2026-08-11T16:18:46Z

Plan scope: Created the canonical task-004 plan for exact transaction-body/output span evidence, complete Conway encoding coverage, fail-closed malformed CBOR, SDK evaluation, dependency disposition, and Dijkstra readiness. Production cursor and semantic model work remain with tasks 302 and 303.

Interaction mode: autonomous. No user inputs, manual validation, wallet, network, hardware, or external evidence are required.

Key evidence: Resolved and installed `@cardano-sdk/core@0.41.4` accepts trailing bytes, duplicate and unknown body keys, and tagged/untagged sets, so it cannot replace task-302's strict cursor. Dijkstra changes its envelope, body fields, guards, scripts, redeemers, witnesses, and protocol parameters and requires a separate fail-closed readiness disposition.

Expected artifacts: Narrow exact-CBOR fixtures and manifest, a test-local validation harness, `research/04-exact-cbor-era-coverage.md`, and concise PRD/tracker lifecycle updates. `package.json` and `yarn.lock` change only if an exact candidate closes a fixture-proven semantic gap.

Self-review: The plan covers exact body/output/witness/`isValid`/auxiliary spans, all valid Conway keys and shapes, tagged sets, duplicates, trailing bytes, unknown fields, protocol-parameter boundaries, SDK comparison, and Dijkstra readiness without introducing production abstractions or editing either review log. Formatting and whitespace checks pass.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-11T16:22:32Z

The plan has the correct fail-closed direction and downstream boundaries, but its acceptance policy, provenance proof, SDK comparison, and proposed corpus are not yet precise enough for a reproducible validation gate. Several requirements also expand the spike into partial implementations of tasks 302, 303, and 800.

Blocking findings:
1. **Critical: CBOR acceptance rules remain ambiguous and duplicate detection is byte-based rather than ledger-semantic.** The plan alternates between covering “malformed/indefinite containers” (`task-004.md:33`), walking indefinite containers (`task-004.md:137`), and rejecting only malformed/truncated indefinite forms (`task-004.md:151`) without fixing which valid indefinite encodings Conway accepts. Likewise, “non-minimal integers where ledger CDDL constrains them” does not identify the authoritative decoder/canonicality rule, and comparing exact encoded map keys or set members (`task-004.md:139`) misses semantically duplicate values represented by different legal encodings. Freeze an encoding-family acceptance table for definite/indefinite containers, minimal/non-minimal integers and lengths, tags including tag 258, embedded CBOR, map ordering, and permitted legacy forms. Add byte-distinct semantic duplicate vectors for inputs, required signers, scripts, datums, redeemers, assets, withdrawals, and governance maps, and assign semantic uniqueness enforcement explicitly to task 302 or 303 rather than treating encoded-byte equality as sufficient.
2. **High: positive fixture provenance and exact-hash evidence still permit circular or non-ledger-valid proof.** The plan allows locally reproduced fixtures and runs the authoritative ledger generator only “where feasible” (`task-004.md:147,198`), while its own risk section acknowledges that hand-built internally consistent values are not ledger-valid (`task-004.md:210`). `cbor-diag` proves CBOR structure, not ledger acceptance, and a test-local scanner validating offsets that it also discovers is not independent span evidence. Require every positive semantic fixture to identify immutable source bytes from a pinned ledger golden or a reproducible pinned-ledger serializer command/environment, with source commit/blob or artifact hash and exact extraction procedure. Require manifest offsets to be independently established and then asserted by the scanner. Body hashes must be checked against a ledger-produced transaction ID or a second true Blake2b-256 implementation, explicitly excluding Blake2b-512 truncation, which already caused incorrect evidence in task-003.
3. **High: the corpus and test-local scanner exceed the smallest truthful task-004 spike and blur tasks 302, 303, and 800.** Executably covering every certificate alternative, governance action, script/redeemer shape, nested duplicate domain, malformed container, canonicality rule, and Dijkstra delta (`task-004.md:142-155`) requires much of the semantic grammar and adversarial parser that tasks 303 and 800 own. At the same time, the task forbids implementing those production semantics (`task-004.md:40-42`). Narrow task-004 to provenance-backed exact envelope/body/output span vectors, one machine-checkable source-derived inventory of every Conway field and union alternative, and targeted SDK probes sufficient to identify concrete representation gaps. Keep exhaustive semantic recognition/commitment fixtures with task 303 and exhaustive differential/fuzz mutations with task 800. If executable coverage of every union alternative is retained here, remove the “small fixture spike” claim, define the authoritative oracle and realistic artifact/test structure, and update the effort and ownership consistently.
4. **High: the installed/candidate SDK comparison is not reproducibly executable as written.** The checked-in Jest spec naturally resolves installed `0.41.4`, while the candidate lives in an isolated `/tmp/opencode` dependency tree (`task-004.md:158-160,193-194`); no parameterized harness, module-resolution mechanism, or normalized result format explains how both versions run the same cases. Name the candidate-selection rule, require exact tarball URL/integrity/version/git identity, and define one reproducible harness that accepts an SDK module root and emits the same acceptance/preservation/semantic-coverage table for both versions. Tighten the update gate: an upgrade may occur only when a named required task-303 semantic gap is closed, all current SDK consumers compile and their focused tests pass, the candidate is compatible with the project Node/TypeScript/bundling constraints, and no consumer migration outside task-004 is required. Otherwise record the candidate result and retain the dependency.
5. **High: the supported-era and Dijkstra disposition can overstate evidence that task-004 cannot yet possess.** A “supported row” currently requires backend capability intersection (`task-004.md:133`), but phase-2 backend capabilities are not implemented, and production parsers from tasks 302/303 do not exist. Dijkstra also has known wallet TODOs and materially different semantics (`task-004.md:96-97`). Define separate statuses for wire fixture recognized, SDK represented, backend planned/implemented, production parser implemented, and product supported. Task-004 may conclude only conditional Conway readiness and Dijkstra `unsupported/readiness-blocked`; it must not produce a product-support claim. State the exact downstream evidence and tracker change required to promote Dijkstra later.
6. **High: protocol-parameter boundaries are described generically rather than assigned completely.** The plan says task-004 records affected fields and tasks 303/304 perform later validation (`task-004.md:85,187`), but it does not inventory fee parameters, max transaction/value sizes, min-UTxO, collateral percentage/count, execution prices/limits, cost models and language views, reference-script fees, deposits, governance parameters, protocol version, or Dijkstra additions. Add a matrix naming each context-dependent rule, its authoritative exact-chain-point parameter source, and its downstream task/test owner. State explicitly that task-004 performs no live-network policy claim and therefore does not fetch arbitrary “fresh” parameters; later runtime/integration evidence must use parameters captured for the intended network and chain point and preserve them with transaction artifacts.

Non-blocking observations:
- Exact incoming body hashing, full-root consumption, output slicing, unknown-field rejection, and preservation of outer envelope components are correctly treated as security evidence rather than SDK behavior.
- Keeping strict framing in task 302 regardless of SDK outcome is correct.
- The autonomous interaction mode is appropriate; Dijkstra remaining unsupported and retaining `0.41.4` are valid outcomes without user intervention.
- Conditional `package.json`/`yarn.lock` changes and prohibition of production parser/model files are good scope controls once the update gate is tightened.
- `Build Status: in_progress` is premature while implementation has not started.
- PRD, tracker, research, and lifecycle synchronization after implementation review is appropriate, and no architecture/API documentation update is needed for a validation-only spike.

Approval bar:
Revise the canonical plan to define authoritative Conway CBOR acceptance for indefinite and noncanonical encodings, semantic duplicate handling, and tags; make positive fixture provenance, independent span evidence, and true Blake2b-256 verification mandatory; reduce or explicitly reassign task-303/task-800 work; provide one reproducible installed/candidate SDK harness and a strict dependency-update gate; prevent task-004 from claiming implemented era support; and enumerate every protocol-parameter-dependent rule with an exact downstream owner. Approval does not require implementing production parsing, proving Dijkstra support, changing the SDK dependency, fetching live protocol parameters, or adding exhaustive downstream fuzz infrastructure.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-11T16:43:20Z

Plan revision: Revised only the canonical task-004 plan. Froze the Conway CBOR encoding-family acceptance table, ledger-proven fixture provenance, independent offsets and true Blake2b-256 checks, narrow corpus boundaries, and task-302/303/800 ownership.

SDK gate: Fixed candidate `@cardano-sdk/core@0.47.0` with exact identity and integrity, a reproducible module-root harness and normalized results, plus a strict no-migration dependency-update gate.

Readiness and context: Separated wire, SDK, backend, parser, and product statuses; limited outcomes to conditional Conway readiness and Dijkstra `unsupported/readiness-blocked`; added explicit promotion ownership and the complete exact-chain-point protocol-parameter rule matrix.

Lifecycle: Corrected build status to `not_started`. Direct Prettier and whitespace checks pass; neither review log was edited.

Outcome: All Critiquer iteration-1 blockers are resolved and the plan is ready for focused approval.

Critiquer: Iteration 2
Timestamp: 2026-08-11T16:45:37Z

The revision resolves the complete iteration-1 blocker list without expanding task-004 into production parsing or exhaustive downstream validation. Material claims were consistent with the live dependency lock, installed SDK source, sibling cardano-wallet era support, immutable Conway/Dijkstra CDDL, and the governing PRD/task ownership.

Blocking findings:
None.

Non-blocking observations:
- Pinned ledger package-to-source mapping remains an explicit implementation evidence gate, correctly blocking positive provenance if unresolved.
- Retaining SDK `0.41.4` and keeping Dijkstra readiness blocked remain valid outcomes.

Approval bar:
Satisfied. The plan now fixes the authoritative CBOR policy and semantic-duplicate ownership, mandates ledger-proven positive provenance and independent exact-span/true Blake2b-256 evidence, narrows task-004 relative to tasks 303/800, defines one reproducible installed/candidate SDK harness and no-migration update gate, separates readiness from product support, and assigns the complete exact-chain-point protocol-context matrix downstream.

Decision: approved

