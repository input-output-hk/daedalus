Planner: Iteration 1
Timestamp: 2026-08-20T20:35:21Z

Plan: Created canonical task-202 plan for backend-owned credential/path evidence, required wallet proofs, exact earlier-output resolution, sequential dependencies/conflicts, and 0x02/0x06 context binding. Implementation is autonomous and may proceed after planning approval; capability activation and pinning remain task-209 responsibilities.

Self-review: Passed scope-creep, workflow, manifests/tests/docs, trust/wire drift, phase-0 contract, interaction/evidence, and consistency checks. Focused Prettier and whitespace checks pass; both review logs remain untouched.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-20T20:41:45Z

Outcome: Plan requires one consolidated correction pass before implementation.

Blocking findings:
1. The earlier-output overlay is ledger-incorrect for isValid=false transactions. The plan indexes only ordinary outputs and treats normal and collateral inputs as unconditional spending claims. For an invalid transaction, ordinary outputs are not produced, collateral return may be produced at its ledger index, normal inputs are not consumed, and collateral inputs are. The published decoder exposes exact spans only for ordinary outputs, so the promised exact collateral-return overlay cannot be implemented in cardano-wallet alone. Freeze valid/invalid production and conflict rules, add exact collateral-return span evidence or an explicit fail-closed limitation consistent with the PRD, and account truthfully for any third-repository publication/review dependency instead of retaining a hidden human checkpoint.
2. Credential identity and derivation authority are contradictory for policy keys and incomplete for required signers. The plan first defines the mint policy ID, a script hash, as the policy credential, then calls the CIP-1855 key leaf an owned policy credential whose path must derive that exact credential. A CIP-1855 path derives the leaf key hash, not the policy script hash. Require separate script-policy and key-leaf ownership rows and specify which credential appears in each required-proof row. Explicit required signers are role-neutral and may match captured payment, stake, or stored policy keys; define deterministic cross-domain matching, including legitimate multiple domain matches, without arbitrary precedence.
3. Required-proof semantics are not implementation-ready. The plan excludes existing-witness verification while making required depend on valid existing VKey witnesses, which requires narrow exact-body signature and key-hash verification here. A key-removal test alone cannot express overall native-script satisfiability when the baseline is already unsatisfied. Freeze separately the complete producible-owned-key inventory, valid-existing-witness set, native-script satisfaction result used later by partialSign=false, and per-row required meaning. Specify whether mint-native-policy rows use native_script, policy, or both and how each boolean is calculated.
4. The unsupported Conway boundary is too vague to fail closed reliably. Enumerate the Conway certificate alternatives, voting-procedure voters, proposal-related credentials, pool/committee/DRep cases, Genesis/MIR forms, bootstrap/Byron-address inputs, and exact errors in an accepted/rejected matrix tied to pinned ledger constructors and tests. No credential-bearing field may pass because reduced wallet conversion labels or omits it.
5. Independent overlay validation is impossible at the named decoding seam because the response does not contain ordered request transactions and ordinary FromJSON receives no request. Define a request-aware validator assigned to backend assembly/tests and later Daedalus validation; keep plain JSON decoding structural. Otherwise unrecorded batch_overlay remains free metadata.
6. Equal-source dependency cardinality is ambiguous. Freeze whether equal earlier/pending/node availability emits one authoritative dependency or multiple rows, plus the exact uniqueness key and request-aware parity checks for identical duplicate parents, earlier-plus-pending equality, and pending-plus-node equality.
7. Exact path schemas remain underconstrained. Freeze allowed lengths, hardened prefixes, account index, role/index constraints, and account/policy-xpub derivation procedure for five-component CIP-1852 payment paths, fixed stake role 2/index 0, and three-component CIP-1855 policy paths. Prove private isOurs cannot authorize a path merely by returning it and no post-confirmation key/state read occurs.

Non-blocking observations:
1. Extending the single dormant context route, preserving caller order and duplicates, retaining exact source-byte comparison, and avoiding caches, persistence, activation, or pin changes are appropriately minimal.
2. Same-attempt checkpoint ownership analysis on a private discovery-state copy and an ownership-affecting mutation-tag audit are the correct clock model.
3. Privacy-safe fixed errors, unchanged capability 404, no-schema old-pin reopening, temporary exact-candidate Daedalus builds, consolidated task-209 review/pinning, and final documentation sequencing are well covered.
4. Keep exhaustive credential/script/overlay cases deterministic and use HTTP/mTLS for a small representative runtime proof unless runtime-only behavior requires more.

Approval bar:
1. Correct earlier-output production and spending-conflict rules for both outer validity outcomes, including exact collateral-return bytes/indexes and honest cross-repository interaction classification.
2. Freeze policy script versus policy key identities, role-neutral required-signer matching, exact derivation-path schemas, and exhaustive supported/unsupported credential constructors.
3. Define executable existing-witness, native-script satisfiability, producible-proof, and per-row requiredness semantics without conflicting with task-304 or task-203 ownership.
4. Add a request-aware overlay validator and remove the impossible claim that plain response JSON decoding can recompute request relationships.
5. Resolve dependency cardinality for every equal-provenance combination and propagate all decisions through research 03, Swagger, Haskell goldens, the Daedalus fixture, runtime evidence, and acceptance criteria.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-20T20:51:59Z

Revision summary: Revised only the canonical task-202 plan. Preserved the unchanged request, dormant capability, tracked pin, append-only review logs, and task-203/304/209 boundaries.

Blocker disposition:
1. Added an autonomous fail-closed isValid=true scope, exact invalid-transaction ledger semantics, collateral-return span limitation, and validity-aware consumption/conflict rules without third-repository work.
2. Separated policy script hashes from CIP-1855 leaf hashes and froze role-neutral, multi-domain signer/native-leaf matching.
3. Defined narrow existing-VKey verification, complete producible-key inventory, native-script satisfaction, aggregate satisfaction, per-row requiredness, and policy-only mint proof rules.
4. Added an exhaustive pinned-Conway credential constructor matrix covering certificates, voters, proposals, Genesis/MIR, and bootstrap inputs with exact failures.
5. Kept JSON parsing structural and assigned request-aware overlay validation to backend assembly/tests and the task-209 consumer seam.
6. Froze dependency uniqueness and cardinality for all earlier/pending/node equality combinations and duplicate parents.
7. Froze exact CIP-1852/CIP-1855 paths, captured-state derivation checks, private isOurs limits, generation rules, and prohibition of post-confirmation ownership reads.

Self-review: Propagated corrections through interaction mode, scope, fixed contract, expected files, implementation, acceptance, verification, risks, research/docs, and lifecycle. Corrected producer-removal semantics so existing witnesses remain available and verified valid-item conflicts derive only from prior normal consumption. Focused Prettier and whitespace checks pass; review logs remain untouched.

Outcome: Plan revised and ready for focused approval check

Critiquer: Iteration 2
Timestamp: 2026-08-20T20:54:20Z

Outcome: The focused approval check confirms that all seven Iteration 1 blockers are resolved and propagated through implementation, acceptance, verification, research, fixtures, Swagger, runtime evidence, and downstream task boundaries.

Blocking findings:
None.

Non-blocking observations:
1. Live task-201 source confirms ordinary-output spans only, ledger-accessible outer validity, captured checkpoint discovery/account/policy state, fixed CIP-1855 storage path, and structural-only response decoding, supporting the revised design.
2. The valid-only boundary avoids collateral-return parser or third-repository work while preserving exact invalid-transaction ledger truth.
3. Autonomous classification is truthful: all task-202 evidence is locally executable, while upstream review, activation, and pinning remain assigned to task-209.

Approval bar:
Satisfied. The plan freezes fail-closed all-true validity and validity-aware production/consumption; separates policy script hashes from CIP-1855 leaf hashes with role-neutral multi-domain matching; defines valid-existing-witness, complete producible-key, native-script, aggregate-satisfaction, and requiredness semantics; exhaustively gates pinned Conway credential-bearing constructors; separates structural parsing from request-aware overlay validation; fixes one authoritative dependency per uniqueness key for every equal-provenance combination; and specifies exact CIP-1852/CIP-1855 paths with captured-public-state derivation checks, private isOurs limits, mutation-clock coverage, and no post-confirmation ownership reads.

Decision: approved

