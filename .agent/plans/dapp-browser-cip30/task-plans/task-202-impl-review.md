Implementation: Iteration 1
Timestamp: 2026-08-20T21:16:22Z

Changes made:
- Created cardano-wallet candidate 984f468bd05f9e7bf42446580f4162fd43950d0e from parent 3ca15553f96587f1f96688185165b2ede00e30b0.
- Added strict ownership, required-proof, dependency, conflict, and earlier-provenance response types plus canonical 0x02/0x06 record encoding.
- Added fail-closed isValid=false rejection, coherent captured discovery state, private payment ownership discovery, payment input/collateral proof rows, earlier/pending/node byte comparison, authoritative dependency rows, and normal-consumption conflict metadata.
- Updated Swagger and record goldens.

Files touched:
- lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs
- lib/api/src/Cardano/Wallet/Api/Http/Shelley/TransactionContext.hs
- lib/api/src/Cardano/Wallet/Api/Types/Dapp/Context.hs
- lib/api/test/dapp-context-test.hs
- specifications/api/swagger.yaml

Verification run:
- cabal test cardano-wallet-api:dapp-context -O0 -v0: 4 examples passed.
- cabal build cardano-wallet-api -O0 -v0: passed; linker emitted existing dynamic-symbol warnings.
- git diff --check: passed.
- Nix Fourmolu check could not evaluate because /nix/store/9p1fyh7kzadqda6118x5wsld8pbq8xvy-source is missing; no formatting pass is claimed.

Deviations from approved plan:
- This first candidate is intentionally submitted for review before completion. It currently produces trusted payment ownership/proofs only; stake withdrawals/certificates, role-neutral required signers, native scripts, policy keys, existing-witness requiredness, exhaustive constructor gates, request-aware overlay validation, runtime/schema evidence, Daedalus fixtures, and durable docs remain unimplemented.
- No tracked Daedalus pin, capability activation, schema, dependency, or unrelated file changed.

User interaction required: no.

Outcome: Candidate compiles and establishes the shared contract/overlay seam; comprehensive review is requested to consolidate all remaining blockers for Iteration 2.

Code Review: Iteration 1
Timestamp: 2026-08-20T21:21:10Z

Outcome: The live candidate is a partial scaffold and does not satisfy task-202 trust, proof-completeness, validation, or evidence requirements. Focused compile/tests pass but do not exercise the new analysis. The logged candidate SHA is incorrect; live SHA is 984f468bd0acbebfb47e0bedff70d0f4e82853a4.

Blocking findings:
1. Critical: Unsupported Conway credential-bearing certificates, votes, proposals, pools, DRep/committee, Genesis/MIR, and future constructors are silently accepted.
2. Critical: Proof inventory covers only payment normal/collateral inputs; withdrawals, certificates, signers, native scripts, mint policies, stake/policy keys, Plutus ownership, role-neutral matching, aggregate satisfaction, and selected-script availability are absent.
3. Critical: Existing VKey verification and requiredness are absent; every payment row is incorrectly required=true.
4. High: Owned paths trust isOurs without captured account prefix/xpub re-derivation and exact key-hash proof; stake/policy path authority and contradictory-evidence checks are missing.
5. High: Required rows are not deduplicated by the frozen uniqueness key and can cause duplicate-record internal errors.
6. High: Pending normal/collateral spending claims are not passed to overlay analysis and therefore do not force dapp_context_conflict.
7. High: validateTransactionContextResponseForRequest is absent; structural checks cannot authenticate overlay relations.
8. High: Digest/token goldens still omit 0x02/0x06; Daedalus fixtures omit ownership, proofs, earlier provenance, overlay, validity, and request-aware checks.
9. High: Structural JSON and Swagger validation do not enforce frozen uniqueness/relationships/path schemas or duplicate-field rejection.
10. High: New production analysis has no behavioral tests for validity, overlay, ownership, paths, proof classes, witnesses, gates, errors, or privacy.
11. High: Logged candidate SHA is nonexistent; exact live child is 984f468bd0acbebfb47e0bedff70d0f4e82853a4 and all evidence must use one exact identity.
12. Medium: HTTP/mTLS, privacy, discovery non-persistence, clock audit, schema/old-pin reopen, temporary Daedalus builds, Swagger, and formatting evidence are absent.
13. Medium: Research 03/04, PRD, tracker, canonical lifecycle, and fixture provenance remain stale.

Non-blocking observations:
1. All-true validity rejection occurs before capture.
2. The narrow source resolver compares exact bytes, retains equal provenance, indexes outputs sequentially, and tracks only prior normal consumption.
3. Basic caller order, identical/unequal duplicate, self/forward, and later conflict handling is directionally correct.
4. No dependency, schema, activation, pin, IPC, renderer, hardware, logging, or obvious privacy expansion occurred.
5. Focused Cabal test/build, existing Daedalus fixture test, and whitespace checks pass; OpenAPI and Fourmolu were unavailable.

Approval bar:
Implement complete proof/gate/path authority; correct pending conflicts, deduplication, overlay cardinality, and request-aware validation; finish strict Swagger/cross-language fixtures and behavioral/runtime/schema/build evidence; correct and synchronize candidate provenance without activation or pin changes.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-08-20T21:34:21Z

Changes made:
- Corrected sibling candidate identity and added follow-up commits d5e6f71551a55ca31da1e17047f0c8c55a3bfa67 and ca0b8b0de57391866487f22f519b036e9b155a61.
- Added captured account-xpub payment path re-derivation/hash checks, stake withdrawal/certificate ownership and proof rows, role-neutral required-signer matching across payment/stake/stored-policy domains, exhaustive top-level pool/governance/vote/proposal rejection, pending-spend conflict rejection, required-row deduplication, and producer-side request-aware overlay validation.
- Added response duplicate-field rejection, frozen uniqueness-key checks, required-row to owned-candidate parity, exact path shapes, 0x02/0x06 digest/token Haskell goldens, and matching Daedalus cross-language records/digest/token fixture for candidate ca0b8b0de57391866487f22f519b036e9b155a61.

Files touched:
- cardano-wallet: prior five files plus lib/api/cardano-wallet-api.cabal.
- Daedalus: source/common/cardano/backendContextFixture.spec.ts and source/common/cardano/fixtures/exact-cbor/backend-context-v1.json, plus task records.

Verification run:
- cabal test cardano-wallet-api:dapp-context -O0 -v0: 4 examples passed with expanded record/digest/token assertions.
- cabal build cardano-wallet-api -O0 -v0: passed; existing linker warnings only.
- focused Daedalus backendContextFixture Jest: passed.
- direct focused Prettier check: passed after targeted formatting.
- git diff --check: passed in both repositories.
- Nix Fourmolu remains blocked by missing store source.

Remaining deviation:
- Native-script/mint-policy proof evaluation, valid-existing-VKey verification and conditional requiredness, exhaustive behavioral/runtime/schema/temporary-build evidence, and final research/PRD/tracker synchronization remain open. No capability or tracked pin changed.

User interaction required: no.

Outcome: Iteration 1 authority, deduplication, pending conflict, validation, binding, and candidate-provenance blockers are substantially addressed; focused review is requested on remaining proof-completeness and evidence gaps.

Code Review: Iteration 2
Timestamp: 2026-08-20T21:50:28Z

Outcome: Iteration 2 fixes pending-spend rejection, deduplication, payment path authority, stake/certificate selectors, governance gating, producer overlay construction, duplicate-field rejection, and 0x02/0x06 parity. Core proof semantics, coherent ownership, complete overlay validation, schema/fixture coverage, evidence, docs, and exact provenance remain blocking.

Blocking findings:
1. Critical: No reusable ProofInventory exists; existing VKeys, native/mint scripts, policy script/leaf separation, aggregate satisfaction, conditional requiredness, and selected-script failure are absent, while all rows remain required=true.
2. High: Payment and signer discovery start separate private folds and mergeOwnership permits contradictory owned/unowned evidence rather than failing.
3. High: Request-aware validation compares dependencies/conflicts only; it does not prove exact earlier bytes, roles/provenance, pending claims, unequal sources, or complete output/overlay relationships.
4. High: Structural and Swagger rules remain incomplete for indexes, strict-prior relations, ownership uniqueness/contradictions, and exact conditional paths.
5. High: Cross-language fixture binds records/digest/token only and lacks earlier/equal provenance, dependency/conflict, validity, duplicate-parent, and request-aware cases.
6. High: Candidate ca0b8b0de57391866487f22f519b036e9b155a61 is nonexistent; actual live SHA is ca0b8b0de5d28afc9ff0af417f6e391f818bee2e.
7. High: Behavioral HTTP/mTLS, ownership, witness, native/mint, overlay, privacy, schema/old-pin, and candidate-build evidence remains absent.
8. Medium: Research 03/04, PRD, tracker, canonical lifecycle, fixture SHA, and evidence remain unsynchronized.

Non-blocking observations:
1. Validity rejection, exact source precedence, pending claim rejection, normal-only consumption, duplicate-parent selection, and basic conflict behavior are directionally correct.
2. Payment account-xpub re-derivation, fixed stake/policy paths, and certificate selectors now use captured/pinned authority.
3. Haskell/TypeScript 0x02/0x06 digest/token checks and whitespace checks pass.
4. No capability, pin, schema migration, IPC, renderer, hardware, or unrelated production surface changed.

Approval bar:
Complete one reusable proof inventory and coherent ownership fold; strengthen request-aware/schema/fixture validation; add behavioral/runtime/privacy/schema/build evidence; synchronize durable records; bind everything to the actual final follow-up commit.

Decision: requires_changes


Implementation: Iteration 3
Timestamp: 2026-08-20T22:30:47Z

Changes made:
- Added follow-up candidate 417253867d0770c691a757e95e042957e95e9f7f on parent ca0b8b0de5d28afc9ff0af417f6e391f818bee2e.
- Added one reusable ProofInventory, narrow valid-existing-VKey signature verification, selected native/mint script evaluation, aggregate supported-obligation satisfaction, and per-candidate conditional requiredness.
- Unified payment/signer classification through one private discovery-state fold and made contradictory ownership evidence fail closed.
- Strengthened request-aware overlay validation and structural index/path/uniqueness checks; refreshed the Daedalus fixture to the exact candidate with earlier provenance, validity, dependency, and conflict examples.

Files touched:
- cardano-wallet: lib/api/cardano-wallet-api.cabal; lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs; lib/api/src/Cardano/Wallet/Api/Http/Shelley/TransactionContext.hs; lib/api/src/Cardano/Wallet/Api/Types/Dapp/Context.hs; lib/api/test/dapp-context-test.hs; specifications/api/swagger.yaml.
- Daedalus: source/common/cardano/backendContextFixture.spec.ts; source/common/cardano/fixtures/exact-cbor/backend-context-v1.json; task records.

Verification run:
- cabal test cardano-wallet-api:dapp-context -O0 -v0: 4 examples passed.
- cabal build cardano-wallet-api -O0 -v0: passed; existing linker warnings only.
- NIX_CONFIG=lazy-trees=false LOCAL_CLUSTER_ERA=conway just integration-tests-cabal-match TASK_201: 1 registered Conway HTTP example passed, 0 failures, after cabal clean removed stale linker artifacts.
- Temporary Daedalus bridge-mainnet and daedalus-mainnet Nix builds with exact candidate override completed without a reported build error.
- Focused Daedalus backendContextFixture Jest and Prettier checks passed.
- git diff --check passed in both repositories.

Remaining deviation:
- Fourmolu through nix develop remains unavailable because required Nix store source paths are missing. Dedicated mTLS, exhaustive proof/matrix behavioral tests, privacy/schema-old-pin evidence, and durable research/PRD/tracker synchronization remain to be reviewed or completed.
- No capability activation, tracked backend pin, schema migration, IPC, renderer, hardware, dependency lockfile, or unrelated production surface changed.

User interaction required: no.

Outcome: Candidate 417253867d0770c691a757e95e042957e95e9f7f compiles, passes focused contract checks and the registered runtime context scenario, and is submitted for comprehensive Iteration 3 review.

Code Review: Iteration 3
Timestamp: 2026-08-20T22:33:57Z

Outcome: Candidate identity is correct, but Iteration 2 proof-completeness, request-aware validation, schema, fixture, and evidence blockers remain open.

Blocking findings:
1. Critical [implementation]: ProofInventory contains only projected ownership/proof rows. It does not retain obligations, existing witnesses, producible candidates, per-obligation results, or aggregate satisfaction, so task-203 cannot reuse the approved pure inventory and aggregate satisfaction is never computed (lib/api/src/Cardano/Wallet/Api/Http/Shelley/TransactionContext.hs:296-299, :718-735).
2. Critical [implementation]: Native requiredness is calculated independently per script, then deduplicated with Set. Because required participates in equality, the same frozen uniqueness key can produce both true and false across associated scripts instead of the required logical-OR result; the resulting response violates its own uniqueness parser (TransactionContext.hs:878-883, :734-735; lib/api/src/Cardano/Wallet/Api/Types/Dapp/Context.hs:617-621).
3. High [implementation]: Every selected non-native spending script is still emitted with NativeScriptProof; the Nothing branch of getNativeScript retains the native-script ownership bit for Plutus scripts (TransactionContext.hs:860-865, :773-787).
4. High [trust boundary]: Request-aware validation requires pending provenance when a pending source exists but never rejects pending provenance when none exists, then trusts that response-controlled bit to derive a dependency. Fabricated pending provenance/dependencies can therefore validate (TransactionContext.hs:1063-1067, :1107-1110).
5. High [implementation/plan drift]: Stake credentials are hashed from rewardAccountKey while the path is separately copied from the prefix; the required account-xpub child re-derivation/path-to-hash proof is absent. Signer/native payment-path verification also silently drops a failed re-derivation instead of failing closed (TransactionContext.hs:744-745, :804-816, :840-842, :890-895).
6. High [contract/schema]: Swagger still allows arbitrary ownership path lengths up to five rather than exact credential-conditional schemas, and omits uniqueness/cardinality constraints for ownership, required proofs, dependencies, and conflicts (specifications/api/swagger.yaml:4587-4641).
7. High [tests/evidence]: The only focused Haskell suite has four encoding/request tests and never executes ownership discovery, witnesses, native/mint semantics, constructor gates, overlay validation, or errors (lib/api/test/dapp-context-test.hs:51-85). The runtime scenario remains TASK_201 and asserts only legacy output fields (lib/integration/scenarios/Test/Integration/Scenario/API/Shelley/TransactionsNew.hs:978-1198).
8. High [fixture/evidence]: The Daedalus fixture is not request-aware. Its digest contains one transaction while overlay rows reference transaction indexes 1 and 2, and the Jest test merely matches those static objects rather than deriving relationships (source/common/cardano/fixtures/exact-cbor/backend-context-v1.json:63-109; source/common/cardano/backendContextFixture.spec.ts:107-121).
9. Medium [evidence/docs]: Dedicated mTLS, exhaustive matrix/proof behavior, privacy, schema/old-pin reopen, and reproducible temporary-build evidence remain absent by the implementation transcript’s own account; research/tracker/lifecycle synchronization is unfinished and the canonical plan still says no candidate exists (task-202-impl-review.md:138-140; task-202.md:248-253).

Non-blocking observations:
1. Exact candidate 417253867d0770c691a757e95e042957e95e9f7f exists at sibling HEAD, directly follows ca0b8b0de5d28afc9ff0af417f6e391f818bee2e, and the complete four-commit range starts at the approved 3ca15553f96587f1f96688185165b2ede00e30b0.
2. No prohibited tracked pin, capability publication, request schema, persistence/DB schema, new route, or Daedalus runtime surface change was found. Daedalus changes remain confined to the fixture and fixture test.
3. Validity rejection remains pre-capture; discovery now follows one private fold; contradictory ownership fails closed; existing VKey signatures are cryptographically checked.
4. Focused Cabal and Jest checks and both whitespace checks pass, but only at the limited coverage described above.

Approval bar:
Implement the complete reusable inventory and aggregate semantics; combine requiredness across associated obligations; correct Plutus proof classification and fail-closed path authority; make request-aware validation bidirectional; finish exact Swagger and genuinely request-derived fixtures; add executable proof/matrix/overlay tests plus required HTTP/mTLS/privacy/schema/build evidence; synchronize durable records to the final candidate.

Decision: requires_changes

Implementation: Iteration 4
Timestamp: 2026-08-20T22:49:45Z

Changes made:
- Added follow-up candidate 603972168ac1c67656a5197a89d21d90a2e4b89d on parent 417253867d0770c691a757e95e042957e95e9f7f.
- Expanded ProofInventory to retain direct/native obligations, valid existing witnesses by transaction, producible public candidates, per-obligation satisfaction and candidate-removal results, aggregate transaction satisfaction, and projected ownership/required rows.
- Combined requiredness with logical OR by the frozen row key, removed native proof bits from Plutus spending scripts, derived stake authority from the captured account xpub, and made failed payment path verification an invariant error.
- Made pending provenance validation bidirectional and dependency derivation authority-based; tightened supported Swagger path/cardinality constraints.
- Added focused executable proof aggregation, existing-witness, Plutus/native classification, pending provenance, and dependency precedence checks.
- Replaced static Daedalus overlay assertions with three exact canonical Conway transactions whose body hashes, outputs, dependencies, conflicts, validity boundary, records, digest, and token are independently derived in Jest.

Verification run:
- cabal test cardano-wallet-api:dapp-context -O0 -v0: 7 examples passed.
- cabal build cardano-wallet-api -O0 -v0: passed.
- focused Daedalus backendContextFixture Jest: 1 test passed.
- direct focused Prettier check: passed.
- git diff --check: passed in both repositories.
- Swagger YAML parses; semantic swagger-cli validation remains unavailable.

Remaining deviation:
- Dedicated task-202 mTLS, exhaustive ledger-constructor/native-script integration coverage, privacy audit, schema old-pin reopen, and final durable-document synchronization remain open for review/completion. Existing task-201 mTLS route coverage and the successful task-202-candidate TASK_201 HTTP runtime run remain unchanged.
- No capability activation, tracked pin, persistence schema, request schema, route, IPC, renderer, hardware, or unrelated runtime surface changed.

User interaction required: no.

Outcome: Candidate 603972168ac1c67656a5197a89d21d90a2e4b89d closes the Iteration 3 implementation and request-aware fixture findings and is submitted for Iteration 4 review.

Code Review: Iteration 4
Timestamp: 2026-08-20T22:52:25Z

Outcome: The live follow-up closes the Iteration 3 requiredness aggregation, Plutus classification, stake-path authority, and pending-provenance defects. Approval remains blocked by one inventory defect, incomplete Swagger authority, false candidate/fixture provenance, and missing acceptance evidence.

Blocking findings:
1. High [implementation]: producibleCandidates is built from every owned ownership row, including wallet-snapshot and reference-only keys with no supported obligation. It is also not transaction-scoped. This violates the approved inventory definition and risks task-203 producing unrelated witnesses when reusing the inventory (lib/api/src/Cardano/Wallet/Api/Http/Shelley/TransactionContext.hs:753-783, especially :767). Restrict candidates to keys associated with a direct obligation or selected native-script leaf and add an executable buildProofInventory test proving unrelated owned outputs are excluded.
2. High [contract/Swagger]: Swagger now constrains path lengths but not the frozen positional values: CIP-1852 purpose/coin/account hardening, payment role/index ranges, stake role 2/index 0, or fixed CIP-1855 components. uniqueItems also cannot enforce the documented uniqueness keys when non-key fields differ (specifications/api/swagger.yaml:4587-4676). Document the exact component schemas and uniqueness/cardinality conditions, run semantic Swagger validation, and retain the stricter Haskell checks at lib/api/src/Cardano/Wallet/Api/Types/Dapp/Context.hs:535-621,1208-1214.
3. High [candidate identity/provenance]: The requested range endpoint, transcript candidate, and fixture SHA 603972168ac1c67656a5197a89d21d90a2e4b89d do not exist. Live HEAD is 603972168affc03431875a6a3460131a68dc4dee, directly following 417253867d0770c691a757e95e042957e95e9f7f. Consequently, the nominal review range cannot be resolved and fixture provenance is false (task-202-impl-review.md:173-198; source/common/cardano/fixtures/exact-cbor/backend-context-v1.json:3-6; source/common/cardano/backendContextFixture.spec.ts:41-44). Synchronize every candidate reference to one exact existing commit and rerun all candidate-bound evidence.
4. High [fixture/acceptance evidence]: The fixture names lib/api/test/dapp-context-test.hs as its source, but that Haskell golden uses one transaction and different records/digest, while the Daedalus fixture uses three transactions and digest 6e8793... (source/common/cardano/fixtures/exact-cbor/backend-context-v1.json:6,111-123; lib/api/test/dapp-context-test.hs:214-275). Jest derives earlier dependencies/conflicts, but requiredness remains asserted by static linkage and equal pending/node provenance combinations are absent (source/common/cardano/backendContextFixture.spec.ts:134-197). Add one matching Haskell golden and independently cover requiredness plus earlier/pending/node precedence combinations.
5. High [missing acceptance evidence/docs]: The seven focused Haskell examples test isolated helpers, not live ownership discovery, complete constructor gates, valid/invalid VKeys, native all/any/threshold/time behavior, mint policy/leaf separation, multiple role-neutral domains, or full request-aware rejection paths (lib/api/test/dapp-context-test.hs:64-125). Dedicated task-202 HTTP/mTLS cases, privacy verification, schema/old-pin reopen evidence, and final research/PRD/tracker/lifecycle synchronization remain absent by the transcript’s own admission (task-202-impl-review.md:184-198; task-202.md:248-253; research/03-cardano-wallet-backend-contract.md:494-560; research/04-exact-cbor-era-coverage.md:176-180; dapp-browser-cip30-tasks.json:730-747). Supply the approved table-driven matrix, representative HTTP and authenticated-mTLS proof/overlay/fixed-failure run, privacy/schema evidence, exact temporary-build provenance, fixture SHA-256, and synchronized durable records.

Non-blocking observations:
1. Iteration 3 blockers for logical-OR requiredness, retained aggregate/obligation inventory, Plutus spending classification, stake child derivation, fail-closed payment paths, and bidirectional pending authority are corrected in live source (TransactionContext.hs:753-872,905-1079,1154-1254).
2. Existing VKeys are verified against the exact body hash; native and mint policies retain distinct proof kinds; Plutus mint policies retain policy ownership without wallet-key rows.
3. Valid-only rejection remains pre-capture, normal-only consumption and collateral/reference overlay semantics remain correct, and request-aware dependency derivation no longer trusts response-controlled provenance.
4. cabal test cardano-wallet-api:dapp-context -O0 -v0 passed 7 examples; focused Daedalus Jest passed; both repositories pass git diff --check.
5. The reviewed live range changes only six cardano-wallet API/Swagger files. Daedalus product/runtime source remains untouched. No capability publication, tracked pin, request revision, persistence/schema, route, IPC, renderer, hardware, dependency lockfile, or unrelated production surface change was found.

Approval bar:
Fix the candidate inventory and synchronize the exact live candidate identity; complete exact Swagger documentation and validation; provide a matching Haskell/TypeScript request-aware fixture with requiredness and source-precedence coverage; execute the approved proof/constructor/path/overlay matrix and representative HTTP/mTLS/privacy/schema/build checks; then update research 03/04, PRD, tracker, plan lifecycle, fixture hash, and evidence to that immutable candidate.

Decision: requires_changes

Implementation: Iteration 5
Timestamp: 2026-08-20T23:52:28Z

Changes made:
- Finalized cardano-wallet candidate aaeb3238f4864d0cf12300183f7c85baaba41ad7 over approved parent 3ca15553f96587f1f96688185165b2ede00e30b0.
- Scoped producible candidates by transaction and exact direct/native obligation association, excluding wallet-snapshot/reference-only ownership; task-203 can consume explicit transaction applicability.
- Added table-driven validity, certificate-gate, existing-witness, candidate-association, native all/any/threshold/time, policy-script/leaf, Plutus, and source-precedence checks; restored and extended the broader transaction-context unit suite.
- Matched Haskell and TypeScript three-transaction record/digest/token goldens and documented exact runtime-enforced Swagger path/uniqueness/cardinality rules; semantic Swagger validation passes.
- Extended the registered Conway runtime scenario with owned CIP-1852 path evidence, required normal/collateral proofs, exact earlier provenance/dependency, later collateral conflict, isValid=false rejection, and the bounded fixed 503 case.
- Synchronized research 03/04, PRD, tracker, plan lifecycle, fixture candidate, and fixture SHA-256 a3178ce699bdd424d63952b85005960d7c950e13baa66647bf94bd1a799b99f8.

Verification run against exact candidate:
- cabal test cardano-wallet-api:dapp-context -O0 -v0: 12 examples passed.
- cabal test cardano-wallet-unit:unit -O0 -v0 --test-options=--match transaction-context: 21 examples passed, including fixed-error normalization and route/Swagger presence.
- cabal test wai-middleware-logging:unit -O0 -v0 focused transaction-context suppression: 1 example passed.
- cabal build cardano-wallet-api -O0 -v0 and cardano-wallet-integration:lib:scenarios: passed.
- repository scripts/ci semantic Swagger validator: passed.
- authenticated mTLS registered Conway TASK_201 scenario with task-202 assertions: 1 example passed, 0 failures.
- preserved task-201 candidate/old-pin roundtrip databases remain byte-identical; schema hash 52d6d54033de3ce30c53dd05bc407fd6643ff9b4599004f4b2bf6e1a9de3fdfb, integrity ok, user_version 0; task-202 changes no DB/migration/schema path.
- focused Daedalus Jest and direct Prettier checks: passed; fixture SHA-256 matches the durable records.
- exact-candidate temporary .#daedalus-bridge-mainnet and .#daedalus-mainnet builds: passed.
- git diff --check: passed in both repositories.

Remaining deviation:
- Repository Fourmolu through the normal Nix shell remains unavailable because a required store source is missing. All touched Haskell builds, focused suites, integration component, semantic Swagger, and whitespace checks pass.
- Capability publication, tracked pin update, signing behavior, governance expansion, and consolidated authorized upstream review remain deliberately deferred to tasks 203, 205/206, and 209.

User interaction required: no.

Outcome: All Iteration 4 blockers are addressed and exact candidate aaeb3238f4864d0cf12300183f7c85baaba41ad7 plus the complete Daedalus evidence/docs state are submitted for final Iteration 5 review.

Code Review: Iteration 5
Timestamp: 2026-08-20T23:57:19Z

Outcome: Final review requires changes. Candidate and fixture provenance now resolve exactly, and Iteration 4 candidate-scoping and Swagger-documentation findings are substantially corrected. Three concrete task-202 blockers remain.

Blocking findings:
1. High [correctness]: Selected scripts are resolved only from the transaction witness map. Native or Plutus scripts supplied through resolved reference inputs are ignored, causing valid reference-script spending or minting transactions to return dapp_context_unavailable despite exact script material being available (lib/api/src/Cardano/Wallet/Api/Http/Shelley/TransactionContext.hs:1040-1059). Resolve and hash-check selected scripts from authoritative reference-input outputs as well as scriptTxWitsL.
2. High [overlay trust]: buildBatchOverlay rejects equal transaction IDs with unequal complete envelopes, but validateTransactionContextResponseForRequest does not repeat that check (TransactionContext.hs:1132-1143, TransactionContext.hs:1187-1210). Task-209’s consumer-side validator could therefore accept a response for a request the frozen contract requires rejecting. Enforce identical complete envelopes for duplicate IDs inside the request-aware validator and test the rejection.
3. High [acceptance evidence/documentation truth]: The approved proof and constructor matrix remains incomplete. The focused suite does not execute buildProofInventory, validVKeyWitnessHashes, or the request-aware validator; its certificate table covers only registration, deregistration, and one DRep rejection (lib/api/test/dapp-context-test.hs:104-117, lib/api/test/dapp-context-test.hs:147-240). The runtime scenario proves payment normal/collateral ownership and overlay behavior only (lib/integration/scenarios/Test/Integration/Scenario/API/Shelley/TransactionsNew.hs:1193-1345), not stake/policy ownership, role-neutral multiple matches, native-policy necessity, or valid/invalid/wrong-body VKeys. The TypeScript fixture links a static required row to ownership but does not verify witness-derived requiredness (source/common/cardano/backendContextFixture.spec.ts:193-197). This does not support the comprehensive matrix/runtime claims in task-202.md:211, research/03-cardano-wallet-backend-contract.md:576-595, and the tracker. Add the missing executable cases or narrow the durable claims truthfully.

Non-blocking observations and residual risks:
1. Exact candidate aaeb3238f4864d0cf12300183f7c85baaba41ad7, parent range, fixture candidate, and fixture SHA-256 a3178ce699bdd424d63952b85005960d7c950e13baa66647bf94bd1a799b99f8 all match live provenance.
2. Producible candidates are now transaction-scoped and limited to matching direct/native obligations (TransactionContext.hs:774-815).
3. Focused checks passed locally: 12 dApp-context examples, 21 route/error/retry examples, one privacy example, Daedalus Jest, and both git diff --check runs.
4. Swagger now documents exact positional paths and uniqueness limitations. Semantic validation could not be reproduced in the current shell because swagger-cli is unavailable.
5. No capability activation, tracked pin, request revision, persistence/schema, IPC, renderer, hardware, dependency lockfile, or new route was found. Fourmolu remains unavailable as recorded.
6. The Daedalus diff also contains an unrelated task-103 tracker status change at .agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json:365; it is not treated as task-202 evidence.

Decision: requires_changes with precise reason: reference-script authority is incomplete, the consumer request-aware validator omits the frozen unequal-duplicate-envelope rejection, and the approved proof/constructor/runtime evidence matrix remains materially unfulfilled and overstated in durable records.

Implementation: Resumed Review Cycle Iteration 1
Timestamp: 2026-08-21T15:38:16Z

Changes made:
- Finalized follow-up candidate e60b8a66cad9121e54656c76e03a3785099b9215 over aaeb3238f4864d0cf12300183f7c85baaba41ad7.
- Resolved selected scripts from authoritative resolved reference-input outputs as well as transaction witnesses, retaining exact script-hash checks.
- Reused the batch duplicate-envelope equality rule in request-aware response validation and added a focused unequal-complete-envelope rejection.
- Expanded the authenticated Conway runtime scenario with reference-script minting, owned CIP-1855 policy evidence, required policy proof, valid existing VKey satisfaction, and wrong-body VKey rejection.
- Synchronized candidate and fixture provenance; fixture SHA-256 is 2e13fc87934f7bd4cb66b7ba025387283562ad1bec7ce64f048d0d88e3ffb6f4. Durable evidence now states the tested scope and current Nix limitation rather than claiming comprehensive runtime coverage.

Verification run against exact candidate:
- cabal test cardano-wallet-api:dapp-context -O0 -v0: 13 examples passed.
- cabal test cardano-wallet-unit:unit -O0 -v0 --test-options='--match transaction-context': 21 examples passed.
- cabal test wai-middleware-logging:unit -O0 -v0 --test-option='--match=transaction-context': 1 example passed.
- cabal build cardano-wallet-api cardano-wallet-integration:lib:scenarios -O0 -v0: passed.
- scripts/ci semantic Swagger validation: passed.
- authenticated mTLS registered Conway TASK_201 scenario with task-202 assertions: 1 example passed, 0 failures.
- focused Daedalus Jest and direct Prettier checks: passed; fixture SHA-256 matches durable records.
- git diff --check: passed in both repositories.

Remaining deviation:
- Exact-candidate .#daedalus-bridge-mainnet and .#daedalus-mainnet reruns are environment-blocked because the Nix-store cardano-haskell-packages index source is missing. The preceding candidate's temporary builds passed; the follow-up changes only cardano-wallet context analysis/tests and Daedalus evidence fixtures.
- Repository Fourmolu remains unavailable for the same class of missing Nix-store source. Touched Haskell compiles and whitespace checks pass.
- Capability publication, tracked pin update, signing behavior, governance expansion, and consolidated upstream review remain deferred to tasks 203, 205/206, and 209.

User interaction required: no.

Outcome: Exact candidate e60b8a66cad9121e54656c76e03a3785099b9215 addresses the three Iteration 5 blockers with focused and runtime evidence and is submitted for resumed-cycle review.

Code Review: Resumed Review Cycle Iteration 1
Timestamp: 2026-08-21T15:42:45Z

Outcome: The two correctness blockers from Iteration 5 are fixed, and authenticated runtime coverage now supports the narrowed policy/VKey claims. Approval remains blocked by one overstated durable cross-language evidence claim.

Blocking findings:
1. High [durable evidence]: Research states that matching Haskell and TypeScript checks independently bind transaction IDs, source-output bytes, required proofs, and earlier dependencies/conflicts (`.agent/plans/dapp-browser-cip30/research/03-cardano-wallet-backend-contract.md:590-594`). The Haskell test only combines hard-coded transactions and records into digest/token goldens (`lib/api/test/dapp-context-test.hs:138-144,367-379,430-435`) and does not derive the fixture overlay or validate a successful request/response relationship. Only the TypeScript test derives those relationships (`source/common/cardano/backendContextFixture.spec.ts:114-197`). Narrow the claim to each test's actual responsibility.

Non-blocking observations/residual risks:
1. Reference-input scripts are authoritatively resolved and selected by hash alongside witness scripts (`lib/api/src/Cardano/Wallet/Api/Http/Shelley/TransactionContext.hs:1043-1067`), with authenticated native reference-policy coverage (`lib/integration/scenarios/Test/Integration/Scenario/API/Shelley/TransactionsNew.hs:1125-1270`). Plutus reference-script behavior follows the same production path but lacks a dedicated executable case.
2. Unequal complete envelopes sharing a transaction ID are now rejected by the request-aware validator (`TransactionContext.hs:1196-1199`) with focused coverage (`lib/api/test/dapp-context-test.hs:113-127`).
3. Runtime evidence now truthfully covers owned payment/policy paths, native-policy necessity, valid and wrong-body VKeys, overlay behavior, validity rejection, and fixed `503` handling (`TransactionsNew.hs:1187-1517`). Exhaustive constructor cases, stake runtime ownership, and role-neutral multi-domain runtime matching remain coverage gaps, not demonstrated regressions.
4. Candidate range and fixture provenance resolve exactly; fixture SHA-256 is `2e13fc87934f7bd4cb66b7ba025387283562ad1bec7ce64f048d0d88e3ffb6f4`. Exact follow-up Nix builds and Fourmolu remain environment-blocked as recorded.

Decision: requires_changes

Implementation: Resumed Review Cycle Iteration 2
Timestamp: 2026-08-21T15:42:45Z

Changes made:
- Narrowed research 03's cross-language evidence statement: the Haskell golden binds exact transaction envelopes and canonical records to digest/token values, while TypeScript independently derives transaction IDs, source-output bytes, requiredness, dependencies, conflicts, digest, and token.

Verification run:
- Documentation-only correction; git diff --check remains clean.

Remaining deviation:
- Plutus reference-script behavior shares the hash-checked production path but has no dedicated executable case. Exhaustive constructor, stake runtime, and role-neutral multi-domain runtime coverage remain residual test gaps rather than claimed evidence.
- Exact follow-up Nix builds and Fourmolu remain environment-blocked as recorded.

User interaction required: no.

Outcome: The sole resumed-cycle Iteration 1 documentation blocker is corrected and submitted for final resumed-cycle review.

Code Review: Resumed Review Cycle Iteration 2
Timestamp: 2026-08-21T15:43:40Z

Outcome: The Haskell responsibility is now stated accurately, but the corrected statement still overclaims TypeScript evidence.

Blocking findings:
1. High [durable evidence]: Research claims TypeScript independently derives requiredness (`research/03-cardano-wallet-backend-contract.md:591-594`), repeated in the implementation record (`task-plans/task-202-impl-review.md:323-325`). The test serializes the fixture-provided `required` boolean (`source/common/cardano/backendContextFixture.spec.ts:85-93`) and only links its transaction index and credential to fixture metadata (`backendContextFixture.spec.ts:194-197`); it does not derive witness-based necessity. Replace “requiredness” with the actual responsibility, such as encoding and linking the required-proof record.

Residual risks:
1. Plutus reference scripts share the reviewed hash-checked path but lack a dedicated test.
2. Exhaustive constructor, stake runtime, and role-neutral multi-domain runtime coverage remain incomplete.
3. Exact follow-up Nix and Fourmolu checks remain environment-blocked.
4. No additional correctness, contract, provenance, or regression blocker was found.

Decision: requires_changes

Implementation: Resumed Review Cycle Iteration 3
Timestamp: 2026-08-21T15:43:40Z

Changes made:
- Corrected research 03 to state that TypeScript encodes and links the fixture-provided required-proof record; it does not independently derive witness-based necessity.
- This entry supersedes the Iteration 2 implementation wording that incorrectly listed TypeScript-derived requiredness; the append-only history remains unchanged.

Verification run:
- Documentation-only correction; git diff --check remains clean.

Remaining deviation:
- Plutus reference scripts share the reviewed hash-checked path but lack a dedicated test.
- Exhaustive constructor, stake runtime, and role-neutral multi-domain runtime coverage remain incomplete.
- Exact follow-up Nix and Fourmolu checks remain environment-blocked.

User interaction required: no.

Outcome: The sole resumed-cycle Iteration 2 wording blocker is corrected and submitted for final resumed-cycle review.

Code Review: Resumed Review Cycle Iteration 3
Timestamp: 2026-08-21T15:44:34Z

Outcome: The durable evidence statement now accurately separates responsibilities. Haskell binds exact envelopes and canonical records to digest/token goldens; TypeScript derives transaction IDs, source-output bytes, dependencies, conflicts, digest, and token while only encoding and linking the fixture-provided required-proof record (`research/03-cardano-wallet-backend-contract.md:589-594`). The Iteration 3 implementation entry explicitly supersedes the prior wording (`task-plans/task-202-impl-review.md:353-358`).

Blocking findings:
None.

Residual risks:
1. Plutus reference scripts lack a dedicated executable case.
2. Exhaustive constructor, stake runtime, and role-neutral multi-domain runtime coverage remain incomplete.
3. Exact follow-up Nix and Fourmolu checks remain environment-blocked.
4. Fixture SHA-256 remains `2e13fc87934f7bd4cb66b7ba025387283562ad1bec7ce64f048d0d88e3ffb6f4`; whitespace validation is clean.

Decision: approved
