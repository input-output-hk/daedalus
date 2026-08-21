# Task task-202: Add ownership, path, required-witness, and batch-overlay analysis

## Task

- Task ID: `task-202`
- Title: `Add ownership, path, required-witness, and batch-overlay analysis`
- Phase: `phase-2` (`cardano-wallet Backend Foundations`)
- Priority: `critical`
- Tracker state at planning: `pending`
- Repository classification: cross-repository backend task. Production changes belong in sibling `../cardano-wallet`; Daedalus receives contract fixtures and durable task records. `task-209` alone activates the aggregate capability and changes the tracked backend pin.

## Why Now

- `task-201` is complete at cardano-wallet `3ca15553f96587f1f96688185165b2ede00e30b0` with published decoder `44e0a32300ec6d6d03b7578b97b8374820802ba1`. It supplies one dormant Conway context route, exact ordinary-output spans, full pending/node output authority, coherent `W/G/P`, and canonical records/digest/token.
- The response still has no ownership record `0x02`, required-proof record `0x06`, earlier provenance, or sequential dependency/conflict overlay. Tasks 203, 204, 205, 206, 304, and 306 require those backend-owned facts.
- Live signing code contains reusable input, collateral, withdrawal, certificate, required-signer, native-script, and policy-key seams, but its broad certificate/mint heuristics are not a proof inventory. This task adds read-only analysis to the existing route without activating or redesigning signing.

## Interaction Mode

- Mode: `autonomous`.
- Required user input, manual checks, and user-supplied evidence: none. No seed, passphrase, funds, device, product decision, external reviewer identity, publication permission, or pin approval is needed.
- The implementation starts from exact cardano-wallet parent `3ca15553f96587f1f96688185165b2ede00e30b0`, uses the already-published decoder unchanged, and needs no third-repository work. The explicit `isValid=false` boundary below removes the unavailable collateral-return span dependency rather than hiding a publication or review checkpoint.
- The implementer owns deterministic tests, focused plain HTTP/mTLS validation, sibling environment repair, temporary exact-candidate Daedalus builds, and internal review. Only a concrete external infrastructure failure after bounded autonomous remediation may pause work; record it truthfully and keep build status `in_progress`.
- Tasks 200-208 remain a local cardano-wallet series. `task-209` alone opens the consolidated upstream PR, obtains authorized upstream review, activates capabilities, performs aggregate migration/integration review, and updates the Daedalus pin.

## Scope

- Extend only Shelley `POST /v2/wallets/{walletId}/transaction-context` revision 1. Keep the task-201 request byte-for-byte unchanged: expected network identity and `1..50` ordered exact Conway transactions, with no caller paths, ownership, roles, proof lists, or parent summaries.
- Accept task-202 analysis only when every outer transaction `isValid` value is `true`. Reject any `isValid=false` item as fixed `400 dapp_invalid_request` before capture. This is a deliberate dormant-capability scope boundary, not a claim that invalid ledger semantics equal valid semantics.
- For accepted items, derive earlier ordinary outputs from exact task-201 output spans, resolve earlier before pending before node, classify relevant credentials against the same captured wallet state, and return ownership, exact paths, required-wallet-proof rows, dependencies, and earlier-request spending conflicts.
- Preserve caller order and identical duplicate items. Use one sequential fold; never topologically reorder or deduplicate the request.
- Run stateful payment discovery on a private copy of the captured checkpoint state in deterministic transaction/credential order. Verify every returned path from captured public state before emitting it and never persist request-driven discovery.
- Add ownership records `0x02`, required-proof records `0x06`, and the `earlier` provenance bit to the existing canonical record vector. Keep all research-03 codes, widths, proof bits, sorting, true Blake2b-256 digest, HMAC token, and task-201 records unchanged.
- Audit ownership-affecting mutation tags against the task-201 clock. Add a tag only for a live persisted mutation that can change returned ownership/path evidence and is not already `WalletContextChange`; the endpoint itself never bumps a generation.
- Keep capability publication unavailable and the tracked Daedalus pin unchanged.

## Non-Goals

- Do not support `isValid=false` context analysis in task-202. In ledger terms, invalid transactions produce no ordinary outputs, do not consume normal inputs, consume collateral inputs, and may produce collateral return at ledger output index `length ordinary_outputs`. The published decoder exposes exact spans only for ordinary outputs, not collateral return, so exact earlier authority cannot be completed without another decoder change. Task-202 therefore rejects the complete invalid item rather than emitting partial or canonicalized evidence. A later task may widen the contract only with exact collateral-return span publication and review.
- Do not implement software signing, passphrase handling, witness extraction/differencing, `partialSign` behavior, or witness generation. Task-203 consumes the pure proof inventory and owns those side effects.
- Do not perform complete existing-witness-envelope validation, body commitment validation, Plutus execution, semantic review, or independent Daedalus context validation. Tasks 303, 304, and 306 retain those responsibilities. This task verifies only VKey witnesses narrowly needed to calculate current proof satisfaction.
- Do not implement DRep, committee, pool, Genesis/MIR, proposal, or voting ownership/proofs; CIP-105 role 3; public key APIs; or deprecated-certificate domain errors. Tasks 205/206 own reviewed support. The fail-closed constructor matrix below prevents silent omission meanwhile.
- Do not add a new route, request revision, context cache, persisted state, SQL migration, second CBOR parser, decoder dependency change, Daedalus runtime client, IPC/UI/hardware code, dependency, or lockfile change.
- Do not alter task-201 set equations, pending state mapping, exact-point query/retry behavior, process token, privacy boundary, or capability `404`.
- Do not open a task-specific upstream PR, update `flake.nix`/`flake.lock`, or edit either append-only review log.

## Dependencies And Authority

- `task-201`: cardano-wallet parent `3ca15553f96587f1f96688185165b2ede00e30b0`, decoder `44e0a32300ec6d6d03b7578b97b8374820802ba1`, exact ordinary-output spans, fixed route/schema, coherent capture, and pending/node authority.
- `task-003` and research 03: source precedence, records `0x02`/`0x06`, proof bits, errors/privacy, and delivery ownership.
- `task-002`: public CIP-30 witness/error and caller-order behavior remains unchanged.
- `task-004` and research 04: pinned `cardano-ledger-conway-1.22.1.0` constructors, exact-CBOR authority, Conway conditional readiness, and Dijkstra refusal.
- Downstream `task-203` reuses this task's pure proof inventory for signing completeness; tasks 205/206 widen governance/certificate coverage; task-304 independently validates backend context; task-209 owns activation and pinning.
- Authority order for one outpoint is exact earlier ordinary output, exact pending output, then node UTxO at `W`. Reduced checkpoint outputs, caller summaries, renderer calculations, and post-confirmation state reads are never authority.

## Research, Docs, Workflows, And Skills Consulted

- Read in the same required order as iteration 1: `.agent/readme.md`, `.agent/system/architecture.md`, governing PRD/tracker, task-201 plan and complete review evidence, research 03/02/04, `.agent/workflows/build.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, plus API/IPC/Nix guidance.
- Inspected live Daedalus and sibling cardano-wallet source/history, including the task-201 context modules, Shelley signing, sequential discovery, stake path, policy-key storage/derivation, Conway certificate conversion, tests, Swagger guidance, and contribution/testing guidance.
- Loaded `ponytail` at full level. The smallest truthful implementation extends the existing route, captured state, resolver, encoder, and tests; it adds no parser, service, cache, persistence, or dependency.
- Loaded `understand`; no graph is generated because this planning correction may revise only this canonical file. Material relationships were verified directly in live repositories and history.
- Loaded `cbor-encoding-decoding`. `cbor-diag` is fixture diagnostics only; production uses the published exact-span decoder and pinned ledger decode/serialization.

## Live Implementation Findings

- `DecodedTx` currently retains exact envelope bytes, body-derived ID, normal/collateral/reference inputs, exact ordinary-output spans, and expiry. It does not retain outer `isValid` or an exact collateral-return span; the ledger transaction itself exposes `isValid`, so the accepted boundary can be enforced without changing the decoder.
- `Capture` can retain the same checkpoint state already read under the task-201 gate. Payment `isOurs`, captured account xpub/prefix, fixed stake path, and captured stored policy xpub are sufficient; `readPolicyPublicKey` after capture is forbidden.
- Shelley signing currently treats normal/collateral inputs, withdrawals, certificate keys, explicit required signers, mint scripts, and staking scripts through separate heuristics. The task-202 inventory must use pinned ledger witness selectors and native-script evaluation instead of copying those booleans.
- CIP-1855 storage derives exactly `m/1855'/1815'/0'` and stores that leaf xpub. Its Blake2b-224 key hash is distinct from the mint policy ID, which is the native/Plutus script hash.
- Current response `FromJSON` can validate response structure and record parity, but it receives no request and cannot validate indexed overlay relationships. A separate request-aware seam is required.

## Fixed Contract Decisions

### Accepted Validity And Sequential Overlay

- Decode every item fully and inspect outer `isValid` before wallet capture. All items must be `true`; one `false` rejects the complete request as `400 dapp_invalid_request / Invalid backend request` with no context response.
- For accepted `isValid=true` items, ordinary outputs are produced at their body indexes, normal inputs are consumed, collateral inputs are not consumed, and reference inputs never claim. Maintain a prior-consumed set containing only earlier normal inputs. A current normal or collateral input already in that set receives conflict metadata; current collateral never enlarges the set, and reference inputs neither enlarge it nor receive conflict rows.
- The rejected-case ledger truth is frozen for future widening: with `isValid=false`, ordinary outputs are not produced, normal inputs are not consumed, collateral inputs are consumed, and an optional collateral return is produced at index `length ordinary_outputs`. No ordinary-output span may stand in for collateral return, and no canonical reserialization may be called exact source bytes.
- Compute each transaction ID from its exact body. Index exact ordinary output spans only after the producing accepted item. Self/forward references, out-of-range indexes, unresolved inputs, and duplicate transaction IDs with unequal complete envelopes are `dapp_invalid_request`. Identical duplicate envelopes remain separate items; an earlier lookup selects the lowest prior request index with that ID/output.
- Resolve all roles against earlier, pending, and node sources and compare every simultaneously available source byte-for-byte. Unequal bytes are `dapp_context_conflict`; equal bytes retain all provenance labels in `earlier,pending,node` order.
- An exact pending normal/collateral claim remains `dapp_context_conflict`. Within the accepted request, an earlier collateral use does not conflict with a later normal/collateral use, while an earlier normal consumption conflicts with a later normal or collateral use. Reference reuse remains non-conflicting review metadata under the frozen contract.

### Response Extension And Cardinality

- Add exactly the three top-level fields already proposed: `ownership`, `required_wallet_proofs`, and `batch_overlay`. Existing fields and representations remain unchanged.
- `credential_kind` is `payment|stake|policy`; `ownership` is `unowned|owned_key|script`; credentials are exactly 28 lowercase-hex bytes. `owned_key` requires a valid nonempty path; `unowned` and `script` require `[]`.
- Ownership rows are unique by `(credential_kind,credential,ownership,derivation_path)`, merge proof uses, and sort by frozen kind code, credential bytes, ownership code, then encoded path. Contradictory evidence for the same domain credential is an internal invariant failure.
- Required rows are unique by `(transaction_index,proof_kind,credential_kind,credential)`, sort by transaction index, proof-kind code, credential-kind code, then credential bytes, and exist only for producible owned-key candidates.
- A dependency has uniqueness key `(transaction_index,input_role,outpoint)`. Emit exactly one authoritative row per key: choose `earlier` whenever available, otherwise `pending`; node-only emits none. `source_transaction_index` is the lowest strict prior producer index for `earlier` and `null` for `pending`.
- Equal-provenance cardinality is fixed: earlier+pending, earlier+node, and earlier+pending+node each emit one `earlier` dependency; pending+node emits one `pending` dependency; node alone emits none. All equal labels still remain on the corresponding output provenance. Identical duplicate parents never create multiple dependency rows.
- A conflict has uniqueness key `(transaction_index,input_role,outpoint)` and names the lowest prior normal consumer. Its current `input_role` is `normal|collateral`; dependencies retain `normal|collateral|reference`.
- Dependency rows sort by transaction index, role, outpoint; conflict rows sort by transaction index, role, outpoint, earlier index. Duplicate, unsorted, self, forward, non-authoritative, missing, or contradictory rows are invalid.

### Credential Identities And Role-Neutral Matching

- A payment credential is the 28-byte payment key/script hash governing a Shelley output. A stake credential is the 28-byte stake key/script hash governing a withdrawal or accepted certificate. These direct ledger domains determine `credential_kind`.
- A policy script credential is the 28-byte policy ID/script hash. Emit it as `(policy, policy_script_hash, script, [], [policy])`; it never carries a CIP-1855 path and never becomes an owned-key required row.
- A CIP-1855 policy leaf credential is the Blake2b-224 hash of the captured stored policy leaf xpub. Emit it separately as `(policy, leaf_key_hash, owned_key, [1855',1815',0'], proof_kinds)` only when that exact key hash participates in a selected obligation. The path must derive/identify the leaf key hash, never the policy script hash.
- Explicit required-signer hashes and native-script key leaves are role-neutral ledger key hashes. Match each independently against all three captured producer domains: discovered payment keys, fixed stake key, and stored policy key. Emit every legitimate owned domain match; never apply payment-first precedence or reject consistent multiple matches. An unmatched role-neutral hash creates no fabricated domain-specific unowned row, but remains in the internal unsatisfied obligation inventory.
- Direct payment/stake key credentials still emit an `unowned` row when their known domain has no owned match. Direct script credentials emit `script`. Role-neutral matching does not change these domain-known rules.

### Exact Paths And Captured-State Rules

- CIP-1852 payment path schema is exactly five Word32 components: `[0x8000073c,0x80000717,account',role,index]`. `account'` must equal the captured sequential state's hardened account index; `role` is soft `0` external or `1` internal; `index` is soft `0..0x7fffffff`.
- CIP-1852 stake path is exactly `[0x8000073c,0x80000717,account',2,0]` with the same captured hardened account index.
- CIP-1855 policy path is exactly `[0x8000073f,0x80000717,0x80000000]`; all three components are hardened. Current storage supports no other policy index.
- Capture and retain one checkpoint discovery state, its sequential derivation prefix/account xpub, and optional stored policy leaf xpub in the first task-201 DB read. Run `isOurs` only on a private copy. Reject a returned path unless it passes the exact schema, prefix, role/index, and account constraints above.
- Re-derive payment/stake public children from the captured account xpub and hash the raw public key to the exact credential. For policy, hash the captured stored leaf xpub and require the exact fixed path whose write seam derives that xpub. A private `isOurs` result alone never authorizes a path.
- Confirmation reads only current `W/G/P`; no account key, discovery state, policy key, or ownership state is read after confirmation. Any capture mismatch discards all private discovery results. Tests prove endpoint discovery does not persist and policy/account state cannot change without advancing `G`.

### Proof Inventory And Required Semantics

- Build one pure per-transaction `ProofInventory` at the existing transaction-context assembly seam. It contains: direct key obligations; selected native-script trees; the complete set of valid existing VKey witness key hashes; every producible owned-key candidate with domain/path/public key/hash; per-script satisfaction before and after each candidate hash removal; and aggregate supported-obligation satisfaction. Keep this internal value reusable by task-203; do not sign or expose private keys here.
- Narrow existing-witness verification in task-202 is exactly: extract VKey witnesses from the supplied transaction, recompute the exact body hash, verify each Ed25519 signature with its supplied public key, derive its 28-byte key hash, and include only valid witnesses in a duplicate-free hash set. Invalid VKeys do not satisfy an obligation and are not rejected solely by this narrow check; bootstrap and every non-VKey witness class are ignored here and remain task-304/306 validation scope.
- The producible inventory includes every captured owned key that matches a direct obligation or any leaf of a selected native script, including alternatives that are not individually necessary. It is independent of current witness presence and deduplicates only identical `(public_key,key_hash,credential_kind,path)` candidates.
- Evaluate native `RequireSignatureOf`, `RequireAllOf`, `RequireAnyOf`, `RequireSomeOf`, `ActiveFromSlot`, and `ActiveUntilSlot` with pinned Conway semantics and the exact transaction validity interval. Resolve only scripts selected by an input credential or mint policy and verify exact script hash. Missing selected script material is `503 dapp_context_unavailable`.
- Let `E` be valid-existing witness hashes, `P` producible owned-key hashes, and `F = E union P`. Record satisfaction separately for every direct/native obligation and aggregate satisfaction across all of them. For required-row tuple `(transaction,proof_kind,domain,key_hash)`, associate every obligation of that proof kind containing that hash; set `required=true` exactly when at least one associated obligation is satisfied by `F` and becomes unsatisfied by `E union (P minus {key_hash})`. Otherwise it is `false`. Existing evidence for the same hash therefore survives producer removal, an unrelated baseline-unsatisfied obligation does not corrupt a satisfied obligation's necessity result, alternatives/threshold surplus may be false, and all domain rows for the same role-neutral hash share the hash-level boolean.
- Direct normal-input, collateral, withdrawal, accepted-certificate, and required-signer obligations are modeled as signature requirements under the same rule. A valid existing witness makes the corresponding owned candidate row `required=false`; aggregate satisfaction remains true.
- Task-202 computes and tests aggregate satisfaction but does not enforce `partialSign`. Task-203 must reuse this pure result after applying produced witnesses: `partialSign=false` fails when any supported aggregate obligation remains unsatisfied; `partialSign=true` produces all available owned candidates. No signing behavior is added here.
- Non-mint selected native scripts use `proof_kind=native_script` and ownership proof bit `native_script`. Mint native policies use only `proof_kind=policy` and ownership proof bit `policy`, including payment/stake/policy domain leaf matches; do not emit a second `native_script` row/bit for the same mint policy obligation. The separate policy script-hash row also carries only `policy`. Plutus policy IDs emit only the script-hash ownership row and no owned-key required row.
- Reference inputs may create dependency and payment ownership context but never a spending proof bit or required row.

### Pinned Conway Credential Constructor Matrix

All rows use pinned `cardano-ledger-conway-1.22.1.0` constructors/predicates before any reduced wallet conversion. `Accepted` means task-202 can completely decide its ownership/proof subset; it does not claim task-303 semantic review.

| Surface / pinned alternative                                                                                    | Decision                                                               | Task-202 behavior / exact error                                                                                      |
| --------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| Normal/collateral/reference input resolves to Shelley `Addr` with payment key credential                        | Accepted                                                               | Classify payment domain; normal/collateral direct proof as applicable, reference no proof.                           |
| Normal/collateral/reference input resolves to Shelley `Addr` with payment script credential                     | Accepted                                                               | Emit payment script ownership; resolve selected native script or retain Plutus script ownership; reference no proof. |
| Any requested or wallet-snapshot input resolves to `AddrBootstrap` / Byron address                              | Rejected                                                               | `400 dapp_invalid_request`; no bootstrap path/witness inference.                                                     |
| Ordinary parent output Shelley address                                                                          | Accepted                                                               | Its payment credential may become earlier input authority; stake reference is not a spending credential.             |
| Collateral-return address                                                                                       | Inert for accepted `isValid=true`; unavailable for rejected false case | False item rejects `400 dapp_invalid_request` before overlay because no exact collateral-return span exists.         |
| Withdrawal reward account key/script credential                                                                 | Accepted                                                               | Classify stake domain and pinned withdrawal witness obligation.                                                      |
| Conway cert tag 0 `RegTxCert`                                                                                   | Accepted                                                               | Use pinned certificate witness selector; emit stake proof only when selector requires the stake credential.          |
| Conway cert tag 1 `UnRegTxCert`                                                                                 | Accepted                                                               | Same pinned stake selector.                                                                                          |
| Conway cert tag 2 `DelegTxCert` (pool delegation)                                                               | Accepted                                                               | Stake credential is the witness domain; pool target is not treated as a wallet proof.                                |
| Conway cert tag 7 `RegDepositTxCert`                                                                            | Accepted                                                               | Same pinned stake selector.                                                                                          |
| Conway cert tag 8 `UnRegDepositTxCert`                                                                          | Accepted                                                               | Same pinned stake selector.                                                                                          |
| Conway cert tags 9/10 vote or stake+vote delegation                                                             | Accepted                                                               | Stake credential is the witness domain; DRep/pool delegatees are non-witness targets and are not classified here.    |
| Conway cert tags 11/12/13 registration+delegation combinations                                                  | Accepted                                                               | Stake credential is the witness domain under the pinned selector; delegatees are non-witness targets.                |
| Conway cert tags 3/4 pool registration/retirement                                                               | Rejected                                                               | `400 dapp_invalid_request`; pool operator/owner proof is task-206 scope.                                             |
| Conway cert tag 14 committee hot authorization                                                                  | Rejected                                                               | `400 dapp_invalid_request`; committee credentials unsupported.                                                       |
| Conway cert tag 15 committee cold resignation                                                                   | Rejected                                                               | `400 dapp_invalid_request`; committee credential unsupported.                                                        |
| Conway cert tags 16/17/18 DRep registration/deregistration/update                                               | Rejected                                                               | `400 dapp_invalid_request`; DRep proof is tasks 205/206 scope.                                                       |
| Retired Shelley certificate tags 5/6 Genesis delegation/MIR                                                     | Rejected by Conway decode/constructor gate                             | `400 dapp_invalid_request`; never reduce to `CertificateOther` or guess a stake key.                                 |
| Nonempty voting procedures: committee voter                                                                     | Rejected                                                               | `400 dapp_invalid_request`; key or script committee credential unsupported.                                          |
| Nonempty voting procedures: DRep voter                                                                          | Rejected                                                               | `400 dapp_invalid_request`; key or script DRep credential unsupported.                                               |
| Nonempty voting procedures: stake-pool voter                                                                    | Rejected                                                               | `400 dapp_invalid_request`; pool key unsupported.                                                                    |
| Any nonempty proposal-procedure set, including return reward account and governance-action embedded credentials | Rejected                                                               | `400 dapp_invalid_request`; no proposal credential may disappear through reduced conversion.                         |
| Explicit required signer key hash                                                                               | Accepted                                                               | Role-neutral matching across payment/stake/policy domains.                                                           |
| Selected native-script key leaf                                                                                 | Accepted                                                               | Role-neutral matching and exact pinned script evaluation.                                                            |
| Mint policy ID with exact native policy script                                                                  | Accepted                                                               | Separate policy script-hash row and role-neutral leaf candidates; proof kind `policy`.                               |
| Mint policy ID with exact Plutus policy script                                                                  | Accepted for ownership only                                            | Policy script-hash row; no wallet key proof. Missing exact selected material is `503 dapp_context_unavailable`.      |

- The implementation adds an exhaustive table-driven test mirroring every row. Any pinned constructor not listed, any future constructor, or any credential-bearing field reached without a matrix decision is `400 dapp_invalid_request`, never silently ignored.

### Request-Aware Validation, Binding, Errors, And Privacy

- Keep ordinary `FromJSON ApiDappTransactionContextResponse` structural: closed fields, enum/length/path/cardinality/order checks, record parity, digest/token payload shape. It must not claim to recompute request relationships it cannot see.
- Add one explicit pure `validateTransactionContextResponseForRequest request response` seam adjacent to the existing context types/assembly. It decodes the ordered request, enforces all-true validity, recomputes IDs/ordinary output indexes/roles, authoritative dependency cardinality, earliest parent/claim indexes, and exact overlay sorting/uniqueness, and compares the complete typed overlay. Call it on backend assembly results and in API/client/golden tests. Task-209's Daedalus response validator receives the original broker-owned request and implements the same check; task-304 later independently validates the complete context/digest.
- Encode one `0x02` record per ownership row and one `0x06` record per required row. Overlay gets no record because the request-aware validator deterministically derives it from digest-bound ordered transactions plus exact `0x01`/`0x07` authority. If implementation cannot recompute one unique overlay, replan rather than trust metadata.
- Fixed errors: malformed/unsupported constructors, `isValid=false`, invalid paths/references, bootstrap inputs, and unsupported credentials are `400 dapp_invalid_request`; unequal authority or pending spend is `400 dapp_context_conflict`; unavailable exact output/script/context is `503 dapp_context_unavailable`; path/hash/record invariant failure is `500 dapp_internal_error`; account/network drift remains `409 dapp_account_changed`.
- Preserve task-201 route normalization and privacy. Bodies, traces, metrics, fixtures exported as evidence, and logs expose no transaction/output/script/witness/address/credential/path/digest/token/wallet/TLS/database material; only fixed categories and bounded counts/timing are allowed.

## Expected Files

### Sibling `../cardano-wallet`

- `lib/api/src/Cardano/Wallet/Api/Http/Shelley/TransactionContext.hs`: retain captured state, enforce validity/matrix gates, build earlier overlay, run ownership/proof inventory, and assemble response.
- `lib/api/src/Cardano/Wallet/Api/Types/Dapp/Context.hs`: typed response additions, enums/path checks, `earlier`, `0x02`/`0x06`, structural JSON validation, and request-aware validator.
- `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` only if the smallest pure proof inventory belongs beside the existing signer for task-203 reuse. Do not refactor or sign.
- `lib/wallet/src/Cardano/Wallet.hs` and `lib/address-derivation-discovery/**` only if existing captured-state public derivation cannot be reused directly. No broadened discovery behavior is planned.
- Existing focused API/wallet/transaction/discovery test files and Cabal manifests only as imports/components require. No new package or dependency.
- `specifications/api/swagger.yaml`: exact closed fields/enums/cardinalities/path schemas and unchanged request/errors/capability status.
- No cardano-ledger-read file or pin change is expected.

### Daedalus Evidence And Durable Records

- `source/common/cardano/fixtures/exact-cbor/backend-context-v1.json`: add `0x02`, `0x06`, earlier/equal-provenance/dependency/conflict, validity-boundary, updated digest/token, exact candidate, and SHA-256.
- `source/common/cardano/backendContextFixture.spec.ts`: independently reproduce new records/provenance/digest/token and run a request-aware overlay fixture check. This remains evidence, not task-304 production validation.
- Before final review, update research 03 with this exact corrected contract and candidate evidence; research 04 only with refreshed fixture identity; PRD/tracker with truthful candidate/status and unchanged activation/pin gate; this plan with lifecycle/evidence.
- No Daedalus product source, API client, IPC manifest, translation, Storybook, Cucumber, dependency, lockfile, Nix pin, architecture/API endpoint document, or review-log content is expected.

## Minimum Implementation Approach

1. Synchronize research 03 with the validity boundary, identity/path rules, proof inventory, constructor matrix, dependency cardinality, and request-aware validation before sibling code. Confirm exact parent, decoder identity, schema hash, capability `404`, and dirty-worktree boundaries.
2. Extend `DecodedTx` from the ledger transaction with outer validity and pinned credential-bearing constructors. Reject the whole request unless every item is true and matrix-supported. Add no collateral-return parser.
3. Retain captured checkpoint/discovery/account/policy public state. Build one sequential fold that indexes exact earlier ordinary outputs, compares all authorities, emits one dependency per uniqueness key, and tracks only prior normal consumption; later normal/collateral use of consumed inputs conflicts without treating collateral as consumption.
4. Run one private deterministic ownership fold. Enforce exact CIP-1852/CIP-1855 schemas, re-derive/hash from captured public state, and classify role-neutral hashes against every producer domain.
5. Build the pure `ProofInventory`, narrowly verify existing VKeys, evaluate selected native scripts and aggregate satisfaction, then project canonical ownership/required rows. Add only `0x02`, `0x06`, and earlier provenance to existing encoding.
6. Extend structural response parsing, add and invoke the request-aware validator, and update Swagger. Preserve all task-201 normalization, capture, pending, token, and privacy behavior.
7. Add one table-driven executable matrix covering every constructor row, valid/invalid witness, complete producer inventory, baseline-unsatisfied/alternative/threshold/time native scripts, multiple domain matches, policy script-versus-leaf identity, exact paths, all provenance combinations, duplicate parents, and conflicts.
8. Audit mutation tags and schema. Make no persistence change; prove candidate-mutated pinned-schema bytes retain the same schema hash/version and reopen with old pin `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`.
9. Create one non-amended task-202 child commit; review fixes are follow-up commits. Run focused/full sibling checks, representative real Conway HTTP/mTLS success/fixed failures, and temporary exact-candidate Daedalus bridge/mainnet builds without tracked pin edits.
10. Freeze final candidate, fixture hashes, research/PRD/tracker/plan state, and complete evidence before `@Reviewer`. Reviewer examines the final sibling range and Daedalus evidence together; task-209 retains activation and pinning.

## Acceptance Criteria

- Request revision 1 is unchanged. Every false-validity item fails fixed `dapp_invalid_request`; accepted semantics use exact ordinary outputs and prior normal consumption, do not treat collateral/reference inputs as consumption, and flag later normal/collateral use of an already consumed input. The plan and tests explicitly preserve correct rejected invalid-transaction ledger semantics and the absent collateral-return span rationale.
- Haskell types, handler, structural parser, request-aware validator, Swagger, goldens, and Daedalus fixture agree on all fields, enums, paths, sorting, uniqueness, and cardinality.
- Payment/stake direct credentials and policy script hashes are distinct from CIP-1855 leaf hashes. Explicit signers/native leaves match all payment/stake/policy producer domains and retain every legitimate match without arbitrary precedence.
- Every owned path obeys the exact schemas and derives/hashes to its credential from captured public state. `isOurs` alone cannot authorize a path; no post-confirmation ownership/key read occurs; private discovery is discarded and never persisted.
- The internal proof inventory separately and completely records valid existing VKeys, all producible owned candidates, selected native obligations, per-candidate necessity, and aggregate satisfaction. Mint native policies use only policy proof rows/bits; policy script and key leaf remain separate.
- The exhaustive pinned Conway matrix accepts every listed stake/payment/policy case and rejects bootstrap, unsupported pool/governance/voter/proposal/Genesis/MIR/future constructors with the exact fixed error. Reduced conversion cannot silently omit a credential.
- Earlier/pending/node equality emits all provenance but exactly one dependency under the frozen precedence and uniqueness key. Identical duplicate parents select the earliest strict prior producer. Only earlier normal consumption populates the consumed set; later normal/collateral use conflicts, while collateral never consumes and reference never conflicts.
- Plain JSON decoding remains structural. Request-aware validation is called at backend assembly/tests and is assigned to task-209's Daedalus client validation; malformed free overlay metadata cannot pass.
- Records `0x02`/`0x06`, earlier bit, complete-record sorting, digest, and token match research 03 and cross-language fixtures. No alternate overlay record/hash serialization appears.
- Same captured `W/G/P` and checkpoint state authorizes all evidence. Existing task-201 output equations, pending overlay, exact-point retries, errors/privacy, process-memory state, capability `404`, and unrelated endpoints remain unchanged.
- No schema/persisted context field is added; old-pin reopen succeeds. Any persistence or decoder/third-repository need forces replanning.
- Deterministic Conway unit/property/API/Swagger checks pass. Representative plain HTTP and authenticated mTLS calls prove owned payment/stake/policy, multiple role-neutral matches, native policy necessity, earlier dependency/conflict/equal provenance, and fixed rejected false/bootstrap/governance cases against the exact candidate with redacted logs.
- Temporary `.#daedalus-bridge-mainnet` and `.#daedalus-mainnet` builds identify the exact candidate while tracked pins remain unchanged. The candidate remains unadvertised and non-pin-eligible until task-209.

## Verification Plan

- Run `cardano-wallet-api:dapp-context` and focused wallet/transaction/sequential-discovery tests in the sibling-supported Nix/Cabal environment. Build touched API, wallet, application, unit, and integration components; run Swagger validation, Fourmolu, focused HLint, `cabal check`, and `git diff --check`.
- Matrix-test every accepted/rejected constructor and exact error; all-true/all-false/mixed requests; ordinary/collateral-return index semantics; Shelley versus bootstrap inputs; role-neutral zero/one/multiple domain matches; account/stake/policy path bounds; captured-state re-derivation; and no persistent/private discovery mutation.
- Test valid, invalid, duplicate, and wrong-body VKeys; complete producible inventory despite existing witnesses; native all/any/threshold/time trees; baseline-unsatisfied scripts; candidate-hash removal; aggregate satisfaction; reference no-proof; and native-mint policy-only row rules.
- Test earlier/pending/node, all four equal-provenance combinations, unequal conflicts, source/canonical distinction, identical duplicate parents, self/forward/out-of-range/unresolved references, one authoritative dependency per key, pending-spend rejection, earliest normal conflicts, collateral/reference reuse, caller order, and duplicates.
- Test structural parser separately from `validateTransactionContextResponseForRequest`: malformed paths/enums/records fail structurally; request/overlay mismatch, duplicate/non-authoritative dependencies, wrong earliest indexes, and invalid conflict claims fail only at the request-aware seam.
- Independently regenerate fixture records/digest/token in Haskell and TypeScript; use `cbor-diag` only to inspect committed synthetic CBOR; parse JSON, run focused Prettier/Jest, and run whitespace checks in both repositories.
- Run a focused registered Conway local-cluster scenario over plain HTTP and mTLS with representative accepted proof/overlay cases and false/bootstrap/governance fixed failures. Record sanitized candidate/base, decoder identity, commands, normalized results, fixture SHA-256, and absence of sensitive logs. A skipped runtime row is not a pass.
- Compare SQLite schema/version/hash before/after candidate reads/mutations and reopen exact bytes with old pin. Temporarily override Daedalus's backend input to the exact candidate for bridge/mainnet builds without editing tracked Nix files.

## Risks And Mitigations

- Invalid transaction coverage is intentionally narrower than eventual product review. Mitigation: fixed pre-capture rejection is truthful and exact; widening requires published exact collateral-return spans and corrected invalid production/claim logic, never a fallback.
- Native-script necessity is conditional. Mitigation: preserve complete existing/producible sets, aggregate satisfaction, and pinned tree evaluation; necessity is key-hash removal only from an already satisfied baseline.
- One hash may have multiple producer domains. Mitigation: retain all exact verified matches and use hash-level necessity, not domain precedence.
- Address discovery is stateful. Mitigation: one canonical private fold over captured state, public-key re-derivation, no persistence, and `G` mutation audit.
- Governance constructors can be lost by reduced wallet conversion. Mitigation: inspect pinned ledger constructors before conversion and table-test an exhaustive default-reject boundary.
- Overlay has no dedicated record. Mitigation: request-aware deterministic validation at the producer and eventual consumer; ambiguity requires replanning.
- The sibling is maintenance-only. Mitigation: task-202 remains one local child; task-209 owns consolidated authorized upstream review, activation, migration review, and pinning.

## Docs, Tracking, And Research Updates

- Before implementation, update research 03 with the corrected validity boundary, overlay cardinality, policy identities, role-neutral matching, path schemas, proof inventory, constructor matrix, and request-aware seam. Do not alter task-201 meanings or phase-0 public values.
- After candidate verification, record exact candidate/base, fixture hash, deterministic/runtime/privacy/schema evidence, and residual task-203/205/206/209 ownership in research 03. Update research 04 only with fixture identity; Conway remains conditional and Dijkstra blocked.
- Before final review, update PRD/tracker with truthful proposed implementation state, exact evidence, explicit valid-only dormant capability boundary, and unchanged activation/pin gate. Do not mark production readiness.
- Update this plan with candidate, verification, lifecycle, and proposed outcome before review. Do not update shipped architecture/API docs until task-209 activates and consumes the complete backend.

## Review Logs

- Planning review: `.agent/plans/dapp-browser-cip30/task-plans/task-202-plan-review.md`
- Implementation review: `.agent/plans/dapp-browser-cip30/task-plans/task-202-impl-review.md`
- Both are Orchestrator-owned append-only records. Planner, Critiquer, and Reviewer return proposed transcript blocks and never write them.

## Lifecycle Status

- Planning status: `approved`.
- Build status: `approved` in resumed review cycle Iteration 3.
- Candidate range: cardano-wallet `3ca15553f96587f1f96688185165b2ede00e30b0..e60b8a66cad9121e54656c76e03a3785099b9215`.
- Fixture SHA-256: `2e13fc87934f7bd4cb66b7ba025387283562ad1bec7ce64f048d0d88e3ffb6f4`.
- Verification: 13 focused proof/contract examples, 21 route/error/retry examples, logging privacy, API and integration-library builds, semantic Swagger, authenticated mTLS ownership/proof/reference-policy/overlay/rejection/fixed-503 scenario, unchanged-schema old-pin evidence, and cross-language Jest/Prettier pass. Exact-candidate bridge/mainnet Nix reruns and repository Fourmolu are environment-blocked by a missing Nix-store package index; the preceding candidate's temporary builds passed, and touched Haskell compiles and whitespace checks pass.
- Current outcome: approved. Reference-input script authority, unequal duplicate-envelope validation, focused request-validator coverage, and runtime reference-policy plus valid/wrong-body VKey coverage close the review blockers. Capability publication, tracked pin, signing, governance expansion, and consolidated upstream review remain unchanged for tasks 203, 205/206, and 209.

## Planner Self-Review

- Scope: one existing dormant response and pure analysis seam; no invalid-transaction approximation, third repository, signing, governance support, persistence, runtime client, activation, or pinning.
- Blocker 1: corrected valid/invalid production and claims, froze collateral-return index/span truth, selected explicit fail-closed valid-only scope, removed hidden external dependency, and retained autonomous interaction.
- Blocker 2: separated policy script hash from CIP-1855 leaf hash and made explicit signer/native leaf matching role-neutral, all-domain, and multi-match preserving.
- Blocker 3: separated narrow valid-existing VKeys, complete producer inventory, aggregate/native satisfaction, hash-removal requiredness, and mint policy proof kinds without implementing task-203/304 behavior.
- Blocker 4: added an exhaustive pinned-Conway accepted/rejected credential matrix with all certificate alternatives, voters, proposals, bootstrap inputs, Genesis/MIR, defaults, and exact errors.
- Blocker 5: kept JSON structural and assigned request-aware validation to a named producer/test seam and task-209 consumer validation.
- Blocker 6: froze dependency uniqueness and one-row cardinality for earlier/pending/node and duplicate-parent equality combinations.
- Blocker 7: froze exact CIP-1852/CIP-1855 lengths/components/ranges, captured account/policy xpub verification, private `isOurs` limits, mutation clocks, and no post-confirmation state reads.
- Consistency: accepted requests track only normal consumption but may flag later normal/collateral use; collateral proof can still be required despite no valid-execution consumption; reference never requests spending proof; existing witnesses survive producer-removal tests; policy script/leaf identities and proof kinds do not collide; aggregate satisfaction remains distinct from per-row necessity.
- Delivery: expected files, implementation steps, acceptance, verification, risks, research/docs, interaction mode, no-schema rollback, runtime evidence, capability `404`, and task-209 gate all reflect the corrected contract.
