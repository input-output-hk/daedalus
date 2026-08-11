# Task task-004: Validate exact-CBOR and era coverage

## Task

- Task ID: `task-004`
- Title: `Validate exact-CBOR and era coverage`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`

## Why This Task Now

- `task-002` is complete and supplies the frozen CIP-30 wire boundary. Tasks `302` and `303` now need exact ledger-produced span vectors, a frozen incoming-CBOR policy, a source-derived Conway inventory, and a truthful SDK decision before production parsing begins.
- Live inspection confirms that resolved and installed `@cardano-sdk/core@0.41.4` models Conway body fields through key 22 but is not a strict boundary: targeted probes accepted trailing root bytes, duplicate and unknown body keys, and tagged and untagged input sets.
- Dijkstra appears in the sibling wallet's pinned ledger dependencies and local-state-query types, but live wallet paths still report it as not supported and current ledger CDDL changes the transaction envelope and semantics. This task must prevent partial type presence from becoming a product-support claim.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs, manual steps, and user evidence: none.
- No configured wallet, funds, private key, hardware device, packaged Electron build, live transaction, or external audit is required.
- A retained SDK dependency and a Dijkstra `unsupported/readiness-blocked` result are valid evidence-backed outcomes.

## Scope

- Freeze an authoritative Conway incoming-CBOR encoding-family acceptance table and a machine-readable inventory derived from immutable ledger CDDL/source.
- Commit a small set of immutable-ledger-proven transaction vectors containing exact envelope, body, witness, `isValid`, auxiliary-data/null, ordinary-output, and collateral-return spans.
- Prove transaction ID as true Blake2b-256 over the exact incoming body span without semantic reserialization.
- Run targeted installed/candidate SDK probes against the same vectors and record representation gaps and permissive behavior.
- Record conditional Conway readiness and Dijkstra readiness blockers using statuses that cannot imply implemented backend, parser, or product support.
- Inventory every protocol-parameter/context-dependent rule and assign its exact-chain-point source and downstream implementation/test owner.

## Non-Goals

- Do not implement or export the production cursor, envelope API, or span abstraction. `task-302` owns those files and structural enforcement.
- Do not implement exhaustive semantic decoding, semantic uniqueness, ledger validity, commitments, effects, or governance/collateral models. `task-303` owns them.
- Do not build an exhaustive malformed corpus, differential ledger suite, property suite, or fuzz mutator. `task-800` owns those.
- Do not claim that SDK decoding proves wire acceptance, ledger validity, backend availability, parser implementation, or product support.
- Do not infer era solely from transaction bytes; runtime support intersects trusted backend era/capability evidence with implemented parser/product policy.
- Do not modify cardano-wallet, backend pins, renderer, IPC, hardware adapters, Nix, translations, Storybook, Cucumber tests, or either review log.

## Dependencies And Ownership

- `task-002`: completed; freezes `cbor<transaction>`, exact decoded-byte limits, and canonical empty witness behavior while delegating full transaction slicing and era semantics here.
- `task-001`: requires immutable broker-owned bytes and fail-closed incomplete review.
- Task-003 contract: plans exact-point `W/G/P` backend context and era capabilities but does not implement them. Lossy wallet `TxOut` is never exact output or protocol-context evidence.
- `task-302`: production full-consumption cursor, exact spans, structural map-key duplicate rejection, and frozen wire-policy enforcement.
- `task-303`: exhaustive supported-era semantic recognition, semantic uniqueness, commitments, effects, and protocol-rule evaluation.
- `task-304`: reconciliation with authoritative exact-chain-point backend context and protocol parameters.
- `task-800`: differential ledger comparison, systematic mutations, property tests, fuzzing, and regressions for discovered parser defects.

## Research, Docs, Workflows, And Skills Consulted

- `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/plans/readme.md`, the full PRD/task graph, accepted task-002/task-003 plans and research, and the full task-004 planning review through Critiquer iteration 1.
- `.agent/workflows/test.md` and `.agent/workflows/update-doc.md`.
- Installed SDK serialization source under `node_modules/@cardano-sdk/core/dist/cjs/Serialization/` and sibling cardano-wallet local-state-query, protocol-parameter, era, and unsupported-Dijkstra paths.
- Immutable ledger evidence inspected during revision: `IntersectMBO/cardano-ledger` commit `adcb341f236fd224f60577a79ffeb5fb138f051f`, Conway CDDL blob `0e9b927e3cc413d6b352987c19d26b71f0940849`, Dijkstra CDDL blob `f7a843fb7e7081cf44290f686ff8f9bc04b5c22c`, and their transaction-golden blobs. These are planning evidence; implementation must select and record the source revision that matches the pinned ledger packages, not silently substitute this inspected `master` revision.
- `understand`: loaded. No `.understand-anything` graph exists, so material claims were verified against live files.
- `cbor-encoding-decoding`: used for byte-first diagnostics, exact item boundaries, indefinite forms, tags, and the rule that semantic round-trip cannot prove original bytes.
- `cardano-protocol-params`: used to inventory context-dependent rules. Its general fresh-fetch advice applies to an operator diagnosing a chosen live network; task-004 makes no live-policy claim and therefore requires no arbitrary live fetch.

## Verified Live Findings

- `source/common/cardano/` does not exist; task-004 creates only evidence, fixtures, and test-local tooling there.
- `package.json` declares `@cardano-sdk/core: ^0.41.4`; `yarn.lock` resolves `0.41.4` from tarball shasum `5f38368aed8c27295f8f634e1fbe413a5c09633e` and integrity `sha512-fr2P8ZHPPhN7eL8AGvGFUfyDTr0K5Efd6UbH3Xim9NcsjebK8+sprBXCtupf5YiBSwfkpqO815ijKtJKZ7rY/w==`; the installed package reports gitHead `0d9fa5f83b6e9cf34412eaf94cc1e4541f3b0159`.
- SDK `0.41.4` retains original object bytes and exposes Conway fields 19-22, but its public parser does not expose authoritative offsets and does not require complete root consumption or reject unknown/duplicate body keys.
- The frozen comparison candidate is `@cardano-sdk/core@0.47.0`: tarball `https://registry.npmjs.org/@cardano-sdk/core/-/core-0.47.0.tgz`, shasum `795716681420d08e939567bf635df06fd0c8da3b`, integrity `sha512-UANc6oY8Emjf5ID+Qk6XKxujPfrdxu0rDNDNy+jYJGMYh/kyUrLzOK8Ge4ydblqKQwohRXmMKagFusad3yHoDg==`, gitHead `8fb86af308d2045d71b0c18d78abaf0336902983`, Node engine `>=22`. Candidate identity is fixed by these values, not by `latest` at execution time.
- Sibling `cabal.project` pins `cardano-ledger-conway==1.22.1.0` and `cardano-ledger-dijkstra==0.2.0.1`. Its LSQ can retrieve Conway/Dijkstra full UTxO and parameters, but multiple wallet conversion/signing paths explicitly report Dijkstra as not yet supported.
- Conway CDDL has the 4-item envelope, body keys `0,1,2,3,4,5,7,8,9,11,13,14,15,16,17,18,19,20,21,22`, optional tag 258 set forms, Alonzo/Babbage outputs, legacy/map redeemers, Plutus V1-V3, tag-24 embedded data/script references, and legacy auxiliary-data forms.
- Pinned Dijkstra CDDL has a normal 3-item envelope plus a mempool-only compatibility 4-item form whose boolean must be `true`; changes top-level body key 14 to guards; adds top-level keys 23, 25, and 26; adds sub-transaction key 24 for required top-level guards; adds auxiliary-data key 5 for Plutus V4; removes flat-array redeemers; changes guard/script semantics and protocol version encoding; and adds protocol-update keys 34 through 37.

## Frozen Conway CBOR Acceptance Table

This table is the task-004 wire-policy output. “Accept” means eligible for task-302 structural parsing; task-303 may still reject semantically invalid content. Every row must be copied machine-readably into the research/manifest with the exact pinned CDDL and decoder evidence used at implementation.

| Encoding family                   | Task-302 wire decision                                                                                                                                                                                                                                                                                                                          | Boundary and downstream rule                                                                                                                                                                                                                 |
| --------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Root/envelope                     | Accept exactly one complete Conway 4-item transaction `[body, witnesses, bool, auxiliary_data/null]`, definite or well-formed indefinite; reject any trailing item/byte, wrong arity/type, or illegal break.                                                                                                                                    | Exact arity and full consumption are task-302. Dijkstra 3-item and compatibility envelopes remain unsupported here.                                                                                                                          |
| Arrays/maps/strings               | Accept definite and well-formed indefinite forms where the pinned Conway CDDL/ledger decoder admits the underlying array, map, byte string, or text string; enforce declared arity, cardinality, chunk type, chunk/total size, and nesting/decoded-byte limits. Reject indefinite use where the source rule/decoder requires definite encoding. | The manifest names every exception. Task-800 systematically mutates container forms; task-004 keeps only one targeted positive/negative family probe.                                                                                        |
| Integers and length arguments     | Accept minimal and non-minimal representations only when the pinned ledger decoder accepts them and the decoded value satisfies the CDDL type/range. Reject reserved additional-information values, overflow, wrong sign/type/range, and any location whose authoritative rule requires canonical/minimal form.                                 | Exact incoming bytes remain identity. Language-view canonical construction is not incoming transaction canonicalization and is task-303 commitment work. Standalone task-002 `cbor<Coin>` remains canonical by its separate frozen contract. |
| Map ordering                      | Accept any source-permitted map order; never sort before hashing, slicing, or review identity.                                                                                                                                                                                                                                                  | Task-302 preserves spans/order. Task-303 normalizes only after exact-byte identity is retained. Canonical language-view map ordering is separately constructed by task-303.                                                                  |
| Exact duplicate map keys          | Reject repeated byte-identical encoded keys. Also reject repeated decoded scalar keys in structurally known maps, so `00` and `1800` cannot create two body key 0 entries.                                                                                                                                                                      | Task-302 owns this structural rule and one targeted non-minimal scalar-key vector proves it.                                                                                                                                                 |
| Byte-distinct semantic duplicates | Do not claim complete detection from encoded-byte comparison. Inputs, required signers, scripts, datums, redeemer pointers, policy/asset keys, withdrawals, voters/governance-action IDs, and other complex set/map keys can be semantically equal despite different legal bytes.                                                               | Task-004 records one targeted set-member and one complex-map-key vector as requirements. Task-303 owns decoded semantic uniqueness and exhaustive domain fixtures; task-800 owns mutation/differential breadth.                              |
| Set/ordered-set tag 258           | Accept exactly one tag 258 or no tag where Conway defines `set`, `nonempty_set`, `nonempty_list`, or `nonempty_oset`; preserve tag presence and member order. Reject nested/repeated 258, 258 at non-set locations, wrong tags, and empty `nonempty_*`.                                                                                         | Byte-identical structural duplicates reject in task-302; semantic member uniqueness/order semantics belong to task-303.                                                                                                                      |
| Tag 24 embedded CBOR              | Accept only at CDDL-declared `data` and `script_ref` positions, with a byte string containing exactly one structurally well-formed embedded item and no embedded trailing bytes. Preserve wrapper and embedded bytes.                                                                                                                           | Task-302 proves nested full consumption/span; task-303 recognizes the declared embedded semantic type and commitments.                                                                                                                       |
| Other tags                        | Accept only tags explicitly selected by Conway CDDL at that location: Plutus-data bignum tags 2/3, constructor tags 102/121-127/1280-1400, rational tag 30, and auxiliary-data tag 259, in addition to 24/258 above. Reject unknown tags, known tags in wrong locations, and unsupported tag nesting.                                           | Task-004 inventories source alternatives; task-303 implements exhaustive semantic recognition; task-800 mutates tags.                                                                                                                        |
| Legacy Conway-permitted forms     | Accept Alonzo array and Babbage map outputs interchangeably; tagged/untagged set forms; flat-array and map redeemers; raw metadata, Allegra auxiliary array, and tag-259 auxiliary map; preserve original representation.                                                                                                                       | No other historical form is implied. Each accepted legacy family is source-derived in the inventory and receives only the span/SDK probe needed by this task.                                                                                |

If the pinned package's decoder behavior contradicts the inspected CDDL on definite/indefinite or non-minimal forms, implementation must record both and choose the narrower fail-closed intersection unless immutable ledger compatibility evidence proves the broader form is required. It must not silently let SDK behavior choose the policy.

## Protocol Parameter And Context Rule Matrix

For every row, the authoritative runtime source is the full era-specific protocol-parameter value returned by cardano-wallet's node LSQ at the exact captured chain point `W` from task-003's `W/G/P` protocol, serialized/preserved with the reviewed transaction artifacts. No renderer summary, random golden, current-tip-after-the-fact value, or arbitrary “fresh” network fetch is authoritative.

| Rule family                 | Required context                                                                                                                                                 | Downstream implementation and test owner                                                                                                                      |
| --------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Minimum fee                 | `minFeeA`/tx-fee-per-byte, `minFeeB`/fixed fee, exact transaction size                                                                                           | Task-303 computes/checks; task-304 binds exact-point parameters; task-800 differential tests.                                                                 |
| Maximum sizes               | max transaction size and max value size; exact envelope/value sizes                                                                                              | Task-302 supplies sizes; task-303 applies limits; task-304 supplies context; task-800 boundaries.                                                             |
| Minimum UTxO                | coins/ADA per UTxO byte and exact output/value/datum/reference-script size                                                                                       | Task-303 computes; task-304 joins exact outputs/parameters; task-800 ledger differential.                                                                     |
| Collateral                  | collateral percentage, maximum collateral-input count, fee, total collateral, collateral return                                                                  | Task-303 semantics/calculation; task-304 resolves collateral/context; task-800 differential.                                                                  |
| Execution budget/prices     | execution-unit memory/steps prices, maximum transaction execution units, maximum block execution units where relevant                                            | Task-303 reviews transaction budgets/cost; task-304 binds prices/limits; task-800 differential.                                                               |
| Cost models/language views  | complete cost-model map for every used Plutus language and era-specific canonical language-view rules                                                            | Task-303 reconstructs and verifies script-data hash; task-304 supplies exact models; task-800 commitment mutations.                                           |
| Reference scripts           | reference-script fee coefficient and exact referenced script bytes/aggregate size                                                                                | Task-303 computes/reviews; task-304 resolves scripts/parameters; task-800 differential.                                                                       |
| Deposits and pool economics | key, pool, DRep, and governance-action deposits; minimum pool cost where transaction effects require it                                                          | Task-303 models effects; task-304 supplies parameters and registration/governance state; task-800 differential.                                               |
| Governance                  | pool/DRep voting thresholds, committee minimum size/term, governance-action validity period, DRep inactivity period, treasury/current governance state           | Task-303 recognizes effects/rules; task-304 binds parameters and authoritative state; task-800 differential.                                                  |
| Protocol version            | current major/minor protocol version and proposed hard-fork version                                                                                              | Task-303 validates supported semantics; task-304 binds exact-point version; task-301 keeps capability unavailable when unsupported.                           |
| Dijkstra additions          | Plutus V4 cost model; max reference-script size per block/transaction; reference-script cost stride/multiplier; any further fields in the pinned Dijkstra source | Inventory only in task-004. Future Dijkstra parser/context tasks plus task-800 own implementation/evidence before promotion; current product remains blocked. |

Task-004 records field names, source identities, and owners but does not evaluate live policy. Tasks `201`/`209` must later deliver the exact-point full parameter artifact, task-304 must reject missing/stale/mismatched context, and task-303/task-800 must execute the named rules/tests.

## Expected Changed Files

- `.agent/plans/dapp-browser-cip30/research/04-exact-cbor-era-coverage.md`: immutable source identities, the two matrices above, provenance, SDK results, statuses, dependency decision, commands, and downstream gates.
- `source/common/cardano/fixtures/exact-cbor/manifest.json`: machine-readable source-derived Conway inventory, exact positive spans/hashes/provenance, targeted negatives, encoding policy, and SDK results.
- `source/common/cardano/fixtures/exact-cbor/*`: only small exact envelope/body/output vectors and targeted SDK/policy probes.
- `source/common/cardano/exactCborValidation.spec.ts`: unexported test-local span assertion and manifest checks; not a production parser.
- `scripts/exact-cbor-sdk-probe.cjs`: parameterized read-only probe harness accepting an SDK module root and manifest path and emitting normalized JSON.
- PRD, tracker, and this plan: concise result/lifecycle synchronization only after implementation review.
- `package.json` and `yarn.lock`: conditional under the strict no-migration gate below.

No production parser/model, backend, hardware, renderer, IPC, Nix, translation, Storybook, Cucumber, or review-log change is expected.

## Smallest Implementation Approach

1. Pin authoritative sources and build the inventory.

   - Resolve the exact cardano-ledger source revision/artifact corresponding to `cardano-ledger-conway==1.22.1.0` and `cardano-ledger-dijkstra==0.2.0.1`; record package/source hashes, CDDL blobs, relevant decoder source, golden blobs, and reproducible acquisition commands.
   - Generate a machine-checkable inventory from those source files: every Conway body/witness/output key, required/optional field, union/discriminant alternative, cardinality, tag, embedded-CBOR location, accepted legacy family, and Dijkstra delta. Check the committed inventory against a source-derived extraction during tests so hand-maintained omissions fail.
   - The inventory is coverage planning, not executable proof of every semantic alternative. Task-303 must consume it and provide exhaustive semantic fixtures/commitments.

2. Freeze only ledger-proven positive span vectors.

   - Every positive semantic fixture must be immutable bytes extracted from a pinned ledger golden or produced by a checked-in/reproducible pinned-ledger serializer command in a fully identified revision/package/compiler environment. Record artifact SHA-256/blob, generator input/seed where deterministic, command, and exact extraction offset/length. Hand-built CBOR and SDK serialization are negative/probe inputs only.
   - Keep only vectors needed to prove exact full envelope/body/witness/`isValid`/auxiliary spans, multiple ordinary output boundaries, collateral return, Alonzo/Babbage outputs, and one tagged/untagged or legacy representation distinction. Do not execute every Conway union alternative here.
   - Establish manifest offsets independently before implementing the scanner: obtain them from ledger serializer annotations/golden extraction metadata or a separately reviewed byte annotation procedure, record the method, then make the scanner assert those fixed offsets. The scanner must never populate or rewrite expected offsets.

3. Verify exact identity independently.

   - Slice original bytes at manifest offsets and assert all spans are in range, correctly nested, and root consumption equals input length.
   - For every positive body, compare true Blake2b-256 against either the ledger-produced transaction ID or a second true Blake2b-256 implementation from an independent codebase. Where both are available require both. Explicitly reject computing Blake2b-512 and truncating it; that is a different algorithm and cannot satisfy evidence.
   - Use `cbor-diag` only for diagnostic/annotated inspection. Never use semantic decode/re-encode to establish fixture bytes, offsets, or hashes.

4. Add only targeted policy and SDK probes.

   - Keep one or a few vectors per decision family: complete/trailing root; definite/indefinite; minimal/non-minimal integer/length; reordered map; exact and byte-distinct duplicate; tag 258/no tag/wrong tag; tag-24 exact/trailing embedded item; one other allowed/wrong-location tag; and each accepted legacy family needed to identify an SDK representation gap.
   - Record strict expected wire result and each SDK's parse/preserve/normalize/represent result. Task-302 later turns the frozen table into production structural tests. Task-303 adds exhaustive semantic alternatives/uniqueness/commitment tests. Task-800 adds systematic mutations, differential checks, deep malformed data, property tests, and fuzzing.

5. Run one reproducible installed/candidate harness.

   - `scripts/exact-cbor-sdk-probe.cjs --sdk-root <absolute package root> --manifest <path> --output <path>` loads only the public CommonJS entry from the supplied root, verifies package identity before cases, runs the same ordered manifest cases, and never relies on worktree module resolution for the candidate.
   - Normalize output as JSON `{schemaVersion, runtime, sdk:{name,version,gitHead,tarball,shasum,integrity,moduleRoot}, cases:[{id,parse,fullConsumption,preservedExact,toCbor,toCore,representedFields,errorClass}], summary}`. Sort cases by ID; use fixed enum/string/null values; omit stack traces and absolute temporary paths from committed results.
   - Installed run uses the worktree package and lock identity above. Candidate run downloads the fixed `0.47.0` tarball into `/tmp/opencode`, verifies SHA-512 integrity and SHA-1 shasum before extraction, installs its exact peer/transitive tree under a generated frozen lockfile, commits that lockfile or an equivalently complete immutable dependency manifest with the results, records Node/Yarn versions and lock hash, and passes that package root to the same harness.
   - Strict acceptance remains independent of both SDKs. `fullConsumption` is the strict harness result, not an SDK claim.

6. Make one dependency decision under a no-migration gate.

   - Choose exactly one: retain `0.41.4`; retain it as a non-authoritative helper with a named task-303 gap; or update exactly to `0.47.0`.
   - Update only if `0.47.0` closes a named required task-303 semantic representation gap, all current SDK consumers compile and focused tests pass, Node `>=22`, TypeScript 4.9, CommonJS/Webpack/Electron bundling and runtime loading pass, no lock drift is unrelated, and no source/API consumer migration outside task-004 is required.
   - If any import, type, behavior, or consumer needs migration beyond changing the exact dependency/lock and adapting the test-local harness, retain `0.41.4`, record the result, and leave migration to a separately planned task. Newer availability, stricter parsing, or private byte retention alone cannot justify the update.

7. Record statuses without promotion.

   - Use separate columns: `wire_fixture_recognized`, `sdk_represented`, `backend_planned`, `backend_implemented`, `production_parser_implemented`, `product_supported`.
   - Task-004 may set Conway only to `wire_fixture_recognized=yes`, an evidence-based SDK value, `backend_planned=yes`, and all implementation/product columns `no`; its conclusion is `conditional Conway readiness`, not support.
   - Dijkstra must conclude `unsupported/readiness-blocked`, with SDK probe results informational and implementation/product columns `no`.
   - Dijkstra promotion requires a new explicitly owned tracker item approved by the PRD/task-graph owner; pinned final CDDL/decoder/goldens; tasks 200/201/206/209 backend implementation and exact-point parameter evidence; task-302 envelope/span support; task-303 complete semantics/uniqueness/commitments; task-304 context reconciliation; task-800 differential/fuzz evidence; task-301 capability gating; and task-805/807 security/dependency approval. Only that tracker item may change `product_supported`.

8. Synchronize after review.
   - Keep research, manifest, fixtures, harness results, dependency decision, and status table consistent.
   - After implementation review approval, add only a concise PRD pointer/result and truthful task-004 tracker completion metadata. Do not alter task ownership or claim implementation from planning evidence.

## Acceptance Criteria

- The frozen CBOR table states exact decisions for definite/indefinite forms, minimal/non-minimal integers and lengths, tag 258, tag 24/embedded CBOR, every other permitted tag family, map ordering, legacy forms, and full consumption.
- Exact-byte duplicates and byte-distinct semantic duplicates are distinguished. Task-302 owns structural duplicate rejection; task-303 owns decoded semantic uniqueness, with only targeted task-004 evidence and task-800 mutation breadth.
- Every positive fixture has immutable pinned-ledger bytes or reproducible pinned-ledger serialization provenance, independently established manifest offsets, and scanner assertions that cannot generate expected values.
- Every body hash matches a ledger-produced transaction ID or a second true Blake2b-256 implementation; Blake2b-512 truncation is explicitly rejected.
- The corpus is limited to exact envelope/body/output span vectors, a machine-checkable source-derived Conway field/union inventory, and targeted SDK/policy probes. Exhaustive semantics remain task-303 and differential/fuzz mutation remains task-800.
- Installed `0.41.4` and exact candidate `0.47.0` run through one module-root-parameterized harness with verified package identity/integrity and deterministic normalized results.
- Dependency update occurs only under the named semantic-gap, compatibility, focused-consumer, exact-lock, and no-migration gate; otherwise package and lock remain unchanged.
- Statuses separately report wire recognition, SDK representation, backend planned/implemented, production parser implemented, and product support. The only task-004 era conclusions are conditional Conway readiness and Dijkstra `unsupported/readiness-blocked`.
- The protocol matrix covers fees, max transaction/value sizes, min UTxO, collateral percentage/count, execution prices/limits, cost models/language views, reference-script fees/limits, deposits, governance parameters/state, protocol version, and Dijkstra additions, with exact-chain-point source and downstream owner.
- No arbitrary live protocol-parameter fetch or live-network policy claim is made. Later tests preserve intended network/genesis/chain point and full parameters with transaction artifacts.
- No production parser/model, backend, hardware, renderer, IPC, speculative abstraction, exhaustive fuzz corpus, or review-log edit is introduced.

## Verification

- Run `yarn test:jest source/common/cardano/exactCborValidation.spec.ts --runInBand --coverage=false`.
- Run the parameterized harness for installed and candidate roots; validate both result files against the normalized schema and compare ordered case IDs.
- Verify candidate tarball integrity/shasum/gitHead/version before extraction and record generated candidate lock hash and runtime versions.
- Regenerate the Conway/Dijkstra inventory from the exact pinned source and fail on any unaccounted field, union alternative, tag, legacy form, or protocol-parameter delta.
- For every positive fixture, verify immutable provenance/artifact hash, independent extraction metadata, fixed manifest offsets, exact slices, complete root/embedded consumption, and true Blake2b-256 cross-check.
- Decode committed fixtures with `cbor-diag --from hex --to diag`; inspect targeted boundaries with `--to annotated`; do not round-trip goldens.
- Validate manifest uniqueness, provenance, policy/status enums, matrix references, and downstream-owner fields.
- If dependencies do not change: run `yarn compile`, focused ESLint/direct Prettier for added code/data/docs, JSON parsing, and `git diff --check`.
- If dependencies change: additionally run `yarn install --frozen-lockfile`, compile, build main and renderer bundles, task-002 contract tests, all existing direct SDK-consumer tests including data serialization and available Ledger/Trezor utilities, and inspect lock drift. Any required non-harness consumer migration fails the update gate and reverts the proposed dependency outcome before review.
- Parse the tracker and inspect the final diff for review-log edits, production parser/model files, large copied goldens, secrets, sibling changes, unrelated statuses, or support claims beyond the status table.
- No Storybook, Cucumber, Electron E2E, physical hardware, configured-network transaction, or user manual QA is required.

## Risks And Open Questions

- Pinned-source mapping: package versions may not map trivially to one repository commit. Completion requires an immutable package/source artifact relationship; unresolved mapping blocks positive provenance rather than allowing moving `master` evidence.
- Ledger compatibility versus strictness: CDDL and actual decoder acceptance can differ. The narrower fail-closed intersection controls unless immutable compatibility evidence requires a broader form, and the research records the decision.
- Semantic duplicate ambiguity: byte equality is insufficient. Task-004 records the boundary; task-303 must decode and enforce uniqueness before review completeness.
- Candidate blast radius: `0.47.0` may be semantically better but incompatible with old consumers/tooling. The no-migration gate deliberately prefers retention over scope expansion.
- Dijkstra churn: source and package behavior may change. Its blocked status and explicit promotion tracker prevent accidental enablement.
- Protocol drift: values vary by network and chain point. Exact-point backend context, not planning-time examples or random goldens, controls runtime validation.

## Docs, Tracking, And Research Updates

- Add `research/04-exact-cbor-era-coverage.md`, the narrow fixture manifest/corpus, focused validation spec, and parameterized SDK harness.
- Update the PRD only with the accepted dependency decision, conditional Conway/Dijkstra disposition, and artifact links after review.
- Update task-004 tracker completion metadata only after implementation review approval; preserve dependencies and unrelated statuses.
- Update this plan's lifecycle/evidence as orchestration proceeds. Do not update architecture/API/workflow docs because this spike ships no architecture or API.
- Do not edit either review log.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-004-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-004-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Blocker 1: froze all requested encoding families, separated structural from semantic duplicates, and assigned semantic uniqueness/exhaustive evidence to tasks 303/800.
- Blocker 2: made pinned-ledger positive provenance, independently established offsets, ledger/second-implementation true Blake2b-256, and rejection of Blake2b-512 truncation mandatory.
- Blocker 3: reduced the corpus to span vectors, source-derived inventory, and targeted probes; exhaustive recognition/commitments and mutation/fuzzing remain downstream.
- Blocker 4: fixed candidate `0.47.0` identity/integrity, one module-root harness/result schema, and a strict no-consumer-migration update gate.
- Blocker 5: separated six readiness/support statuses, limited conclusions to conditional Conway and blocked Dijkstra, and named later promotion evidence/tracker ownership.
- Blocker 6: added the complete protocol/context matrix, exact-point `W` source, downstream task/test owners, and explicit no-arbitrary-live-fetch rule.
- Scope/lifecycle: no production task 302/303/800 work moved into task-004, no hidden manual checkpoint exists, and implementation has not started.

## Implementation Evidence

- Added the machine-readable wire policy, source-derived Conway inventory, pinned Dijkstra deltas, protocol-context ownership, exact spans, normalized SDK cases/results, era statuses, and dependency decision in `source/common/cardano/fixtures/exact-cbor/manifest.json`.
- Added two immutable ledger-proven fixtures. The 346-byte Conway regression proves exact body, witness, `isValid=false`, two ordinary output, collateral-return, and null auxiliary-data spans. The 865-byte historical ledger golden proves exact `isValid=true` and tag-259 auxiliary-data spans while correctly failing the Conway policy because it contains retired body key 6.
- Added an unexported test-local CBOR scanner in `source/common/cardano/exactCborValidation.spec.ts`. It asserts fixed offsets, complete root consumption, known Conway body keys, structural duplicate handling, input set tagging, fixture SHA-256, and true Blake2b-256 agreement between `blakejs` and the separate `blake2b` package.
- Added `scripts/exact-cbor-sdk-probe.cjs` and deterministic results for installed SDK `0.41.4` and isolated candidate `0.47.0`. Both represent the required Conway fixture; neither supplies the strict wire boundary. Candidate `0.47.0` accepts four strict-reject cases versus two for `0.41.4` and closes no demonstrated task-303 semantic gap.
- Dependency decision: retain `@cardano-sdk/core@0.41.4` as a non-authoritative helper. `package.json` and `yarn.lock` are unchanged.
- Era decision: conditional Conway fixture/inventory readiness only; Dijkstra remains `unsupported/readiness-blocked` with no backend, production parser, or product support claim.
- Pinned implementation evidence corrected planning-time moving-branch assumptions: `cardano-ledger-dijkstra-0.2.0.1` adds body keys `23`, `25`, and `26` and protocol-update keys `34` through `37`. The accepted manifest and research use this narrower pinned inventory.
- Durable findings and reproduction commands are recorded in `.agent/plans/dapp-browser-cip30/research/04-exact-cbor-era-coverage.md`.
- Review iteration 1 added a generated `source-inventory.json` that preserves complete pinned CDDL definition blocks and structured numeric maps. `scripts/extract-cardano-cddl-inventory.cjs` regenerates it byte-for-byte from the two immutable tag checkouts.
- Review iteration 1 added ledger-accepted Babbage map-output and untagged body-set fixtures, independent `cbor-diag` annotation hashes/ranges, and targeted policy cases for container form, width, order, duplicate, tag, embedded-CBOR, and legacy-representation boundaries.
- Review iteration 1 committed the complete candidate npm lock and added `scripts/setup-exact-cbor-sdk-candidate.cjs` for empty-directory setup, `npm ci`, tarball SHA-1/SRI verification, and installed version/git-head verification. The probe now verifies the candidate lock hash, measures root consumption, and reports only defined semantic fields.
- Review iteration 2 replaced the selected CDDL definition list with extraction of every top-level definition and source-derived certificate, governance-action, and native-script discriminants. Manifest summary tags are asserted against that generated source evidence, and required Plutus-data, metadata, withdrawal, mint, required-signer, and witness definitions are coverage-checked.
- Review iteration 2 added location-aware vectors for non-minimal lengths, empty non-empty sets, valid and invalid tag-24 output locations, tag 30 in an invalid output-value location, both redeemer forms, raw metadata, and Allegra auxiliary arrays. The test-local oracle now validates output field/tag locations, redeemer shapes, and all Conway-permitted auxiliary-data forms.
- Review iteration 3 extended top-level CDDL extraction to multiline generic headers and asserts the previously omitted `constr<a0>` definition. It also composes non-empty cardinality and exact tag-24 embedded-item consumption into full Conway body/output locations, with dedicated collateral-input, reference-input, inline-datum, and script-reference vectors.

Verification completed before implementation review:

- `yarn test:jest source/common/cardano/exactCborValidation.spec.ts --runInBand --coverage=false`: 46 tests passed.
- `yarn compile`: passed, including generated Sass typings and `tsc --noEmit`.
- Focused ESLint and direct Prettier checks: passed.
- Installed/candidate SDK probe regeneration matched the committed normalized results byte-for-byte.
- Both committed CBOR fixtures decoded successfully with `cbor-diag --to diag --seq`; all JSON artifacts parsed; candidate tarball SHA-1/SRI and temporary lock SHA-256 matched the manifest.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Completed. Implementation review iteration 4 approved the pinned-source inventory, exact-span and strict-policy evidence, reproducible SDK comparison, dependency retention, conditional Conway readiness, and blocked Dijkstra disposition. The PRD and tracker are synchronized without claiming production parser, backend, or product support.
