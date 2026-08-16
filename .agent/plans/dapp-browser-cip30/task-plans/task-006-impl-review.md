Implementation: Iteration 1
Timestamp: 2026-08-14T23:05:33Z

Changes made:
- Added revision `task-006-matrix-2026-08-14` with orthogonal static/evidence/implementation/enablement dimensions, complete Conway body-key and certificate-tag partitions, exact-CBOR family fail-closed dispositions, immutable Ledger/Trezor artifact identities, exact public errors, and downstream ownership.
- Added a deterministic no-device package probe plus committed Ledger 7.1.4, isolated Ledger 8.0.0, and Trezor Connect 9.7.2 normalized results; committed the exact Ledger 8 dependency lock and complete Trezor runtime/config/protobuf/model/dual-transport identities.
- Added focused Jest coverage and task-607 case/evidence contracts, including schema-validated positive and forbidden privacy examples, synthetic-only task-002 golden equality, physical structural/cryptographic conformance, vendor COSE non-pass-through, and distinct product-versus-hardware limits.
- Added research 07, corrected stale hardware/test workflows, updated PRD hardware boundaries, and rewrote tasks 006 and 600-607 so static implementation, physical certification, and later product enablement have separate owners.

Files touched:
- `source/common/hardware/**`
- `scripts/hardware-wallet-capability-probe.cjs`
- `hardware-wallet-tests/capability-matrix/**`
- `.agent/plans/dapp-browser-cip30/research/07-hardware-wallet-capability-contract.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `.agent/workflows/hardware-wallets.md`
- `.agent/workflows/test.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-006.md` and required review logs

Verification run:
- Focused hardware matrix Jest: 9/9 passed.
- Frozen task-002 CIP-30/CIP-8/CIP-95 Jest: 10/10 passed.
- Frozen task-004 exact-CBOR Jest: 46/46 passed.
- `yarn compile`: passed.
- Focused ESLint: passed.
- Direct Prettier check: passed.
- Probe regeneration and byte comparison for all three artifacts: passed.
- JSON parsing, candidate/runtime digest checks, `node --check`, and `git diff --check`: passed.

Deviations from approved plan:
- Added `evidence-examples.json` as a minimal schema-validation fixture; this narrows verification and does not add runtime surface.
- No production dependency, adapter, IPC, device, or product-enable changes were made. An accidental repository-wide formatter invocation was isolated and its unrelated formatter-only churn was removed before review.

User interaction required: no. The prior user authorization affected only the planning-loop correction; task-006 remains autonomous and task-607 retains every physical/manual checkpoint.

Outcome: Implementation complete and ready for code review

Code Review: Iteration 1
Timestamp: 2026-08-14T23:09:30Z

Outcome: The implementation preserves the no-production-change and static-versus-physical boundary, but the frozen matrix, probes, certification cases, and evidence schema do not satisfy the approved completeness, provenance, or fail-closed acceptance criteria.

Blocking findings:
1. Critical: Ledger certificate coverage is factually incorrect. `manifest.json` marks Conway certificate tags 10-13 representable for Ledger 7.1.4 and candidate 8.0.0, but the installed Ledger public contract omits those combined delegation certificates. The probe reduces all Conway certificates to one string check and commits no source-derived per-tag inventory.
2. Critical: The manifest is not the required field/alternative matrix. It partitions only numeric body keys/certificate tags, omits named nested alternatives and response proofs/cardinalities, and supplies one generic exact-body default rather than a disposition per vendor, operation, and representation family.
3. Critical: The task-607 case set has only generic cases and lacks vendor-row references, model/app/firmware predicates, paths/credentials, signing/display mode, body/payload/hash identity, response shape, physical requirements, and broad field/model/error/cancellation coverage.
4. Critical: The evidence schema permits impossible records. It does not couple vendor/model/version/transport/case/operation/outcome/proof and can approve false proof booleans. It lacks proof digests/cardinality, normalized operation/error classification, separate library identity, and transaction/message conditionals.
5. High: Generated probes make type/string-presence claims rather than approved compatibility claims. They do not identity-gate loading, exercise public/deep imports or validators, compile an isolated consumer, run compatibility builds, or reconstruct a body/hash, yet the manifest recommends Ledger 8 and records deterministic pass.
6. High: Immutable artifact/transitive provenance is incomplete. Probe results are not bound to tarball/lock/source identity, and the Trezor identity is selected files rather than complete resolved graph, lock/config/runtime assets, and a demonstrated resolution trace.
7. High: Model/app/firmware and CIP-8/CIP-95 capability rows are missing, including Ledger model/app v7/v8, Trezor internal model/firmware, Trezor One rejection, Ledger 7 messages, and separate payment/stake/direct-DRep/type-6-DRep identity/result rules.
8. High: The operation-specific error contract collapses approved predicates and leaves malformed inputs, incompatibility, wrong app/path, cancellation causality, app close/lock, proof classes, exceptions, stale-before-settlement, and late outcomes open to reinterpretation.
9. High: Focused tests validate submitted shape rather than approved invariants. They lack a manifest schema, impossible-state mutation, generated inventory comparison, case schema/coverage, complete fixture/digest/limit/error/cancellation proof, and therefore do not establish completeness.
10. Medium: Research 07 labels evidence accepted and overstates field coverage and enforcement before review approval.

Non-blocking observations:
- Production dependencies, IPC, hardware types/adapters/stores, and runtime behavior remain unchanged.
- Static rows remain physically uncertified, unimplemented, and disabled; ownership separation is correct.
- Trezor COSE prohibition, synthetic-only golden equality, closed-schema direction, and product-versus-hardware limits are directionally correct.
- Existing focused tests and checks pass but are too shallow to clear the blockers.

Approval bar:
- Replace aggregate claims with source-derived per-artifact field/nested-alternative inventories, correct Ledger certificates, and explicit per-vendor/operation exact-family dispositions.
- Make the Ledger recommendation reproducible from identity-verified import, validator, isolated compile/build, and current-consumer compatibility evidence; bind probes to immutable identities.
- Add complete model/app/firmware and message rows while retaining physical/product disabled states.
- Expand task-607 cases/schema across every capability, representation, model/version, limit, proof, error, cancellation, and late-result boundary with conditional impossible-state rejection.
- Strengthen tests so omissions, contradictions, incomplete cases, invalid evidence, provenance drift, COSE pass-through, and error drift fail mechanically.
- Keep production dependencies, runtime hardware paths, IPC, device operations, and product enablement unchanged.

Decision: requires_changes


Implementation: Iteration 2
Timestamp: 2026-08-14T23:37:21Z

Changes made:
- Replaced aggregate package claims with source-derived transaction field, certificate, nested-alternative, response-proof, model/version, message-mode, exact-body-family, request-limit, error, cancellation, and ownership rows; corrected Ledger 7 certificate tags 10-13 to non-representable.
- Identity-gated all probes, committed complete candidate/runtime provenance, exercised Ledger 8 public and Daedalus deep imports, isolated TypeScript consumers, and main/renderer builds, and recorded the three bounded task-600 hex_to_buf migrations.
- Expanded task-607 descriptors and closed schemas with exact capability/model/version/path/mode/identity/proof/error/limit references, operation-specific outcome/error coupling, artifact/library/version coupling, privacy rejection, impossible-state mutations, and reviewer-gated physical evidence.
- Made probe and case generation deterministic and Prettier-compliant, added explicit 65,536/65,537 and smaller-hardware-limit cases for both operations, and strengthened mechanical completeness/provenance/COSE/ownership tests.
- Kept research status pending review and retained physical certification, implementation, product enablement, production dependencies, runtime hardware paths, IPC, and device operations out of task-006.

Verification run:
- Focused task-002/task-004/task-006 Jest: 65/65 passed.
- `yarn compile`: passed.
- Focused ESLint: passed.
- Direct Prettier check for every task-local non-review artifact: passed.
- Probe/case regeneration and byte comparison: passed.
- Ledger 8 main and renderer compatibility builds: passed; isolated consumer diagnostics are the three expected task-600 migrations.
- Script syntax checks and `git diff --check`: passed.
- Repository-wide Prettier remains red on pre-existing unrelated files; task-local files pass the direct check.

Deviations from approved plan:
- Added `scripts/hardware-wallet-ledger-consumer-probe.cjs`, `manifest.schema.json`, `cases.schema.json`, candidate/runtime identity files, and closed evidence examples as the smallest artifacts needed to make the approved compatibility, completeness, and impossible-state checks executable.
- No production dependency, adapter, service, IPC, shared hardware type, store, device, or product-enable change was made.

User interaction required: no. Physical and manual evidence remains exclusively task-607.

Outcome: Iteration-1 review blockers addressed; ready for code review

Code Review: Iteration 2
Timestamp: 2026-08-14T23:40:41Z

Outcome: Iteration 2 preserves the production boundary and corrects Ledger 7 certificate tags 10-13, but substantial matrix, schema, case, provenance, and executable-evidence acceptance criteria remain unresolved.

Blocking findings:
1. Critical: The matrix remains aggregate and exact-body claims are not executable. Nested alternatives/cardinalities/response proofs are incomplete, dispositions are conditional rather than the frozen enum, and no probe reconstructs a body/hash despite deterministic pass rows.
2. Critical: Ledger app and Trezor firmware/model predicates are incomplete or contradictory. Ledger message signing needs the 7.1 gate, Ledger 8 needs app-v7 and app-v8 rows, and Trezor needs source-derived 2.6.0 transaction and 2.9.1 message gates without aggregate support overclaim.
3. Critical: Manifest and evidence schemas admit impossible records. Evidence is not coupled across case/artifact/vendor/operation/model/version, passing transaction proof cardinality and key association are weak, and reviewer rejection can coexist with pass.
4. Critical: Task-607 descriptors still use generic placeholders and omit exact fixture/digest/model/version/path/credential/response bindings plus nested/cardinality/proof-corruption/vendor-specific failure coverage.
5. High: Recommendation and provenance remain caller-asserted. Probe identity arguments do not verify tarballs, consumer build pass/restoration flags are accepted from the caller, research reproduction is incomplete, and Trezor runtime resolution is declarative.
6. High: Tests prove counts/self-consistency rather than validator, reconstruction, case-binding, golden COSE, tracker, and full fail-closed invariants.
7. High: Vendor cancellation not caused by host cancellation or explicit refusal is missing from the frozen operation-specific ProofGeneration mapping.

Non-blocking observations:
- No prohibited production dependency, runtime, IPC, device, adapter, shared type, store, or product-enable changes were introduced.
- Ledger 7 certificate correction, pending research status, workflow corrections, physical ownership, COSE non-pass-through direction, privacy closure, and tracker ordering are directionally correct.

Approval bar:
- Replace aggregate/type-presence claims with source-derived nested inventories and exact app/firmware rows; freeze unconditional exact-body dispositions and executable proof or rejection.
- Couple both schemas and generated cases across exact fixtures, rows, versions, operations, outcomes, proofs, and reviewer approval.
- Make acquisition, identity verification, imports, validators, compilation/builds, runtime resolution, and regeneration executable instead of caller-asserted.

Decision: requires_changes

Implementation: Iteration 3
Timestamp: 2026-08-15T19:59:13Z

Changes made:
- Froze every transaction exact-body family to unconditional `reject_pre_device` and changed unexecuted transaction reconstruction probes to `not_run`; no semantic vendor API is represented as exact-body proof.
- Replaced coarse model/version claims with Ledger app-v7/app-v8 routing and the source-derived 7.1 message gate, plus exact Trezor T2T1/T2B1/T3B1/T3T1/T3W1 transaction and message minimums, T1B1 rejection, and T3W1 message unresolved status.
- Expanded nested request/cardinality and response-proof inventories, added payment/stake/direct-DRep/type-6-DRep fixture bindings, and generated concrete per-model cases for fields, certificates, exact families, nested alternatives, limits, both-operation errors, cancellation causality, and message modes.
- Generated grouped case-to-evidence schema bindings for exact artifact/library/lock/model/version/operation/input/outcome/error identity; enforced pass/reviewer/proof invariants and mutation rejection without the previous AJV stack overflow.
- Removed caller-asserted identity and build flags. Probes now derive lock identities, verify package trees/tarball SRI, generate the complete Node-resolved Trezor dependency graph, exercise Ledger app validators/imports, and derive main/renderer webpack outcomes from aliased Node-API builds.
- Added direct synthetic CIP-8 Sig_structure reconstruction and Ed25519 verification, exact fixture digest checks, committed case/schema regeneration equality, complete runtime-graph regeneration, and retired-flag rejection tests.
- Preserved all production dependency, runtime, IPC, adapter, device, physical-certification, and product-enablement boundaries.

Verification run:
- Focused task-006 Jest: 13/13 passed.
- Frozen task-002/task-004 Jest: 56/56 passed; combined related total 69/69.
- `yarn compile`: passed.
- Focused ESLint: passed.
- Direct task-local Prettier check: passed.
- Ledger 7, Ledger 8, Trezor package/runtime, task-607 cases/schema/examples, and Ledger 8 consumer/build regeneration byte comparisons: passed.
- Ledger 8 aliased main/renderer webpack builds: passed from webpack callbacks; candidate consumer retains exactly three expected TS2339 task-600 migrations.
- `git diff --check`: passed.
- Repository-wide Prettier remains red only on pre-existing unrelated baseline files; every task-local artifact passes the direct check.

Deviations from approved plan:
- Transaction exact-body support is frozen more conservatively than initially anticipated: all current semantic vendor paths reject pre-device because task-006 has no executable exact-body reconstruction. A later reviewed matrix revision is required after adapter-owned reconstruction evidence exists.
- Generated task-607 cases and evidence bindings are intentionally large because every model/version/capability/error boundary is explicit and machine-coupled; they remain generated from the compact manifest and script.
- No prohibited production or physical-device work was performed.

User interaction required: no. Physical execution and reviewer evidence remain exclusively task-607.

Outcome: Iteration-2 review blockers addressed; ready for code review

Code Review: Iteration 3
Timestamp: 2026-08-15T20:03:13Z

Outcome: Iteration 3 materially improves exact-body fail-closed behavior, version gates, evidence coupling, and executable build provenance, but five static-state, case-input, proof-state, and transitive-provenance blockers remain.

Blocking findings:
1. Critical: Trezor model message pass cases bypass the unresolved artifact-level message state, and model operation rows do not carry the six orthogonal dimensions required for the fail-closed intersection.
2. Critical: Generated cases still reuse one transaction fixture or symbolic limit labels rather than concrete field-bearing/boundary-sized/path-bound inputs and complete input digests.
3. Critical: Case-to-evidence bindings do not enforce each case proof required/forbidden set; a pre-device rejection can carry positive proof booleans, returned digests, and witnesses and still validate.
4. High: Trezor runtime graph nodes lack per-node lock resolved URL/integrity, and task-607 evidence is not bound to the complete runtime/config graph identity.
5. High: Firmware/model gates and nested inventories are manually restated rather than extracted from the hashed vendor gate/schema/parser sources; tests repeat expected values rather than comparing generated inventory.

Non-blocking observations:
- All transaction exact-body families now reject before device interaction; corrected certificate and firmware facts pass focused tests.
- No prohibited production dependency, IPC, adapter, shared type, store, device, or product behavior changed.
- PRD phase wording still contains a stale upgrade-and-certify phrase.

Approval bar:
- Couple artifact and model operation states across all six dimensions and suppress execution when any prerequisite is unresolved or unpassed.
- Generate concrete field/limit/path inputs and enforce case-specific proof states.
- Add lock URL/integrity to every Trezor runtime node, bind physical evidence to the runtime/config graph digest, and extract gates/nested inventory from hashed sources.

Decision: requires_changes

Implementation: Iteration 4
Timestamp: 2026-08-15T22:46:47Z

Changes made:
- Added all six orthogonal dimensions to every model/operation row and generated each case from the fail-closed intersection with its artifact operation row; unresolved/not-representable rows cannot become physical pass cases.
- Changed the Trezor artifact message surface to source-probed representable/pass while retaining exact per-model T1B1 rejection and T3W1 unresolved state; adapter, physical, emulator, and product dimensions remain not implemented/not_run/disabled.
- Added deterministic concrete input recipes with canonical JSON, complete digests, exact CBOR field/tag bytes, exact authoritative fixture/mutation bindings, explicit derivation path arrays and credential predicates, synthetic-versus-physical key separation, and exact repeated-byte 65,536/65,537 boundary payloads.
- Bound evidence to every case required/forbidden proof field, runtime graph/config identities, exact state row, input digest, outcome, and error. All rejection cases now require false proofs, null returned digests, zero witnesses, and no vendor COSE pass-through.
- Bound every Node-resolved Trezor transitive runtime node to its yarn.lock selectors, resolved URL, SHA-1/SRI identity, and package tree; lock identity now participates in the runtime graph digest.
- Extended probes to extract normalized model gates, request/nested/cardinality inventories, and response-proof shapes from hashed Ledger/Trezor source/config/schema files; Jest compares manifest claims to generated probe output.
- Updated stale PRD wording to static recommendation plus task-607 physical certification.

Verification run:
- Focused task-006 Jest: 14/14 passed.
- Prior frozen task-002/task-004 Jest: 56/56 passed.
- `yarn compile`: passed.
- Focused ESLint, task-local Prettier, script checks, and `git diff --check`: passed.
- Package/runtime/case/schema regeneration and candidate aliased webpack evidence remain deterministic and byte-identical.

Deviations from approved plan:
- Current adapterImplementation remains not_implemented, so generated message/device cases are fail-closed pre-device descriptors rather than premature physical pass claims. Task-607 can execute physical certification only after downstream adapter prerequisites revise the state.
- Pre-device transaction field/tag recipes intentionally use exact minimal CBOR values because the unconditional capability gate is evaluated before vendor semantic translation; no exact-body support is inferred from those recipes.

User interaction required: no. Physical device evidence remains task-607.

Outcome: Iteration-3 review blockers addressed; ready for code review

Code Review: Iteration 4
Timestamp: 2026-08-16T14:19:48Z

Outcome: Iteration 4 is fail-closed and proof-safe, but the six-dimensional promotion path, executable transaction recipes, and device identity separation remain incomplete.

Blocking findings:
1. Critical: The generator intersects only representability/probe and unconditionally converts representable signData to pre-device rejection; frozen cases cannot become task-607 physical pass evidence after downstream prerequisite promotion.
2. Critical: Transaction recipes use null body values, incomplete certificate arrays, descriptor-object mutations, and raw repeated bytes that would fail as malformed input before the claimed capability/size gate.
3. Critical: Fixed task-002/wire identities are paired with hard-coded physical derivation paths without key ownership evidence, and Trezor request parameters are incomplete.
4. High: The complete worktree contains unrelated production/runtime changes outside task-006. These files pre-existed or changed concurrently and must be excluded from the task commit/review scope rather than modified or reverted.

Non-blocking observations:
- Rejection proof mutations are now rejected.
- All Trezor runtime graph nodes carry lock URL/SRI/tree identity and graph/config digests are evidence-bound.
- Probe-derived gates and nested inventories match committed output.

Approval bar:
- Implement all-six-dimension intersection plus a frozen conditional task-607 promotion branch.
- Use valid transaction envelopes or truthful static-source assertions; generate valid exact-size transaction payloads.
- Separate synthetic golden vectors from device-derived physical identity and bind concrete vendor request templates.
- Stage/commit only the task-006 allowlist; do not include unrelated dirty-worktree files.

Decision: requires_changes

Implementation: Iteration 5
Timestamp: 2026-08-16T14:19:48Z

Changes made:
- Implemented true artifact/model intersection for all six dimensions and added conditional certification targets with stable case IDs, exact prerequisite-state digests, nonzero adapter-commit requirements, target proof requirements, and promoted evidence branches.
- Replaced malformed transaction recipes with valid authoritative transaction inputs where the fixture demonstrates the claim and truthful source-probe assertions for unsupported fixture claims; no static assertion is represented as transaction or hardware execution.
- Generated structurally validated 65,536- and 65,537-byte Conway transaction envelopes in a committed deterministic input-recipe fixture; removed the invented 1,024-byte hardware limit case.
- Split exact task-002 COSE bytes into synthetic-golden nonphysical cases. Physical message recipes now digest only path/payload/vendor request templates and require device-derived identity/public-key/signature outputs under explicit credential-role association rules.
- Added concrete Ledger/Trezor request templates, runtime/config binding, current rejection evidence branches, and promoted physical pass branches that require prerequisite attestations and positive proofs.
- Flattened promoted evidence alternatives before grouping to retain practical AJV compilation and deterministic schema generation.

Verification run:
- Focused task-006 Jest: 14/14 passed.
- Cases schema validation, generated case/schema/example/input-recipe byte regeneration, focused formatting, and `git diff --check`: passed.
- Prior compile, related 56 task-002/task-004 tests, provenance/build regeneration, and focused lint remain passed.

Deviations from approved plan:
- Claims without a complete authoritative transaction fixture are frozen as source-probe assertions, not fabricated hardware requests.
- Physical message identity is intentionally an execution-time bound output; task-006 freezes deterministic path/payload/credential/request rules without touching a device.
- Unrelated dirty-worktree production/runtime files are excluded from task-006 and were not modified or reverted during this correction.

Outcome: Iteration-4 review blockers addressed; ready for code review

Code Review: Iteration 5
Timestamp: 2026-08-16T15:21:08Z

Outcome: Iteration 5 preserves provenance/promotion safety but has four bounded validity and ownership blockers.

Blocking findings:
1. Critical: Exact-size transaction fixtures use metadata text values above the Cardano 64-byte metadatum limit; generic CBOR round-trip validation does not catch this.
2. Critical: The six-dimension test helper omits emulatorEvidence from product-enabled validation.
3. High: Promotable Ledger/Trezor message templates contain inconsistent branches or non-vendor placeholders instead of concrete address/network/addressParameters templates.
4. High: Static source assertions are assigned to task-607 even though the physical evidence outcome enum intentionally excludes static-source-assertion.

Non-blocking observations:
- Current/promoted proof and prerequisite mutations reject correctly.
- Stable case IDs, source/lock/runtime provenance, cross-key separation, and source assertion downgrades are correct.

Approval bar:
- Use ledger-valid <=64-byte metadata components and assert those limits.
- Enforce emulator evidence in the six-state invariant.
- Freeze internally consistent vendor request templates with explicit device-derived output bindings.
- Assign static assertions to tasks 602/603 and task-607 only executable/promotable cases.

Decision: requires_changes

Implementation: Iteration 6
Timestamp: 2026-08-16T15:21:08Z

Changes made:
- Rebuilt exact-size Conway envelopes from metadata lists whose every text item is <=23 bytes and added generator/runtime checks enforcing the Cardano 64-byte metadata text/bytes maximum, canonical round-trip, and exact 65,536/65,537 lengths.
- Added emulatorEvidence to product-enabled validation and explicit positive/negative six-dimension mutation coverage.
- Replaced Trezor request placeholders with concrete CardanoAddressType values and exact path/stakingPath addressParameters for base, reward, and enterprise modes; corrected Ledger address-vs-key-hash routing and bound dynamic address parameters by exact path/role plus network.
- Added explicit evidenceOwner to every case. Ledger/Trezor static-source assertions are owned by tasks 602/603; task-607 owns only executable/promotable cases, and the evidence schema enumerates only task-607-owned case IDs.
- Updated task-602/task-603 acceptance and task-607 description to freeze that ownership boundary.
- Restored byte-for-byte regeneration checks for cases, evidence schema/examples, and exact-size input recipes.

Verification run:
- Focused task-006 Jest: 14/14 passed, including exact metadata component limits and all generated artifact byte comparisons.
- Focused ESLint, task-local formatting, and `git diff --check`: passed.
- Prior compile, frozen task-002/task-004 tests, provenance/build regeneration, and candidate webpack checks remain passed.

Outcome: Iteration-5 review blockers addressed; ready for code review

Code Review: Iteration 6
Timestamp: 2026-08-16T16:30:13Z

Outcome: Iteration 6 resolves metadata, emulator, and ownership blockers; one Ledger request-shape blocker remains.

Blocking finding:
1. High: Ledger address-mode templates use abstract address descriptors rather than vendor-valid DeviceOwnedAddress structures; all address-mode templates fail the installed parser.

Non-blocking observations:
- Exact-size fixtures, static ownership, promoted evidence, provenance, and no-production boundaries pass review.

Approval bar:
- Freeze concrete base/reward/enterprise DeviceOwnedAddress templates and validate every Ledger template with both 7.1.4 and candidate 8.0.0 parsers.

Decision: requires_changes

Implementation: Iteration 7
Timestamp: 2026-08-16T16:30:13Z

Changes made:
- Replaced Ledger address descriptors with exact vendor DeviceOwnedAddress requests: base type 0 with spending/staking paths, reward type 14 with staking path, enterprise/type-6 type 6 with spending path, and direct DRep key-hash mode without address/network.
- The case generator now passes every Ledger request template through installed 7.1.4 parseMessageData before emission.
- The candidate consumer probe reads all frozen Ledger physical-message templates, deduplicates them, and requires every template to pass both installed 7.1.4 and isolated candidate 8.0.0 parsers before webpack evidence is written.
- Regenerated cases, evidence, input recipes, and candidate consumer results; committed parser evidence records four unique templates and both parser passes.

Verification run:
- Focused task-006 Jest: 14/14 passed.
- Actual Ledger 8 candidate consumer probe and aliased main/renderer webpack builds passed; output regenerated.
- Script syntax, task-local formatting, and `git diff --check`: passed.

Outcome: Iteration-6 blocker addressed; ready for code review

Code Review: Iteration 7
Timestamp: 2026-08-16T16:40:50Z

Outcome: Iteration 7 resolves the sole prior blocker without regressing earlier acceptance areas.

Blocking findings:
- No blockers.

Verified:
- Concrete Ledger base, reward, enterprise/type-6, and direct-DRep templates pass installed Ledger 7.1.4 parsing.
- Candidate probe validates all four unique templates against Ledger 7.1.4 and 8.0.0 before webpack execution; both parser sets and both webpack builds pass.
- Focused task-006 tests pass 14/14; frozen task-002/task-004 tests pass 56/56.
- Candidate evidence regenerates byte-for-byte; scripts and allowlisted diff checks pass.
- Physical certification remains not_run, adapters remain unimplemented, and product enablement remains disabled.

Non-blocking observation:
- Consumer status intentionally remains fail only for the three bounded task-600 TS2339 hex_to_buf migrations while both webpack builds pass.

Decision: approved
