# Task task-006: Freeze Ledger and Trezor capability matrices

## Task

- Task ID: `task-006`
- Title: `Freeze Ledger and Trezor capability matrices`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`

## Why This Task Now

- `task-002` is complete and supplies the frozen CIP-30, CIP-8, CIP-95, error, limit, and witness contracts that hardware capabilities must implement or reject without reinterpretation.
- Phase-6 tasks `600` through `605` currently depend on broad PRD claims about Ledger 7/8, Trezor models/firmware, Conway fields, message signing, cancellation, and returned hashes. Those tasks need a versioned, machine-readable contract before they design adapters or change dependencies.
- Live code still reconstructs reduced coin-selection transactions, has no CIP-8 hardware channel, and does not verify returned hashes or witnesses. Freezing support and rejection rows now prevents later adapters from silently omitting unrepresentable fields.
- The current tracker description says phase 0 will "certify" libraries, device apps, firmware, and models. Static package/source inspection cannot certify physical devices. This plan corrects task-006 to a capability-contract freeze and leaves physical model/app/firmware certification to `task-607` and later release gates.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs for task-006: none. No device inventory, wallet, funds, firmware installation, USB access, or model availability is needed to freeze the static contract and later-certification specification.
- Required manual tests for task-006: none. An agent can inspect immutable library artifacts, run isolated package probes, and validate fixtures while leaving the orthogonal physical-certification dimension `not_run`.
- Required evidence back from the user for task-006: none.
- Implementation can proceed immediately and can complete without a physical-device result. Physical Ledger/Trezor execution is a mandatory downstream checkpoint: `task-607` must run the frozen cases on exact models, Ledger Cardano app versions, Trezor firmware versions, and transports and return normalized evidence before `physicalCertification` can become `pass`; product enablement remains a later release decision.
- If static evidence cannot distinguish support from omission, library representability freezes as `unresolved` and the operation rejects before device interaction; it is not a reason to ask the user to test hardware during this phase or to guess support.

## Scope

- Freeze a versioned capability contract for installed Ledger `7.1.4`, exact candidate Ledger `8.0.0`, and installed Trezor Connect `9.7.2`, including immutable artifact identity and source/type/runtime evidence.
- Inventory every task-004 supported Conway transaction-body field and relevant encoding choice against each library request and response, including nested certificate/voter/output alternatives and structural limits. Distinguish library representability, app/firmware gating, emulator evidence if available, physical evidence, adapter implementation, and product enablement.
- Freeze Ledger 7-versus-8 dependency disposition for task-600 based on an isolated candidate compatibility probe. Task-600, not task-006, applies any package/lock update and migrates consumers.
- Freeze hardware CIP-8/CIP-95 message requirements from task-002: exact payload, payment/stake full-address mode, direct and matching-type-6 DRep key-hash mode, path/credential binding, no hashing fallback, returned material, local COSE construction/verification, and the distinct 64 KiB product request limit versus any smaller vendor protocol/device limit.
- Freeze each applicable task-004 exact-body representation family to one exact disposition: reproducible exact body/hash, deterministic host reconstruction with hash equality, physical returned-hash proof still required, or mandatory pre-device rejection.
- Freeze operation-specific public errors for unsupported field/shape, model/app/firmware gate, malformed payload, wrong path/credential, wrong app, user refusal, disconnect, transport close, host/vendor cancellation, late result, returned-hash mismatch, and invalid/extra/missing response proof.
- Freeze Trezor Connect 9.7.2 message output as non-pass-through: Daedalus must reconstruct the task-002 COSE bytes from independently validated raw identity and signature material rather than releasing vendor `coseSignature` or `coseKey`.
- Add deterministic transaction/message case descriptors and a normalized result/evidence schema for task-602/603/604 mocked tests and task-607 physical certification. Reuse task-002 and task-004 bytes by reference rather than duplicating or changing their wire contracts.
- Correct stale hardware/test workflow statements discovered against live code.

## Non-Goals

- Do not connect to, update, reset, seed, or sign with a physical Ledger or Trezor. Do not claim model, firmware, Ledger app, USB transport, screen-display, button/touch, cancellation, returned-hash, or signature certification.
- Do not implement `HardwareWalletService`, arbitrary-CBOR adapters, message-signing IPC, cancellation generations, trusted broker access, consent, submission, collateral, or batch signing. These remain tasks `600` through `606`.
- Do not run or complete task-607's physical matrix, phase-8 hardening, partner interoperability, or release QA.
- Do not change `package.json` or `yarn.lock` in task-006. The dependency result is an input to task-600, which owns the reviewed production update and migration.
- Do not reshape current production hardware IPC or `source/common/types/hardware-wallets.types.ts`; its reduced transaction contracts are baseline evidence, not the phase-0 contract output or a vendor-neutral model implementation.
- Do not treat a TypeScript property, vendor source branch, emulator, mocked APDU/protobuf response, or successful package load as physical support or exact on-device display/signing evidence.
- Do not broaden the frozen CIP-30/CIP-8/CIP-95 wire behavior, add unsupported CIPs, or resolve production transaction parsing owned by tasks 302/303.
- Do not edit either review log.

## Dependencies And Ownership

- `task-002`: completed in commit `5a6b33c19`; its manifest and fixtures are normative for hardware-visible CIP-30/CIP-8/CIP-95 arguments, errors, limits, witness sets, and DRep normalization.
- `task-001`: the hardware boundary must sign only capability-supported broker-owned bytes and verify returned material before release.
- `task-004`: its exact-CBOR fixture inventory and Conway/Dijkstra dispositions define transaction fields and encodings to inventory. Dijkstra remains unsupported/readiness-blocked and must not appear as a hardware-supported era.
- Task-006 owns only the phase-0 static capability contract, reproducible library evidence, dependency recommendation, fail-closed mappings, and later-certification case/evidence specification.
- `task-600` owns applying the Ledger dependency decision and extracting transport authority; `task-601` owns the vendor-neutral exact transaction/path model; `task-602/603` own exact adapters and returned transaction proof; `task-604` owns message adapters; `task-605` owns cancellation/error normalization; `task-607` owns mocked and physical certification.
- Production and release capability is always the intersection of frozen library representability, exact adapter implementation, configured app/firmware/model gate, successful physical certification, wallet/network/policy capability, and release configuration. A positive static row alone never enables a method.
- Tasks `600` through `606` consume the frozen matrix revision and case IDs but cannot set physical certification or product enablement. Only task-607 may promote an exact production-artifact/model/app-or-firmware row after complete reviewed physical evidence; later product policy/release tasks separately set product enablement.

## Research, Docs, Workflows, And Skills Consulted

- Read in required order: `.agent/readme.md`, then `.agent/system/architecture.md`.
- Read `.agent/workflows/hardware-wallets.md`, `.agent/workflows/test.md`, and `.agent/workflows/update-doc.md`.
- Read the full PRD, task graph, empty task-006 planning log, accepted task-002/task-004 plans, and relevant research `01`, `02`, and `04`. Research `03`, `05`, and `06` were indexed but do not define this hardware contract.
- Inspected live package manifests/locks, installed package artifacts, production IPC/types/utilities, renderer signing call sites, current hardware scripts, Jest roots, and relevant dApp-task history.
- `understand` was loaded before repository exploration. No `.understand-anything` graph or metadata exists, so material package, architecture, test, and ownership findings were verified directly against live files rather than generating a broad graph for this bounded planning task.
- No Cardano operator skill is applicable: this task neither creates keys nor submits/signs a transaction. Task-002's already accepted CBOR/Bech32/COSE evidence is consumed without regeneration.

## Verified Live Findings

- `package.json` and `yarn.lock` pin Ledger `7.1.4` (tarball SHA-1 `e3e484edf950a871d3d3c87750077565162eee9f`, SRI `sha512-bkZ78H0m6E22Fe4nN+K0HY0O2lrPk9Pjs/gv0U5xvJyrMqwmR4wm9h8QXd/AwJ084KIhfpCSGDCQ0CN/K++vNw==`) and Trezor Connect `9.7.2` (SHA-1 `bb6e06f1a28bac41266ef936fea38f653122afa3`, SRI `sha512-Sn6F4mNH+yi2vAHy29kwhs50bRLn92drg3znm3pkY+8yEBxI4MmuP8sKYjdgUEJnQflWh80KlcvEDeVa4olVRA==`, git head `d5ff5430501946cf37717a7ba694953c843bd18e`).
- The exact Ledger candidate observed from npm is `8.0.0`, git head `b1a914c28c2180c8396bd34adc99c8dc31368557`, SHA-1 `7f6b1dcfcc5b397156507b0c82d25d7595687a68`, SRI `sha512-hyWBk4HQApPdIvidQOExOP+GxD36WDsgzCz1PAFeJ4heL/b5Bmplyyg03/lA95NDNjjpqgDzN2rJyBHYpqgfmQ==`, Node `>=22.0.0`. Daedalus declares Node `>=v22.0.0`; compatibility still requires imports/types/build/runtime-consumer probes rather than an engine-only conclusion.
- Installed Ledger `7.1.4` exposes `signTransaction`, `signMessage`, a returned `txHashHex`, message `address`/`key_hash` modes, Babbage datum/reference-script outputs, Conway certificates, at most one voter with at most one vote, treasury, and donation. Its compatibility code caps all supported app predicates at major `7`, so app major `8` fails before capability use. It has no proposal-procedure transaction field.
- Installed Trezor Connect `9.7.2` exposes `cardanoSignTransaction`, `cardanoSignMessage`, `unsignedTx {body, hash}`, returned transaction `hash` and witnesses, Babbage output data/reference scripts, mint/collateral/required-signers/reference-input fields, and message address parameters. Presence in types is only static representability; firmware/model support and exact output behavior remain unverified.
- Trezor Connect 9.7.2 also returns vendor-built `coseSignature`/`coseKey`, but these are not the frozen Daedalus task-002 wire result and must never pass through. Its raw `payload`, protected-address identity, `pubKey`, and `signature` are inputs to independent validation and exact Daedalus reconstruction only.
- Root `package.json` directly pins `@trezor/transport@1.5.4`, while `@trezor/connect@9.7.2` resolves `@trezor/transport@1.6.2`; probes must preserve and report this two-version runtime graph rather than treating "Trezor 9.7.2" as a complete identity.
- Current `LedgerSignTransactionRequest` omits most installed Ledger fields and current renderer helpers rebuild transactions from coin selection. The Trezor channel accepts the vendor `CardanoSignTransaction` shape directly, but current helpers likewise construct only reduced values. No hardware CIP-8 channel exists, and neither current path independently verifies the returned body hash, public key, witness signature, cardinality, or COSE result.
- `hardware-wallet-tests/index.ts` is an interactive Ledger connection/public-key/disconnect menu. It has no Trezor, transaction-field, message, returned-hash, cancellation, or certification coverage. `yarn test:hardware-wallets` is therefore not a current Ledger-and-Trezor integration/certification suite.
- Stale workflow claims requiring correction:
  - `.agent/workflows/hardware-wallets.md` lists Trezor Safe 3 as supported and describes general Trezor address verification without evidence; live shared model types list only One/Model T, and `showAddressChannel` explicitly rejects Trezor.
  - The same workflow's sample `TrezorConnect.init` does not match the current `source/main/trezor/connection.ts` seam and should point to live initialization rather than freeze an obsolete snippet.
  - The workflow says a Trezor popup appears, while live initialization sets `popup: false` and Daedalus renders device interaction UI itself.
  - Its debugging example logs `devicePath`, contrary to this feature's sensitive hardware evidence/logging boundary.
  - `.agent/workflows/test.md` says the hardware command runs Ledger or Trezor integration tests, while the live script is Ledger-only and interactive.

## Expected Files

- `.agent/plans/dapp-browser-cip30/research/07-hardware-wallet-capability-contract.md`: authoritative artifact/source identities, static matrices, conflict resolutions, dependency recommendation, reproduction commands, residual physical gates, and no-certification disclaimer.
- `source/common/hardware/fixtures/capability-matrix/manifest.json`: versioned machine-readable orthogonal dimensions and invariants, full field/message/error/model rows, downstream owners, and references to task-002/task-004 fixtures.
- `source/common/hardware/fixtures/capability-matrix/*-library-results.json`: normalized installed Ledger, candidate Ledger, and installed Trezor static probe results with exact package/source/runtime identity.
- `source/common/hardware/fixtures/capability-matrix/ledger-8.0.0-package-lock.json`: mandatory complete isolated candidate dependency identity for any runtime/import/build probe.
- `source/common/hardware/fixtures/capability-matrix/trezor-9.7.2-runtime-identity.json`: exact resolved transitive versions/integrities plus hashes for behavioral protobuf schemas, device-model constants, firmware-range/network configuration, Connect assets, and both root transport 1.5.4 and Connect transport 1.6.2.
- `source/common/hardware/hardwareCapabilityMatrix.spec.ts`: focused coverage/provenance/status/reference and fail-closed matrix tests; no device access.
- `scripts/hardware-wallet-capability-probe.cjs`: read-only, package-root-parameterized static/runtime export probe that emits deterministic normalized JSON and never opens USB/HID.
- `hardware-wallet-tests/capability-matrix/cases.json`: deterministic transaction/message/error/cancellation case descriptors for later mocked/physical execution, referencing existing immutable bytes where possible.
- `hardware-wallet-tests/capability-matrix/evidence.schema.json`: privacy-safe task-607 result schema separating artifact, library, transport, model, app/firmware, case, observed result, returned-proof verification, and operator/reviewer evidence.
- `.agent/workflows/hardware-wallets.md` and `.agent/workflows/test.md`: narrow corrections to current supported-versus-planned claims and the real scope of `yarn test:hardware-wallets`.
- PRD, task tracker, and this canonical plan: phase-0-versus-physical-certification wording, artifact links, dependency result, and lifecycle synchronization after review.

`package.json`, `yarn.lock`, `source/common/types/hardware-wallets.types.ts`, current IPC/adapters/stores, architecture/API docs, translations, Storybook, Cucumber, and both review logs are not expected to change.

## Smallest Implementation Approach

1. Freeze evidence vocabulary and immutable artifact identity first.
   - Every row has six orthogonal validated dimensions, never one mixed status: `libraryRepresentability = representable | not_representable | unresolved`; `deterministicProbe = pass | fail | not_applicable | not_run`; `emulatorEvidence = pass | fail | unavailable | not_run`; `physicalCertification = pass | fail | not_run`; `adapterImplementation = pass | fail | not_implemented`; and `productEnablement = enabled | disabled`.
   - Task-006 may populate only library representability and deterministic probe. Emulator evidence is recorded only when an immutable emulator artifact was actually run; it is never inferred. Physical certification remains `not_run`, adapter implementation remains `not_implemented`, and product enablement remains `disabled` throughout task-006.
   - Fail-closed predicates are mechanical. Device interaction is allowed only when representability is `representable`, every required deterministic probe is `pass`, the exact encoding disposition permits it, and the configured model/app-or-firmware predicate matches. Public product use additionally requires physical certification `pass`, adapter implementation `pass`, product enablement `enabled`, and matching production artifact/matrix revision. Any other combination rejects before device interaction or remains product-disabled.
   - Reject impossible records: `not_representable` or `unresolved` with any probe/certification/adapter pass; deterministic probe `fail` with physical/adapter/product pass; physical `pass` without exact artifact/model/version and reviewed case evidence; adapter `pass` without representability/probe pass; or product `enabled` unless all required dimensions pass. A failed dimension cannot be overridden by a later positive dimension.
   - Record package version, tarball URL, SHA-1, SRI, git head when published, complete package-lock hash, Node version, public entry points, all exact Ledger deep imports used by Daedalus, and source/type/config files used. Never use moving `latest`, an unpinned branch, or type presence alone as a support claim.

2. Generate one source-derived field and response inventory per package root.
   - Cover transaction envelope identity; inputs; Alonzo/Babbage outputs; assets; datum hash/inline datum/reference scripts; fee/TTL/validity start; all task-004 Conway certificate discriminants; withdrawals; auxiliary-data hash/CIP-36 only where relevant; mint; script-data hash; collateral inputs/return/total; required signers; network ID; reference inputs; set-tag option; signing modes; voting voter/vote variants and cardinalities; proposal procedures; treasury; donation; and Dijkstra exclusion.
   - Cover transaction response hash, witness path/public key/signature/chain code, auxiliary supplement, serialized transaction where applicable, and whether exact incoming body bytes/hash can be supplied. Missing request or response proof freezes fail-closed adapter behavior.
   - Compare generated inventory with the committed manifest so hand-maintained omissions fail tests.

   Each vendor/operation row must also resolve every applicable task-004 encoding family as follows; `physical_hash_required` is an additional gate, not proof that deterministic reconstruction already matches:

   | Exact-body family | Required phase-0 classification and later evidence |
   |---|---|
   | Alonzo array output versus Babbage map output, including datum hash, inline datum, and reference script | Preserve the explicit output format and nested bytes; deterministically reconstruct the complete vendor body and compare its true Blake2b-256 with the exact incoming task-004 body hash. If the library cannot reproduce the selected form exactly, reject pre-device. Passing rows also require task-607 returned-hash equality. |
   | Tagged versus untagged sets, including inputs, collateral/reference inputs, required signers, certificates, withdrawals, mint, and voter/action collections where applicable | Prove the vendor option preserves every selected tag choice. A global option cannot represent mixed per-location choices; such input rejects pre-device. Deterministic reconstructed-hash equality and task-607 returned-hash equality are both mandatory for passing rows. |
   | Source-admitted body/map ordering | Preserve original order only if vendor serialization can reproduce it deterministically. If the semantic API canonicalizes/reorders and the resulting hash differs, reject pre-device; never normalize the broker-owned body to make it pass. |
   | Source-admitted non-minimal integer/length forms | Compare deterministic vendor reconstruction with exact incoming bytes/hash. Any normalized width that changes the body hash requires pre-device rejection. Type acceptance alone is insufficient. |
   | Source-admitted definite/indefinite arrays, maps, strings, and legacy forms | Require byte-for-byte deterministic reconstruction or exact-body passthrough with independently recomputed hash. If vendor requests expose only semantics and normalize the container family, reject pre-device. |
   | Auxiliary-data hash body field and vendor auxiliary-data supplement | Supply the exact committed body hash field without re-deriving it from a reduced semantic object. Verify any returned supplement independently; inability to reproduce the exact body field or distinguish supplement-only data rejects pre-device. Auxiliary-data bytes themselves remain bound by the host review, not silently replaced by vendor output. |
   | Other semantically reconstructed forms: token ordering, certificates/anchors, withdrawals, mint, collateral return, voter/vote ordering/cardinality, treasury, donation, network ID, validity fields, and script/reference hashes | For each nested alternative, generate the vendor request, deterministically reconstruct the complete body, and compare exact bytes when available plus true Blake2b-256. Any omitted, reordered, normalized, cardinality-reduced, or unrepresentable form rejects before device interaction. Every static passing row still requires task-607 returned-hash evidence. |

   The manifest stores one disposition per vendor/operation/family: `exact_reproducible`, `deterministic_hash_match`, or `reject_pre_device`, plus `physicalReturnedHashRequired: true` for every transaction row eligible for task-607. `exact_reproducible` and `deterministic_hash_match` require executable phase-0 proof; neither means physically certified.

3. Run one isolated Ledger 7.1.4-versus-8.0.0 candidate probe and make a bounded dependency recommendation.
   - Verify the exact candidate tarball and a committed complete isolated lock before loading it. Probe package exports, every exact deep import currently used (`dist/utils/address`, `dist/types/internal`, and any discovered import), transaction/message type surface, compatibility predicates for app major 7/8, deterministic parser acceptance/rejection for matrix-shaped data without transport, and compile/build compatibility through an isolated consumer fixture.
   - Select one result: retain 7.1.4 and keep app-v8 unsupported; recommend exact 8.0.0 for task-600; or block both pending a new candidate. Recommend 8.0.0 only if it removes the app-major-8 library rejection, preserves required app-v7 behavior statically, supports the required field/message contract, and requires no unrelated product migration. Physical app-v7/v8 behavior remains task-607 regardless.
   - Do not commit the dependency change. Record every required task-600 consumer/import/type migration explicitly.

4. Freeze message, model/app/firmware, and failure matrices without physical promotion.
   - For Ledger, distinguish Nano S/Nano S Plus/Nano X and Cardano app v7/v8 planned rows, full-address versus key-hash message modes, payload limits, app compatibility predicates, and every physical observation still required.
   - For Trezor, use vendor internal-model identifiers rather than collapsing all devices into current `'1' | 'T'` UI types. Include One, Model T, and each named Safe/Core model only when immutable Connect/firmware source identifies it; otherwise set library representability `unresolved` or omit a non-target row. Trezor One `signData` remains fail-closed; Model T/Safe/Core message rows keep `physicalCertification: not_run` until task-607.
   - For Trezor, hash and bind the installed Connect package, its complete resolved lock graph, Cardano protobuf/schema files, device/internal-model constants, firmware-range and Cardano network/config assets, and runtime-loaded Connect assets. Record the root direct `@trezor/transport@1.5.4` and Connect-owned `1.6.2` separately and exercise the actual Daedalus resolution path.
   - Freeze Trezor message identity rows separately for payment-base full address, stake/reward full address, direct DRep key hash, and matching type-6 DRep key hash. Each row defines expected returned raw payload, address/key-hash identity, public key, and signature. Reject vendor COSE bytes as public output and independently verify the raw values and signature before reconstructing task-002-conformant protected/unprotected headers, `Signature1`, `COSE_Sign1`, and `COSE_Key`.
   - Exact byte equality with task-002 golden COSE is required only in synthetic cases that use the exact frozen fixture payload, key, address/key-hash identity, and signature material. Physical payment, stake, and DRep cases use device-specific keys/signatures and therefore must not compare complete COSE bytes with that golden; they instead prove the frozen encoding rules, byte-exact original payload preservation, expected full-address or key-hash identity, public-key association, Ed25519 signature validity, locally reconstructed COSE validity, and absence of vendor COSE pass-through.

   Public failure mapping is exact and operation-specific:

   | Predicate | Transaction operation | Data-signing operation |
   |---|---|---|
   | Malformed public shape/hex/CBOR, decoded request payload above the task-002 65,536-byte product limit, or invalid address/DRep syntax | `APIError.InvalidRequest` | `APIError.InvalidRequest` |
   | Valid request at or below 65,536 decoded bytes that exceeds a smaller vendor protocol/device transaction or message capability | `TxSignError.ProofGeneration` | `DataSignError.ProofGeneration` |
   | Unsupported field/encoding/cardinality, incompatible model/app/firmware, wrong app, unavailable required key/path, pre-device exact-hash reconstruction failure | `TxSignError.ProofGeneration` | `DataSignError.ProofGeneration` |
   | Wrong returned address/key-hash/public-key identity or invalid/missing/extra signature/witness/body hash | `TxSignError.ProofGeneration` | `DataSignError.ProofGeneration` |
   | Explicit device refusal | `TxSignError.UserDeclined` | `DataSignError.UserDeclined` |
   | Host cancellation before completion | `TxSignError.UserDeclined` | `DataSignError.UserDeclined` |
   | Vendor cancellation explicitly caused by the host cancellation request | `TxSignError.UserDeclined` | `DataSignError.UserDeclined` |
   | Disconnect before confirmation, spontaneous disconnect during confirmation, spontaneous transport close, app closes/locks, or vendor cancellation not caused by host/user refusal | `TxSignError.ProofGeneration` | `DataSignError.ProofGeneration` |
   | Malformed vendor response that cannot be classified as missing/invalid proof, unexpected library/transport exception, or active-generation protocol corruption | `APIError.InternalError` | `APIError.InternalError` |
   | Generation becomes stale before the call has settled | `APIError.InternalError` | `APIError.InternalError` |
   | Late completion after cancellation or another terminal settlement | No second public settlement; discard and retain the already returned exact terminal error | No second public settlement; discard and retain the already returned exact terminal error |

   No row permits a catch-all, slash-separated alternative, generic JavaScript `Error`, software fallback, or release of partial material. Task-605 implements these frozen predicates without changing public codes.

5. Define reusable case descriptors and evidence schema, not a device runner.
   - Reference task-002's CIP-8/DRep vectors and task-004's exact Conway bytes/inventory. Add only the minimal deterministic descriptors needed for each support/rejection boundary and vendor mapping; do not duplicate large CBOR or build production adapters.
   - Every case states pre-device expected disposition, required model/app/firmware row, expected display/signing mode, expected body/payload/hash/path/credential, allowed result shape, local verification, typed error, and whether physical execution is required.
   - The task-607 evidence schema requires matrix revision; case IDs; exact production dependency lock and package/config-asset digests; adapter source commit; normalized transport/model/internal-model/app-or-firmware enums; bounded numeric versions/timing; input digest; normalized outcome/error enum; returned hash/key/signature verification booleans/digests; negative, refusal, disconnect, cancellation, transport-close, and stale/late-result case outcomes; opaque operator/reviewer IDs; execution date; external-evidence digest/access-policy ID when applicable; and reviewer disposition.
   - Committed/exported evidence allows only fixed enums, bounded integers, booleans, approved opaque IDs, and fixed-length digests. It forbids screenshots, raw vendor payloads, free-form prompt/error text, USB paths, serials, labels, addresses, xpubs, host paths/argv/environment, raw transactions/messages/signatures/keys, seeds, PINs, passphrases, and other secrets. Necessary raw evidence remains in access-controlled external storage and is referenced only by digest plus a non-secret access-policy identifier and retention class.

6. Synchronize documentation without broadening implementation.
   - Correct the workflows to distinguish current Ledger-only interactive diagnostics from future matrix certification and current live support from planned PRD scope.
   - Rewrite every affected phase-6 tracker contract, not only task-006: phase 6 and tasks 600-606 consume the frozen matrix revision/case IDs, may update library/adapter dimensions only, and remain product-disabled; remove claims that they consume a certified phase-0 matrix or certify models/firmware. Task-607 alone runs and reviews the exact production dependency lock, adapter source commit, artifact/model/app-or-firmware rows, all positive/negative/cancellation/late-result case IDs, and evidence schema. It may set only physical-certification results; later release policy sets product enablement.
   - Specifically, task-600 applies the static dependency recommendation without claiming app/model certification; task-601 models only frozen exact/reject dispositions; tasks 602/603 implement vendor rows without physical promotion; task-604 implements reconstructed/verified message output without vendor COSE pass-through and preserves the product-versus-vendor limit distinction; task-605 implements the exact failure/cancellation/limit predicates without reinterpreting them; task-606 integrates only behind disabled capability gates; and task-607 records reviewer-approved physical results against immutable production identities.
   - After implementation review, record the dependency recommendation and artifact paths, then update only task-006 lifecycle metadata. Production guest launch and all hardware connector capabilities remain disabled.

## Acceptance Criteria

- One versioned machine-readable manifest accounts for every task-004 Conway transaction field/alternative relevant to signing, all task-002 hardware message modes, transaction/message response proofs, cancellation/failure classes, and every planned model/app/firmware row, with no unowned or orphan entry.
- Every capability row has independently schema-validated library representability, deterministic probe, emulator evidence, physical certification, adapter implementation, and product enablement dimensions. Impossible combinations are rejected; static inspection never sets physical certification, adapter implementation, or product enablement.
- Every applicable task-004 representation family, including output form, tag choice, map ordering, admitted non-minimal/definite/indefinite forms, auxiliary-data hash, and nested semantically reconstructed forms, has executable exact/deterministic hash evidence or mandatory pre-device rejection; every eligible transaction row also requires task-607 returned-hash equality.
- Installed Ledger 7.1.4, exact candidate Ledger 8.0.0, and installed Trezor Connect 9.7.2 have immutable package identities and deterministic normalized probe results. Generated source inventory agrees with the committed matrix.
- The Ledger dependency decision is one explicit evidence-backed retain/recommend/block outcome. Any recommendation names exact task-600 import/type/build migrations and does not modify the production manifest or lock in task-006.
- Ledger proposal procedures and unsupported multi-voter/multi-vote shapes are fail-closed. Trezor unsupported governance/certificate/proposal/treasury/donation fields are enumerated from exact installed evidence rather than grouped under an ambiguous "governance unsupported" label.
- Base payment/stake full-address and CIP-95 DRep key-hash message modes are bound to task-002 exact payload/path/address/credential rules. Hashing fallback, malformed payload, wrong credential, unsupported model/version, and unverifiable response all fail before public result release.
- Trezor Connect 9.7.2 vendor `coseSignature` and `coseKey` are never public pass-through values. Synthetic cases using the exact frozen task-002 key/signature material require byte-equal reconstructed golden COSE. Physical payment, stake, direct-DRep, and matching-type-6-DRep rows instead require frozen-encoding conformance, original-payload preservation, expected identity, public-key association, signature verification, and valid local COSE reconstruction without cross-key golden-byte comparison.
- The 65,536 decoded-byte task-002 request limit is enforced before hardware capability checks and violations return `APIError.InvalidRequest`. A valid request within that product limit but beyond a smaller vendor protocol/device capability returns `TxSignError.ProofGeneration` for transaction signing or `DataSignError.ProofGeneration` for data signing; tasks 604/605 may not remap either boundary.
- Every listed transaction/data-signing failure predicate maps to exactly one frozen public error or, for an already settled late result, explicit discard with no second settlement. No generic/catch-all or alternative outcome remains.
- Returned transaction hash, witness public key/hash/signature/cardinality, message address field/public key/signature, and COSE verification requirements are explicit. Missing, extra, mismatched, malformed, or late returned material releases no witness or COSE.
- The later-certification case set covers every supported and unsupported field family, boundary/cardinality, Ledger app v7/v8 row, exact Trezor internal-model/firmware row, base/DRep message mode, refusal, disconnect, app-not-open/wrong-app, transport failure, cancellation/late response, and returned-proof corruption.
- The evidence schema is reproducible and privacy-safe: normalized enums/bounded numerics/booleans/digests only, opaque operator/reviewer IDs, no screenshots/raw payloads/free text/personal-device-wallet-host data, and external raw evidence referenced only by digest/access-policy ID. Task-607 requires reviewer disposition before a row is physically certified.
- All phase-6 tracker entries are internally ordered: tasks 600-606 consume static revision/case contracts and remain product-disabled; task-607 consumes exact production lock/config identity, adapter commit, all required case outcomes, and reviewed evidence to set physical results only.
- Workflow, PRD, tracker, research, fixtures, tests, and downstream ownership agree that task-006 is phase-0 contract evidence, task-600 applies dependency changes, tasks 602-605 implement behavior, and task-607 performs physical certification.
- No production adapter/service/IPC/type/store behavior, dependency pin, hardware operation, dApp enablement, or review log is changed.

## Verification

- Run `yarn test:jest source/common/hardware/hardwareCapabilityMatrix.spec.ts --runInBand --coverage=false`.
- Run the package-root probe against installed Ledger 7.1.4, verified isolated Ledger 8.0.0, and installed Trezor Connect 9.7.2; regenerate normalized output and require byte-for-byte agreement with committed results.
- Verify candidate tarball SHA-1/SRI/version/git head/Node engine and, when used, isolated lock hash before loading; reject moving or mismatched artifacts.
- Verify every exact Ledger deep import and the complete candidate lock. Verify Trezor's resolved package/config-asset digests, protobuf and model/firmware sources, actual runtime resolution, and simultaneous root transport 1.5.4 versus Connect transport 1.6.2 identities.
- Regenerate each package's request/response/compatibility inventory and fail for an unaccounted task-004 field, nested discriminant/cardinality, task-002 message mode, returned-proof field, failure class, or model/app/firmware row.
- Assert every manifest row references existing immutable evidence and downstream owner; every case references an existing capability row and fixture/digest; and each non-supported row has a deterministic pre-device fail-closed disposition.
- Assert exact-body family coverage and hash dispositions; mutate every orthogonal dimension to prove contradictory-state rejection and the complete fail-closed intersection.
- Validate every synthetic positive/negative case descriptor and normalized physical-evidence example against `evidence.schema.json`, including forbidden screenshots/raw/free-text/sensitive fields and the impossibility of physical certification without matrix/cases/production lock/adapter commit/operator/result/reviewer evidence.
- Assert the complete operation-specific error table at the 65,536/65,537-byte product boundary and at synthetic smaller vendor limits. Assert Trezor COSE non-pass-through, exact golden equality only with exact task-002 fixture material, and structural/cryptographic conformance without golden-byte equality for different physical keys/signatures.
- Run `yarn compile`, focused ESLint and direct Prettier checks for changed TypeScript/JSON/Markdown, parse all JSON, and run `git diff --check`.
- If the recommendation is Ledger 8.0.0, use a temporary isolated consumer or reversible unstaged patch to run TypeScript and main/renderer build compatibility plus focused current Ledger/Trezor utility tests; restore the worktree state without committing dependency/source changes. Record failures and required task-600 migrations rather than fixing production consumers here.
- Re-run task-002 contract fixtures and task-004 exact-CBOR tests to prove referenced wire/field inventories did not drift.
- Inspect the final diff for review-log edits, secrets/device identifiers, production hardware or IPC changes, package/lock changes, copied large fixtures, unsupported standard/era claims, and unrelated tracker statuses.
- Parse the task graph and assert the revised phase-6 wording assigns static consumption/implementation to 600-606, physical promotion only to 607, and product enablement only to later release policy.
- Do not run `yarn test:hardware-wallets` as task-006 acceptance: it requires physical Ledger interaction and currently covers only legacy Ledger diagnostics. No physical Ledger/Trezor, firmware/app, configured-network, wallet-funds, Electron E2E, Storybook, Cucumber, or release certification is required in this phase.

## Risks And Open Questions

- Library/source versus device reality: declarations can overstate firmware behavior and cannot prove display or returned bytes. Preserve `physicalCertification: not_run` and task-607 ownership; never convert static success into certification.
- Living vendor sources: npm tags, Trezor firmware metadata, Ledger app releases, and supported models can change. Freeze exact artifacts/revisions and require later deltas to create a reviewed matrix revision rather than mutating this baseline silently.
- Ledger 8 migration risk: app-v8 compatibility may require changed imports, package exports, Node semantics, transport peers, or existing utility changes. The bounded recommendation may block or defer; task-006 must not absorb task-600.
- Trezor model naming risk: UI-facing `'1' | 'T'`, `model`, and `internal_model` are not interchangeable, and "Safe/Core" is not one certifiable model. Rows must use immutable vendor identifiers or remain omitted.
- Field-presence risk: vendor types can contain fields that runtime validation or firmware rejects, and semantic reconstruction can normalize task-004 ordering, widths, container forms, tags, or output variants. Orthogonal dimensions and exact-hash dispositions default every unresolved/mismatched row to pre-device rejection.
- Cancellation is vendor- and transport-specific. Static `TrezorConnect.cancel()` or Ledger transport-close availability does not prove no late device result; task-605 implements generations and task-607 exercises timing.
- Existing logs include device paths, transport lists, product/device data, and vendor payloads. Task-006 evidence and corrected workflow advice must exclude these; production logging cleanup belongs to implementation/security tasks and must not be hidden in this contract change.
- No open product decision is needed. Any unresolved static row remains disabled pending implementation and physical evidence.

## Required Docs, Tracking, And Research Updates

- Add `research/07-hardware-wallet-capability-contract.md` and the machine-readable static matrix/probe/case/evidence artifacts.
- Update the PRD current-baseline, hardware-validation, phase, testing, and artifact-pointer text only enough to record the frozen contract, dependency recommendation, and task-006/task-607 boundary. Do not claim device support.
- Update task-006 tracker wording from certification to static contract freeze, then revise the phase-6 description and every task `600` through `607` description/notes/acceptance that implies premature certification or enablement. Preserve dependencies, unrelated task status/order, and task-607's sole physical-promotion ownership; record task-006 completion metadata only after implementation review approval.
- Correct `.agent/workflows/hardware-wallets.md` and `.agent/workflows/test.md` to match live current support and command behavior, including `popup: false`, renderer-owned Trezor UI, and prohibition on logging `devicePath` or equivalent sensitive identifiers, while labeling phase-6/607 procedures as planned.
- Update this canonical plan as planning/build/review evidence changes.
- No architecture/API endpoint update is required because task-006 changes no runtime architecture or channel. No translation, Storybook, Cucumber, package, lock, or sibling-repository update is required.
- Research update is mandatory; `no new research` is not a valid completion result for this evidence task.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-006-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-006-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope creep: limited task-006 to immutable evidence, one declarative matrix, one static package probe, focused validation, later-certification specifications, and wording corrections. Dependency application, production types/services/adapters/IPC/cancellation, behavioral mocks, physical execution, and product enablement remain downstream.
- Stale workflow/dependency text: included the Ledger-only script, unsupported live Trezor verification, unproven Safe claim, real `popup: false` initialization, sensitive `devicePath` logging, Ledger 7 app-major cap, exact Ledger 8 candidate/deep imports, and Trezor's split transport/config identity.
- Evidence/manifests/tests/docs: added task-004 exact-representation/hash dispositions, orthogonal dimensions/impossible-state checks, complete immutable dependency/config identity, separate Trezor message identity/COSE reconstruction rows, exact public errors, privacy-normalized task-607 evidence, and complete phase-6 tracker ownership.
- Hardware fail-closed boundaries: unresolved representability, reconstruction/hash mismatch, unsupported field/model/version, wrong app, malformed/wrong message identity, disconnect/cancellation/stale completion, and returned-proof mismatch have exact no-release behavior; no software fallback, generic error, vendor COSE pass-through, or equal-vendor-coverage claim is introduced.
- Standards/wire drift: task-002 remains normative for CIP-8/CIP-95 payload/address/DRep/COSE/errors and task-004 remains normative for exact Conway bytes/fields and blocked Dijkstra. This task references rather than reserializes or redefines them.
- Human checkpoint: task-006 requires no user evidence because physical certification remains `not_run`. Task-607 must bind matrix/case IDs, actual production lock/config identity, adapter commit, normalized negative/cancellation/late-result evidence, opaque operator identity, and reviewer disposition before physical promotion.
- Consistency: tasks 600-606 consume static contracts and remain product-disabled; task-607 alone sets physical results; later release policy alone enables products. Expected files exclude production manifests/types and both logs; production guest launch remains disabled.
- Authorized correction consistency: synthetic task-002 fixture material alone uses golden COSE byte equality; physical device-specific keys/signatures use encoding, payload, identity, association, and signature conformance without impossible cross-key equality.
- Authorized correction consistency: the task-002 64 KiB product boundary maps only to `APIError.InvalidRequest`, while smaller intrinsic hardware limits for otherwise valid requests map only to the operation-specific proof-generation error and cannot be reinterpreted by tasks 604/605.

## Planning Status

- `approved`

## Build Status

- `completed`

Implementation review iteration 7 approved the static capability contract. No dependency update, hardware execution, physical certification, adapter implementation, or product capability was included.

## Canonical Outcome

- Approved static matrix revision `task-006-matrix-2026-08-14` for installed Ledger `7.1.4`, recommended candidate Ledger `8.0.0`, and installed Trezor Connect `9.7.2`.
- All vendor transaction paths remain unconditional `reject_pre_device` until downstream exact-body reconstruction proves immutable body-hash equality. Ledger 8 application and its three bounded `hex_to_buf` migrations belong to task-600.
- Tasks 602/603 own nonphysical static-source assertions. Stable task-607-owned cases and promoted evidence branches bind exact production lock/config/runtime graph, adapter commit, model/app-or-firmware row, path/payload/request recipe, proofs, operator, and reviewer.
- Physical certification remains `not_run`, adapter implementation remains `not_implemented`, and product enablement remains `disabled`.
- Implementation review decision: `approved` at iteration 7.
