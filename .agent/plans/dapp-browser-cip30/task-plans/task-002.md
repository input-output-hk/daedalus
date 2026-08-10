# Task task-002: Freeze CIP-30 and extension wire contracts

## Task

- Task ID: `task-002`
- Title: `Freeze CIP-30 and extension wire contracts`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`

## Why This Task Now

- `task-001` is complete and has fixed the hostile-renderer boundary, so public wallet values and internal Electron results can now be specified without ambiguity about which process owns validation and error reconstruction.
- This task is on the critical path. Backend validation (`task-003`), exact-CBOR validation (`task-004`), hardware capability work (`task-006`), the guest preload (`task-104`), and shared schema implementation (`task-300`) all depend on one frozen contract inventory.
- The CIPs are living sources and currently contain known contradictions or incomplete definitions. Downstream code must depend on reviewed local contract fixtures rather than silently changing when upstream prose changes.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs: none. The PRD already fixes every product/security decision needed here and explicitly permits only the CIP-104 encoding to remain unresolved behind its gate.
- Required manual steps: none for the user. The implementer must perform network research against authoritative living CIP sources and public ecosystem implementations, but can collect and record that evidence directly.
- Required user evidence: none. No wallet account, funds, hardware device, private key, packaged Electron build, or external audit evidence is needed.
- Implementation can proceed now. If no public implementor evidence can prove the required CIP-95 DRep normalization vector, completion must stop and the missing evidence must be surfaced rather than inventing a vector. This is a fail-closed evidence condition, not a currently expected user checkpoint.

## Scope

- Freeze a complete, machine-readable inventory of provider, base CIP-30, CIP-8, CIP-95, CIP-103, gated CIP-104, and gated CIP-142 request, success, and rejection shapes.
- Freeze extension negotiation and namespace composition, including unsupported-extension omission, negotiated namespace presence, CIP-95's base `signTx` override, and CIP-103 composition with the effective base signer.
- Freeze the JavaScript-facing invocation/property contract that JSON Schema cannot express by itself: positional arity, omitted versus explicit `undefined`, defaults, extra arguments, method/namespace presence, provider metadata, and non-JSON scalar rejection.
- Freeze public typed errors and the Electron-safe internal success/rejection envelope from which the guest preload will later reconstruct public rejection values locally.
- Record provenance-backed contract excerpts and golden fixtures for every wire representation implemented by this feature at this stage.
- Validate CBOR fixtures as bytes and diagnostic structure, and validate Bech32 address fixtures against their raw bytes and intended HRP.
- Record the explicit disabled/omitted contract for CIP-104 until `task-404` proves one exact interoperable encoding; do not manufacture an account-xpub encoding fixture.
- Preserve exact PRD product limits and before-side-effect error mappings: 65,536 decoded bytes per transaction or message payload, 1-50 CIP-103 items, page `limit` 1-100, and five minutes of consent inactivity.

## Non-Goals

- Do not implement production runtime validators, dispatch, extension registry behavior, preload/provider injection, broker authority, consent, backend methods, Cardano semantic parsing, signing, submission, or UI. Executable shared runtime schemas and IPC integration remain `task-300`; this task freezes the schema source material that task consumes.
- Do not resolve exact transaction-body/output slicing, era coverage, ledger canonicalization, or full transaction fixtures owned by `task-004` and phase 3.
- Do not resolve CIP-104's raw-bytes-versus-CBOR-byte-string ambiguity. `task-404` owns the implementor interoperability gate and may terminate with CIP-104 disabled.
- Do not add CIP-106, CIP-141, CIP-144, or CIP-147 placeholders. CIP-106 and CIP-141 are explicitly absent from the runtime provider.
- Do not add a production schema-validation dependency, code generator, fixture downloader, conformance harness, network client, or later-task infrastructure. Use the already resolved test-time `ajv@6.12.6` and Node structured-clone mechanism; add no package or lockfile change.
- Do not modify the planning or implementation review logs.

## Dependencies

- Task dependency: `task-001`, completed in commit `2bf49be1d` with the accepted separate-guest/main-broker ADR.
- Normative product decisions:
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- Downstream consumers include `task-003`, `task-004`, `task-006`, `task-104`, and especially `task-300`; they must consume the frozen local manifest/fixtures rather than reinterpret upstream prose.

## Research Consulted

- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`: confirms that the guest must use a dedicated scoped protocol and that internal result values cannot rely on legacy IPC error transport.
- Current authoritative sources were fetched from `cardano-foundation/CIPs` on 2026-08-10:
  - CIP-30 and its extensions register.
  - CIP-8, CIP-95, CIP-103, CIP-104, and CIP-142.
  - Path-level revision provenance observed through GitHub: `86b89208d3b2aabb5dcc5b778dfbe09096b4e114` for the fetched CIP-30, CIP-8, CIP-103, CIP-104, and CIP-142 paths, and `20c819b25abee6551a3ef51778b975e7463e1269` for CIP-95. These revisions are evidence provenance, not normative document pins; the local fixtures freeze behavior.
- Material current-source findings:
  - CIP-30 requires Bech32-or-raw-hex address inputs and raw-hex address outputs, unknown extension omission, zero-indexed pagination, and witness-set-only `signTx`, but its deprecated `getCollateral` prose conflicts with its nullable return type and suggests an imprecise 5 ADA cap. The PRD's nullable, side-effect-free, no-Daedalus-cap decision controls.
  - CIP-8 requires body `version: 1` and `hashed: false`; base/CIP-95 `signData` additionally fixes attached exact payload bytes, empty external AAD, EdDSA labels, untagged CBOR, and consistent `kid` omission.
  - CIP-95 is Active, adds `DeprecatedCertificate = 3`, overrides base `signTx`, and contains the known printed omission of `.cip95` on `getRegisteredPubStakeKeys` while its own prose says methods are namespaced. Its test-vector checklist is still incomplete, so Daedalus must record its own independently checked DRep vector.
  - Current CIP-95 lists type-6 enterprise key addresses as payment-key identifiers and says ordinary address signing protects the complete raw address. The PRD adds a narrower compatibility reinterpretation only when the type-6 payment credential equals the selected wallet's DRep key hash; named implementor evidence is therefore a completion gate for that reinterpretation.
  - CIP-103 is Active and requires aligned results, all-or-nothing signing disclosure, attempt-all ordered submission, and direct rejection with an aligned `(hash32 | TxSendError)[]`.
  - CIP-104 remains Proposed and defines `cbor<Bip32PublicKey>` without defining the CBOR shape. Only omission/disabled behavior can be frozen here.
  - CIP-142 remains Proposed; its prose says `cip-142` while its example uses `api.cip142`. The PRD's JavaScript-valid `api.cip142` namespace controls. CIP-142 is not in the fetched CIP-30 accepted extensions register, reinforcing its proposed policy gate.
  - The ledger CDDL used by CIP-30 defines `coin = uint`; for the Cardano CBOR wire contract this task freezes canonical unsigned-integer encodings in the `0..2^64-1` range. Larger tagged bignums, negatives, non-integers, and non-minimal integer encodings are not valid `cbor<Coin>` inputs.
- Public Lace, Yoroi, Cardano JS SDK, and available Eternl/Typhon/Lucid-compatible source or release behavior must be consulted during implementation for the specific compatibility claims recorded in the evidence note. Source identity, revision/release, exact observed behavior, and reproducible input/output must be recorded; a name-only claim is insufficient.

## Docs, Workflows, And Skills Consulted

- Documentation:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
  - `.agent/plans/dapp-browser-cip30/task-plans/task-001.md`
  - `.agent/plans/dapp-browser-cip30/task-plans/task-002-plan-review.md` (read in full through the iteration-1 Critiquer decision and all seven consolidated blockers)
- Workflows:
  - `.agent/workflows/frontend.md`
  - `.agent/workflows/ipc.md`
  - `.agent/workflows/test.md`
  - `.agent/workflows/update-doc.md`
- Skills:
  - `understand`: loaded before repository exploration. No `.understand-anything` graph exists, so important architecture, package, test, and path findings were verified against live files and history rather than generating a broad graph for this narrow planning task.
  - `cbor-encoding-decoding`: used to set byte-first fixture rules; local `cbor-diag` was verified by decoding `83010203` to `[1, 2, 3]`. Re-encoding is not treated as proof that original bytes were preserved.
  - `bech32-encoding-decoding`: used to set explicit-HRP and raw-byte round-trip rules; the local `bech32` CLI was verified against the skill's address vector.
- `understand-explain` was not needed because direct live-file and history inspection resolved the targeted questions.

## Live Repository Findings

- `source/common/cip30/` does not exist yet; this task establishes only its contract/evidence seam.
- `package.json` already provides `cbor@5.0.2`, `borc@2.1.2`, `bech32@2.0.0`, Jest, and TypeScript. No general runtime-schema library is present, so this task must not choose or add one ahead of `task-300`.
- The installed Yarn graph already resolves `ajv@6.12.6`, which supports JSON Schema draft-07 meta-validation and positive/negative instance validation. Live Node is v24 and the manifest requires Node `>=22`; runtime structured cloning is available. Focused tests can use Node `MessageChannel` for an actual clone boundary without adding an application dependency.
- `tsconfig.json` supports JSON imports and `source/**/*.spec.ts` is included by the existing Jest configuration.
- Existing CBOR use is permissive application decoding rather than a reusable strict wire-contract layer. It is not an appropriate source of CIP-30 truth.
- The IPC workflow's generic thrown-error examples and advice to log request/response payloads are stale and unsafe for this feature. The PRD controls: resolve an explicit serializable envelope and never log wallet wire values, origins, signatures, keys, or payloads.
- The frontend workflow is consulted for project context only; this task has no renderer, i18n, styling, Storybook, or route change.

## Expected Files

- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - Add a concise contract-freeze/provenance table and clarify the phase-0 artifact versus task-300 runtime-validator boundary without changing locked behavior.
- `.agent/plans/dapp-browser-cip30/research/02-cip30-wire-contract-evidence.md`
  - Record authoritative excerpts, source revisions/releases, conflict resolutions, ecosystem observations, and reproduction commands/results. Keep the PRD normative.
- `source/common/cip30/contracts/contract-manifest.json`
  - Machine-readable JavaScript property/namespace and invocation inventory for provider, base, and extension paths, including arity/default/`undefined`/extra-argument rules, deterministic negotiation, method-level errors, and exact limits.
- `source/common/cip30/contracts/schemas/*.schema.json`
  - JSON Schema draft-07 schemas for representable argument values, success values, each public rejection, and the internal result envelope. Closely related definitions may share files to avoid reference churn. These are frozen schema inputs; task-300 implements strict production validators.
- `source/common/cip30/contracts/fixtures/*.json`
  - Small reviewed golden and negative vectors with provenance and expected representations.
- `source/common/cip30/contracts/contractFixtures.spec.ts`
  - Focused manifest completeness, schema meta/instance validation, CBOR, Bech32/raw-address, JavaScript invocation, negotiation, error-envelope structured-clone, and exact limit-boundary tests.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - Update only after implementation review confirms acceptance: mark `task-002` complete and update truthful tracker metadata/notes without changing dependency structure.
- `.agent/plans/dapp-browser-cip30/task-plans/task-002.md`
  - Later lifecycle updates for approved planning status, completed build status, verification, and outcome.

No package manifest, lockfile, production IPC file, renderer file, backend file, architecture document, translation, Storybook story, or Cucumber feature is expected to change.

## Smallest Implementation Approach

1. Create one evidence table before creating fixtures.
   - For each public path, record authoritative source URL, observed source revision/release, exact excerpt or behavior, PRD decision, and whether the fixture is normative, compatibility-only, negative, or gated.
   - Record contradictions explicitly instead of blending them: deprecated `getCollateral`, CIP-95 registered-getter namespace, CIP-142 namespace prose, and CIP-104 encoding.
   - Treat fetched revisions as reproducibility evidence only. A future upstream delta is assessed in conformance work; it does not silently mutate local contracts.

2. Build one declarative JavaScript contract manifest, not a registry implementation.
   - Enumerate exact property paths and kinds: `window.cardano.daedalus` is a provider object; `apiVersion`, `name`, `icon`, and `supportedExtensions` are values; callable entries are marked `method`; extension entries are marked `namespace`; absent namespaces are represented by missing own properties, equivalent to JavaScript property access yielding `undefined`.
   - For every method, record `minArgs`, `maxArgs`, each positional schema, whether explicit `undefined` is equivalent to omission, the default, strict object-property policy, success schema, method-level rejection union, required extension/scope, and applicable limit. Any argument beyond `maxArgs`, including trailing `undefined`, rejects with `APIError.InvalidRequest`; zero-argument methods reject any supplied argument. Required arguments reject omission or `undefined`.
   - Optional arguments accept omission or explicit `undefined` identically: `enable(undefined)` means no requested extensions, `getUtxos(undefined, paginate)` omits amount, omitted pagination means unpaged, and omitted/`undefined` `partialSign` means `false`. Draft-07 `additionalProperties:false` rejects unknown own fields; the invocation model separately rejects inherited enumerable input fields. JavaScript-only fixture cases cover `undefined`, functions, symbols, bigint arguments, `NaN`, and infinities outside JSON Schema.
   - Freeze provider metadata: `apiVersion` is exactly `'1'`; `name` and `icon` are nonempty trusted local strings; `supportedExtensions` is a document-generation snapshot in fixed registry order `[95, 103, 104, 142]`, filtered by current route wallet/backend/device capability and packaged policy. Proposed-but-policy-disabled CIP-104/CIP-142 are omitted from metadata as well as negotiation and namespaces.

3. Freeze deterministic extension negotiation and composition.
   - `{cip}` is a strict singleton with a positive finite safe integer. A malformed entry or malformed outer `enable` shape rejects the whole call with `APIError.InvalidRequest`; a well-formed unknown or currently unsupported CIP is simply omitted.
   - Duplicate requested CIPs collapse to one candidate. Request order does not control output: `supportedExtensions`, `getExtensions()`, and namespace construction use fixed registry order `[95, 103, 104, 142]` after filtering.
   - Declared dependencies are evaluated after capability/policy filtering; a candidate with an unavailable dependency is omitted. For any declared incompatibility, the first candidate in registry order wins and later conflicting candidates are omitted, so `enable` does not fail merely because a combination cannot be supported. The current frozen descriptors declare no dependency or incompatibility among 95/103/104/142.
   - CIP-95 is the sole base override (`api.signTx`); when 95 and 103 are enabled, CIP-103 delegates each item to the effective CIP-95-aware signer while preserving caller item order. CIP-8 is never requested or advertised. CIP-106/CIP-141 have no descriptor or placeholder.
   - Each successful repeated `enable` replaces the live enabled-extension set for that guest document and returns a fresh API object; the old live API capability is revoked. Existing approved scopes may avoid another prompt, but newly requested disclosure scopes still require consent. The new `getExtensions()` and own namespace properties reflect only the latest successful set.

4. Define draft-07 runtime-value schemas and the exact internal result envelope.
   - Every schema declares `"$schema": "http://json-schema.org/draft-07/schema#"`, strict object `required`/`additionalProperties:false`, finite-integer bounds where relevant, and reusable lowercase even-length unprefixed hex definitions. Invocation-only JavaScript behavior remains in the manifest and fixtures rather than being misrepresented as JSON.
   - Freeze prototype-free wire data as scalars, arrays, and records with own enumerable data properties only: no `Error`, class instance, function, symbol, accessor, `Date`, `Map`, `Set`, or custom prototype crosses the boundary. Records are created/validated as plain data and the structured-cloned copy must have exactly the declared own keys and ordinary data-record/array prototypes, never an error/custom prototype.
   - Freeze the envelope exactly as `{ status: 'fulfilled', value: <method success> }` or `{ status: 'rejected', rejection: { type, value } }`, with no extra fields. `type` is exactly `'api-error'`, `'paginate-error'`, `'tx-sign-error'`, `'data-sign-error'`, `'tx-send-error'`, or `'cip103-submit-error'`; `value` is respectively the exact public `{code, info}`, `{maxSize}`, or aligned `(hash32 | {code, info})[]` value. The preload rejects with `rejection.value` directly and never exposes `status`, `type`, or a generic `Error` publicly.
   - Freeze method-level rejection unions in the manifest: provider methods, ordinary getters, CIP-95 getters, CIP-104, and CIP-142 allow `APIError`; paginated reads additionally allow `PaginateError`; `signTx`/`cip103.signTxs` additionally allow `TxSignError`; base/CIP-95 `signData` additionally allow `DataSignError`; `submitTx` additionally allows `TxSendError`; `cip103.submitTxs` allows `TxSendError` before attempts and the mixed array after attempt-all execution begins.
   - Freeze five-minute inactivity outcomes before execution: connection/elevated disclosure rejects `APIError.Refused {-3}`; `signTx`/`signTxs` rejects `TxSignError.UserDeclined {2}`; base/CIP-95 `signData` rejects `DataSignError.UserDeclined {3}`; `submitTx`/`submitTxs` rejects `TxSendError.Refused {1}`. No mixed submission array is produced when batch consent expires before the first attempt.
   - Freeze CIP-103 signing failure `info` as the exact ASCII string `Transaction at index <n> failed`, where `<n>` is the first failing zero-based decimal index with no leading zeros or appended backend/sensitive text.
   - Keep authority identity, request IDs, and trusted approval records outside this envelope artifact. Task-300 and later IPC tasks own operational request schemas, but may not change these public result discriminants or values.

5. Add the minimum fixture matrix needed to lock every material decision.
   - Provider/invocation/negotiation: exact metadata and property kinds; absent own namespace property/`undefined`; omitted versus explicit `undefined`; extra and zero-method arguments; non-finite/non-JSON values; malformed entries; duplicate and reordered requests; unknown/unsupported/proposed-disabled omission; dependency/conflict resolution; repeated `enable`; authoritative registry ordering; CIP-95 override plus CIP-103 composition; and CIP-106/CIP-141 absence.
   - Address/signData: owned key-payment base types 0 and 2, pointer type 4, enterprise type 6, and key reward type 14 in raw hex and Bech32; mainnet `addr`/`stake` and test-network `addr_test`/`stake_test` HRPs; exact network nibble/route-network agreement; malformed/wrong HRP, wrong network, Byron/future address, and malformed raw bytes as `APIError.InvalidRequest`; valid unowned key credentials as `DataSignError.ProofGeneration`; payment/reward script types 1/3/5/7/15 as `DataSignError.AddressNotPK`. Address-returning methods emit raw lowercase hex only.
   - Base CBOR/scalars: minimal `value`, `transaction_unspent_output`, canonical empty witness set (`a0`), hash32, nullable `getUtxos`, and nullable side-effect-free `getCollateral`. `Coin` range is `0..18446744073709551615`; include canonical `00`, `17`, `1818`, and `1bffffffffffffffff` transition/boundary vectors, and reject negative `20`, tagged bignum above uint64, non-integer, trailing, and non-minimal integer encodings with `APIError.InvalidRequest`.
   - Errors/limits: every method/error union and envelope discriminant; out-of-range `PaginateError {maxSize}` distinct from policy validation, with `maxSize` a finite safe integer `>=0` describing the current result count; pagination `page` is a finite safe integer `>=0`, `limit` is a finite safe integer `1..100`, and `limit=0`, negative values, fractions, unsafe integers, `NaN`, and infinities reject. CIP-103 arrays contain `1..50` items; empty and 51-item arrays reject.
   - Size measurement is over decoded bytes represented by the hex argument, not hex character count and not the extracted transaction-body span. Exactly 65,536 bytes is accepted and 65,537 rejected for each complete `signTx`/`submitTx` `cbor<transaction>`, each CIP-103 item independently, and the exact decoded `signData` payload. There is no additional Daedalus aggregate batch-byte cap; the 50-item and per-item limits both apply.
   - CIP-142 network magic is a finite JavaScript integer in `0..4294967295` (Cardano `Word32`), with 0/max/unsafe-negative/fraction/`NaN`/infinity boundaries plus mainnet/preprod/preview/custom positives. Base `getNetworkId()` remains its separate CIP-30 number.
   - CIP-8: exact untagged `COSE_Sign1`, protected-header bytes, unprotected `hashed:false` and `version:1`, attached payload, empty AAD signature structure, `COSE_Key`, public key, and verified signature. Include negative odd-length, `0x`-prefixed, malformed-hex, tagged-CBOR, changed-payload, and legacy-missing-version verification-only cases.
   - CIP-95 precedence: raw 28-byte DRep ID selects DRep. A route-network-valid type-6 address whose credential equals the selected DRep hash is reinterpreted as DRep and emits the raw 28-byte hash header only after a named implementor/release proves that compatibility form; absent that evidence, task completion stops. A nonmatching type-6 address always follows ordinary payment semantics: owned payment success protects the full raw address, unowned key returns `DataSignError.ProofGeneration`; it is not categorically invalid. Script credentials remain the separate `AddressNotPK` case.
   - CIP-103: malformed outer/item shapes, empty/51-item arrays, duplicate transactions retained as independent aligned items, caller order, same-request parent dependencies, reference dependencies, forward/self/unresolved references, and conflict annotations are contract fixtures. These fixtures freeze request/result identity and rejection shape only; task-700/701/702 implement ledger resolution and conflict algorithms. Include exact first-index `info`, zero witness disclosure on any signing failure, all-success hashes, pre-attempt refusal, and direct rejected aligned mixed hash/`TxSendError` array surviving the internal envelope.
   - CIP-104: requested-but-policy-disabled omission and absent namespace only, plus an explicit unresolved-encoding marker linked to `task-404`. Do not include a guessed positive encoding.

6. Verify each fixture through independent representations and runtime-neutral schema tests.
   - Store exact lowercase, even-length, unprefixed hex plus expected CBOR diagnostic notation where applicable. Decode the original bytes; do not derive the golden hex by reserializing a semantic object.
   - For addresses, store raw bytes, intended `addr`/`addr_test`/`stake`/`stake_test` HRP, Bech32 text, address type/network nibble, route network, ownership/script classification, and normalized credential. Decode Bech32 to exact raw bytes and reject HRP/payload disagreement; never infer the HRP.
   - For the CIP-95 DRep fixture, independently compute/verify the public-key Blake2b-224 hash, type-6 address bytes, Bech32 form, precedence decision, and COSE protected-header bytes, then compare the matching reinterpretation against at least one named public ecosystem implementation or reproducible release artifact. Record source/release identity and reproducible output; self-derived bytes alone cannot pass the gate.
   - Keep test cryptographic material deterministic and explicitly non-secret; never use a real wallet key or user data.
   - Use Ajv 6.12.6 in draft-07 mode to meta-validate every schema and validate every declared positive and negative JSON-representable fixture. The manifest's JavaScript-only cases are exercised by a small table-driven invocation-model test, not a production validator.
   - Enforce ingress order in fixtures/documentation: authenticate the minimum exact guest WebContents/top-frame/document boundary before parsing detailed attacker-controlled payloads; then validate invocation shape, decoded hex/CBOR lengths, schemas, capability/route state, and all product limits before backend, consent, signer, submission, or other privileged side effects. Wrong authority never gains a payload-dependent privileged side effect or detailed stale result.

7. Add one focused Jest specification and documentation synchronization.
   - Assert every manifest method references existing request/success/rejection schemas and every implemented/gated standard has the required fixture disposition.
   - Assert every draft-07 schema meta-validates, every positive/negative fixture has the declared result, every manifest method/property/error is covered, every reference resolves, and no schema/fixture/public path is orphaned.
   - Send every success/rejection envelope, including CIP-103 mixed results, through Node's `worker_threads.MessageChannel` and assert exact own keys, discriminants, values, array alignment, and absence of `Error`/custom prototypes after the actual structured-clone round trip. JSON stringify/parse is not accepted as clone evidence.
   - Assert namespace/override/absence and repeated-enable rules, exact consent/error mappings and CIP-103 info, exact CBOR/CIP-8 bytes and signature, Bech32/raw-byte equality for all four HRP classes, Coin and scalar boundaries, and exact 65,536/65,537-byte behavior.
   - Update the PRD and evidence note with the same conflict decisions and artifact paths. Do not duplicate full schemas into the PRD.
   - After planning and implementation review approval, update only the selected task's tracker status and this plan's lifecycle fields.

## Acceptance Criteria

- A single declarative manifest covers every provider property, namespace, base CIP-30, CIP-95, CIP-103, gated CIP-104, and gated CIP-142 public path. Every method fixes positional arity, omission/explicit-`undefined` behavior, defaults, strict object fields, extra-argument rejection, success schema, method-level rejection union, and limits; JavaScript-only negative values are explicit rather than forced into JSON.
- Every JSON-representable runtime value uses a meta-valid JSON Schema draft-07 schema. Ajv validates every positive/negative fixture, all references resolve, every public path/error is covered, and no schema or fixture is orphaned.
- Provider/namespace behavior is exact: own property kinds and metadata are frozen; unnegotiated namespaces are absent own properties and read as `undefined`; policy-disabled CIP-104/142 are omitted from `supportedExtensions`; repeated `enable` replaces the prior live API set; and malformed entries reject while well-formed unknown entries are omitted.
- Negotiation is deterministic: duplicate CIPs collapse, caller request order cannot alter fixed registry order `[95, 103, 104, 142]`, unavailable dependencies are omitted, registry-first conflict resolution is fixed, `getExtensions()` is authoritative, CIP-95 overrides base `signTx`, and CIP-103 composes with the effective signer without reordering.
- CIP-8 is not advertised as an extension and its produced vectors are exact untagged CBOR with `alg:-8`, exact raw `address`, attached original payload, empty AAD, `hashed:false`, `version:1`, consistent `kid` omission, and a correct COSE key/signature.
- The full method-level error matrix is frozen, including `TxSignError.DeprecatedCertificate = 3`, `PaginateError {maxSize}`, exact five-minute consent-expiry mappings, exact CIP-103 `Transaction at index <n> failed` info, pre-attempt batch refusal, and direct post-attempt mixed-array rejection.
- The internal envelope has only the exact `status`/`value` or `status`/`rejection.type`/`rejection.value` fields and six declared rejection discriminants. Values are data-only with no `Error` or custom prototype and survive an actual Node `MessageChannel` structured-clone round trip unchanged, including the CIP-103 mixed array.
- Address schemas and fixtures cover key-payment base types 0/2, pointer 4, enterprise 6, key reward 14, script-payment/reward negatives, owned/unowned keys, raw hex, all four `addr`/`addr_test`/`stake`/`stake_test` HRPs, route-network agreement, malformed HRP/bytes, and wrong-network cases. Address results are raw lowercase hex only.
- CIP-95 precedence is locked: raw 28-byte DRep ID selects DRep; a matching route-valid type-6 address receives the compatibility DRep reinterpretation/raw-hash header only with named implementor evidence; nonmatching type-6 remains ordinary payment signing with full-address header and owned success or unowned `DataSignError.ProofGeneration`; script credentials map separately to `AddressNotPK`. Missing matching-form provenance blocks completion.
- Deprecated `getCollateral` is side-effect-free, returns current pure-ADA candidates or `null`, never prepares collateral, and has no Daedalus-defined 5 ADA cap. Valid `cbor<Coin>` is canonical unsigned CBOR in `0..2^64-1`; zero/transition/max encodings pass and negative, overflow bignum, non-integer, trailing, or non-minimal encodings reject with `APIError.InvalidRequest`.
- `partialSign=true` with no applicable wallet key is frozen as successful canonical empty witness-set CBOR (`a0`), not `ProofGeneration`.
- Exactly 65,536 decoded bytes is accepted and 65,537 rejected for each complete transaction-CBOR argument and each exact decoded `signData` payload; every CIP-103 item receives the per-item limit and no additional aggregate-byte cap is introduced. Pagination requires finite safe `page>=0` and `limit=1..100`; CIP-103 requires 1-50 items; CIP-142 network magic requires a finite `Word32` integer `0..4294967295`.
- Minimal exact guest sender/top-frame/document authentication occurs before detailed payload parsing. Invocation/schema/decoded-byte/limit/capability checks then complete before backend access, consent, signing, submission, or any other privileged side effect.
- CIP-103 fixtures preserve malformed rejection, duplicate item identity, caller order, aligned indexes, dependency/reference/forward/self/unresolved/conflict representations, and first-failure semantics without implementing the later ledger graph. Signing failure releases no witness array; submission failure after attempts rejects directly with the attempt-all mixed array.
- CIP-104 has no guessed positive wire fixture. Its requested-but-disabled omission and absent namespace are tested, and the sole unresolved API-shape decision is explicitly delegated to `task-404`.
- CIP-142 uses negotiated `api.cip142`, returns a plain number, and has mainnet/preprod/preview/custom fixtures while base network ID behavior remains unchanged.
- Every implemented standard has provenance-backed golden/negative fixtures; all source revisions/releases and ecosystem observations are reproducible. Upstream prose conflicts are documented and resolved only according to locked PRD decisions.
- CIP-106 and CIP-141 have no method, namespace, descriptor, placeholder, or supported-extension fixture.
- No runtime connector, schema framework, dispatcher, backend, hardware, renderer, or later-phase infrastructure is introduced.

## Verification

- Run `yarn test:jest source/common/cip30/contracts/contractFixtures.spec.ts --runInBand`.
- Run `yarn compile` to prove JSON schema/fixture imports and test code type-check in the existing project.
- Run focused Prettier checks on the changed Markdown, JSON, and TypeScript files, followed by `git diff --check`.
- In the focused Jest test, instantiate the already resolved `ajv@6.12.6` in draft-07 mode, call schema meta-validation, compile every schema, validate every positive and negative JSON fixture, and assert all manifest schema/fixture references resolve with no orphan or unaccounted public method/error/property.
- Exercise omitted/explicit-`undefined`, excess arguments, zero-argument getters, strict object fields, functions/symbols/bigints, finite-number rules, duplicate/order/malformed negotiation, repeated enable, dependencies/conflicts, and namespace own-property absence through the table-driven declarative invocation model.
- Send every envelope variant through `worker_threads.MessageChannel`; assert exact own keys/discriminants/value, ordinary data-only prototypes, no `Error`/custom instance, and aligned CIP-103 mixed-array preservation after structured cloning.
- Use `cbor-diag --from hex --to diag` and `--to annotated` over each CBOR golden during authoring/review; compare exact bytes and expected diagnostic structure. Do not normalize a golden by round-tripping it.
- Use `bech32` without an HRP argument to decode every Bech32 fixture to its exact raw hex; encode only with the fixture's explicit `addr`, `addr_test`, `stake`, or `stake_test` HRP and compare exact text and route-network nibble.
- Independently verify the CIP-8 signature and public-key/address association and the CIP-95 DRep Blake2b-224/type-6-address/header equivalence. Record tool/library version, command, and result in the evidence note.
- Generate deterministic decoded-byte payloads rather than storing huge hex blobs and assert 65,536 acceptance/65,537 rejection for each single/batch transaction and data-signing path; assert Coin, pagination, item-count, and network-magic exact boundaries.
- Re-fetch authoritative CIPs and named ecosystem evidence immediately before freezing. Compare with the planning-time findings and document any delta rather than silently changing a PRD decision.
- Review the focused diff to ensure no package/lockfile, production source, review log, unrelated task status, or secret/test-wallet material changed.
- After implementation review, parse `dapp-browser-cip30-tasks.json` and verify only truthful selected-task/tracker metadata changed.
- No Storybook, Cucumber, renderer manual QA, Electron package, hardware, wallet, or network transaction test is required for this contract-only task.

## Risks And Open Questions

- Living-standard drift: source revisions can change after fixtures are frozen. Preserve provenance and require later conformance tasks to assess deltas; do not auto-download fixtures in tests.
- Specification contradictions: CIP-30 `getCollateral`, CIP-95 namespacing, and CIP-142 naming cannot be mechanically copied. The locked PRD decisions take precedence and the evidence note must retain the upstream discrepancy.
- CIP-95 interoperability evidence: the authoritative CIP still lacks completed test vectors. The DRep normalization fixture needs independent derivation plus one reproducible ecosystem comparison; a self-generated-only vector does not meet acceptance.
- CIP-104 remains the only allowed open API shape. A positive encoding in this task would be security/privacy-relevant scope creep and would bypass `task-404`.
- Canonicalization risk: decoding and re-encoding CBOR can alter bytes. Goldens are byte-first, carry diagnostic expectations, and are never regenerated from semantic values during tests.
- Boundary-fixture size: do not commit 64 KiB duplicated hex blobs. Generate deterministic boundary payloads in the focused test while freezing lengths and expected errors in the manifest.
- JavaScript/JSON mismatch risk: draft-07 cannot represent functions, namespaces, omitted arguments, or `undefined`. Keep those in the declarative invocation manifest and table tests; do not claim JSON Schema handles them.
- Schema-boundary risk: Ajv is used only to prove the frozen draft-07 artifacts and fixtures. It is already resolved in the repository graph and is not selected as the production validator; `task-300` must implement strict validators and prove equivalence without changing the frozen behavior.
- Scalar-range risk: JavaScript safe-integer limits do not constrain CBOR `Coin`, which remains hex-encoded bytes up to uint64, while pagination and network magic are plain JavaScript numbers with explicit safe/Word32 bounds.
- IPC workflow drift: generic thrown errors and payload logging would violate the PRD. The explicit result envelope and forbidden-data policy override those stale examples.

## Docs, Tracking, And Research Updates

- Update the PRD with a compact contract-freeze artifact/provenance table and the explicit task-002/task-300 boundary; do not rewrite its already locked public API sections.
- Add `research/02-cip30-wire-contract-evidence.md` as supporting evidence for source excerpts, revisions, ecosystem comparisons, commands, and conflict resolutions. It must state that the PRD and frozen artifacts are normative.
- Add the machine-readable contract manifest, runtime-value schema definitions, fixtures, and focused test under `source/common/cip30/contracts/`.
- Update `dapp-browser-cip30-tasks.json` only after implementation review confirms all acceptance criteria; preserve unrelated statuses, dependencies, and task graph structure.
- Update this canonical plan during later review/build lifecycle steps.
- Do not update `.agent/system/architecture.md`, `.agent/system/api-endpoints.md`, workflows, package manifests, translations, or research `01`; this task adds no live architecture, channel, UI, or dependency.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-002-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-002-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope creep: limited implementation to evidence, a declarative JavaScript invocation/namespace manifest, draft-07 value schemas, small/generated fixtures, Ajv meta/fixture proof, and MessageChannel clone proof. Production validators, dispatch, IPC, parser, dependency/conflict algorithms, backend, hardware, and UI remain downstream.
- Stale workflow text: explicitly rejected the IPC workflow's generic thrown-error and payload-logging examples for hostile/wallet data; live PRD security decisions control.
- Missing manifests/tests/docs: included exact JS invocation/property representation, draft selection, method-error inventory, schema meta/instance validation, runtime clone proof, complete address/Coin/scalar/limit/negotiation fixtures, PRD synchronization, evidence note, tracker lifecycle update, and focused commands. No i18n, Storybook, Cucumber, or production dependency is applicable.
- Security/wire drift: retained authority-before-detailed-payload ordering and pre-side-effect validation; exact data-only envelope/local rejection reconstruction; no sensitive error text; byte-first CBOR; four address HRPs; exact per-item limits; canonical empty partial witness success; direct CIP-103 mixed rejection; and CIP-106/CIP-141 absence.
- Hidden manual checkpoints: no user/manual checkpoint is required. The only fail-closed evidence condition is public implementor proof for the CIP-95 DRep vector; inability to obtain it blocks completion and must be reported rather than waived.
- Plan consistency: corrected CIP-95 precedence so nonmatching type-6 is ordinary payment, selected draft-07 plus a JS-only manifest, fixed exact limits/ordering/negotiation/envelopes/errors/addresses/Coin, and made schema/clone checks executable. CIP-104 remains the sole unresolved API shape under `task-404`; task-300 still owns production validators.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Completed. Planning and implementation reviews are approved. The frozen manifest, draft-07 schemas, provenance-backed fixtures, focused executable contract tests, PRD boundary, and standards evidence are synchronized with task tracking. Tagged `COSE_Sign1` rejection and Blake2b-224 public-key-to-DRep association are executable regression evidence. Ten focused Jest tests, TypeScript, ESLint, Prettier, JSON parsing, Git tracking, and whitespace checks pass. CIP-104 remains omitted pending `task-404`; CIP-106 and CIP-141 remain absent. No production connector, validator, IPC, package, backend, hardware, renderer, or user-manual step was introduced.
