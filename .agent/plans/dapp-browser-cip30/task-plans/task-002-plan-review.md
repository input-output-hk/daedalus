Planner: Iteration 1
Timestamp: 2026-08-10T20:43:09Z

Created canonical plan `.agent/plans/dapp-browser-cip30/task-plans/task-002.md`.

Interaction mode: `autonomous`. No user input, manual testing, hardware, wallet, or audit evidence is required. Network research against living CIP and ecosystem sources is required and can proceed without user interaction.

Evidence consulted includes the PRD/task graph, threat-model research, required workflows and skills, live repository/history, current CIP-30/8/95/103/104/142 sources, the extensions register, and verified local CBOR/Bech32 tooling.

Plan summary:
- Freeze machine-readable public method, namespace, error, limit, and Electron result-envelope contracts.
- Add provenance-backed CBOR, Bech32, CIP-8, CIP-95, CIP-103, and negotiation fixtures with focused Jest coverage.
- Preserve PRD resolutions for `getCollateral`, CIP-95 namespacing/DRep normalization, CIP-103 mixed rejection, and `api.cip142`.
- Keep CIP-104 disabled and omitted until task-404 proves an exact interoperable encoding.
- Leave production validators, registry, IPC, parser, backend, hardware, and UI infrastructure to downstream tasks.
- Update PRD, supporting research, and selected-task tracking only through the later reviewed implementation lifecycle.

Self-review found no unassigned manifest/test/documentation work, hidden user checkpoint, security/wire drift, or later-task infrastructure creep. Both review logs remain untouched.

Outcome: Canonical plan drafted with planning status `in_review` and build status `in_progress`, ready for critique.

Critiquer: Iteration 1
Timestamp: 2026-08-10T20:48:17Z

The plan has a sound phase boundary and evidence-first direction, but several wire decisions remain underspecified or conflict with the locked PRD and current CIP text. These gaps would let task-002 complete without actually satisfying its “no open API-shape decision” acceptance criterion.

Blocking findings:
1. **Critical: CIP-95 type-6 handling is internally inconsistent and not yet provenance-safe.** The plan classifies nonmatching type-6 enterprise credentials as negative vectors (`task-002.md:142`), while the PRD says a nonmatching type-6 address follows ordinary payment-key semantics (`dapp-browser-cip30-prd.md:571`). Current CIP-95 also identifies type 6 as a payment-key address and requires an address input’s full raw address bytes in the protected header; only raw `DRepID` unambiguously denotes the DRep key. Revise the fixture matrix to lock precedence explicitly: a type-6 address matching the selected DRep hash receives Daedalus’s compatibility reinterpretation and raw-hash header only if named implementor evidence proves it; otherwise completion must stop. A nonmatching type-6 address must be tested as an ordinary payment credential, with owned success and unowned `DataSignError.ProofGeneration`, not categorically rejected. Script credentials must separately map to `AddressNotPK`.
2. **High: JSON Schema alone cannot express the promised JavaScript invocation contract.** The plan requires schemas for every argument/default but does not define how schemas represent positional arity, omitted arguments versus explicit `undefined`, extra arguments, absent namespace properties, methods/functions, or non-JSON JavaScript values such as `NaN` and `Infinity`. This prevents exact schemas for `enable()`, optional pagination, optional `partialSign`, zero-argument getters, and provider properties. Select a JSON Schema draft and add a small explicit invocation/namespace representation that freezes arity, omission/default behavior, strict object properties, and JavaScript-only negative cases without turning it into the task-300 validator framework.
3. **High: product-limit scope and measurement remain ambiguous.** Freeze that 64 KiB means 65,536 decoded bytes, state whether equality is accepted, and specify whether it applies to the complete transaction CBOR or only its body span. Explicitly apply it to every CIP-103 transaction item and the exact `signData` payload bytes; state whether there is any aggregate batch-byte limit. Also freeze finite safe-integer pagination rules, whether `limit=0` or an empty CIP-103 array is valid, and the unsigned range for CIP-142 network magic. Preserve “before side effects,” but do not imply that payload validation precedes the minimal sender/frame authentication boundary.
4. **High: extension negotiation is not fully deterministic.** The plan says enabled order will be explicit but does not choose the algorithm. It must freeze duplicate requests, request order versus registry order, malformed extension entries, dependency behavior, mutually incompatible extensions, repeated `enable()` calls with a different requested set, and the distinction among `supportedExtensions`, policy-enabled extensions, and the authoritative `getExtensions()` result. It must also specify whether proposed-but-policy-disabled CIP-104/142 appear in provider metadata or are omitted there, not only from the enabled API.
5. **High: the complete rejection inventory and Electron envelope are still open designs.** “One clone-safe discriminated envelope” is insufficient without fixed fields and discriminants for success, `APIError`, `PaginateError`, `TxSignError`, `DataSignError`, `TxSendError`, and CIP-103’s directly rejected mixed array. Freeze method-by-method allowed rejection unions, exact canonical consent-expiry mappings, and a stable zero-based CIP-103 failing-index `info` format. Require prototype-free values and an actual structured-clone round trip, not merely JSON stringify/parse, so task-104/task-300 cannot choose incompatible envelopes later.
6. **High: the address and ledger-scalar fixture plan is incomplete.** Verification currently names only `addr`/`addr_test` HRPs (`task-002.md:185`), but base `signData` includes reward addresses and therefore needs `stake`/`stake_test` fixtures and network/credential checks. Add payment base, pointer, enterprise, reward, malformed-HRP/network, raw-hex, and script/unowned cases. Freeze the valid ledger `Coin` range and boundary encodings so “every valid Coin” and invalid-Coin rejection are testable rather than delegated implicitly.
7. **High: the proposed focused test does not prove schema or Electron-contract validity.** Reference existence and hand-written fixture assertions do not establish that every schema is valid under one declared draft, that positive and negative instances behave as declared, or that no public path/error is omitted. Add schema meta-validation and fixture validation using an explicitly available mechanism, or narrow the artifact claim and assign executable schema-equivalence proof to task-300. Independently test the envelope with the runtime’s structured-clone semantics and assert exact CIP-8 bytes, signature verification, Bech32 byte equality, limit boundaries, method/error coverage, and orphan detection.

Non-blocking observations:
- The task-002/task-300 split is appropriate: declarative frozen contracts belong here, while production validators and dispatch belong downstream.
- Keeping CIP-104 positive encoding out of this task is correct and avoids bypassing task-404.
- The autonomous interaction classification is reasonable because failure to obtain public evidence can block the task without user action.
- `Build Status: in_progress` is misleading while only the plan exists; use the lifecycle’s not-started equivalent until implementation begins.
- The artifact set could be simplified by grouping closely related schemas where that reduces reference churn without weakening method-level inventory.

Approval bar:
Revise the canonical plan to resolve the seven blocking items above, especially the CIP-95 type-6 contradiction, JavaScript invocation representation, exact limit semantics, deterministic negotiation, method-level rejection/envelope contract, complete address/Coin fixtures, and executable schema/structured-clone verification. Approval does not require resolving CIP-104’s positive encoding or adding downstream runtime infrastructure.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-10T20:55:12Z

Revised `.agent/plans/dapp-browser-cip30/task-plans/task-002.md` to resolve all seven consolidated blockers:

- Fixed CIP-95 type-6 precedence: matching DRep reinterpretation requires named implementor evidence; nonmatching type-6 addresses retain ordinary payment semantics; script credentials map separately to `AddressNotPK`.
- Selected JSON Schema draft-07 and added a declarative JavaScript invocation/property model covering arity, omission versus `undefined`, extra arguments, namespace absence, strict objects, and non-JSON values.
- Froze exact limits: 65,536 decoded bytes accepted and 65,537 rejected per complete transaction or payload, 1-50 batch items, pagination `page>=0` and `limit=1..100`, Word32 network magic, no aggregate batch-byte cap, and authority authentication before detailed payload validation.
- Made negotiation deterministic across duplicates, malformed entries, request ordering, dependencies, conflicts, repeated `enable`, provider metadata, policy filtering, namespace composition, and CIP-95/CIP-103 overrides.
- Defined exact data-only result-envelope fields and six rejection discriminants, method-level rejection unions, consent-expiry mappings, direct CIP-103 mixed rejection, and stable `Transaction at index <n> failed` information.
- Expanded fixtures for base, pointer, enterprise, reward, script, unowned, malformed, HRP/network, raw-hex, and Coin boundary cases.
- Added executable draft-07 meta/fixture validation through existing `ajv@6.12.6` and actual structured-clone testing through Node `MessageChannel`, without production infrastructure or package changes.

CIP-104 remains omitted pending task-404. Task-300 still owns production runtime validators, and later CIP-103 tasks retain dependency/conflict implementation ownership.

Final self-review confirmed the task boundary, security ordering, wire decisions, manifests, tests, documentation, lifecycle statuses, and review-log isolation are consistent. Focused Prettier and whitespace checks passed; both review logs remain untouched.

Outcome: Canonical task-002 plan revised and ready for focused approval.

Critiquer: Iteration 2
Timestamp: 2026-08-10T20:56:56Z

The revision truthfully resolves all seven iteration-1 blockers without expanding task-002 into production implementation:

- CIP-95 type-6 precedence now matches the PRD, and the compatibility reinterpretation remains blocked on named implementor evidence.
- JavaScript invocation behavior is separated from draft-07 value schemas.
- Size, pagination, batch, network-magic, and authority-order boundaries are exact.
- Negotiation, metadata, repeated enablement, dependencies, conflicts, ordering, and namespace composition are deterministic.
- Method-level errors and the data-only Electron envelope are fully discriminated and clone-tested.
- Address, HRP, credential, Coin, and scalar fixtures are complete.
- Ajv meta/instance validation and actual `MessageChannel` structured-clone verification provide executable evidence.

CIP-104 remains correctly delegated to task-404, and production validators, IPC, ledger algorithms, backend work, hardware work, and UI remain downstream. No revision-introduced blocker was found.

Blocking findings:
None.

Non-blocking observations:
- `Build Status: in_progress` remains premature while implementation has not started, as noted in iteration 1.
- The focused test relies on hoisted transitive `ajv@6.12.6`; it is verified in the current live dependency graph, but implementation verification should fail clearly if the Nix/CI install does not expose it.
- Grouping related schemas remains a reasonable simplification and is now explicitly permitted.

Approval bar:
Met. Implementation must still stop if the required named CIP-95 matching type-6 interoperability evidence cannot be reproduced; that is an acceptance gate, not a remaining planning defect.

Decision: approved
