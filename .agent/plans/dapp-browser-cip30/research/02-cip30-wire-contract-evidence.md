# CIP-30 Wire Contract Evidence

Status: accepted supporting evidence for task-002. The
[PRD](../dapp-browser-cip30-prd.md) and frozen artifacts under
`source/common/cip30/contracts/` are normative for Daedalus. Upstream documents
remain living standards and do not silently mutate these contracts.

## Frozen Sources

Sources were re-fetched on 2026-08-10. The revisions below identify the latest
commit affecting each path at review time; they are provenance, not dependency
pins.

| Contract | Status | Source path | Observed revision |
|---|---|---|---|
| CIP-8 | Active | `cardano-foundation/CIPs/CIP-0008/README.md` | `86b89208d3b2aabb5dcc5b778dfbe09096b4e114` |
| CIP-30 | Active | `cardano-foundation/CIPs/CIP-0030/README.md` | `86b89208d3b2aabb5dcc5b778dfbe09096b4e114` |
| CIP-30 extensions register | Registry | `cardano-foundation/CIPs/CIP-0030/extensions-register.md` | `aa314c019b857cbaea38e16b1570af90cecefd38` |
| CIP-95 | Active | `cardano-foundation/CIPs/CIP-0095/README.md` | `20c819b25abee6551a3ef51778b975e7463e1269` |
| CIP-103 | Active | `cardano-foundation/CIPs/CIP-0103/README.md` | `86b89208d3b2aabb5dcc5b778dfbe09096b4e114` |
| CIP-104 | Proposed | `cardano-foundation/CIPs/CIP-0104/README.md` | `86b89208d3b2aabb5dcc5b778dfbe09096b4e114` |
| CIP-142 | Proposed | `cardano-foundation/CIPs/CIP-0142/README.md` | `86b89208d3b2aabb5dcc5b778dfbe09096b4e114` |

## Resolved Differences

| Topic | Upstream or ecosystem evidence | Frozen Daedalus decision |
|---|---|---|
| Deprecated `getCollateral` | CIP-30 prose discusses errors, on-demand preparation, and an imprecise 5 ADA cap, while its API type is nullable. | Side-effect-free current pure-ADA candidates or `null`; every canonical ledger `Coin` in `0..2^64-1` is accepted; 5 ADA is only a preparation preference. |
| CIP-95 getter namespace | The registered getter example omits `.cip95`, but CIP-95 says its methods are namespaced. | All four getters/signing methods are under `api.cip95`; only the backwards-compatible `api.signTx` behavior is overridden. |
| CIP-95 type-6 DRep form | Cardano JS SDK commit `0c0e700237ca524d8bd73054e3518fb182d09e46`, `packages/key-management/src/cip8/cip30signData.ts`, selects `DREP_KEY_DERIVATION_PATH` when a type-6 enterprise credential equals the DRep key hash. Its matching test is in `packages/key-management/test/cip8/cip30signData.test.ts`. The SDK preserves the complete address in COSE headers. | A matching route-valid type-6 address selects the DRep key, proving the compatibility form. Daedalus then applies the PRD's stricter normalization: matching type-6 and direct raw DRep-ID input both produce the raw 28-byte DRep hash in the protected `address` header. A nonmatching type-6 address keeps ordinary payment semantics. |
| CIP-8 output profile | CIP-8 permits more general forms and hashing; CIP-30 fixes un-hashed address signing. | Produce untagged `COSE_Sign1`, attached exact payload, empty external AAD, `alg:-8`, `hashed:false`, `version:1`, and no `kid`. Never produce the legacy missing-version form. |
| CIP-103 failure | CIP-103 requires caller order, all-or-nothing witness disclosure, attempt-all submission, and a thrown aligned mixed array. | Preserve order and indexes. Signing uses `Transaction at index <n> failed`. Submission rejects directly with the mixed array after attempts begin; the Electron envelope carries it as plain data. |
| CIP-104 encoding | `cbor<Bip32PublicKey>` has no precise CDDL, byte-string rule, or independent golden vector in the Proposed CIP or committed implementor evidence. | Terminal-disabled by task-404. Omit CIP-104 from metadata, negotiation, namespaces, consent, and backend access. Reopen only with a named implementor release and reproducible exact output for a deterministic 64-byte account xpub. |
| CIP-142 namespace | Prose says `cip-142`; the JavaScript example uses `api.cip142`. | Use negotiated `api.cip142`; return a finite Word32 JavaScript number. |

## CIP-104 Terminal Decision

On 2026-08-27, task-404 closed with the disabled outcome. The Proposed CIP
names Eternl, newm-chain, and Gero as implementors but provides no
`Bip32PublicKey` CDDL or reproducible request/result vector. Repository
contracts, task-002 research, committed history, and the pinned cardano-wallet
capability evidence contain no independent 64-byte account-xpub CBOR result.
Raw 64 bytes, a CBOR byte string, and any other encoding therefore remain
intentionally undecided.

Daedalus exposes no CIP-104 namespace, method, disclosure prompt, supported
extension claim, executor operation, or backend request. No xpub can enter the
dApp logging, telemetry, grants, or response-cache path. Future enablement
requires a new reviewed task with a named implementor/release, deterministic
input, exact output, reproducible command, and byte-for-byte Daedalus
comparison; this terminal record must not be reinterpreted as support.

## Golden Evidence

- `contract-manifest.json` freezes provider properties, JavaScript invocation
  semantics, all 22 public method paths, deterministic extension negotiation,
  method-level rejection unions, exact product limits, consent-expiry mappings,
  and the data-only Electron result envelope.
- Draft-07 schemas freeze JSON-representable common values, typed errors, and
  result envelopes. JavaScript-only values such as `undefined`, functions,
  symbols, bigints, `NaN`, infinities, inherited fields, and accessors remain in
  the declarative invocation contract rather than being misrepresented as JSON.
- `cip8-cip95-fixture.json` uses an explicitly non-secret deterministic Ed25519
  seed. Its Blake2b-224 DRep ID is
  `27e38d0e19e3434e33fbd001d3fe04b5b76763f88acd625e0d770b43`; the matching
  mainnet type-6 address is
  `addr1vyn78rgwr835xn3nl0gqr5l7qj6mwemrlz9v6cj7p4msksc89qqvj`.
- The positive CIP-8 vector is byte-first. Tests decode the committed bytes,
  verify the Ed25519 signature, require the raw 28-byte protected address,
  require `hashed:false` and `version:1`, and reject tagged, malformed, changed,
  or production-missing-version forms.
- Address fixtures cover key base types 0/2, pointer type 4, enterprise type 6,
  reward type 14, script types 1/3/5/7/15, all four address HRPs, network
  agreement, matching/nonmatching DRep precedence, ownership, and script errors.
- Coin fixtures freeze canonical CBOR transitions and uint64 maximum. The
  canonical empty witness-set success is `a0`.
- Base fixtures freeze a minimal `value`, exact transaction input/output/UTxO,
  and nullable `getUtxos`/`getCollateral` results. CIP-103 fixtures freeze
  duplicate identity, dependency/reference/conflict representations,
  all-or-nothing signing failure, all-success results, pre-attempt refusal, and
  the direct aligned mixed post-attempt rejection.

## Reproduction And Tooling

Evidence was reproduced with Node `v24.16.0`, `cbor@5.0.2`, `bech32@2.0.0`,
`blakejs@1.1.0`, `ajv@6.12.6`, `cbor-diag-cli 0.1.8`, and Bech32 CLI `1.1.2`.

Representative checks:

```bash
cbor-diag --from hex --to diag <<< '<coseSign1 hex>'
bech32 <<< 'addr1vyn78rgwr835xn3nl0gqr5l7qj6mwemrlz9v6cj7p4msksc89qqvj'
yarn test:jest source/common/cip30/contracts/contractFixtures.spec.ts --runInBand --coverage=false
yarn compile
```

The focused Jest test meta-validates and compiles every file, definition, and
inline schema; requires valid/invalid samples for every definition; resolves
every manifest reference; checks every public path, argument/default, success,
and rejection mapping; executes invocation and negotiation edge cases; verifies
base/CIP-103/CBOR/COSE/signature/address/error/limit fixtures; and sends every
envelope variant through Node `MessageChannel` structured clone.

## Residual Gates

- Task-300 must implement production strict validators and prove behavior
  equivalent to these artifacts. Ajv is test evidence only.
- Task-004 owns complete transaction/output CBOR slicing, eras, and semantic
  ledger validation. These fixtures do not claim full transaction coverage.
- Task-404 owns the terminal CIP-104 interoperability decision.
- Later conformance work must compare upstream and implementor changes against
  this frozen baseline and explicitly reopen affected decisions when needed.
