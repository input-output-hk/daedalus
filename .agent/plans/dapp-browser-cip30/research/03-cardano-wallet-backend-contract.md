# cardano-wallet Backend Contract

Status: accepted task-003 contract under the user-confirmed cardano-wallet
implementation-signoff assumption recorded on 2026-08-11. Local validation is
complete; phase-2 implementation must still produce its own commit, review,
migration/rollback, integration, and pin evidence.

Normative public wallet behavior remains in the
[PRD](../dapp-browser-cip30-prd.md) and the frozen task-002 artifacts under
`source/common/cip30/contracts/`. This note freezes the backend delivery
contract that tasks 200-209 must implement and verify. It does not describe a
currently shipped API.

## Baselines

| Baseline                | Revision                                                                | Meaning                                                                                          |
| ----------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| Daedalus packaged input | `cardano-foundation/cardano-wallet/v2026-07-23`                         | Release input in `flake.nix`.                                                                    |
| Daedalus locked source  | `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`                              | Exact `flake.lock` revision; authoritative for bundled behavior.                                 |
| Initial sibling review checkout | branch `amw/cip30`, revision `d3d170d02df9e39be04d85f3ce09fca98c9c5380` | Aligned with `upstream/master` when validated on 2026-08-11; historical implementation starting point only. |
| Task-200 local foundation | branch `amw/cip30`, revision `b2d20f4385bfcb92454b4dec91f954a0babd13ac` | Rebased onto upstream `6761259ee91b138921231ce7fd1198679abfcc82`; unavailable and non-pin-eligible until the consolidated task-209 gate. |

The complete pin-to-sibling range changes 175 files. Most are CI, release,
delta-library publication, and unrelated feature work, but the range also
touches the checkpoint store, submission/migration/store tests, wallet tests,
delta-store architecture, Shelley server, and Swagger. Those changes are
relevant starting-point drift for `W/G/P`, persistence, migration, and rollback
implementation and must be reassessed by tasks 201, 202, and 208. Direct
inspection still confirms that no dApp capability, coherent context,
CIP-8/CIP-95, or write-ahead submission API exists at either baseline. The
sibling's untracked `.idea/` directory is unrelated and was not modified.

Representative validation commands:

```bash
git -C ../cardano-wallet show 724be55dc66cf67bc4427e8f1a9657a9d1d33d71:<path>
git -C ../cardano-wallet diff --stat 724be55dc66cf67bc4427e8f1a9657a9d1d33d71..d3d170d -- <paths>
git -C ../cardano-wallet status --short
git -C ../cardano-wallet rev-parse HEAD upstream/master
```

## Existing-Seam Matrix

| Requirement            | Pinned and sibling evidence                                                                                                                                                                                               | Decision                                                                                                                                                        |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Capability negotiation | No dApp capability endpoint exists.                                                                                                                                                                                       | Add strict `GET /v2/dapp-capabilities`; never infer support from a version string or endpoint probing.                                                          |
| Wallet UTxO snapshot   | `ApiWalletUtxoSnapshotEntry` contains only ADA and assets; `getWalletUtxoSnapshot` returns token bundles.                                                                                                                 | It is not a CIP-30 UTxO or review context.                                                                                                                      |
| Stored full output     | Primitive `TxOut` stores only address and token bundle.                                                                                                                                                                   | Never use it as exact output evidence; obtain full ledger/sealed bytes.                                                                                         |
| Full input lookup      | `getUTxOByTxIn` returns ledger `UTxO` only for Conway and Dijkstra; older eras return `InNonRecentEra`.                                                                                                                   | Advertise exact era coverage and fail closed; task-004 controls Daedalus's supported-era intersection.                                                          |
| Coherent wallet state  | `readWallet` reads checkpoint, metadata/delegation, and pending transactions in one wallet DB action, but node LSQ is separate.                                                                                           | Use the `W/G/P` capture protocol below; do not claim global atomicity.                                                                                          |
| Software signing       | `/wallets/{walletId}/transactions-sign` returns a modified full transaction and preserves existing witnesses by contract. V1 uses `addVkWitnesses`; V2 reseals recent-era transactions and unions address/VKey witnesses. | Reuse only if path-specific differential tests prove exact envelope invariants and reviewed-context input can be added safely.                                  |
| Data signing           | Existing metadata signing is Catalyst-oriented.                                                                                                                                                                           | Do not reuse it for CIP-8. Backend produces the complete frozen CIP-8/CIP-95 COSE result.                                                                       |
| Wallet submit          | `/wallets/{walletId}/transactions-submit` rejects foreign transactions. `submitTx` broadcasts before adding submission state.                                                                                             | Extend the wallet-scoped path with the write-ahead state machine below.                                                                                         |
| Proxy submit           | `/proxy/transactions` broadcasts through `submitExternalTx`; Swagger explicitly assigns retries to the caller and no wallet submission state is written.                                                                  | Never use it for the dApp path.                                                                                                                                 |
| Submission store       | Existing store retains sealed transactions and supports status/rollback operations.                                                                                                                                       | Reuse if task-208 proves it can represent every required state and identity without schema change.                                                              |
| Migration              | New-style migration creates a versioned backup before a forward step.                                                                                                                                                     | Backup is not proof an old binary can open the new DB. Prove old-pin open compatibility or require backup restoration.                                          |
| Upstream delivery      | `CONTRIBUTING.md` says the project is maintenance-only, while external features and PRs remain welcome subject to thorough review.                                                                                        | Tasks 200-208 accumulate as reviewable local commits. Task-209 owns one consolidated upstream PR, named authorized review, durable evidence, activation, and pinning for the complete range. |
| Daedalus octet stream  | `request.ts` accepts a hex string, sets length to half its characters, and writes with `hex` encoding.                                                                                                                    | Task-209 replaces this with one runtime-validated exact-byte representation.                                                                                    |

## Capability Contract

Add an additive endpoint with no body or query parameters:

```http
GET /v2/dapp-capabilities
```

The strict V1 response is:

```json
{
  "api_version": 1,
  "backend_build": {
    "version": "diagnostic-version",
    "source_revision": "0000000000000000000000000000000000000000"
  },
  "network": {
    "network_id": 0,
    "network_magic": 1,
    "genesis_hash": "0000000000000000000000000000000000000000000000000000000000000000",
    "current_era": "conway"
  },
  "capabilities": [
    {
      "name": "transaction-context",
      "revision": 1,
      "available_eras": ["conway"]
    },
    {
      "name": "reviewed-context-signing",
      "revision": 1,
      "available_eras": ["conway"]
    },
    { "name": "cip8-cip95", "revision": 1, "available_eras": ["conway"] },
    {
      "name": "durable-wallet-submit",
      "revision": 1,
      "available_eras": ["conway"]
    }
  ]
}
```

- Every object rejects unknown fields. `api_version` is exactly `1`.
- `source_revision` is 40 lowercase hex characters and must equal the exact
  source revision pinned by the packaged Daedalus build. `version` is
  diagnostic only.
- Network ID is `0` or `1`; magic is Word32; genesis is 32-byte lowercase hex.
- Capability names are nonempty lowercase kebab case, revisions are positive
  safe integers, and era lists are nonempty and unique.
- The four names above are required at revision 1. Future unknown names are
  ignored only after strict validation of the whole response. Duplicate known
  or unknown names, duplicate eras, malformed fields, contradictory network
  identity, partial capability sets, and old revisions invalidate the response.
- Daedalus validates the complete response at startup. It rechecks cached
  source/network identity and the relevant capability before every context,
  signing, data-signing, and submission call. Network, genesis, era, source, or
  capability change forces a fresh fetch and invalidates stale context.
- Capability eras indicate backend availability, not ledger validity. Daedalus
  uses the intersection with task-004's supported-era matrix.

## Coherent Transaction Context

Use one wallet-scoped operation for exact normal, collateral, and reference
inputs, all wallet UTxOs, ownership/proof state, pending submissions, protocol
context, and earlier outputs in the same ordered request. The route binds the
wallet. The request carries expected genesis/network identity and ordered exact
transaction CBOR. It may identify requested outpoints, but never supplies
authoritative paths, ownership flags, reduced outputs, protocol summaries, or
parent-output summaries.

### Capture Protocol

1. In one wallet DB transaction read checkpoint chain point `W`, monotonic
   wallet generation `G`, monotonic pending generation `P`, discovery and
   registration state, pending records, and wallet outpoints.
2. Acquire one node local-state query exactly at `W` and obtain era, protocol,
   network, protocol parameters, and every requested chain UTxO.
3. In a second wallet DB transaction confirm the same `W/G/P`.
4. On mismatch discard all material and retry the complete capture. Permit at
   most three attempts total.
5. Failure to acquire `W`, rollback/pruning during capture, missing generations,
   or three mismatches returns `context_unavailable` with no partial response.

`G` increments on checkpoint, discovery, registration, or ownership/proof-state
mutation. `P` increments on every submission insertion or transition affecting
available/pending UTxO. These generations are required new contract state, not
claims about the current implementation.

### Provenance And Conflicts

For each outpoint, authoritative exact-byte precedence is:

1. Output independently decoded from a strictly earlier transaction in this
   ordered request.
2. Output decoded from the wallet's durable pending sealed transaction.
3. Node UTxO acquired at `W`.

The wallet checkpoint `TxOut` supplies candidate membership/discovery only. If
multiple authoritative sources provide unequal bytes, return `context_conflict`;
equal bytes retain every provenance label. Missing/spent node inputs,
rolled-back/expired pending records, pending records without exact sealed bytes,
unsupported eras, or wallet outpoints without recoverable full bytes fail
closed. There is no reduced-output fallback.

Earlier outputs are derived by hashing the exact parent body and decoding its
indexed exact output. Self/forward references, out-of-range indexes, malformed
parents, duplicate parent identities with unequal envelope bytes, unresolved
outputs, incompatible normal/collateral duplication within one body, and
attempts to spend an already-pending input fail. A later CIP-103 item claiming
an input already claimed by an earlier item receives deterministic conflict
metadata naming the earlier index; it remains reviewable/signable and is
submitted under attempt-all semantics. Reference-input reuse is a dependency,
not a spending conflict.

### Recomputable Binding

`context_digest` is Blake2b-256 over this binary preimage:

```text
ASCII("daedalus-dapp-context-v1")
|| bytes(wallet_id)
|| bytes(genesis_hash)
|| chain_point(W)
|| u64be(G)
|| u64be(P)
|| vector(ordered_exact_transaction_bytes)
|| vector(canonical_context_records)
```

Encoding rules are normative:

- `u8`, `u32be`, and `u64be` are unsigned fixed-width big-endian integers.
  Boolean is `u8(0|1)`. `bytes(x)` is `u32be(length) || x`; `text(x)` is
  `bytes(UTF8(x))`; no JSON bytes are hashed. `vector(xs)` is `u32be(count) || each bytes(item)`. A derivation path is `u32be(count) || each u32be(index)`.
- `chain_point(W)` is `u8(0)` for genesis, otherwise `u8(1) || u64be(slot) || bytes(32-byte block_hash)`.
- A context record is `u8(record_type) || bytes(record_body)`. Records sort by
  complete encoded record bytes before vector encoding. Duplicate encoded
  records are invalid rather than collapsed.
- Record `0x01` (full output) body is `outpoint || u8(provenance_bits) || u8(role_bits) || bool(wallet_member) || u8(pending_state) || bytes(exact_ledger_txout_cbor)`. Outpoint is `32-byte tx_id || u32be(index)`.
  Provenance bits are earlier `0x01`, pending `0x02`, and node `0x04`; role bits
  are normal `0x01`, collateral `0x02`, reference `0x04`, and wallet snapshot
  `0x08`. Pending states are none `0`, authorized `1`, broadcasting `2`,
  submitted `3`, outcome-unknown `4`, in-ledger `5`, rejected `6`, expired `7`.
- Record `0x02` (ownership) body is `u8(credential_kind) || bytes(credential) || u8(ownership) || path || u32be(proof_bits)`. Credential kinds are payment `1`,
  stake `2`, DRep `3`, policy `4`; ownership is unowned `0`, owned key `1`,
  script `2`. Unowned/script paths have count zero. Proof-bit assignments are
  normal-input `0x00000001`, collateral `0x00000002`, withdrawal `0x00000004`,
  certificate `0x00000008`, required-signer `0x00000010`, native-script
  `0x00000020`, policy `0x00000040`, DRep-vote `0x00000080`, and DRep-certificate
  `0x00000100`; all other bits are zero at capability revision 1.
- Record `0x03` (protocol) body is `text(era) || u8(network_id) || u32be(network_magic) || u32be(protocol_major) || u32be(protocol_minor) || bytes(protocol_parameters_cbor)`. The last value is exactly
  `Cardano.Ledger.Binary.serialize' shelleyProtVer protocolParameters` using the
  pinned cardano-ledger dependency and era-specific protocol-parameter type at
  capability revision 1. Task-201 must freeze cross-language golden bytes
  before that revision can ship.
- Record `0x04` (registration) body is `u8(credential_kind) || bytes(credential) || u8(state)`, where states are unknown `0`, unregistered
  `1`, registered `2`, pending-registration `3`, pending-deregistration `4`.
- Record `0x05` (governance) body is `u8(credential_kind) || bytes(credential) || u8(governance_role) || u8(state) || u64be(deposit_coin) || bool(has_delegate) || [u8(delegate_credential_kind) || bytes(delegate)]`.
  Roles are DRep `1`, constitutional committee `2`, and stake pool `3`; states
  are unknown `0`, inactive `1`, active `2`, pending-registration `3`, and
  pending-deregistration `4`. The optional delegate suffix is present only when
  `has_delegate=1`; unknown deposit is invalid rather than encoded as zero.
- Record `0x06` (required proof) body is `u32be(transaction_index) || u8(proof_kind) || u8(credential_kind) || bytes(credential) || bool(required)`.
  Proof kinds are normal-input `1`, collateral `2`, withdrawal `3`, certificate
  `4`, required-signer `5`, native-script `6`, policy `7`, DRep-vote `8`, and
  DRep-certificate `9`; other values are invalid at capability revision 1.
- Record `0x07` (pending transaction) body is `32-byte tx_id || u8(pending_state) || bytes(exact_sealed_transaction) || vector(normal_outpoints) || vector(collateral_outpoints) || bool(has_expiry) || [u64be(expiry_slot)]`.
  Each outpoint vector item is exactly 36 bytes. The expiry slot is present only
  when `has_expiry=1`.

Daedalus independently recomputes the digest before trusted review and before
accepting a signer result. Mutation of any bound field changes the digest.

The backend creates a random 32-byte process key and random 16-byte process
generation at startup. Token payload is `u8(1) || 16-byte process_generation || u32be(capability_revision) || bytes(wallet_id) || 32-byte genesis_hash || 32-byte context_digest`. Token MAC is HMAC-SHA-256 over
`ASCII("daedalus-dapp-context-token-v1") || bytes(payload)` under the process
key. `context_token` is lowercase hex of `payload || 32-byte mac`; no alternate
serialization is accepted. The digest already binds ordered bodies and all
authoritative context. The backend stores no per-context record and compares
MACs in constant time.

Signing decodes the exact token, checks process generation/capability/wallet/
network, and recomputes the digest before key use. The backend token has no
wall-clock timeout. Main alone enforces the five-minute consent-inactivity rule.
Restart/key loss, capability change, or wallet/network mismatch requires new
context and trusted review. Later chain movement does not invalidate the
approved snapshot; submission may fail normally.

## Software Signing Decision

Keep `/wallets/{walletId}/transactions-sign` only if both `RootKeyAccessV1` and
`RootKeyAccessV2` pass task-203 differential evidence after accepting the exact
context token/digest, exact transaction and ordered parent bytes, request index,
and `partialSign`.

For both paths, body bytes, `isValid`, auxiliary data, native/Plutus scripts,
bootstrap witnesses, existing VKeys, datums, redeemers, and every non-VKey
witness class must be byte-identical. The sole allowed change is set-union
addition of valid VKey witnesses. Daedalus verifies the complete envelope/body,
immutable classes, exact VKey `(public_key, signature)` difference, each
signature, and expected credential. Removal, replacement, duplicate ambiguity,
or any other mutation fails.

`partialSign=false` requires complete wallet key/native-script proof.
`partialSign=true` returns every producible owned VKey and succeeds with the
canonical empty witness set when none applies. Current-batch outputs, collateral,
withdrawals, required signers, certificates, governance, and policy keys use
backend-owned context. Staged witnesses remain memory-only in Daedalus.

If either path cannot satisfy these rules through an additive request extension,
task-203 must record the failed seam and justify the smallest replacement. A
witness-only endpoint is not assumed merely because it is convenient.

## CIP-8 And CIP-95 Boundary

The backend owns credential authentication and key selection and returns the
complete frozen task-002 `COSE_Sign1` and `COSE_Key`; there is no raw-signature
alternative. The request contains exact address or raw DRep-ID input, exact
payload bytes, expected network/genesis, and transient passphrase. Backend alone
chooses payment, stake, or role-3/index-0 DRep key and classifies registration,
including pending certificates.

The result is exact untagged COSE hex plus normalized credential kind/bytes. It
uses `alg:-8`, attached payload, empty external AAD, `hashed:false`, `version:1`,
and no `kid`, including task-002's matching type-6 DRep normalization. Daedalus
independently verifies all exact bytes, headers, address/DRep association,
public-key hash, payload, COSE key, and Ed25519 signature. Catalyst metadata,
committee/pool credentials, script credentials, and caller paths are excluded.

## Durable Wallet Submission

Reuse the wallet-scoped submit endpoint and one cardano-wallet submission store.
The dApp path never uses `/proxy/transactions` or a Daedalus journal.

Identity is `(wallet_id, tx_id)` plus exact sealed bytes and exact normal/
collateral input sets. Recompute `tx_id` from exact body bytes. The same key with
different envelope bytes or input accounting is `identity_conflict` and never
broadcasts, including same-body envelopes with different witnesses.

| State                                  | Durable transition and behavior                                                                                                                                                     |
| -------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `authorized`                           | Persist exact bytes, input sets, expiry, authorization marker, and `attempt_generation=0` in one wallet DB transaction before any node call. This commit is the point of no return. |
| `broadcasting(generation, started_at)` | Commit before `postSealedTx`; only one serialized caller owns an attempt.                                                                                                           |
| `submitted(accepted_at)`               | Commit after deterministic node acceptance.                                                                                                                                         |
| `rejected(code)`                       | Terminal deterministic rejection; release pending input claims atomically but retain evidence.                                                                                      |
| `outcome_unknown(generation)`          | Transport outcome is not authoritative; do not blindly retry.                                                                                                                       |
| `in_ledger(slot)`                      | Chain observation from `submitted` or `outcome_unknown`; retain exact bytes.                                                                                                        |
| `expired`                              | Terminal non-ledger expiry; release claims but retain evidence.                                                                                                                     |

Rollback transitions `in_ledger -> submitted` and restores relevant pending
claims. A persisted `authorized` record means no node call was started. Startup
does not submit it or resume an interrupted batch; it remains recoverable until
an exact dApp retry owns the next serialized attempt, it expires, or existing
safe user-forget semantics remove its claim. A persisted `broadcasting` record
becomes `outcome_unknown` before reconciliation.

Chain and mempool observations use separate same-node protocols with a bounded
consistency bracket: read synced canonical `tip_before`; query canonical
transaction observation; query local mempool membership; read synced canonical
`tip_after`. Accept the observations only when `tip_before == tip_after`; retry
the complete bracket at most three times, otherwise retain `outcome_unknown`
without broadcast. Chain or mempool presence advances to `in_ledger` or
`submitted`. If the transaction has an `invalid_hereafter` slot and the stable
tip is beyond it with no canonical inclusion, it becomes `expired` and is never
retried. Without expiry, or while still valid, coherent absence from both views
makes the exact same sealed bytes retry-eligible: transition to `authorized`
with incremented `attempt_generation`, but wait for an exact dApp retry. This
does not claim global network absence; exact identity and the unique wallet
record make later retransmission idempotent. An unavailable/unsynced query
retains `outcome_unknown`. No startup or reconciliation cycle initiates a node
attempt by itself.

Exact replay in `authorized` may own the next serialized attempt. Replay in
`broadcasting`, `outcome_unknown`, `submitted`, or `in_ledger` returns existing
hash/status without directly broadcasting; only the reconciliation transition
above can re-authorize an unknown attempt. Replay in `rejected` or `expired`
returns the recorded terminal failure. A per-wallet transaction lock and unique
DB key serialize first calls and replay. Normal and collateral inputs are
claimed in the same transaction as `authorized`; reference inputs are not.
Never-broadcast records remain explicit and recoverable. Transactions without
an upper validity bound do not expire by wall clock.

CIP-103 runs this state machine sequentially in request order after consent,
attempts each item independently despite earlier failures, and returns aligned
results. It adds no batch journal.

Prefer the existing schema only after task-208 proves it can express every
state. Otherwise require a versioned atomic forward migration from fixtures at
the pinned schema, automatic backup, restore rehearsal, and explicit proof that
the old pin can open the result or must restore the backup before rollback.

## Error And Privacy Matrix

| HTTP | Backend tag                   | Frozen public result and `info`                                        |
| ---- | ----------------------------- | ---------------------------------------------------------------------- |
| 400  | `dapp_invalid_request`        | `APIError.InvalidRequest`, `Invalid backend request`                   |
| 400  | `dapp_context_conflict`       | `APIError.InvalidRequest`, `Backend context conflict`                  |
| 400  | `dapp_identity_conflict`      | `APIError.InvalidRequest`, `Submission identity conflict`              |
| 409  | `dapp_account_changed`        | `APIError.AccountChange`, `Wallet or network changed`                  |
| 503  | `dapp_context_unavailable`    | `APIError.InternalError`, `Wallet context unavailable`                 |
| 500  | `dapp_internal_error`         | `APIError.InternalError`, `Backend operation failed`                   |
| 403  | `dapp_tx_proof_generation`    | `TxSignError.ProofGeneration`, `Transaction proof unavailable`         |
| 403  | `dapp_deprecated_certificate` | `TxSignError.DeprecatedCertificate`, `Deprecated certificate`          |
| 403  | `dapp_data_proof_generation`  | `DataSignError.ProofGeneration`, `Data proof unavailable`              |
| 403  | `dapp_data_address_not_pk`    | `DataSignError.AddressNotPK`, `Address is not a public-key credential` |
| 409  | `dapp_submission_failed`      | `TxSendError.Failure`, `Transaction submission failed`                 |
| 503  | `dapp_submission_unavailable` | `TxSendError.Failure`, `Transaction submission unavailable`            |

Malformed bytes/body/token, wrong network/era/field, invalid references, and
same-request conflicts use the three 400 tags. Wallet/network route change uses
`dapp_account_changed`. Backend process-generation mismatch after restart,
capture retry exhaustion, and node query failure use
`dapp_context_unavailable`; unexpected persistence failure uses
`dapp_internal_error`. Wrong passphrase, missing/incomplete proof, unsupported
coverage, deprecated certificates, unowned/script data credentials, and
deterministic/transient submission failures use their exact domain tags above.
No other HTTP/tag pair is accepted at capability revision 1.

User refusal remains main-owned. Each mapping uses empty or one fixed
nonsensitive `info` value. Public errors, backend HTTP text, traces, metrics, and
transaction logs must never contain exact transactions/outputs, addresses,
payloads, public keys, signatures, passphrases, tokens/digests, origins,
derivation paths, full URLs, or database content. Local diagnostics retain only
a static redacted category.

## Requirement-To-Evidence Assignment

| Task       | Required test/evidence layers                                                                                                                                                                                                                                                                                                                                                            |
| ---------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `task-200` | `lib/api` schema units, Swagger/golden consistency, strict malformed/duplicate/partial/old/build/network cases, exact HTTP/tag/info mapping and response-body redaction, exact bare-`err404` handler proof, compiled unavailable-route integration scenario, and exact candidate package identity. Live success is deferred to task-209. |
| `task-201` | Network LSQ units and local-cluster integration for exact-point capture, three-attempt races, rollback/pruning, full-output pin/sibling goldens, datum/reference-script fidelity, era failure, digest fixtures, and token restart invalidation.                                                                                                                                          |
| `task-202` | Wallet unit/property and discovery tests for `G/P`, ownership, source precedence, and adversarial earlier/pending/self/forward/duplicate/conflicting dependencies.                                                                                                                                                                                                                       |
| `task-203` | `Cardano.Wallet.Shelley.TransactionSpec`, `TransactionLedgerSpec`, API integration, and V1/V2 differential fixtures for every immutable envelope/witness class, VKey-only delta, current-batch context, partial modes, collateral, and required signers.                                                                                                                                 |
| `task-204` | Wallet/API unit and integration goldens for complete payment/stake COSE, exact headers/payload/key/signature, wrong passphrase, script, and unowned credentials.                                                                                                                                                                                                                         |
| `task-205` | Derivation/discovery units and wallet/API integration for role-3 DRep, public keys, pending registration, type-6 normalization, and DRep COSE.                                                                                                                                                                                                                                           |
| `task-206` | Wallet transaction-ledger differential coverage for Conway fields, DRep certificates/votes, deprecated certificates, completeness, and unsupported credentials.                                                                                                                                                                                                                          |
| `task-208` | Submission model/property, `DB.Store.Submissions`, SQLite, API/local-cluster fault injection around every durable/broadcast transition, same-node reconciliation snapshots, bounded retry generations, concurrency, unknown outcome, confirmation/expiry/rollback, CIP-103 order, pinned-schema migration, backup restore, old-pin open-or-restore proof, and transaction-log redaction. |
| `task-209` | One consolidated upstream PR/review for the complete task-200-through-task-208 range; Daedalus runtime/API validation and Jest for exact octet streams, exact error/info mapping, capability downgrade/malformed behavior, and no sensitive HTTP body/trace/metric/log values; real aggregate HTTP/mTLS and candidate-backend Nix smoke; migration/rollback evidence review; activation and integration before pin; rerun after exact pin update. |
| `task-304` | Independently encode every context record and recompute digest/token-bound identity from backend fixtures; mutate every field; reject unknown tags, duplicate records, malformed lengths, network/wallet mismatch, and stale process generations before review.                                                                                                                          |
| `task-306` | Independently compare full signing envelopes, immutable witness classes, exact body, and VKey set difference; verify every new key/signature and reject all non-VKey mutation.                                                                                                                                                                                                           |
| `task-307` | Independently decode and verify backend-produced COSE bytes, headers, payload, key association, normalization, and Ed25519 signature against task-002 goldens.                                                                                                                                                                                                                           |

There is no task-207 in the task graph. Every reassignment must update the
tracker. The user-authorized assumption accepts this task-003 assignment only;
tasks 200-209 and 304/306/307 must provide their concrete implementation and
review evidence.

## Delivery And Pin Gate

Task-003 is a validation-only exception to candidate-commit sequencing because
it changes no sibling source. Phase-2 tasks start from an upstream-aligned
sibling branch. Tasks 200-208 produce self-reviewed, tested local implementation
commits without opening incomplete per-task upstream pull requests. Task-209
submits the complete range in one consolidated upstream PR, records named
authorized review and durable evidence, applies review fixes as follow-up commits
rather than amendments, runs full-range migration/restore or rollback evidence
and Daedalus integration, activates the complete capability response, and alone
updates `flake.nix`/`flake.lock` to the exact reviewed revision before rerunning
post-pin integration.

## Task-200 Implementation Evidence

Task-200 completed the intentionally unavailable local foundation at
`b2d20f4385bfcb92454b4dec91f954a0babd13ac`, rebased onto upstream
`6761259ee91b138921231ce7fd1198679abfcc82`. It adds the strict capability and
fixed-error contracts, clients, links, Swagger, golden coverage, a Conway-only
constructor, deterministic Dijkstra refusal, and an unconditional bare-`err404`
handler with no activation input. It adds no persisted state or migration.

Clean Cabal API/application/unit/integration builds, 28 focused dApp tests, 200
capability roundtrip/schema checks, error-to-Swagger coverage, OpenAPI and
formatting checks, and exact-revision Daedalus bridge/mainnet builds passed. The
local-cluster scenario compiles, but real HTTP/mTLS execution was blocked by a
missing sibling-flake Nix store source and is not claimed. Task-209 must run that
evidence against the complete range before activation and pinning. The local
foundation remains unavailable, unpinned, and not submitted upstream by design.

## Local Validation Evidence

Agent-executable validation on 2026-08-11 produced:

- A table-driven strict capability checker passed one valid example and seven
  negative cases: unknown field, duplicate capability, duplicate era, partial
  set, zero revision, uppercase source revision, and out-of-range network magic.
  This checker is design evidence only; task-200 owns production schemas and
  Haskell/Swagger tests.
- A synthetic context preimage using wallet text `wallet-test`, genesis bytes
  `01` repeated 32 times, point slot 42 with block hash `02` repeated 32 times,
  `G=7`, `P=9`, transaction bytes `84a0a0f5f6`, full-output record hex
  `0100000033aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa00000000040101000000000782011a000f4240`,
  and protocol record hex
  `030000001c00000006636f6e776179000000002a000000090000000000000001a0`
  produced true 32-byte Blake2b digest
  `282df275055e203adbdc00ea80ec82556b0fea423d834c05e18c8ff388ec3846`.
  Blake2b-512 truncation is not equivalent and is rejected.
  Mutating `G`, `P`, and the transaction bytes produced, respectively,
  `98cd9842ca26d77d2024474816ee3626489087fd30dd323880d68246c72ce991`,
  `ef3520b552c2d4bd3f2a989e9177739bee87e1b809895c14c2dbe1d3940f0348`,
  and `e604d429ae4a562c6d8a7fe407df204fed67d7f56bf21088033f8f2fe67e9826`.
  All four differ. Task-201 owns ledger-derived golden records and independent
  cross-language recomputation.
- A deterministic token fixture using process generation `44` repeated 16
  bytes, capability revision 1, wallet `wallet-test`, genesis `01` repeated 32
  bytes, the base context digest above, and HMAC key `55` repeated 32 bytes
  produced payload
  `0144444444444444444444444444444444000000010000000b77616c6c65742d746573740101010101010101010101010101010101010101010101010101010101010101282df275055e203adbdc00ea80ec82556b0fea423d834c05e18c8ff388ec3846`
  and MAC
  `3acbbd506128450a4840ea2619e1319fc3286d1ba9d9620b03ba7c4ea3aa2dc0`.
  The token is their direct concatenation. Production keys/generations are
  random and never logged; this synthetic fixture is non-secret.
- The submission model classified 12 legal and 30 illegal directed transitions
  across the seven states. Legal transitions are `authorized -> broadcasting| expired`, `broadcasting -> submitted|outcome_unknown|rejected|expired`,
  `submitted -> in_ledger|expired`, `outcome_unknown -> authorized|in_ledger| expired`, and `in_ledger -> submitted`. The return to `authorized` requires a
  stable tip-bracket around separate same-node chain/mempool observations,
  exact-byte identity, and an incremented attempt generation; it does not start
  a retry without a later exact dApp call.
- The tracker parsed successfully, task-002 remained `completed`, task-003
  remained `pending`, all four task-003 target paths are documentation/evidence
  paths, `git diff --check` passed, and four local Markdown link targets existed.
- The repository-wide `yarn prettier:check <paths>` wrapper still prepends its
  global `**/*.*` input and reports unrelated baseline formatting warnings.
  `.agent/` is also ignored by the repository Prettier configuration. The new
  research and canonical plan pass direct Prettier with
  `--ignore-path /dev/null`; established PRD/tracker files were instead checked
  by JSON parsing, focused diff inspection, and `git diff --check` to avoid
  unrelated whole-file reformatting.

## Upstream Review Record

Current state: signoff assumed by explicit user direction on 2026-08-11 so the
orchestration may proceed. The user did not provide external identities or a
durable upstream URL; these fields are recorded as unavailable rather than
fabricated. This assumption accepts the contract and evidence matrix for
task-003 planning only and does not substitute for the concrete reviews and
artifacts required from tasks 200-209.

| Field                                  | Recorded value                                                     |
| -------------------------------------- | ------------------------------------------------------------------ |
| Implementation work owner              | Not supplied; covered by the user's assumed implementation signoff |
| Authorized cardano-wallet reviewer     | Not supplied; covered by the user's assumed implementation signoff |
| Durable issue/discussion/design-PR URL | Not supplied                                                       |
| Contract decision                      | Assumed accepted by user direction for task-003 continuation       |
| Evidence-matrix decision               | Assumed accepted by user direction for task-003 continuation       |
| Conditions/reassignments               | None supplied                                                      |

The consolidated task-209 review must explicitly decide: capability schema/check timing; `W/G/P`
capture and provenance; digest/token lifetime; V1/V2 signing reuse; complete
backend-produced COSE; submission transitions/replay; migration/rollback;
error/privacy boundaries; task/test assignment; API compatibility; and intended
upstream implementation PR sequence. Tasks 200-208 provide local implementation
and internal review evidence; task-209 replaces this planning assumption with
the concrete consolidated upstream review and delivery evidence.
