# Task task-201: Implement full-ledger UTxO and transaction-context resolution

## Task

- Task ID: `task-201`
- Title: `Implement full-ledger UTxO and transaction-context resolution`
- Phase: `phase-2` (`cardano-wallet Backend Foundations`)
- Priority: `critical`
- Tracker state at planning: `pending`
- Repository classification: cross-repository backend task. The production implementation belongs in sibling `../cardano-wallet`; Daedalus receives only contract fixtures and task records now, and consumes the complete reviewed backend through `task-209`.

## Why Now

- Completed dependency `task-200` provides the strict dApp capability/error foundation at sibling commit `b2d20f4385bfcb92454b4dec91f954a0babd13ac`, but deliberately leaves capability publication unavailable and implements no context operation.
- Every later review, signing, read-API, collateral, and CIP-103 path depends on exact full outputs. The existing wallet snapshot and primitive `TxOut` retain only reduced address/value data and cannot recover datum, inline datum, or reference scripts.
- The existing `getUTxOByTxIn` query is the smallest reusable ledger seam, but it acquires `VolatileTip`, runs only one query per acquisition, retries acquisition indefinitely, and exposes no exact-point transaction-context operation. Task-201 must make this seam point-bound and compose UTxO, era, and protocol-parameter queries under one acquisition.
- Conway is only conditionally ready at the fixture layer and Dijkstra remains `unsupported/readiness-blocked`. Implementing the Conway backend foundation now is required before task-202 can add ownership, required-proof, and earlier-batch analysis without inventing another context model.

## Interaction Mode

- Mode: `autonomous`.
- Required user inputs, manual steps, and user evidence: none. No product decision, configured wallet, live funds, physical device, upstream reviewer identity, or tracked-pin approval is needed.
- The implementer repairs or recreates the disposable sibling Nix/local-cluster environment and runs the focused real HTTP/mTLS proof. The stale missing-store path seen during task-200 is historical environment state, not a planned handoff.
- Only a concrete infrastructure failure still present after bounded remediation may stop execution. Record its exact sanitized command/error and keep the task `in_progress`; do not predeclare operator work, waive runtime proof, or relabel compilation as integration.
- Deterministic network/DB tests own races, point-pruning, retries, and generation interleavings. The real local cluster owns one narrow success plus one observable exact-point acquisition-failure proof; it does not manufacture timing-sensitive rollbacks.
- Tasks 200-208 remain local reviewable commits. `task-209` alone opens the consolidated upstream pull request, obtains authorized upstream review, activates the complete capability document, runs aggregate HTTP/mTLS validation, updates the Daedalus pin, and performs post-pin verification.

## Scope

- Add one additive Shelley wallet operation, `POST /v2/wallets/{walletId}/transaction-context`, for the accepted revision-1 backend context contract.
- Accept only expected network/genesis identity and ordered exact transaction CBOR. Decode every body and derive the complete normal, collateral, and reference input sets; revision 1 has no caller-supplied outpoint/role lists to omit or relabel. The request never supplies authoritative paths, ownership, reduced outputs, protocol summaries, or parent-output summaries.
- Capture wallet point `W`, wallet generation `G`, pending generation `P`, checkpoint outpoint membership, and exact pending transaction material in one wallet DB transaction; derive the live `availableUTxO` membership frozen below, acquire node local state exactly at `W`, then confirm unchanged `W/G/P` in a second wallet DB transaction.
- Retry the entire capture at most three attempts. Point acquisition failure, rollback/pruning, a relevant generation change, unavailable exact pending bytes, unsupported era, or retry exhaustion returns the already-frozen fixed dApp error without a partial response.
- Query the full ledger UTxO for every available-wallet outpoint and every transaction-derived foreign, script, normal, collateral, and reference input. The reduced wallet checkpoint is membership evidence only and is never serialized as a ledger output.
- Return canonical ledger CBOR for each `TxIn`, full `TxOut`, and CIP-30 `transaction_unspent_output` pair, preserving address, multi-assets, datum hash, inline datum, and reference script.
- Return the exact acquired chain point, a distinct deterministic volatile-delta projection, Conway era and protocol version, configured network identity, full era-specific protocol parameters, explicit node/pending provenance, and the frozen task-201 pending overlay needed by later context consumers.
- Implement revision-1 output/protocol/pending context records, canonical record ordering, true Blake2b-256 `context_digest`, and the stateless process-bound HMAC-SHA-256 `context_token` frozen by research 03.
- Add ledger-derived and cross-language fixtures for exact output forms, protocol-parameter bytes, context records, digest, token, and restart invalidation.
- Preserve the task-200 capability endpoint's exact ordinary `404`; task-201 must not advertise partial backend readiness.

## Non-Goals

- Do not implement authoritative derivation paths, credential ownership, required wallet witnesses, registration/governance analysis, earlier-request output derivation, or batch conflict graphs. `task-202`, `task-205`, and `task-206` own those additions to the same context model.
- Do not implement software signing, witness differencing, CIP-8/CIP-95 signing, DRep derivation, or durable write-ahead submission. Those remain tasks 203-208.
- Do not implement a second full-UTxO endpoint, serialize primitive wallet `TxOut`, add a reduced-output fallback, query a fresh tip after `W`, or accept renderer-calculated context.
- Do not promote Dijkstra, widen `ApiEra`, infer support from LSQ type presence, or claim product-era readiness. Non-Conway context requests fail closed before partial era conversion or serialization.
- Do not add a Daedalus runtime API client, Electron IPC, `source/main/cip30`, `source/main/cardano`, renderer, guest, frontend, hardware, collateral, or public CIP-30 method implementation. Those live seams do not yet exist and are not needed for this backend task.
- Do not activate `GET /v2/dapp-capabilities`, change `flake.nix` or `flake.lock`, or update the tracked backend pin.
- Do not add persisted context records, tokens, generation values, or a context cache. Process-lifetime generations and token secrets are memory-only.
- Do not add task-208 submission states or persistence. Revision 1 task-201 conservatively exposes exact current legacy `InSubmission` records as research state `outcome-unknown`; the live store cannot distinguish pre-broadcast, accepted, and uncertain paths. `InLedger`, `Expired`, and `Unknown` records are excluded; if one would be needed to resolve an outpoint in `Q`, the request fails closed. Every current `InSubmission` belongs to `T`, so malformed or byte-unavailable material also fails closed rather than being omitted from the overlay or its `0x07` records.
- Do not open a task-specific upstream pull request or require upstream approval. The complete tasks-200-through-208 range is reviewed upstream once in `task-209`.
- Do not edit either review log directly.

## Dependencies

- `task-200`: completed sibling foundation `b2d20f4385bfcb92454b4dec91f954a0babd13ac` on upstream parent `6761259ee91b138921231ce7fd1198679abfcc82`. Task-201 starts from that exact commit and adds a child commit without amendment.
- `task-003`: accepted exact-point `W/G/P`, provenance, digest/token, error/privacy, and evidence contract. Its concrete task-201 requirements remain normative.
- `task-002`: frozen CIP-30 `transaction_unspent_output`, error, exact-CBOR, and nullable read-method fixtures remain the public wire boundary. Task-201 produces backend context, not public CIP-30 JavaScript values.
- `task-004`: Conway fixture/inventory readiness is conditional; the full era-specific protocol parameters must come from node LSQ at `W`. Dijkstra remains blocked.
- Current Daedalus backend pin remains tag `v2026-07-23`, revision `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`; it has the same recent-era `getUTxOByTxIn` source but no dApp context API.
- Downstream `task-202` extends this response with ownership/proof and earlier-request provenance; `task-209` owns complete-range upstream review, activation, consumer validation, real aggregate HTTP/mTLS, migration/rollback review, and pinning.

## Research Consulted

- `research/03-cardano-wallet-backend-contract.md`: principal accepted source for the exact-point capture protocol, source precedence, context record encodings, digest/token construction, errors/privacy, evidence assignment, and consolidated delivery gate.
- `research/02-cip30-wire-contract-evidence.md`: freezes canonical minimal `TxIn`, `TxOut`, and `transaction_unspent_output` CBOR plus public errors; its minimal fixture is not full-output coverage.
- `research/04-exact-cbor-era-coverage.md`: freezes true Blake2b-256 evidence, exact-CBOR preservation constraints, Conway conditional readiness, Dijkstra refusal, and the exact-point protocol-parameter authority.
- `task-003.md` and `task-004.md`: checked to preserve downstream task ownership and avoid pulling semantic parsing, ownership, or protocol-rule evaluation into task-201.
- `task-200.md` and its implementation seams: checked to preserve the unavailable capability route, fixed dApp errors, strict type module, generated client/link structure, Swagger/golden conventions, and local-commit delivery sequence.
- Live pin-versus-sibling inspection confirmed `LocalStateQuery/UTxO.hs` is unchanged at the old pin, while current `NetworkLayer.getUTxOByTxIn` still queries recent-era ledger `UTxO`; no context, digest, token, exact-point command, or context route exists.

## Docs, Workflows, And Skills Consulted

- Read in the required order: `.agent/readme.md`, `.agent/system/architecture.md`, relevant PRD sections, the task tracker entry/dependency, accepted research 03/02/04, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`, and `.agent/workflows/update-doc.md`.
- Backend scope also required `.agent/system/api-endpoints.md`, `.agent/workflows/build.md`, `.agent/workflows/nix.md`, sibling `CONTRIBUTING.md`, sibling testing guidance, and sibling Swagger guidance.
- IPC guidance confirms task-201 adds no Electron channel and that transactions, outputs, addresses, tokens, digests, and arbitrary backend details must not enter logs. Existing authenticated Electron wrappers are not a backend HTTP template.
- Build/Nix guidance applies to temporary exact-candidate integration only. It does not authorize a tracked pin edit or substitute a compile for the required local-cluster proof.
- Sibling guidance requires `nix develop`, focused Cabal/unit/integration tests, OpenAPI validation, canonical formatting, and no drive-by reformatting.
- `ponytail` was loaded at full level. The plan reuses one existing wallet route family, one LSQ DSL, one wallet DB transaction seam, ledger serializers, task-200 types/errors, and existing test frameworks rather than adding a context service framework or cache.
- `understand` was loaded before repository exploration. Generating its graph would write `.understand-anything/**` and violate this planning turn's one-file write constraint, so important findings were verified directly against live Daedalus and sibling files.
- `cbor-encoding-decoding` was consulted for exact-versus-canonical CBOR, full-root consumption, and byte-preservation pitfalls. Golden diagnostics may use `cbor-diag`, but production serialization remains the pinned Haskell ledger serializer.
- `cardano-protocol-params` was consulted for the network-specific nature of protocol parameters and artifact preservation. No arbitrary live CLI fetch is authoritative here; the exact Conway value returned by LSQ at `W` is stored with the context fixture.

## Live Implementation Findings

- `NetworkLayer.getUTxOByTxIn` returns `MaybeInRecentEra Write.UTxO`; `LocalStateQuery.UTxO` supports Conway and Dijkstra and returns `InNonRecentEra` for older eras.
- `Network.Implementation._getUTxOByTxIn` sends a single LSQ through the rewards query queue. Empty requests use cached era state; nonempty requests do not expose the acquisition point.
- `localStateQuery` always sends `SendMsgAcquire VolatileTip`, retries acquisition without a bounded caller-visible failure, and releases after each command. Its `LSQ` monad can already combine UTxO, `currentEra`, and `protocolParams` queries under one acquired state once the command carries an exact target.
- `readWallet` atomically reads checkpoint, metadata/delegation, and pending transaction history, but there are no monotonic context generations and node LSQ is separate.
- `getWalletUtxoSnapshot` intentionally drops outpoints and returns token bundles. `Cardano.Wallet.DB.Store.UTxOHistory.TxOutCBOR` is a custom reduced address/value encoding and is not ledger `TxOut` CBOR.
- Live `availableUTxO` is exactly checkpoint UTxO minus pending normal and collateral inputs and never adds pending outputs; its unit properties require a checkpoint submap with every such input absent. Live `totalUTxO` is a different successful-application projection: it removes pending normal inputs, retains collateral-only inputs, and adds wallet-known pending change.
- Pending `TransactionInfo` can carry `txInfoCBOR`; entries without exact CBOR cannot be authoritative context material and must fail closed when needed.
- Task-200 added `Cardano.Wallet.Api.Types.Dapp`, fixed dApp error tags, a dormant complete capability constructor, client/link seams, Swagger conventions, and an unconditional `dappCapabilitiesUnavailable`. Task-201 extends these seams but does not change that handler.
- `source/main/cip30/**` does not exist. Existing Daedalus `source/main/cardano/**` contains node/wallet lifecycle launchers only, and `source/common/cip30/**` currently contains frozen contracts/fixtures only. No Daedalus production seam is required now.

## Fixed Contract Decisions

### API Boundary

- Add exactly one route: `POST /v2/wallets/{walletId}/transaction-context` under the existing Shelley `Wallets` group. Byron and shared/native-script wallets do not receive this route.
- Use strict task-200-style JSON objects with unknown-field rejection and one lowercase even-length hex wrapper. JSON integers are used only for `Word32`; `Word64` values are canonical decimal strings matching `^(0|[1-9][0-9]*)$` and bounded to `0..18446744073709551615`.
- Bind the route wallet ID and expected `network_id`, `network_magic`, and `genesis_hash`. An initial request identity mismatch, malformed value, incompatible derived role overlap, invalid CBOR, unsupported era, or unsupported request shape returns `400 dapp_invalid_request`; `409 dapp_account_changed` is reserved for wallet/network authority changing during an otherwise valid invocation.
- Ordered transaction bytes participate in the digest now. Task-201 ledger-decodes each complete Conway transaction and derives its normal body-key `0`, collateral body-key `13`, and reference body-key `18` inputs. Task-202, not task-201, derives earlier-request outputs, ownership, witnesses, or conflict graphs.

### Frozen Revision-1 HTTP Schema

The request shape is exactly the following; the short transaction value is illustrative, while committed goldens supply ledger-valid Conway bytes:

```json
{
  "revision": 1,
  "network": {
    "network_id": 0,
    "network_magic": 1,
    "genesis_hash": "0000000000000000000000000000000000000000000000000000000000000000"
  },
  "transactions": ["84a0a0f5f6"]
}
```

- `revision` is integer `1`. `network_id` is integer `0|1`; `network_magic` is integer `0..4294967295`; `genesis_hash` is exactly 64 lowercase hex characters.
- `transactions` has `1..50` entries, preserves caller order including duplicate items, and each entry is lowercase even-length hex decoding to `1..65536` bytes. Each value must be exactly one fully consumed Conway transaction accepted by the pinned ledger decoder. Dijkstra and all non-Conway values reject.
- The request contains no `inputs`, role arrays, wallet-UTxO selector, parent-output summaries, or extension object. The backend derives every role from every decoded body. An outpoint may have multiple roles across transactions; one transaction using the same outpoint as normal and collateral is `dapp_invalid_request`. Reference overlap is retained as another role.

The response is exactly the following closed object (illustrative values only):

```json
{
  "revision": 1,
  "wallet_id": "0000000000000000000000000000000000000000",
  "network": {
    "network_id": 0,
    "network_magic": 1,
    "genesis_hash": "0000000000000000000000000000000000000000000000000000000000000000"
  },
  "chain_point": {
    "kind": "block",
    "slot": "42",
    "block_hash": "0000000000000000000000000000000000000000000000000000000000000000"
  },
  "wallet_generation": "7",
  "pending_generation": "9",
  "era": "conway",
  "protocol_version": { "major": 9, "minor": 0 },
  "protocol_parameters_cbor": "a0",
  "volatile_delta": {
    "point": {
      "kind": "block",
      "slot": "42",
      "block_hash": "0000000000000000000000000000000000000000000000000000000000000000"
    },
    "node_transaction_inputs": []
  },
  "outputs": [],
  "pending_overlay": {
    "transactions": [],
    "spent_wallet_inputs": [],
    "produced_wallet_outputs": []
  },
  "records": [],
  "context_digest": "0000000000000000000000000000000000000000000000000000000000000000",
  "context_token": "010000000000000000000000000000000000000001000000283030303030303030303030303030303030303030303030303030303030303030303030303030303000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f5cf75cf8a98a29bfdd35dfd4471671df93ed091c67c1ec2b9ec5d44fde1fefa"
}
```

- `wallet_id` is the canonical 40-lowercase-hex route ID and `bytes(wallet_id)` in research 03 is its UTF-8 text. `chain_point` is exactly `{"kind":"genesis"}` or the shown block object; no nullable/mixed form exists. `wallet_generation` and `pending_generation` are bounded Word64 decimal strings.
- `era` is exactly `conway`. Protocol version components are Word32 JSON integers. `protocol_parameters_cbor` is nonempty lowercase even-length hex of the complete pinned Conway ledger serialization queried at `W`.
- Freeze these outpoint sets for revision 1: `C = dom(checkpoint UTxO)`; `N = union of normal inputs of the exact current legacy InSubmission set T`; `K = union of collateral inputs of T`; `A = C - (N union K) = dom(availableUTxO T checkpoint)`; `S = C - A = C intersection (N union K)`; `Q = union of normal, collateral, and reference inputs derived from the ordered request transactions`; and `O = A union Q`. These are mathematical sets, so `-` means set difference and every union and difference is duplicate-free. Task-201 deliberately preserves live `availableUTxO` semantics: pending-produced outputs are not added to `A`; that separate projection is `totalUTxO` behavior and is not a revision-1 overlay.
- `outputs` has exactly one entry for each outpoint in `O`. An entry has `wallet_member=true` and role `wallet_snapshot` exactly when its outpoint is in `A`; membership in `C`, `S`, or a pending transaction's outputs alone does not set either field. Entries sort by the complete encoded `0x01` record and have this closed shape:

```json
{
  "outpoint": { "transaction_id": "<64 lowercase hex>", "index": 0 },
  "transaction_input_cbor": "<canonical pinned-ledger CBOR>",
  "source_transaction_output_cbor": "<authoritative exact source bytes>",
  "canonical_transaction_output_cbor": "<canonical pinned-ledger CBOR>",
  "transaction_unspent_output_cbor": "<canonical [input, output] CBOR>",
  "provenance": ["pending", "node"],
  "roles": ["normal", "collateral", "reference", "wallet_snapshot"],
  "wallet_member": true,
  "pending_state": "outcome_unknown"
}
```

- `outpoint.index` is Word32. `provenance` is the nonempty subset of `pending,node` in that fixed order in task-201; task-202 may prepend `earlier`. `roles` is the nonempty subset of `normal,collateral,reference,wallet_snapshot` in that fixed order. `pending_state` is exactly `none|outcome_unknown`; it is `outcome_unknown` only when legacy pending is an output authority for that outpoint.
- `transaction_input_cbor` is canonical. `source_transaction_output_cbor` is the research-03 `exact_ledger_txout_cbor`: the original output span from an exact pending transaction or canonical pinned-ledger serialization from node state. `canonical_transaction_output_cbor` always reserializes the decoded ledger value canonically. The CIP-30 pair uses canonical input plus canonical output; it does not replace source bytes in record `0x01`.
- `volatile_delta` remains a distinct PRD field rather than redefining accepted research. Its `point` equals `chain_point`; `node_transaction_inputs` is the lexicographically sorted, duplicate-free list of `transaction_input_cbor` from output entries whose provenance includes `node`. It is a deterministic projection of digest-bound `0x01` records, not a second tip query or new record family.
- `pending_overlay.transactions` is sorted by transaction ID and contains closed entries `{transaction_id,state,transaction_cbor,normal_inputs,collateral_inputs,expiry_slot}`. `state` is exactly `outcome_unknown`; `transaction_cbor` is original exact sealed bytes; each input array is sorted, duplicate-free outpoint objects; `expiry_slot` is `null` or a Word64 decimal string derived from body `invalid_hereafter`, never the store's `maxBound` sentinel.
- `spent_wallet_inputs` is exactly `S`, sorted by outpoint and duplicate-free. `produced_wallet_outputs` is exactly the empty array for revision 1. Pending-produced outputs remain bound inside the exact `pending_overlay.transactions[*].transaction_cbor` and corresponding `0x07` records; they produce no `0x01` record and require no output recovery unless the same outpoint independently belongs to `Q`. This is an available-wallet overlay only, not `totalUTxO` and not ownership/path proof.
- `records` contains all and only task-201 record types `0x01`, `0x03`, and `0x07` as nonempty lowercase hex, sorted by complete bytes with no duplicates. Cardinality is `|O| + 1 + |pending_overlay.transactions|`: one `0x01` for each `outputs` entry, one protocol `0x03`, and one `0x07` for each emitted pending transaction. No `0x01` is synthesized for a pending-produced outpoint outside `O`. `context_digest` is exactly 64 lowercase hex. `context_token` is lowercase even-length hex whose decoded bytes exactly match the variable-length research-03 payload followed by a 32-byte MAC; parsing validates all internal lengths rather than accepting a fixed text length.

### Coherent Capture

- Add an exact-point LSQ command target while retaining `VolatileTip` as the unchanged default for existing callers. Existing network methods must not change behavior.
- An exact-point acquisition failure is returned to the caller; it must not silently retry at a newer point or loop indefinitely. The wallet-level capture owns the bounded three-attempt policy.
- Each attempt atomically reads `W/G/P`, `C`, and the exact current pending set `T`; derives `N/K/A/S/Q/O` by the frozen equations; performs one composite LSQ at `W` for era, full era-specific protocol parameters, and the members of `O` needing node resolution; then atomically confirms the same `W/G/P`.
- Implement a process-memory `ContextClockRegistry` in `DBFactory`, keyed by `WalletId`, with one shared `MVar` gate containing checked Word64 `G/P` per wallet. Every loaded/booted `DBLayer` for that wallet references the same clock, so worker/DB-layer reopen inside one backend process cannot reset it. Wallet deletion/replacement increments both counters under the gate and leaves the advanced tombstone entry in the process registry; improbable same-ID recreation reuses that advanced clock. The registry is never serialized and resets only with the backend process, together with process generation/key. Preserve the existential `stm` (`SqlPersistT IO` in production); do not call it STM or place an `IORef` update inside SQL.
- Decorate `DBLayer.atomically` so every existing DB action takes the same gate. Add the smallest tagged executor on that same layer for `NoContextChange|WalletContextChange|PendingContextChange|WalletAndPendingContextChange`; it takes the gate, runs the existing `atomically_` SQL transaction, and only after successful commit increments the selected checked counters before releasing the gate. SQL exception/rollback leaves counters unchanged. Counter overflow fails `dapp_internal_error` before accepting another mutation.
- Capture uses the same gate: under it, run one SQL transaction reading checkpoint `W`, wallet membership, exact current submissions, then copy `G/P`; release before LSQ. Confirmation takes the gate, reads current `W` in one SQL transaction and copies `G/P`. Equality is all three fields. Thus no capture can observe old-state/new-generation or new-state/old-generation. A process crash between SQL commit and memory increment destroys the process generation/key and clock; restart initializes `G=P=0`, and no old token authenticates.
- Tag these current central mutation boundaries, with no endpoint-side bumps: `restoreBlocks` (`WalletAndPendingContextChange`, because checkpoint/discovery/delegation and submission confirmation/expiry/pruning change together), `rollbackTo`/`rollbackBlocks` (`WalletAndPendingContextChange`), every successful `ReplacePrologue` address-discovery mutation (`WalletContextChange`), and current submission `addTxSubmission`, `resubmitTx`, `removePendingOrExpiredTx`, and standalone roll-forward/status changes (`PendingContextChange`). Metadata, rewards, passphrase/private-key storage, and read-only actions are `NoContextChange` because they cannot alter task-201 membership/output/pending evidence. Task-202 extends the tagged set for ownership/registration mutations it introduces; task-208 does the same for its new durable states.
- Tests instrument the raw SQL runner and gate to pause before commit, after commit/before increment, and before capture/confirmation. They prove both impossible mixed pairs, SQL rollback/no bump, successful exact single bump, mutate-then-revert ABA detection, rollback bumping both clocks, worker/DB-layer reopen retaining counters, delete/recreate advancing both, three-attempt exhaustion, process restart reset with token invalidation, and that direct production mutation call sites cannot bypass the tagged executor (source/API coverage plus DB-layer property tests).
- `volatile_delta` is the separate response projection frozen above. Do not change accepted research 03's record meanings, add a second tip query/block stream, or defer its representation to later documentation.

### Full Outputs And Provenance

- Source precedence implemented in this task is exact pending sealed transaction before node UTxO at `W`; task-202 adds the higher-priority earlier-request source. Equality is byte-for-byte equality of `source_transaction_output_cbor`, exactly as research 03 freezes it. Equal bytes retain all source bits; unequal bytes return `400 dapp_context_conflict` even when both decode to the same ledger value.
- Wallet checkpoint `TxOut` identifies membership in `C` only. Every outpoint in `A` must recover full bytes from pending exact CBOR or node UTxO at `W`; outpoints only in `S` need no output recovery. Every outpoint in `Q` still resolves regardless of membership; otherwise the complete request fails.
- Every transaction-derived normal, collateral, and reference outpoint is resolved even when foreign or script-controlled. Missing/spent inputs fail closed. Reference inputs receive role/provenance but no witness requirement; witness analysis remains task-202.
- Preserve an original pending output span before decoding. Separately serialize canonical Conway ledger `TxIn`, decoded full `TxOut`, and protocol parameters with `Cardano.Ledger.Binary.serialize' shelleyProtVer` and pinned ledger types. Build `transaction_unspent_output` from canonical input/output without primitive `TxOut` or JSON. A noncanonical pending-only fixture must expose different source/canonical bytes and a canonical pair; adding equal-semantic canonical node bytes must conflict, while identical source bytes combine provenance.
- Reject non-Conway acquired state before serialization. Dijkstra type availability is not implementation readiness.

### Pending Subset

- Task-201 reads the existing `TxSubmissions` store in the first DB transaction. Only `InSubmission` has exact current pending authority. Because live call paths can write it before or after broadcast and store no distinguishing evidence, map it conservatively to research state code `4` (`outcome-unknown`). Existing `InLedger`, `Expired`, and `Unknown` are not emitted as pending records; task-201 does not guess task-208's future `authorized`, `broadcasting`, `submitted`, or `rejected` distinctions.
- For every emitted pending transaction, decode the original sealed CBOR with full root consumption, verify the transaction ID from exact body bytes, derive normal/collateral/reference sets, output spans, and optional upper validity slot. Record `0x07` contains exactly the research-frozen normal and collateral vectors and expiry; reference inputs remain derivable roles on `0x01` and are not added to the frozen record body.
- The available-wallet overlay is exactly `A = C - (N union K)`, matching live `availableUTxO`: normal and collateral inputs of `T` are removed, and no pending-produced output is added. The response exposes that delta as `spent_wallet_inputs=S` and `produced_wallet_outputs=[]`. Reduced checkpoint outputs establish `C` membership only; exact bytes are required only for `O=A union Q`.
- If an `InSubmission` record lacks exact sealed bytes, fails ID/body/input/expiry decoding, or prevents deriving `N/K/A/S` or resolving an outpoint in `O`, return `503 dapp_context_unavailable`. Unsupported future submission states fail `503` when they affect requested or wallet availability; otherwise they are omitted rather than guessed.

### Binding And Token

- Implement context record `0x01` for full outputs, `0x03` for protocol context, and `0x07` for available existing pending material exactly as research 03 specifies. Ownership/registration/governance/required-proof records remain absent until their owning tasks add real evidence.
- Sort records by complete encoded bytes, reject duplicate records, and compute true Blake2b-256 over the exact domain-separated, fixed-width, length-prefixed preimage. Never hash JSON or rendered hex.
- Generate one random 32-byte HMAC key and random 16-byte process generation at backend startup. Inject deterministic values only in tests. Return lowercase hex of the exact payload plus HMAC-SHA-256 and compare MACs in constant time.
- Store no context server-side. The token has no backend wall-clock expiry. Process restart/key loss, malformed token, capability/wallet/network mismatch, or digest mismatch invalidates it; chain movement after a successful capture does not.
- Logs and errors contain only fixed categories and bounded counts/timing already allowed by policy. They never contain exact request/response bytes, outpoints, addresses, assets, protocol parameters, pending records, digests, tokens, keys, or database text.

### Exhaustive Route Error Boundary

- Put one route-scoped Servant/WAI normalization boundary around the exact transaction-context path so errors raised before, inside, or after `withWorkerCtx` cannot escape as ordinary cardano-wallet bodies. Reuse task-200's fixed dApp error constructor; do not create another public error format. Responses already carrying a valid frozen dApp pair pass through unchanged.
- The exact revision-1 mapping is:

| Source                                                                                                                                                                                                                                                                                                                                    | HTTP/tag/fixed `message`                                    |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------- |
| Wrong method, unmatched transaction-context suffix, unsupported/missing `Content-Type`, missing/empty body, malformed JSON, unknown/duplicate JSON field, invalid wallet capture text, invalid hex/length/cardinality, malformed/non-Conway transaction, derived incompatible normal/collateral role, configured/request network mismatch | `400 dapp_invalid_request / Invalid backend request`        |
| Pending/node source bytes differ                                                                                                                                                                                                                                                                                                          | `400 dapp_context_conflict / Backend context conflict`      |
| Wallet absent before worker acquisition, deleted/replaced while acquiring, worker wallet identity mismatch, configured network/genesis changes during a valid invocation                                                                                                                                                                  | `409 dapp_account_changed / Wallet or network changed`      |
| Worker exists but is not responding/has stopped during capture, exact-point acquire failure, point pruned/rolled back, unavailable exact pending or wallet output, moving `W/G/P`, retry exhaustion, LSQ disconnect/cancellation before result                                                                                            | `503 dapp_context_unavailable / Wallet context unavailable` |
| DB/serialization/crypto failure, counter overflow, invariant violation, uncaught synchronous exception, or any otherwise unmapped application exception                                                                                                                                                                                   | `500 dapp_internal_error / Backend operation failed`        |

- TLS handshake failure, client disconnect before WAI receives a complete request, client disconnect after receipt but before response delivery, and server-process termination cannot produce an HTTP body; the pre-receipt cases perform no operation, while post-receipt disconnect may cancel or complete server work without a deliverable body. These are the only transport cases outside the table. Once WAI identifies the exact route prefix and a response remains deliverable, no generic Servant 400/404/405/415, `no_such_wallet`, worker error, exception text, HTML, or empty body may escape.
- The normalizer matches only `/v2/wallets/<single-segment>/transaction-context[/...]`; unrelated routes, including task-200 capability `404`, remain byte-for-byte unchanged. Tests cover the exact route, wrong method, suffix, malformed body/content type, missing/deleted wallet before and during `withWorkerCtx`, stopped worker, each typed domain failure, and an injected exception.
- Every body is exactly task-200's JSON error shape with the fixed tag/message and no extra fields. Capture actual API tracer and transaction-log output with sensitive sentinel values in route/body, wallet ID, transaction, output, protocol bytes, token/digest, SQL exception, and TLS path; assert none appears in body, traces, metrics, or logs. Only static category, status, and bounded timing/count metadata are allowed.

## Expected Files

### Sibling `../cardano-wallet`

- `lib/network-layer/src/Cardano/Wallet/Network/Implementation/Ouroboros.hs`: make LSQ commands carry a target and report exact-point acquisition failure while preserving existing volatile-tip callers.
- `lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery/UTxO.hs` and, if separation is clearer, one focused `.../TransactionContext.hs`: compose current era, exact full UTxO, and era-specific protocol parameters under one acquisition.
- `lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery.hs`, `.../Network.hs`, and `.../Implementation.hs`: expose the smallest typed exact-point context query and map acquisition failures without altering existing methods.
- `lib/wallet/src/Cardano/Wallet.hs` plus narrow `Cardano/Wallet/DB.hs` and `DB/Layer.hs` changes for the process `ContextClockRegistry`, per-wallet gated/tagged DB execution, coherent reads, deletion/reopen handling, and enumerated current mutation call sites. Existing wallet/submission models are read, not extended; no schema or migration module is expected.
- `lib/api/src/Cardano/Wallet/Api/Types/Dapp.hs` or one focused adjacent context module, plus `Types.hs`: strict request/response, role/provenance, full-output, protocol, digest, and token types.
- `lib/api/src/Cardano/Wallet/Api.hs`, `Api/Http/Server.hs`, and `Api/Http/Shelley/Server.hs`: additive wallet route and handler using `withWorkerCtx`, plus the exact-path error normalizer; preserve unrelated routes and the unavailable capability handler.
- `lib/api/src/Cardano/Wallet/Api/Client.hs`, `Api/Clients/**`, and `Api/Link.hs`: generated typed context client/link where required by compile and integration tests.
- `lib/application/shelley/Cardano/Wallet/Application.hs` and a focused process-secret module if needed: create and inject process generation/HMAC key once without logging or persistence.
- `lib/api/cardano-wallet-api.cabal`, `lib/network-layer/cardano-wallet-network.cabal`, `lib/unit/cardano-wallet-unit.cabal`, and integration component manifests only as needed for focused modules/tests.
- `specifications/api/swagger.yaml`: exact additive route, closed request/response schemas, Conway-only availability, fixed errors, and no capability-activation claim.
- Focused network/wallet/API unit specs and goldens, including a new LSQ exact-point spec if no suitable module exists; a focused Conway local-cluster API scenario registered in the existing integration suite.

### Daedalus Evidence And Records

- `source/common/cardano/fixtures/exact-cbor/backend-context-v1.json` (exact name may follow the existing fixture naming): ledger-derived full-output/protocol/context/digest/token vectors with source revisions and hashes.
- A focused Jest fixture check under `source/common/cardano/` that independently reproduces only the committed fixture bytes/digest/MAC. Task-304 still owns the production independent context encoder/validator and exhaustive mutation coverage.
- `research/03-cardano-wallet-backend-contract.md`: before sibling implementation, add the exact revision-1 HTTP schema, derived-role rule, task-201 pending subset, original/canonical output rule, distinct volatile-delta projection, and concrete `ContextClock`/error boundary without changing existing record bytes. After verification, add candidate evidence, local-cluster result, no-schema proof, and residual gates.
- `research/04-exact-cbor-era-coverage.md`: link the exact Conway output/protocol fixture without changing Dijkstra or product-readiness status.
- `dapp-browser-cip30-prd.md` and `dapp-browser-cip30-tasks.json`: after candidate verification but before final internal review, record the exact local candidate and truthful task status while preserving task-209 upstream PR, activation, aggregate integration, migration review, and pin ownership.
- This canonical plan: before final internal review, record candidate/base, complete verification evidence, documentation revision, lifecycle status, and proposed outcome; after approval, only record the final review decision/outcome.
- No Daedalus product source, Electron IPC manifest, package lock, Nix pin, system API-endpoint documentation, or review-log content is an implementation requirement. `.agent/system/api-endpoints.md` is updated by task-209 when the complete backend is activated and consumed.

Expected paths may narrow to existing modules during implementation. A second context API, persisted context state, broad DB refactor, Daedalus runtime client, or tracked pin change requires plan review.

## Smallest Implementation Approach

1. Synchronize the frozen contract, then reconfirm immutable baselines.
   - Apply this plan's schema, derived-role, pending-subset, byte-authority, volatile-delta, `ContextClock`, and error decisions to research 03 before sibling code. Keep its record encodings unchanged.
   - Require sibling branch `amw/cip30` at task-200 commit `b2d20f4385bfcb92454b4dec91f954a0babd13ac`; leave untracked `.idea/` and concurrent changes untouched.
   - Compare old pin and candidate versions of exact target files. Record that the UTxO query is unchanged while API/network/wallet drift is reviewed where relevant.
   - Create no tracked Daedalus pin change and no sibling history rewrite.
2. Extend the existing LSQ command rather than adding another node client.
   - Parameterize a command with `VolatileTip` or exact `Point`; retain the current behavior for every existing caller.
   - Make exact acquisition failure observable. Compose era, full UTxO, and full era-specific parameters in one `LSQ` action so release happens only after the complete query.
   - Add focused state-machine tests for exact target, rollback/pruning failure, one acquisition for all subqueries, and unchanged volatile-tip behavior.
3. Add the process-memory DB-layer gate and coherent snapshot seam.
   - Create one process `ContextClockRegistry`; inject the same wallet clock into every loaded/booted `DBLayer`, make existing `atomically` use its gate, and add the tagged executor with commit-then-increment-under-gate ordering. Cover worker reopen and wallet deletion/replacement. Do not alter the existential SQL action or persist generations.
   - Tag only the enumerated checkpoint/discovery/current-submission boundaries. Read `W/G/P`, `C`, and exact `InSubmission` material under the gate; derive the frozen `N/K/A/S/Q/O` sets, query members of `O` at `W`, confirm `W/G/P`, discard on mismatch, and cap at three complete attempts.
   - Add deterministic pause/failure injection at DB-layer and LSQ test seams, not production configuration. Recover a pending output from its original transaction span only when its outpoint is independently in `Q`; reduced checkpoint values establish `C` membership only.
4. Encode exact ledger context once.
   - Fully decode ordered transactions, derive all three input roles and `Q`, form `O=A union Q`, query the node set, and enforce exact-source byte equality/precedence.
   - Preserve pending original output spans and separately emit canonical `TxIn`, canonical full `TxOut`, canonical pair, protocol bytes, frozen pending/volatile projections, record bytes, sorted record vector, digest, and token from one typed result.
   - Keep token construction pure and injectable for deterministic tests; production creates secrets once at startup.
5. Add one additive HTTP operation through task-200 seams.
   - Add the exact closed types, route, handler, client/link, Swagger, and exact-path error normalizer using task-200 fixed errors.
   - Reject Byron/shared/non-Conway, network mismatch, malformed bytes, unavailable exact material, context conflict, and exhausted capture before returning any partial response.
   - Leave `dappCapabilitiesUnavailable` byte-for-byte behavior unchanged.
6. Produce focused evidence.
   - Cover schema cardinalities, derived-role completeness, ADA/multi-asset outputs, datum hash, inline datum, native/Plutus reference scripts, wallet/foreign/script inputs, exact pending state/expiry/overlay, source equality/conflict, noncanonical source/canonical output, missing output, unsupported era, and deterministic ordering. An executable fixture with nonempty `C`, pending normal input `n`, collateral input `k`, and wallet-known pending output `p` must assert `A=C-{n,k}`, `S={n,k}`, `produced_wallet_outputs=[]`, `O=A union Q`, no `0x01` for `p` when `p` is outside `Q`, and exactly one additional `0x01` with pending provenance when a second case places `p` in `Q`.
   - Cover gate ordering at every pause point, first/second-attempt recovery, three-attempt exhaustion, point pruning, mutate/revert ABA, rollback changing both clocks, SQL rollback/no bump, restart token invalidation, exhaustive route errors, and captured trace/log redaction.
   - Generate pin-versus-sibling and ledger-derived golden bytes. Independently reproduce fixture digest/MAC in Daedalus without implementing task-304 production code.
7. Create and validate the cross-repository candidate.
   - Self-review the sibling diff, run focused/full relevant checks, and create one task-201 child commit. Review fixes are follow-up commits, never amendments.
   - Repair/recreate the sibling Nix shell autonomously and run one focused Conway real HTTP/mTLS scenario proving exact-point success and one controlled exact-point acquisition failure. If bounded remediation still fails, record a concrete infrastructure blocker and stop in progress without a preplanned human handoff.
   - Build Daedalus bridge/mainnet against the exact candidate through a temporary override and verify the embedded candidate revision without editing `flake.nix` or `flake.lock`.
8. Freeze one final reviewable state before review.
   - After all candidate/follow-up commits and tests, update fixtures, research, PRD, tracker, and this plan truthfully with the exact sibling range, Daedalus evidence revision, complete verification, and proposed outcome. Do not mark completion or activation.
   - Re-run affected schema/golden/docs checks. Any later review fix is a follow-up commit and requires refreshed evidence/docs plus affected reruns.
9. Complete exact internal review.
   - `@Reviewer` examines the task-200 base, complete task-201 candidate/follow-up range, complete Daedalus evidence/docs diff, no-schema/old-pin proof, deterministic checks, real HTTP/mTLS proof, and proposed canonical outcome together.
   - After approval, change only final review/lifecycle/outcome fields. Keep capability publication unavailable and the tracked pin unchanged. Task-209 alone owns the consolidated upstream PR/review, aggregate activation/integration, migration review, exact pin, and post-pin rerun.

## Acceptance Criteria

- The exact closed request/response schema, cardinalities, decimal/hex/point representations, Servant route, Haskell types, client/link seams, Swagger, handler, and goldens agree. Unknown fields and caller-supplied input-role lists reject; existing endpoints remain behavior-compatible.
- Every normal/collateral/reference set is derived from every ordered transaction body. The implementation computes exactly `A=C-(N union K)`, `S=C-A`, `Q`, and `O=A union Q`; `spent_wallet_inputs=S`, `produced_wallet_outputs=[]`, and output/`0x01` cardinality is exactly `|O|`. Every outpoint in `O` resolves to canonical ledger `TxIn`, canonical full `TxOut`, canonical `transaction_unspent_output`, and separately preserved authoritative source bytes from pending CBOR or node LSQ at `W`; reduced wallet `TxOut` is membership-only.
- Address, ADA/multi-assets, datum hash, inline datum, reference-script, and full protocol-parameter forms match pinned ledger serialization and cross-language goldens. A noncanonical pending fixture proves original-source preservation, canonical response pairing, exact byte-conflict behavior, and no semantic-equality substitution.
- One attempt reads `W/G/P`, performs one composite LSQ exactly at `W`, and confirms unchanged `W/G/P`. Any mismatch discards all data; no more than three attempts occur; acquisition/rollback/pruning/exhaustion returns fixed `503 dapp_context_unavailable` with no partial response.
- The live `SqlPersistT IO` transaction and memory clock are serialized by one process-registry per-wallet gate shared across DB-layer reopen. Every enumerated mutation commits before exactly the tagged checked increment while still gated; SQL rollback does not bump; no mixed state/generation pair passes; restart resets are safe because the process generation/key also reset. Task-202/task-208 own only their new mutation tags/states.
- The response binds the exact chain point, configured network/genesis, Conway era/protocol version, full exact-point protocol parameters, distinct deterministic volatile delta, frozen legacy `outcome-unknown` pending subset/expiry, the live-`availableUTxO` equation and empty produced-output projection, wallet membership, derived roles, and ordered transaction bytes.
- Full-output/protocol/pending records, canonical sorting, true Blake2b-256 digest, token payload, and HMAC-SHA-256 exactly match research 03 and cross-language fixtures. Token MAC comparison is constant-time; secrets/tokens are memory-only and never logged.
- Equal authoritative source bytes combine provenance; unequal bytes return fixed `400 dapp_context_conflict`, including equal-semantic/different-encoding values. Missing/spent/unrecoverable outputs, malformed requests, unsupported pending states that affect context, incompatible derived roles, and unsupported eras fail closed without reduced fallback.
- Dijkstra and every non-Conway era remain unavailable; task-201 does not widen public era types or product-readiness claims.
- The task-200 capability endpoint remains the exact ordinary `404`, with no activation input. The tracked Daedalus pin remains unchanged and the task-201 candidate is explicitly non-pin-eligible alone.
- Deterministic unit/property/golden/API tests pass, and one focused real Conway HTTP/mTLS scenario proves exact-point success and controlled acquisition failure against the exact candidate. Compilation alone is not runtime evidence; a concrete unremediable environment failure leaves the task in progress.
- Every recognized transaction-context route failure maps to the fixed dApp status/tag/message table. Actual HTTP bodies, API traces, metrics, transaction logs, review records, and normalized evidence contain no transaction/output/address/asset/protocol-parameter/token/digest/key/path/TLS/database payload.
- No persisted state, model field, SQL table/column/index, schema version, migration, or generated migration change is introduced. A byte-copied pinned-schema fixture opened and mutated by the candidate remains at the identical schema/version and is reopened by old pin `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`; rollback removes the route and restart invalidates memory tokens without restore work. Any need to persist a generation, pending representation, token, or context stops implementation and forces a revised versioned migration/backup/restore/old-pin plan.
- The exact candidate and complete evidence have no unresolved internal `@Reviewer` condition. Consolidated authorized upstream review, aggregate activation, successful full-range HTTP/mTLS, Daedalus consumer checks, and pinning remain mandatory in task-209.

## Verification Plan

### Agent-Executable Sibling Checks

- Run focused network-layer tests for target selection, exact-point acquisition/failure, composite-query single acquisition, and unchanged volatile-tip behavior using `nix develop` with `cabal test cardano-wallet-network-test --test-options='<match>'` or the live equivalent.
- Run focused unit/property matches for closed schema/cardinalities, transaction-derived roles, original/canonical serialization, the exact `C/N/K/A/S/Q/O` equations and empty `produced_wallet_outputs`, pending subset/expiry, protocol/volatile fields, source provenance/conflicts, every gated `W/G/P` interleaving and mutation tag, digest/token/restart, exact route errors, and captured tracer/log redaction via the live `just unit-tests-cabal-match '<match>'` or exact Cabal component commands.
- Build `cardano-wallet-network`, `cardano-wallet`, `cardano-wallet-api`, `cardano-wallet-application`, unit, and integration components with `-Werror` using repository-supported commands.
- Validate `specifications/api/swagger.yaml` with `openapi-spec-validator --schema 3.0.0` and `scripts/ci/validate-swagger.sh` where its pinned environment is available.
- Run `just check-fmt`, focused `hlint`, `cabal check` for touched packages, and `git diff --check`; inspect formatting diffs to reject drive-by changes.
- Run relevant existing network, wallet transaction, API routing, capability, error, and Swagger regression tests.

### Exact Fixture Checks

- Decode every committed TxIn/TxOut/pair/protocol/record vector with `cbor-diag --from hex --to diag` and verify full consumption and the expected datum/reference-script structures.
- Regenerate sibling goldens from pinned ledger serializers and compare byte-for-byte, including the executable `availableUTxO` equation fixture, pending-produced exclusion and `Q`-driven inclusion cases, pending-only noncanonical source/canonical pair, and equal-semantic pending/node conflict vectors.
- Run the focused Daedalus Jest fixture check to independently reproduce true Blake2b-256 context digest and HMAC token bytes; run the existing task-002 contract and task-004 exact-CBOR Jest checks unchanged.
- Parse every changed JSON file, run direct focused Prettier for task-scoped Daedalus files, and run `git diff --check` in both repositories.

### Required Local-Cluster Validation

- Run the registered focused Conway scenario title from the exact candidate's integration suite through the sibling-supported Nix command; do not freeze an illustrative match string before registration.
- Through real cardano-wallet HTTP/mTLS and local node state, prove one successful request containing wallet plus foreign/script normal/collateral/reference inputs and full datum/inline-datum/reference-script context, then one controlled exact-point acquisition failure returning the fixed redacted `503`. Deterministic tests, not the cluster, prove pruning, generation races, retry counts, and exhaustion.
- Record candidate hash, command/environment identity, normalized result, and fixture hashes. A missing environment or skipped/pending scenario is not a pass.
- First repair/recreate stale flake inputs/store paths or use a clean disposable checkout without changing product pins. If bounded attempts still hit a concrete external infrastructure failure, record it and stop with build `in_progress`; no automatic operator handoff is part of the approved plan.

### Candidate, Rollback, And Combined Review

- Confirm the task-201 candidate is a non-amended descendant of `b2d20f4385bfcb92454b4dec91f954a0babd13ac`; inspect the full task-200-through-candidate range and preserve unrelated `.idea/` content.
- Inspect all DB/store/schema/migration paths and compare schema dump/version before and after candidate mutation of a byte-copied pinned-schema fixture. Reopen that exact DB with old pin `724be55dc66cf67bc4427e8f1a9657a9d1d33d71` and verify wallet/checkpoint/submission reads. Prove clocks/secrets/tokens/context are process-memory only and no migration exists. Any persisted field or schema delta stops implementation and forces replanning for a versioned atomic migration, backup/restore rehearsal, and old-pin open-or-restore gate.
- Without a tracked pin edit, build `.#daedalus-bridge-mainnet` and `.#daedalus-mainnet` against the exact candidate through a temporary override and verify the bundled source revision.
- `@Reviewer` examines the already-final candidate range and Daedalus evidence/docs state: schema/derived roles, exact-point authority, gated counters, original/canonical bytes, pending/volatile/protocol context, provenance/conflicts, digest/token, exhaustive errors/privacy, deterministic plus real HTTP/mTLS evidence, old-pin reopen proof, additive compatibility, unchanged capability route/pin, and task-202/task-209 boundaries.

## Risks And Open Questions

- Exact-point LSQ currently retries `VolatileTip` acquisition indefinitely. Extending the command incorrectly could change every network query. Mitigation: preserve volatile-tip defaults and tests; make only the new exact-target path return acquisition failure.
- `W/G/P` counters can miss an ABA race if updates bypass the gate. Mitigation: all `DBLayer.atomically` actions share one process gate, relevant central SQL actions use the closed mutation tags, source coverage finds bypasses, and pause-point/ABA tests prove commit/increment ordering.
- Existing pending state has only `InSubmission|InLedger|Expired|Unknown`, not task-208's richer evidence. Mitigation: conservatively emit exact `InSubmission -> outcome-unknown`, derive bytes/inputs/expiry from sealed CBOR, preserve live `availableUTxO` rather than inventing a produced-output overlay, and fail closed when unsupported/unavailable material affects context.
- Full wallet UTxO queries may be large. Correctness and no policy item cap are required; batch the node query only if the local-state protocol has a demonstrated limit, while keeping one acquired state and one atomic response. Do not add caching or pagination to the backend context.
- Pending output encoding may be noncanonical. Mitigation: preserve original source bytes in record `0x01`, separately return canonical output/pair, and apply research-03 byte equality rather than semantic equality. Task-302 still owns Daedalus's production input-span parser.
- Protocol parameters are era/network/point specific. Cached `currentPParams`, CLI-fetched JSON, or renderer summaries are invalid authorities. Query and serialize the complete Conway value under the same LSQ acquisition at `W`.
- The accepted research lists earlier-request provenance above pending/node, but task-202 owns its derivation. Task-201 must not create caller-authoritative parent summaries; capability publication stays unavailable until the complete range lands.
- The PRD requires a distinct `volatile_delta`, while research 03 has no new record type. The frozen projection above keeps it separate and deterministically derivable from digest-bound node-provenance records without changing accepted record bytes or querying another point.
- Context operation success becomes reachable to mutually authenticated local clients before capability activation. It remains unadvertised and unused by Daedalus; task-209 must still verify the complete API range and activate atomically.
- The historical local-cluster Nix source failure may recur. Autonomous bounded remediation is required; only a still-live external failure may pause the task, with no runtime claim and no user dependency assumed in advance.

## Docs, Tracking, And Research Updates

- Before sibling code, update research 03 with the exact schema/representations, derived roles, pending subset, source/canonical rule, distinct volatile projection, concrete gated `W/G/P`, and error boundary. After candidate verification but before final review, add exact candidate/base, fixture hashes, HTTP/mTLS evidence, privacy result, no-schema/old-pin proof, and task-202/task-208/task-209 residual work.
- Update `research/04-exact-cbor-era-coverage.md` with the ledger-derived Conway full-output/protocol fixture link and exact source identities; keep Dijkstra `unsupported/readiness-blocked` and product support false.
- Before final review, update the PRD only with truthful implementation state and the exact pending/volatile context; do not claim complete semantic review or production readiness. Update the tracker with proposed status, exact local candidate, automated/runtime evidence, no-schema rollback, unchanged capability response/pin, and task-209 ownership.
- Before final review, update this plan with candidate, verification, documentation state, lifecycle status, and proposed outcome. After approval, add only final review/outcome. Reviewer must see the complete state it approves. If no additional durable finding exists, record `no new research` rather than creating another note.
- Do not update `.agent/system/api-endpoints.md`, architecture, IPC/build/Nix workflows, or public CIP-30 contract fixtures merely because the dormant backend route exists. Task-209 updates shipped API documentation when the complete reviewed backend is activated and consumed.

## Review Logs

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-201-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-201-impl-review.md`
- Both logs are Orchestrator-owned append-only records. Planner, Critiquer, and Reviewer return proposed transcript blocks and never write these files directly.

## Lifecycle Status

- Planning status: `approved`
- Build status: `completed`
- Candidate ranges: `cardano-ledger-read` `54f91a71874c50c14260de4eab6b2c2123e7d391..44e0a32300ec6d6d03b7578b97b8374820802ba1`; `cardano-wallet` `b2d20f4385bfcb92454b4dec91f954a0babd13ac..3ca15553f96587f1f96688185165b2ede00e30b0`.
- Internal review: Iteration 4 approved the isolated bounded exact-point implementation; a focused later review approved the optional mTLS framework change. Iteration 5 requested executable resolver-equation coverage, pending-record digest coverage, and live full-output fidelity; Iteration 6 closed those gaps; Iteration 7 approved the final corrected fixture provenance with no findings.
- Verified locally: 51 decoder examples, 4 context encoder/token examples, 10 logging examples, 10 network-layer examples, 5 DBFactory lifecycle examples, 2 capture/query retry-exhaustion examples, 18 focused route/Swagger examples, 8 TLS examples, and 57 Daedalus fixture/contract examples passed. Fresh public-pin Cabal builds, plain HTTP and mTLS local-cluster success plus controlled fixed-503 acquisition failure, old-pin checkpoint/submission reopen with identical schema hash, and temporary exact-candidate Daedalus bridge/mainnet Nix builds passed.
- Current outcome: `completed`. Candidate `3ca15553f96587f1f96688185165b2ede00e30b0` and all task-201 acceptance evidence passed final combined review. The published decoder is pinned from `https://github.com/riverArk/cardano-ledger-read` at `44e0a32300ec6d6d03b7578b97b8374820802ba1`, with upstream PR `https://github.com/cardano-foundation/cardano-ledger-read/pull/20`. Capability publication and the tracked Daedalus pin remain unchanged; task-209 retains consolidated upstream review, activation, aggregate integration, migration review, pinning, and post-pin verification.

## Planner Self-Review

- Scope creep: the plan adds one context route, exact-point extension to the existing LSQ client, one coherent wallet snapshot bracket, ledger serialization, stateless binding, and focused evidence. It excludes ownership/earlier-batch analysis, signing, CIP-8/CIP-95, durable submission, Daedalus clients, IPC/UI/hardware, activation, and pinning.
- Missing tests/docs: schema cardinalities, derived roles, gate/commit interleavings, pending/volatile forms, noncanonical source/canonical bytes, protocol bytes, provenance/conflict, digest/token/restart, exhaustive route errors/actual traces, focused real HTTP/mTLS, old-pin reopen, Swagger, and final-state governing-document review are assigned.
- Trust drift: node LSQ at `W` and exact pending bytes remain output authorities; wallet checkpoint data is membership-only; network/genesis is backend-configured; renderer paths/summaries and fresh-tip queries are rejected.
- Wire drift: research-03 source bytes and record tags/fields/order remain unchanged; canonical CIP-30 bytes are separate; true Blake2b-256, HMAC payload, fixed errors, Conway-only status, and task-200 unavailable capability behavior remain unchanged. Task-304 retains production independent validation.
- Pending-overlay consistency: live sibling source and properties establish `availableUTxO T checkpoint = C - (N union K)` and reserve pending change for `totalUTxO`; every response, recovery, record-cardinality, acceptance, and fixture requirement now uses `A/S/Q/O` from that one equation, with `produced_wallet_outputs=[]`.
- Migration/rollback: clocks/tokens are memory-only and the plan requires schema/version comparison plus old-pin reopen after mutation. Any persisted state forces replanning; task-201 never updates the tracked pin, and task-209 still reviews full-range migration evidence.
- Interaction consistency: execution is autonomous. A historical ephemeral Nix failure is not treated as a human checkpoint; a concrete unremediable infrastructure failure truthfully pauses work in progress without waiving runtime proof.
- Delivery consistency: task-201 creates a reviewed local child commit only. Task-209 retains the single consolidated upstream review, complete capability activation, aggregate HTTP/mTLS, consumer identity checks, migration/rollback review, exact pin update, and post-pin rerun.
