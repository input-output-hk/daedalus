# Task task-003: Validate the cardano-wallet backend contract

## Task

- Task ID: `task-003`
- Title: `Validate the cardano-wallet backend contract`
- Phase: `phase-0` (`Contracts, Threat Model, And Validation Spikes`)
- Priority: `critical`

## Why This Task Now

- `task-001` has fixed the backend trust boundary and `task-002` has frozen the public CIP-30 wire behavior that the backend must support.
- Phase-2 backend tasks `task-200` through `task-209` must not start from ambiguous assumptions about node/wallet consistency, lossy UTxO state, signing reuse, pending submission, database compatibility, or upstream acceptance.
- Live evidence confirms material gaps between the pinned `cardano-wallet` API and the frozen connector contract. This task should turn those gaps into one reviewed delivery contract, not implement the phase-2 backend.
- The selector reported stale `task-002` tracking, but the live tracker now truthfully marks `task-002` completed with completion metadata. This task must preserve that synchronization and verify it in the focused tracker diff rather than reopening or duplicating task-002.

## Interaction Mode

- Mode: `manual_execution`.
- Required user/operator input was requested after agent-executable drafting. On
  2026-08-11 the user directed the Orchestrator to assume cardano-wallet
  implementation signoff and proceed.
- The user did not supply owner/reviewer identities or a durable URL. The task
  records those fields as unavailable instead of fabricating external evidence.
- The assumption accepts the complete task-003 contract and evidence matrix for
  this validation gate. It does not satisfy or weaken the concrete candidate
  commit, owner, authorized upstream review, migration/rollback, Daedalus
  integration, or exact pin evidence required from tasks 200-209.
- The manual checkpoint is satisfied for task-003 continuation. Implementation
  review and final synchronization remain required before completion.
- No wallet funds, secrets, configured network, physical hardware device, packaged Electron build, or external security audit is required for this phase-0 task.

## Scope

- Validate the pinned Daedalus backend and the live sibling checkout against every backend capability required by the PRD and frozen task-002 contract.
- Produce one canonical supporting research note containing the exact proposed cardano-wallet request, response, error, version/capability, consistency, lifecycle, privacy, and compatibility contract.
- Freeze a coherent transaction-context model covering full requested inputs, the wallet UTxO set, wallet ownership, pending submissions, and earlier outputs in the same ordered batch without trusting renderer-supplied paths or reduced `TxOut` summaries.
- Decide the smallest backend signing seam by evaluating the existing wallet-scoped `transactions-sign` endpoint plus Daedalus-side witness-set diffing first. Any new signing endpoint or persisted context mechanism requires written necessity evidence.
- Define the CIP-8/CIP-95 backend responsibilities needed by later tasks, including credential ownership, payment/stake/DRep key selection, registration classification, and fail-closed error boundaries, without implementing key derivation or signing.
- Define wallet-scoped write-ahead submission semantics that persist recoverable pending state before broadcast, support exact idempotent replay, and remain the sole durable recovery record.
- Define additive API capability negotiation and old-backend fail-closed behavior.
- Record upstream ownership, review, acceptance path, migration/rollback expectations, and future Daedalus pin sequencing.
- Mandatorily reconcile the PRD and task-003 tracker definition, not only its eventual status, so description, target paths, implementation notes, acceptance criteria, and completion notes describe a phase-0 validation artifact rather than an unproduced phase-2 sibling commit or pin.

## Non-Goals

- Do not implement or prototype cardano-wallet API handlers, Haskell types, ledger queries, signing, DRep derivation, submission persistence, database migrations, or tests. Those changes belong to `task-200` through `task-208`.
- Do not change `flake.nix` or `flake.lock`; `task-209` owns the reviewed backend pin after all required phase-2 implementation, migration, rollback, and integration gates pass.
- Do not create a placeholder backend commit merely to satisfy cross-repository wording. A design review reference is not a pin-eligible implementation commit.
- The project cross-repository build-loop rule applies only when the selected task changes the sibling repository. Task-003's approved validation-only exception makes no sibling diff, candidate commit, migration execution, or pin change; tasks 200-209 remain fully subject to candidate-commit, upstream review, migration/rollback, integration-before-pin, and follow-up-commit sequencing.
- Do not add Daedalus API clients, production validators, transaction parsing, witness differencing, IPC, Electron, frontend, hardware, collateral, or UI behavior.
- Do not reopen task-002 wire decisions or resolve exact-CBOR era coverage owned by `task-004`.
- Do not claim that a node local-state query and a wallet database read are globally atomic unless the contract provides and tests a concrete consistency protocol.
- Do not edit either review log.

## Dependencies

- `task-001`: completed in Daedalus commit `2bf49be1d`; establishes cardano-wallet as trusted authority for UTxO, ownership, and pending-submission state while main validates broker-bound result identity.
- `task-002`: completed in Daedalus commit `5a6b33c19`; freezes public CIP-30/CIP-8/CIP-95/CIP-103 behavior and local fixtures under `source/common/cip30/contracts/`.
- Current Daedalus backend pin:
  - release input: `cardano-foundation/cardano-wallet/v2026-07-23`
  - locked revision: `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`
- Sibling baseline observed during planning:
  - checkout: `/home/westbam/Development/cardano-wallet`
  - branch: `amw/cip30`
  - HEAD: `d3d170d02df9e39be04d85f3ce09fca98c9c5380`, aligned with `upstream/master` at inspection time
  - no task-003 backend changes are present; untracked `.idea/` is unrelated and must remain untouched
- Downstream consumers: `task-200` through `task-209`, then transaction-context, witness, CIP-8, and submission consumers in phases 3, 5, and 7.

## Research Consulted

- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`: assigns backend consistency and pending-submission evidence to task-003.
- `.agent/plans/dapp-browser-cip30/research/02-cip30-wire-contract-evidence.md`: freezes the public values and errors the backend-facing design must support without changing task-002 wire behavior.
- `.agent/plans/dapp-browser-cip30/task-plans/task-001.md` and `task-002.md`, including task-002 planning and implementation review history: establish the authority boundary, exact wire decisions, reuse-first signing requirement, and truthful dependency completion.
- `research/03-cardano-wallet-backend-contract.md` is the principal
  implementation artifact and records the user-authorized signoff assumption.

## Docs, Workflows, And Skills Consulted

- Documentation and process:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
  - `.agent/system/api-endpoints.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - `.agent/plans/dapp-browser-cip30/prompt.md`, especially cross-repository build-loop step 1a
- Required workflows:
  - `.agent/workflows/ipc.md`
  - `.agent/workflows/nix.md`
  - `.agent/workflows/build.md`
  - `.agent/workflows/test.md`
  - `.agent/workflows/update-doc.md`
- `.agent/workflows/electron.md`, `.agent/workflows/frontend.md`, and `.agent/workflows/hardware-wallets.md` were not needed: this task changes no Electron lifecycle, renderer API, or device flow. Their downstream surfaces must not be pulled into this validation spike.
- Skill:
  - `understand` was loaded before nontrivial repository exploration. No `.understand-anything` graph exists, so material findings were verified directly against live Daedalus and sibling files.
- No Cardano CLI, protocol-parameter, encoding, or operator-only skill is directly applicable. This task specifies a backend contract and does not execute chain operations.

## Live Evidence For Planning

- Daedalus pins cardano-wallet by tag in `flake.nix` and exact revision in `flake.lock`; the sibling HEAD is newer and must not be treated as the bundled backend without pin-specific comparison.
- Daedalus currently submits external transaction bytes through `/v2/proxy/transactions`. Its request helper treats a hex string as octet-stream data, calculates `Content-Length` as half the string length, and writes with `hex` encoding. The phase-2 client contract must replace this ambiguity with one validated exact-byte representation.
- The pinned/sibling API exposes:
  - `/wallets/{walletId}/statistics/utxos`, which is only a histogram.
  - `/wallets/{walletId}/utxo`, whose `ApiWalletUtxoSnapshotEntry` contains only ADA and assets.
  - `/wallets/{walletId}/transactions-sign`, which returns a complete modified transaction and has no `partialSign` or reviewed-context input.
  - `/wallets/{walletId}/transactions-submit`, which rejects foreign transactions and uses the wallet submission path.
  - `/proxy/transactions`, which explicitly does not add wallet pending-state guarantees.
  - Catalyst-oriented metadata signing, which is not CIP-8 `signData`.
- Wallet primitive `TxOut` stores only address and token bundle. Datum and reference-script data are discarded, so it cannot be serialized into an exact CIP-30 UTxO.
- `getUTxOByTxIn` provides full ledger `UTxO` only in recent Conway/Dijkstra eras in the inspected sibling source. The delivery contract must state supported-era behavior and fail closed rather than silently falling back to reduced wallet state.
- `readWallet` captures checkpoint metadata and pending transactions in one wallet DB atomic action, but node local-state query is a separate authority. The contract needs an explicit consistency/retry or named-snapshot protocol; “atomic” cannot be asserted by prose alone.
- Existing wallet-scoped `submitTx` broadcasts first and records the submission second, leaving a crash window. `submitExternalTx` never records local submission state. The required durable-before-broadcast/idempotent behavior is not present.
- Existing submission storage already retains sealed transactions and status transitions and supports chain rollback operations. Reuse may avoid a new schema, but that must be proven during `task-208`; no-migration is the preferred outcome, not an assumption.
- The sibling migration framework backs up each database before a forward step and rejects unexpected versions. It does not by itself prove that an older pinned binary can reopen a database written by a newer schema.
- `CONTRIBUTING.md` states that cardano-wallet is maintenance-only but accepts externally developed features after thorough review. Upstream maintainer engagement is therefore a real completion checkpoint, not paperwork that can be inferred from local code.

## Expected Files

- `.agent/plans/dapp-browser-cip30/research/03-cardano-wallet-backend-contract.md`
  - Pin-specific and sibling baseline evidence, exact candidate contract, gap/reuse matrix, consistency protocol, error/privacy rules, test obligations, owner/reviewer/review-link record, migration/rollback policy, and pin gate.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
  - Add a concise link/status summary for the accepted backend contract and correct any validation-discovered contradiction without duplicating the research note.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
  - Preserve the already completed `task-002`. Mandatory task-003 reconciliation changes its description, target paths, implementation notes, and acceptance criteria to the validation-only contract/evidence scope and explicitly delegates sibling commits/migrations/pinning to tasks 200-209. After manual evidence and implementation review approval, mark task-003 complete with truthful completion notes naming the accepted design review, not a pin-eligible commit.
- `.agent/plans/dapp-browser-cip30/task-plans/task-003.md`
  - Later lifecycle, handoff, review, and outcome synchronization.

No sibling cardano-wallet source, Swagger, test, migration, Daedalus source, package manifest, lockfile, Nix pin, API endpoint reference, review-log, frontend, translation, Storybook, or Cucumber file is expected to change in the smallest solution.

## Smallest Implementation Approach

1. Establish two explicit baselines before designing anything.

   - Inspect every material backend surface at locked revision `724be55d...` with `git show` or a detached read-only worktree and separately at the sibling review baseline.
   - Record tag, exact commit, branch, remotes, and relevant path-level differences. Never use newer sibling behavior as evidence for the bundled pin.
   - Build a requirement-to-existing-seam matrix for capabilities, full UTxO/context, signing, CIP-8/CIP-95, and submission.

2. Freeze one additive capability/version contract with no endpoint ambiguity.

   - The proposed V1 endpoint is `GET /v2/dapp-capabilities`, with no request body or query parameters and one strict JSON response: `{api_version: 1, backend_build: {version: string, source_revision: 40-lowercase-hex}, network: {network_id: 0|1, network_magic: Word32, genesis_hash: 64-lowercase-hex, current_era: string}, capabilities: [{name, revision, available_eras}]}`. Every object rejects unknown fields; capability entries require a nonempty lowercase kebab-case name, positive safe-integer revision, unique name, and nonempty unique era list.
   - V1 names are exactly `transaction-context`, `reviewed-context-signing`, `cip8-cip95`, and `durable-wallet-submit`, each initially revision `1`. Unknown future names are ignored only after the enclosing response passes strict structural validation; duplicate known or unknown names, duplicate eras, nonpositive/unsafe revisions, contradictory network identity, or malformed build identity invalidate the whole response.
   - `backend_build.source_revision` is the binary's compiled source revision and must equal the exact Nix-pinned revision expected by the packaged Daedalus build. `version` is diagnostic only and never substitutes for source-revision or capability checks.
   - At startup Daedalus validates the whole response and requires all four V1 capabilities before making the connector available. Missing/partial/old capabilities disable the connector without probing endpoints. Before every context, signing, data-signing, or submission invocation, Daedalus rechecks cached source/network identity and the relevant capability; network/genesis/era change forces a fresh capability fetch and revokes stale context.
   - Capability `available_eras` is authoritative only for advertised implementation availability, not transaction validity. A transaction outside the intersection of capability availability and task-004's supported-era matrix fails closed.
   - Existing non-dApp callers and endpoints remain additive and behavior-compatible.

3. Freeze one implementable wallet-scoped transaction-context operation rather than separate overlapping UTxO and review endpoints.

   - The request binds wallet ID in the route, network/genesis identity, ordered exact transaction CBOR values, and which normal/collateral/reference inputs and wallet UTxOs are requested. It never accepts authoritative derivation paths, ownership flags, reduced TxOuts, or caller-calculated parent output summaries.
   - Acquisition attempt 1 reads one wallet DB transaction containing checkpoint chain point `W`, monotonic `wallet_generation G`, monotonic `pending_generation P`, wallet discovery/registration state, pending records, and wallet outpoints. It then acquires one node LSQ at exactly `W`, obtaining era/protocol/network/parameters and all requested chain UTxOs. A second wallet DB transaction must confirm the same `W/G/P`; otherwise discard all material and retry the complete acquisition at most two more times. Three mismatches, inability to acquire `W`, rollback/pruning during acquisition, or any missing generation fails closed as `context_unavailable` with no partial response.
   - `G` increments on any checkpoint/discovery/registration state mutation relevant to ownership or proof selection. `P` increments on every submission-record insertion or state transition relevant to available/pending UTxO. They are contract fields to be implemented and tested, not assumed to exist today.
   - Response provenance is explicit per resolved outpoint. Precedence is: an exact output independently derived from an earlier transaction in this ordered request; then an exact output decoded from the wallet's durable pending record; then node UTxO at `W`. Wallet checkpoint `TxOut` supplies only candidate membership/discovery and never output bytes. If two authoritative sources claim the same outpoint with unequal exact bytes, fail `context_conflict`; equal bytes retain all provenance labels.
   - Earlier outputs are derived only from a strictly lower request index by hashing the exact parent body and decoding the indexed exact output. Self and forward references, out-of-range indexes, malformed parents, duplicate parent identities with unequal full bytes, and unresolved outputs fail. Duplicate spending/collateral claims and attempts to spend an already-pending input are reported as ordered conflicts, while reference-input reuse is a dependency but not a spending conflict. A single input used incompatibly within one body fails invalid request.
   - Node inputs spent or absent at `W`, pending records rolled back/expired or lacking exact sealed bytes, unsupported eras, and wallet outpoints whose full output cannot be recovered all fail closed. No reduced-output fallback is permitted. Outputs from pending and earlier transactions are decoded from their exact sealed transaction bytes, preserving address, value, datum/inline datum, and reference script.
   - The response includes canonical source bytes and an independently recomputable `context_digest = Blake2b-256(domain || length-prefixed wallet_id || genesis_hash || W || G || P || ordered exact transaction bytes || canonical sorted provenance/output/ownership/parameter records)`, where domain is ASCII `daedalus-dapp-context-v1`. Numeric and point encodings, record sort keys, and length prefixes are fixed in the research contract; JSON serialization is never hashed.
   - The backend also returns a stateless authenticated `context_token` containing a random per-process generation and a MAC that binds exactly that generation, digest, and capability revision under a random process-memory key. No per-context record is persisted. Signing accepts only the exact token, digest, wallet/network, and bodies; it verifies the process generation/MAC and recomputes the digest from supplied authoritative context before key use. Daedalus independently recomputes the public digest before review and before accepting a result.
   - The token has no wall-clock timeout beyond the main-owned five-minute consent-inactivity rule, but is valid only for the current backend process and capability revision. Backend restart/key loss, malformed token, capability change, or wallet/network mismatch invalidates it and requires a complete new context and trusted review. Ordinary chain advancement/rollback after successful capture does not invalidate it; signing uses the approved snapshot and later submission may fail normally.

4. Apply the reuse-first signing decision with decisive V1/V2 evidence.

   - Test and document both live paths selected by `RootKeyAccessV1` and `RootKeyAccessV2`, not merely the HTTP endpoint. V1 uses `addVkWitnesses` with wallet UTxO/address lookup; V2 derives input/stake/policy paths and unions only address/VKey witnesses, currently requires recent-era resealing, and has different coverage and failure behavior.
   - For each path, compare exact input/output transaction envelopes. Body bytes, outer `isValid`, auxiliary data, native/Plutus scripts, bootstrap witnesses, existing VKeys, datums, redeemers, and every non-VKey witness class must be byte-identical. The only accepted mutation is set-union addition of independently valid VKey witnesses; removal, replacement, duplicate ambiguity, or any other mutation fails the backend contract.
   - Full-transaction response plus task-306 main-side witness differencing is accepted only when differential tests prove those invariants across both paths. Daedalus validates the envelope, exact body hash, all immutable witness classes, computes the set difference by exact VKey `(public_key, signature)`, rejects pre-existing or invalid entries, and verifies each new signature and expected credential before release.
   - Prefer an additive extension to the existing request or another minimal reviewed-context input over a parallel witness-only endpoint. A new endpoint is allowed only if the backend cannot safely bind the reviewed context/current-batch overlay or implement `partialSign` completeness through the existing seam; the evidence note must name the failed seam and the reason.
   - Keep all-or-nothing CIP-103 witness disclosure and final witness-set verification in Daedalus unless backend atomicity is concretely necessary. Do not add backend persistence for staged witnesses.
   - The signing request must carry the authenticated context token/digest, exact transaction and ordered parent bytes, request index, and `partialSign`. Backend-owned context supplies current-batch output ownership; caller paths and summaries remain forbidden.
   - Freeze body-identity response evidence, `partialSign=false` complete key/native-script proof, `partialSign=true` all-owned-witness behavior including canonical empty success, collateral/required-signer/governance coverage, and wrong-passphrase/unsupported-proof errors. If either V1 or V2 cannot meet one required invariant through additive `transactions-sign` input, the research must reject reuse for that path and justify the smallest replacement endpoint.

5. Freeze one exact CIP-8/CIP-95 COSE ownership boundary.

   - Specify payment, stake, and role-3/index-0 DRep credential ownership and derivation; raw public-key outputs; registration classification including pending certificates; and exact payload/address inputs.
   - Select backend production of the complete frozen task-002 `COSE_Sign1` and `COSE_Key`; no raw-signature alternative is accepted. The request contains exact address or raw DRep-ID form, exact payload bytes, expected network/genesis, and passphrase. Backend alone resolves wallet ownership and chooses payment, stake, or role-3/index-0 DRep key.
   - Backend returns exact untagged COSE hex plus normalized credential kind and bytes. It must produce the frozen protected/unprotected headers, attached payload, empty AAD, `hashed:false`, `version:1`, no `kid`, and the task-002 matching type-6 DRep normalization. Daedalus independently decodes the exact COSE bytes, checks address/DRep and public-key hash association, exact payload, headers, COSE key, and Ed25519 signature before release.
   - Explicitly exclude Catalyst metadata signing, committee/pool signing, and renderer-supplied paths.

6. Freeze a precise write-ahead wallet-scoped submission state machine without inventing a second journal.

   - Reuse `/wallets/{walletId}/transactions-submit` and the existing submission store where possible; retire `/proxy/transactions` from the dApp path.
   - Identity is `(wallet_id, tx_id)` plus exact sealed transaction bytes and exact normal/collateral input sets. Recompute `tx_id` from exact body bytes. An existing key with unequal sealed bytes or input accounting is `identity_conflict` and never broadcasts; this explicitly covers the ordinary case where two envelopes have the same body/transaction ID but different witnesses or auxiliary material, not only hash collision.
   - Persist state `authorized` with exact bytes, input sets, expiry, authorization marker, and `attempt_generation=0` in one wallet DB transaction before any node call; this commit is the point of no return. Atomically transition to `broadcasting(generation, started_at)` before `postSealedTx`, then to `submitted(accepted_at)` after node acceptance. Deterministic node rejection transitions to terminal `rejected(code)`; transient/unknown transport outcome transitions to `outcome_unknown(generation)`; chain observation transitions `submitted|outcome_unknown -> in_ledger(slot)`; expiry transitions non-ledger states to `expired`; rollback transitions `in_ledger -> submitted` without deleting exact bytes.
   - Startup never submits a persisted `authorized` record and never resumes an
     interrupted batch. It converts persisted `broadcasting` to
     `outcome_unknown`, then brackets separate same-node canonical-chain and
     mempool observations with equal synced tips, retrying the bracket at most
     three times. Presence advances state; coherent absence may return an
     unexpired exact transaction to `authorized` with incremented generation,
     but only a later exact dApp retry may start an attempt. Unavailable,
     unsynced, or moving-tip observations retain `outcome_unknown`.
   - Exact replay in `authorized` may own the next serialized attempt; replay in `broadcasting`, `outcome_unknown`, `submitted`, or `in_ledger` returns the existing hash/status without broadcasting; replay in `rejected` or `expired` returns the recorded terminal failure. A per-wallet transaction lock and unique DB key serialize concurrent first calls/replays.
   - Wallet relevance and ownership are computed from authoritative context before the `authorized` commit. Normal and collateral inputs enter pending accounting in that same transaction; reference inputs do not. Transition to `rejected` or `expired` releases those input claims atomically while retaining the terminal evidence; rollback to `submitted` restores the relevant pending claims. Never-broadcast `authorized` records remain explicit and recoverable until an exact retry attempts them, their `invalid_hereafter` expires at a synced tip, or they are user-forgotten under existing safe semantics; transactions without an upper validity bound do not expire by wall clock.
   - CIP-103 submission performs the same state machine sequentially in caller order, creates/attempts each item independently after batch consent, continues after prior failures, and returns aligned results. It does not use a batch journal, and one item's state cannot suppress another item's required attempt.
   - Record whether existing storage is sufficient. Prefer no schema change. If phase-2 implementation needs one, require versioned atomic forward migration, fixtures from the currently pinned schema, backup/restore rehearsal, and an explicit statement that old-pin rollback is supported or is blocked until database restoration. Never claim binary downgrade compatibility without evidence.

7. Freeze backend-to-public error and privacy boundaries.

   - The research note contains an exhaustive table from backend HTTP/status/error tags to task-002 public errors. At minimum: malformed hex/CBOR/request/context-body mismatch, same-process invalid context MAC, wrong network, unsupported era/field, provenance conflict, self/forward/unresolved input, and replay identity conflict map to `APIError.InvalidRequest`; wallet/network route change maps to `APIError.AccountChange`; snapshot retry exhaustion, context process-generation mismatch after backend restart, node query failure, and unexpected persistence failure map to `APIError.InternalError`; missing proof, wrong passphrase, unsupported signer coverage, or incomplete `partialSign=false` map to `TxSignError.ProofGeneration`; deprecated Genesis/MIR maps to `TxSignError.DeprecatedCertificate`; unowned/unavailable data key maps to `DataSignError.ProofGeneration`, script credential to `DataSignError.AddressNotPK`; deterministic/transient submission rejection after authorization maps to `TxSendError.Failure`. User-decline/refusal remains main-owned and is not synthesized by backend.
   - Each mapping fixes one canonical nonsensitive `info` value or empty info. Backend detail is retained only as a local redacted diagnostic category; no raw transaction, output, address, payload, public key, signature, passphrase, context token/digest, origin, derivation path, full URL, or database content appears in public `info`, HTTP error text, traces, metrics, or transaction logs.
   - Capability validation occurs at startup and relevant identity/capability validation repeats before each invocation. Detailed attacker-controlled bytes are parsed only after the authenticated Daedalus-to-backend TLS boundary and before context/key/persistence/node side effects.

8. Produce and assign the requirement-to-test/evidence matrix.

   - `task-200`: API type/schema unit tests, Swagger/golden consistency, strict capability malformed/duplicate/partial/old/build-mismatch cases, and API integration tests in `lib/api` plus `lib/integration/scenarios/.../API`.
   - `task-201`: network-layer LSQ unit tests and local-cluster integration for exact-point acquisition, three-attempt races, rollback/pruning, pin-versus-sibling full-output goldens, datum/reference-script fidelity, unsupported eras, and context binding.
   - `task-202`: wallet unit/property and address-discovery tests for authoritative ownership, generations, pending/earlier provenance precedence, adversarial self/forward/unresolved/duplicate/conflicting dependencies, and current-batch derivation.
   - `task-203`: `Cardano.Wallet.Shelley.TransactionSpec`, `TransactionLedgerSpec`, API integration, and differential fixtures for V1/V2 exact body/envelope preservation, every immutable witness class, VKey-only delta, current-batch context, partial modes, collateral, and required signers.
   - `task-204`: wallet/API unit and integration golden vectors for complete payment/stake CIP-8 COSE production, exact payload/header/key/signature verification, wrong passphrase, script, and unowned credentials.
   - `task-205`: address-derivation/discovery unit vectors and wallet/API integration for role-3 DRep, stake/DRep public keys, pending registration classification, matching type-6 normalization, and DRep COSE.
   - `task-206`: wallet transaction-ledger differential tests for Conway fields, DRep certificates/votes, deprecated certificates, completeness, and fail-closed unsupported credentials.
   - No `task-207` exists in the task graph; tracker reconciliation records that no obligation is silently assigned to a nonexistent task.
   - `task-208`: submission model/state-machine property tests, `DB.Store.Submissions` and SQLite unit tests, API/local-cluster fault injection before and after every transition, concurrent replay, unknown outcomes, confirmation/expiry/rollback, attempt-all ordering, pinned-schema migration, backup restore, and explicit old-pin open-or-restore behavior.
   - `task-209`: Daedalus API-client runtime validation/Jest, exact octet-stream byte tests, capability downgrade/malformed integration, candidate-backend Nix build/smoke, migration/rollback evidence review, and integration-before-pin plus post-pin rerun.
   - The user-authorized assumption accepts this task-003 assignment. Concrete
     downstream implementations still require their named sibling test layers,
     fixtures/fault points, evidence, and authorized reviews.

9. Produce the upstream review handoff and stop at the human checkpoint.

   - Prepare a concise review checklist and proposed decision record from the research note.
   - Ask the operator to route it to a named owner and active maintainer reviewer through a durable upstream issue/discussion/design PR.
   - Require explicit responses on current-batch derivation/ownership, exact-point/generation/retry protocol, digest/token lifetime, V1/V2 signing reuse, complete backend-produced COSE, write-ahead submission transitions, capability/error/privacy contract, migration/rollback, assigned evidence matrix, API compatibility, and the intended upstream implementation PR sequence.
   - The user satisfied this checkpoint by directing the Orchestrator to assume
     implementation signoff. Missing external metadata is recorded explicitly.
     Any later concrete feedback that changes task-002 public behavior still
     reopens the affected contract instead of silently changing it.

10. Record future cross-repository delivery and pin sequencing without performing it.

- Phase-2 work starts from an upstream-aligned sibling branch and lands in reviewable commits/PRs for API capability/types, context/ownership, signing/CIP-8/CIP-95, and submission/migration as appropriate.
- For every candidate implementation revision: complete sibling self-review and tests, create candidate commit(s), obtain upstream/sibling review, run database migration/restore or rollback compatibility evidence, and run Daedalus integration against the candidate without changing the tracked pin.
- Only after those gates pass may `task-209` update `flake.nix`/`flake.lock` to the exact reviewed commit and rerun integration. Review fixes use follow-up commits, never amended reviewed history, and repeat affected pre-pin gates.
- Task-003 records the accepted design-review reference and delivery path. The eventual pin-eligible implementation hash is intentionally produced by phase 2 and must not be fabricated here.

11. Mandatorily reconcile documentation, tracker authority, and completion state after review approval.

- Add a compact PRD pointer to the accepted research artifact and its human review evidence.
- Preserve task-002's existing completed state. Before task-003 can complete, change task-003's tracker description to validation/design, target paths to the PRD/research/task-plan/tracker only, implementation notes to the accepted contract and future sequencing, and acceptance criteria to the design review plus accepted evidence matrix. Remove requirements that task-003 itself produce sibling code, a migration run, a pin-eligible commit, or a pin update.
- Add an explicit tracker note that task-003 is the validation-only exception to cross-repository build-loop step 1a because it makes no sibling diff; tasks 200-209 produce and review candidate commits and task-209 performs the pin.
- Update task-003 status only after the user-authorized signoff assumption
  and implementation-review evidence are incorporated; completion notes must
  disclose missing external metadata and preserve all downstream gates.
- Parse the tracker and inspect a focused diff to prove no unrelated task/dependency/status drift.

## Acceptance Criteria

- `research/03-cardano-wallet-backend-contract.md` records the pinned and sibling revisions, exact inspected surfaces, and a requirement-to-gap/reuse matrix; bundled behavior is never inferred from newer sibling HEAD.
- Before completion, the task-003 tracker description, target paths, implementation notes, acceptance criteria, and completion notes are reconciled to phase-0 validation-only scope. They explicitly exempt this no-sibling-diff task from immediate cross-repository candidate-commit sequencing while retaining that mandatory sequence for tasks 200-209 and task-209 pinning.
- The accepted capability contract fixes `GET /v2/dapp-capabilities`, its strict V1 schema, unique names/revisions/eras, source revision and packaged-build relationship, network identity, malformed/duplicate/contradictory/partial/old behavior, startup check, per-invocation recheck, and fail-closed availability.
- The context contract captures wallet point `W`, wallet generation `G`, pending generation `P`, and one LSQ acquired exactly at `W`; verifies unchanged `W/G/P` after query; permits at most three complete attempts; and returns no partial context on mismatch, rollback, unavailable point, or retry exhaustion.
- Context provenance and precedence are exact for earlier-request, durable-pending, and node outputs. Missing/spent/rolled-back/expired/duplicate/self/forward/unresolved/conflicting/unsupported-era cases are fixed, and every wallet-controlled output is recovered from exact ledger/sealed bytes rather than lossy `TxOut` persistence.
- The response covers exact full normal/collateral/reference outputs, complete wallet UTxOs, pending overlay, ownership/paths, required proofs, registration/governance state, and current-batch dependencies. Its domain-separated, length-prefixed, canonical `context_digest` is independently recomputable by Daedalus.
- The selected context lifecycle is stateless authenticated context: the backend retains only a random process-memory MAC key, persists no context record, and accepts a token only for its exact digest/wallet/network/bodies/capability revision. Restart/key loss or identity change forces a fresh context and review; ordinary later chain movement does not.
- Renderer-supplied derivation paths, ownership flags, TxOut summaries, parent-output summaries, or protocol context are never authoritative.
- Full outputs come from ledger representation/local-state query and preserve address, value, datum/inline datum, and reference script; the lossy wallet `TxOut` is not used as exact output evidence.
- Both `RootKeyAccessV1` and `RootKeyAccessV2` paths have decisive evidence for exact body/envelope preservation and all witness classes. Accepted output changes are limited to independently verified new VKey set members; task-306 differencing is selected only if both paths pass. Current-batch context and complete/partial semantics are exact, and any replacement endpoint has path-specific written justification.
- The CIP-8/CIP-95 ownership boundary is singular: backend authenticates ownership/key selection and returns complete frozen `COSE_Sign1`/`COSE_Key`; Daedalus independently verifies exact bytes, normalized credential association, payload, headers, public key, and signature. Catalyst signing, raw-proof alternatives, and caller paths are excluded.
- Submission uses the exact write-ahead states and transitions: `authorized -> broadcasting`; `broadcasting -> submitted|outcome_unknown|rejected`; `submitted|outcome_unknown -> in_ledger`; active non-ledger states may become `expired`; and rollback moves `in_ledger -> submitted`. It has durable exact-byte identity, atomic normal/collateral claim/release accounting, serialized concurrent replay, never-broadcast recovery, terminal rejection/expiry, and no blind restart rebroadcast.
- Exact replay and same-hash/different-bytes behavior are fixed for every state. CIP-103 attempts every item sequentially in caller order and uses only per-item cardano-wallet records for recovery, without a Daedalus or batch journal.
- The backend-to-task-002 error matrix fixes every accepted HTTP status, backend
  tag, public error, and nonsensitive `info` value. Sensitive values are absent
  from public errors, HTTP bodies, traces, metrics, and transaction logs, with
  explicit downstream redaction-test owners.
- Migration analysis states either proven no-schema-change reuse or the exact forward migration, backup/restore, and old-pin rollback compatibility gate required later. No unsupported downgrade claim remains.
- The research artifact contains a requirement-to-test/evidence matrix assigned
  to tasks 200-206, 208, 209, 304, 306, and 307 and named sibling/Daedalus test
  layers, explicitly noting that task-207 does not exist. It covers every race,
  fidelity, context, signing, COSE, capability, privacy, crash/replay, lifecycle,
  migration, restore, rollback, and pin-integration obligation.
- The research record contains either named durable upstream acceptance or the
  explicit user-authorized task-003 signoff assumption, without fabricated
  metadata. Tasks 200-209 retain concrete implementation/review evidence gates.
- The delivery record distinguishes the accepted design-review reference from the future pin-eligible implementation commit. It records that candidate implementation commits, sibling review, migration/rollback evidence, Daedalus integration, and only then pin update are mandatory in phase 2.
- The PRD, research, task plan, and tracker agree. `task-002` remains truthfully completed, and no phase-2/backend/pin implementation is smuggled into task-003.

## Verification

- Compare relevant files at `724be55dc66cf67bc4427e8f1a9657a9d1d33d71` and sibling review HEAD with `git show`, `git diff`, and direct reads; record commands and findings in the research note.
- Verify every proposed field and guarantee against live API types, Swagger, wallet core, network local-state query, submission store, and migration code. Mark unproven behavior as required future work, not current capability.
- Trace every frozen task-002 backend-relevant method/result to a contract capability or an explicit non-backend responsibility.
- Validate the exact capability schema examples and negative cases with a local schema validator or equivalent table-driven checker; verify startup/invocation timing, source revision, network/era, and partial capability outcomes are represented.
- Independently recompute context-digest fixtures from the specified binary preimage and mutate each bound field to prove the digest changes. Table-review exact-point acquisition, `W/G/P` races, three-attempt exhaustion, provenance precedence/conflicts, and process restart/token invalidation.
- Compare V1/V2 input/output fixtures across body, envelope, every witness class, auxiliary data, scripts, datums, and redeemers. Verify only valid new VKeys survive differencing, and verify complete backend-produced CIP-8/CIP-95 COSE vectors against task-002 goldens.
- Model-check the submission transition table for every legal/illegal transition and crash edge, concurrent replay, exact identity conflict, unknown outcome, never-broadcast record, confirmation, expiry, rollback, and CIP-103 continuation.
- Review the exhaustive backend/public error and forbidden-observability matrices for full coverage and static nonsensitive information.
- Verify every contract requirement has exactly one phase-2 task owner and named sibling/Daedalus test layer in the evidence matrix. Confirm no obligation is assigned to nonexistent task-207.
- Verify the research record truthfully captures the user-authorized task-003
  signoff assumption and missing metadata, and that no downstream concrete
  review, migration, integration, or pin gate was waived.
- Parse `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`, check internal Markdown paths/links, run focused Prettier checks on changed Markdown/JSON, and run `git diff --check`.
- Diff task-003's governing tracker object before and after reconciliation. Require validation-only description/paths/notes/acceptance, preserved dependencies, preserved task-002 completion, no unrelated task edits, and no claim of a sibling implementation hash or migration run.
- Inspect Daedalus and sibling `git status` separately. Do not modify or stage unrelated Daedalus work or sibling `.idea/` content.
- No Haskell build, backend test, database migration rehearsal, Daedalus build, Nix pin build, Jest, Cucumber, Electron, hardware, or network transaction test is required for the smallest design-validation outcome. The research note must enumerate the exact sibling and cross-repository tests required when phase-2 code exists.

## Risks And Open Questions

- Human-checkpoint evidence risk: the user authorized an assumed signoff without
  supplying external identities or a URL. This is sufficient only for task-003;
  tasks 200-209 must produce concrete owner and review evidence.
- Task wording risk: the tracker currently asks task-003 for an exact reviewed commit eligible for the Daedalus pin even though tasks 200-208 create that implementation and task-209 pins it. Mandatory reconciliation is an acceptance gate, not optional wording cleanup; task-003 cannot complete while the contradiction remains.
- Consistency risk: wallet DB and node LSQ are separate snapshots. The accepted contract must define correlation/retry semantics and avoid an impossible global-atomicity promise.
- Context-lifetime risk: the stateless token depends on a process-memory MAC key. Backend restart intentionally invalidates review and requires a fresh context; the implementation must avoid logging/persisting the token or treating later chain movement as token invalidation.
- Era risk: current full-output LSQ support is limited to recent eras in live code. Supported-era behavior and fail-closed errors must be agreed with `task-004`; no reduced fallback is allowed.
- Signing risk: current V2 signing uses wallet checkpoint UTxO and has known collateral/required-signer gaps. Reuse of the endpoint does not mean reuse of its current incomplete semantics.
- Submission risk: persisting before broadcast can leave a never-broadcast pending record, while persisting after broadcast has the observed crash gap. The phase-2 state machine and retry tests must make both windows recoverable.
- Rollback risk: cardano-wallet's migration backups do not prove older-binary database compatibility. If a schema change is unavoidable, release rollback may require restoring the pre-migration backup rather than simply repinning.
- Upstream risk: cardano-wallet is maintenance-only. Rejection or requested redesign is a valid blocking result and must be reflected in the phase-2 path, not bypassed with an undocumented long-lived fork.
- Privacy risk: exact transactions, addresses, key associations, payloads, signatures, and passphrases must not be copied into logs, review URLs, fixtures, or research. Use synthetic/redacted examples only.

## Required Docs, Tracking, And Research Updates

- Create `.agent/plans/dapp-browser-cip30/research/03-cardano-wallet-backend-contract.md` as supporting evidence and the durable accepted contract record.
- Update the PRD with a concise accepted-contract link, review status, and downstream pin/migration gate; keep normative public wire behavior in the PRD/task-002 artifacts.
- Preserve the live completed state and metadata for `task-002`. If concurrent/stale tracker content reintroduces `pending`, restore only its already evidenced completion metadata from commit `5a6b33c19` and make no other task-002 change.
- Reconcile the task-003 tracker object before completion: validation-only
  description and paths; concrete contract/evidence notes; the user-authorized
  signoff assumption; explicit no-sibling-diff exception; and delegation of
  commits, reviews, migrations, rollback, integration, and pinning downstream.
- After implementation review approval, update task-003 completion metadata with
  the user-authorized assumption, unavailable external metadata, accepted
  evidence matrix, and validation-only outcome. Do not record an unproduced
  backend hash, migration result, or pin.
- Update this canonical plan with the handoff evidence, final planning/build status, and outcome during later lifecycle steps.
- Do not update `.agent/system/api-endpoints.md` until a backend API is implemented and consumed; documenting proposed endpoints there would misrepresent live behavior.
- Do not update workflows, architecture, manifests, lockfiles, source, translations, or sibling files for the smallest task-003 outcome.

## Review-Log Paths

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-003-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-003-impl-review.md`
- Planner does not write either review log.

## Planning Self-Review

- Scope creep: kept task-003 to pin-specific evidence, an exact reviewed design contract, upstream handoff, and documentation/tracker synchronization. Phase-2 Haskell, tests, migrations, clients, and pin changes remain downstream.
- Stale workflow wording: treated workflow examples as orientation only. Live `flake.nix`/`flake.lock`, current request code, sibling API/core/DB files, and the project-specific cross-repository build loop control.
- Blocker 1, tracker/orchestration: made task-definition reconciliation mandatory at description/path/note/acceptance/completion level and recorded the validation-only no-sibling-diff exception while preserving full phase-2 candidate-commit and pin gates.
- Blocker 2, coherent context: fixed `W/G/P`, exact-point LSQ, three-attempt capture, provenance precedence/conflicts, full-output recovery, canonical recomputable digest, stateless process-key token, and restart/chain-movement semantics.
- Blocker 3, signing/COSE: required decisive V1/V2 envelope and every-witness-class evidence, VKey-only verified deltas, exact current-batch/partial input, and selected complete backend-produced COSE with independent Daedalus verification.
- Blocker 4, submission: replaced atomic-with-broadcast prose with exact write-ahead identity, states, transitions, commit point, crash/unknown-outcome recovery, replay/concurrency, terminal, rollback, expiry, input accounting, and CIP-103 rules.
- Blocker 5, capability/error/privacy: selected an exact strict capability endpoint/schema and timing, source/build/network/era behavior, exhaustive backend/public mapping, static nonsensitive errors, and forbidden observability values.
- Blocker 6, evidence assignment: mapped requirements to tasks 200-206, 208,
  209, 304, 306, and 307 plus named sibling/Daedalus layers, identified absent
  task-207, and recorded the task-003 signoff assumption without weakening
  downstream evidence.
- Missing manifests/tests/docs: included capability schema examples, digest fixtures, transition model, error/privacy matrices, assigned phase-2 evidence, PRD/research/tracker synchronization, links, formatting, and focused diff verification.
- Security/wire-contract drift: retained backend authority for full outputs, ownership, paths, pending state, and parent derivation; retained main-side independent result verification; prohibited reduced TxOut fallback, renderer authority, sensitive logs, and changes to frozen task-002 behavior.
- Hidden human checkpoints: classified the task as `manual_execution`, issued
  the exact handoff, and recorded the user's explicit assumed-signoff response
  plus missing metadata. Downstream concrete evidence remains mandatory.
- Cross-repository consistency: explicitly selected a no-backend-change validation outcome. It records the full candidate-commit, upstream review, migration/rollback, integration-before-pin, follow-up-commit, and pin sequencing required if/when phase-2 backend changes are made.
- Tracker consistency: verified that the selector's task-002 stale-status observation is already superseded in the live tracker and planned preservation/focused verification rather than unrelated edits.
- Internal consistency: capability identity binds context; context binds signing and ownership; submission independently revalidates exact bytes and wallet relevance; errors and privacy cover each boundary; downstream tests are assigned to the same task owners that implement those guarantees.

## Planning Status

- `approved`

## Build Status

- `completed`

## Current Outcome

- Completed after approved planning and implementation review. The accepted
  validation-only contract, PRD boundary, research evidence, and tracker are
  synchronized; no sibling source, migration, Daedalus source, or pin changed.
- On 2026-08-11 the user directed the Orchestrator to assume cardano-wallet
  implementation signoff and proceed. No owner, reviewer identity, or durable
  URL was supplied, so the research record states that limitation instead of
  inventing external evidence.
- The assumed signoff accepts the contract and evidence matrix for task-003
  only. Tasks 200-209 still require concrete sibling commits, authorized review,
  migration/rollback evidence, integration, and exact pin evidence.
- Verification passed for strict capability negatives, true Blake2b-256 digest
  and mutation fixtures, HMAC token fixture, submission-transition model,
  exact error/privacy ownership, tracker JSON/state, internal links, 175-path
  sibling drift, ignore-bypassed Prettier on the new research/canonical docs,
  focused PRD/tracker diff inspection, and `git diff --check`.
- Planning review is approved in `task-003-plan-review.md`; implementation
  review is approved at the latest entry in `task-003-impl-review.md`.
