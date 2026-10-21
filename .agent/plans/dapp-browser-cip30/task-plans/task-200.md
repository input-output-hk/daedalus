# Task task-200: Add backend capability version and API contracts

## Task

- Task ID: `task-200`
- Title: `Add backend capability version and API contracts`
- Phase: `phase-2` (`cardano-wallet Backend Foundations`)
- Priority: `critical`
- Tracker state at planning: `pending`
- Repository classification: cross-repository backend task. Production changes belong in the sibling `../cardano-wallet`; Daedalus consumes the reviewed backend only later through `task-209`.

## Why Now

- Completed dependency `task-003` fixed the backend delivery contract, but the pinned backend still has no dApp capability endpoint or dApp-specific API/error types. Downstream backend tasks must not invent incompatible contracts independently.
- Daedalus still pins `cardano-wallet` tag `v2026-07-23` at revision `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`; that older backend must remain a deterministic fail-closed case rather than being mistaken for connector support.
- The sibling checkout is branch `amw/cip30`; candidate `b2d20f4385bfcb92454b4dec91f954a0babd13ac` is rebased directly onto upstream `6761259ee91b138921231ce7fd1198679abfcc82`. The candidate adds the unavailable route and reuses the existing Servant API, API type/golden framework, network handler, error encoding, Swagger consistency test, and local-cluster API suite.
- Contract-first implementation is required before tasks 201-208 add context, signing, CIP-8/CIP-95, or durable submission behavior. The standalone task-200 candidate must keep the capability route fail-closed and must not publish a successful V1 document until all four operation families exist.

## Interaction Mode

- Mode: `autonomous` with internal combined review.
- No user input, manual handoff, upstream reviewer identity, upstream URL, or task-200 `cardano-wallet` pull request is required. The capability/API foundation is intentionally incomplete and remains unavailable, so upstream review at this point would review neither the complete backend range nor an activatable API.
- Tasks 200-208 accumulate locally as reviewable commits on `amw/cip30`. Task-209 owns the single consolidated upstream pull request and authorized upstream review for the complete task-200-through-task-208 range, followed by aggregate activation, the Daedalus pin update, and post-pin verification.
- Task-200 requires `@Reviewer` to review the exact candidate commit, implementation evidence, and canonical task outcome together. Review fixes are follow-up sibling commits, not amendments, and repeat affected task-200 checks.
- The implementation log's earlier upstream handoff is preserved as append-only history but is superseded by this current process decision; it is not a task-200 blocker or completion input.

## Scope

- Add the strict version-1 `GET /v2/dapp-capabilities` response contract defined by accepted research 03, but keep the task-200 HTTP handler on the exact standard route-not-found response until aggregate activation.
- Add minimal versioned Haskell types and strict JSON decoding/encoding for backend build identity, network identity, capability names/revisions, and available eras.
- Wire the route through the existing Servant `Network`/server seam, add a production response constructor supplied by existing network/genesis state plus the binary's embedded package version and source revision, and test those producer-owned fields without making the constructor reachable as HTTP success.
- Add the frozen dApp backend error tags, statuses, and fixed nonsensitive messages as a reusable server-error mapping for tasks 201-208.
- Keep Swagger, Haskell types, generated/golden JSON expectations, Haskell clients/links, and integration behavior aligned.
- Prove the old pinned backend has no endpoint and that malformed, duplicate, partial, old, and value-domain-invalid capability documents fail strict shape parsing without fallback probing.
- Produce a reviewable local sibling commit and verify its exact revision through temporary Daedalus Nix overrides before any tracked pin change.

## Non-Goals

- Do not implement coherent transaction context, full-output lookup, ownership/path analysis, reviewed-context signing, witness extraction, CIP-8/CIP-95 key operations, or write-ahead submission. Those belong to tasks 201-208.
- Do not add placeholder operational routes for context, signing, data signing, or submission. Capability descriptors are the versioned contract boundary; future task routes land with their implementations.
- Do not add a feature flag, environment variable, launcher option, test-only identity validator, or other way to activate successful capability publication in the task-200 candidate. Tasks 201-208 must not advertise partial readiness; task-209 owns the single aggregate activation after all four families are wired.
- Do not add a permissive capability parser, version-string inference, endpoint probing fallback, partial capability success, compatibility alias, or unversioned response.
- Do not add or modify persisted backend records, wallet database schemas, migration code, submission state, or backups.
- Do not change Daedalus API clients, expected pin/source/network comparisons, production unavailable classification, runtime feature activation, `flake.nix`, or `flake.lock`; `task-209` owns those consumer checks, final sibling capability activation, and the exact aggregate backend pin after all phase-2 gates.
- Do not open or require an upstream `cardano-wallet` pull request for task-200. Task-209 owns the one consolidated pull request and authorized upstream review for the complete local task-200-through-task-208 range.
- Do not add Electron IPC, renderer, guest, hardware, frontend, localization, packaging, or public CIP-30 wire behavior.
- Do not edit either review log directly.

## Dependencies

- `task-003`: completed in Daedalus commit `f2e133d78`; its accepted technical contract is normative. Its prior per-task upstream-review wording is superseded by the user-confirmed task-209 consolidated review process.
- Fixed public behavior from completed `task-002` remains normative. Backend errors map to those frozen public errors; this task does not alter public CIP-30 shapes.
- Exact era policy from completed `task-004` remains separate: capability `available_eras` states backend implementation availability, and later Daedalus support uses its intersection with the task-004 matrix. Dijkstra remains `unsupported/readiness-blocked`; task-200 must reject it before calling the partial `ApiEra.fromReadEra` conversion.
- Current packaged backend baseline: tag `v2026-07-23`, locked revision `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`.
- Current sibling implementation state: `/home/westbam/Development/cardano-wallet`, branch `amw/cip30`, candidate `b2d20f4385bfcb92454b4dec91f954a0babd13ac` rebased onto upstream `6761259ee91b138921231ce7fd1198679abfcc82`; unrelated untracked `.idea/` content must remain untouched.
- Downstream: tasks 201-208 add reviewable local commits implementing the four operation families without partial advertisement. Task-209 owns the one consolidated upstream PR/review for the complete task-200-through-task-208 range, aggregate activation only after that range is accepted, expected Daedalus pin/source/network equality, production unavailable classification, Daedalus clients, full-range migration/rollback and real HTTP/mTLS validation, the tracked pin update, and post-pin reruns.

## Research

- `research/03-cardano-wallet-backend-contract.md` is the principal accepted technical source. Its frozen successful V1 document and complete-set rule remain unchanged. Current canonical ownership supersedes its per-task upstream-review sequence: task-200 owns producer schema/value domains, authoritative runtime-field construction, unavailable routing, and old-route absence; task-209 owns the consolidated upstream review, live aggregate success, expected Daedalus identity equality, and production unavailable classification.
- `research/04-exact-cbor-era-coverage.md` supplies the accepted Conway conditional-readiness and Dijkstra blocked status relevant to `available_eras`; it does not authorize backend or product support by itself.
- `research/02-cip30-wire-contract-evidence.md` and checked-in task-002 contract fixtures remain the public-error boundary; task-200 maps backend failures but must not change those public values.
- Live repository verification confirmed the research baseline: no dApp route exists; `Api` composes reusable Servant groups; `Network` already carries runtime network/era state; `ApiLayer.netParams` contains genesis parameters; `Application.Version` supplies the embedded revision; `ApiErrorInfo` and `apiError` are the existing error seam; the live API unit modules are `TypesSpec.hs` and `ServerSpec.hs`; and `TypesSpec` checks JSON goldens and Swagger error coverage.

## Docs, Workflows, And Skills

- Read in the required order: `.agent/readme.md`, `.agent/system/architecture.md`, the PRD, task tracker, relevant accepted research, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, `.agent/workflows/nix.md`, and `.agent/workflows/build.md`.
- IPC workflow was consulted to confirm there is no Electron IPC work in task-200 and that sensitive backend payloads must not drift into logging. Its current authenticated Electron patterns are not a backend HTTP implementation template.
- The Nix/build workflows apply only to compiling/bundling the unavailable candidate and proving its packaged identity without a tracked pin change. The task-200 candidate is not expected to serve successful capability JSON. The test workflow's Daedalus Jest/Cucumber commands are not substitutes for the sibling Haskell tests.
- Sibling guidance consulted before sibling analysis: `../cardano-wallet/README.md`, `CONTRIBUTING.md`, contributor manual, testing guide, and Swagger development guide. Follow focused Cabal tests, formatting checks, and OpenAPI validation; avoid drive-by formatting. The sibling flake cannot currently provide local-cluster execution because it references a missing Nix store source.
- `understand` was loaded before nontrivial repository exploration. Generating its graph would violate the one-file planning constraint, so all material findings were verified directly in live Daedalus and sibling files.
- No Cardano CLI, protocol-parameter, encoding, frontend, Storybook, i18n, or hardware skill is applicable to this API-contract task.

## Fixed Contract Decisions

### Capability Endpoint

- Route: `GET /v2/dapp-capabilities`, with no request body and no query parameters.
- Response object fields are exactly `api_version`, `backend_build`, `network`, and `capabilities`; every object rejects unknown fields.
- `api_version` is exactly integer `1`.
- `backend_build.version` is diagnostic text. `backend_build.source_revision` is exactly 40 lowercase hexadecimal characters from the built binary and is the identity Daedalus will compare to its exact expected pin.
- `network.network_id` is `0` or `1`; `network_magic` is a JSON integer in `Word32`; `genesis_hash` is exactly 64 lowercase hexadecimal characters; `current_era` uses the existing lowercase API era spelling.
- A successful Version 1 document has exactly these required unique capability entries at revision `1`: `transaction-context`, `reviewed-context-signing`, `cip8-cip95`, and `durable-wallet-submit`. Initial `available_eras` is the unique nonempty list `['conway']` for each.
- Capability names are nonempty lowercase kebab case. Revisions are positive JavaScript-safe integers. Era lists are nonempty and unique.
- Future unknown capability names may be retained/ignored by a consumer only after the entire response validates. Duplicate known or unknown names, duplicate eras, partial required sets, old/zero revisions, malformed source identity, out-of-domain network fields, and unknown object fields invalidate the whole response. Equality with Daedalus's expected source revision and configured network/genesis is not a `FromJSON` concern and belongs to task-209.
- Do not infer capability from `backend_build.version`, a successful unrelated endpoint, or route probing.
- The task-200 candidate mounts the typed route but its handler always emits the same status, headers, and body as the server's ordinary unmatched-route `404`. It has no activation input. This preserves the already-frozen old-backend unavailable behavior and prevents a standalone successful V1 response.
- The production response constructor is intentionally real code for the aggregate endpoint, not a consumer identity validator: it accepts only producer-owned runtime authorities and produces the complete set atomically. Unit tests may call it directly, but task-200 HTTP integration must never expose its result.
- Tasks 201-208 add the actual families without changing the unavailable response. Task-209, after proving all four family bindings in the exact final sibling range, makes the one reviewed handler change from route-not-found to the complete constructor result and tests live success. No partial list, `available` flag, placeholder operation, or per-family activation is permitted.

### Era And Availability Decision

- V1 success is Conway-only. The response constructor matches the internal `Read.Era` value before any `ApiEra` conversion and constructs `ApiConway` only for `Read.Conway`; it returns unavailable for every other era.
- `Read.Dijkstra` deterministically follows the exact same ordinary unmatched-route `404` response. It is never converted with the currently partial `ApiEra.fromReadEra`, never serialized as Conway, and never added to public `ApiEra` or `available_eras` by this task.
- Unit and server tests must exercise Dijkstra explicitly and compare the full status/headers/body to an actually unmatched route. No new backend error tag is needed. Task-200 records the confirmed consolidated delivery boundary in the PRD, research 03, and tracker now; task-209 later records the final complete reviewed range and activation/live-success evidence.

### Error And Privacy Contract

- Add one exhaustive mapping helper for the accepted revision-1 backend pairs only:

| HTTP | Backend tag                   | Fixed message                            |
| ---: | ----------------------------- | ---------------------------------------- |
|  400 | `dapp_invalid_request`        | `Invalid backend request`                |
|  400 | `dapp_context_conflict`       | `Backend context conflict`               |
|  400 | `dapp_identity_conflict`      | `Submission identity conflict`           |
|  409 | `dapp_account_changed`        | `Wallet or network changed`              |
|  503 | `dapp_context_unavailable`    | `Wallet context unavailable`             |
|  500 | `dapp_internal_error`         | `Backend operation failed`               |
|  403 | `dapp_tx_proof_generation`    | `Transaction proof unavailable`          |
|  403 | `dapp_deprecated_certificate` | `Deprecated certificate`                 |
|  403 | `dapp_data_proof_generation`  | `Data proof unavailable`                 |
|  403 | `dapp_data_address_not_pk`    | `Address is not a public-key credential` |
|  409 | `dapp_submission_failed`      | `Transaction submission failed`          |
|  503 | `dapp_submission_unavailable` | `Transaction submission unavailable`     |

- The helper must not accept or append arbitrary exception text. Tests use sentinel transactions, addresses, payloads, keys, signatures, passphrases, context tokens/digests, origins, derivation paths, URLs, and database text and prove none appears in encoded HTTP bodies.
- User refusal remains Daedalus-main-owned and is not a backend error tag.

## Expected Files

### Sibling `../cardano-wallet`

- `lib/api/src/Cardano/Wallet/Api.hs`: add the route to the existing `Network` group and export its Servant alias.
- `lib/api/src/Cardano/Wallet/Api/Types/Dapp.hs` (preferred focused module) and `lib/api/src/Cardano/Wallet/Api/Types.hs`: define/re-export the strict versioned capability types, closed parser, complete response constructor, and non-Conway unavailability result; keep the giant umbrella module from absorbing all implementation detail.
- `lib/api/src/Cardano/Wallet/Api/Types/Error.hs` and `lib/api/src/Cardano/Wallet/Api/Http/Server/Error.hs` or one focused dApp error module: add exact tags and a fixed-message/status conversion seam without arbitrary detail leakage.
- `lib/api/src/Cardano/Wallet/Api/Http/Server/Handlers/DappCapabilities.hs` and `lib/api/src/Cardano/Wallet/Api/Http/Server.hs`: add the producer constructor inputs and compose an always-unavailable handler through `Network`; ensure Dijkstra/non-Conway selection occurs before `ApiEra.fromReadEra`.
- `lib/application/shelley/Cardano/Wallet/Application.hs`: pass the embedded `Application.Version` build identity toward the production constructor without introducing an API-to-application package cycle; this must not add an activation input.
- `lib/api/src/Cardano/Wallet/Api/Clients/Network.hs`, `lib/api/src/Cardano/Wallet/Api/Client.hs`, and `lib/api/src/Cardano/Wallet/Api/Link.hs`: preserve existing generated client decomposition and add the typed capability client/link only where compilation and tests require it.
- `lib/api/cardano-wallet-api.cabal`: register any focused new modules and only required dependencies.
- `specifications/api/swagger.yaml`: document the exact route, conditional `200` response schemas, ordinary `404` unavailable response, required/closed objects, constraints, examples, and dApp error anchors without documenting unimplemented operation routes.
- `lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs`, `lib/unit/test/unit/Cardano/Wallet/Api/ServerSpec.hs`, and focused new specs if separation is clearer: strict parser/encoder, complete constructor, unavailable route, Dijkstra, runtime producer fields, fixed-error, redaction, Swagger correspondence, and regression tests. There is no live `ApiSpec.hs`.
- `lib/unit/test/data/Cardano/Wallet/Api/ApiDappCapabilities.json` and any minimal focused golden fixtures: one canonical valid response; invalid cases stay table-driven in tests rather than multiplying files.
- `lib/integration/scenarios/Test/Integration/Scenario/API/Network.hs` or a focused dApp capability scenario registered in the live integration component: compile the unavailable-route scenario now; task-209 runs it over real HTTP/mTLS against the complete range and adds aggregate-success coverage.

Expected-file names may narrow to existing modules during implementation, but adding any other route, persistence, Daedalus product code, or broad refactor requires plan review. No database migration file is expected.

### Daedalus Completion Records

- `.agent/plans/dapp-browser-cip30/task-plans/task-200.md`: exact local candidate hash/base, rollback statement, verification limits, internal `@Reviewer` decision, superseded handoff, and truthful outcome.
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`: after internal implementation approval, record task-200 complete with the route unavailable, local-cluster/TLS execution deferred, unchanged pin, and task-209 ownership of the consolidated upstream review, aggregate activation, real HTTP/mTLS validation, consumer checks, and pinning.
- Task-200 synchronizes the governing PRD backend-delivery wording, research 03 delivery/evidence sequencing, and tracker task-209 ownership with the confirmed consolidated delivery boundary now. Task-209 later records the final complete reviewed range, aggregate activation, and resulting integration/pin evidence.
- No Daedalus implementation, manifest, lockfile, pin file, or review log changes are task-200 completion requirements.

## Approach

1. Reconfirm baselines and isolate unrelated work.
   - Record Daedalus pin/tag and sibling branch/candidate/upstream relation: `b2d20f4385bfcb92454b4dec91f954a0babd13ac` on `6761259ee91b138921231ce7fd1198679abfcc82`.
   - Preserve unrelated sibling `.idea/` and any concurrent changes; branch from the upstream-aligned baseline without rewriting history.
   - Reconfirm the old pinned revision returns route-not-found for `/v2/dapp-capabilities` and has no dApp persisted state. Record absence only; task-200 does not implement Daedalus's production classification of that result.
2. Implement strict focused types first.
   - Use smart constructors/custom Aeson parsing for exact keys, bounds, regex/hex constraints, uniqueness, exact required capability set, and revision-1 semantics. Generic Aeson defaults alone are insufficient because they accept unknown fields.
   - Keep shape/value-domain validation distinct from consumer policy. Do not add expected revision, expected network, or expected genesis arguments to parsing code.
   - Reuse `ApiEra`, `NetworkId`, `ProtocolMagic`, genesis hash, and existing text/hex conventions where they preserve the frozen wire exactly; do not create alternate spellings or add Dijkstra to public `ApiEra`.
3. Add the additive route through existing seams.
   - Extend the `Network` Servant group, generated network client/link, and server tuple without changing existing route behavior or order-visible public paths.
   - Keep the task-200 HTTP handler unconditionally on ordinary `err404`, with no environment/configuration/argument escape hatch, and prove its full response equals an actually unmatched route.
   - Build the dormant complete response from the same runtime network ID, protocol magic, genesis parameters, and current-node-era authority already used by network information. Inject diagnostic version and exact embedded git revision from the application layer so the API package does not depend upward on the application package.
   - Pattern-match internal era first. Construct the V1 response only for Conway; return unavailable for Dijkstra and every non-Conway era without invoking `ApiEra.fromReadEra`.
4. Add fixed errors and Swagger.
   - Add the twelve accepted tags to the existing error schema and a closed mapping to exact status/message pairs.
   - Keep arbitrary diagnostics behind a static local category; never serialize supplied exception/context text.
   - Add only `/dapp-capabilities` and reusable schemas/error definitions to Swagger. Document the complete successful contract and its aggregate/Conway availability precondition without claiming the task-200 candidate or downstream routes are live.
5. Add focused evidence.
   - Golden/roundtrip one valid response.
   - Table-test unknown fields at every object level, malformed/uppercase hashes, out-of-range IDs/magic/revisions, duplicate capabilities/eras, missing/partial required set, old revision, and future-valid unknown capability handling. Do not test expected-pin or expected-network equality in task-200.
   - Unit-test the complete constructor with producer-owned Conway network magic/genesis/era and embedded candidate source revision; separately test that Dijkstra and every non-Conway era produce unavailable before API-era conversion.
   - Test exact bare-`err404` handler equality and all fixed status/tag/message bodies with sensitive sentinels absent. The compiled local-cluster scenario expects `404`, but task-209 owns its real HTTP/mTLS execution and later aggregate-success assertions.
6. Verify persistence and rollback explicitly.
   - Confirm the diff contains no DB/store/schema/migration changes and creates no persisted record. Therefore the task acceptance criterion for every new persisted record is satisfied by having none.
   - Record pin rollback compatibility: a database used by the candidate is unchanged and can be reopened by the current pin; both the candidate and rollback pin expose the same route-not-found behavior. No restore step is required for task-200.
   - If implementation discovers persistence is necessary, stop and return to planning; do not improvise a migration in this task.
7. Complete cross-repository candidate sequencing.
   - Self-review the sibling diff, run the focused/full task-200 checks, and retain candidate `b2d20f4385bfcb92454b4dec91f954a0babd13ac` as the first reviewable local commit in the phase-2 range.
   - Build exact `.#daedalus-bridge-mainnet` and `.#daedalus-mainnet` outputs through a temporary exact-revision override and verify the bundled candidate revision. Do not edit `flake.nix`/`flake.lock`.
   - Accept compiled local-cluster scenario coverage plus exact bare-`err404` handler equality as sufficient task-200 endpoint evidence because the sibling flake's missing Nix store source blocks real local HTTP/mTLS execution. Record the blocker; do not claim runtime passage.
   - Tasks 201-208 add local reviewable commits. Task-209 opens the single consolidated upstream PR, obtains authorized review of the complete range, owns any review fixes and final activation, runs the real aggregate local HTTP/mTLS and full-range validation, then updates the exact pin and reruns post-pin verification.
8. Perform exact combined review.
   - Internal `@Reviewer` examines the exact candidate/base, strict producer and error tests, compiled integration scenario, exact bare-`err404` proof, unavailable/Dijkstra evidence, no-persistence rollback evidence, exact Daedalus bridge/mainnet builds, the unresolved sibling-flake blocker, and this canonical outcome together.
   - Task-200 may complete when that review has no unresolved blocker. The later task-209 combined review examines the consolidated upstream-reviewed sibling range, activation, successful real aggregate HTTP/mTLS response, Daedalus expected-identity/unavailable checks, clients, exact `flake.nix`/`flake.lock` pin, and post-pin evidence. Neither review may treat task-200 alone as aggregate backend readiness.

## Acceptance Criteria

- The exact additive `GET /v2/dapp-capabilities` V1 success contract is represented consistently in Servant, strict Haskell JSON types, Swagger, client/link seams, and goldens, while task-200 proves the exact ordinary route-not-found response through handler equality and a compiled integration scenario rather than claiming runtime HTTP execution.
- Older bundled backends fail closed because the endpoint is absent. The task-200 candidate is equally unavailable and has no activation escape hatch. Malformed, unknown-field, duplicate, partial, old, and value-domain-invalid documents are rejected by strict parsing without fallback inference or endpoint probing.
- The production constructor can produce only the exact four revision-1 capability names with Conway availability and producer-owned binary source revision plus runtime network ID/magic/genesis/current era. It is not reachable as HTTP success until task-209's reviewed aggregate activation.
- Dijkstra and every non-Conway era deterministically return the same status/headers/body as an unmatched route, without calling partial `ApiEra.fromReadEra`, advertising Dijkstra, or widening public `ApiEra`.
- Existing endpoints, generated clients, and Daedalus callers remain additive and behavior-compatible; focused API regression tests pass.
- All twelve accepted HTTP/tag pairs and fixed messages match research 03 and Swagger. Tests prove sensitive sentinel values are absent from error bodies.
- No persisted backend record or database migration is introduced. Evidence states that the old pin can reopen unchanged databases, rollback makes the endpoint absent, and Daedalus remains feature-disabled; if this premise changes, the task is not accepted without a revised versioned atomic migration/restore plan.
- Concrete local candidate `b2d20f4385bfcb92454b4dec91f954a0babd13ac` on upstream `6761259ee91b138921231ce7fd1198679abfcc82` has passing task-200 verification and no unresolved internal `@Reviewer` condition. No task-200 upstream identity, URL, approval, or PR is required.
- Temporary exact-revision Daedalus Nix integration builds `.#daedalus-bridge-mainnet` and `.#daedalus-mainnet` and proves bundled build identity before any pin update. The tracked Daedalus pin remains unchanged, the endpoint remains unavailable, and the candidate is explicitly non-pin-eligible.
- Compiled local-cluster integration coverage and exact bare-`err404` handler equality are sufficient for task-200 completion alongside clean Cabal tests, OpenAPI validation, and exact Daedalus builds. Real local HTTP/mTLS success is not claimed and is mandatory in task-209 against the complete range.
- Current records assign task-200 strict shape/value domains, producer runtime fields, fixed errors, Dijkstra/unavailable behavior, and old-route absence; task-209 owns the consolidated upstream PR/review, expected Daedalus source/network equality, live complete-set success, unavailable classification, and pinning.
- Exact internal combined review covers the local sibling commit, rollback/no-migration evidence, verification and blocker, Daedalus candidate builds, and canonical task outcome without claiming downstream backend behavior.

## Verification

### Agent-Executable Sibling Checks

- Run focused unit tests in `nix develop`, including matches for `ApiDappCapabilities`, API JSON goldens/Swagger error correspondence, strict value domains, complete producer construction, API routing, exact unmatched-route equivalence, Dijkstra/non-Conway behavior, and redacted dApp errors, using the live `just unit-tests-cabal-match '<match>'` or the equivalent exact Cabal component invocation.
- Compile the focused `DAPP_CAPABILITIES` local-cluster scenario. Runtime execution is not a task-200 completion gate while blocked by a missing sibling-flake Nix store source; task-209 must run real local HTTP/mTLS against the complete range.
- Run `openapi-spec-validator --schema 3.0.0 specifications/api/swagger.yaml` and `scripts/ci/validate-swagger.sh` where its pinned environment is available.
- Run `cabal build cardano-wallet-api cardano-wallet-application cardano-wallet-unit cardano-wallet-integration -O0 -v0 --ghc-options='-Werror'` or the repository's equivalent `just build` targets.
- Run `just check-fmt`, focused `hlint` for touched Haskell modules, and `git diff --check`. Use `just fmt` only with careful diff inspection to avoid drive-by formatting.
- Inspect `git diff --name-only` and migration/store/schema paths to prove no persistence change, and compare API path behavior at the old pin and candidate.

### Agent-Executable Daedalus Candidate Integration

- Without modifying the tracked pin, use a temporary clean Daedalus worktree or exact-revision Nix override to build `.#daedalus-bridge-mainnet` and `.#daedalus-mainnet` against the exact candidate.
- Verify both bundled outputs contain the exact candidate source revision. Do not require or claim task-200 launcher/TLS startup or HTTP execution; task-209 owns that full-range proof.
- Verify from the currently pinned backend source/API definition that it has no route. Do not claim pinned-backend runtime HTTP execution or add Daedalus feature-unavailable classification; task-209 owns that production consumer behavior.
- Record commands, hashes, candidate revision, and normalized results without credentials, TLS key material, wallet data, full paths from user environments, or sensitive response data.

### Internal Combined Validation

- `@Reviewer` reviews the exact candidate/base and task-200 evidence together, including endpoint/schema strictness, producer identity sources, complete four-capability meanings, exact bare-`err404` and Dijkstra behavior, error/privacy mapping, additive compatibility, no-persistence rollback, compiled integration scenario, exact Daedalus builds, and the explicit local-cluster/TLS blocker.
- Task-200 can complete after internal approval because it remains unavailable and non-pin-eligible. Authorized upstream identity, durable PR URL, exact full-range approval, real aggregate HTTP/mTLS execution, and all full-range review conditions are task-209 inputs.

## Risks And Questions

- Advertising the four V1 names before tasks 201-208 are complete would be false. Mitigation: the task-200 HTTP handler is unconditionally identical to an unmatched route; only task-209 may activate the complete document after proving all four operation bindings.
- Generic Aeson derivation accepts unknown fields and can silently loosen the contract. Use closed custom decoding and adversarial tests.
- Build revision injection can report zeros or a worktree revision if wired at the wrong package layer. Test producer construction under normal Cabal development behavior and the Nix-injected exact candidate identity. Task-209 separately compares the packaged value to its expected pin.
- Current era/network/genesis values must come from one configured backend authority, not Swagger constants or renderer input. Reuse the network layer and genesis parameters and assert produced values; expected Daedalus equality tests belong to task-209.
- Adding dApp tags to the generic error type can accidentally permit arbitrary messages. Route all dApp errors through the fixed mapping and inspect traces/body fixtures for sentinel leakage.
- The accepted contract requires a complete capability set, while operation implementations arrive later. Any proposal to return partial sets, an `available` boolean, or placeholder operation routes is wire drift and requires task-003/PRD review rather than local convenience.
- The live `ApiEra.fromReadEra` throws for Dijkstra. The capability seam must branch on internal era before conversion and reuse exact route-not-found behavior; widening `ApiEra`, reporting Conway for Dijkstra, or introducing an unfrozen error is prohibited.
- The focused types/error-helper module placement is an internal review concern only; it may not change the frozen wire or increase scope.
- Local-cluster/TLS execution is blocked by the missing sibling-flake Nix store source. This does not weaken task-200's unavailable handler proof, but task-209 cannot activate or pin until real aggregate HTTP/mTLS and full-range validation pass.

## Docs, Tracking, And Research Updates

- After internal implementation review approval, update the task tracker status/completion notes with the exact local candidate/base, no-migration rollback result, unavailable behavior, unresolved local-cluster/TLS environment blocker, task-209 consolidated upstream-review/activation/consumer ownership, and unchanged Daedalus pin.
- Record the confirmed consolidated delivery boundary now in the PRD, research 03, and tracker: tasks 200-208 accumulate local reviewable commits, and task-209 owns the single consolidated upstream PR/review, activation, full-range validation, and pin. Task-209 later adds the exact complete reviewed range and activation evidence; `.agent/system/api-endpoints.md` is updated when the endpoint is actually activated.
- Update this plan with final verification and outcome.
- Do not update architecture, IPC, Nix/build workflow text, public CIP-30 fixtures, package manifests outside the sibling Cabal module list, or Daedalus product code unless implementation proves a concrete stale statement; any such addition requires focused review.

## Review Logs

- Planning review log: `.agent/plans/dapp-browser-cip30/task-plans/task-200-plan-review.md`
- Implementation review log: `.agent/plans/dapp-browser-cip30/task-plans/task-200-impl-review.md`
- Both logs are Orchestrator-owned append-only historical records and must not be rewritten. The implementation log's prior upstream handoff is explicitly superseded by the current canonical task-209 consolidated-review process; no planning-log transition or append entry is valid for this revision.

## Lifecycle Status

- Planning status: `approved`
- Build status: `completed`
- Current outcome: internal `@Reviewer` approval found no blockers for candidate `b2d20f4385bfcb92454b4dec91f954a0babd13ac`, rebased onto sibling upstream `6761259ee91b138921231ce7fd1198679abfcc82`. The unavailable-before-complete contract, strict parsing, fixed privacy-safe errors, Conway-only constructor, deterministic Dijkstra refusal, clients/links/Swagger, and additive server wiring are approved. Clean Cabal builds, focused unit/golden/schema tests, exact bare-`err404` handler proof, compiled integration scenario, OpenAPI validation, formatting checks, and exact `.#daedalus-bridge-mainnet`/`.#daedalus-mainnet` builds pass with the embedded rebased revision. No persistence path or tracked Daedalus pin changed. Local-cluster/TLS execution did not pass and remains blocked by a missing sibling-flake Nix store source; task-209 owns real aggregate HTTP/mTLS validation before activation and pinning.
- Superseded handoff: the implementation log's task-200 upstream-review request is historical only. Task-200 neither requires nor opens an upstream PR; task-209 owns the single consolidated upstream PR/review for the complete task-200-through-task-208 range.
- Completion rule: task-200 may complete after internal `@Reviewer` combined approval of the exact candidate and truthful evidence above. It remains endpoint-unavailable, non-pin-eligible, and unable to establish aggregate readiness. Task-209 must obtain authorized full-range upstream approval, run real aggregate local HTTP/mTLS and full-range validation, activate the complete capability response, update the pin, and pass post-pin verification.

## Planner Self-Review

- Scope creep: the plan adds one unavailable capability route, focused strict types/producer constructor, fixed errors, tests, and required contract documentation. It excludes all context/signing/CIP-8/submission implementation, persistence, Daedalus clients, IPC, UI, hardware, and pin changes.
- Workflow freshness: sibling Haskell/Nix/Swagger commands come from live sibling guidance; Daedalus workflows are used only for the temporary candidate integration and documentation pass.
- Manifests/tests/docs: the sibling Cabal module list, JSON golden, Swagger route/schemas/errors, live `TypesSpec.hs`/`ServerSpec.hs`, compiled integration scenario, current governing-document synchronization, and task-209 final evidence ownership are named. No nonexistent `ApiSpec.hs`, frontend package, or translation manifest is referenced.
- Security and wire drift: closed JSON objects, producer-owned runtime identity, complete-set semantics, exact unavailable response, Conway-only success, deterministic Dijkstra refusal, exact statuses/tags/messages, redaction, no endpoint probing, and non-pin-eligible sequencing preserve the accepted contract.
- Backend ownership: runtime network/genesis/era/build data remains backend-owned; no renderer input or reduced wallet state is introduced.
- Migration/rollback: no persistence is the smallest solution and is explicitly verified. Any persisted record forces replanning rather than a hidden migration.
- Plan consistency: task-200 internally reviews and may complete an unavailable local foundation without an upstream PR. Tasks 201-208 accumulate reviewable local commits; task-209 alone owns their consolidated upstream review, complete aggregate activation, expected Daedalus identity/unavailable validation, real HTTP/mTLS/full-range proof, pinning, and post-pin verification.
