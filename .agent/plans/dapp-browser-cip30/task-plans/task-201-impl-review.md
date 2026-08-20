Implementation: Iteration 1
Timestamp: 2026-08-19T19:13:46Z

Changes made:
- Synchronized research 03 with the approved revision-1 schema, overlay equations, byte authority, clocks, and error boundary.
- Added exact-point LSQ targeting with fail-closed acquisition results while preserving existing volatile-tip retries.
- Added one composite exact-point era/protocol-parameter/full-UTxO query to the network layer.
- Added process-memory per-wallet context clocks and a shared database gate with commit-then-increment operations; tagged current restore, rollback, submission, and selected prologue mutation paths.
- Added strict revision-1 API value types plus record encoding, Blake2b-256 digest, HMAC token validation, and deterministic goldens.

Files touched:
- Daedalus: `research/03-cardano-wallet-backend-contract.md` and task-201 planning/review records.
- cardano-wallet: API context core/test/manifest; network interface, LSQ, and dummy layer; wallet DB interface/factory/pure layer; current wallet mutation call sites.

Verification run:
- `cabal test cardano-wallet-api:dapp-context --offline -O0`: passed, 4 examples.
- `cabal build cardano-wallet-network-layer --offline`: passed.
- `cabal build lib:cardano-wallet --offline -O0`: passed with pre-existing dynamic-link warnings.
- `git diff --check` passed in both repositories.
- `nix develop --command cabal build cardano-wallet-network` remains blocked by missing Nix store source `/nix/store/l3sqyxfc4wb237j8gaxv1brq8naq6rja-source`. Direct offline Cabal builds remain usable.

Deviations from approved plan:
- Route/server/Swagger/client integration is not yet implemented. The pinned decoder validates a transaction but exposes decoded outputs only, not each original pending output span required by the approved byte-authority contract. No canonical-reserialization fallback was added.
- No sibling candidate commit or Daedalus fixture has been created because the architecture decision below blocks truthful completion.

User interaction required: yes. Work is blocked on ownership of the exact-output-span decoder seam.

User Handoff:
Why interaction is required now: `cardano-wallet` pins `cardano-foundation/cardano-ledger-read` at `242c5c856f51aebe17f2d39b2320df6de1c05293`. Its `deserializeTx` returns only the ledger transaction, and `getEraOutputs` returns decoded values. The original per-output CBOR spans required for pending source bytes are unavailable. Adding an independent approximate parser would violate the approved contract.

Decision requested:
1. Authorize a third-repository `cardano-ledger-read` change that returns a ledger-validated Conway transaction together with original ordinary-output byte spans, then update the cardano-wallet source pin to its reviewed commit. Recommended.
2. Authorize a local cardano-wallet CBOR span parser paired with the ledger decoder. This is narrower operationally but creates duplicate parsing logic and needs differential/fuzz proof before use.
3. Revise the contract to canonical output bytes only. This weakens the approved exact-source conflict rule and is not recommended.

Expected result: return option `1`, `2`, or `3`, plus any repository/pinning constraint. No credentials or runtime data are needed.

Work status: blocked pending this architecture decision; no review dispatch occurs until implementation can continue.

Outcome: Partial implementation is compile-checked and fail-closed; task-201 remains in progress pending user direction.

Implementation: Iteration 2
Timestamp: 2026-08-19T23:45:00Z

Changes made:
- Added and reviewed `cardano-ledger-read` exact Conway ordinary-output source-span support through `ffc13193454d5260a6ae8735a9d85e00ae49e8e7`; term headers are retained and every span is checked against the ledger-decoded value.
- Completed the Shelley-only transaction-context route, exact-point composite query, pending overlay and conflict rules, canonical records/digest/token, fixed error normalization, and sensitive-route logging suppression.
- Bound context clocks to wallet incarnations, made deletion marking idempotent, and moved deletion marking before worker shutdown so stale handlers fail closed with `dapp_account_changed`.
- Updated the canonical task, tracker, and backend-contract research with candidate ranges, evidence, and unresolved completion gates.

Files touched:
- Daedalus: task-201 plan, tracker, implementation review, and backend-contract research.
- cardano-ledger-read: Conway CBOR transaction decoder and unit tests.
- cardano-wallet: API types/route/server/errors/tests, network LSQ/interface, wallet DB clocks/incarnations/tombstones, logging middleware, Swagger, and the exact decoder source pin.

Verification run:
- `cabal test cardano-ledger-read:unit-tests -O0`: passed, 51 examples.
- `cabal test cardano-wallet-api:dapp-context -O0`: passed, 4 examples.
- `cabal test wai-middleware-logging:unit -O0`: passed, 10 examples.
- Focused transaction-context route and Swagger tests: passed, 18 examples.
- API and application library builds, Swagger validation, Fourmolu checks, Word64 boundary probe, and `git diff --check`: passed.

Implementation review:
- Final internal reviewer reported no remaining correctness or security findings and conditionally approved `cardano-ledger-read` through `ffc13193454d5260a6ae8735a9d85e00ae49e8e7` and `cardano-wallet` through `10175bcacf`.
- Approval is conditional on publication and acceptance evidence; it is not task completion or pin eligibility.

Deviations from approved plan:
- The exact decoder commit is local-only and absent from advertised refs at the declared GitHub source. No push was performed because publication was not authorized.
- Clean exact-pin build/test, deterministic deletion/generation race tests, exact-candidate Conway HTTP/mTLS success and controlled exact-point failure, old-pin compatibility proof, and final external fixtures remain open.
- Capability publication remains the exact ordinary `404`; Daedalus has not pinned or activated the candidate.

User interaction required: yes. Publishing the decoder commit or supplying a reviewed reachable remote reference requires user authorization.

Outcome: Code is conditionally approved, but task-201 remains in progress and uncommitted in Daedalus until publication and acceptance gates pass.

Code Review: Iteration 2
Timestamp: 2026-08-19T21:30:27Z

Historical sequencing defect:
- The log incorrectly placed `Implementation: Iteration 2` directly after `Implementation: Iteration 1`, without `Code Review: Iteration 1`. Per the recovery rule in `prompt.md`, this entry reviews Iteration 2 as the current final implementation and does not rewrite prior history.

Blocking findings:
- High: Exact-point LSQ can wait indefinitely after a node connection loss, violating the bounded three-attempt and fixed-`503` contract. `localStateQuery` retains the queued command and retries it after reconnection, while `send` has no request-level timeout or disconnect result. Consequently, `resolveTransactionContext` only decrements its retry count after `getDappTransactionContext` returns. A disconnected or unavailable node can therefore hang the HTTP request and block the shared LSQ queue instead of returning `503 dapp_context_unavailable`. Exact-point commands must receive a bounded disconnect/cancellation failure without changing existing volatile-tip retry behavior.

Acceptance evidence still missing:
- Decoder commit publication at the declared source and a clean exact-pin build.
- Deterministic deletion/incarnation/generation race and retry-exhaustion tests.
- Real Conway HTTP/mTLS success and controlled exact-point failure evidence.
- Old-pin schema/reopen compatibility proof.
- Final ledger-derived and cross-language fixtures.

Non-blocking observations:
- The reviewed decoder preserves complete output term bytes and validates each captured span against the ledger-decoded output.
- Snapshot assembly, source-byte conflict handling, deletion tombstones/incarnations, fixed route errors, Swagger shape, and sensitive-route logging suppression otherwise match the reviewed contract in the inspected ranges.

Outcome: The final local candidate has one unresolved correctness/availability defect and lacks the explicitly required acceptance evidence. Task-201 remains in progress and is not pin-eligible.

Decision: requires_changes

Implementation: Iteration 3
Timestamp: 2026-08-19T21:48:43Z

Changes made:
- Bounded each exact-point transaction-context query attempt to 30 seconds, so the existing three-attempt wallet policy now terminates with the fixed unavailable error instead of waiting indefinitely.
- Added cancellation state only to exact-point LSQ commands. Timed-out commands are skipped when the LSQ client next consumes the queue; volatile-tip commands retain their existing reconnect retry behavior.
- Added a focused network-layer unit test proving a cancelled exact-point command is discarded before the next live command is acquired.

Files touched:
- `lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs`
- `lib/network-layer/src/Cardano/Wallet/Network/Implementation/Ouroboros.hs`
- `lib/network-layer/test/Cardano/Wallet/Network/Implementation/OuroborosSpec.hs`
- `lib/network-layer/cardano-wallet-network-layer.cabal`

Verification run:
- `cabal test cardano-wallet-network-layer:unit -O0`: passed, 9 examples.
- Exact local decoder checkout build of `cardano-wallet-api` and `cardano-wallet-application:lib:cardano-wallet-application`: passed.
- `cabal test cardano-wallet-api:dapp-context -O0`: passed, 4 examples.
- Fourmolu check on all changed Haskell files and `git diff --check`: passed.
- Nix formatter-shell attempts remain blocked by missing store source paths; the existing Fourmolu store binary provided the formatter check.

Candidate:
- Follow-up commit `2b9e084713` (`fix(dapp): bound exact-point context queries`).
- The committed GitHub decoder location was restored after local exact-commit verification; no `cabal.project.local` remains.

Remaining completion gates:
- Decoder publication and clean remote exact-pin build.
- Deterministic deletion/incarnation/generation race and retry-exhaustion coverage.
- Real Conway HTTP/mTLS success and controlled exact-point failure evidence.
- Old-pin compatibility proof and final fixtures.

Outcome: The iteration-2 availability finding is fixed and locally verified; task-201 remains in review and is not pin-eligible.

Code Review: Iteration 3
Timestamp: 2026-08-19T21:52:38Z

Findings:
- High: The timeout bounds the caller but does not reliably cancel the queued or active exact-point command. The cancellation flag is written only after `race` returns, allowing `clientStIdle` to read `False` and start acquisition during that gap. Once acquisition starts, cancellation is never checked in the acquiring, acquired, or querying states. A timed-out command can therefore execute after reconnect or remain stuck awaiting a node response, blocking the shared LSQ queue and subsequent retries or volatile-tip queries. Cancellation must be published before timeout completion and active timed-out commands must be retired or force a bounded client reconnect.

Evidence assessment:
- The 30-second `race`, combined with the existing three-attempt loop, bounds the HTTP caller to approximately 90 seconds and produces the fixed unavailable result.
- Existing volatile-tip behavior is source-preserved: `SomeLSQ` is never considered cancelled and still retries failed volatile acquisition.
- The focused test passes but covers only a command already marked cancelled before `clientStIdle` consumes it. It does not cover timeout publication races, timeout after acquisition, stalled queries, reconnect behavior, queue drainage, or a following volatile-tip command, so it is insufficient for code approval.
- Decoder publication, clean exact-pin build, deterministic generation/deletion races, real Conway HTTP/mTLS evidence, old-pin proof, and final fixtures remain separate acceptance gates.

Outcome: Commit `2b9e084713` fixes bounded caller failure but does not fully resolve stale-command and shared-queue availability behavior. A follow-up implementation and deterministic race/reconnect test are required.

Decision: requires_changes

Implementation: Iteration 4
Timestamp: 2026-08-19T21:58:03Z

Changes made:
- Replaced shared-queue cancellation flags with one isolated node-to-client LSQ connection and queue per exact-point context attempt.
- Wrapped connection and query in structured nested races. Query completion cancels its isolated connection; the 30-second deadline cancels both connection and query before returning unavailable. The long-lived rewards and volatile-tip queue is never used by context attempts.
- Removed the ineffective cancellation field and pre-consumption-only test from commit `2b9e084713`.
- Added deterministic `runBoundedQuery` tests proving timeout cancels both isolated activities and query completion cancels the isolated client.

Verification run:
- `cabal test cardano-wallet-network-layer:unit -O0`: passed, 10 examples.
- Exact local decoder commit build of `cardano-wallet-api` and `cardano-wallet-application:lib:cardano-wallet-application`: passed.
- `cabal test cardano-wallet-api:dapp-context -O0`: passed, 4 examples.
- Fourmolu check on changed Haskell files and `git diff --check`: passed.
- The temporary local decoder URL was restored to the committed GitHub source; no local project override remains.

Tradeoff:
- Each context attempt performs a fresh node handshake. This keeps timeout ownership exact and cannot poison shared LSQ work; pooling is deferred unless measured dApp query throughput requires a dedicated cancellable client.

Remaining completion gates:
- Decoder publication and clean remote exact-pin build.
- Deterministic deletion/incarnation/generation race and route-level retry-exhaustion coverage.
- Real Conway HTTP/mTLS success and controlled exact-point failure evidence.
- Old-pin compatibility proof and final fixtures.

Outcome: The iteration-3 stale-command/shared-queue finding is structurally removed and deterministically checked; task-201 remains in review and is not pin-eligible.

Code Review: Iteration 4
Timestamp: 2026-08-19T22:01:05Z

Findings:
- No blocking correctness or security findings in the Iteration 4 diff.
- Non-blocking: exporting `runBoundedQuery` slightly widens the exposed implementation API solely for testing, but the helper is generic, preserves no authority, and introduces no material compatibility or security risk.

Evidence assessment:
- Each attempt creates a private queue and node connection; the persistent rewards/volatile queue remains used only by existing methods.
- `race timeout (race runClient query)` has structured cleanup. Timeout cancellation tears down the inner race and both isolated children; query completion cancels and waits for the isolated client. An already-acquired or stalled query therefore loses its socket and cannot remain queued or block persistent LSQ work.
- Retryable connection loss remains handled by existing retry handlers inside the isolated client, but only until the 30-second outer deadline. Non-retryable synchronous exceptions propagate to the route fixed internal-error boundary; request-side asynchronous cancellation propagates while structured cleanup closes the isolated work.
- Timeout maps to `ErrDappTransactionContextPointUnavailable`; the wallet-level three-attempt loop consequently terminates with fixed `503 dapp_context_unavailable`.
- The two focused tests deterministically prove timeout cleanup of both activities and query-result cleanup of the client. Source inspection establishes queue isolation; real node cleanup and HTTP behavior remain acceptance evidence rather than a code-review blocker.
- Publication, clean exact-pin build, deterministic generation/deletion and route retry-exhaustion coverage, real Conway HTTP/mTLS evidence, old-pin proof, and final fixtures remain separate completion gates.

Outcome: Iteration 4 resolves the stale-command and shared-queue availability finding. The current code is approved, while task-201 remains in progress and not pin-eligible until the separate completion gates pass.

Decision: approved

Implementation: Iteration 5
Timestamp: 2026-08-20T17:48:55Z

Changes made:
- Published and pinned the exact-output decoder at `44e0a32300ec6d6d03b7578b97b8374820802ba1`; upstream PR 20 is open.
- Added deterministic generation/deletion/recreation races, live Conway HTTP coverage, optional integration mTLS with full server validation, and a controlled relay-socket acquisition failure that exercises all three isolated attempts and fixed redacted `503`.
- Added final follow-up `cardano-wallet` commits through `3fa5fd446b4b8fc7a5eeaa81ba406c91cd32e170` without amendment.
- Added `backend-context-v1.json` and a TypeScript test independently reproducing all three record types, Blake2b-256 digest, token payload, and HMAC.
- Updated research, PRD, tracker, and canonical lifecycle state for final combined review without activating capabilities or changing the tracked backend pin.

Verification run:
- Fresh public decoder-pin Cabal builds and focused decoder/context/network/API/DB/logging/Swagger checks passed.
- Plain HTTP and mutually authenticated TLS `TASK_201` local-cluster runs passed; each proved a nonempty Conway success response and controlled `503 dapp_context_unavailable` after three isolated connection failures.
- Eight focused TLS tests passed, including wrong CA, IP mismatch, and required client authentication.
- Candidate mutation preserved SQLite schema hash `52d6d54033de3ce30c53dd05bc407fd6643ff9b4599004f4b2bf6e1a9de3fdfb`; old pin `724be55dc66cf67bc4427e8f1a9657a9d1d33d71` reopened the exact resulting DB and passed checkpoint/submission reads.
- Three Daedalus Jest suites passed 57 tests; direct Prettier and `git diff --check` passed for task fixtures.
- Temporary `git+file` override builds of `.#daedalus-bridge-mainnet` and `.#daedalus-mainnet` passed with embedded candidate revision `3fa5fd446b4b8fc7a5eeaa81ba406c91cd32e170`; no lock or pin changed.

Deviations and residual ownership:
- Determinate Nix lazy trees were disabled per command to avoid the environment's missing ephemeral source path; no Nix repository workaround remains.
- Task-209 still owns consolidated upstream review, complete capability activation, aggregate integration, migration review, tracked pin update, and post-pin verification.

Outcome: All task-201 implementation and acceptance gates are satisfied. The proposed lifecycle outcome is `completed`, pending one final combined internal review of the frozen candidate and Daedalus evidence.

Code Review: Iteration 5
Timestamp: 2026-08-20T19:57:27Z

Blocking findings:
- High: the resolver equations, derived roles, provenance merge, source/canonical separation, exact-byte conflict, and missing-source behavior were documented but lacked one executable focused check.
- High: the independent digest/token fixture did not include the pending `0x07` record in the digest preimage.
- High: the live Conway scenario used ordinary foreign outputs and did not prove node-resolved datum-hash, inline-datum, and reference-script source-byte fidelity across normal, collateral, and reference roles.

Non-blocking observations:
- No additional correctness, security, migration, or capability-activation defect was identified.

Approval bar:
- Add the three bounded evidence checks above, rerun plain HTTP and authenticated mTLS, refresh the frozen candidate and fixture hashes, and return the complete state for combined review.

Decision: requires_changes

Implementation: Iteration 6
Timestamp: 2026-08-20T19:57:27Z

Changes made:
- Extracted `contextSets` and added one focused wallet test covering the snapshot equations, requested pending-output exclusion, derived roles, equal-source provenance merge, source/canonical separation, exact-byte conflict, and missing-source failure.
- Included the pending `0x07` record in the independent digest preimage and refreshed the Haskell and TypeScript digest/token goldens.
- Reworked the live Conway scenario to submit and confirm one transaction containing a datum-hash output, inline-datum output, and native reference-script output; assigned those outputs to normal, collateral, and reference roles and compared each node-resolved source-output CBOR byte-for-byte.
- Committed the review fixes as `cardano-wallet` `3ca15553f96587f1f96688185165b2ede00e30b0`.

Verification run:
- The focused resolver/route suite passed 21 examples; the digest/token suite passed 4 examples; the independent TypeScript fixture test passed.
- The integration scenario component and executable built with Nix; Fourmolu, Cabal formatting, and `git diff --check` passed.
- The enriched plain HTTP run passed 1 example in 186.7 seconds; the enriched client-authenticated mTLS run passed 1 example in 173.5 seconds. Each observed the controlled fixed redacted `503` after three isolated acquisition failures.

Outcome: All Iteration 5 blocking findings are addressed. The candidate remains `review_pending` until final combined review approves the refreshed evidence.

Verification Addendum: Iteration 6
Timestamp: 2026-08-20T19:57:27Z

- Temporary `git+file` override builds of `.#daedalus-bridge-mainnet` and `.#daedalus-mainnet` passed against exact wallet revision `3ca15553f96587f1f96688185165b2ede00e30b0`; tracked `flake.nix` and `flake.lock` remained unchanged.
- The final three focused Daedalus suites passed 57 tests, both JSON artifacts parsed, and `git diff --check` passed.

Code Review: Iteration 6
Timestamp: 2026-08-20T20:00:00Z

Review scope:
- Inspected the complete canonical task-201 plan and append-only planning/implementation logs.
- Inspected the complete Daedalus task worktree, final cardano-wallet range `b2d20f4385bfcb92454b4dec91f954a0babd13ac..3ca15553f96587f1f96688185165b2ede00e30b0`, and decoder range `54f91a71874c50c14260de4eab6b2c2123e7d391..44e0a32300ec6d6d03b7578b97b8374820802ba1`.
- Verified that the executable resolver test covers the frozen A/S/Q/O equations, pending-output exclusion and requested inclusion, derived normal/collateral/reference roles, equal-source provenance merge, source/canonical separation, exact-byte conflict, and missing-source failure.
- Verified that both final Haskell and TypeScript digest constructions include pending record `0x07`.
- Verified that the live scenario confirms datum-hash, inline-datum, and native reference-script outputs on-node, assigns them to normal, collateral, and reference roles, and compares each returned node source-output CBOR byte-for-byte.
- Verified exact-point composite authority, relevant Shelley mutation generations/incarnations/deletion gates, isolated bounded retries, pending/volatile/protocol context, provenance/conflicts, exhaustive fixed errors and logging suppression, no schema/persisted context additions, unchanged capability handler and Daedalus pin, and preserved task-202/task-209 ownership.

Blocking findings:
- High: `source/common/cardano/fixtures/exact-cbor/backend-context-v1.json:5` declares source candidate `3fa5fd446b4b8fc7a5eeaa81ba406c91cd32e170`, but its pending-bound digest and token come from final wallet commit `3ca15553f96587f1f96688185165b2ede00e30b0`. At the declared commit, the Haskell digest omitted pending record `0x07` and had different digest/token goldens. This conflicts with the final-source research claim, and the TypeScript test does not check provenance metadata. Update the fixture source commit, then rerun the focused fixture, SHA-256, JSON, and formatting checks.

Non-blocking observations:
- The three Iteration 5 functional evidence gaps are closed in the final wallet source.
- No other correctness, security, migration, capability-activation, tracked-pin, old-pin compatibility, or task-202/task-209 boundary issue was identified.
- The supplied focused, integration, mTLS, formatting, Daedalus, and temporary override build results are consistent with the inspected implementation.

Approval bar:
- Make the committed fixture provenance identify `3ca15553f96587f1f96688185165b2ede00e30b0`, verify its recorded SHA-256 remains truthful, and rerun the focused Daedalus fixture checks. No code change or broader re-review is required unless those bytes change.

Decision: requires_changes

Implementation: Iteration 7
Timestamp: 2026-08-20T20:00:00Z

Changes made:
- Corrected the fixture source candidate to `3ca15553f96587f1f96688185165b2ede00e30b0`.
- Added a focused provenance assertion so future vector refreshes cannot retain a stale wallet candidate silently.

Outcome: The sole Iteration 6 provenance blocker is fixed; focused fixture/hash/format verification and bounded re-review remain.

Verification Addendum: Iteration 7
Timestamp: 2026-08-20T20:00:00Z

- The provenance-aware fixture test passed, the fixture parsed as JSON, direct Prettier and `git diff --check` passed, and the refreshed fixture SHA-256 is `017c9b99beb791fd7a73b2bcdf53403ceeead9b6c7255d54dac27ac684bf6d35`.

Code Review: Iteration 7
Timestamp: 2026-08-20T20:00:00Z

Review scope:
- Performed the bounded re-review required by the Iteration 6 approval bar.
- Verified `backend-context-v1.json:5` identifies exact wallet candidate `3ca15553f96587f1f96688185165b2ede00e30b0`.
- Verified `backendContextFixture.spec.ts:35-38` asserts that exact provenance.
- Independently computed fixture SHA-256 `017c9b99beb791fd7a73b2bcdf53403ceeead9b6c7255d54dac27ac684bf6d35`, matching `research/03-cardano-wallet-backend-contract.md:549-552`.
- Verified the append-only implementation log preserves the complete Iteration 6 review and `Decision: requires_changes` before the Iteration 7 implementation and verification entries.
- Accepted the supplied focused test, JSON parse, direct Prettier, and `git diff --check` results; the local whitespace check also passed.

Blocking findings:
- None.

Non-blocking observations:
- None.

Approval bar:
- Satisfied. The sole Iteration 6 provenance blocker is closed, and no task-201 condition remains unresolved.

Decision: approved
