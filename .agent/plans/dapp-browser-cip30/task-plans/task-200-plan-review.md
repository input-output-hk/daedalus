Planner: Iteration 1
Timestamp: 2026-08-19T00:46:14Z

Plan: Created canonical draft `.agent/plans/dapp-browser-cip30/task-plans/task-200.md`. It classifies task-200 as a cross-repository backend task with `interactive_validation`, defines the strict additive capability/error contract, preserves task-209 pin ownership, and requires candidate commit, upstream review, no-persistence rollback evidence, temporary Daedalus integration, and exact combined review.

Consulted: Required architecture, PRD/tracker, accepted backend and era research, IPC/test/documentation/Nix/build workflows, sibling cardano-wallet guidance, live API/Swagger/test seams, relevant git history, both empty review logs, and the `understand` skill.

Self-review: Scope excludes downstream backend operations, persistence, Daedalus clients, IPC, UI, hardware, and pin changes. Contract strictness, backend ownership, error redaction, manifests, tests, documentation, migration/rollback disposition, and cross-repository review sequencing are covered. Focused Prettier and whitespace checks passed.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-19T00:51:05Z

The plan is well-researched and appropriately limits task-200 to backend contract work, but three contract-boundary issues prevent approval.

Blocking findings:

1. The candidate would advertise capabilities that do not exist. `.agent/plans/dapp-browser-cip30/task-plans/task-200.md:85-89` requires the endpoint to report all four revision-1 capabilities, while lines 43-44 explicitly exclude their operational routes and tasks 201-208 remain pending. The task-003 contract defines these capabilities as runtime availability predicates rechecked before operations, so “non-pin-eligible” labeling does not make a valid but false capability response honest or fail-closed. Preserve the complete-set rule, but specify sequencing under which an incomplete candidate cannot return a successful V1 document. Either defer successful handler activation until all four families exist or define an exact fail-closed unavailable response until then. Synchronize the tracker/research if task-200 can no longer claim a successful live aggregate endpoint by itself.

2. Daedalus consumer validation is assigned to the cardano-wallet producer without an executable owner. The plan requires rejection of build/source/network mismatches and classification of an absent old-backend endpoint at `.agent/plans/dapp-browser-cip30/task-plans/task-200.md:38`, `:158-160`, `:177`, and `:199-202`, while also prohibiting a Daedalus client in task-200. A cardano-wallet `FromJSON` instance can enforce document shape, but it cannot establish equality with the packaged Daedalus pin or expected configured network. Research 03 assigns startup and per-call expected-identity checks to Daedalus, and tracker task-209 owns runtime validation and old-backend feature refusal. Narrow task-200 to strict shape/value-domain parsing, producer runtime-field assertions, and proof that the old route is absent. Move expected-pin/network comparison and production feature-unavailable classification explicitly to task-209, eliminating any test-only expected-identity validator that would otherwise become dead production machinery.

3. Unsupported-era behavior is undefined at a live boundary. The plan reuses `ApiEra` for `current_era` and records Dijkstra as unsupported, but live `Cardano.Wallet.Api.Types.Era.fromReadEra` throws on `Read.Dijkstra`. The plan therefore does not establish whether `/v2/dapp-capabilities` crashes, returns an uncontrolled 500, falsely reports Conway, or fails with a fixed response when the node enters Dijkstra. Specify and test one deterministic fail-closed behavior without advertising Dijkstra capability or casually widening the existing public `ApiEra`. If this requires a capability-endpoint error not frozen by research 03, reconcile that contract before implementation.

Non-blocking observations:

- The interaction mode is honest: candidate creation may proceed, but named ownership, authorized upstream review, a durable substantive record, and unresolved-condition handling are explicit.
- Candidate/review/follow-up-commit/integration/pin sequencing correctly preserves task-209 as the sole tracked pin owner.
- No-persistence rollback is appropriately narrow and requires replanning if persistence appears.
- Swagger, golden, client/link, server, local-cluster, fixed-error, and response-body redaction coverage are otherwise comprehensive.
- Update stale lifecycle text stating that both review logs are empty and that build is already in progress. Also replace the nonexistent `ApiSpec.hs` example with live test modules during the revision; neither issue should keep the loop open independently.

Approval bar:

Approve once the plan makes successful capability publication truthful relative to implemented operations, restores the producer/consumer validation boundary with task-209 owning expected identity and old-backend activation refusal, and defines tested fail-closed Dijkstra behavior. The revised plan should preserve the current scope exclusions, complete capability-set contract, upstream checkpoint, no-persistence rollback, and unchanged Daedalus pin.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-19T00:57:03Z

Plan: Revised only `.agent/plans/dapp-browser-cip30/task-plans/task-200.md`.

Revision: The task-200 candidate now returns the exact unmatched-route `404` until task-209 activates the complete four-family V1 response. Task-200 owns strict shape/value-domain parsing, producer runtime fields, deterministic non-Conway/Dijkstra refusal, and old-route absence proof. Task-209 owns expected Daedalus identity checks, production unavailable classification, aggregate activation, and pinning.

Self-review: Preserved upstream review, interactive validation, no persistence, unchanged pin, complete-set semantics, fixed errors, and downstream boundaries. Replaced nonexistent `ApiSpec.hs` references and corrected lifecycle/build wording.

Verification: Direct Prettier check and repository whitespace checks passed; no review log or implementation file was modified.

Outcome: Canonical plan revised and ready for critique

Critiquer: Iteration 2
Timestamp: 2026-08-19T00:58:41Z

The focused convergence check confirms that the revision resolves all three original blockers without introducing a new material defect.

Blocking findings:

None.

Non-blocking observations:

- The task-200 HTTP handler is unconditionally identical to ordinary unmatched-route `404` behavior, with no activation input. Successful publication is deferred to task-209 after all four operation families exist.
- Task-209 explicitly owns expected source/network/genesis identity comparison, production unavailable classification, aggregate activation, and the tracked backend pin.
- The constructor branches on internal `Read.Era` before `ApiEra.fromReadEra`; Dijkstra and every non-Conway era deterministically return the tested unmatched-route response without widening `ApiEra` or advertising Dijkstra.
- The plan preserves the complete four-capability revision-1 set and prohibits partial publication, placeholders, availability flags, and per-family activation.
- Authorized upstream review remains a mandatory manual checkpoint. No-persistence rollback evidence, old-pin compatibility, temporary candidate integration, and unchanged tracked pin are explicit acceptance gates.
- Research 03 and tracker reconciliation remain required completion work and are correctly scoped without changing the frozen successful wire contract.

Approval bar:

Met. Successful capability publication is truthful, producer and consumer responsibilities are correctly separated, Dijkstra behavior is deterministic and contract-preserving, and the upstream, rollback, complete-set, and unchanged-pin gates remain intact.

Decision: approved

