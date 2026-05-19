Planner: Iteration 1
Timestamp: 2026-05-19T17:18:27Z

Task: `task-003` - `Define immutable range derivation and chain-layout preconditions`

Actions taken:
- Read the required planning context in the requested order: `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/workflows/test.md`, `.agent/workflows/ipc.md`, `.agent/workflows/update-doc.md`, the Mithril partial-sync PRD/tasks/research files, `bootstrap-cardano-node.md`, and the chain-storage hardening summary.
- Loaded the `understand` skill as required for nontrivial repository understanding, then verified the important findings directly against live files instead of relying on notes alone.
- Verified the requested backend seams in `source/main/utils/chainStorageManager.ts`, `chainStorageManagerLayout.ts`, `chainStoragePathResolver.ts`, `chainStorageCoordinator.ts`, and `source/main/mithril/MithrilBootstrapService.ts`.
- Performed a brief self-review for scope creep, stale assumptions, hidden manual checkpoints, and avoidable plan ambiguity.

Key findings and decisions:
- Why chosen now: `task-001` and `task-002` already locked the staged-only restore boundary, so `task-003` is the next design gate needed before backend work can safely calculate a partial immutable range or decide when partial sync must be blocked.
- Interaction mode: `autonomous`.
- Scope: define the backend-only source of truth for local immutable position, the exact range-derivation rule against latest Mithril metadata, and the chain-layout preconditions that must pass before partial sync can begin.
- Scope: keep the solution anchored to existing chain-storage seams by reusing `ensureManagedChainLayout`, `resolveChainStoragePath`, and coordinator serialization instead of inventing a separate filesystem-discovery path.
- Non-goal: renderer sync percentage, slot lag, or recommendation copy must not participate in backend range calculation.
- Non-goal: do not reopen the staged-only cutover rule from `task-002`.
- Non-goal: do not implement the product feature in this task; this should stay mostly documentation/spec work, with no product-code change planned unless a later implementation task proves a tiny backend helper is necessary.
- Relevant dependencies: formal dependency `task-001`; locked design input from completed `task-002`.
- Research consulted: `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`, `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`, `.agent/plans/mithril/bootstrap-cardano-node.md`, `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`.
- Docs, workflows, and skills consulted: `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/workflows/test.md`, `.agent/workflows/ipc.md`, `.agent/workflows/update-doc.md`, and `understand`.
- Live repo finding: `chainStorageManagerLayout.ts` already owns managed-layout normalization and recovery semantics; `chainStoragePathResolver.ts` already resolves the active managed chain target from the live `stateDir/chain` entry point; `chainStorageCoordinator.ts` already serializes chain mutations and enforces stopped-node preconditions; `MithrilBootstrapService.ts` is still full-bootstrap-only; there is no existing repo-local helper that already derives a local immutable file number for partial sync.
- Live repo finding: because `ensureManagedChainLayout()` can recover by falling back to default storage, partial sync must not silently continue when that recovery path was taken; otherwise Daedalus could derive a range from the wrong chain root after a broken custom-storage fallback.
- Approved range rule for the canonical plan: resolve and normalize the managed chain target first, derive the local immutable position from the highest parseable immutable file number in the resolved managed chain `immutable/` directory, resolve the latest certified Mithril immutable position from latest snapshot metadata, then request `start = localImmutable + 1` and `end = latestCertifiedImmutable`.
- Approved range rule for the canonical plan: if `localImmutable >= latestCertifiedImmutable`, partial sync does not start because there is no certified missing immutable range to restore.
- Approved range rule for the canonical plan: the backend must own the snapshot lookup and range calculation close together enough to handle latest-snapshot drift; renderer-provided sync state is user context only.
- Approved preconditions for the canonical plan: partial sync may start only after coordinator serialization is held, `cardano-node` is stopped, managed-layout normalization succeeds without recovery fallback, and the resolved managed chain target is readable as a Daedalus-managed chain root.
- Approved preconditions for the canonical plan: the resolved managed chain target must contain a readable `immutable/` directory and a readable `protocolMagicId`, and must not contain obvious type mismatches such as required entries existing as the wrong filesystem kind.
- Approved block conditions for the canonical plan: if the managed chain target is missing `immutable/`, has no parseable immutable files, has unreadable core entries, reflects broken-layout recovery fallback, or otherwise looks partially corrupted or layout-inconsistent, partial sync must be blocked and another recovery path must be used instead of guessing a range.
- Approved block conditions for the canonical plan: `volatile/` is not an input to range derivation and must not be used to infer the missing range.
- Files expected to change: `.agent/plans/mithril-partial-sync/task-plans/task-003.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`.
- Files expected to change: update `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` only if a short addendum is needed to lock the latest-snapshot-drift and layout-blocking interpretation alongside the earlier spike evidence; no product code is expected to change in this task.

Implementation approach:
- Create the canonical `task-003` plan as a design-hardening doc that defines the backend source of truth and blocking rules without implementing partial sync code.
- State that chain ownership and path resolution come from existing chain-storage helpers, while immutable-position inspection is a narrow backend concern to be implemented later near those helpers rather than inside renderer-facing code.
- Define one conservative parsing contract for local immutable position: use the highest parseable immutable file number present in the resolved managed chain `immutable/` directory; if that cannot be derived truthfully, block partial sync.
- Define one conservative preflight contract: normalize layout first, reject recovery fallback and obviously inconsistent chain roots, then derive range from local immutable state plus latest snapshot metadata only.
- Keep later implementation work minimal by reusing current seams in `chainStorageCoordinator` and chain-storage utilities instead of adding a second chain-layout model.

Acceptance criteria:
- The canonical plan names the backend source of truth for current local immutable position.
- The canonical plan states the immutable range derivation rule as `local + 1` through latest certified immutable, with backend-owned snapshot resolution.
- The canonical plan explicitly keeps renderer sync percentage out of backend range calculation.
- The canonical plan lists the chain-layout and node-state preconditions that must pass before partial sync starts.
- The canonical plan states which inconsistent or partially corrupted local states block partial sync instead of allowing heuristic fallback.
- The canonical plan stays within doc/spec scope and does not speculate into unrelated IPC, renderer, or rollback-policy work.

Verification plan:
- Verify the plan against live repo seams in `chainStorageManager.ts`, `chainStorageManagerLayout.ts`, `chainStoragePathResolver.ts`, `chainStorageCoordinator.ts`, and `MithrilBootstrapService.ts`.
- Verify the range rule and drift handling remain consistent with `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`.
- Verify the plan does not conflict with the chain-storage hardening rule that `stateDir/chain` is the authoritative entry point and that broken custom storage may fall back at startup.
- Verify the task remains docs/spec-first and does not pre-decide rollback or failure-option rules reserved for `task-004`.
- No build or test run is required for this planning-only iteration.

Risks and open questions:
- The repo does not yet contain a reusable immutable-file parser, so later implementation must keep that helper narrow and colocated with existing chain-storage utilities.
- Latest-snapshot drift can invalidate a stale `end` selection, so later implementation must resolve latest metadata immediately before download or fail safely and retry in a bounded way.
- The exact immutable filename-completeness heuristic should be verified against real Cardano DB naming during implementation, but this does not change the higher-level source-of-truth rule in the plan.

Required docs, tracking, and research updates:
- Add the canonical task plan doc for `task-003`.
- Tighten PRD wording so the backend-only immutable-range derivation and blocking preconditions are explicit.
- Tighten task-graph wording if later tasks still imply renderer-driven thresholds or heuristic filesystem inference.
- Prefer not to touch product code in this task.
- Prefer not to add a new research note; update the existing spike-results note only if needed for a durable drift/precondition handoff.

Review-log paths:
- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-003-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-003-impl-review.md`

Self-review:
- Scope creep check passed: this plan stays in design/spec territory and does not pull in implementation, rollback policy, or renderer behavior beyond the explicit backend boundary.
- Simplicity check passed: the plan reuses current chain-layout and coordination seams and avoids introducing a second filesystem model.
- Consistency check passed: the proposed source-of-truth and blocking rules match the staged-only restore decision from `task-002`, the latest-snapshot-drift finding from `task-001`, and the live chain-storage architecture.

Status:
- Canonical plan file to create: `.agent/plans/mithril-partial-sync/task-plans/task-003.md`
- Planning status: `approved`
- Build status: `not_run`

Outcome:
- Prepared a docs/spec-first canonical plan for `task-003` that defines backend-only immutable range derivation and chain-layout preconditions using current repo seams, with no planned product-code changes in this task unless a later implementation step proves a minimal helper is necessary.

Critiquer: Iteration 1
Timestamp: 2026-05-19T17:20:53Z

Blocking findings:
- `task-003` still treats `task-002` as only a locked input instead of an explicit dependency in the planning artifacts. That is too weak for a safety gate, because the range/precondition design now depends on the staged-only cutover contract and the fallback semantics that were locked in `task-002`. Make that ordering explicit in both the task plan and task graph so this task cannot be truthfully reviewed or executed out of sequence. References: `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json:95`, `.agent/plans/mithril-partial-sync/task-plans/task-003.md:33-35`.
- The current plan still overclaims local-state validation beyond what the live repo seams can actually enforce today without inventing a new filesystem validator. Phrases like "required entries are present with the expected filesystem kind" and broad "partially corrupted or layout-inconsistent" blockers are too open-ended unless the plan narrows them to concrete checks Daedalus can truthfully perform from the managed root, `immutable/`, `protocolMagicId`, and parseable immutable filenames. Otherwise later implementation will drift into unsupported heuristics. References: `.agent/plans/mithril-partial-sync/task-plans/task-003.md:88-100`, `source/main/utils/chainStorageManagerLayout.ts:24-108`, `source/main/utils/chainStoragePathResolver.ts:37-126`, `source/main/utils/chainStorageManagerShared.ts:30-33`.
- The plan correctly notices that `ensureManagedChainLayout()` can recover by falling back to default storage, but it does not yet force that rule into the canonical downstream updates strongly enough. Given live behavior, partial sync must explicitly inspect `ManagedChainLayoutResult.isRecoveryFallback` and fail closed before deriving any immutable range; this cannot remain only a local note in `task-003.md`. Propagate that constraint into the PRD/tasks/research updates and make it part of the verification checklist. References: `source/main/utils/chainStorageManagerLayout.ts:42-50,90-99`, `source/main/utils/chainStorageCoordinator.ts:140-143,212-219`, `.agent/plans/mithril-partial-sync/task-plans/task-003.md:67-68,83-85,94`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md:83-91`.

Non-blocking observations:
- Keeping this task docs/spec-first is the right call. The live repo does not justify a product-code change yet.
- The plan is correctly backend-owned: renderer sync percentage stays out of range derivation, and `volatile/` is not treated as a source of truth.
- Latest-snapshot drift should be promoted from a risk note to a concrete design rule: either re-resolve `latest` immediately before command issuance or treat Mithril range rejection as a bounded retriable preflight failure. The spike already proved this is real.
- `source/main/utils/chainStorageManager.ts` looks unnecessary as a planned edit target for this task unless a tiny helper becomes unavoidable; trimming that keeps the scope truthfully narrow.

Approval bar:
- Make `task-002` an explicit dependency anywhere task ordering is recorded.
- Narrow the preflight/blocking rules to checks the current repo can truthfully implement without speculative corruption heuristics.
- Require the PRD/tasks/research updates and verification notes to carry the `isRecoveryFallback` fail-closed rule so partial sync can never derive a range from default-storage fallback state.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-19T17:21:08Z

Response to critique:
- Made completed `task-002` an explicit safety-gate dependency in the canonical task plan and will mirror that dependency in the tasks graph update for this task so the staged-only restore contract remains an ordering requirement, not just background context.
- Narrowed the planned preflight and blocking checks to the concrete repo-validatable inputs the later backend can truthfully enforce: managed-layout normalization outcome, `ManagedChainLayoutResult.isRecoveryFallback`, readable managed root, readable `immutable/` directory, readable `protocolMagicId` file, and parseable immutable filenames.
- Promoted latest-snapshot drift from a generic risk note to a concrete downstream design rule: later implementation must re-resolve `latest` immediately before command issuance or fail safely with a bounded retriable preflight error.
- Promoted `ManagedChainLayoutResult.isRecoveryFallback` from a local observation to a required downstream rule that must be carried into the PRD, task graph, research update, and verification checklist so partial sync fails closed before deriving any range from fallback state.
- Trimmed the plan scope further by keeping this task docs/spec-first and by avoiding any speculative filesystem-corruption validator beyond the concrete checks above.

Canonical plan revisions made:
- `Relevant Dependencies` now names completed `task-002` as an explicit safety-gate dependency.
- `Live Repo Findings` now names `ManagedChainLayoutResult.isRecoveryFallback` as the concrete live seam later implementation must inspect.
- `Implementation Approach` now includes the explicit latest-snapshot drift rule.
- `Preconditions To Start Partial Sync` and `Blocking Conditions` now use concrete managed-root, `immutable/`, `protocolMagicId`, and parseable-filename checks instead of broad heuristic corruption language.
- `Acceptance Criteria`, `Verification Plan`, and `Required Docs, Tracking, And Research Updates` now explicitly require propagation of the `isRecoveryFallback` fail-closed rule into downstream PRD, tasks, and research artifacts.
- The task remains docs/spec-first with no product-code change planned in this task.

Resulting approved plan:
- Scope remains limited to defining backend-only immutable range derivation and chain-layout preconditions.
- The source of truth for local immutable position is the highest parseable immutable file number in the resolved managed chain `immutable/` directory.
- The backend-owned requested range is `start = localImmutable + 1` through `end = latestCertifiedImmutable`, with no partial sync when no certified missing immutable range exists.
- Partial sync later must fail closed if managed-layout normalization reports `isRecoveryFallback`, if `immutable/` or `protocolMagicId` are unreadable or wrong-kind, if immutable filenames are not parseable, or if latest snapshot metadata cannot be resolved truthfully.
- Later implementation must keep snapshot lookup and command issuance close enough to handle latest-snapshot drift safely.

Outcome:
- Critique blockers were resolved in the canonical `task-003` plan by making dependency ordering explicit, narrowing blockers to concrete repo-validatable checks, and promoting the `isRecoveryFallback` and latest-snapshot-drift rules into required downstream updates.

