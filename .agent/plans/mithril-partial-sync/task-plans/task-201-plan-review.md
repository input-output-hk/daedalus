Planner: Iteration 1
Timestamp: 2026-05-20T14:32:05Z
Task: `task-201` - `Implement range calculation, staging directory preparation, and preflight checks`
Selection: `new`
Why now: next unblocked pending task on the critical path after completed `task-200`; dependencies `task-003` and `task-200` verified complete.

Work completed:
- Read required docs:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
  - `.agent/workflows/test.md`
  - `.agent/workflows/update-doc.md`
- Read relevant research:
  - `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
  - `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
  - `.agent/plans/mithril-partial-sync/research/05-task-102-coordinator-seam-notes.md`
  - `.agent/plans/mithril-partial-sync/research/06-task-103-node-lifecycle-seam-notes.md`
  - `.agent/plans/mithril-partial-sync/research/07-task-200-service-skeleton-notes.md`
  - `.agent/plans/mithril/bootstrap-cardano-node.md`
  - `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- Consulted `understand` skill first, then verified important findings against live repo files.
- Verified live code constraints in:
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/utils/chainStorageManager.ts`
  - `source/main/utils/chainStorageManagerLayout.ts`
  - `source/main/utils/chainStoragePathResolver.ts`
  - `source/main/utils/chainStorageManagerShared.ts`
  - `source/common/types/mithril-partial-sync.types.ts`

Key repo-verified planning findings:
- `MithrilPartialSyncService` is the correct task-201 implementation boundary; task-200 currently stops at `PARTIAL_SYNC_NOT_READY` after latest-snapshot lookup.
- `ChainStorageCoordinator` already enforces stopped-node and `isRecoveryFallback` rejection before service start.
- Important live constraint: `resolveMithrilWorkDir()` currently resolves to the active managed chain location itself, so task-201 must prepare a separate Daedalus-owned staging subtree and must not use the workdir root as the restore target.
- No reusable immutable filename parser exists yet; task-201 should add only the smallest parser/helper needed.
- Preflight checks must stay concrete and fail closed: readable managed root, readable `immutable/` directory, readable `protocolMagicId`, and at least one parseable immutable filename.

Canonical task-plan artifact:
- Created `.agent/plans/mithril-partial-sync/task-plans/task-201.md`

Plan summary:
- Interaction mode: `autonomous`
- Scope:
  - extend `MithrilPartialSyncService`
  - derive local immutable position from highest parseable filename in managed `immutable/`
  - derive backend-only range `localImmutable + 1` through latest certified immutable
  - fail early on missing `immutable/`, missing `protocolMagicId`, no parseable immutable filename, or no certified missing range
  - prepare a dedicated partial-sync staging directory separated from live chain entries
  - add focused Jest coverage
- Non-goals:
  - no actual download/verification flow yet
  - no LSM conversion or cutover
  - no recovery branching
  - no renderer or IPC contract changes unless a tiny internal-only seam is forced by implementation
- Expected files to change:
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - possibly small helper changes in `source/main/utils/chainStorageManager.ts` or `source/main/utils/chainStorageManagerLayout.ts` only if truly necessary
- Acceptance criteria and verification plan are captured in the canonical plan doc.
- Review-log paths defined:
  - `.agent/plans/mithril-partial-sync/task-plans/task-201-plan-review.md`
  - `.agent/plans/mithril-partial-sync/task-plans/task-201-impl-review.md`

Planning status:
- `draft`

Build status:
- `in_progress`

Self-review:
- No scope creep into `task-202` through `task-204`
- No stale workflow usage; IPC/frontend workflows were correctly not pulled in
- Canonical task-plan doc gap is fixed
- Plan is consistent with PRD, completed dependency tasks, consulted research, and live repo constraints, especially the current workdir-to-managed-chain resolution behavior.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-05-20T14:34:50Z

Blocking findings:
1. The staging-location rule is still too loose and currently allows an unsafe interpretation. `task-201.md:123-127` says staging should be "derived from the workdir context" and even suggests a scratch subtree there, but live code shows `context.mithrilWorkDir` is the resolved managed chain path itself (`source/main/utils/chainStoragePathResolver.ts:120-126`, `source/main/utils/chainStorageCoordinator.ts:233-240`). The spike locked the opposite safety rule: staged work must be fully separate from the live managed chain target (`.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:268-274`). As written, the plan could bless scratch under the live chain root, which reintroduces pre-cutover mutation risk and creates a brittle hidden dependency on task-203 to not wipe its own staging subtree.
2. The proposed "successful preparing-stage handoff" is incompatible with the current runtime contract. `task-201.md:129-132` wants to replace `PARTIAL_SYNC_NOT_READY` with some bounded pre-download handoff, but there is no such status in the shared contract (`source/common/types/mithril-partial-sync.types.ts:3-15`), and the coordinator drops `_partialSyncInProgress` as soon as `start()` resolves (`source/main/utils/chainStorageCoordinator.ts:245-249`). That would end the task-103 safety boundary early and silently change lifecycle semantics even though this plan says it is not touching IPC/shared contracts. The plan needs to keep task-201 inside the existing `start()` flow and preserve a truthful pre-download terminal failure/not-ready boundary until task-202 extends the live operation.

Non-blocking observations:
1. The plan should make stale staging cleanup explicit in acceptance/tests, not just staging creation. The spike proved cancelled partial restores leave partial artifacts behind (`.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:233-246`), so idempotent preflight cleanup is part of the real safety seam for task-201.
2. Add explicit fixture coverage for immutable-position extraction from both `show latest` and list-fallback payload shapes. Current service normalization drops unknown fields early (`source/main/mithril/MithrilPartialSyncService.ts:32-54,184-230`), so this is an easy place for a later regression if the plan stays too abstract.
3. Avoid nudging implementation toward generic chain-storage install helper reuse. Current `installSnapshot()` still moves every top-level entry from `dbDirectory` into the managed target (`source/main/utils/chainStorageManagerLayout.ts:524-567`), which is broader than the fixed allowlist task-203 must enforce.

Approval bar:
1. Rewrite the staging section so the prepared partial-sync scratch path must be outside `layoutResult.managedChainPath`, not merely "not the root itself", and require a test that locks that separation for default and custom storage.
2. Remove the proposed successful pre-download handoff from task-201. Either keep the current truthful `PARTIAL_SYNC_NOT_READY`-style terminal boundary after preflight/staging completes, or explicitly defer all runtime boundary changes to task-202.
3. Tighten verification to cover stale staging cleanup and immutable-position extraction across both latest metadata paths.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-20T14:37:48Z
Task: `task-201` - `Implement range calculation, staging directory preparation, and preflight checks`
Selection: `revise`
Why now: final allowed planning pass after critique; blocking issues were narrowed to staging-path safety, runtime-boundary preservation, and tighter verification.

Work completed:
- Re-read required docs:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
  - `.agent/plans/mithril-partial-sync/task-plans/task-201.md`
  - `.agent/plans/mithril-partial-sync/task-plans/task-201-plan-review.md`
  - `.agent/workflows/test.md`
  - `.agent/workflows/update-doc.md`
- Re-read relevant research:
  - `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
  - `.agent/plans/mithril-partial-sync/research/07-task-200-service-skeleton-notes.md`
  - `.agent/plans/mithril/bootstrap-cardano-node.md`
  - `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`
- Consulted `understand` skill first, then re-verified the critique’s live-code claims in:
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/utils/chainStoragePathResolver.ts`
  - `source/common/types/mithril-partial-sync.types.ts`

Key repo-verified findings carried into the revision:
- `MithrilPartialSyncService` remains the correct task-201 boundary and still stops at `PARTIAL_SYNC_NOT_READY` after latest lookup.
- `ChainStorageCoordinator` still clears `_partialSyncInProgress` as soon as `start()` resolves, so task-201 must not invent a new successful pre-download handoff.
- `resolveMithrilWorkDir()` still resolves to the active managed chain path, so any task-201 scratch area must resolve outside `layoutResult.managedChainPath`, not merely outside its current top-level entries.
- The spike still locks two relevant safety rules:
  - staged work must be fully separate from the live managed chain target
  - cancelled partial restores leave partial artifacts behind, so stale staging cleanup is part of the real preflight seam
- Latest immutable-position extraction still needs backend-local normalization that works across both `snapshot show latest --json` and snapshot-list fallback metadata shapes.

Canonical plan revisions applied to `.agent/plans/mithril-partial-sync/task-plans/task-201.md`:
- Tightened staging requirements:
  - staging must resolve in a Daedalus-owned location fully outside `layoutResult.managedChainPath`
  - task-201 must reject any computed staging path that lands inside the managed chain subtree
  - tests must lock this separation for both default and custom storage contexts
- Removed the proposed successful pre-download handoff:
  - task-201 now explicitly preserves the current bounded pre-download `PARTIAL_SYNC_NOT_READY` terminal boundary
  - any runtime/lifecycle expansion is explicitly deferred to `task-202`
- Tightened verification and acceptance:
  - stale staging cleanup is now explicit in implementation approach, acceptance, and verification
  - immutable-position extraction is now explicitly covered for both latest metadata paths
  - verification now checks that task-201 still ends at the current truthful pre-download boundary
- Kept the plan otherwise narrow:
  - no new shared-contract expansion
  - no new runtime semantics
  - no install/cutover/recovery branching pulled forward

Plan summary after revision:
- Scope remains:
  - extend `MithrilPartialSyncService`
  - derive local immutable position from highest parseable managed `immutable/` filename
  - derive backend-only range from local immutable through latest certified immutable
  - perform concrete fail-closed preflight checks only
  - prepare and clean a Daedalus-owned staging area outside the managed chain subtree
  - add focused Jest coverage
- Non-goals remain:
  - no actual download execution yet
  - no conversion or cutover
  - no recovery-action behavior
  - no renderer or IPC/shared status changes

Planning status:
- `ready`

Build status:
- `not_started`

Self-review:
- Blocking critique items addressed directly in the canonical plan.
- Scope stayed narrow and did not expand into `task-202`.
- Verification now covers the two real regression seams the critique called out: stale staging cleanup and dual latest-metadata normalization.
- Plan remains aligned with PRD, completed dependency tasks, spike evidence, and current live runtime contracts.

Outcome: Plan revised after critique and approved for build

