Planner: Iteration 1
Timestamp: 2026-05-19T16:53:02Z

Task: `task-002` - `Lock the restore and install strategy from spike evidence`
Actions taken:
- Read required planning context in the requested order, including the PRD, tasks graph, spike results, relevant Mithril historical research, and the `test` and `update-doc` workflows.
- Loaded the `understand` skill first per instruction, then verified the important planning facts directly against live repo files instead of relying on prior notes alone.
- Created the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-002.md`.
- Performed a brief self-review for scope creep, missing docs/tests, and consistency with fixed decisions.

Key findings and decisions:
- Locked the approved strategy to staged restore plus validated staged install only.
- Rejected direct in-place restore into the live managed chain target, including treating `--allow-override` as evidence for live-target safety.
- Preserved the fixed feature decisions in the plan: manual diagnostics entry, supported networks `mainnet`/`preprod`/`preview`, automatic node stop before partial sync, automatic node restart after success, and no regression of the existing empty-chain Mithril bootstrap flow.
- Locked the evidence-backed top-level handling posture:
- `immutable`: only from validated staged output, never direct live restore target
- `ledger`: staged replacement only after successful restore plus required conversion
- `lsm`: must be produced by Daedalus-owned conversion before install; preserve live `lsm/` until then
- `volatile`: no approved direct merge rule from this task; preserve live runtime state during staging
- `clean`: treated as staged metadata installed with the validated staged snapshot set, not mutated in place during download
- Kept the plan intentionally narrow by deferring immutable-range derivation to `task-003` and failure branching / restart-normal safety / kill-switch rules to `task-004`.

Evidence basis captured in plan:
- `task-001` spike proved scratch-target partial restore semantics, ancillary-enabled restores still requiring conversion, latest-snapshot drift risk, and cancellation leaving partial artifacts behind.
- Live repo verification confirmed current bootstrap architecture still depends on empty-chain startup gating in `handleDiskSpace.ts`, stopped-node mutation coordination in `chainStorageCoordinator.ts`, Mithril restart suppression in `cardano/setup.ts`, and explicit conversion/install behavior in `MithrilBootstrapService.ts`.

Required follow-up updates identified by the plan:
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` so restore strategy is stated as the approved staged strategy rather than a provisional assumption, and record the rejected direct-override alternative.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only where later task wording still leaves room for direct in-place restore or unspecified live merge behavior.
- Add a short decision addendum to `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` only if needed to make the handoff from spike evidence to approved strategy explicit.

Self-review:
- Scope creep check passed: no implementation, range-derivation detail, kill-switch design, or failure-option policy was pulled into this planning task.
- Docs/tests check passed for planning scope: required plan sections, verification plan, and minimal doc/tracking updates are present; no build/test run was needed for the planning loop.
- Consistency check passed: the new plan aligns with spike evidence, existing Mithril/bootstrap architecture, and the fixed product constraints.

Status:
- Canonical plan file created: `.agent/plans/mithril-partial-sync/task-plans/task-002.md`
- Planning status: `approved`
- Build status: `not_run`

Open items for critique:
- Whether the conservative top-level rule for `volatile` should stay phrased as `no approved merge rule yet` or be tightened further in the PRD wording before `task-003`.
- Whether the spike-results addendum is necessary, or whether updating only the PRD and tasks graph is sufficient for a clean source-of-truth handoff.

Critiquer: Iteration 1
Timestamp: 2026-05-19T16:56:05Z

Task: `task-002` - `Lock the restore and install strategy from spike evidence`

Blocking findings:
1. The plan does not actually lock the install strategy far enough for a task whose purpose is to lock it. It explicitly defers the final live cutover semantics for `volatile/` and the broader replace-vs-preserve rule set to `task-003` and `task-203`, but those later tasks need this decision as an input, not an open question. Right now the plan says `volatile` has `no approved merge rule yet` and keeps the `exact live install cutover semantics` open. That leaves the main safety question unresolved: after staged restore and conversion, does Daedalus replace the managed target wholesale, replace only a fixed top-level subset, or preserve any live directories at all? This is a blocker because the current live installer in `source/main/utils/chainStorageManagerLayout.ts:524` empties the managed chain and moves all staged top-level entries, so downstream implementation will otherwise fill in a critical safety decision ad hoc.
2. The locked top-level handling set is incomplete for the live chain root Daedalus actually installs. The spike results show Mithril restores `protocolMagicId` alongside `clean/`, `immutable/`, and optional `ledger/` (`.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:135-139`, `174-178`), and the current installer moves every top-level entry it finds from `db/` into the managed chain target (`source/main/utils/chainStorageManagerLayout.ts:557-563`). The plan only locks rules for `immutable`, `volatile`, `ledger`, `lsm`, and `clean`. Leaving `protocolMagicId` unspecified means the install contract is still materially incomplete.
3. The documentation handoff is too weak for a gating design decision. The task’s own scope is to translate spike evidence into the approved strategy and update planning artifacts immediately, but the plan makes the spike-results addendum optional (`only if needed`). Because later tasks are supposed to inherit this lock without reopening the question, the executed evidence note should be updated with the selected strategy and rejected direct-override alternative, not left as a pure spike log plus an implicit PRD/task-plan interpretation. Otherwise the source-of-truth boundary stays split across files and invites stale assumptions during implementation.

Non-blocking observations:
1. The plan is appropriately conservative in rejecting direct live-target `--allow-override` and in keeping partial sync separate from empty-chain bootstrap.
2. The live-file verification is good and correctly anchored to current boundaries in `chainStorageCoordinator.ts`, `handleDiskSpace.ts`, `MithrilBootstrapService.ts`, and `cardano/setup.ts`.
3. Simplification is available here: prefer one explicit install rule such as `never merge live runtime state; cut over only from validated staged output with a fixed top-level entry allowlist` over leaving partial preserve/merge semantics open.
4. `task-004` is the right place for restart-normal eligibility and kill-switch policy, so those deferrals are fine as long as the filesystem cutover rule itself is fully locked here.

Approval bar:
1. Update the task-002 plan so it names one explicit managed-target cutover rule, not just a staged boundary.
2. Extend the locked top-level entry rules to cover every top-level artifact the staged DB can install, including `protocolMagicId`.
3. Update the PRD/tasks wording to match that exact cutover rule.
4. Add a short strategy decision addendum to `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` so the spike evidence and approved interpretation stay linked in the repo.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-19T16:58:54Z

Task: `task-002` - `Lock the restore and install strategy from spike evidence`
Actions taken:
- Re-read the required planning context, spike results, historical Mithril research, and the `test` and `update-doc` workflows.
- Verified the critical live-code assumptions again against current repo files, including `source/main/utils/chainStorageManagerLayout.ts`, `source/main/utils/chainStorageCoordinator.ts`, `source/main/mithril/MithrilBootstrapService.ts`, and the current Mithril pin in `flake.lock`.
- Revised the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-002.md`.
- Updated the canonical planning docs to match the locked strategy:
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`

Key findings and decisions:
- Closed the main blocker by locking one explicit managed-target cutover rule instead of leaving `volatile` or broader cutover semantics open.
- The approved rule is now:
- partial sync is staged-only
- Daedalus never points Mithril restore at the live managed chain target
- after staged restore, verification, and required LSM conversion succeed, Daedalus cuts over by emptying the managed chain target and reinstalling only a fixed allowlist of validated staged entries
- the fixed allowlist is `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`
- no live top-level entry is merged or preserved across cutover
- live `volatile/` is discarded during cutover and must be recreated by `cardano-node` after restart
- any unexpected staged top-level entry, including `volatile/`, is a validation failure that blocks install
- Extended the locked top-level artifact rules to cover the full staged install set evidenced by the spike, including `protocolMagicId`, instead of only `immutable`/`volatile`/`ledger`/`lsm`/`clean`.
- Preserved the already locked product constraints: manual diagnostics entry, supported networks `mainnet`/`preprod`/`preview`, automatic node stop before work, automatic node restart after success, and no regression of the existing empty-chain Mithril bootstrap path.
- Kept `task-003` and `task-004` scoped correctly:
- `task-003` still owns range derivation and start preconditions
- `task-004` still owns restart-normal eligibility, rollback safety, and kill-switch policy
- neither task is now allowed to reopen the locked filesystem cutover rule

Doc and task-graph updates completed:
- PRD restore strategy wording now states the exact approved staged cutover rule rather than a provisional assumption.
- PRD validation and implementation wording now reflect the fixed allowlist and the explicit rejection of direct live-target restore via `--allow-override`.
- Tasks JSON now mirrors that same rule in:
- task metadata `importantNote`
- `task-002` description and acceptance
- phase-2 engine wording
- `task-203`, which now explicitly owns validated staged cutover rather than ambiguous install-or-merge behavior
- Added the required strategy decision addendum to `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` so the spike evidence and approved interpretation remain linked in one repo-local source of truth.

Evidence basis confirmed:
- The spike note still shows staged artifact layouts containing `clean/`, `immutable/`, optional `ledger/`, and `protocolMagicId`, with no staged `lsm/` until Daedalus conversion and no evidence that live `volatile/` can be merged safely.
- The current shared installer in `source/main/utils/chainStorageManagerLayout.ts` still empties the managed chain target and then moves every staged top-level entry it finds, so the revised plan now explicitly requires later implementation to narrow that generic move-all behavior to the locked allowlist.

Self-review:
- Blocker coverage check passed:
- one explicit cutover rule is now named
- locked top-level rules now include `protocolMagicId`
- PRD/tasks wording now matches the exact cutover rule
- the research addendum is now explicit and required, not optional
- Scope check passed: this revision did not pull `task-003` range-derivation detail or `task-004` recovery-policy detail back into `task-002`.
- Consistency check passed: the revised plan now matches the spike evidence, the current live installer behavior, and the existing chain-storage hardening rule that managed chain mutations must stay explicit and bounded.

Status:
- Canonical plan file revised: `.agent/plans/mithril-partial-sync/task-plans/task-002.md`
- Supporting canonical docs revised:
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- Planning status: `approved`
- Build status: `not_run`

