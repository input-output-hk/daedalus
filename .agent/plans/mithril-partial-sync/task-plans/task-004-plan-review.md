Planner: Iteration 1
Timestamp: 2026-05-19T17:29:24Z

- Read required planning docs, workflows, partial-sync materials, and relevant live repo seams affecting failure containment and rollout safety.
- Created the canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-004.md`.

Plan summary:
- Why now: `task-004` is the next unblocked critical-path safety gate after `task-002` and `task-003`.
- Interaction mode: `autonomous`.
- Scope: define stage-based failure containment, recovery-option eligibility, crash interruption behavior, and a launcher-config kill switch for diagnostics-launched partial sync.
- Non-goals: no product-code implementation, no reopening staged-only restore or range-derivation decisions, no final QA or rollout enablement.

Approved approach:
- Use three safety boundaries: pre-cutover, live cutover in progress, and validated staged DB installed but handoff failed.
- Boundary A allows retry, restart-normal, or wipe-plus-full-sync after staged cleanup.
- Boundary B disables retry and restart-normal; wipe-plus-full-sync is the only safe recovery.
- Boundary C allows restart-normal on the newly installed DB or wipe-plus-full-sync; retry remains disabled.
- Cancellation is allowed only before live cutover begins.
- Startup must use a durable partial-sync marker to distinguish interrupted Boundary A from Boundary B/C states.
- The kill switch should live in `LauncherConfig`, be enforced in both renderer and main, and must not affect the existing empty-chain Mithril bootstrap path.

Verification and docs:
- Verified the plan against current seams in `chainStorageCoordinator`, `handleDiskSpace`, `cardano/setup`, `MithrilBootstrapService`, `config`, and shared Mithril types.
- Recorded required downstream PRD and task-graph updates in the canonical plan.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-05-19T17:31:06Z

Blocking findings:
1. The interrupted `Boundary B` recovery path is underspecified at the process boundary, and the current plan can strand users in a node-blocked startup state with no defined UI or IPC path to recover. `task-004.md:111-115` says startup must detect an interrupted partial sync after cutover began, refuse normal node start, and force wipe-plus-full-sync. But the same plan keeps the new UX anchored in diagnostics and explicitly out of startup flow, which leaves no defined recovery owner when diagnostics is unavailable because the node never starts.
2. The plan overstates `restart normally` safety for all `Boundary C` failures without evidence that every post-cutover node-start failure is DB-safe. Grouping renderer handoff failure and backend node-start failure together is too broad. The plan should fail narrower.
3. The rollback/kill-switch story does not preserve recovery for already-affected installs. Disabling diagnostics-launched partial sync must not remove the minimal recovery affordance needed when a persisted partial-sync marker already indicates an unsafe interrupted state.

Non-blocking observations:
1. The three-boundary model is a good simplification and matches the staged-only cutover evidence better than a finer-grained state machine.
2. Backend ownership of recovery eligibility is the right direction and fits the existing coordinator and restart-suppression seams.
3. Reusing the existing state-dir marker pattern would likely keep startup ownership narrower than inventing a new storage location later.
4. Later task wording should also call out launcher-config wiring, not only PRD text.

Approval bar:
1. Define the startup-owned recovery surface for interrupted `Boundary B` states so recovery remains possible when diagnostics UI is unavailable.
2. Narrow `Boundary C` so `restart normally` is not automatically allowed after every node-start failure.
3. Clarify that the kill switch disables new partial-sync entry points, but does not disable the minimal recovery path required for already-interrupted or unsafe installs.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-19T17:32:49Z

- Read the current canonical task plan, critique log, required docs, workflows, and relevant Mithril research before revising the plan.
- Updated `.agent/plans/mithril-partial-sync/task-plans/task-004.md` to address the full critique blocker set.

Fixes applied:
- Assigned interrupted unsafe states to a startup-owned minimal recovery surface so users are not stranded when diagnostics UI is unavailable.
- Narrowed post-cutover safety by splitting the old Boundary C into `Boundary C1` (validated DB installed but first node start not yet proven) and `Boundary C2` (successful node start already observed, only later UI/marker-clear work failed).
- `Boundary C1` now follows the same wipe-plus-full-sync-only recovery rule as interrupted `Boundary B`.
- Only `Boundary C2` allows restart-normal on the installed DB.
- Clarified that the launcher-config kill switch disables new diagnostics-launched partial sync entry and retry paths, but does not disable the startup-owned recovery path for already-unsafe installs.

Plan/doc updates:
- Expanded scope, acceptance criteria, rollback rules, and downstream doc/task follow-ups to carry the startup-owned recovery model and narrowed Boundary C semantics forward.

Outcome: Final planning pass completed; proceeding to implementation

