Implementation: Iteration 1
Timestamp: 2026-05-19T17:00:13Z

Task: `task-002` - `Lock the restore and install strategy from spike evidence`

Changes made:
- Revised `.agent/plans/mithril-partial-sync/task-plans/task-002.md` to lock the approved staged-only cutover rule, include `protocolMagicId`, and align the interaction mode with the required vocabulary (`autonomous`).
- Updated `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` so the restore strategy now explicitly names the staged-only allowlist cutover, rejects direct live-target `--allow-override`, and carries the locked top-level artifact handling into implementation/testing sections.
- Updated `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` so task metadata and downstream task wording match the fixed allowlist cutover contract and required spike-note handoff.
- Updated `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md` with a required strategy decision addendum linking the executed spike evidence to the approved task-002 design decision.

Files touched:
- `.agent/plans/mithril-partial-sync/task-plans/task-002.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`

Verification run:
- `jq empty .agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `git diff --check -- .agent/plans/mithril-partial-sync/task-plans/task-002.md .agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md .agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json .agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- Manual repo cross-check against current live code constraints in `source/main/utils/chainStorageManagerLayout.ts`, `source/main/utils/chainStorageCoordinator.ts`, and `source/main/mithril/MithrilBootstrapService.ts` to confirm the locked docs match current staged-install and bootstrap architecture boundaries.

Deviations from approved plan:
- None. The implementation stayed within the approved documentation-hardening scope.

User interaction required:
- No

Code Review: Iteration 1
Timestamp: 2026-05-19T17:02:10Z

Task: `task-002` - `Lock the restore and install strategy from spike evidence`

Blocking findings:
None.

Non-blocking observations:
1. The docs now consistently lock one exact staged-only cutover rule instead of leaving merge/preserve behavior open. The PRD, task plan, tasks graph, and spike note all agree on the fixed allowlist `clean`, `immutable`, `ledger`, `lsm`, and `protocolMagicId`, explicit rejection of live-target `--allow-override`, and explicit discard of live `volatile/` (`.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md:197-221`, `.agent/plans/mithril-partial-sync/task-plans/task-002.md:95-117`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json:20`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json:63-85`, `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md:293-312`).
2. The locked wording matches the live repo constraints it cites. Current install code still empties the managed target and moves every staged top-level entry, so documenting a future narrowed allowlist is the right hardening move (`source/main/utils/chainStorageManagerLayout.ts:524-563`). Current coordinator/bootstrap behavior is still empty-chain-only and serialized (`source/main/utils/chainStorageCoordinator.ts:128-159`, `241-256`), bootstrap still performs explicit conversion before install (`source/main/mithril/MithrilBootstrapService.ts:201-229`), and generic crash restarts are still suppressed only while Mithril bootstrap is active (`source/main/cardano/setup.ts:123-133`).
3. Scope stayed appropriately tight for a docs-only gate. `task-003` still owns range derivation and layout preconditions, and `task-004` still owns restart-normal eligibility, rollback, and kill-switch policy, while `task-002` now fully locks the filesystem cutover rule those later tasks must not reopen (`.agent/plans/mithril-partial-sync/task-plans/task-002.md:30-32`, `95-111`, `148-150`).
4. Workflow compliance looks reasonable for this iteration: the implementation review records syntax/whitespace validation for the edited planning files and a manual live-code cross-check, which is proportionate for documentation hardening (`.agent/plans/mithril-partial-sync/task-plans/task-002-impl-review.md:18-21`).

Approval bar:
1. Keep the staged-only cutover contract unchanged in subsequent implementation tasks unless a new spike reopens the evidence base.
2. Ensure `task-203` actually narrows the current move-all installer behavior to the documented allowlist rather than reusing `installSnapshot` semantics unchanged.
3. Preserve the separation now documented between this filesystem decision and the later `task-003` / `task-004` decisions.

Decision: approved

