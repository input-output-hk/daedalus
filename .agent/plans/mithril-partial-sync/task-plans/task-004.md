# Task task-004: Define failure containment, kill-switch, and rollback rules

## Task ID and Title

- ID: `task-004`
- Title: `Define failure containment, kill-switch, and rollback rules`

## Why This Task Was Chosen Now

- `task-002` locked the staged-only cutover rule.
- `task-003` locked the backend range-derivation and preflight contract.
- `task-004` is the next safety gate because later backend and renderer work cannot truthfully expose `retry`, `restart-normal`, and `wipe-and-full-sync` actions until Daedalus has explicit rules for when each option remains safe.

## Interaction Mode

- Mode: `autonomous`
- Required user inputs: none for this planning task.
- Required manual test steps from the user: none before implementation starts; supported-network manual QA belongs to `task-401`.
- Evidence needed back from the user later: manual QA results for `mainnet`, `preprod`, and `preview`, including default and custom chain storage, before rollout enablement is finalized.
- Can implementation proceed before user interaction: yes. Code, automated tests, and disabled-by-guard rollout preparation can proceed before manual QA or release enablement decisions.

## Scope

- Define failure-containment rules for partial sync by progress boundary.
- Define when `retry partial sync` remains safe.
- Define when `restart normally on existing DB` remains safe and when it must be disabled.
- Define when `wipe chain and perform full Mithril sync` becomes the only safe recovery.
- Define the minimal startup-owned recovery surface for interrupted unsafe partial-sync states when diagnostics cannot load.
- Define one rollout kill switch and where it lives.
- Keep the existing empty-chain Mithril bootstrap flow unaffected.

## Non-Goals

- Implementing the partial sync service, IPC channels, or renderer UI.
- Reopening the staged-only restore rule from `task-002`.
- Reopening immutable range derivation or managed-layout preconditions from `task-003`.
- Designing a second storage model, resume-in-place flow, or live-target merge strategy.
- Final manual QA execution or release enablement; that belongs to `task-401`.

## Relevant Dependencies

- Required completed dependencies:
  - `task-002`
  - `task-003`
- Direct downstream tasks that depend on this safety contract:
  - `task-101`
  - `task-102`
  - `task-103`
  - `task-200`
  - `task-201`
  - `task-203`
  - `task-204`
  - `task-300`
  - `task-303`
  - `task-400`
  - `task-401`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/01-codebase-and-cli-findings.md`
- `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`
- `.agent/plans/mithril/bootstrap-cardano-node.md`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril/research/mithril-chain-storage-hardening-summary.md`

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/readme.md`
  - `.agent/system/architecture.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- Workflows:
  - `.agent/workflows/update-doc.md`
  - `.agent/workflows/test.md`
  - `.agent/workflows/ipc.md`
  - `.agent/workflows/frontend.md`
- Skills:
  - `understand` not used; direct repo reads were sufficient and important findings were verified against live files before writing this plan.

## Live Repo Findings Verified For Planning

- `source/main/utils/chainStorageCoordinator.ts` already serializes chain-storage mutations, rejects mutations while Mithril bootstrap is active, and enforces a stopped-node precondition for destructive chain work.
- `source/main/utils/handleDiskSpace.ts` already distinguishes between Mithril states that block node start and fallback paths that can safely restart the node only after cleanup or terminal success.
- `source/main/cardano/setup.ts` already suppresses generic `cardano-node` crash restart while Mithril bootstrap is active, which is the existing seam partial sync should mirror instead of inventing a second restart policy model.
- `source/main/mithril/MithrilBootstrapService.ts` already cleans staged artifacts on cancel, leaves a lock-backed safety boundary for startup bootstrap failure handling, and wipes managed contents only through explicit service methods.
- `source/main/config.ts` already exposes `LauncherConfig` as the smallest existing main-process rollout seam for a Daedalus-controlled kill switch.
- `source/common/types/mithril-bootstrap.types.ts` already separates Mithril status from error stage, which supports a later partial-sync-specific safety contract where enabled actions depend on backend state instead of renderer guesses.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `.agent/plans/mithril-partial-sync/task-plans/task-004.md`

## Implementation Approach

1. Lock one stage-based failure containment model.
   - Treat partial sync as three safety boundaries, not one generic failure bucket.
   - Boundary A: pre-cutover work only. This includes `stopping-node`, `preparing`, `downloading`, `verifying`, `converting`, and any staged validation failure that occurs before the live managed chain target is emptied.
   - Boundary B: cutover started but not fully completed. This begins the moment Daedalus empties the managed chain target or otherwise starts replacing live top-level chain entries.
   - Boundary C: cutover completed and the validated staged DB is fully installed, but node handoff or normal app return still failed.

2. Lock recovery-action eligibility by boundary.
   - Boundary A: live managed chain data is still the pre-partial-sync DB, so `retry partial sync`, `restart normally on existing DB`, and `wipe chain and perform full Mithril sync` are all safe.
   - Boundary B: the original live DB is no longer trustworthy, and the local immutable baseline needed for another partial restore is no longer truthfully available. In this boundary, disable both `retry partial sync` and `restart normally on existing DB`; only `wipe chain and perform full Mithril sync` remains safe.
   - Boundary C: the managed target now contains a fully installed validated staged DB. `restart normally on existing DB` is safe, `wipe chain and perform full Mithril sync` is still safe as an escalation path, and `retry partial sync` should be disabled because there is no remaining partial-gap basis to derive against the newly installed latest-certified DB.

3. Lock cancellation and crash-interruption rules.
    - Cancellation before Boundary B must clean partial-sync staging artifacts, clear active-process state, and preserve the preexisting managed chain target so normal restart remains safe.
    - Cancellation must not be accepted once Boundary B starts. After live cutover begins, user-driven cancel becomes a denied action and the operation must run to a terminal success or terminal failure state.
    - Partial sync needs its own durable operation marker so post-crash startup does not guess whether restart-normal is safe.
    - If startup detects an interrupted partial sync whose recorded phase was still in Boundary A, Daedalus should remove staged partial-sync artifacts, clear the marker, and allow normal startup on the untouched DB.
    - If startup detects an interrupted partial sync whose recorded phase had entered Boundary B, Daedalus must not start `cardano-node` on the managed target automatically and must not wait for diagnostics to recover the install.
    - Boundary B interruption is owned by startup. The app must expose a minimal blocking startup recovery surface before normal node start, with backend-owned actions only: `wipe chain and perform full Mithril sync` as the required recovery path, plus non-recovery affordances such as `quit` and support-log access if already available through existing seams. This surface must not depend on the diagnostics dialog or a running node.
    - Boundary C is narrower than one generic post-cutover bucket:
      - Boundary C1: validated staged DB installed, but the first `starting-node` handoff did not complete and backend has not yet proved a successful node start on that installed DB.
      - Boundary C2: backend has already observed a successful node start on the installed DB, and only renderer handoff, marker cleanup, or normal app return failed afterward.
    - If startup detects an interrupted Boundary C1 state, it must follow the same startup-owned minimal recovery path as interrupted Boundary B. `restart normally` is not automatically safe just because cutover finished.
    - If startup detects an interrupted Boundary C2 state, Daedalus may clear the marker and allow normal startup because the installed DB has already passed one successful node-start handoff.

4. Lock one rollout kill switch.
    - Use `LauncherConfig` as the primary kill-switch location because it is an existing Daedalus-controlled startup configuration seam.
    - Add one boolean launcher-controlled flag for this feature, with partial sync disabled whenever the flag is false.
    - The guard must be enforced in both renderer and main process:
      - renderer: hide or disable the diagnostics CTA and any partial-sync recovery affordances
      - main: reject partial-sync start and partial-sync-specific fallback IPC requests when disabled
    - The kill switch must disable only new diagnostics-launched partial sync entry and retry paths. It must not disable the startup-owned minimal recovery path for installs already marked unsafe by an interrupted Boundary B or Boundary C1 state.
    - The kill switch must not disable or alter the existing empty-chain Mithril bootstrap path.
    - Rollout posture for implementation planning: keep the feature under this guard until `task-401` manual QA approves enablement.

5. Lock backend ownership of safety decisions.
   - The renderer may show only the actions the backend exposes as allowed for the current terminal state.
   - The renderer must not infer restart-normal safety from visible status text alone.
   - The backend must compute and persist an explicit recovery-eligibility result from the current operation boundary so IPC and UI cannot request an unsafe branch.

6. Keep the rollback story minimal and explicit.
    - Product rollback for field issues is: turn off the launcher kill switch, which removes new diagnostics entry points and causes the partial-sync IPC surface to reject new start requests.
    - Product rollback must still preserve the startup-owned unsafe-install recovery path so already-interrupted installs are not stranded behind the disabled diagnostics entry point.
    - Operational rollback after a single failed run is boundary-dependent:
      - Boundary A: restart-normal, retry, or wipe-full-sync
      - Boundary B: wipe-full-sync only
      - Boundary C1 node-start failure before one proven successful node start on the installed DB: wipe-full-sync only
      - Boundary C2 post-start handoff failure after one proven successful node start on the installed DB: restart-normal or wipe-full-sync
    - Existing startup Mithril bootstrap remains the safety fallback for empty-chain recovery and must remain unchanged.

## Acceptance Criteria

- The canonical plan defines explicit recovery rules instead of leaving rollback implied.
- The canonical plan states when `restart normally on existing DB` is safe and when it must be disabled.
- The canonical plan states when `retry partial sync` remains safe and when it must be disabled.
- The canonical plan states when `wipe chain and perform full Mithril sync` becomes the only safe recovery.
- The canonical plan defines a durable interrupted-operation rule for app crash or relaunch during partial sync.
- The canonical plan defines a startup-owned minimal recovery surface for interrupted unsafe states that does not depend on diagnostics availability.
- The canonical plan defines one rollout kill switch and where it will live.
- The canonical plan states that the kill switch disables new diagnostics-launched partial sync but preserves recovery for already-unsafe installs.
- The canonical plan keeps diagnostics-launched partial sync separate from the existing empty-chain Mithril bootstrap flow.
- Later backend and renderer tasks can implement failure actions without reopening stage-safety questions.

## Verification Plan

- Verify the boundary rules against the already locked staged-only strategy in `.agent/plans/mithril-partial-sync/research/02-validation-spike-results.md`:
  - pre-cutover work leaves the live DB untouched
  - cutover empties the managed target and discards live `volatile/`
  - cancellation left partial artifacts in staging during the spike, so restart-normal cannot be assumed safe without explicit cleanup
  - the spike did not prove that every post-cutover `starting-node` failure is DB-safe, so Boundary C must stay narrower than a generic restart-allowed bucket
- Verify the live repo seams still support this design:
  - `source/main/utils/chainStorageCoordinator.ts` for serialized mutation and stopped-node guards
  - `source/main/utils/handleDiskSpace.ts` for current Mithril failure branching and startup fallback patterns
  - `source/main/cardano/setup.ts` for active-state restart suppression
  - `source/main/mithril/MithrilBootstrapService.ts` for cleanup semantics and marker precedent
  - `source/main/config.ts` for launcher-controlled rollout gating
- Verify downstream work can remain minimal:
  - `task-204` should only encode these eligibility rules, not invent new ones
  - `task-300` and `task-303` should render backend-allowed actions, not compute safety locally
  - `task-401` should treat kill-switch enablement as a release decision backed by QA evidence

## Risks or Open Questions

- The exact shape and storage location of the partial-sync durable marker still needs implementation selection, but the safety rule is fixed: startup must distinguish Boundary A interruption from Boundary B interruption.
- The exact startup UI shell for the minimal unsafe-install recovery path still needs implementation selection, but ownership is fixed: startup, not diagnostics, must own interrupted Boundary B and Boundary C1 recovery.
- If future implementation proves there is a tiny sub-phase inside `installing` where the live target is still untouched, that sub-phase may stay in Boundary A, but the rule cannot broaden restart-normal safety once the managed target has been emptied.
- A launcher-only kill switch is the minimal rollout control. If later QA needs local developer overrides, that can be added as a narrow secondary convenience, but it is not required for this task.

## Required Docs or Tracking or Research Updates

- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` so rollback and rollout text names the three safety boundaries, the boundary-dependent recovery rules, and the launcher-config kill switch.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md` so rollback and rollout text also names the startup-owned unsafe-install recovery surface and the narrower `Boundary C1` versus `Boundary C2` distinction.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` so later task wording reflects that:
  - `retry partial sync` is not always safe
  - `restart-normal` is boundary-dependent
  - cancellation is denied after live cutover begins
  - interrupted Boundary B and Boundary C1 recovery is startup-owned rather than diagnostics-owned
  - the rollout guard lives in launcher config, protects only new diagnostics-launched partial sync entry, and must not remove unsafe-install recovery
- No separate research note is required unless later implementation uncovers a contradiction in crash-interruption behavior.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-004-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-004-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Approved Plan

- Partial sync safety is enforced through boundary-dependent backend rules rather than renderer inference.
- Boundary A allows `retry`, `restart-normal`, and `wipe-and-full-sync` because the live managed DB remains untouched.
- Boundary B and Boundary C1 allow `wipe-and-full-sync` only because normal restart cannot be trusted once live cutover has started or the installed DB has not yet passed one successful node-start handoff.
- Boundary C2 allows `restart-normal` or `wipe-and-full-sync` after the installed DB has already survived one successful node start.
- Cancellation is allowed only before live cutover begins.
- Startup, not diagnostics, owns recovery for interrupted unsafe installs.
- `LauncherConfig` is the primary kill switch for disabling new diagnostics-launched partial sync without affecting existing empty-chain Mithril bootstrap or startup-owned unsafe-install recovery.

## Final Outcome

- Updated the PRD to encode the approved failure-containment boundaries, startup-owned interrupted-state recovery, and the LauncherConfig rollout guard.
- Updated the task graph so downstream implementation tasks inherit the boundary-dependent safety contract and `task-004` is tracked as completed.
- No new research note was added because this task codified accepted spike and repo findings rather than discovering new runtime evidence.

## Self-Review

- Scope-creep check: kept renderer copy, exact IPC payload shape, and service implementation details out of this task.
- Workflow freshness check: used the requested planning, docs, test, IPC, and frontend workflow context; no stale workflow text conflicts were found that changed the task approach.
- Manifests/tests/docs check: identified only the PRD and tasks graph as required downstream doc updates; no code or test changes are needed in the planning step itself.
- Consistency check: the boundary rules match the staged-only cutover decision from `task-002`, the fail-closed startup posture implied by `task-003`, and the live repo's existing Mithril restart-suppression and cleanup seams.
- Critique-fix check: added startup ownership for interrupted unsafe states, narrowed `Boundary C` so restart-normal is not assumed after every node-start failure, and preserved unsafe-install recovery even when the rollout kill switch disables new diagnostics-launched entry.
