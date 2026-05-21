# Task task-301: Add Diagnostics Recommendation Copy And Manual Action

## Task

- Task ID: `task-301`
- Title: `Add diagnostics recommendation copy and manual action`

## Why This Task Now

- `task-300` is complete, so the renderer now has a dedicated `stores.mithrilPartialSync` seam with truthful backend-owned active-state and recovery data.
- The next smallest truthful UX slice is the diagnostics entry point itself: show users the existing sync context they already rely on and add the visible Mithril partial-sync affordance without pulling confirmation-modal ownership or backend start forward.
- Live-file verification found the key ownership boundary that forces a scope reduction here: `MithrilPartialSyncStore.startPartialSync()` already starts backend work immediately, and the diagnostics dialog has no intermediate confirmation owner yet, so `task-301` cannot truthfully ship an enabled start button without absorbing `task-302` behavior.
- The canonical plan doc for `task-301` did not exist yet under `.agent/plans/mithril-partial-sync/task-plans/`, so this planning pass also closes that tracking gap.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Required manual validation before implementation can proceed: none.
- Required evidence back from the user during implementation: none.
- Manual QA remains later `task-401` work.

## Scope

- Extend `DaedalusDiagnostics` to render a small Mithril partial sync recommendation area near the existing sync diagnostics rows.
- Add a visible manual `Mithril Partial Sync` CTA in diagnostics as a not-yet-actionable placeholder seam for the later confirmation flow.
- Keep the CTA unavailable in this task: it stays disabled both because confirmation ownership is not added yet and while Mithril-managed work is active.
- Use already available renderer sync context only:
  - `isSynced`
  - `isNodeInSync`
  - `isNodeSyncing`
  - `syncPercentage`
  - existing local-tip and network-tip context if helpful for copy placement
- Add only the minimum container wiring needed to pass recommendation and CTA state into the diagnostics component.
- Add only the minimum styling needed for the new recommendation block and button state inside the existing diagnostics dialog layout.
- Add user-facing copy through the normal `react-intl` message pattern already used in `DaedalusDiagnostics.tsx`.
- Run the repo's i18n artifact workflow in this task for any newly committed messages: `yarn i18n:manage`, carrying the resulting `translations/messages.json` and locale placeholder updates in `source/renderer/app/i18n/locales/en-US.json` and `source/renderer/app/i18n/locales/ja-JP.json` if extraction/checking changes them.

## Non-Goals

- Do not add threshold logic, auto-trigger rules, slot-lag heuristics, or any renderer-side decision engine.
- Do not start partial sync directly from this task.
- Do not add any intermediate click-owner or hidden callback shim that effectively opens or owns confirmation behavior; that remains `task-302`.
- Do not pull the confirmation modal or acknowledgement flow from `task-302` into this task.
- Do not add progress, cancel, failure-action, or overlay reuse from `task-303`.
- Do not widen backend contracts, IPC channels, or store semantics.
- Do not move or redesign the broader diagnostics dialog structure beyond what the new recommendation row needs.
- Do not introduce Storybook or theme-expansion work; those remain `task-304` scope.

## Dependencies

- Required completed dependency:
  - `task-300`
- Supporting completed prerequisites used by this plan:
  - `task-100`
  - `task-101`
  - `task-103`
  - `task-204`
- This task directly supports:
  - `task-302`
  - `task-303`
  - renderer coverage in `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`
- `.agent/plans/mithril/mithril-snapshot-ux.md`
- `.agent/plans/mithril/research/mithril-bootstrap-ui-model.md`

## Docs, Workflows, And Skills Consulted

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/test.md`
- `.agent/workflows/update-doc.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`
- `understand` skill guidance was loaded first for non-trivial repo understanding, then all task claims were verified against live files.
- `i18n-messaging` guidance materially affects this task because it adds user-facing diagnostics copy and therefore should follow both the `defineMessages()` pattern and the repo's explicit `yarn i18n:manage` artifact workflow.

## Live Repo Findings Verified For Planning

- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` already owns a large local `defineMessages()` block, so the smallest truthful copy change is to add new diagnostics-local messages there rather than introducing a new message module for this task alone.
- `DaedalusDiagnostics` currently has no Mithril partial sync props or UI. The component is still class-based, renders the diagnostics tables directly, and already owns the cardano-node restart CTA in the section header.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` currently injects only `app` and `networkStatus` data into the diagnostics dialog. It does not yet read `stores.mithrilPartialSync` or `stores.mithrilBootstrap`.
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` exposes `isActive`, `isWorking`, and status state, but `startPartialSync()` immediately triggers backend work. That makes direct button-to-store-start wiring incompatible with the locked task boundary that confirmation belongs to `task-302`.
- `source/common/types/mithril-bootstrap.types.ts` already exports `isMithrilBootstrapBlockingNodeStart(status)`, which is the smallest truthful helper for bootstrap-side CTA gating.
- `source/common/types/mithril-partial-sync.types.ts` already exports `isMithrilPartialSyncBlockingNodeStart(status)`, but for this task the simpler live renderer truth is the store's existing `mithrilPartialSync.isActive` flag because the CTA remains disabled until `task-302` adds confirmation ownership anyway.
- `source/renderer/app/components/status/DaedalusDiagnostics.scss` already styles section-header buttons and layout rows, so the smallest styling change is to add a small recommendation block and CTA modifiers inside the existing module instead of introducing a new stylesheet.
- No existing renderer Jest coverage currently targets `DaedalusDiagnostics` or `DaedalusDiagnosticsDialog`, so this task will likely need a new focused component or container spec rather than extending an existing diagnostics test.
- `package.json` and the loaded i18n guidance confirm that committed new messages are expected to run through `yarn i18n:manage`, and `check:all` also includes that command.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-301.md`
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.scss`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
- one new focused renderer spec for diagnostics CTA rendering and gating, likely under one of:
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.tsx`
- `translations/messages.json`
- `source/renderer/app/i18n/locales/en-US.json`
- `source/renderer/app/i18n/locales/ja-JP.json`

## Implementation Approach

1. Add a diagnostics-local recommendation block, not a new flow owner.
   - Render the new content near the existing `Daedalus status` sync rows so the user sees current sync context and the manual action together.
   - Keep the copy short and truthful: surface current sync state and explain that Mithril partial sync is a manual recovery option if the user believes catch-up is taking too long.
   - Avoid any copy that implies Daedalus has determined the user is definitely far enough behind.

2. Reduce the CTA to a truthful placeholder in this task.
   - Render the new button label now so the diagnostics surface visibly converges on the intended flow.
   - Keep the button disabled in `task-301` rather than inventing an enabled no-op or a hidden handoff owner.
   - Do not add a new click callback prop yet, because the current diagnostics button pattern expects concrete immediate handlers and adding a fake seam here would only shift `task-302`'s ownership without delivering user value.
   - `task-302` should be the first task that makes the CTA actionable by introducing the confirmation-opening owner and only then wiring the confirmed path to backend start.

3. Gate CTA disabled-state from existing store truth only.
   - Keep the CTA disabled unconditionally in this task because confirmation ownership is intentionally absent.
   - Still surface truthful disabled reasoning in code comments/tests by deriving conflicting-operation state from existing store truth only.
   - Use `stores.mithrilPartialSync.isActive` for partial-sync-side blocking.
   - Use shared helper `isMithrilBootstrapBlockingNodeStart(stores.mithrilBootstrap.status)` for bootstrap-side blocking instead of duplicating a renderer-local bootstrap status list.
   - Do not introduce any sync-percentage-based enablement logic.

4. Keep copy logic simple and explicit.
   - Prefer one or two recommendation variants driven by existing booleans already in diagnostics, for example synced vs not-synced context, without introducing numeric thresholds.
   - If `syncPercentage` is interpolated into copy, treat it only as surfaced context for the user, not as a trigger.
   - Reuse existing formatter utilities already used by diagnostics when showing any sync value.

5. Keep styling minimal and local.
   - Add a small recommendation container and CTA styling inside `DaedalusDiagnostics.scss`.
   - Reuse the existing diagnostics button language and section spacing so the new block feels native to the current modal instead of like a separate feature card.

6. Add focused renderer verification.
   - Add a small spec that proves:
     - recommendation copy renders from existing sync props
     - the CTA exists
     - the CTA is disabled in the base task-301 state because confirmation ownership is not present yet
     - the CTA remains disabled while partial sync is active
     - the CTA remains disabled while bootstrap work is active
   - Keep confirmation-modal assertions and start-action assertions out of this task's tests.

7. Make i18n artifact handling explicit.
   - Because this task adds committed diagnostics copy, run `yarn i18n:manage` in this task instead of deferring catalog sync.
   - Commit the generated `translations/messages.json` diff and any locale placeholder changes produced in `en-US.json` and `ja-JP.json`.
   - If `yarn i18n:manage` is blocked by an unrelated repo issue, record that explicitly in implementation notes and keep the generated source-message diff truthful; do not leave the artifact decision ambiguous.

## Acceptance Criteria

- Diagnostics shows Mithril partial sync recommendation copy near existing sync diagnostics.
- Diagnostics shows a visible manual `Mithril Partial Sync` CTA.
- The CTA does not contain threshold or auto-trigger logic.
- The CTA remains disabled in `task-301`, so the task does not ship an enabled no-op button and does not start backend partial sync directly.
- The CTA also remains disabled while Mithril partial sync is already active.
- The CTA also remains disabled while bootstrap-side Mithril-managed work is active.
- The task does not add confirmation-modal behavior.
- New copy follows the repo's `react-intl` message pattern.
- `yarn i18n:manage` is part of this task's intended verification/artifact workflow.
- Focused renderer coverage exists for rendering and CTA disabled-state gating.

## Verification Plan

- Run focused Jest coverage for the touched diagnostics component or container spec.
- Verify directly that:
  - recommendation copy appears in the diagnostics dialog with the intended sync-context props
  - the CTA renders
  - the CTA is disabled in the baseline `task-301` state
  - the CTA remains disabled when `mithrilPartialSync.isActive` is true
  - the CTA remains disabled when bootstrap status is in a helper-recognized blocking state
- Run `yarn i18n:manage` because this task adds committed user-facing copy.
- If broader repo checks stay blocked by unrelated existing issues, record that and rely on focused renderer verification plus diff inspection.

## Risks And Open Questions

- The main scope-creep risk is accidentally letting the new CTA call `startPartialSync()` directly, or introducing a synthetic click-owner seam that effectively does `task-302` early.
- A second risk is over-designing the recommendation copy into a hidden heuristic. This task must only surface already-known sync context and leave the decision with the user.
- Bootstrap-active gating needs to be derived from the shared helper so the disabled-state reasoning matches startup-owned bootstrap truth rather than a duplicated renderer list.
- Intentional tradeoff: this task leaves the CTA disabled even in the idle case to keep task ownership truthful. Making it actionable is explicitly deferred to `task-302`.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-301.md`.
- Preserve review-log paths for planning and implementation review, but do not write the review logs during planning.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- No `.agent/system/architecture.md` update is expected for this task unless implementation ends up adding a durable diagnostics ownership seam that changes renderer structure materially.
- Add a task-scoped research note capturing the narrowed disabled-CTA boundary and the i18n gotcha uncovered during review so later renderer tasks do not repeat that work.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-301-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-301-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- Approved implementation keeps `task-301` intentionally narrow:
  - diagnostics now shows Mithril partial-sync recommendation copy next to existing sync context
  - diagnostics now shows a visible `Mithril Partial Sync` CTA placeholder
  - the CTA remains disabled in this task so confirmation ownership stays with `task-302`
  - bootstrap and partial-sync conflicting work are surfaced through existing renderer truth only
- Final implementation changed:
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.scss`
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - `translations/messages.json`
  - `source/renderer/app/i18n/locales/defaultMessages.json`
  - `source/renderer/app/i18n/locales/en-US.json`
  - `source/renderer/app/i18n/locales/ja-JP.json`
  - `.agent/plans/mithril-partial-sync/research/13-task-301-diagnostics-cta-notes.md`
- Verification completed:
  - `yarn test:jest "source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx"`
  - `yarn i18n:manage`
- Final review result:
  - `Code Review: Iteration 3` approved in `.agent/plans/mithril-partial-sync/task-plans/task-301-impl-review.md`
- Follow-up boundary preserved for `task-302`:
  - no hidden confirmation owner was introduced
  - no backend start wiring was added
  - the CTA becomes actionable only when the confirmation modal is implemented later

## Self-Review

- Scope-creep check: keeps threshold logic, backend start wiring, confirmation modal work, and progress or error overlay reuse out of `task-301`.
- Workflow freshness check: frontend, test, doc-update, and i18n guidance are current and were applied only where they affect this diagnostics-copy task.
- Missing-tests/docs check: the plan now requires focused renderer coverage and explicit `yarn i18n:manage` artifact handling instead of leaving either ambiguous.
- Consistency check: the plan now matches the PRD, task graph, task-300 store notes, shared helper seams, and the live-store fact that direct CTA-to-start wiring would violate the task-302 boundary.
