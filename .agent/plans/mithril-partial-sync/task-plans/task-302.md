# Task task-302: Implement Confirmation Modal And Acknowledgement Flow

## Task

- Task ID: `task-302`
- Title: `Implement confirmation modal and acknowledgement flow`

## Why This Task Now

- `task-300` is complete, so the renderer already has a dedicated `stores.mithrilPartialSync` seam with the real backend start action and truthful active-state tracking.
- `task-301` is complete, so diagnostics already shows the recommendation copy and visible `Mithril Partial Sync` CTA, but that CTA is intentionally disabled until confirmation ownership exists.
- Live-file verification shows the next smallest truthful step is to add a confirmation owner at the diagnostics surface itself, because `MithrilPartialSyncStore.startPartialSync()` starts backend work immediately and there is still no confirmation layer between the diagnostics button and that store action.
- The plan must stay narrower than `task-303`: make the CTA actionable only through confirmation, without pulling progress or error overlay reuse forward.

## Interaction Mode

- `autonomous`
- Required user input before implementation: none.
- Required manual validation before implementation can proceed: none.
- Required evidence back from the user during implementation: none.
- Manual QA remains later `task-401` work.

## Scope

- Make the existing diagnostics CTA actionable only through a confirmation step.
- Add confirmation copy that explains:
  - Daedalus will stop the node automatically.
  - Daedalus will download verified Mithril data.
  - Daedalus will restart the node automatically on success.
  - If the attempt fails, the backend may allow retry, restart normally, or wipe and perform full Mithril sync.
- Keep confirmation ownership local to the diagnostics renderer flow instead of adding a new global modal owner.
- Keep the existing diagnostics-modal close contract explicit and unchanged while confirmation is visible:
  - the confirmation view's own cancel action returns to diagnostics
  - overlay click, ESC, and the visible close button still close the whole diagnostics dialog
- Wire backend start so it happens only after explicit confirmation.
- Keep the post-confirmation behavior minimal and truthful:
  - no progress overlay work
  - no error-action surface reuse
  - no snapshot-selection or bootstrap-only UI
- Update localized copy and the generated i18n artifacts required for committed user-facing strings.
- Add focused renderer coverage for the confirmation gate and confirmed-start handoff.

## Non-Goals

- Do not pull progress, cancel, terminal error, or recovery-action UI from `task-303` into this task.
- Do not add a new app-level modal host in `App.tsx`.
- Do not route this confirmation through `uiDialogs` or invent a second shared dialog system for diagnostics.
- Do not change backend contracts, IPC channels, or `MithrilPartialSyncStore` semantics.
- Do not add threshold logic, auto-trigger behavior, slot-lag heuristics, or renderer-owned recovery rules.
- Do not redesign the diagnostics dialog beyond what the confirmation step needs.
- Do not add Storybook, theme-expansion, or manual QA scope; those remain later work.

## Dependencies

- Required completed dependencies:
  - `task-300`
  - `task-301`
- Supporting completed prerequisites used directly by this plan:
  - `task-100`
  - `task-101`
  - `task-103`
  - `task-204`
- This task directly supports:
  - `task-303`
  - renderer verification in `task-400`

## Research Consulted

- `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`
- `.agent/plans/mithril-partial-sync/research/13-task-301-diagnostics-cta-notes.md`
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
- `understand` skill guidance was loaded first for the required repository-understanding workflow, then all material claims were verified against live files.
- `i18n-messaging` guidance was loaded because this task materially changes user-facing confirmation copy and therefore needs explicit message-catalog and locale follow-through.

## Live Repo Findings Verified For Planning

- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` already owns local class state and the current disabled partial-sync CTA rendering, so the smallest truthful acknowledgement owner can live there instead of widening store state or adding new actions.
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` exposes `startPartialSync()` and that method immediately triggers backend work. Any enabled diagnostics button must therefore sit behind a real confirmation gate.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` currently injects `mithrilPartialSync.isActive` and bootstrap blocking state into diagnostics, but it does not yet pass a start callback.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` is the real `ReactModal` owner and currently routes `onRequestClose` directly to `closeDaedalusDiagnosticsDialog.trigger`, so overlay click and ESC already mean full diagnostics-dialog exit.
- `source/renderer/app/App.tsx` mounts `DaedalusDiagnosticsDialog` directly from the app-level active-dialog system, and `DaedalusDiagnosticsDialog.tsx` itself already owns a top-level `ReactModal`.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` wires its visible close button straight to `onClose`, so the existing close affordance also means full diagnostics-dialog exit rather than an inner-view back action.
- `source/renderer/app/stores/UiDialogsStore.ts` and the shared `Dialog` component serve a different dialog system. Reusing them for this task would introduce a second modal owner inside an already-open diagnostics modal.
- Existing local confirmation ownership patterns already exist in the renderer, for example `source/renderer/app/components/wallet/transactions/Transaction.tsx`, where the component keeps confirmation visibility in local state and only calls the real destructive action on confirm.
- `source/renderer/app/components/widgets/Dialog.tsx` wraps its own modal. That pattern is suitable for normal standalone confirmation dialogs, but it is not the smallest truthful fit here because diagnostics already occupies the app-level dialog slot.
- `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` already covers the task-301 placeholder CTA state. Extending that focused spec is likely sufficient if the confirmation owner remains inside `DaedalusDiagnostics` and the start callback is injected as a prop.
- `task-301` research already recorded the i18n gotcha: new runtime strings require `yarn i18n:manage`, and locale outputs must be polished so `!!!` placeholders do not ship in user-visible diagnostics copy.

## Files Expected To Change

- `.agent/plans/mithril-partial-sync/task-plans/task-302.md`
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.scss`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
- `translations/messages.json`
- `source/renderer/app/i18n/locales/defaultMessages.json`
- `source/renderer/app/i18n/locales/en-US.json`
- `source/renderer/app/i18n/locales/ja-JP.json`
- `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md`

## Implementation Approach

1. Keep the confirmation owner local to `DaedalusDiagnostics`.
   - Add one new callback prop such as `onStartMithrilPartialSync` from `DaedalusDiagnosticsDialog.tsx` into `DaedalusDiagnostics.tsx`.
   - Keep the acknowledgement visibility in `DaedalusDiagnostics` local state, alongside the existing local restart-node UI state.
   - Do not widen `MithrilPartialSyncStore` with renderer-only modal booleans.

2. Avoid a second modal layer.
   - Do not open `uiDialogs` or mount a nested shared `Dialog` modal on top of the existing diagnostics `ReactModal`.
   - Reuse the current diagnostics modal owner and switch its inner content between the diagnostics table and a lightweight confirmation view when the CTA is clicked.
   - Keep the confirmation presentation visually aligned with existing Daedalus dialog copy and action rows, but under the same diagnostics modal owner.
   - Keep `DaedalusDiagnosticsDialog.tsx` close semantics unchanged instead of adding a container-level remapping handshake for confirmation-visible state.

3. Make confirmation the only path to backend start.
   - The diagnostics CTA becomes enabled only when no conflicting Mithril-managed work is active.
   - Clicking the CTA should only open the confirmation step.
   - The confirmation view's explicit cancel action should return to the diagnostics content without side effects.
   - Confirming should call the injected start callback exactly once and nowhere else.

4. Lock one explicit close contract for confirmation-visible state.
   - Keep overlay click and ESC behavior owned by `ReactModal` in `DaedalusDiagnosticsDialog.tsx`, which continues to close the whole diagnostics dialog.
   - Keep the visible close button in `DaedalusDiagnostics.tsx` mapped to full diagnostics-dialog close.
   - Do not add a special "back to diagnostics" remap in the container for those paths, because that would widen ownership work beyond the smallest truthful solution.
   - Only the confirmation view's own cancel button returns to diagnostics.

5. Keep the post-confirmation handoff intentionally minimal.
   - After confirm, dismiss the confirmation step back to the existing diagnostics content rather than inventing task-303 progress UI.
   - Let the existing `mithrilPartialSync.isActive` truth immediately re-disable the CTA and show the already-established blocked hint if the diagnostics dialog remains open.
   - Do not add new loading, progress, cancel, or failure-state copy in this task.

6. Keep the copy short and backend-truthful.
   - Explain the automatic stop and restart behavior directly.
   - Refer to verified Mithril data without implying the user can choose a snapshot.
   - Phrase failure actions as possible recovery options the app can offer if the attempt fails, not as renderer-guaranteed buttons in this task.

7. Prefer a minimal file footprint.
   - Keep the confirmation markup inside `DaedalusDiagnostics.tsx` unless the render method becomes materially harder to read.
   - Only extract a tiny sibling confirmation component if the final JSX is too large to keep the parent understandable.
   - No new action file is expected; existing injected stores and actions are enough.

8. Make i18n artifact handling explicit.
   - Because this task adds committed confirmation copy, run `yarn i18n:manage`.
   - Update `translations/messages.json`, `defaultMessages.json`, `en-US.json`, and `ja-JP.json` as required.
   - Polish new locale values so the confirmation copy does not remain as raw `!!!` placeholders.
   - Focused renderer assertions should prove the rendered confirmation copy does not contain `!!!` placeholders.

9. Add focused renderer verification.
   - Extend `DaedalusDiagnostics.spec.tsx` to prove the CTA opens confirmation, confirm and cancel behave correctly, and backend start is not invoked before confirmation.
   - No container-level close-handshake assertion is required in this narrowed plan because overlay click, ESC, and top-right close intentionally retain the existing full-dialog-close behavior.

## Acceptance Criteria

- Clicking the diagnostics `Mithril Partial Sync` CTA opens a confirmation step first.
- The CTA is enabled only when no conflicting Mithril-managed work is active.
- No backend partial-sync work starts when the CTA is merely clicked.
- The confirmation view's explicit cancel action returns to diagnostics without starting partial sync.
- While confirmation is visible, overlay click, ESC, and the visible close button still close the whole diagnostics dialog.
- Confirming starts partial sync through the existing renderer store handoff.
- The confirmation copy explains automatic node stop, verified Mithril data download, automatic node restart on success, and the possible failure recovery options.
- The implementation does not add progress or error overlay behavior from `task-303`.
- The implementation does not add a second modal owner through `uiDialogs`, `Dialog`, or `App.tsx`.
- New user-facing strings follow the repo's `react-intl` workflow and land with updated i18n artifacts.
- Rendered confirmation copy does not ship with `!!!` placeholders.
- Focused renderer coverage exists for the confirmation gate and confirmed-start boundary.

## Verification Plan

- Run focused Jest coverage for `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`.
- Verify directly that:
  - the CTA is enabled in the idle state
  - clicking the CTA opens the confirmation content
  - the confirmation content includes the required stop/download/restart/failure-option messaging
  - cancelling the confirmation does not call the start callback
  - confirming calls the start callback exactly once
  - no start callback is invoked before confirmation
  - rendered confirmation copy does not contain `!!!`
  - the CTA remains unavailable while partial sync is already active
  - the CTA remains unavailable while bootstrap-managed Mithril work is active
- Run `yarn i18n:manage` because this task adds committed confirmation copy.
- If broader repo checks remain blocked by unrelated issues, record that explicitly and rely on focused renderer verification plus diff inspection.

## Risks And Open Questions

- The main scope-creep risk is accidentally importing `task-303` by adding interim progress or error states once confirmation succeeds.
- A second risk is introducing a stacked-modal interaction by reusing the shared `Dialog` modal or `uiDialogs` inside the already-open diagnostics `ReactModal`.
- A third risk is planning behavior that contradicts the live modal owner. The smallest truthful contract keeps overlay click, ESC, and the visible close button as full-dialog-close paths while confirmation is visible.
- A smaller copy risk is overstating recovery actions as immediate UI in this task; the confirmation text should describe the backend-supported recovery paths without promising that task-303's recovery surface already exists.
- Open implementation choice to keep honest during coding: if keeping confirmation markup inline in `DaedalusDiagnostics.tsx` makes the class unreadable, extract only a tiny local presentational sibling and keep all ownership in the diagnostics component.

## Required Docs, Tracking, And Research Updates

- Create this canonical task plan doc at `.agent/plans/mithril-partial-sync/task-plans/task-302.md`.
- Preserve review-log paths for planning and implementation review, but do not write the review-log files during planning.
- Add `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md` after implementation to capture the approved same-owner confirmation decision and any i18n follow-up nuance for later task-303 work.
- Update `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json` only after implementation and review approval.
- No `.agent/system/architecture.md` update is expected unless implementation ends up creating a broader renderer dialog-ownership seam than currently planned.

## Review-Log Paths

- Planning review log: `.agent/plans/mithril-partial-sync/task-plans/task-302-plan-review.md`
- Implementation review log: `.agent/plans/mithril-partial-sync/task-plans/task-302-impl-review.md`

## Planning Status

- `approved`

## Build Status

- `completed`

## Final Outcome

- Approved implementation keeps `task-302` intentionally narrow:
  - diagnostics now makes the `Mithril Partial Sync` CTA actionable only through a same-owner confirmation view
  - backend partial sync still starts only after explicit confirmation
  - overlay click, ESC, and the top-right close button still exit the full diagnostics dialog while confirmation is visible
  - progress, cancel, terminal error, and recovery-action surfaces remain deferred to `task-303`
- Final implementation changed:
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.scss`
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
  - `translations/messages.json`
  - `source/renderer/app/i18n/locales/defaultMessages.json`
  - `source/renderer/app/i18n/locales/en-US.json`
  - `source/renderer/app/i18n/locales/ja-JP.json`
  - `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md`
- Verification completed:
  - `yarn i18n:manage`
  - `yarn test:jest "source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx"`
- Final review result:
  - `Code Review: Iteration 2` approved in `.agent/plans/mithril-partial-sync/task-plans/task-302-impl-review.md`
- Follow-up boundary preserved for `task-303`:
  - no new modal host was introduced
  - no progress or error overlay behavior was added
  - backend recovery options are described in confirmation copy only and not yet rendered as task-303 UI

## Self-Review

- Scope-creep check: keeps progress or error overlay reuse, new modal-host work, backend contract changes, and Storybook/theme work out of `task-302`.
- Workflow freshness check: frontend, test, doc-update, understand, and i18n guidance were applied only where they materially affect this confirmation task.
- Missing-tests/docs check: the plan now requires focused renderer assertions, explicit i18n artifact handling, and one follow-up research note instead of leaving any of those ambiguous.
- Consistency check: the plan matches the PRD, the task graph, the task-300 and task-301 research notes, and the live-file fact that diagnostics already owns the active app-level modal while `startPartialSync()` immediately starts backend work.
