Planner: Iteration 1
Timestamp: 2026-05-21T15:20:39Z

- Read in the requested order: `.agent/readme.md`, `.agent/system/architecture.md`, `.agent/workflows/frontend.md`, `.agent/workflows/test.md`, `.agent/workflows/update-doc.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-prd.md`, `.agent/plans/mithril-partial-sync/mithril-partial-sync-tasks.json`, `.agent/plans/mithril-partial-sync/research/12-task-300-renderer-store-and-app-wiring-notes.md`, `.agent/plans/mithril-partial-sync/research/13-task-301-diagnostics-cta-notes.md`, then the historical Mithril UX context and live in-scope renderer files.
- Loaded `understand` first for repository-understanding workflow expectations, then verified all material planning claims directly against live files before writing the plan. Loaded `i18n-messaging` because this task materially changes localized confirmation copy.
- Created the canonical task plan at `.agent/plans/mithril-partial-sync/task-plans/task-302.md`.
- Classified interaction mode as `autonomous` because no user decision or manual prerequisite is required before implementation; manual QA remains later `task-401` work.
- Important live-file findings that drove the plan:
  - `source/renderer/app/stores/MithrilPartialSyncStore.ts` already exposes `startPartialSync()` and it starts backend work immediately, so confirmation must be a real gate and not a cosmetic step.
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx` already owns local class state and the currently disabled CTA, so the smallest truthful confirmation owner can stay local there.
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` and `source/renderer/app/App.tsx` confirm diagnostics already owns an app-level `ReactModal`.
  - `source/renderer/app/stores/UiDialogsStore.ts` and `source/renderer/app/components/widgets/Dialog.tsx` represent a separate modal system; reusing them here would create a second competing modal owner inside the existing diagnostics modal.
  - Existing renderer precedent supports local confirmation ownership before calling a real action, for example `source/renderer/app/components/wallet/transactions/Transaction.tsx`.
- Approved implementation direction is intentionally narrow:
  - make the existing diagnostics CTA actionable only through a confirmation step
  - keep confirmation ownership local to diagnostics rather than adding a new global host
  - pass one real start callback from `DaedalusDiagnosticsDialog.tsx` into `DaedalusDiagnostics.tsx`
  - enable the CTA only when no conflicting Mithril-managed work is active
  - do not start backend work until explicit confirmation
  - return to diagnostics cleanly on cancel with no side effects
  - do not pull progress or error overlay reuse from `task-303`
- Planned file scope is minimal and renderer-only:
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.scss`
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - i18n artifacts: `translations/messages.json`, `source/renderer/app/i18n/locales/defaultMessages.json`, `source/renderer/app/i18n/locales/en-US.json`, `source/renderer/app/i18n/locales/ja-JP.json`
  - follow-up research note: `.agent/plans/mithril-partial-sync/research/14-task-302-confirmation-acknowledgement-notes.md`
- Acceptance and verification in the plan are explicit:
  - clicking the CTA opens confirmation first
  - no backend start happens on the initial CTA click
  - cancel/close does not start partial sync
  - confirm starts partial sync exactly through the existing store handoff
  - confirmation copy explains automatic stop, verified Mithril data download, automatic restart, and possible failure recovery options
  - focused Jest coverage extends `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - `yarn i18n:manage` is required for this task
- Self-review completed before finalizing the plan:
  - scope creep: blocked `task-303` progress/error work and any new modal-host design
  - stale workflow text: none carried forward
  - missing tests/docs: covered in the plan
  - plan consistency: aligned with the PRD, task graph, task-300/task-301 research notes, and the live renderer/modal ownership seams

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-05-21T15:23:26Z

- Broad pass result: the plan is mostly well-scoped, stays faithful to the `task-302` / `task-303` boundary, and correctly prefers the existing diagnostics modal owner over introducing `uiDialogs` or a second modal system. It also correctly treats `MithrilPartialSyncStore.startPartialSync()` as an immediate side-effect boundary that must stay behind explicit confirmation.

Blocking findings:
- `task-302.md:115-124,152-156` says confirmation will live entirely inside `DaedalusDiagnostics` and that cancelling or closing confirmation returns to diagnostics, but the actual modal owner is `ReactModal` in `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx:84-91`, which currently routes overlay click and ESC straight to `closeDaedalusDiagnosticsDialog.trigger`. `source/renderer/app/components/status/DaedalusDiagnostics.tsx:621-625` also wires the visible close button to full dialog close. As written, the plan does not define the required owner handshake for confirmation-visible close behavior, so its acceptance cannot be met truthfully with the current seam. Update the plan to make one explicit contract: either `DaedalusDiagnosticsDialog` becomes aware of confirmation state and remaps `onRequestClose` to “return to diagnostics” while confirmation is visible, or the acceptance is narrowed so overlay/ESC/close intentionally exits the whole diagnostics dialog. If the first path is kept, the verification plan also needs at least one container-level assertion for `onRequestClose` behavior, not only the component spec.

Non-blocking observations:
- `task-302.md:95-106` includes `.agent/.../task-302.md` and the follow-up research note in expected file changes. That is fine for task bookkeeping, but it is not part of the minimum product slice.
- `mithril-partial-sync-tasks.json:476-480` still points broadly at `source/renderer/app/actions/`, while the task plan correctly narrows away from new actions. The narrower plan is the better interpretation.
- `task-302.md:141-148` correctly requires `yarn i18n:manage`, but adding an explicit “no `!!!` placeholders in rendered confirmation copy” assertion would better lock in the task-301 i18n gotcha.

Approval bar:
- Make the modal-close ownership path explicit and testable: define what overlay click, ESC, and the close affordance do while confirmation is visible, then align acceptance and verification with that exact behavior.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-05-21T15:25:18Z

- Re-read the requested inputs in context: `.agent/plans/mithril-partial-sync/task-plans/task-302.md`, `.agent/plans/mithril-partial-sync/task-plans/task-302-plan-review.md`, `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`, and the live diagnostics component around CTA and close behavior in `source/renderer/app/components/status/DaedalusDiagnostics.tsx`.
- Verified the blocker directly against live ownership seams before revising the plan:
  - `DaedalusDiagnosticsDialog.tsx:84-91` is the real `ReactModal` owner and currently routes `onRequestClose` directly to `closeDaedalusDiagnosticsDialog.trigger`, so overlay click and ESC already mean full dialog exit.
  - `DaedalusDiagnostics.tsx:621-625` wires the visible close button directly to `onClose`, so that affordance also means full diagnostics-dialog exit, not an inner confirmation back action.
- Updated the canonical plan at `.agent/plans/mithril-partial-sync/task-plans/task-302.md` to lock one explicit and minimal ownership contract instead of implying a return-to-diagnostics behavior for all close paths.
- The revised contract is now explicit:
  - the confirmation view’s own cancel action returns to diagnostics
  - overlay click, ESC, and the visible close button continue to close the whole diagnostics dialog while confirmation is visible
  - no container-level `onRequestClose` remapping handshake is added
- Why this is the smallest truthful solution:
  - it matches the current `ReactModal` owner and `DialogCloseButton` wiring exactly
  - it avoids widening task-302 into extra modal-owner orchestration that is not needed to make confirmation actionable
  - it keeps task-302 narrow and still separate from `task-303`
- Revised live-file findings were added to the plan to support the new contract:
  - `DaedalusDiagnosticsDialog.tsx` is now called out explicitly as the `ReactModal` close owner
  - `DaedalusDiagnostics.tsx` is now called out explicitly as keeping the top-right close affordance mapped to full dialog close
- Revised implementation approach in the plan:
  - keep confirmation local to `DaedalusDiagnostics`
  - keep `DaedalusDiagnosticsDialog.tsx` close semantics unchanged
  - use only the confirmation view’s explicit cancel action as the “return to diagnostics” path
  - do not add a container-level remap for overlay/ESC/close
- Acceptance and verification were aligned to the revised contract:
  - removed the over-broad “cancelling or closing confirmation returns to diagnostics” statement
  - replaced it with:
    - explicit cancel returns to diagnostics
    - overlay click, ESC, and visible close still close the whole diagnostics dialog
  - kept verification focused at the component level because the narrowed contract intentionally avoids a container handshake
- Also incorporated the natural i18n tightening from the critique:
  - the plan now explicitly requires rendered confirmation copy to avoid `!!!` placeholders
  - verification now includes a focused assertion that rendered confirmation copy does not contain `!!!`
- Self-review after the fix pass:
  - blocker resolved: close behavior now matches the live owner truthfully
  - scope creep still blocked: no task-303 progress/error work and no new modal-host logic
  - tests/docs remain covered and consistent with the revised ownership contract

Outcome: Canonical `task-302` plan revised and finalized around the explicit minimal close contract: confirmation cancel returns to diagnostics, while overlay click, ESC, and the visible close button still exit the diagnostics dialog.

