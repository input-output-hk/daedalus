# Task-302 Confirmation And Acknowledgement Notes

- Purpose: record the durable renderer and i18n decisions from `task-302` for later diagnostics and progress-overlay work.
- Date: 2026-05-21

## Durable Findings

- `task-302` keeps confirmation ownership local to `source/renderer/app/components/status/DaedalusDiagnostics.tsx` instead of widening `MithrilPartialSyncStore`, `uiDialogs`, or `App.tsx`.
- The diagnostics flow now has one truthful start boundary:
  - clicking the diagnostics CTA opens confirmation only
  - confirming is the first path that calls `stores.mithrilPartialSync.startPartialSync()`
  - cancelling returns to diagnostics without backend side effects
- The approved close contract stays minimal and matches the live modal owner:
  - the confirmation view's explicit cancel button returns to diagnostics
  - overlay click, ESC, and the top-right close button still close the full diagnostics dialog because `DaedalusDiagnosticsDialog.tsx` remains the `ReactModal` owner
  - later tasks should not assume those close paths are an in-dialog back action unless they deliberately change the owner contract
- Confirmation copy must explicitly mention downloading verified Mithril data, not only restoring it. Review found that weaker wording is not sufficient for this acknowledgement step.
- `yarn i18n:manage` still needs manual locale polish for new runtime strings. The new `en-US` and `ja-JP` confirmation entries required a follow-up edit after catalog generation so rendered copy did not keep `!!!` placeholders.

## Verification Evidence

- Focused Jest coverage passed:
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
- The spec now locks:
  - CTA enabled/disabled gating
  - confirmation opens before backend start
  - cancel returns to diagnostics
  - confirm starts partial sync exactly once
  - rendered confirmation copy contains no `!!!` placeholders
- i18n workflow passed:
  - `yarn i18n:manage`

## Follow-Ups

- `task-303` can reuse this same diagnostics owner contract when it adds progress/error surfaces, but it should avoid silently changing overlay/ESC/top-right close behavior unless that new behavior is planned and tested explicitly.
- The old `daedalus.diagnostics.dialog.mithrilPartialSyncButtonHint` catalog entry remains unused after the new ready-state hint landed. That is non-blocking cleanup and should be handled only if a later task is already touching the same diagnostics copy surface.
