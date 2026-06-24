# task-ux-303 — Research note (decision-style confirmation modal)

Captures the verified design facts behind the D6 modal conversion so downstream tasks
(task-ux-302 deep-link, task-ux-501/D8 i18n cleanup) have a single reference.

## DD1 — Nested `widgets/Dialog` gives scrim + focus trap for free
`MithrilPartialSyncConfirmation` is now rendered through `source/renderer/app/components/widgets/Dialog.tsx`,
which renders a react-polymorph `Modal` (skin `ModalSkin`) → `react-modal` under the hood. That provides
the scrim (`.ReactModal__Overlay`) and focus trap D6 requires without a bespoke overlay. This is a
**nested** `react-modal`: it stacks above the outer diagnostics `ReactModal`
(`source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx:105`). `ariaHideApp: false` is set by
`ModalSkin`, so unit tests render it bare (no `ReactModal.setAppElement` needed) and the portal lands in
`document.body`.

## DD2 — ESC / scrim-click routes to "Back to diagnostics", never the outer dialog
`ModalSkin` wires `onRequestClose={props.onClose}` and
`shouldCloseOnOverlayClick={props.triggerCloseOnOverlayClick}`. The Dialog passes `onClose={onCancel}`
(Section maps `onCancel → hideConfirmation`) and `closeOnOverlayClick`. Because `react-modal` delivers ESC
and overlay-clicks to the **topmost** open modal (the confirmation), the outer diagnostics
`ReactModal.onRequestClose` does NOT fire while the inner modal is open — so ESC/scrim returns to the
recommendation row instead of tearing down the whole diagnostics dialog (gap #23 fixed). No change to the
container's `onRequestClose` was needed. The Section's existing `onRestoreFocus` →
`restoreDialogCloseOnEscKey` (`DaedalusDiagnostics.tsx:878-890`) still refocuses the diagnostics modal
content AFTER the inner modal closes, which remains correct.

Tested at two levels: Confirmation-level (`fireEvent.keyDown` Escape on `.ReactModal__Content` calls
`onCancel`) and Section-level (ESC returns the recommendation copy to screen).

## DD3 — Behind-ness line consumes `behindByImmutables` verbatim (lock #5)
The modal formats `behindByImmutables` via a plain `{count}` interpolation when it is a finite number, and
falls back to a generic line otherwise. It performs NO threshold comparison — PRD D2 owns the threshold;
the renderer only formats the opaque figure. `undefined`/non-finite never renders the string "undefined".

## `showConfirmationOnOpen` handoff seam (consumed by task-ux-302)
`MithrilPartialSyncSection` now accepts `showConfirmationOnOpen?: boolean` and, in `componentDidMount`,
opens the confirmation **once** when it is true AND `!isActionBlocked` (calls `showConfirmation()`, which
itself no-ops when blocked). This opens the **confirmation modal** — never a second start path; confirmation
still precedes start (lock #3), no auto-confirm/auto-start.

The container `DaedalusDiagnosticsDialog.tsx` currently passes the literal
`showMithrilPartialSyncConfirmationOnOpen={false}`. **task-ux-302 replaces this single literal `false`** with
the real deep-link boolean from the dialog-open payload. That is the one and only wiring point 302 needs to
touch for the proactive "Review" deep-link.

## Retained-but-unrendered message ids (for task-ux-501 / D8 i18n cleanup)
`mithrilPartialSyncConfirmationIntro` and `mithrilPartialSyncConfirmationSuccess` are kept in the
`defineMessages` block and the catalogs but are **no longer rendered** (superseded by the three discrete
`Step*` keys). They are intentionally NOT deleted here — orphan i18n cleanup is task-ux-501/D8. Do not
double-remove them.

## Environment note (pre-existing sandbox defect)
`require('sass')` (dart-sass 1.44.0, `sass.default.dart.js`) throws "Invalid or unexpected token" under
Node v24 in this sandbox. This breaks (a) `yarn compile`'s `typedef:sass` precompile hook and (b)
`jest-css-modules-transform` ("Can't find sass or node-sass module") for ANY spec importing `.scss` —
including untouched specs like `MithrilStepIndicator.spec.tsx`. It is environmental, not introduced by this
task. Real gates used: `tsc --noEmit` directly (passes, exit 0) and jest run with `.scss` mapped to
`identity-obj-proxy` (all 29 touched-spec tests pass). The committed jest config remains unchanged.
