# task-ux-303 — Implementation review log (append-only)

Implementation:
Timestamp: 2026-06-24T16:26:30Z
Outcome: Implemented PRD D6 — replaced the in-place prose confirmation row-swap with a focus-trapped,
scrim-backed decision-style modal carrying behind-ness context. Executed the approved canonical plan
(`task-ux-303.md`) step by step. Autonomous mode (no blocking user question; D6 copy fully specified).

Changes (per file):
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` — converted from an in-place
  `<div>` class component into a nested `widgets/Dialog` (react-polymorph `Modal` → `react-modal`), giving
  scrim + focus trap. Added `behindByImmutables?: number` prop with an undefined/non-finite-safe behind-ness
  line (never renders "undefined"; no threshold compare — lock #5). Replaced the three free-text paragraphs
  with a 3-step `<ol>` (stop → download+verify → restart) plus the retained `recovery` note; `stepDownload`
  keeps the locked "downloads and verifies Mithril data" wording (lock #4). Distinct primary "Start Mithril
  partial sync" (`primary: true` + `mithrilPartialSyncConfirmationPrimaryButton`) vs secondary "Back to
  diagnostics" (`flat` + `mithrilPartialSyncConfirmationSecondaryButton`) via `Dialog.actions` — fixes the
  shared-cancel-style bug (#14). `onClose={onCancel}` + `closeOnOverlayClick` + `DialogBackButton(onBack)`
  make ESC/scrim/back-arrow = back to diagnostics (#23). `primaryButtonAutoFocus` for focus-trap entry.
  `startError` still rendered. Added 5 new `!!!`-prefixed message keys: `behind` ({count}), `behindUnknown`,
  `stepStop`, `stepDownload`, `stepRestart`. Retained (unrendered) `intro`/`success` ids for task-ux-501/D8.
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — added `behindByImmutables?: number`
  and `showConfirmationOnOpen?: boolean` props. `componentDidMount` opens the confirmation once when
  `showConfirmationOnOpen && !isActionBlocked` (deep-link seam for task-ux-302; opens the confirmation, not a
  start path — lock #3). Threaded `behindByImmutables` into the rendered `MithrilPartialSyncConfirmation`. The
  `isShowingConfirmation` branch still precedes the `!shouldShowRecommendation → null` gate, so a deep-linked
  open renders regardless of the recommendation gate (verified, not reordered).
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — added
  `mithrilPartialSyncBehindByImmutables?: number` and `showMithrilPartialSyncConfirmationOnOpen?: boolean`
  props, destructured them, and passed them into `MithrilPartialSyncSection` as `behindByImmutables` /
  `showConfirmationOnOpen`.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — passes
  `mithrilPartialSyncBehindByImmutables={mithrilPartialSync.behindByImmutables}` (store already exposes the
  `@observable` from task-ux-301) and the explicit placeholder
  `showMithrilPartialSyncConfirmationOnOpen={false}`. The literal `false` is the single wiring point
  task-ux-302 replaces with the real deep-link boolean.
- `source/renderer/app/components/status/DaedalusDiagnostics.scss` — added decision-modal classes
  (`...Dialog`, `...Behind`, `...Steps`, `...Recovery`, `...PrimaryButton`, `...SecondaryButton`). Left the
  now-unused `...CancelButton`/`...Actions`/`...Title` classes in place (smaller diff; dead-scss removal is
  optional and deferred). The gitignored `.scss.d.ts` sidecar was updated locally for parity but is not
  committed (auto-generated, typed `any` via the global `*.scss` module declaration, so not a compile gate).
- i18n: added first-class EN strings (no `!!!`) to `en-US.json` and placeholder JA (no `!!!`, `{count}`
  preserved) to `ja-JP.json`, both in alphabetical position within the `...Confirmation*` block. Ran
  `yarn i18n:extract` → `translations/messages.json` regenerated; the 5 new ids are present.
- Tests: NEW `MithrilPartialSyncConfirmation.spec.tsx` (9 tests: modal+scrim, ESC→onCancel, behind-ness with
  42 and undefined-fallback, verified-data wording, distinct primary/secondary classes, Start is the only
  start path incl. disabled-when-blocked, startError render). Updated `MithrilPartialSyncSection.spec.tsx`
  (added the two new defaultProps + 5 tests: mount-open without start, deep-link open when
  `shouldShowRecommendation:false`, no deep-link open when blocked, behind-ness thread-through, ESC returns to
  recommendation). Updated `DaedalusDiagnostics.spec.tsx` defaultProps with the two new optional props.

Verification (honest results):
- `tsc --noEmit` (real TS gate): PASS — exit 0, zero errors project-wide (all touched files clean).
- `yarn compile`: FAIL — but ONLY because the `typedef:sass` precompile hook crashes on
  `node_modules/sass/sass.default.dart.js` ("Invalid or unexpected token") under Node v24. This is a
  pre-existing, environment-wide dart-sass/Node-v24 sandbox defect (same root cause documented in task-ux-301
  and the plan's Risks). It is NOT a TypeScript error; `tsc --noEmit` is the authoritative gate and passes.
- `eslint` (touched .ts/.tsx): PASS — exit 0, 0 errors. Only pre-existing warnings (`no-explicit-any`,
  `@ts-ignore`, unused legacy imports, empty-arrow in the pre-existing unmount test) — none in new logic.
- `yarn test:jest` (3 specs): PASS — 29/29 tests green (9 Confirmation + 12 Section + 8 Diagnostics). Because
  `jest-css-modules-transform` cannot load sass under Node v24 (same defect; breaks even untouched
  scss-importing specs like `MithrilStepIndicator.spec.tsx`), the run used a CLI override mapping `.scss` →
  `identity-obj-proxy`. The committed jest config is unchanged; class-name assertions target both the
  Dialog-added literal `primary`/`flat` classes and the module class names, which are preserved under both
  transforms.
- `prettier --check` (touched files): PASS — write-fixed `MithrilPartialSyncConfirmation.tsx`,
  `DaedalusDiagnostics.tsx`, and `MithrilPartialSyncConfirmation.spec.tsx`; re-check clean (the formatter also
  reflowed some long message `id` lines to multiline — cosmetic, consistent with the codebase).
- `yarn i18n:extract`: ran clean ("Done in 9.36s"); 5 new ids in `messages.json`; no `!!!` leakage on the new
  EN/JA runtime values (the 41/51 baseline `!!!` hits are pre-existing, none on partial-sync-confirmation
  keys); all 3 JSON catalogs parse.

Deviations from the plan: none substantive.
- Per the plan's own contingency, the TS gate was run as `tsc --noEmit` directly (since `yarn compile`'s sass
  hook crashes pre-existingly), and jest was run with `.scss` identity-mapped (since the committed
  `jest-css-modules-transform` cannot load sass under Node v24). Both are environment workarounds the plan
  anticipated; no product/source behavior changed.
- The `.scss.d.ts` sidecar is gitignored/auto-generated, so it is not staged (plan Step 5c allowed for this).

Locked invariants upheld: #3 (confirmation is the only start path; mount-open shows the confirmation, never
auto-starts), #4 ("verifies/verified" retained in `stepDownload` + `recovery`), #5 (no threshold compare;
opaque `{count}`; undefined → generic fallback). Non-goals honored (no snapshot/storage picker; styling-only).
Empty-chain bootstrap untouched (`MithrilDecisionView.tsx`/`MithrilBootstrap.tsx` read-only).

---

Code Review: (round 1 of up to 5)
Timestamp: 2026-06-24T16:31:30Z

Reviewed the full working-tree diff against the approved task-ux-303 plan and PRD D6. Independently
re-ran verification — results match the implementation log.

D6 / gap coverage (all delivered):
- #22 real modal: confirmation now renders through `widgets/Dialog` (react-polymorph `Modal` →
  `react-modal` via `ModalSkin`), giving scrim + focus trap. Old in-place `<div>` removed.
- #23 ESC/scrim-click: `Dialog onClose={onCancel}` is wired to `react-modal onRequestClose`
  (verified in ModalSkin.js:19). The inner (topmost) modal captures ESC, so the outer diagnostics
  ReactModal does not close; `onCancel` → `hideConfirmation` returns to the recommendation row. The
  `isShowingConfirmation` branch (Section :116) precedes the `!shouldShowRecommendation` null gate
  (:128), so a deep-linked open is shown regardless of the gate. Section-level ESC test asserts the
  recommendation copy returns; Confirmation-level ESC test asserts `onCancel` fires once.
- #11/#29 behind-ness: `behindByImmutables` read verbatim; `hasBehindFigure` is a typeof+isFinite
  check only (no threshold compare — lock #5). undefined → generic fallback line; `queryByText(/undefined/)`
  asserted null.
- #14 button hierarchy: actions drive Dialog, which applies `primary` vs `flat` (distinct) plus
  per-action module classes `...PrimaryButton`/`...SecondaryButton`. Old shared
  `...CancelButton` no longer applied to the primary. Test asserts primary has `primary` + module
  class and NOT `flat`; secondary inverse.
- behindByImmutables threaded end-to-end: store `@observable` (MithrilPartialSyncStore.ts:67) →
  `@observer` container reads `mithrilPartialSync.behindByImmutables` (DaedalusDiagnosticsDialog.tsx:139)
  → DaedalusDiagnostics prop → Section prop → Confirmation prop. Reactive and complete.
- `showConfirmationOnOpen` opens the modal on mount (Section componentDidMount, guarded by
  `!isActionBlocked`), never auto-starts; container passes literal `false` as the task-ux-302 handoff seam.

Locked invariants: #3 (startFromConfirmation → onStartMithrilPartialSync remains the only start path;
mount-open shows the confirmation, no auto-confirm/auto-start), #4 ("verifies"/"verified" retained in
stepDownload + recovery + behind line), #5 (no renderer threshold). No snapshot/storage picker; bootstrap
files untouched. No IPC/contract drift (renderer-only).

i18n: 5 new keys extracted to messages.json; first-class EN + real JA (no placeholder dump); `{count}`
preserved in JA; no `!!!` leakage on the new partial-sync-confirmation runtime values (the baseline `!!!`
hits are pre-existing unrelated keys).

Independent verification (re-run by reviewer):
- tsc --noEmit: PASS — exit 0, zero errors project-wide.
- eslint (touched .ts/.tsx + specs): PASS — exit 0, 0 errors (only pre-existing warnings).
- prettier --check (all touched files incl. scss + specs): PASS — clean.
- jest (3 specs): PASS — 29/29 green. The raw `yarn test:jest` run fails only because
  `jest-css-modules-transform` cannot load sass under Node v24 — a pre-existing environment defect that
  breaks even untouched scss-importing specs; reproduced the green run with a CLI-only `.scss` →
  identity-obj-proxy override (committed jest config unchanged). Module-class assertions remain valid since
  identity-obj-proxy returns the key verbatim.

Blockers: none.

Minor (non-blocking, not requiring changes):
- The modal exposes TWO back-to-diagnostics affordances (DialogBackButton arrow + the secondary action
  button), both calling onCancel. The plan explicitly endorsed keeping both (DD4 note); harmless and
  consistent with the Dialog convention.
- `intro`/`success` message ids are retained-but-unrendered (DD4); orphan cleanup is task-ux-501/D8 —
  correctly deferred, recorded in the research note.

Decision: approved
