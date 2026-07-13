# task-ux-303 — Plan review log (append-only)

Planner:
Timestamp: 2026-06-24T16:09:56Z
Summary: Wrote the canonical plan for task-ux-303 (PRD D6 — replace the in-place prose confirmation
row-swap with a decision-style modal carrying behind-ness context). Interaction mode = autonomous
(justified: D6 copy is fully specified; no blocking user question; all seams pre-exist; renderer does no
thresholding).

Smallest-truthful change decided:
- Convert `MithrilPartialSyncConfirmation` from an in-place `<div>` into a nested `widgets/Dialog`
  (react-polymorph `Modal` → `react-modal`), which provides scrim + focus trap for free. Verified
  `ModalSkin.js` wires `onRequestClose={props.onClose}` and
  `shouldCloseOnOverlayClick={triggerCloseOnOverlayClick}`, so `Dialog.onClose=onCancel` makes
  ESC/scrim-click = back to diagnostics; the nested modal captures ESC so the OUTER diagnostics
  `ReactModal` does not close (#23 fixed). No change to the container `onRequestClose` needed.
- Behind-ness line from a new `behindByImmutables?: number` prop with an undefined-safe fallback (never
  renders "undefined"); no threshold compare (lock #5).
- What-happens steps as a 3-item `<ol>` (stop / download+verify / restart) + the retained `recovery`
  note; `stepDownload` keeps the locked "verifies/verified Mithril data" wording (lock #4).
- Distinct primary/secondary buttons via `Dialog.actions` (`primary:true` adds the `primary` class;
  per-action scss classes added) — kills the shared-cancel-style bug (#14).
- New `showConfirmationOnOpen?: boolean` prop opens the modal on mount (deep-link seam consumed by the
  downstream task-ux-302); confirmation stays the only start path (lock #3) — no auto-start.
- Threaded `behindByImmutables` store→container→DaedalusDiagnostics→Section→Confirmation; the store
  already exposes `@observable behindByImmutables` (from task-ux-301), so no store change.
- No snapshot-selection / no storage-location picker; only `MithrilDecisionView.scss` styling is reused,
  not its metadata grid (`MithrilDecisionView.tsx:88-95` NOT ported).

New i18n keys (in `MithrilPartialSyncConfirmation.tsx` defineMessages, `!!!`-prefixed defaultMessage;
`yarn i18n:extract`; EN first-class, JA placeholder, no `!!!` leakage):
`mithrilPartialSyncConfirmationBehind` ({count}), `...BehindUnknown`, `...StepStop`, `...StepDownload`,
`...StepRestart`. Existing `intro`/`success` ids retained-but-unrendered (DD4) and flagged for the
task-ux-501/D8 i18n cleanup; `recovery`/`cancel`/`confirm`/`title` kept.

Tests specified: new `MithrilPartialSyncConfirmation.spec.tsx` (5 JSON testCases — modal+scrim, ESC→
onCancel, behind-ness line incl. undefined fallback, distinct primary/secondary classes via the
non-hashed `primary`/`flat` literals, Start is the only start path); updates to
`MithrilPartialSyncSection.spec.tsx` (new props, mount-open via `showConfirmationOnOpen`, deep-link open
when `shouldShowRecommendation:false`, ESC returns to recommendation) and `DaedalusDiagnostics.spec.tsx`
(defaultProps for the two new optional props).

Anchor corrections recorded in the plan (Live-seam verification → Anchor drift):
- Section render in `DaedalusDiagnostics.tsx` is `:708-723` (brief said 709-722) — the
  `{isMithrilPartialSyncEnabled && (` gate opens at `:708`.
- `DaedalusDiagnosticsDialog.tsx` passes `isMithrilPartialSyncSignificantlyBehind` at `:135-137` but NOT
  `behindByImmutables` — confirmed must add; container default `showMithrilPartialSyncConfirmationOnOpen`
  = literal `false` (task-ux-302 replaces it).
- `MithrilPartialSyncConfirmation.tsx` both buttons share
  `styles.mithrilPartialSyncConfirmationCancelButton` at `:78` and `:85` (confirmed #14).
- `MithrilPartialSyncSection.tsx` early-returns `:111-120` (confirmation), `:122-124`
  (`!shouldShowRecommendation` null) — ordering verified so a deep-linked open renders before the null
  gate; do not reorder.
- `restoreDialogCloseOnEscKey` at `:878-890` (matches brief).
Live tree confirms task-ux-301 already landed `behindByImmutables`/`isSignificantlyBehind` observables
and the `isMithrilPartialSyncWorking`/`shouldShowRecommendation` props — so this task adds NO store
change and only threads behind-ness + the deep-link prop.

Outcome: canonical plan written to small-model-implementable bar (exact files, ordered mechanical steps,
code seams with verified anchors, i18n keys, locked invariants inline, named spec paths + commands).
Planning status set to approved (pre-approved for the critique pass to validate). Build status
in_progress. No blocking user question — autonomous.

Critiquer:
Timestamp: 2026-06-24T16:18:02Z
Scope: one adversarial pass over the canonical plan against the live working tree, PRD D6
(:236-259), and tasks JSON task-ux-303 (acceptance/testCases/implementationNotes).

Live-seam re-verification (all CONFIRMED, no anchor drift beyond what the plan already records):
- MithrilPartialSyncConfirmation.tsx — in-place <div> class component; both buttons share
  styles.mithrilPartialSyncConfirmationCancelButton (:78,:85 → #14 real); defineMessages
  title/intro/success/recovery/cancel/confirm (:6-44); Props { isActionBlocked, startError,
  onCancel, onConfirm } (:46-51). Matches plan.
- MithrilPartialSyncSection.tsx — isShowingConfirmation early-return :111-120 precedes the
  !shouldShowRecommendation null gate :122-124 (so deep-link open renders regardless of the
  recommendation gate — plan's ordering claim verified). componentDidMount :36-38. Matches.
- DaedalusDiagnosticsDialog.tsx — passes isMithrilPartialSyncSignificantlyBehind :135-137 but
  NOT behindByImmutables; outer ReactModal onRequestClose :108. Matches.
- MithrilPartialSyncStore.ts :65-67 — isPartialSyncEnabled / isSignificantlyBehind /
  behindByImmutables: number | undefined all present (no store change needed). Matches.
- widgets/Dialog.tsx — actions array → classnames([action.className, action.primary?'primary':
  'flat']); onClose wired to react-polymorph Modal; backButton rendered raw. Matches DD5.
- ModalSkin.js (react-polymorph) — VERIFIED at source: onRequestClose: props.onClose (ESC →
  onClose), shouldCloseOnOverlayClick: props.triggerCloseOnOverlayClick, ariaHideApp: false.
  This validates DD1 (scrim + focus trap for free), DD2 (ESC/scrim = onCancel = back to
  diagnostics, captured by the topmost nested modal so the outer diagnostics dialog does NOT
  close → #23), and the test note that a bare render works without setAppElement.
- DialogBackButton.tsx — renders a <button> wrapping only an SVG (no accessible name), so
  getByRole('button', { name: 'Back to diagnostics' }) resolves ONLY the secondary action
  button — no test ambiguity from rendering both the back-arrow chrome and the secondary action.

D6 coverage: COMPLETE. Real modal+scrim+focus-trap (DD1); ESC/scrim = back-to-diagnostics not
full-close (DD2, source-verified); behind-ness line with undefined-safe fallback and NO renderer
threshold (DD3, lock #5); what-happens 3-step <ol> + retained recovery note keeping "verifies/
verified" (DD4, lock #4); distinct primary/secondary via Dialog primary/flat + per-action scss
classes (DD5, #14); showConfirmationOnOpen mount-open as a confirmation (not a start) path (DD6,
lock #3); behindByImmutables threaded store→container→DaedalusDiagnostics→Section→Confirmation.
Closes #11/#14/#22/#23 and surfaces the behind-ness half of #26/#29. Non-goals honored (no
snapshot/storage picker; styling-only reuse of the decision view). Empty-chain bootstrap left
READ-ONLY. The plan is concise and genuinely small-model-implementable: exact files, ordered
mechanical steps, named props/keys/types, verified seams.

Findings (all MINOR — none break an invariant, miss a D6 requirement, or block a small model):
1. (minor) .scss.d.ts sidecar IS present and hand-maintained
   (source/renderer/app/components/status/DaedalusDiagnostics.scss.d.ts), but the default import
   `import styles from './DaedalusDiagnostics.scss'` resolves through the global
   `declare module '*.scss' { const content: any; export default content; }`
   (source/renderer/declaration.d.ts + root declaration.d.ts), so styles.<newKey> is typed `any`
   and missing sidecar entries do NOT fail tsc. Step 5c's "optional/conditional" handling is
   therefore CORRECT; the precompile `typedef:sass` hook regenerates the sidecar. No change
   required — flagging only so the implementer does not treat the sidecar as a compile gate.
2. (minor) Runtime catalogs ALREADY contain `!!!` at baseline (en-US.json: 41, ja-JP.json: 51
   pre-existing hits). The bare verification command `grep -n '!!!' en-US.json ja-JP.json` will
   return those pre-existing hits and could mislead a small model into seeing a regression. The
   plan's parenthetical scopes it correctly ("no partial-sync-confirmation hits"); recommend the
   implementer scope the grep to the new keys, e.g.
   `grep -nE '!!!.*mithrilPartialSyncConfirmation(Behind|Step)' ...` (expect zero), and NOT treat
   the 41/51 pre-existing `!!!` as introduced by this task.
3. (minor) Redundant back affordance: the Dialog renders BOTH a back-arrow chrome button
   (DialogBackButton, no accessible name) AND a secondary "Back to diagnostics" action button —
   both call onCancel. Harmless and the plan acknowledges it; keep as-is for the Dialog
   convention. (The arrow button having no accessible name is a pre-existing a11y trait of
   DialogBackButton, not introduced here; the labeled secondary action covers the affordance.)
4. (minor) tasks JSON targetPaths lists only 4 files; the plan correctly touches 9+ (container +
   DaedalusDiagnostics.tsx + en-US/ja-JP + 3 specs) because the brief mandates threading
   behindByImmutables end-to-end. targetPaths is an authoritative minimum, not a ceiling — the
   expansion is justified and consistent. No action.

Locked-invariant consistency: #3 (confirmation is the only start path; mount-open shows the
confirmation, never auto-starts), #4 ("verifies/verified" retained in stepDownload + recovery),
#5 (no threshold compare; opaque {count}; undefined → generic fallback, never "undefined"),
non-goals (no pickers), #10 kill-switch gate untouched, bootstrap not regressed — all upheld.

Decision: approved
