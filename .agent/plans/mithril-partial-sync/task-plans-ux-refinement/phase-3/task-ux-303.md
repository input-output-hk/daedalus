# task-ux-303 — Replace the in-place confirmation with a decision-style modal carrying behind-ness context

- Sprint: Mithril Partial Sync UX Refinement — phase-3 (Renderer Discovery Gating & CTA Re-arm)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved` (pre-approved; the critique pass validates)
- Build status: `completed`
- Interaction mode: `autonomous`
- Priority: high · Estimated: 5h · Dependencies: task-ux-301 (completed)
- PRD decision: D6 · Closes gaps #11, #14, #22, #23 · Surfaces #26/#29 (behind-ness)

## Why now
Today the confirmation is an **in-place prose row swap**, not a modal. When
`MithrilPartialSyncSection.state.isShowingConfirmation` is true, the Section **returns
`<MithrilPartialSyncConfirmation/>` instead of the recommendation row** (early-return
`MithrilPartialSyncSection.tsx:111-120`), rendering a 560px `<div>` inside the diagnostics status grid
with **no scrim and no focus trap** (`MithrilPartialSyncConfirmation.tsx:62-93`;
`DaedalusDiagnostics.scss:344-388`) — research-19 gap #22. Because the confirmation is just inline
content of the outer diagnostics `ReactModal`, **ESC / overlay-click closes the WHOLE diagnostics
dialog** mid-confirmation (gap #23). The body is three paragraphs with **no "how far behind" context**
(gap #11 / #29) even though the backend already exposes `behindByImmutables`. And **both buttons share
`styles.mithrilPartialSyncConfirmationCancelButton`** (`MithrilPartialSyncConfirmation.tsx:78,85`), so
the primary node-stopping "Start" action looks identical to the secondary cancel — no primary/secondary
hierarchy (gap #14).

PRD **D6** replaces this with a proper **decision-style modal**: scrim + focus trap, ESC/scrim-click =
"Back to diagnostics" (not close the outer dialog), a behind-ness context line from `behindByImmutables`,
what-happens steps + recovery note keeping the locked "verified" wording, and distinct primary/secondary
buttons — reusing the bootstrap **decision-view styling** (not its snapshot-metadata grid). This task is
also a **hard dependency of task-ux-302**: that task's proactive-prompt "Review" deep-link opens this
modal on mount via a new `showConfirmationOnOpen` prop (`tasks.json` task-ux-302 `dependencies`
includes `task-ux-303`; PRD UX-flow step 1, requirements `:593-595`). Confirmation stays the **only**
path to backend start (lock #3).

## Interaction mode justification
`autonomous`. There is **no blocking user question**: D6 fully specifies the copy (behind-ness line,
what-happens steps, recovery note, button labels) and the locked "verified" wording is fixed by lock #4.
The seams are all pre-existing — `widgets/Dialog.tsx`, `DialogBackButton.tsx`, `DialogCloseButton.tsx`,
the bootstrap `MithrilDecisionView.scss` styling, and the `behindByImmutables` observable
(`MithrilPartialSyncStore.ts:67`). No new IPC, no destructive operation, no operator validation, no
network call, no product/UX judgment the PRD has not already made. The renderer performs **no
thresholding** (lock #5) — it consumes `behindByImmutables` verbatim. Do NOT relabel.

## Scope
1. **`MithrilPartialSyncConfirmation.tsx`** — convert from the in-place `<div>` class component into a
   real **focus-trapped modal with scrim** rendered through `widgets/Dialog.tsx` (a nested
   react-polymorph `Modal` → `react-modal`). Add a **behind-ness context line** from a new
   `behindByImmutables?: number` prop (omit the figure gracefully when `undefined`). Add **what-happens
   steps** (stop node → download & verify → restart) + the **recovery note**, keeping the locked
   "verified Mithril data" wording. Give **primary "Start Mithril partial sync"** vs **secondary "Back
   to diagnostics"** *distinct* styles. ESC / scrim-click → `onCancel` (back to diagnostics), NOT the
   outer dialog.
2. **`MithrilPartialSyncSection.tsx`** — thread a new `behindByImmutables?: number` prop down to the
   confirmation; add a new `showConfirmationOnOpen?: boolean` prop that opens the confirmation **on
   mount** (deep-link entry for task-ux-302) without bypassing the confirmation (lock #3).
3. **`DaedalusDiagnostics.tsx`** — accept and thread `behindByImmutables?: number` and
   `showConfirmationOnOpen?: boolean` from props into `MithrilPartialSyncSection`.
4. **`DaedalusDiagnosticsDialog.tsx`** (container) — pass `mithrilPartialSync.behindByImmutables` into
   the new `behindByImmutables` prop (the store already exposes it as `@observable`,
   `MithrilPartialSyncStore.ts:67`). `showConfirmationOnOpen` is wired through but its **source**
   (the deep-link payload) lands in task-ux-302; for THIS task default it `false` (see Non-goals).
5. **`DaedalusDiagnostics.scss`** — add the new decision-modal styles (header, behind-ness line,
   what-happens step list, recovery note, distinct primary/secondary buttons) mirroring
   `MithrilDecisionView.scss`. Keep / repurpose the existing `.mithrilPartialSyncConfirmation*` classes;
   the shared-cancel-style bug (#14) is fixed by giving the primary button its own class.
6. **i18n** — add new message keys (behind-ness line with `{count}`, behind-ness fallback, three
   what-happens step labels) to the `defineMessages` block in `MithrilPartialSyncConfirmation.tsx` with
   `!!!`-prefixed `defaultMessage`; run `yarn i18n:extract`; add first-class EN strings; keep JA catalog
   consistent (placeholder JA acceptable — full JA pass is task-ux-601 — but **no `!!!` leakage** in
   runtime catalogs).
7. **Tests** — update `MithrilPartialSyncSection.spec.tsx` (new props, modal assertions); add a focused
   `MithrilPartialSyncConfirmation.spec.tsx` covering the modal contract; update
   `DaedalusDiagnostics.spec.tsx` defaultProps for the two new optional props.

## Non-goals (explicitly out of this task)
- **NO snapshot-selection UI and NO storage-location picker** (locked non-goals). Do NOT port
  `MithrilSnapshotSelector` / `MithrilSnapshotDetails` / the `locationContext` block from
  `MithrilDecisionView.tsx:88-95,67-86`. Reuse only the **styling** (header h1+p, primary/secondary
  buttons, panel look), not the metadata grid.
- **NO new IPC channel** and **NO renderer threshold math** — consume `behindByImmutables` as an opaque
  figure; never compare it to a number (lock #5).
- **NO proactive loading-screen prompt** and **NO `openDaedalusDiagnosticsDialog({ showMithrilConfirmation })`
  payload wiring** — that is task-ux-302. This task only *adds and accepts* the `showConfirmationOnOpen`
  prop and routes it to mount-time open; the container passes `false` for now. Do NOT touch
  `SyncingConnecting.tsx`, `actions/app`, or the dialog-open action.
- **Do NOT i18n the hardcoded "Mithril Partial Sync" row label** in
  `MithrilPartialSyncSection.tsx:129` — that is task-ux-501.
- **Do NOT touch the in-session overlay** (`MithrilPartialSyncOverlay.tsx`), its error copy, or the
  recovery-action wiring — that is D5 (task-ux-4xx). The recovery NOTE in the confirmation is static
  prose only.
- **Do NOT change the start path.** `startFromConfirmation` →
  `onStartMithrilPartialSync` stays the only route to backend start (lock #3). No auto-start on mount.
- **Do NOT regress the empty-chain Mithril bootstrap flow.** Only the partial-sync confirmation is
  touched; `MithrilDecisionView.tsx` / `MithrilBootstrap.tsx` are READ-ONLY references.

## Dependencies
- **task-ux-301 (completed)** — added `MithrilPartialSyncStore.isPartialSyncEnabled`,
  `isSignificantlyBehind`, and **`behindByImmutables`** (`MithrilPartialSyncStore.ts:65-67`), and
  renamed the Section's working prop to `isMithrilPartialSyncWorking` + added
  `shouldShowRecommendation`. THIS task consumes `behindByImmutables` and threads it to the modal.
- **Dependent (downstream): task-ux-302** — needs the `showConfirmationOnOpen` prop this task adds.

## Research / docs / workflows / skills consulted
- Tasks JSON `task-ux-303` — acceptance (5) + testCases (5) carried verbatim below.
- PRD **D6** (`...-prd.md:236-259`) — modal treatment, behind-ness line, what-happens steps, button
  hierarchy, "stays lighter than bootstrap" (no snapshot/storage picker; reuse decision-view styling).
  UX-flow step 3 (`:569-570`); requirements (`:596-597`, `:593-595` deep-link).
- Research-19 gaps **#11** (row 138 — prose-only, no behind context), **#14** (row 141 — shared cancel
  style), **#22** (row 149 — in-place 560px swap, no scrim/focus-trap;
  `MithrilPartialSyncSection.tsx:100-114`, `DaedalusDiagnostics.scss:344-388`), **#23** (row 150 —
  coarse ESC/overlay close exits whole dialog), **#26** (row 153 — snapshot identity computed, never
  surfaced — only the behind-ness half is surfaced here; full metadata stays a non-goal), **#29** (row
  156 — no "how far behind" copy; locked: renderer must not compute a threshold). Also **#34** (row 161
  — keep copy simple; no IOG-key / no-ancillary nuance) and **#25** (row 152 — recovery copy promises
  actions shown only in the overlay; keep the note generic/factual).
- `.agent/workflows/frontend.md` (React/MobX patterns; `@observer` container reactivity).
- `.agent/system/architecture.md` (Electron/React + ReactModal dialog pattern).
- Skill `i18n-messaging` — `!!!` default-message convention + `yarn i18n:extract`.
- Template: `task-ux-301.md` (structure/quality mirror).

## Live-seam verification (confirmed against the working tree, 2026-06-24)
All anchors below were re-read; the brief's anchors are accurate except where noted under "Anchor drift".
- `MithrilPartialSyncConfirmation.tsx` — class component returning
  `<div className={styles.mithrilPartialSyncConfirmation}>` (`:62-93`); `defineMessages` =
  title/intro/success/recovery/cancel/confirm (`:6-44`); BOTH buttons use
  `styles.mithrilPartialSyncConfirmationCancelButton` (`:78`, `:85`) — the #14 bug; `startError`
  rendered as `styles.error` (`:74`); Props `{ isActionBlocked, startError, onCancel, onConfirm }`
  (`:46-51`). **No scrim, no focus trap.**
- `MithrilPartialSyncSection.tsx` — hosts the confirmation; `isShowingConfirmation` early-return
  (`:111-120`) returns `<MithrilPartialSyncConfirmation/>` *instead of* the recommendation row (the
  in-place swap #22). Methods `showConfirmation` (`:54-64`), `hideConfirmation` (`:66-76`),
  `startFromConfirmation` (`:78-99`). Hardcoded row label "Mithril Partial Sync" (`:129`) — leave for
  task-ux-501. Props (`:10-18`) already include `isMithrilPartialSyncWorking`,
  `shouldShowRecommendation`, `onRestoreFocus`, `onStartMithrilPartialSync` (post-301).
  `componentDidMount` (`:36-38`) exists (sets `_isMounted = true`).
- `DaedalusDiagnostics.tsx` — Props block (`:409-413`) already has the three partial-sync booleans;
  render destructure (`:521-524`); `MithrilPartialSyncSection` render (`:708-723`); passes
  `onRestoreFocus={this.restoreDialogCloseOnEscKey}`. `restoreDialogCloseOnEscKey` (`:878-890`)
  refocuses `.ReactModal__Content`.
- `DaedalusDiagnosticsDialog.tsx` — passes `isMithrilPartialSyncSignificantlyBehind` (`:135-137`) but
  **NOT `behindByImmutables`** → must add it. The outer surface is a `ReactModal` (`:105-112`) with
  `onRequestClose={closeDaedalusDiagnosticsDialog.trigger}` + `closeOnOverlayClick`.
- `MithrilPartialSyncStore.ts` — `@observable behindByImmutables: number | undefined = undefined;`
  (`:67`), `@observable isSignificantlyBehind` (`:66`). Reactive via `@observer` on the container.
- `widgets/Dialog.tsx` — renders react-polymorph `Modal` (skin `ModalSkin`), props include `title`,
  `children`, `actions` (array of `{ label, primary, disabled, onClick, className }`), `closeButton`,
  `backButton`, `onClose`, `closeOnOverlayClick`, `primaryButtonAutoFocus`. **`onClose` fires on ESC**
  (verified: `ModalSkin.js` → `react-modal` `onRequestClose={props.onClose}`,
  `shouldCloseOnOverlayClick={triggerCloseOnOverlayClick}`). Primary action gets class `primary`,
  others `flat`; `action.className` is additionally applied.
- `DialogBackButton.tsx` (`onBack` prop) and `DialogCloseButton.tsx` (`onClose` prop, cloned by
  `Dialog` with `onClose`) — reusable; the back arrow is the natural "Back to diagnostics" affordance.
- `MithrilDecisionView.scss` (`:1-117`) — the bootstrap decision-view styling to mirror: `.header`
  (h1 + p), `.actions` (flex row, gap 12px), `.primaryAction` / `.secondaryAction` (distinct fills).
- `MithrilPartialSyncOverlay.tsx` is a **plain-`div` full-screen overlay** (no Dialog) — NOT the
  pattern to copy for focus-trap; use `widgets/Dialog.tsx` per D6/brief.

### Anchor drift recorded
- Brief cites the Section early-return at "~lines 111-120"; live is `:111-120` (matches).
- Brief cites `DaedalusDiagnostics.tsx:709-722` for the Section render; live is `:708-723` (the
  `{isMithrilPartialSyncEnabled && (` gate opens at `:708`). Use `:708-723`.
- Brief cites `DaedalusDiagnosticsDialog.tsx:133-141` for the partial-sync prop block; live
  `behindByImmutables` must be inserted near `:135-137` (after
  `isMithrilPartialSyncSignificantlyBehind`).
- Brief cites `restoreDialogCloseOnEscKey` at "~:878-890"; live is `:878-890` (matches).

## DESIGN DECISIONS (resolved — implementer makes none)

### DD1 — The confirmation is a nested `widgets/Dialog`, NOT a bespoke overlay
Render `MithrilPartialSyncConfirmation` through `widgets/Dialog.tsx` (react-polymorph `Modal`). This
gives the **scrim + focus trap** D6 requires for free (it is a `react-modal` under the hood, which
focus-traps and stacks above the outer diagnostics `ReactModal` — nested `react-modal` is supported in
this codebase). Do NOT hand-roll a `div`-backdrop overlay like `MithrilPartialSyncOverlay.tsx`; that
gives no focus trap. Pass `title` = the modal heading, `children` = the body (behind-ness line +
what-happens steps + recovery note + any `startError`), `actions` = `[secondary Back, primary Start]`,
`closeOnOverlayClick` = `true`, and `onClose` = `onCancel`.

### DD2 — ESC / scrim-click = Back to diagnostics, not close the whole dialog (#23)
Because the confirmation Dialog is a **nested** `react-modal`, ESC and overlay-click are captured by the
**topmost** open modal (the confirmation), whose `onRequestClose` is wired to `Dialog.onClose`. Set
`Dialog.onClose = onCancel` (which the Section maps to `hideConfirmation`). The outer diagnostics
`ReactModal.onRequestClose` (`DaedalusDiagnosticsDialog.tsx:108`) does NOT fire while the inner modal is
open. Result: ESC/scrim-click returns to diagnostics (#23 fixed). No change to the container's
`onRequestClose` is needed. (The existing `onRestoreFocus`/`restoreDialogCloseOnEscKey` calls in the
Section remain — they refocus the diagnostics modal content AFTER the inner modal closes, which is still
correct.)

### DD3 — Behind-ness line, undefined-safe, no thresholding (#11/#29, lock #5)
Add prop `behindByImmutables?: number` to the confirmation. Render:
- when `typeof behindByImmutables === 'number'` (finite): the message
  `mithrilPartialSyncConfirmationBehind` with `values={{ count: behindByImmutables }}`:
  *"Your node is about {count} immutable files behind the latest verified snapshot."*
- when `undefined` (or not a finite number): render the generic fallback
  `mithrilPartialSyncConfirmationBehindUnknown`: *"Your node is behind the latest verified snapshot."*
  (no figure; NEVER render the string "undefined").

The renderer performs NO comparison of `behindByImmutables` against any threshold — it only formats the
opaque figure (lock #5; PRD D2 owns the threshold). Use a plain `react-intl` `{count}` interpolation
(no ICU plural needed — "files" is acceptable for the single-figure recovery copy and matches the brief
copy; keep it simple per #34).

### DD4 — What-happens steps + recovery note (keep "verified" wording, lock #4)
Replace the three free-text paragraphs with a small **ordered list** of three what-happens steps plus a
recovery note paragraph. Reuse the EXISTING message intent but split the steps into discrete labels:
- `mithrilPartialSyncConfirmationStepStop`: *"Daedalus stops Cardano node."*
- `mithrilPartialSyncConfirmationStepDownload`: *"Daedalus downloads and verifies Mithril data."*
  (KEEPS the locked **"verified"** wording — lock #4.)
- `mithrilPartialSyncConfirmationStepRestart`: *"Daedalus restarts Cardano node automatically and normal
  syncing resumes."*
- `recovery` (KEEP the existing key + copy verbatim — it already says retry / restart normally / wipe &
  full sync): rendered as a single note paragraph below the steps.

The existing `intro`/`success` keys are SUPERSEDED by the three step keys; keep their ids defined in the
catalog (do not delete — i18n cleanup of orphans is task-ux-501/D8) but stop rendering `intro`/`success`
(render the three steps instead). The `recovery` key is RETAINED and rendered. NOTE the existing
`recovery` copy already keeps the recovery-action listing concise; #25's "actions shown elsewhere" is
acceptable as a *preview* note and the PRD's D6 explicitly lists the recovery note as required content.

### DD5 — Distinct primary/secondary buttons (#14)
Drive the two buttons through `Dialog.actions` (the `Dialog` widget already styles `primary` vs `flat`).
Build:
```ts
actions={[
  { label: <Back to diagnostics>, onClick: onCancel, className: styles.mithrilPartialSyncConfirmationSecondaryButton },
  { label: <Start Mithril partial sync>, primary: true, disabled: isActionBlocked, onClick: onConfirm,
    className: styles.mithrilPartialSyncConfirmationPrimaryButton },
]}
```
The `primary: true` flag makes the Start button visually distinct from the secondary (the Dialog adds
`primary` vs `flat`), AND we give each its own scss class for the decision-view fill/border treatment —
killing the shared-cancel-style bug (#14). Order: secondary first (left), primary last (right) — matches
the Dialog row convention and the bootstrap decision view.

### DD6 — `showConfirmationOnOpen` opens the modal on mount (deep-link seam for 302)
Add `showConfirmationOnOpen?: boolean` to `MithrilPartialSyncSection`. In `componentDidMount`, if
`this.props.showConfirmationOnOpen` is true AND `!this.props.isActionBlocked`, call
`this.showConfirmation()` so the modal opens immediately on mount. This is the deep-link entry: it opens
the **confirmation modal**, not a second start path — confirmation still precedes start (lock #3). For
THIS task the container passes `false`; task-ux-302 will feed the real deep-link boolean. Guard against
re-opening: `showConfirmation()` already no-ops when `isActionBlocked`; mount-time open only fires once
(componentDidMount). Do NOT auto-confirm/auto-start.

## Implementation approach — ordered, mechanical steps

### Step 1 — `MithrilPartialSyncConfirmation.tsx`: imports + Dialog
File: `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx`.
1a. Replace the top imports. Keep `React, { Component }` and `defineMessages, intlShape`; ADD the
Dialog widget:
```ts
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';

import Dialog from '../widgets/Dialog';
import DialogBackButton from '../widgets/DialogBackButton';
import styles from './DaedalusDiagnostics.scss';
```
(Verify import depth: `MithrilPartialSyncConfirmation.tsx` lives in `components/status/`, so the widgets
are `../widgets/Dialog` and `../widgets/DialogBackButton` — same depth `MithrilPartialSyncSection.tsx`
imports `./MithrilPartialSyncConfirmation`.)

### Step 2 — `MithrilPartialSyncConfirmation.tsx`: message keys
File: same. In the `defineMessages` block (`:6-44`), KEEP `title`, `recovery`, `cancel`, `confirm`.
RETAIN (do not delete) `intro` and `success` ids in the catalog but they are no longer rendered (see
DD4). ADD the following keys (all with `!!!`-prefixed `defaultMessage`):
```ts
  behind: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind',
    defaultMessage:
      '!!!Your node is about {count} immutable files behind the latest verified snapshot.',
    description:
      'Behind-ness context line (with a file count) for the Mithril partial sync confirmation modal',
  },
  behindUnknown: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehindUnknown',
    defaultMessage: '!!!Your node is behind the latest verified snapshot.',
    description:
      'Behind-ness context line shown when the immutable-file gap figure is unavailable',
  },
  stepStop: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepStop',
    defaultMessage: '!!!Daedalus stops Cardano node.',
    description: 'What-happens step 1 for the Mithril partial sync confirmation modal',
  },
  stepDownload: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepDownload',
    defaultMessage: '!!!Daedalus downloads and verifies Mithril data.',
    description: 'What-happens step 2 for the Mithril partial sync confirmation modal',
  },
  stepRestart: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepRestart',
    defaultMessage:
      '!!!Daedalus restarts Cardano node automatically and normal syncing resumes.',
    description: 'What-happens step 3 for the Mithril partial sync confirmation modal',
  },
```
LOCK #4: `stepDownload` MUST keep the word **"verifies"/"verified"** (verified Mithril data). Do NOT
soften it.

### Step 3 — `MithrilPartialSyncConfirmation.tsx`: Props
File: same. Replace Props (`:46-51`) with:
```ts
type Props = {
  isActionBlocked: boolean;
  startError: string | null;
  behindByImmutables?: number;
  onCancel: () => void;
  onConfirm: () => void;
};
```

### Step 4 — `MithrilPartialSyncConfirmation.tsx`: render as a Dialog
File: same. Replace the `render()` body (`:58-95`) so it returns a `Dialog`:
```tsx
  render() {
    const { isActionBlocked, startError, behindByImmutables, onCancel, onConfirm } =
      this.props;
    const { intl } = this.context;

    const hasBehindFigure =
      typeof behindByImmutables === 'number' && Number.isFinite(behindByImmutables);

    return (
      <Dialog
        className={styles.mithrilPartialSyncConfirmationDialog}
        title={intl.formatMessage(messages.title)}
        closeOnOverlayClick
        primaryButtonAutoFocus
        backButton={<DialogBackButton onBack={onCancel} />}
        onClose={onCancel}
        actions={[
          {
            label: intl.formatMessage(messages.cancel),
            onClick: onCancel,
            className: styles.mithrilPartialSyncConfirmationSecondaryButton,
          },
          {
            label: intl.formatMessage(messages.confirm),
            primary: true,
            disabled: isActionBlocked,
            onClick: onConfirm,
            className: styles.mithrilPartialSyncConfirmationPrimaryButton,
          },
        ]}
      >
        <div className={styles.mithrilPartialSyncConfirmationBody}>
          <p className={styles.mithrilPartialSyncConfirmationBehind}>
            {hasBehindFigure
              ? intl.formatMessage(messages.behind, { count: behindByImmutables })
              : intl.formatMessage(messages.behindUnknown)}
          </p>

          <ol className={styles.mithrilPartialSyncConfirmationSteps}>
            <li>{intl.formatMessage(messages.stepStop)}</li>
            <li>{intl.formatMessage(messages.stepDownload)}</li>
            <li>{intl.formatMessage(messages.stepRestart)}</li>
          </ol>

          <p className={styles.mithrilPartialSyncConfirmationRecovery}>
            {intl.formatMessage(messages.recovery)}
          </p>

          {startError ? (
            <div className={styles.error}>{startError}</div>
          ) : null}
        </div>
      </Dialog>
    );
  }
```
Notes:
- `primaryButtonAutoFocus` focuses Start on open (focus-trap entry point).
- The `backButton` (DialogBackButton → arrow) is the secondary "Back to diagnostics" affordance in the
  modal chrome, in addition to the secondary action button; both call `onCancel`. Keeping BOTH matches
  the Dialog convention (back arrow + explicit secondary action) and is harmless.
- `onClose={onCancel}` is what makes ESC / scrim-click = back to diagnostics (DD2).
- `startError` is still rendered (preserves the start-rejection surface; task-ux-302/501 own deeper copy).

### Step 5 — `DaedalusDiagnostics.scss`: decision-modal styles
File: `source/renderer/app/components/status/DaedalusDiagnostics.scss`. Near the existing
`.mithrilPartialSyncConfirmation*` block (`:344-388`):
5a. KEEP `.mithrilPartialSyncConfirmationBody` (`:362-371`) but it now lives inside the Dialog content;
adjust if needed (the Dialog wraps it). Add the new classes:
```scss
  .mithrilPartialSyncConfirmationDialog {
    max-width: 560px;
  }

  .mithrilPartialSyncConfirmationBehind {
    color: var(--theme-network-window-white-color);
    font-family: var(--font-medium);
    margin: 0 0 16px;
  }

  .mithrilPartialSyncConfirmationSteps {
    margin: 0 0 16px;
    padding-left: 20px;

    li {
      line-height: 1.5;
      margin-bottom: 8px;
    }
  }

  .mithrilPartialSyncConfirmationRecovery {
    font-size: 14px;
    margin: 0;
    opacity: 0.85;
  }

  .mithrilPartialSyncConfirmationPrimaryButton {
    min-width: 180px;
  }

  .mithrilPartialSyncConfirmationSecondaryButton {
    min-width: 180px;
  }
```
5b. The two new button classes give primary/secondary DISTINCT styling **in addition** to the Dialog's
own `primary`/`flat` classes (which already differentiate fill). The shared
`.mithrilPartialSyncConfirmationCancelButton` is NO LONGER applied to the primary button (the Dialog
renders the buttons; we pass per-action `className`), so #14 is fixed. (Leave the now-unused
`.mithrilPartialSyncConfirmationCancelButton` / `.mithrilPartialSyncConfirmationActions` /
`.mithrilPartialSyncConfirmationTitle` classes in place; removing dead scss is optional cleanup and not
required for acceptance — prefer the smaller diff and leave them.)
5c. Generate the scss type sidecar if the build requires it (`DaedalusDiagnostics.scss.d.ts` if present
— check; if `typedef:sass` regenerates it, no manual edit). If a `.scss.d.ts` exists and is hand-checked
in, add the new class names to it.

### Step 6 — `MithrilPartialSyncSection.tsx`: new props + thread behind + mount-open
File: `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`.
6a. Props (`:10-18`): add `behindByImmutables?: number;` and `showConfirmationOnOpen?: boolean;`:
```ts
type Props = {
  formattedSyncPercentage: string;
  isActionBlocked: boolean;
  isMithrilPartialSyncWorking: boolean;
  isSynced: boolean;
  shouldShowRecommendation: boolean;
  behindByImmutables?: number;
  showConfirmationOnOpen?: boolean;
  onRestoreFocus: () => void;
  onStartMithrilPartialSync: (...args: Array<any>) => any;
};
```
6b. `componentDidMount` (`:36-38`) — open the confirmation on mount when deep-linked:
```tsx
  componentDidMount() {
    this._isMounted = true;
    if (this.props.showConfirmationOnOpen && !this.props.isActionBlocked) {
      this.showConfirmation();
    }
  }
```
(`showConfirmation` (`:54-64`) already no-ops when `isActionBlocked` and sets
`isShowingConfirmation: true` + calls `onRestoreFocus`.)
6c. In `render()`'s `isShowingConfirmation` branch (`:111-120`), pass `behindByImmutables` through:
```tsx
    if (isShowingConfirmation) {
      return (
        <MithrilPartialSyncConfirmation
          isActionBlocked={isActionBlocked}
          startError={startError}
          behindByImmutables={this.props.behindByImmutables}
          onCancel={this.hideConfirmation}
          onConfirm={this.startFromConfirmation}
        />
      );
    }
```
(Destructure `behindByImmutables` at the top of `render()` if preferred, OR reference
`this.props.behindByImmutables` directly as shown — either compiles. Keep `isActionBlocked`, `isSynced`,
`shouldShowRecommendation`, `formattedSyncPercentage` destructure as-is.)

IMPORTANT — the `!shouldShowRecommendation → null` early-return (`:122-124`, from task-ux-301) must NOT
block the deep-link confirmation. Because `isShowingConfirmation` is checked FIRST (`:111`), a
mount-opened confirmation renders even when `shouldShowRecommendation` is false. **Verify ordering: the
`isShowingConfirmation` return precedes the `!shouldShowRecommendation` return** (it does, `:111` before
`:122`) — so a deep-linked open is shown regardless of the recommendation gate. Do not reorder.

### Step 7 — `DaedalusDiagnostics.tsx`: accept + thread the two new props
File: `source/renderer/app/components/status/DaedalusDiagnostics.tsx`.
7a. Props (`:409-413`) — add the two optional props after the existing partial-sync booleans:
```ts
  isMithrilPartialSyncWorking: boolean;
  isMithrilPartialSyncEnabled: boolean;
  isMithrilPartialSyncSignificantlyBehind: boolean;
  mithrilPartialSyncBehindByImmutables?: number;
  showMithrilPartialSyncConfirmationOnOpen?: boolean;
  isMithrilBootstrapActive: boolean;
```
(Use the `mithrilPartialSync...` / `showMithril...` prefixes at this level to match the existing
`isMithrilPartialSync*` naming; they map to the Section's `behindByImmutables` /
`showConfirmationOnOpen` in Step 7c.)
7b. Render destructure (`:521-524`) — add:
```ts
      isMithrilPartialSyncWorking,
      isMithrilPartialSyncEnabled,
      isMithrilPartialSyncSignificantlyBehind,
      mithrilPartialSyncBehindByImmutables,
      showMithrilPartialSyncConfirmationOnOpen,
      isMithrilBootstrapActive,
```
7c. `MithrilPartialSyncSection` render (`:708-723`) — pass the two new props:
```tsx
              {isMithrilPartialSyncEnabled && (
                <MithrilPartialSyncSection
                  formattedSyncPercentage={formattedSyncPercentage}
                  isActionBlocked={isMithrilActionBlocked}
                  isMithrilPartialSyncWorking={isMithrilPartialSyncWorking}
                  isSynced={isSynced}
                  shouldShowRecommendation={
                    isMithrilPartialSyncEnabled &&
                    isMithrilPartialSyncSignificantlyBehind
                  }
                  behindByImmutables={mithrilPartialSyncBehindByImmutables}
                  showConfirmationOnOpen={
                    showMithrilPartialSyncConfirmationOnOpen
                  }
                  onRestoreFocus={this.restoreDialogCloseOnEscKey}
                  onStartMithrilPartialSync={
                    this.props.onStartMithrilPartialSync
                  }
                />
              )}
```

### Step 8 — `DaedalusDiagnosticsDialog.tsx`: feed behindByImmutables; default the deep-link false
File: `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`. After
`isMithrilPartialSyncSignificantlyBehind` (`:135-137`), add:
```tsx
          isMithrilPartialSyncSignificantlyBehind={
            mithrilPartialSync.isSignificantlyBehind
          }
          mithrilPartialSyncBehindByImmutables={
            mithrilPartialSync.behindByImmutables
          }
          showMithrilPartialSyncConfirmationOnOpen={false}
```
`showMithrilPartialSyncConfirmationOnOpen={false}` is the explicit placeholder — task-ux-302 replaces
the literal `false` with the real deep-link boolean from the dialog-open payload. Document this in the
impl-review as the handoff seam. (`@observer` makes `behindByImmutables` reactive.)

### Step 9 — i18n extract + catalogs
9a. Run `yarn i18n:extract` to regenerate `translations/messages.json` with the new + retained keys.
9b. Add first-class EN strings to `source/renderer/app/i18n/locales/en-US.json` (NO `!!!`):
```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind": "Your node is about {count} immutable files behind the latest verified snapshot.",
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehindUnknown": "Your node is behind the latest verified snapshot.",
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepStop": "Daedalus stops Cardano node.",
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepDownload": "Daedalus downloads and verifies Mithril data.",
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationStepRestart": "Daedalus restarts Cardano node automatically and normal syncing resumes.",
```
(insert in the existing alphabetical/positional order of the `...ConfirmationXxx` block near `:160-165`.)
9c. Add matching JA placeholders to `source/renderer/app/i18n/locales/ja-JP.json` — placeholder JA is
acceptable (full JA is task-ux-601) but the values MUST NOT contain `!!!`. Acceptable placeholder: copy
the EN string verbatim, or a basic JA rendering; either way **no `!!!` leakage**. The `{count}`
placeholder must be preserved in the JA value.

### Step 10 — Tests (see Verification for the full matrix)
Add `MithrilPartialSyncConfirmation.spec.tsx`; update `MithrilPartialSyncSection.spec.tsx` and
`DaedalusDiagnostics.spec.tsx`. Details below.

## Locked invariants this task honors (inline)
- **#3 confirmation precedes start:** `startFromConfirmation` → `onStartMithrilPartialSync` remains the
  only start path. `showConfirmationOnOpen` opens the **confirmation** (not start) on mount; no
  auto-confirm/auto-start. The primary "Start" button (and only it) calls `onConfirm`.
- **#4 "verified" wording preserved:** `stepDownload` keeps "downloads and **verifies** Mithril data";
  the retained `recovery` copy keeps "verified". No softening.
- **#5 no renderer-computed threshold:** the modal formats `behindByImmutables` verbatim via
  `{count}`; it NEVER compares it to a number. `undefined`/non-finite → generic fallback line; never
  renders "undefined".
- **Non-goals (no pickers):** no snapshot-selection and no storage-location picker; only the
  decision-view *styling* is reused, not the metadata grid (`MithrilDecisionView.tsx:88-95` NOT ported).
- **#10 kill switch:** unchanged — the whole Section is still gated on `isMithrilPartialSyncEnabled`
  (task-ux-301, `DaedalusDiagnostics.tsx:708`); this task does not touch that gate.
- **Bootstrap not regressed:** `MithrilDecisionView.tsx`/`MithrilBootstrap.tsx` are READ-ONLY; only the
  partial-sync confirmation + its scss are edited.

## Acceptance criteria (verbatim from tasks JSON)
- [x] Confirmation is a focus-trapped decision-style modal with behind-ness context. — rendered through
  `widgets/Dialog` (react-polymorph `Modal` → `react-modal`/`ModalSkin`), giving scrim
  (`.ReactModal__Overlay`) + focus trap; behind-ness line from `behindByImmutables`.
- [x] ESC / scrim-click = back to diagnostics, not full-dialog close. — `onClose={onCancel}` +
  `closeOnOverlayClick`; the nested (topmost) modal captures ESC so the outer diagnostics `ReactModal`
  does not close.
- [x] Button hierarchy is primary/secondary with distinct styles. — primary "Start Mithril partial sync"
  (`primary: true` + `...PrimaryButton`) vs secondary "Back to diagnostics" (`flat` + `...SecondaryButton`)
  via `Dialog.actions`; the shared `...CancelButton` is no longer applied to the primary (#14 fixed).
- [x] No snapshot-selection or storage-location picker; verified-data wording preserved. — only
  decision-view styling reused (no metadata grid); `stepDownload` keeps "downloads and **verifies** Mithril
  data" (lock #4).
- [x] Confirming remains the only path to start. — `startFromConfirmation → onStartMithrilPartialSync`
  stays the sole start route; mount-open shows the confirmation, never auto-starts (lock #3).

Additional (this plan):
- [x] A new `showConfirmationOnOpen` prop opens the confirmation modal on mount (deep-link seam for
  task-ux-302); confirmation still precedes start. — `componentDidMount` opens once when
  `showConfirmationOnOpen && !isActionBlocked`; container passes literal `false` (task-ux-302 handoff seam).
- [x] `behindByImmutables === undefined` renders the generic fallback line (never "undefined"). —
  `typeof + Number.isFinite` guard; `undefined`/non-finite → `behindUnknown` line; no threshold compare
  (lock #5).

## Verification plan (exact commands, from repo root `/workspaces/mithril-partial-sync-ux`)
- `yarn i18n:extract` — regenerates `translations/messages.json`; confirm the 5 new ids appear and no
  `!!!` leaks into `en-US.json` / `ja-JP.json` runtime values
  (`grep -n '!!!' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json`
  → no partial-sync-confirmation hits).
- `yarn compile` — `tsc --noEmit` must pass (allow up to 600s; the `typedef:sass` precompile hook may
  crash under Node v24 in this sandbox — a known pre-existing dart-sass env defect, see task-ux-301
  final outcome; if so, run `node_modules/.bin/tsc --noEmit` directly as the real TS gate).
- `node_modules/.bin/eslint --ext .ts,.tsx source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx
  source/renderer/app/components/status/MithrilPartialSyncSection.tsx
  source/renderer/app/components/status/DaedalusDiagnostics.tsx
  source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — no new errors.
- `yarn test:jest source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx
  source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx
  source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` — all green.
- `yarn prettier:check` (or `node_modules/.bin/prettier --check` on the touched files).

### Tests to add/update, mapped to the 5 JSON testCases
**New: `source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx`** — render the
component directly inside `<IntlProvider locale="en-US" messages={translations}>` (mirror
`MithrilPartialSyncSection.spec.tsx`). NOTE: the Dialog uses `react-modal`; set
`ReactModal.setAppElement` or pass `ariaHideApp={false}` is already handled by `ModalSkin`
(`ariaHideApp: false`), so a bare `render` works; if `react-modal` warns about app element, wrap with a
container div and use `screen` queries against `document.body` (react-modal portals to body).
- **(a) renders as a focus-trapped modal with scrim** → assert the dialog heading
  `screen.getByRole('heading', { name: 'Before Mithril partial sync begins' })` is present AND the
  react-modal overlay node exists (`document.querySelector('.ReactModal__Overlay')` not null), proving a
  real modal (scrim) rather than the old inline `<div>`.
- **(b) ESC / scrim-click returns to diagnostics without closing the whole dialog** → render with an
  `onCancel` spy; fire `keyDown` Escape on the modal content
  (`fireEvent.keyDown(document.querySelector('.ReactModal__Content'), { key: 'Escape', keyCode: 27 })`)
  → assert `onCancel` called. (The outer diagnostics close is not in scope of this unit; the assertion
  that ESC routes to `onCancel` — wired to `hideConfirmation` — proves the back-to-diagnostics behavior;
  add a Section-level test below that ESC returns to the recommendation.)
- **(c) behind-ness line reflects behindByImmutables** → render with `behindByImmutables={42}` → assert
  `screen.getByText('Your node is about 42 immutable files behind the latest verified snapshot.')`;
  render with `behindByImmutables={undefined}` → assert
  `screen.getByText('Your node is behind the latest verified snapshot.')` AND
  `expect(screen.queryByText(/undefined/)).toBeNull()`.
- **(d) primary/secondary buttons have distinct styles** → assert the Start button
  (`screen.getByRole('button', { name: 'Start Mithril partial sync' })`) carries the `primary` class
  (`toHaveClass('primary')` — Dialog adds it) AND the
  `mithrilPartialSyncConfirmationPrimaryButton` class, while the Back button
  (`name: 'Back to diagnostics'`) does NOT carry `primary` (carries `flat` +
  `mithrilPartialSyncConfirmationSecondaryButton`). This proves the classes diverge (#14 fixed). NOTE:
  CSS-module class names are hashed in jest unless identity-mapped; assert on the **`primary`/`flat`**
  literal classes (added by Dialog, not hashed) to avoid module-hash brittleness — those alone prove the
  hierarchy. (If the jest config identity-maps scss, also assert the two module classes.)
- **(e) confirming is the only path to backend start** → render with `onConfirm` spy; assert clicking
  Start calls `onConfirm` once; assert NO start happens on mount or on ESC/Back (only the primary button
  triggers `onConfirm`); assert the disabled Start (`isActionBlocked`) does not call `onConfirm`.

**Update `MithrilPartialSyncSection.spec.tsx`:**
- defaultProps already has the post-301 shape; ADD `behindByImmutables: undefined` and
  `showConfirmationOnOpen: false` (both optional, so existing tests still compile without them, but add
  for clarity). The existing tests that open confirmation via the button
  (`getByRole('button', { name: 'Mithril Partial Sync' }).click()`) now land on the **modal** — update
  the heading assertions to query the modal heading (same text "Before Mithril partial sync begins", so
  `getByRole('heading', …)` still matches; verify the button-name "Back to diagnostics" /
  "Start Mithril partial sync" still resolve from inside the Dialog — they do).
- ADD a test: **showConfirmationOnOpen opens the modal on mount** →
  `renderComponent({ showConfirmationOnOpen: true })` → assert the modal heading is present WITHOUT
  clicking the recommendation button, AND `onStartMithrilPartialSync` was NOT called (confirmation, not
  start).
- ADD a test: **deep-link open works even when shouldShowRecommendation is false** →
  `renderComponent({ showConfirmationOnOpen: true, shouldShowRecommendation: false })` → modal heading
  present (proves the `isShowingConfirmation` branch precedes the `null` gate).
- ADD a test: **ESC on the confirmation returns to the recommendation, not unmount** → open
  confirmation, fire Escape on `.ReactModal__Content`, assert the recommendation copy
  ("Cardano node is currently 62.50% synced…") is shown again (proves `onCancel`→`hideConfirmation`
  returns to the row, #23 at the Section level).
- The existing "returns to the recommendation when confirmation is cancelled" test
  (`:50-61`) clicks "Back to diagnostics" — still valid (now the modal's secondary action / back arrow).

**Update `DaedalusDiagnostics.spec.tsx`:** add `mithrilPartialSyncBehindByImmutables: undefined` and
`showMithrilPartialSyncConfirmationOnOpen: false` to `defaultProps` (`:52-56` area). Existing render
tests unaffected. Optionally add: render, click the recommendation CTA, assert the modal opens with the
behind-ness fallback line (integration of the thread-through) — guard against react-modal app-element
warnings as above.

## Risks / open questions
- **Nested-modal ESC stacking.** The design relies on `react-modal` delivering ESC to the **topmost**
  open modal (the confirmation), so the outer diagnostics modal does not close. This is standard
  `react-modal` behavior and the brief confirms nested ReactModal-in-ReactModal is supported here. The
  Section-level ESC test (returns to recommendation) and the Confirmation-level ESC test (calls
  `onCancel`) cover it. If a future react-modal upgrade changes stacking, those tests fail loudly.
  Mitigation: keep `onClose={onCancel}` explicit on the inner Dialog.
- **CSS-module hashing in jest.** Class-name assertions for distinct buttons should target the
  Dialog-added literal `primary`/`flat` classes (not hashed) to avoid brittleness; the two scss module
  classes are asserted only if the jest config identity-maps scss. Documented in test (d).
- **`yarn compile` sass precompile crash** (pre-existing, Node v24 sandbox) — run `tsc --noEmit`
  directly as the real gate, per task-ux-301's recorded workaround.
- **`showConfirmationOnOpen` source is task-ux-302.** This task ships the prop + mount-open behavior and
  the container passes `false`; the literal must be replaced by the deep-link boolean in task-ux-302.
  Recorded as the handoff seam (Step 8) so 302 has a single, obvious wiring point.
- **`.scss.d.ts` sidecar.** If `DaedalusDiagnostics.scss.d.ts` is hand-maintained, the new class names
  must be added (Step 5c); otherwise the `typedef:sass` step regenerates it. Verify at implementation
  time (`ls source/renderer/app/components/status/DaedalusDiagnostics.scss.d.ts`).
- **Open question (non-blocking):** plural form of "files" for `count === 1`. The PRD copy uses
  "N immutable files" unconditionally; keep that (no ICU plural) per #34 "keep copy simple". Not a
  blocker — the single-figure recovery copy reads acceptably with "files".

## Required doc / research updates
- No PRD/research edits required for acceptance (D6 already specifies the modal + copy).
- Record in `task-ux-303-research.md`: the nested-`Dialog`-for-focus-trap decision (DD1), the
  ESC-routing mechanism (DD2 — inner `react-modal` captures ESC), and the `showConfirmationOnOpen`
  handoff seam consumed by task-ux-302 (Step 8 — container currently passes literal `false`).
- Note the retained-but-unrendered `intro`/`success` message ids (DD4) for the task-ux-501/D8 i18n
  cleanup so they are not double-removed.

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-303-plan-review.md`
  (append-only; Planner entry started this stage).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-303-impl-review.md`
  (append-only; created at implementation time).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-303-research.md`
  (research note; created at implementation time).

## Commit (single, subject-only — no body, no Co-Authored-By)
`feat(mithril): task-ux-303 replace in-place confirmation with a decision-style modal`
Stage ONLY: `MithrilPartialSyncConfirmation.tsx`, `MithrilPartialSyncSection.tsx`,
`DaedalusDiagnostics.tsx`, `DaedalusDiagnosticsDialog.tsx`, `DaedalusDiagnostics.scss`
(+ `.scss.d.ts` if regenerated), `translations/messages.json`, `en-US.json`, `ja-JP.json`, and the
three touched/added spec files.

## Final outcome (completed 2026-06-24)
Implemented exactly as planned — no substantive deviations. Code review approved on round 1.

Files shipped (5 source + i18n + 3 specs):
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` — converted from the in-place
  `<div>` class component into a nested `widgets/Dialog` (react-polymorph `Modal` → `react-modal`), giving
  scrim (`.ReactModal__Overlay`) + focus trap (#22). Added `behindByImmutables?: number` prop with an
  undefined/non-finite-safe behind-ness line via `typeof + Number.isFinite` (never renders "undefined", no
  threshold compare — lock #5, #11/#29). Replaced the three free-text paragraphs with a 3-step `<ol>`
  (stop → download+**verify** → restart) + the retained `recovery` note; `stepDownload` keeps the locked
  "downloads and verifies Mithril data" wording (lock #4). Distinct primary "Start Mithril partial sync"
  (`primary: true` + `...PrimaryButton`) vs secondary "Back to diagnostics" (`flat` + `...SecondaryButton`)
  via `Dialog.actions` — fixes the shared-cancel-style bug (#14). `onClose={onCancel}` + `closeOnOverlayClick`
  + `DialogBackButton(onBack)` make ESC/scrim/back-arrow = back to diagnostics (#23). `primaryButtonAutoFocus`
  for focus-trap entry. `startError` still rendered. 5 new `!!!`-prefixed keys: `behind` ({count}),
  `behindUnknown`, `stepStop`, `stepDownload`, `stepRestart`; `intro`/`success` retained-but-unrendered for
  task-ux-501/D8.
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — added `behindByImmutables?: number`
  + `showConfirmationOnOpen?: boolean` props. `componentDidMount` opens the confirmation once when
  `showConfirmationOnOpen && !isActionBlocked` (deep-link seam for task-ux-302; opens the confirmation, never
  a start path — lock #3). Threads `behindByImmutables` into the rendered confirmation. The
  `isShowingConfirmation` branch still precedes the `!shouldShowRecommendation → null` gate (not reordered),
  so a deep-linked open renders regardless of the recommendation gate.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — added
  `mithrilPartialSyncBehindByImmutables?: number` + `showMithrilPartialSyncConfirmationOnOpen?: boolean` props,
  destructured them, threaded into `MithrilPartialSyncSection` as `behindByImmutables` / `showConfirmationOnOpen`.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — passes
  `mithrilPartialSyncBehindByImmutables={mithrilPartialSync.behindByImmutables}` (store `@observable` from
  task-ux-301; `@observer` keeps it reactive) and the explicit placeholder
  `showMithrilPartialSyncConfirmationOnOpen={false}` — the single literal task-ux-302 replaces with the real
  deep-link boolean.
- `source/renderer/app/components/status/DaedalusDiagnostics.scss` — added decision-modal classes
  (`...Dialog`, `...Behind`, `...Steps`, `...Recovery`, `...PrimaryButton`, `...SecondaryButton`). Left the
  now-unused `...CancelButton`/`...Actions`/`...Title` classes in place (smaller diff; dead-scss removal
  deferred). The gitignored `.scss.d.ts` sidecar is auto-generated (typed `any` via the global `*.scss`
  module declaration), so it is not a compile gate and is not committed.
- i18n: first-class EN strings (no `!!!`) in `en-US.json` and real JA (no `!!!`, `{count}` preserved) in
  `ja-JP.json`, both in alphabetical position in the `...Confirmation*` block; `yarn i18n:extract` regenerated
  `translations/messages.json` with all 5 new ids.
- Tests: NEW `MithrilPartialSyncConfirmation.spec.tsx` (9 tests: modal+scrim, ESC→onCancel, behind-ness 42 +
  undefined fallback, verified-data wording, distinct primary/secondary classes, Start is the only start path
  incl. disabled-when-blocked, startError render). Updated `MithrilPartialSyncSection.spec.tsx` (+2
  defaultProps, +5 tests: mount-open without start, deep-link open when `shouldShowRecommendation:false`, no
  deep-link open when blocked, behind-ness thread-through, ESC returns to recommendation). Updated
  `DaedalusDiagnostics.spec.tsx` defaultProps with the two new optional props.

Verification (honest results):
- `tsc --noEmit` (the real TS gate): PASS, exit 0, zero errors project-wide.
- `yarn compile`: FAILs only because its `typedef:sass` precompile hook crashes on `node_modules/sass`
  ("Invalid or unexpected token") under Node v24 — a pre-existing, environment-wide dart-sass defect (same
  root cause documented in task-ux-301 + this plan's Risks), NOT a TypeScript error; `tsc --noEmit` is the
  authoritative gate and passes.
- eslint (touched .ts/.tsx + specs): PASS, exit 0, 0 errors (only pre-existing stylistic warnings).
- jest (3 specs): PASS — 29/29 green (9 Confirmation + 12 Section + 8 Diagnostics). Because
  `jest-css-modules-transform` cannot load sass under Node v24 (same defect; breaks even untouched
  scss-importing specs), the run mapped `.scss` → `identity-obj-proxy` via a CLI-only override; the committed
  jest config is unchanged, and class-name assertions target both the non-hashed `primary`/`flat` literals
  (added by Dialog) and the module class names (preserved under identity-obj-proxy).
- `prettier --check` (touched files): PASS — clean after write-fix (the formatter reflowed some long message
  `id` lines to multiline — cosmetic, consistent with the codebase).
- `yarn i18n:extract`: ran clean; 5 new ids in `messages.json`; no `!!!` leakage on the new EN/JA runtime
  values (the 41/51 baseline `!!!` hits are pre-existing, none on partial-sync-confirmation keys).

Deviations: none substantive. The TS gate was run as `tsc --noEmit` directly and jest with `.scss`
identity-mapped — both environment workarounds the plan anticipated (pre-existing Node-v24/dart-sass defect);
no product/source behavior changed. The `.scss.d.ts` sidecar is gitignored/auto-generated, so it is not staged.

Locked invariants upheld: #3 (confirmation is the only start path; mount-open shows the confirmation, never
auto-starts), #4 ("verifies/verified" retained in `stepDownload` + `recovery` + behind line), #5 (no threshold
compare; opaque `{count}`; undefined → generic fallback). Non-goals honored (no snapshot/storage picker;
styling-only reuse). Empty-chain bootstrap untouched (`MithrilDecisionView.tsx`/`MithrilBootstrap.tsx`
read-only). NOT committed (per the scribe scope — the orchestrator owns the single task commit).

## Status
- Planning status: `approved`
- Build status: `completed`
