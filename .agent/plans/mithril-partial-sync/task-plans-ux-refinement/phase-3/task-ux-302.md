# task-ux-302 — Proactive fork prompt on the syncing screen with concise inline confirm + direct start

- Sprint: Mithril Partial Sync UX Refinement — phase-3 (Renderer Discovery, Gating & Confirmation)
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved`
- Build status: `completed`
- Interaction mode: `interactive_decision` — **RESOLVED 2026-06-25**. The user reviewed the original
  deep-link plan and materially redesigned the flow: a two-way **fork** prompt (Mithril vs Standard) with a
  **concise inline confirm** step that starts partial sync DIRECTLY (no Diagnostics deep-link), plus
  user-facing terminology **"Mithril"** (drop "partial sync" to the user). The final EN copy is approved
  verbatim (see **FINAL COPY**); JA is first-class below. This redesign is recorded as PRD amendment **D13**
  (orchestrator-owned). No further copy stop remains; the implementer pastes the pinned FINAL COPY.
- Priority: high · Dependencies: task-ux-301 (completed), task-ux-303 (completed), task-ux-304 (completed)
- PRD decision: **D1** (proactive surface) **as amended by D13** (fork prompt + concise inline confirm +
  DIRECT start, no Diagnostics deep-link; user-facing "Mithril" term) · reuses **D11/D12** epochs figure
  (epochs-only here) · honors **D2/D3** (backend-owned gate, kill-switch). Closes blocker #2's prompt half.

---

## Why now

D1's hybrid trigger has two halves. The **manual** half (Diagnostics recommendation + confirmation + start)
shipped via task-ux-301/303/304. The **proactive** half — a dismissible prompt on the syncing screen — is
unbuilt. task-ux-304 (just completed) established the canonical behind-ness figure
(`behindByEpochs = max(1, networkTip.epoch − localTip.epoch)`, renderer-computed). Per the user's redesign
(**D13**) the prompt is a self-contained **fork** with its own concise inline confirm that starts partial
sync directly via the same store action the modal uses — it does NOT deep-link into Diagnostics. This task
builds that prompt and closes D1.

## Interaction mode justification

`interactive_decision` — **resolved 2026-06-25** by the user's redesign (D13). The flow + the exact EN copy
are approved (FINAL COPY block); JA is first-class below; the sync-% line is dropped (epochs-only). No
remaining copy stop condition — the implementer pastes the pinned FINAL COPY into the new prompt component's
`defineMessages` and the catalogs.

---

## Scope (revised per D13 — fork prompt + concise inline confirm + DIRECT start)

1. **`MithrilPartialSyncStore.ts`** — add an in-memory session flag
   `proactivePromptDismissedThisSession` (mirror `isCompletedOverlayDismissed` `:64`) + an `@action`
   `dismissProactivePrompt`. NOT persisted across restarts. (The existing `startPartialSync` action `:202-231`
   is REUSED as-is for the confirm-view "Start now" — no new start channel.)
2. **New prompt component** `SyncingConnectingMithrilPrompt.tsx` (co-located in
   `components/loading/syncing-connecting/`) — a **two-state** component (own local `view: 'choice' | 'confirm'`
   state): the **choice view** (epochs body + "Mithril Sync (fast)" / "Standard Sync (slow)" + handoff note)
   and the **confirm view** (concise confirm body + "Start now" / "Cancel"). It receives the figure + an async
   `onStart` + an `onDismiss` from the container; it owns the choice↔confirm transition internally. Mount it
   from `SyncingConnecting.tsx`.
3. **`SyncingConnecting.tsx`** — accept new props (`showMithrilPrompt`, `mithrilBehindByEpochs`,
   `onStartMithrilSync`, `onDismissMithrilPrompt`) and render `<SyncingConnectingMithrilPrompt>` when
   `showMithrilPrompt` is true.
4. **`SyncingConnectingPage.tsx`** (container) — read the `mithrilPartialSync` store availability flags +
   compute `behindByEpochs` from the already-injected `networkStatus.networkTip`/`.localTip` (mirror
   task-ux-304's exact computation); compute the gate boolean; pass the prompt props + handlers into
   `SyncingConnecting`. `onStartMithrilSync` is wired to **`mithrilPartialSync.startPartialSync`** (the SAME
   store action `DaedalusDiagnosticsDialog.tsx:142` passes today); `onDismissMithrilPrompt` is wired to
   `mithrilPartialSync.dismissProactivePrompt`.
5. **i18n** — add NEW `mithrilProactivePrompt*` keys to `en-US.json` + `ja-JP.json` (first-class EN+JA, no
   `!!!`) and regenerate `translations/messages.json` via `yarn i18n:extract` from the `defineMessages` block
   in the new component. Do NOT hand-edit `source/renderer/app/i18n/locales/defaultMessages.json` (NOT the
   extract target; not imported at runtime — established by task-ux-304 Scope item 4).
6. **Tests** — new `SyncingConnectingMithrilPrompt.spec.tsx` (choice/confirm two-state, gating-by-props,
   dismiss, start-only-on-confirm); extend `MithrilPartialSyncStore.spec.ts` for the session flag. No AppStore
   or Diagnostics test changes (those files are no longer touched).

> **DROPPED from the original deep-link design (per D13):** `AppStore.ts` payload capture, the
> `DaedalusDiagnosticsDialog.tsx` `false`→flag change, the `openDaedalusDiagnosticsDialog.trigger({...})`
> payload, and `app-actions.ts` type-narrow. The prompt no longer touches Diagnostics. The already-built
> `showConfirmationOnOpen` seam (`MithrilPartialSyncSection.tsx:17,38-43`;
> `DaedalusDiagnostics.tsx:414,527,731-733`) is **left as-is, unused** — a benign residual, not this task's
> concern (a later cleanup/task may remove it; out of scope here).

## Non-goals (explicitly out of this task)

- **NO unconfirmed start path.** The choice-view "Mithril Sync (fast)" button NEVER starts partial sync — it
  only opens the concise confirm view. **Only the confirm-view "Start now" calls `startPartialSync`**, so
  confirmation precedes start (lock #1 below). No separate/second backend start channel.
- **NO Diagnostics deep-link.** The prompt does not open Diagnostics, does not use
  `openDaedalusDiagnosticsDialog` with a payload, and does not reuse the full `MithrilPartialSyncConfirmation`
  modal. The concise confirm is self-contained on the syncing screen.
- **NO renderer-computed threshold.** The gate booleans `isPartialSyncEnabled` / `isSignificantlyBehind` stay
  backend-owned; the renderer only consumes them. `behindByEpochs` is a DISPLAY figure, not the gate (lock #3).
- **NO change to the backend / availability contract** (`MithrilPartialSyncService.ts`,
  `mithril-partial-sync.types.ts`). No new backend field; no new start channel.
- **NO change to the confirmation modal / recommendation / overlay structure or copy** (task-ux-303/304 own
  them). The confirmation-modal sync-% removal and the sprint-wide "Mithril" terminology rollout (modal,
  recommendation, overlay) are **deferred to task-ux-601** — the orchestrator updates the JSON + PRD.
- **NO sync-% in the prompt** (epochs-only, per D13). No `…SyncContext` key.
- **NO persisted dismissal.** Session-scoped, in-memory only.
- **NO Storybook stories** (task-ux-502 owns partial-sync stories). Do NOT add/edit `*.stories.tsx`.
- **NO i18n of the hardcoded "Mithril Partial Sync" diagnostics row label** (task-ux-501).
- **Do NOT regress the empty-chain Mithril bootstrap flow** — `MithrilBootstrapPage`/`MithrilBootstrap.tsx`
  read-only. (`LoadingPage` already routes bootstrap separately from `SyncingConnectingPage`, `:71-86`.)

## Dependencies

- **task-ux-301 (completed)** — added the store availability flags **`isPartialSyncEnabled`**,
  **`isSignificantlyBehind`**, `behindByImmutables`, the `_refreshAvailability` poll, and `isWorking`.
- **task-ux-303 (completed)** — built the confirmation modal + start flow; this task **reuses its
  `startPartialSync` store action** for the confirm-view "Start now" (does NOT reuse its modal UI).
- **task-ux-304 (completed)** — established the canonical renderer node-tip `behindByEpochs` computation this
  prompt reuses (epochs-only here).

## Research / docs / workflows / skills consulted

- Tasks JSON `task-ux-302` — title/description/implementationNotes/targetPaths/acceptance carried + verified
  below.
- PRD **D1** (`…-prd.md:35-102`) — hybrid trigger; proactive prompt; session-scoped dismissal. The original
  D1 deep-link mechanism (`openDaedalusDiagnosticsDialog({ showMithrilConfirmation: true })` +
  `showConfirmationOnOpen`) is **superseded by D13** (orchestrator-owned amendment) — the prompt is now a
  self-contained fork + concise inline confirm + DIRECT start; it does NOT open Diagnostics.
- PRD **D13** (orchestrator-owned amendment, 2026-06-25) — fork prompt (Mithril vs Standard) with a concise
  inline confirm that starts partial sync directly via `startPartialSync`; user-facing "Mithril" term;
  confirmation-precedes-start preserved via the concise confirm.
- PRD **D2/D3** (`…-prd.md:104-141`+) — backend-owned `isSignificantlyBehind` gate + kill-switch (store
  observable `isPartialSyncEnabled`); "no renderer threshold".
- PRD **D11/D12** (`…-prd.md`) — canonical renderer node-tip `behindByEpochs` figure (epochs-only used here).
- Completed sibling **task-ux-301.md / task-ux-303.md / task-ux-304.md** — store flags, the reused
  `startPartialSync` action, canonical epochs figure.
- User memory `ux-copy-cardano-vocabulary.md` — epochs framing, NEVER immutable files, benefit-vs-waiting
  framing.
- `.agent/workflows/frontend.md` (React/MobX/container patterns), `.agent/system/state-management.md`.
- i18n conventions: `defineMessages` `!!!`-prefixed `defaultMessage`; `yarn i18n:extract` writes
  `translations/messages.json` (`package.json:52`); runtime catalogs `en-US.json` + `ja-JP.json` carry
  first-class strings (no `!!!`). `defaultMessages.json` is NOT the extract target (task-ux-304 Scope item 4).

---

## Current-state findings — already-built vs DELTA (verified against the working tree 2026-06-25)

> These findings are RETAINED from the original deep-link trace (all anchors re-verified). Under the **D13
> redesign** the deep-link seam below is **no longer used by this task** — it is left in place, unused (benign
> residual). The trace is kept to prove (a) the prompt does not need to touch any of it, and (b) why the
> AppStore/Diagnostics/app-actions edits are DROPPED. The load-bearing finding for the new design is the
> **start-action seam** and the **gate/figure source**, immediately below the trace.

### Deep-link seam — END-TO-END trace (LEFT UNUSED per D13)

| Hop | File:line | Live state | D13 disposition |
|---|---|---|---|
| Status-icon open trigger | `SyncingConnectingPage.tsx:97-102` | `openDaedalusDiagnosticsDialog.trigger()` with **NO payload** | UNCHANGED — status-icon open stays as-is; the prompt does NOT use this path |
| Action carries payload | `app-actions.ts:16` | `openDaedalusDiagnosticsDialog: Action<any>` (already payload-capable) | UNCHANGED — no edit (DROPPED) |
| AppStore listener | `AppStore.ts:50-53` | `openDaedalusDiagnosticsDialog.listen(() => { this._updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS); })` — payload ignored | UNCHANGED — no edit (DROPPED) |
| Container reads the flag | `DaedalusDiagnosticsDialog.tsx:138` | `showMithrilPartialSyncConfirmationOnOpen={false}` — HARDCODED false | UNCHANGED — stays `false` (DROPPED) |
| Component threads the prop | `DaedalusDiagnostics.tsx:414` (Props), `:527` (destructure), `:731-733` (pass) | `showMithrilPartialSyncConfirmationOnOpen?: boolean` → `<MithrilPartialSyncSection showConfirmationOnOpen={…}>` | BUILT — left intact, unused |
| Section opens on mount | `MithrilPartialSyncSection.tsx:17` (prop), `:38-43` (`componentDidMount` opens, respecting `isActionBlocked`) | opens confirmation on mount when `showConfirmationOnOpen && !isActionBlocked` | BUILT — left intact, unused (always fed `false`) |

**Conclusion (D13):** the Diagnostics-side deep-link threading is fully built but **no longer wired to the
prompt**. This task touches **none** of it. The prompt instead carries its own concise confirm and starts
directly via the store action below.

### Start-action seam — VERIFIED (the load-bearing seam for the new direct-start flow)

The prompt's confirm-view "Start now" must invoke the **same** backend start path the Diagnostics modal uses
today — **`mithrilPartialSync.startPartialSync`** (the store action), NOT a new channel:

| Hop | File:line | Symbol | Note |
|---|---|---|---|
| Store action | `MithrilPartialSyncStore.ts:202-231` | `@action startPartialSync = async () => {…}` | Sets `START_PENDING_STATUS`, `await mithrilPartialSyncStartChannel.request()`, re-throws `toStartError(startError)` on failure (so the confirm view can show a start error). Async. |
| IPC start channel | `MithrilPartialSyncStore.ts:215`, `ipc/mithrilPartialSyncChannel.ts:28` | `mithrilPartialSyncStartChannel.request()` | The single start channel — REUSED via the store action; do NOT call it directly. |
| Diagnostics wiring (the pattern to mirror) | `DaedalusDiagnosticsDialog.tsx:142` | `onStartMithrilPartialSync={mithrilPartialSync.startPartialSync}` | Proves the store action is the canonical start entry. |
| Section confirm-start (reference behavior) | `MithrilPartialSyncSection.tsx:83-104` | `startFromConfirmation` → `await onStartMithrilPartialSync()`, hides confirm on success, shows `startError` on reject | The new prompt's confirm view mirrors this success/error handling locally. |
| Also reused for retry | `App.tsx:115` | `onRetry={mithrilPartialSync.startPartialSync}` | Confirms the action is the shared start entry across surfaces. |

→ `SyncingConnectingPage` passes `mithrilPartialSync.startPartialSync` as `onStartMithrilSync`; the prompt's
confirm view `await`s it on "Start now" and, on reject, shows a start-error line (mirroring
`MithrilPartialSyncSection.startFromConfirmation`). No new start channel; no second backend start path.

### Where the gate booleans + `behindByEpochs` come from (verified)

- **Store availability flags** (task-ux-301): `MithrilPartialSyncStore.ts:65-67`
  `@observable isPartialSyncEnabled = false; @observable isSignificantlyBehind = false; @observable
  behindByImmutables: number | undefined`. Set by `_applyAvailability` (`:184-193`) from the
  `mithrilPartialSyncAvailabilityChannel`. **CONFLICT vs JSON/PRD wording:** the brief/PRD say `isEnabled`; the
  **live store observable is `isPartialSyncEnabled`** (the IPC payload field is `isEnabled`, surfaced on the
  store as `isPartialSyncEnabled`). Prefer live code: gate on
  `mithrilPartialSync.isPartialSyncEnabled && mithrilPartialSync.isSignificantlyBehind`.
- **The store is registered** as `stores.mithrilPartialSync` (`stores/index.ts:41,65,119`) and is read directly
  by containers today (e.g. `DaedalusDiagnosticsDialog.tsx:51,133-136`). So `SyncingConnectingPage` can read
  `this.props.stores.mithrilPartialSync` directly — it is NOT limited to the `networkStatus` store.
- **`behindByEpochs` computation** (task-ux-304, `DaedalusDiagnostics.tsx:570-577`):
  ```ts
  const networkEpoch =
    networkTip && Number.isFinite(networkTip.epoch) ? networkTip.epoch : null;
  const localEpoch =
    localTip && Number.isFinite(localTip.epoch) ? localTip.epoch : null;
  const behindByEpochs =
    networkEpoch !== null && localEpoch !== null
      ? Math.max(1, networkEpoch - localEpoch)
      : undefined;
  ```
  `SyncingConnectingPage` already destructures `networkStatus` (`:18`) which exposes `networkTip`/`localTip`
  (same `NetworkStatusStore` source the Diagnostics container uses, `DaedalusDiagnosticsDialog.tsx:70-71`,
  `:144-145`). Mirror this EXACT block in the container. `TipInfo.epoch` is a non-nullable `number`; the tip
  object is `| null | undefined` — guard the tip, not the epoch (task-ux-304 verified).
- **sync-% is NOT used** (epochs-only, D13) — no `syncPercentage`/`formattedNumber` is needed in the container
  or the prompt.

### SyncingConnecting injection model (verified)

- `SyncingConnecting.tsx` is a **pure `@observer` class component** (`:54-225`) that receives ALL data via
  `Props` (`:23-52`) from its container; it does **not** inject stores. The new prompt data/handlers must be
  added to `Props` and passed by the container.
- `SyncingConnectingPage.tsx` is the `@inject('stores','actions')` container (`:9-11`); it already destructures
  `networkStatus`/`app`/`profile`/`appUpdate`/`newsFeed` (`:18-19`) and already wires
  `openDaedalusDiagnosticsDialog.trigger()` (no payload) for the status-icon click (`:68`, `:97-102`).

---

## Files expected to change (exact paths — revised per D13)

1. `source/renderer/app/stores/MithrilPartialSyncStore.ts` — add session flag + `dismissProactivePrompt`
   action (reuse the existing `startPartialSync`).
2. `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` **(NEW)** —
   two-state (choice/confirm) prompt component + its `defineMessages`.
3. `source/renderer/app/components/loading/syncing-connecting/SyncingConnecting.tsx` — new props + mount.
4. `source/renderer/app/containers/loading/SyncingConnectingPage.tsx` — gate, epochs figure, start/dismiss
   handler wiring.
5. `source/renderer/app/i18n/locales/en-US.json` — new `mithrilProactivePrompt*` keys.
6. `source/renderer/app/i18n/locales/ja-JP.json` — same keys, JA values.
7. `translations/messages.json` (regenerated by `yarn i18n:extract`).
8. Tests: `SyncingConnectingMithrilPrompt.spec.tsx` (NEW), `MithrilPartialSyncStore.spec.ts` (extend).

> **DROPPED from the original list (per D13):** `AppStore.ts`, `DaedalusDiagnosticsDialog.tsx`,
> `app-actions.ts`, `DaedalusDiagnostics.tsx`, `MithrilPartialSyncSection.tsx`, and any AppStore spec — the
> prompt no longer touches Diagnostics or the dialog payload.
>
> Do NOT stage `source/renderer/app/i18n/locales/defaultMessages.json` (not the extract target; not imported
> at runtime — task-ux-304 Scope item 4).

---

## DESIGN DECISIONS (resolved — implementer makes none except pasting the approved copy)

### DD1 — Session-dismissal flag on the store (mirror `isCompletedOverlayDismissed`)
Add to `MithrilPartialSyncStore`:
```ts
@observable proactivePromptDismissedThisSession = false;

@action
dismissProactivePrompt = () => {
  this.proactivePromptDismissedThisSession = true;
};
```
- In-memory only; NOT persisted (no IPC, no localStorage) — exactly like `isCompletedOverlayDismissed`
  (`:64`, `:196-200`). It naturally resets on app restart because the store is reconstructed.
- Do **not** reset it on status changes (unlike `isCompletedOverlayDismissed`, which `_updateStatus` resets on
  non-completed status `:152-154`). The dismissal is for the whole session per D1 (`:50-51`).

### DD2 — Prompt is a NEW two-state component, mounted from `SyncingConnecting`
Create `SyncingConnectingMithrilPrompt.tsx` co-located with the other syncing-connecting parts (repo
convention: `ReportIssue.tsx`, `StatusIcons.tsx`, etc. are sibling components composed by `SyncingConnecting`).
The component owns a local `view` state — it is the only place that needs internal state, so a small class (or
hooks) component is fine; mirror the class+`intlShape` pattern of `MithrilPartialSyncConfirmation.tsx:98-101`.
Gating (whether to render at all) is decided by the container; the prompt owns ONLY the choice↔confirm
transition and the start-error display.

Component shape:
```ts
type Props = {
  behindByEpochs?: number;
  onStart: () => Promise<void>; // = mithrilPartialSync.startPartialSync (re-throws on failure)
  onDismiss: () => void;        // = mithrilPartialSync.dismissProactivePrompt
};
type State = {
  view: 'choice' | 'confirm';
  isStarting: boolean;
  startError: string | null;
};
```
Behavior:
- **Choice view** (`view === 'choice'`, default): renders the epochs body (line 1 `promptBody` with
  `{ epochs: behindByEpochs }` when finite, else `promptBodyUnknown`; line 2 `promptBodyBenefit` as a separate
  clean sentence/element), the handoff note `promptHandoffNote`, and two buttons —
  **primary "Mithril Sync (fast)"** (`promptMithrilButton`, visually emphasized/lead) → `setState({ view:
  'confirm' })`; **secondary "Standard Sync (slow)"** (`promptStandardButton`) → `onDismiss()`. The fast button
  alone NEVER starts (lock #1).
- **Confirm view** (`view === 'confirm'`): renders the concise confirm body `promptConfirmBody`, and two
  buttons — **primary "Start now"** (`promptConfirmStart`) → `handleStart`; **secondary "Cancel"**
  (`promptConfirmCancel`) → `setState({ view: 'choice', startError: null })`. On `handleStart`:
  ```ts
  handleStart = async () => {
    this.setState({ isStarting: true, startError: null });
    try {
      await this.props.onStart();
      // success: the store flips status to a working/overlay status; the prompt naturally unmounts
      // (gate goes false / the syncing overlay takes over) — no extra navigation needed.
    } catch (error) {
      if (!this._isMounted) return;
      this.setState({
        isStarting: false,
        startError: error instanceof Error ? error.message : 'Unable to start Mithril sync.',
      });
    }
  };
  ```
  Disable "Start now" while `isStarting`. Mirror the mounted-guard pattern from
  `MithrilPartialSyncSection.tsx:36,45-47,71-74,92-104` (`_isMounted` flag set in
  `componentDidMount`/`componentWillUnmount`) so a late reject after unmount does not setState.
- `hasBehindFigure = typeof behindByEpochs === 'number' && Number.isFinite(behindByEpochs)` (mirror
  `MithrilPartialSyncConfirmation.tsx:114-115`).

> Why the prompt unmounts itself on success (not the container): `startPartialSync` (`:202-231`) immediately
> sets `START_PENDING_STATUS` (`stopping-node`), which is a working/overlay status. The container gate
> (`isSignificantlyBehind` typically still true, but the syncing-screen presentation yields to the partial-sync
> overlay flow per the existing loading routing). The prompt does not need to self-dismiss; if it remains
> mounted briefly, "Start now" is disabled via `isStarting`. No new navigation is added by this task.

### DD3 — Gating + data flow through the container
`SyncingConnecting` is pure; the container decides everything. In `SyncingConnectingPage.tsx`:
1. Destructure the store: add `mithrilPartialSync` to `const { … } = this.props.stores;` (`:18`).
2. Compute the gate (live observable names — note the `isPartialSyncEnabled` conflict above):
   ```ts
   const showMithrilPrompt =
     mithrilPartialSync.isPartialSyncEnabled &&
     mithrilPartialSync.isSignificantlyBehind &&
     !mithrilPartialSync.proactivePromptDismissedThisSession;
   ```
3. Compute `behindByEpochs` from `networkStatus.networkTip`/`.localTip` using the EXACT task-ux-304 block
   (Current-state findings above). Add `networkTip`, `localTip` to the `networkStatus` destructure (`:20-36`).
4. Pass into `<SyncingConnecting>`: `showMithrilPrompt`, `mithrilBehindByEpochs={behindByEpochs}`,
   `onStartMithrilSync={mithrilPartialSync.startPartialSync}`,
   `onDismissMithrilPrompt={mithrilPartialSync.dismissProactivePrompt}`.

`SyncingConnecting.tsx` adds these to `Props` (`:23-52`), destructures them in `render()` (`:144-171`), imports
the component, and inside `render()` `<div className={styles.content}>` after `<LogosDisplay …/>` (`:200`)
renders:
```tsx
{showMithrilPrompt && (
  <SyncingConnectingMithrilPrompt
    behindByEpochs={mithrilBehindByEpochs}
    onStart={onStartMithrilSync}
    onDismiss={onDismissMithrilPrompt}
  />
)}
```
- **Kill-switch (lock #5):** when `isPartialSyncEnabled` is false the gate is false → the prompt is fully
  unmounted (never a disabled control).
- **No Diagnostics/dialog action** is triggered. The container does NOT call `openDaedalusDiagnosticsDialog`
  for the prompt (the existing status-icon `openDaedalusDiagnosticsDialog.trigger()` at `:97-102` is unrelated
  and unchanged).

### DD4 — Epochs-only; NO sync-% (D13)
The prompt body is epochs-only: line 1 = epochs behind (or the unknown fallback), line 2 = the benefit
sentence. No sync-% line, no `…SyncContext` key, no `formattedSyncPercentage` prop. (The confirmation modal's
sync-% line is a separate surface, not touched here; its removal is task-ux-601.)

### DD5 — Direct start via the reused store action (NO deep-link)
- Confirm-view "Start now" calls the prop `onStart`, which the container binds to
  **`mithrilPartialSync.startPartialSync`** — the SAME action `DaedalusDiagnosticsDialog.tsx:142` uses and that
  `MithrilPartialSyncSection.startFromConfirmation` awaits. No new IPC/channel; no `openDaedalusDiagnosticsDialog`
  payload; no `showConfirmationOnOpen` wiring.
- `startPartialSync` re-throws `toStartError` on failure (`:230`); the confirm view catches it and shows a
  `startError` line (DD2), mirroring `MithrilPartialSyncSection.tsx:96-101`.
- **Lock #1 (confirmation precedes start):** the only call site of `onStart` is the confirm-view "Start now"
  button. The choice-view "Mithril Sync (fast)" only transitions `view → 'confirm'`. There is no path from the
  choice view (or from mount) to `onStart`.

### DD6 — Message keys (NEW) — `mithrilProactivePrompt*` family, "Mithril" user-facing term
Place the prompt keys under a NEW `daedalus.diagnostics.dialog.mithrilProactivePrompt*` family (the
`daedalus.diagnostics.dialog.` id prefix is shared by the whole Mithril family for catalog grouping; the
surface is the loading screen but the id is just a string key). User-facing copy uses **"Mithril" / "Mithril
Sync" / "standard sync"**, never "Mithril partial sync" (D13). Keys:

| Constant | id | Placeholders |
|---|---|---|
| `promptBody` | `daedalus.diagnostics.dialog.mithrilProactivePromptBody` | `{epochs}` |
| `promptBodyUnknown` | `daedalus.diagnostics.dialog.mithrilProactivePromptBodyUnknown` | — |
| `promptBodyBenefit` | `daedalus.diagnostics.dialog.mithrilProactivePromptBodyBenefit` | — |
| `promptHandoffNote` | `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote` | — |
| `promptMithrilButton` | `daedalus.diagnostics.dialog.mithrilProactivePromptMithrilButton` | — |
| `promptStandardButton` | `daedalus.diagnostics.dialog.mithrilProactivePromptStandardButton` | — |
| `promptConfirmBody` | `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmBody` | — |
| `promptConfirmStart` | `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmStart` | — |
| `promptConfirmCancel` | `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmCancel` | — |

EN+JA values are pasted from FINAL COPY. `promptBodyUnknown` (no `{epochs}`) is shown when `behindByEpochs` is
undefined so the prompt never fabricates a number; `promptBodyBenefit` (line 2) is shown in both cases.

---

## FINAL COPY — RESOLVED (D13, approved 2026-06-25)

> EN is the user-approved verbatim copy. JA is first-class (mirrors the established ja-JP Mithril rendering;
> note JA retains the untranslated term "Mithril" / "Mithril Sync"). NO `!!!` in runtime catalogs; `{epochs}`
> preserved in JA. The implementer pastes these verbatim into the `defineMessages` (`!!!`-prefixed) + the EN/JA
> catalogs.

```
# Choice view
promptBody (EN):            Your node is about {epochs} epochs behind.
promptBody (JA):            ノードは約{epochs}エポック遅れています。
promptBodyUnknown (EN):     Your node is behind the blockchain tip.
promptBodyUnknown (JA):     ノードはブロックチェーンの先端より遅れています。
promptBodyBenefit (EN):     Mithril can catch you up faster than the standard sync.
promptBodyBenefit (JA):     Mithrilを使えば、標準同期よりも速く追いつけます。
promptHandoffNote (EN):     If skipped, you can still start the Mithril sync from the Diagnostics screen.
promptHandoffNote (JA):     スキップした場合でも、Diagnostics画面からMithril syncを開始できます。
promptMithrilButton (EN):   Mithril Sync (fast)
promptMithrilButton (JA):   Mithril Sync（高速）
promptStandardButton (EN):  Standard Sync (slow)
promptStandardButton (JA):  標準同期（低速）

# Confirm view
promptConfirmBody (EN):     Mithril will stop your Cardano node, restore verified chain data, and restart it — so you catch up faster.
promptConfirmBody (JA):     Mithrilはお使いのCardanoノードを停止し、検証済みのチェーンデータを復元してから再起動します。これにより、より速く追いつけます。
promptConfirmStart (EN):    Start now
promptConfirmStart (JA):    今すぐ開始
promptConfirmCancel (EN):   Cancel
promptConfirmCancel (JA):   キャンセル
```

- Locked benefit wording preserved: confirm body keeps "verified … chain data … catch up faster".
- No "immutable files" and no "Mithril partial sync" term reaches the user (D13; lock #4/#6).

---

## Implementation approach — ordered, mechanical steps

> Precondition: FINAL COPY block above (RESOLVED) is pasted verbatim into the `defineMessages` (`!!!`-prefixed)
> + the EN/JA catalogs. Epochs-only — no sync-% anywhere.

### Step 1 — `MithrilPartialSyncStore.ts`: session flag + action (DD1)
File: `source/renderer/app/stores/MithrilPartialSyncStore.ts`.
1a. After `@observable isCompletedOverlayDismissed = false;` (`:64`) add:
```ts
@observable proactivePromptDismissedThisSession = false;
```
1b. After the `dismissCompletedOverlay` action (`:195-200`) add:
```ts
@action
dismissProactivePrompt = () => {
  this.proactivePromptDismissedThisSession = true;
};
```
1c. Do NOT reset it in `_updateStatus` (session-scoped, not status-scoped). REUSE the existing
    `startPartialSync` (`:202-231`) for the prompt's confirm-view "Start now" — no new action/channel.

### Step 2 — NEW `SyncingConnectingMithrilPrompt.tsx` (DD2 + DD6) — two-state choice/confirm
File: `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx`.
2a. `defineMessages` block with the DD6 keys (all nine: `promptBody {epochs}`, `promptBodyUnknown`,
    `promptBodyBenefit`, `promptHandoffNote`, `promptMithrilButton`, `promptStandardButton`,
    `promptConfirmBody`, `promptConfirmStart`, `promptConfirmCancel`), each `defaultMessage` `!!!`-prefixed with
    the FINAL COPY EN string, plus a `description`.
2b. `Props` + `State` per DD2; `static contextTypes = { intl: intlShape.isRequired }`; `_isMounted` flag set in
    `componentDidMount`/`componentWillUnmount` (mirror `MithrilPartialSyncSection.tsx:36,45-47`).
2c. **Choice view** (`state.view === 'choice'`): render `hasBehindFigure ? promptBody({ epochs: behindByEpochs })
    : promptBodyUnknown` (line 1), `promptBodyBenefit` (line 2, separate element), `promptHandoffNote`, and two
    buttons — primary "Mithril Sync (fast)" (`promptMithrilButton`, lead/emphasized) → `setState({ view:
    'confirm', startError: null })`; secondary "Standard Sync (slow)" (`promptStandardButton`) →
    `this.props.onDismiss()`.
2d. **Confirm view** (`state.view === 'confirm'`): render `promptConfirmBody` and two buttons — primary
    "Start now" (`promptConfirmStart`, `disabled={state.isStarting}`) → `this.handleStart`; secondary "Cancel"
    (`promptConfirmCancel`) → `setState({ view: 'choice', startError: null })`. Render `state.startError` (if
    set) as an error line.
2e. `handleStart` (DD2/DD5): `await this.props.onStart()`; on reject (and `_isMounted`) set
    `{ isStarting: false, startError }`. The choice-view fast button NEVER calls `onStart` (lock #1).
2f. `hasBehindFigure = typeof behindByEpochs === 'number' && Number.isFinite(behindByEpochs)`
    (`MithrilPartialSyncConfirmation.tsx:114-115`).
2g. Layout: a small local `.scss` (or reuse `SyncingConnecting.scss` classes); a functional card is the bar
    (task-ux-502 owns stories/visual polish). New `*.scss.d.ts` sidecars are gitignored — do not stage them.

### Step 3 — `SyncingConnecting.tsx`: new props + mount the prompt (DD3)
File: `source/renderer/app/components/loading/syncing-connecting/SyncingConnecting.tsx`.
3a. Add to `Props` (`:23-52`):
```ts
showMithrilPrompt: boolean;
mithrilBehindByEpochs?: number;
onStartMithrilSync: () => Promise<void>;
onDismissMithrilPrompt: () => void;
```
3b. Destructure them in `render()` (`:144-171`).
3c. Import the new component (`import SyncingConnectingMithrilPrompt from './SyncingConnectingMithrilPrompt';`).
3d. Inside `<div className={styles.content}>` after `<LogosDisplay …/>` (verified `:200`), add:
```tsx
{showMithrilPrompt && (
  <SyncingConnectingMithrilPrompt
    behindByEpochs={mithrilBehindByEpochs}
    onStart={onStartMithrilSync}
    onDismiss={onDismissMithrilPrompt}
  />
)}
```

### Step 4 — `SyncingConnectingPage.tsx`: gating + figure + handler wiring (DD3 + DD5)
File: `source/renderer/app/containers/loading/SyncingConnectingPage.tsx`.
4a. Add `mithrilPartialSync` to the `stores` destructure (`:18`); add `networkTip`, `localTip` to the
    `networkStatus` destructure (`:20-36`).
4b. In `render()` before the `return`, compute the EXACT task-ux-304 `behindByEpochs` block (Current-state
    findings) and:
```ts
const showMithrilPrompt =
  mithrilPartialSync.isPartialSyncEnabled &&
  mithrilPartialSync.isSignificantlyBehind &&
  !mithrilPartialSync.proactivePromptDismissedThisSession;
```
4c. Pass into `<SyncingConnecting>`: `showMithrilPrompt`, `mithrilBehindByEpochs={behindByEpochs}`,
    `onStartMithrilSync={mithrilPartialSync.startPartialSync}`,
    `onDismissMithrilPrompt={mithrilPartialSync.dismissProactivePrompt}`.
4d. **No** `openDaedalusDiagnosticsDialog` change — the existing status-icon handler (`:97-102`) is untouched;
    the prompt does NOT open Diagnostics.

### Step 5 — i18n catalogs (DD6)
> Edit ONLY `en-US.json`, `ja-JP.json`, and (regenerated) `translations/messages.json`. Do NOT touch
> `defaultMessages.json`.
5a. Run `yarn i18n:extract` to regenerate `translations/messages.json` from the new component's
    `defineMessages` (writes `translations/messages.json` per `package.json:52`).
5b. Add the nine DD6 keys to `en-US.json` (alphabetical placement within the `daedalus.diagnostics.dialog.*`
    block — `…mithrilProactivePrompt*` sorts just after `…mithrilPartialSync*`) with the FINAL COPY EN values.
5c. Add the same keys to `ja-JP.json` with the FINAL COPY JA values (preserve the `{epochs}` placeholder).
5d. Verify no `!!!` leaks: `grep -n '!!!' source/renderer/app/i18n/locales/en-US.json
    source/renderer/app/i18n/locales/ja-JP.json` → no new prompt hits. Verify no "immutable" and no
    "Mithril partial sync" / "部分同期" in any NEW prompt string (D13 terminology).

### Step 6 — Tests (see Verification)
Add `SyncingConnectingMithrilPrompt.spec.tsx`; extend `MithrilPartialSyncStore.spec.ts`. No AppStore /
Diagnostics test changes.

---

## Locked invariants this task honors (inline)

1. **Confirmation precedes start (lock #1).** The prompt NEVER starts on the choice-view fast button. The ONLY
   call site of `startPartialSync` is the **confirm-view "Start now"** button; "Mithril Sync (fast)" only
   transitions `view → 'confirm'`. The concise confirm is a real confirmation gate — there is no unconfirmed
   start path. (D13 preserves the original lock #3 via the concise confirm.)
2. **No second/unconfirmed backend start channel.** "Start now" reuses the existing
   `mithrilPartialSync.startPartialSync` store action (the same one `DaedalusDiagnosticsDialog.tsx:142` uses) →
   `mithrilPartialSyncStartChannel`. No new channel.
3. **No renderer-computed THRESHOLD.** The gate booleans `isPartialSyncEnabled` / `isSignificantlyBehind` are
   backend-owned; the renderer only consumes them. `behindByEpochs` is a DISPLAY figure, not the gate (D2/D12).
4. **No 'immutable files' AND no 'Mithril partial sync' language reaches the user.** The prompt uses epochs
   only + "Mithril" / "standard sync" (D13). The `behindByImmutables` store field is never displayed.
5. **Kill-switch gating:** when `isPartialSyncEnabled` is false the gate is false → the prompt is fully hidden
   (unmounted), never a disabled control (D3, lock #6 from task-ux-304).
6. **Session-scoped dismissal is in-memory only** (mirror `isCompletedOverlayDismissed`); NOT persisted across
   restarts.
7. **Bootstrap not regressed** — `LoadingPage` routes bootstrap separately (`LoadingPage.tsx:71-86`); this task
   only edits the SyncingConnecting path + the partial-sync store.

## Acceptance criteria (revised per D13 — made checkable)

- [ ] A dismissible **fork** prompt renders on the SyncingConnecting screen **only** when
  `isPartialSyncEnabled && isSignificantlyBehind && !proactivePromptDismissedThisSession`.
- [ ] The choice-view body uses the canonical epochs figure (renderer node-tip
  `max(1, networkTip.epoch − localTip.epoch)`) as line 1, a benefit-vs-standard-sync sentence as line 2, and a
  handoff note; shows NO "immutable files" and NO "Mithril partial sync" language; an epochs-unknown fallback
  renders when the figure is unavailable.
- [ ] Primary **"Mithril Sync (fast)"** opens a concise inline **confirm** step on the syncing screen (does NOT
  open Diagnostics, does NOT start). Confirm-view **"Start now"** invokes `mithrilPartialSync.startPartialSync`
  (the existing start path); **"Cancel"** returns to the choice view. Confirmation precedes start.
- [ ] Secondary **"Standard Sync (slow)"** sets `proactivePromptDismissedThisSession` (in-memory) so the prompt
  does not reappear this session; the handoff note tells the user they can start the Mithril sync later from
  Diagnostics.
- [ ] When `isPartialSyncEnabled` is false the prompt is fully hidden (no disabled control).
- [ ] New EN+JA `mithrilProactivePrompt*` keys are first-class (no `!!!` placeholders in runtime catalogs);
  epochs-only (no sync-% key).

## Verification plan (exact commands, from `/workspaces/mithril-partial-sync-ux`)

```bash
cd /workspaces/mithril-partial-sync-ux
yarn i18n:extract            # regenerate translations/messages.json with the new mithrilProactivePrompt* keys
grep -n '!!!' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json
                            # → no NEW prompt hits
grep -rni 'immutable' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json
                            # → no "immutable" in any new prompt string
grep -n 'mithrilProactivePrompt' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json
                            # → confirm none of the new strings contain "partial sync" / "部分同期" (D13 term)
yarn compile                # tsc --noEmit must pass (allow up to 600s)
yarn lint                   # ESLint clean on touched files
yarn test:jest \
  source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx \
  source/renderer/app/stores/MithrilPartialSyncStore.spec.ts
yarn prettier:check         # (or prettier --check on the touched files)
```

> KNOWN ENV WORKAROUND (recorded by task-ux-301/303/304): `yarn compile`'s `typedef:sass` precompile hook and
> jest's css-modules transform can crash under Node v24 dart-sass. If hit, run `node_modules/.bin/tsc --noEmit`
> directly as the authoritative TS gate, and run jest with `.scss` mapped to `identity-obj-proxy` via a
> CLI-only override (committed config unchanged). Pre-existing env defect, not a regression. New scss
> sidecars (`*.scss.d.ts`) are gitignored — do not stage them.

### Tests to add/update

**NEW `SyncingConnectingMithrilPrompt.spec.tsx`** (wrap in `IntlProvider` with `en-US.json`, like
`MithrilPartialSyncSection.spec.tsx:36-40`):
- **Choice view, known figure:** `behindByEpochs={3}` → renders the EN body line 1 with "3" + the benefit
  line; asserts NO `/immutable/i` and NO `/partial sync/i` text.
- **Choice view, unknown figure:** `behindByEpochs={undefined}` → renders `promptBodyUnknown`; asserts no
  `/undefined/` text.
- **Fast button does NOT start:** click "Mithril Sync (fast)" with a spy `onStart` → confirm view shows
  ("Start now" present); `onStart` NOT called yet (lock #1).
- **Dismiss:** click "Standard Sync (slow)" → `onDismiss` called once; `onStart` never called.
- **Confirm → start:** from confirm view, click "Start now" → `onStart` called once.
- **Confirm → cancel:** from confirm view, click "Cancel" → returns to choice view ("Mithril Sync (fast)"
  visible again); `onStart` not called.
- **Start error:** `onStart` rejecting with `new Error('…')` → after click, the confirm view shows the error
  message (mirror `MithrilPartialSyncSection.spec.tsx:126-152`).
- **Late-reject after unmount does not setState** (mirror `MithrilPartialSyncSection.spec.tsx:84-124`).

**`MithrilPartialSyncStore.spec.ts`** (extend — unit-test the `@action`s directly, no full `setup()` harness
where avoidable):
- `proactivePromptDismissedThisSession` defaults `false`; calling `store.dismissProactivePrompt()` sets it
  `true`.
- A status update via `registeredStatusHandler` (e.g. → `idle`) does NOT reset
  `proactivePromptDismissedThisSession` (session scope, distinct from `isCompletedOverlayDismissed` which IS
  reset on non-completed status, `:152-154`).

> No AppStore / DaedalusDiagnostics / MithrilPartialSyncSection test changes — those files are untouched. The
> already-built `showConfirmationOnOpen` tests stay green (the seam is left as-is, fed `false`).

## Risks / open questions

- **`interactive_decision`:** RESOLVED — flow + EN copy approved (D13); FINAL COPY block is pinned. No
  remaining copy stop.
- **CONFLICT — `isEnabled` vs `isPartialSyncEnabled`:** the JSON/PRD say `isEnabled`; the live store observable
  is `isPartialSyncEnabled` (the IPC payload field is `isEnabled`). Prefer live code: gate on
  `isPartialSyncEnabled`. Flagged so the implementer does not invent a non-existent `isEnabled` getter.
- **Prompt self-unmount on success:** `startPartialSync` immediately sets `START_PENDING_STATUS`
  (`stopping-node`), a working/overlay status; the syncing-screen presentation yields to the partial-sync
  overlay flow. The prompt does not navigate; if it remains briefly mounted, "Start now" is disabled via
  `isStarting`. If QA finds the prompt lingers, a follow-up may add an explicit local "started" view — out of
  scope unless observed.
- **Residual unused deep-link seam:** `showConfirmationOnOpen` + the `DaedalusDiagnostics`/`Section` threading
  remain in the tree, fed `false`. Benign; a later cleanup task (or task-ux-601) may remove them. Not this
  task's concern.
- **`epoch === 0` near genesis:** `Number.isFinite` treats `0` as valid; figure floored at 1; matches
  task-ux-304's established guard. Not a regression.
- **Prompt visual placement/polish:** a functional card is the bar here; visual refinement + a Storybook story
  are task-ux-502's scope. Do not block on `.scss` polish.
- **Env defect (Node v24 dart-sass):** use the recorded `tsc --noEmit` + identity-obj-proxy workarounds.

## Required doc / research updates

- **PRD amendment D13** (orchestrator-owned) records the redesign: fork prompt + concise inline confirm +
  DIRECT start (no Diagnostics deep-link), user-facing "Mithril" term. The original D1 deep-link mechanism is
  superseded for the prompt. (The orchestrator writes D13 + updates the tasks JSON targetPaths/acceptance.)
- **Deferred to task-ux-601** (orchestrator updates JSON + PRD): the confirmation-modal sync-% removal and the
  sprint-wide "Mithril" terminology rollout to the modal / recommendation / overlay.
- Record in `task-ux-302-research.md`: the verified start-action seam (`MithrilPartialSyncStore.startPartialSync`
  `:202-231` → `mithrilPartialSyncStartChannel`; mirrored from `DaedalusDiagnosticsDialog.tsx:142`), the
  `isEnabled` vs `isPartialSyncEnabled` conflict (resolved to live code), the reused task-ux-304 `behindByEpochs`
  block, and the now-unused deep-link seam left in place.

## Review-log paths

- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-302-plan-review.md` (append-only;
  Planner entry started this stage).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-302-impl-review.md` (append-only;
  Implementation / Code-Review entries appended later).
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-3/task-ux-302-research.md` (durable findings).

## Commit (single, subject-only — no body, no Co-Authored-By)
`feat(mithril): task-ux-302 add proactive syncing-screen Mithril fork prompt with inline confirm`
Stage ONLY: `MithrilPartialSyncStore.ts`, `SyncingConnectingMithrilPrompt.tsx` (+ its `.scss` if added),
`SyncingConnecting.tsx`, `SyncingConnectingPage.tsx`, `en-US.json`, `ja-JP.json`, `translations/messages.json`,
and the new/extended spec files. Do NOT stage `defaultMessages.json` or any `*.scss.d.ts` sidecar.

## Status
- Planning status: `approved`
- Build status: `completed`

---

## Copy — RESOLVED (interactive_decision closed, D13, 2026-06-25)

The fork-prompt flow + the EN copy are user-approved; the pinned EN+JA strings live in the **FINAL COPY** block
above and are the single source of truth for the implementer. The original copy-options menu (Review/Not now,
sync-% include/omit, "Mithril partial sync" body) is **superseded** by D13:
- Body is epochs-only ("Your node is about {epochs} epochs behind." + "Mithril can catch you up faster than the
  standard sync.").
- Buttons are the fork pair **"Mithril Sync (fast)" / "Standard Sync (slow)"** (choice view) and **"Start now" /
  "Cancel"** (confirm view) — NOT Review/Not now.
- Handoff note: "If skipped, you can still start the Mithril sync from the Diagnostics screen."
- NO sync-% line (epochs-only); user-facing term is **"Mithril"**, never "Mithril partial sync".

No open copy-approval questions remain.

---

## Final Outcome (completed 2026-06-25)

Shipped the D13 fast/standard fork prompt with a concise inline confirm; planning + implementation + both
subagent reviews are clean.

**Files created/modified (task-relevant):**
- NEW `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` — two-state
  (`choice` → `confirm`) presentational prompt; `_isMounted` guard; `isStarting` double-start guard; `defineMessages`
  with the 9 `mithrilProactivePrompt*` keys.
- NEW `…/SyncingConnectingMithrilPrompt.scss` — local layout (visual polish/positioning deferred to task-ux-502).
- NEW `…/SyncingConnectingMithrilPrompt.spec.tsx` — 8 component tests.
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — `@observable proactivePromptDismissedThisSession` +
  `@action dismissProactivePrompt` (mirrors `isCompletedOverlayDismissed`; NOT reset by `_updateStatus`); reuses the
  existing `startPartialSync` action (no new channel). `+` 2 store tests in `MithrilPartialSyncStore.spec.ts`.
- `…/SyncingConnecting.tsx` — 4 additive props; mounts the prompt after `LogosDisplay`.
- `…/containers/loading/SyncingConnectingPage.tsx` — reads the `mithrilPartialSync` store; computes the gate
  (`isPartialSyncEnabled && isSignificantlyBehind && !proactivePromptDismissedThisSession`) and `behindByEpochs`
  (`Math.max(1, networkTip.epoch − localTip.epoch)`, mirroring task-ux-304) with a behind-unknown fallback; binds
  `onStartMithrilSync = startPartialSync` and `onDismissMithrilPrompt = dismissProactivePrompt`.
- i18n: 9 first-class EN+JA keys in `en-US.json` + `ja-JP.json`; `translations/messages.json` regenerated via
  `yarn i18n:extract`. `defaultMessages.json` left untouched.

**Verification:** `tsc --noEmit` PASS · eslint (changed files) PASS · prettier --check PASS · jest 25/25 PASS
(8 component + 17 store). `yarn i18n:check` PASS (no `!!!`); greps confirm no `immutable` / `partial sync` /
sync-% in any new user-facing key. (Jest/tsc used the repo's known Node-v24 dart-sass workaround — committed
config unchanged.)

**Locked invariants — all hold:** single `startPartialSync` call-site is the confirm-view "Start now"
(`SyncingConnectingMithrilPrompt.tsx:120`); the fast button only reveals the confirm view (no start on mount) →
confirmation-precedes-start (lock #3) preserved; gate on backend booleans only (no renderer threshold); kill-switch
hides (never disables); in-memory session dismissal; epochs-only "Mithril" vocabulary (no immutable-file / partial-sync
/ sync-% language to the user).

**Deviations / residuals:** gated on live observable `isPartialSyncEnabled` (not the JSON's `isEnabled`); the
already-shipped `showConfirmationOnOpen` deep-link seam is left in place, unused (benign residual) — its removal +
the sprint-wide "Mithril" vocabulary rollout + the confirmation-modal sync-% removal are tracked under **task-ux-601**.
`.scss` positioning polish is task-ux-502.

**Reviews:** plan-critique `Decision: approved` (×2, pass-1 on the original design + pass-2 on the D13 redesign);
code-review `Decision: approved` (no blockers). Logs: `task-ux-302-plan-review.md`, `task-ux-302-impl-review.md`.
