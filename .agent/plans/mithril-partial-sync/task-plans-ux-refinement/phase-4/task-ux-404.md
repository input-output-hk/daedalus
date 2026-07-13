# task-ux-404 — Overlay recovery action safety + success-dismiss finalize wiring

- **Phase:** phase-4 (Renderer Progress And Error Overlay Honesty)
- **Implements:** PRD **D5c** (hide Cancel for all post-cutover phases, gap #9 / lock #6), **D5d** (defensive Quit fallback / no dead-end, gaps #8/#31), **D5f** renderer half (disable Cancel during `stopping-node` + always resync after cancel, gap #39), **D8 #33** (remove dead `onWipeRetry`/`onDecline` from the overlay path) and **D8 #24** (document retry = start-path reuse), and the **dismiss half of D9** (`dismissCompletedOverlay` invokes the finalize channel; lock #16 as amended). Closes research-19 **gaps #8, #9, #31, #33, #39 (renderer)** and **documents #24**.
- **Planning status:** `approved`
- **Build status:** `completed`
- **Interaction mode:** `autonomous`
- **Single commit subject (planned):** `feat(mithril): task-ux-404 harden overlay recovery actions and wire success-dismiss finalize`

---

## Why now

task-ux-401/402/403 made the partial-sync overlay's progress readout, live affordances, and error *copy* honest. The remaining dishonesty is in the overlay's **recovery affordances and lifecycle wiring** — the safety/finalize layer:

- **gap #9 / D5c:** Cancel is hidden only for `starting-node` (`MithrilPartialSyncOverlay.tsx:123`), so it still renders during `installing` and `finalizing`. The backend rejects cancel post-cutover, so the button is a lie there.
- **gaps #8/#31 / D5d:** the overlay builds its `actions[]` purely from `canRetry`/`canRestartNormally`/`canWipeAndFullSync`. If all three are false the array is `[]`, and `MithrilErrorView` renders **zero** buttons (`actions || [...]` short-circuits on a truthy empty array) — an unclosable dead-end. Not reachable today (backend always emits ≥1), but there is no safety net.
- **gap #39 / D5f:** clicking Cancel during the multi-minute `stopping-node` window is a silent no-op (`MithrilPartialSyncService.cancel` early-returns while no work dir/process exists) and the store never resyncs, so the button looks dead.
- **#33 / D8:** the overlay still passes dead `onWipeRetry`/`onDecline` props into `MithrilErrorView` (it always passes an explicit `actions[]`, so those handlers are unreachable on this path).
- **D9 dismiss half:** `dismissCompletedOverlay` is a renderer-only flag flip (`MithrilPartialSyncStore.ts:220-225`); the finalize IPC channel (built end-to-end by task-ux-202) is **never invoked**, so the backend never resets to idle / cleans staging / clears the marker on success dismiss.

This is renderer-only. The backend correctness pieces (true abort-during-stop, staging cleanup, marker C2) are the backend-correctness track (D7) and task-ux-202/203 — **already in place**; this task wires the renderer to them.

---

## Interaction mode + justification

**`autonomous`. `needsUserDecision = false`.** Every one of the five items is functional safety/recovery wiring or functional copy fully governed by ALREADY-LOCKED rules and PRD decisions: hide-cancel-post-cutover (#6 / D5c), render recovery strictly from `allowedRecoveryActions` + defensive-Quit-only-when-empty (#5 / D5d), disable-cancel-during-stop + resync-after-cancel (D5f), dead-prop removal (#33), and finalize-on-dismiss (#16 as amended by D9). The two new strings are functional, not marketing: a Quit button label and a one-line disabled-Cancel explanation whose exact text the PRD already dictates ("Cancellation available once the node has stopped", D5f). The app-quit seam is pinned below to an existing renderer→main channel, so item 2 needs no new mechanism. No blocking decision exists that the locked rules / PRD cannot resolve.

---

## Scope

1. **Item 1 — Hide Cancel for all post-cutover phases.** Extend `hideAction` (`MithrilPartialSyncOverlay.tsx:123`) from `status === 'starting-node'` to `['installing','finalizing','starting-node'].includes(status)`.
2. **Item 2 — Defensive Quit fallback (renderer-only).** When the overlay's recovery-actions array is empty (`!canRetry && !canRestartNormally && !canWipeAndFullSync`), render a single **Quit** action instead, wired to a new `onQuit` prop. `quit` is NOT an `allowedRecoveryActions` value (the union is `retry | restart-normal | wipe-and-full-sync`), so this is purely renderer-side and never inferred from status names (lock #5). The Quit handler reuses the existing app-close seam (pinned below).
3. **Item 3 — Disable Cancel during `stopping-node` + tooltip + resync-after-cancel.** Add an `actionDisabled`/`actionDisabledTooltip` capability to `MithrilProgressView` (react-polymorph `PopOver`); the overlay disables Cancel and shows the explanatory tooltip while `status === 'stopping-node'`. In the store, `cancelPartialSync` always calls `syncStatus()` afterwards (try/finally) so the UI never sticks.
4. **Item 4 — Remove dead `onWipeRetry`/`onDecline` from the overlay path.** Drop them from the overlay's `<MithrilErrorView>` call site (`:173-174`) and make them **optional** in `MithrilErrorView`'s `Props` (`onWipeRetry?(): void; onDecline?(): void;`). Do NOT delete them — the empty-chain bootstrap (`MithrilBootstrap.tsx:205-213`) uses them LIVE via the default fallback.
5. **Item 5 — `dismissCompletedOverlay` invokes the finalize channel.** Import `mithrilPartialSyncFinalizeChannel` in the store, make the action `async`, and `await mithrilPartialSyncFinalizeChannel.request()` after flipping the dismiss flag (the wrapper already exists from task-ux-202). Keep the success screen visible until explicit dismiss (lock #16 / D9). Document that `retry` re-invokes `startPartialSync` with no dedicated channel (#24).

### Non-goals (do NOT do here)

- **No** new `allowedRecoveryActions` value, no `quit` enum, no backend/IPC/contract change. The defensive Quit is a renderer-only last-resort rendered ONLY for the empty-actions case (lock #5).
- **No** generic "close & ignore" / dismiss on post-cutover failures — that would bypass the safety boundary (PRD D5d explicitly forbids it).
- **No** change to which recovery actions render from a non-empty `allowedRecoveryActions` (the `canRetry`/`canRestartNormally`/`canWipeAndFullSync` array logic is untouched except for the empty-case substitution).
- **No** backend `MithrilPartialSyncService.cancel` change (true abort-during-stop is deferred to D7); item 3 is the renderer disable + resync only.
- **No** change to `MithrilErrorView`'s `ERROR_COPY_BY_STAGE` / bootstrap title-hint fallback (lock #11 — bootstrap depends on it).
- **No** error-copy change (that was task-ux-403; `resolvePartialSyncErrorCopy` stays as-is).
- **No** removal of `onWipeRetry`/`onDecline` from `MithrilErrorView` or from `MithrilBootstrap.tsx` (bootstrap still wires them).

---

## Dependencies

JSON `dependencies: ["task-ux-202","task-ux-203","task-ux-403"]` — all **completed**.
- **task-ux-202** built the finalize channel end-to-end (api const + types, main handler → `controller.finalizePartialSync()`, renderer wrapper `mithrilPartialSyncFinalizeChannel`) and explicitly handed the renderer `dismissCompletedOverlay → finalize` wiring to this task. Verified live below.
- **task-ux-203** is the backend `stopping-node` re-emit / resync pairing for D5f; this task adds the renderer disable + `syncStatus()`-after-cancel half.
- **task-ux-403** owns the error-copy resolver already wired into the overlay (`MithrilPartialSyncOverlay.tsx:75,131-132`); this task does not touch it.

---

## Research / docs / workflows / skills consulted

- PRD `mithril-partial-sync-ux-refinement-prd.md`: **D5c** lines 307-310, **D5d** lines 312-317, **D5f** lines 322-331, **D8 #33** line 408, **D8 #24** lines 391-393, **D9** lines 427-496 (ordered finalization steps 462-470; dismiss→finalize seam 471-485; amends #16 492-496), components-impact lines 900-902, store-impact lines 881-884, acceptance lines 813-814 / 821-827.
- research-19 `19-ux-refinement-state-and-gaps.md`: **gap #8** (line 135), **gap #9** (line 136), **gap #24** (line 151), **gap #31** (line 158), **gap #33** (line 160), **gap #39** (line 392).
- prompt `prompt-ux-refinement.md`: locked safety boundaries **#5/#6/#9 (#16 as amended by D9)/#11**, "smallest truthful change, reuse seams" (retry=start; finalize is the one new channel, already built).
- **task-ux-403** four docs — format template + the Node v24 `tsc`/jest scss-sidecar/i18n env workarounds.
- Grounding brief `/tmp/.../task-ux-404-grounding.md` — every seam mapped with line anchors; all re-verified live below.
- Workflows: `.agent/workflows/frontend.md`, `.agent/workflows/ipc.md`, `.agent/workflows/test.md`.

---

## Live-code verification (re-verified against the working tree 2026-06-26)

| Anchor | Live finding | Status |
|---|---|---|
| Cancel-visibility seam | `MithrilPartialSyncOverlay.tsx:123` — `hideAction={status === 'starting-node'}` | CONFIRMED — item 1 target |
| Single action button handler | `MithrilPartialSyncOverlay.tsx:125` — `onAction={status === 'completed' ? onDismissCompleted : onCancel}` | CONFIRMED — leave as-is |
| Recovery `actions[]` from the 3 booleans | `MithrilPartialSyncOverlay.tsx:133-172` — spreads `canRetry`/`canRestartNormally`/`canWipeAndFullSync`; empty array if all false | CONFIRMED — item 2 target |
| Dead props passed to error view | `MithrilPartialSyncOverlay.tsx:173-174` — `onWipeRetry={onRetry}` / `onDecline={onRestartNormally}` | CONFIRMED — item 4 removes these |
| Overlay has no `allowedRecoveryActions` prop | Props `MithrilPartialSyncOverlay.tsx:20-35` — only the three booleans; "empty" is `!canRetry && !canRestartNormally && !canWipeAndFullSync` | CONFIRMED |
| `MithrilErrorView` renders zero buttons on `[]` | `MithrilErrorView.tsx:97-108` `resolvedActions = actions \|\| [default]` (`[]` is truthy → 0 buttons); `:155-168` maps them | CONFIRMED — the dead-end |
| `MithrilErrorView` props REQUIRED today | `MithrilErrorView.tsx:28-29` `onWipeRetry(): void;` / `onDecline(): void;` (no `?`); used only in the default fallback `:100,105` | CONFIRMED — make optional, do not delete |
| Bootstrap uses the fallback LIVE | `MithrilBootstrap.tsx:205-213` renders `<MithrilErrorView … onWipeRetry … onDecline … />` with **no `actions` prop** | CONFIRMED — must not regress (lock #11) |
| Cancel button in progress view | `MithrilProgressView.tsx:207-217` — `<Button … onClick={onAction} disabled={isStartingNode} />`; `isStoppingNode` computed `:95` but not used for disable | CONFIRMED — item 3 target |
| Store `dismissCompletedOverlay` (flag-only) | `MithrilPartialSyncStore.ts:220-225` — flips `isCompletedOverlayDismissed` only; no finalize | CONFIRMED — item 5 target |
| Store `cancelPartialSync` (no resync) | `MithrilPartialSyncStore.ts:263-266` — `await mithrilPartialSyncCancelChannel.request()`; no `syncStatus()` | CONFIRMED — item 3 target |
| Store imports (finalize absent) | `MithrilPartialSyncStore.ts:15-22` — availability/cancel/restartNormal/start/status/wipe only | CONFIRMED — add finalize import |
| Store `syncStatus` exists | `MithrilPartialSyncStore.ts:137-145` — `_isTornDown`-guarded; pulls status channel + `_updateStatus` | CONFIRMED |
| Dismiss flag reset on non-completed | `MithrilPartialSyncStore.ts:178-180` — `if (update.status !== 'completed') isCompletedOverlayDismissed = false` | CONFIRMED — visibility preserved until dismiss |
| Finalize renderer wrapper EXISTS | `source/renderer/app/ipc/mithrilPartialSyncChannel.ts:58-61` — `export const mithrilPartialSyncFinalizeChannel = new RendererIpcChannel(MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL)` | CONFIRMED — item 5 imports + calls it |
| Finalize channel const + types | `source/common/ipc/api.ts:489-492` — `MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL`, request `void`, response `void` | CONFIRMED |
| Finalize main handler | `source/main/ipc/mithrilPartialSyncChannel.ts:127-129` — `mithrilPartialSyncFinalizeChannel.onRequest(async () => { await controller.finalizePartialSync(); })` | CONFIRMED — `.request()` pairs with `onRequest` |
| `quit` is NOT a backend value | `mithril-partial-sync.types.ts:17-20` — `MithrilPartialSyncFailureAction = 'retry' \| 'restart-normal' \| 'wipe-and-full-sync'` | CONFIRMED — Quit is renderer-only |
| **App-quit seam (pinned for item 2)** | `source/renderer/app/actions/window-actions.ts:8` `closeWindow: Action<{}>` → `WindowStore.ts:11,15-18` listens & `ipcRenderer.send('close-window')` → `source/main/windows/main.ts:79-82` `ipcMain.on('close-window', …) → window.close()` | CONFIRMED — reuse via `actions.window.closeWindow.trigger()` |
| `actions.window` exists | `source/renderer/app/actions/index.ts:21,44,67` — `window: new WindowActions()`; `Action.trigger` is bound (`actions/lib/Action.ts`) and takes an optional param | CONFIRMED — safe to wire |
| Overlay container wiring | `App.tsx:97-123` — `<MithrilPartialSyncOverlay … />`; `actions` is in scope (`App.tsx:42`); `onDismissCompleted={mithrilPartialSync.dismissCompletedOverlay}` (`:118-120`) | CONFIRMED — add `onQuit` here |
| PopOver tooltip pattern | `SidebarCategory.tsx:6,39-49` — `import { PopOver } from 'react-polymorph/lib/components/PopOver'`; `<PopOver content={…} …><button…/></PopOver>` | CONFIRMED — mechanism for item 3 |
| Store spec channel mock | `MithrilPartialSyncStore.spec.ts:16-38` — mocks the channel module; **no finalize entry**; cancel test asserts `mockStatusRequest` called **1×** (`:228`) | CONFIRMED — both must be updated |
| Overlay spec defaults | `MithrilPartialSyncOverlay.spec.tsx:22-48` — `renderComponent` passes all props incl. the three booleans `false`; mocks `react-polymorph/lib/components/Link` (`:14-19`) | CONFIRMED — add `onQuit` default + PopOver mock |

---

## Locked invariants this change MUST NOT break (inline)

- **#5 — render recovery actions strictly from `allowedRecoveryActions`.** The non-empty recovery array logic is unchanged. The defensive **Quit is rendered ONLY when that array is empty** (`!canRetry && !canRestartNormally && !canWipeAndFullSync`), never inferred from a status name, never mixed in alongside a real recovery action.
- **#6 / #9 — cancellation forbidden after cutover.** Item 1 *hides* Cancel for `installing`/`finalizing`/`starting-node`. The backend's post-cutover cancel hard-rejection is NOT touched and must remain; the renderer simply stops offering the button. Do NOT add any post-cutover dismiss.
- **#16 as amended by D9 — success overlay stays until explicit dismiss.** Item 5 keeps the `isCompletedOverlayDismissed` flag flip (the only thing that drops the success screen) and only *adds* the finalize request after it. The backend reset-to-idle happens via the finalize channel on dismiss, not at verified success — so the success screen is never pulled out from under the user.
- **#11 — do not regress the empty-chain bootstrap.** `MithrilErrorView` is shared. Item 4 makes `onWipeRetry`/`onDecline` **optional** (does not delete them); `MithrilBootstrap.tsx:205-213` still passes them and still reaches the default fallback (`MithrilErrorView.tsx:97-108`). Bootstrap is never routed through the partial-sync overlay path. Re-run `MithrilBootstrap.spec.tsx`.

---

## Implementation approach — ordered, mechanical steps

> Paths relative to repo root `/workspaces/mithril-partial-sync-ux`. Line anchors are LIVE (verified 2026-06-26). Apply items in this order.

### Step 1 — i18n: define the two new messages (`source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`)

Add two entries to the `defineMessages({...})` object (place after `partialSyncWipeAndFullSync`, ~`:426-431`), `!!!`-prefixed defaults:

```ts
  partialSyncQuit: {
    id: 'loading.mithrilPartialSync.error.quit',
    defaultMessage: '!!!Quit Daedalus',
    description:
      'Defensive Quit fallback button shown on a partial-sync failure only when no recovery actions are available (so the overlay is never an unclosable dead-end).',
  },
  partialSyncCancelStoppingTooltip: {
    id: 'loading.mithrilPartialSync.progress.cancelStoppingTooltip',
    defaultMessage: '!!!Cancellation available once the node has stopped',
    description:
      'Tooltip explaining why the Cancel button is disabled while the Cardano node is still stopping (stopping-node phase).',
  },
```

### Step 2 — Item 1: hide Cancel for all post-cutover phases (`source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`)

Replace line 123:
```tsx
              hideAction={status === 'starting-node'}
```
with:
```tsx
              hideAction={['installing', 'finalizing', 'starting-node'].includes(
                status
              )}
```
(Spellings are the exact `MithrilPartialSyncStatus` strings — `mithril-partial-sync.types.ts:10-13`. `stopping-node`/`preparing`/`downloading`/`verifying`/`converting` keep Cancel; `completed` keeps the Continue button.)

### Step 3 — Item 3 (renderer): `MithrilProgressView` disabled-Cancel + tooltip (`source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx`)

3a. Add the import after the existing react-polymorph Button imports (`:4-5`):
```ts
import { PopOver } from 'react-polymorph/lib/components/PopOver';
```
3b. Add two optional props to `interface Props` (after `hideAction?: boolean;`, `:36`):
```ts
  actionDisabled?: boolean;
  actionDisabledTooltip?: string;
```
3c. Destructure them in the function body (alongside `hideAction`, `:89`):
```ts
    actionDisabled,
    actionDisabledTooltip,
```
3d. Replace the action block (`:207-217`). Build the button once, then wrap in a `PopOver` (with a host `<span>` so the tooltip works even when the button is disabled — disabled `<button>`s do not emit hover events):
```tsx
      {!hideAction && (
        <div className={styles.actions}>
          {(() => {
            const actionButton = (
              <Button
                className={styles.secondaryAction}
                skin={ButtonSkin}
                label={actionLabel || intl.formatMessage(messages.cancel)}
                onClick={onAction}
                disabled={isStartingNode || actionDisabled}
              />
            );
            return actionDisabledTooltip ? (
              <PopOver content={actionDisabledTooltip}>
                <span>{actionButton}</span>
              </PopOver>
            ) : (
              actionButton
            );
          })()}
        </div>
      )}
```
Note `disabled={isStartingNode || actionDisabled}` keeps the bootstrap `starting-node` disable intact (lock #11) while adding the partial-sync `stopping-node` disable. Bootstrap passes neither new prop, so its behavior is unchanged.

### Step 4 — Item 3 (overlay): pass disable + tooltip during `stopping-node` (`MithrilPartialSyncOverlay.tsx`)

On `<MithrilProgressView>`, just after `hideAction=…` (Step 2), add:
```tsx
              actionDisabled={status === 'stopping-node'}
              actionDisabledTooltip={
                status === 'stopping-node'
                  ? intl.formatMessage(
                      MithrilBootstrapMessages.partialSyncCancelStoppingTooltip
                    )
                  : undefined
              }
```

### Step 5 — Item 2: defensive Quit fallback (`MithrilPartialSyncOverlay.tsx`)

5a. Add `onQuit(): void;` to the `Props` type (after `onDismissCompleted(): void;`, `:33`).
5b. Destructure `onQuit` in the function body (after `onDismissCompleted`, `:67`).
5c. Replace the inline `actions={[…]}` (`:133-172`) by first computing the recovery array, then substituting Quit when it is empty. Insert above the `return` (e.g. right after `const errorCopy = …`, `:75`):
```tsx
  const recoveryActions = [
    ...(canRetry
      ? [
          {
            label: intl.formatMessage(MithrilBootstrapMessages.partialSyncRetry),
            onClick: onRetry,
            variant: 'primary' as const,
          },
        ]
      : []),
    ...(canRestartNormally
      ? [
          {
            label: intl.formatMessage(
              MithrilBootstrapMessages.partialSyncRestartNormally
            ),
            onClick: onRestartNormally,
            variant: canRetry ? ('secondary' as const) : ('primary' as const),
          },
        ]
      : []),
    ...(canWipeAndFullSync
      ? [
          {
            label: intl.formatMessage(
              MithrilBootstrapMessages.partialSyncWipeAndFullSync
            ),
            onClick: onWipeAndFullSync,
            variant:
              canRetry || canRestartNormally
                ? ('secondary' as const)
                : ('primary' as const),
          },
        ]
      : []),
  ];
  // lock #5: recovery actions render strictly from allowedRecoveryActions.
  // Defensive Quit (D5d, gaps #8/#31) appears ONLY when none are available, so a
  // failure can never become an unclosable dead-end. `quit` is renderer-only — it
  // is NOT a backend allowedRecoveryActions value.
  const errorActions =
    recoveryActions.length > 0
      ? recoveryActions
      : [
          {
            label: intl.formatMessage(MithrilBootstrapMessages.partialSyncQuit),
            onClick: onQuit,
            variant: 'primary' as const,
          },
        ];
```
Then set the prop on `<MithrilErrorView>` to `actions={errorActions}` (replacing the inline literal at `:133-172`).
5d. Wire the seam in the container (`source/renderer/app/App.tsx`). In the `<MithrilPartialSyncOverlay>` block (`:97-123`), add a prop (e.g. after `onDismissCompleted={…}`):
```tsx
                    onQuit={() => actions.window.closeWindow.trigger()}
```
`actions` is already destructured at `App.tsx:42`. `actions.window.closeWindow.trigger` → `WindowStore.closeWindow` → `ipcRenderer.send('close-window')` → `main/windows/main.ts:79-82` `window.close()` (the existing, only renderer→main app-close seam).

### Step 6 — Item 4: remove dead props (overlay) + make them optional (error view)

6a. In `MithrilPartialSyncOverlay.tsx`, **delete** the two dead lines `:173-174`:
```tsx
              onWipeRetry={onRetry}
              onDecline={onRestartNormally}
```
(`onRetry`/`onRestartNormally` stay destructured — they are still used by the `recoveryActions` array.)
6b. In `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx`, make the two props optional (`:28-29`):
```ts
  onWipeRetry?(): void;
  onDecline?(): void;
```
Do NOT change anything else in `MithrilErrorView` — the default fallback `:97-108` (which uses them) is unchanged and bootstrap still passes them.

### Step 7 — Item 3 (store) + Item 5 (store): cancel-resync, finalize-on-dismiss, retry doc (`source/renderer/app/stores/MithrilPartialSyncStore.ts`)

7a. Add the finalize wrapper to the import block (`:15-22`), keeping it sorted:
```ts
  mithrilPartialSyncCancelChannel,
  mithrilPartialSyncFinalizeChannel,
  mithrilPartialSyncRestartNormalChannel,
```
7b. **Item 5** — make `dismissCompletedOverlay` async and invoke finalize (`:220-225`):
```ts
  @action
  dismissCompletedOverlay = async () => {
    if (this.status !== 'completed') {
      return;
    }
    // Flip the renderer dismiss flag first so the success screen hides on the
    // user's explicit "Continue" (lock #16 / D9), then tell the backend to
    // finalize: reset-to-idle + remove staging + clear the marker.
    this.isCompletedOverlayDismissed = true;
    await mithrilPartialSyncFinalizeChannel.request();
  };
```
7c. **Item 3** — always resync after a cancel (`:263-266`):
```ts
  @action
  cancelPartialSync = async () => {
    try {
      await mithrilPartialSyncCancelChannel.request();
    } finally {
      // Always resync so the UI never sticks on the optimistic frame (D5f) —
      // including the stopping-node no-op and the post-cutover rejection.
      await this.syncStatus();
    }
  };
```
7d. **#24 doc** — add a one-line comment above `startPartialSync` (`:232`) so the retry-reuse reads intentionally:
```ts
  // `retry` reuses this start path — there is no dedicated retry IPC channel
  // (PRD D8 / gap #24). onRetry in the overlay wires straight to startPartialSync.
  @action
  startPartialSync = async () => {
```

### Step 8 — i18n catalogs

8a. `yarn i18n:extract` → regenerates `translations/messages.json` with the 2 new ids.
8b. Hand-edit `source/renderer/app/i18n/locales/en-US.json` — add first-class EN (no `!!!`), in alphabetical id order among the `loading.mithrilPartialSync.*` keys:
```
"loading.mithrilPartialSync.error.quit": "Quit Daedalus",
"loading.mithrilPartialSync.progress.cancelStoppingTooltip": "Cancellation available once the node has stopped",
```
(`error.quit` sorts between `…error.noCertifiedRange.title` and `…error.restartNormally`; `progress.cancelStoppingTooltip` sorts among the `…progress.*` keys.)
8c. Hand-edit `source/renderer/app/i18n/locales/ja-JP.json` — same slots, first-class JA (no `!!!`):
```
"loading.mithrilPartialSync.error.quit": "Daedalusを終了",
"loading.mithrilPartialSync.progress.cancelStoppingTooltip": "ノードが停止した後にキャンセルできます",
```
8d. `yarn i18n:check` → regenerates `source/renderer/app/i18n/locales/defaultMessages.json` (and may sort catalogs). **Stage** the regenerated `defaultMessages.json` + `translations/messages.json` alongside `en-US.json`/`ja-JP.json` (do not hand-edit `defaultMessages.json`).

### Step 9 — Tests

9a. **Store spec** (`source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`):
- Add a finalize mock to the channel module mock (`:16-38`):
  ```ts
  const mockFinalizeRequest = jest.fn();
  // …inside the jest.mock factory object:
  mithrilPartialSyncFinalizeChannel: {
    request: (...args) => mockFinalizeRequest(...args),
  },
  ```
  (Mocking the new channel is REQUIRED — without it `dismissCompletedOverlay` calls `.request()` on `undefined` and rejects.)
- In the dismiss test (`~:187`): set `mockFinalizeRequest.mockResolvedValue(undefined)`, change `store.dismissCompletedOverlay();` to `await store.dismissCompletedOverlay();`, and assert `expect(mockFinalizeRequest).toHaveBeenCalledTimes(1)` (the `shouldShowOverlay === false` assertion still holds because the flag flips before the await).
- In the "delegates recovery and lifecycle actions" test (`~:226-229`): cancel now also resyncs, so update the final assertion `expect(mockStatusRequest).toHaveBeenCalledTimes(1)` → `toHaveBeenCalledTimes(2)` (1 from `startPartialSync`'s finally + 1 from `cancelPartialSync`'s finally).
- Add a focused test: `cancelPartialSync` calls `syncStatus()` even when the cancel request rejects — e.g. `mockCancelRequest.mockRejectedValue(new Error('post-cutover'))`, `await expect(store.cancelPartialSync()).rejects.toThrow()`, then `expect(mockStatusRequest).toHaveBeenCalled()`.

9b. **Overlay spec** (`source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`):
- Add `onQuit={jest.fn()}` to `renderComponent`'s default props (`:22-48`) — `onQuit` is now a required prop, so `tsc` would otherwise flag the spec.
- Add a `react-polymorph/lib/components/PopOver` mock next to the existing `Link` mock (`:14-19`) so the disabled-Cancel tooltip renders without a skin/theme context:
  ```tsx
  jest.mock('react-polymorph/lib/components/PopOver', () => ({
    PopOver: ({ children, content }: { children?: any; content?: any }) => (
      <>
        {children}
        <span>{content}</span>
      </>
    ),
  }));
  ```
- New test — **Cancel hidden post-cutover, shown pre-cutover** (testCase 1): for `installing`/`finalizing`/`starting-node`, `screen.queryByRole('button', { name: /cancel/i })` is null; for `downloading`/`preparing`, the Cancel button is present.
- New test — **defensive Quit only when actions empty** (testCase 2): `renderComponent({ status: 'failed', error: null })` (all three booleans default `false`) shows a `/quit daedalus/i` button and clicking it calls `onQuit`; `renderComponent({ status: 'failed', canRetry: true })` shows the retry button and **no** Quit button.
- New test — **Cancel disabled + tooltip during stopping-node** (testCase 3): `renderComponent({ status: 'stopping-node' })` → the `/cancel/i` button has `disabled` set, and `screen.getByText(/cancellation available once the node has stopped/i)` is present (from the PopOver mock's `content`).
- (testCase 4 — dead props removed — is structural; the existing recovery-action tests at `:61-87,120-148` continue to pass and guard it. testCase 5 — finalize on dismiss — is covered by the store spec 9a.)
- The existing "ships polished runtime strings without placeholder markers" test (`:196-207`) automatically extends to the two new `loading.mithrilPartialSync.*` keys (asserts no `!!!` in en-US **and** ja-JP) — re-run it as the catalog-completeness guard.

9c. **Bootstrap spec** — run `MithrilBootstrap.spec.tsx` unchanged to prove lock #11 (the optional-prop change does not regress the bootstrap fallback path).

---

## Acceptance criteria (from the tasks JSON `acceptance`)

- **Cancel is hidden for all post-cutover phases; recovery actions render strictly from `allowedRecoveryActions`.** → item 1 `hideAction` extension; item 2 leaves the non-empty recovery array untouched (lock #5).
- **A defensive Quit fallback exists only when no actions are available.** → item 2 `errorActions` substitution + `onQuit` wired to the app-close seam.
- **Cancel during `stopping-node` is disabled-with-explanation and the UI always resyncs after a cancel.** → item 3 `MithrilProgressView` PopOver + overlay `actionDisabled`/`actionDisabledTooltip`; store `cancelPartialSync` try/finally `syncStatus()`.
- **Success dismiss invokes the backend finalize channel; dead props removed; retry=start documented.** → item 5 store finalize call; item 4 dead-prop removal + optional in `MithrilErrorView`; #24 comment.

### testCases (from the tasks JSON)

- Cancel hidden for installing/finalizing/starting-node; shown pre-cutover → overlay spec 9b test 1.
- Defensive Quit renders only when `allowedRecoveryActions` is empty → overlay spec 9b test 2.
- Cancel during `stopping-node` is disabled with a tooltip; status always resyncs after a cancel → overlay spec 9b test 3 + store spec 9a (resync-on-reject).
- Dead `onWipeRetry`/`onDecline` props removed → structural (item 4); guarded by existing recovery-action + bootstrap specs.
- `dismissCompletedOverlay` invokes the finalize channel → store spec 9a (`mockFinalizeRequest` called once on dismiss).

---

## Verification plan (exact commands, from repo root)

```bash
cd /workspaces/mithril-partial-sync-ux
yarn i18n:extract            # regenerate translations/messages.json with the 2 new ids
yarn i18n:check              # validate ids exist in en-US/ja-JP; regenerates defaultMessages.json
git status --porcelain source/renderer/app/i18n/locales/defaultMessages.json   # → modified; STAGE it
grep -c 'mithrilPartialSync.error.quit\|mithrilPartialSync.progress.cancelStoppingTooltip' \
  source/renderer/app/i18n/locales/defaultMessages.json   # → 2
grep -n '!!!' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json   # → no NEW !!! for the 2 keys
node_modules/.bin/tsc --noEmit -p .   # authoritative TS gate (allow up to 600s)
yarn lint
node_modules/.bin/jest \
  source/renderer/app/stores/MithrilPartialSyncStore.spec.ts \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx \
  --config <scratchpad>/jest.scss-override.js     # overlay spec imports scss → needs the sidecar
node_modules/.bin/jest \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx \
  --config <scratchpad>/jest.scss-override.js     # lock #11: bootstrap not regressed
yarn prettier:check
```

> **KNOWN ENV WORKAROUNDS (Node v24, recorded by task-ux-301/303/304/401/402/403):**
> - `yarn compile`'s `typedef:sass` precompile hook crashes under Node v24 dart-sass. Run the authoritative TS gate directly: `node_modules/.bin/tsc --noEmit -p .`. This task touches scss-importing components (`MithrilProgressView`/`MithrilPartialSyncOverlay`) only at the TSX level — no `.scss` edits — so a `.scss.d.ts` error is the env quirk; regenerate with `node_modules/.bin/typed-scss-modules 'source/renderer/app/**/*.scss'` (or the project's `typedef:sass` equivalent) only if `tsc` complains about a stale `.scss.d.ts`, then re-run `tsc`.
> - The overlay spec **and** `MithrilBootstrap.spec.tsx` import scss, so they need the gitignored `identity-obj-proxy` jest sidecar (`--config <scratchpad>/jest.scss-override.js`, **NOT staged**) — the same workaround as 401/402/403. The store spec imports no scss and runs with the committed jest config.
> - `<scratchpad>` = `/tmp/claude-1000/-workspaces-mithril-partial-sync-ux/c19c2017-e95f-4152-b18a-d86309672ced/scratchpad`. If `jest.scss-override.js` is absent, create it: a jest config that extends the committed config and maps `\\.scss$` to `identity-obj-proxy` via `moduleNameMapper`.

---

## Risks / open questions

- **Risk — disabled-button tooltip not firing.** A disabled `<button>` swallows pointer events, so the `PopOver` is attached to a wrapping host `<span>` (Step 3d), which receives hover. Mitigation verified against the `SidebarCategory` PopOver pattern. The overlay spec mocks `PopOver` to render `content`, so the test asserts the tooltip text regardless of hover mechanics.
- **Risk — store spec false-regression.** Two existing store-spec assertions MUST be updated (the missing finalize channel mock would reject `dismissCompletedOverlay`; the cancel resync makes `mockStatusRequest` fire twice). Both are called out explicitly in Step 9a — a small model must not treat these as code bugs.
- **Risk — bootstrap regression (lock #11).** Mitigated structurally: `onWipeRetry`/`onDecline` are made optional (not deleted); bootstrap still passes them and still hits the default fallback. Re-running `MithrilBootstrap.spec.tsx` (Step 9c) confirms.
- **Risk — `onQuit` required prop breaks `tsc` on the overlay spec.** Mitigated: Step 9b adds `onQuit={jest.fn()}` to the spec defaults. App.spec mocks the overlay component, so it is unaffected.
- **Open question (Quit label wording, NON-BLOCKING).** "Quit Daedalus" maps to `window.close()` of the main window (effectively app exit). The PRD's startup native-dialog recovery (D5a) "includes Quit"; this in-session Quit uses the same verb. A holistic copy/JA pass is task-ux-601 — both readings ship correct copy, so this does not gate the task.
- **Open question (true abort-during-stop, NON-BLOCKING / out of scope).** Item 3 only *disables* Cancel during `stopping-node` + resyncs; a real cancel-flag the coordinator honors before the node-stop handler is deferred to the backend-correctness track (D7), per PRD D5f.

---

## Required doc / research updates

- **No** `.agent/system/api-endpoints.md` change — the finalize channel was already documented by task-ux-202; this task only adds the renderer call. No new IPC/contract surface (`onQuit` reuses the existing `close-window` seam; no new channel).
- At completion: fill "Final outcome", set the JSON task `status: completed` (+ `completedAt`), and record durable findings in `task-ux-404-research.md` (the pinned app-quit seam, the empty-`[]` dead-end mechanics + lock-#5-safe Quit substitution, the two mandatory store-spec assertion updates, the disabled-button PopOver-on-span pattern, and the retry=start / #24 note).

## Review-log paths

- Planning review: `task-plans-ux-refinement/phase-4/task-ux-404-plan-review.md`
- Implementation review: `task-plans-ux-refinement/phase-4/task-ux-404-impl-review.md`
- Research note: `task-plans-ux-refinement/phase-4/task-ux-404-research.md`

## Final outcome

Implemented as planned (2026-06-26), renderer-only, reusing existing seams (no backend/IPC/contract
change). **Item 1:** `MithrilPartialSyncOverlay` `hideAction` extended from `status === 'starting-node'`
to `['installing','finalizing','starting-node'].includes(status)` so Cancel is hidden for ALL
post-cutover phases (D5c / gap #9 / lock #6); pre-cutover phases keep Cancel, `completed` keeps Continue.
**Item 2:** added `onQuit(): void` to the overlay; the recovery `actions[]` is computed into a
`recoveryActions` array (logic byte-identical, still strictly from `canRetry`/`canRestartNormally`/
`canWipeAndFullSync`), and a new `errorActions` substitutes a single `partialSyncQuit` action ONLY when
`recoveryActions.length === 0` — closing the empty-`[]` dead-end (`MithrilErrorView`'s `actions || [...]`
short-circuits on a truthy empty array → zero buttons). `quit` is renderer-only (not a backend
`allowedRecoveryActions` value, never inferred from a status name); wired in `App.tsx` as
`onQuit={() => actions.window.closeWindow.trigger()}` → `WindowStore.closeWindow` →
`ipcRenderer.send('close-window')` → `main/windows/main.ts` `window.close()` (the existing, only
renderer→main app-close seam). **Item 3:** `MithrilProgressView` gained optional `actionDisabled?`/
`actionDisabledTooltip?`; the action `<Button>` is now `disabled={isStartingNode || actionDisabled}`
(bootstrap `starting-node` disable preserved) and, when a tooltip is supplied, wrapped in
`<PopOver content={...}><span>{button}</span></PopOver>` (host `<span>` so the disabled button still
surfaces the tooltip — the `SidebarCategory`/`StatusIcons` pattern). The overlay passes
`actionDisabled={status === 'stopping-node'}` + the `partialSyncCancelStoppingTooltip` text; store
`cancelPartialSync` is now `try { request } finally { await this.syncStatus() }` so the UI never sticks
on the optimistic frame (including the stopping-node no-op and the post-cutover rejection — the original
rejection still propagates after the resync). **Item 4:** dropped the dead `onWipeRetry`/`onDecline`
from the overlay's `<MithrilErrorView>` call site (it always passes an explicit `actions`), and made
those two props OPTIONAL in `MithrilErrorView` (`onWipeRetry?()`/`onDecline?()`) — NOT deleted; the
empty-chain bootstrap (`MithrilBootstrap.tsx:205-213`) still passes them and still reaches the default
fallback (lock #11). **Item 5:** store imports `mithrilPartialSyncFinalizeChannel`;
`dismissCompletedOverlay` is now `async`, early-returns unless `status === 'completed'`, flips
`isCompletedOverlayDismissed = true` FIRST (sole driver of `shouldShowOverlay`, so the success screen
stays until explicit dismiss — lock #16 / D9), then `await mithrilPartialSyncFinalizeChannel.request()`
(`.request()` to pair with the main `.onRequest` handler). A `#24` doc comment above `startPartialSync`
records that `retry` reuses the start path with no dedicated retry channel. Added the 2 new i18n keys
(`partialSyncQuit`, `partialSyncCancelStoppingTooltip`) with first-class EN+JA and regenerated
`defaultMessages.json`/`translations/messages.json`.

**Verification:** `tsc --noEmit -p .` **EXIT 0** (caught one required-prop consumer —
`MithrilPartialSyncOverlay.stories.tsx` missing `onQuit` — fixed, re-ran clean); jest over the touched
specs via the gitignored scss→identity-obj-proxy sidecar (`MithrilPartialSyncStore.spec.ts`,
`MithrilPartialSyncOverlay.spec.tsx`, `MithrilBootstrap.spec.tsx`) → **3 suites / 43 tests PASS, 0
failed** (incl. the new hidden-Cancel-post-cutover, defensive-Quit-only-when-empty,
disabled-Cancel-tooltip-during-stopping-node, finalize-called-once-on-dismiss, and
cancel-rejection-still-resyncs tests; bootstrap unchanged → lock #11 intact); eslint **0 errors**
(pre-existing style warnings only); i18n both new ids present with no `!!!` in EN/JA; prettier clean on
all touched files except the pre-existing `MithrilErrorView.tsx` discrepancy (see Deviations). Code
review (pass 1) **approved**, no blockers.

**Deviations (2, documented in task-ux-404-impl-review.md):** (1) **Storybook story update beyond the
plan's named files** — `MithrilPartialSyncOverlay.stories.tsx` is a tsc-compiled overlay consumer;
making `onQuit` required broke `tsc` (TS2741), so `onQuit: action('onQuit')` was added (mirrors the spec
default) — the minimal change to keep the TS gate green. (2) **`MithrilErrorView.tsx` pre-existing
prettier discrepancy left untouched** — `prettier --check` flags the unrelated `ERROR_COPY_BY_STAGE`
`Partial<Record<...>>` block (the local prettier-2.1.2 devDep vs the team's Nix `treefmt` form + the
`.editorconfig`-only-in-repo behaviour); proven pre-existing by restoring HEAD content at the in-repo
path (still fails). The only ErrorView change is the 2 `?` markers (lines 28-29), which are themselves
prettier-clean and in a different region; per "smallest truthful change" the committed block was NOT
reformatted. No backend/IPC/`api-endpoints.md`/`ERROR_COPY_BY_STAGE` changes.

See `task-ux-404-impl-review.md` (Implementation + Code Review entries) for full detail.
