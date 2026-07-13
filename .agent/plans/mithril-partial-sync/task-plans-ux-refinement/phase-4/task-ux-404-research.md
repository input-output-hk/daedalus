# task-ux-404 — Research note

Durable findings recorded at completion (2026-06-26T13:02:25Z). Renderer-only task; no
backend/IPC/contract change — every seam below already existed and was reused.

## App-quit seam used (item 2 — defensive Quit)

`quit` is NOT a backend `allowedRecoveryActions` value (the union is
`'retry' | 'restart-normal' | 'wipe-and-full-sync'`, `mithril-partial-sync.types.ts:17-20`), so the
defensive Quit is purely renderer-side and never inferred from a status name. It reuses the existing —
and only — renderer→main app-close seam, verified END-TO-END:

`App.tsx:121` `onQuit={() => actions.window.closeWindow.trigger()}`
→ `actions/window-actions.ts:8` `closeWindow: Action<{}>` (registered `actions/index.ts:44,67`; `actions`
  in scope at `App.tsx:42`)
→ `stores/WindowStore.ts:11,15-17` listens and `ipcRenderer.send('close-window')`
→ `main/windows/main.ts:79-81` `ipcMain.on('close-window', …) → window.close()`.

`Action.trigger(params?)` takes an optional param, so `trigger()` with no argument is type-safe. No new
IPC channel, no new mechanism. "Quit Daedalus" effectively exits the app via the main window close.

## Finalize-channel wiring (item 5 — dismiss → backend finalize)

The finalize channel already existed end-to-end from **task-ux-202** — task-ux-404 only added the
renderer store call:
- api const + types: `source/common/ipc/api.ts:489-492` (`MITHRIL_PARTIAL_SYNC_FINALIZE_CHANNEL`,
  request `void` / response `void`).
- main handler: `source/main/ipc/mithrilPartialSyncChannel.ts:127-129`
  `mithrilPartialSyncFinalizeChannel.onRequest(async () => { await controller.finalizePartialSync(); })`
  → `MithrilController.finalizePartialSync()` → `MithrilPartialSyncService.finalizeCompletedPartialSync()`
  (reset-to-idle + `fs.remove(stagingRoot)` + clear marker).
- renderer wrapper (pre-existing): `source/renderer/app/ipc/mithrilPartialSyncChannel.ts:58-61`
  `mithrilPartialSyncFinalizeChannel`.

**`.request()` not `.send()`** — the task-ux-202 hand-off note suggested `.send()`, but the main side
registers via `.onRequest` and every other store action uses `.request()`; `.request()` is what pairs
with `onRequest` (the `.send()` fire-and-forget variant would not). The store now imports
`mithrilPartialSyncFinalizeChannel` (`MithrilPartialSyncStore.ts:18`) and `dismissCompletedOverlay`
(`:222-231`) is `async`: it early-returns unless `status === 'completed'`, flips
`isCompletedOverlayDismissed = true` FIRST (the sole driver of `shouldShowOverlay`, so the success
screen stays up until the user's explicit "Continue" — lock #16 / D9), THEN
`await mithrilPartialSyncFinalizeChannel.request()`. Ordering matters: flip-then-finalize keeps the
success screen from being pulled out from under the user, and the backend reset-to-idle happens on
dismiss, not at verified success.

## MithrilErrorView optional-props approach (item 4 — bootstrap-preserving)

`MithrilErrorView` is SHARED with the empty-chain bootstrap path. The dead-prop removal therefore could
NOT delete `onWipeRetry`/`onDecline` — it had to make them **optional**:
- `MithrilErrorView.tsx:28-29` changed from `onWipeRetry(): void;` / `onDecline(): void;` to
  `onWipeRetry?(): void;` / `onDecline?(): void;`. They are still consumed (`:100,105`) in the default
  `resolvedActions` fallback (`:97-108`).
- The partial-sync overlay always passes an explicit `actions` prop, so the default fallback is never
  reached on that path → `onWipeRetry`/`onDecline` were genuinely dead there and were removed from the
  overlay's `<MithrilErrorView>` call site (now `actions={errorActions}` only).
- **Bootstrap still uses them LIVE:** `MithrilBootstrap.tsx:205-213` renders `<MithrilErrorView … onWipeRetry … onDecline … />` with **no `actions` prop**, so it relies on the default fallback (lock #11).
  The optional-prop change is backward compatible; `MithrilBootstrap.spec.tsx` re-run unchanged → green.

**Empty-`[]` dead-end mechanics (the bug item 2 fixes):** `MithrilErrorView`'s
`resolvedActions = actions || [default]` short-circuits on a *truthy* empty array — `[]` is truthy, so
passing `actions={[]}` renders ZERO buttons (not the fallback). The overlay therefore must detect "empty"
from `!canRetry && !canRestartNormally && !canWipeAndFullSync` (it has no `allowedRecoveryActions` prop)
and substitute a single Quit action when `recoveryActions.length === 0` (lock #5: the non-empty recovery
logic is byte-identical to the prior inline array; Quit appears only in the empty case, never mixed in).

## hideAction + stopping-node tooltip decisions (items 1 & 3)

**Item 1 (hideAction):** extended from `status === 'starting-node'` to
`['installing','finalizing','starting-node'].includes(status)` so Cancel is hidden for ALL post-cutover
phases (D5c / gap #9 / lock #6). Exact `MithrilPartialSyncStatus` spellings (hyphenated, lowercase).
Pre-cutover phases (`stopping-node`/`preparing`/`downloading`/`verifying`/`converting`) keep Cancel;
`completed` keeps the Continue button. The backend's post-cutover cancel hard-rejection is NOT touched —
the renderer just stops offering the button.

**Item 3 (disabled-Cancel-with-tooltip during `stopping-node`):**
- `MithrilProgressView` gained optional `actionDisabled?`/`actionDisabledTooltip?`. The action `<Button>`
  is `disabled={isStartingNode || actionDisabled}` — the bootstrap `starting-node` disable is preserved
  (lock #11) while adding the partial-sync `stopping-node` disable.
- **Disabled-button-tooltip pattern:** a disabled `<button>` swallows pointer events, so the `PopOver` is
  attached to a wrapping host `<span>` (`<PopOver content={…}><span>{button}</span></PopOver>`) which
  still receives hover. This mirrors the in-repo `SidebarCategory`/`StatusIcons` react-polymorph
  `PopOver` pattern (there was no pre-existing "disabled button with tooltip" helper in the
  mithril-bootstrap dir). The overlay passes `actionDisabled={status === 'stopping-node'}` + the
  `partialSyncCancelStoppingTooltip` text only during `stopping-node`.
- **Resync-after-cancel (D5f):** the renderer half only *disables* Cancel during `stopping-node`
  (clicking it during the multi-minute stop is otherwise a silent backend no-op) and always resyncs:
  `cancelPartialSync` is now `try { await …cancel.request() } finally { await this.syncStatus() }` so the
  UI never sticks on the optimistic frame — including the stopping-node no-op AND the post-cutover
  rejection. The original rejection still propagates after the resync (guarded by the new
  cancel-rejection-still-resyncs store spec asserting `rejects.toThrow('post-cutover')`). True
  abort-during-stop (a cancel flag the coordinator honors before the node-stop handler) is OUT OF SCOPE,
  deferred to the backend-correctness track (D7).

## Mandatory store-spec assertion updates (so a re-run is not mistaken for a regression)

Two existing `MithrilPartialSyncStore.spec.ts` assertions HAD to change — they are NOT code bugs:
1. The channel-module mock needed a `mithrilPartialSyncFinalizeChannel` entry; without it
   `dismissCompletedOverlay` calls `.request()` on `undefined` and rejects. The dismiss test was made
   `await store.dismissCompletedOverlay()` and asserts finalize called **1×** (the
   `shouldShowOverlay === false` assertion still holds because the flag flips before the await).
2. The lifecycle test's `mockStatusRequest` count went **1 → 2** because `cancelPartialSync` now resyncs
   (1 from `startPartialSync`'s finally + 1 from `cancelPartialSync`'s finally).

## retry = start reuse (#24, documented)

`retry` re-invokes `startPartialSync` — there is NO dedicated retry IPC channel. The overlay's `onRetry`
wires straight to `startPartialSync`. Recorded as a one-line comment above `startPartialSync` in the
store (PRD D8 / gap #24) so the reuse reads intentionally.

## Residuals (non-blocking, owned elsewhere)

- **Quit label wording / holistic copy + JA pass (task-ux-601).** "Quit Daedalus" maps to `window.close()`
  of the main window (effectively app exit); the startup native-dialog recovery (D5a) "includes Quit"
  uses the same verb. A holistic copy/JA review is task-ux-601. Both readings ship correct copy → not a
  blocker.
- **True abort-during-stop (D7 / backend-correctness track).** Item 3 only disables Cancel during
  `stopping-node` + resyncs; a real coordinator-honored cancel flag before the node-stop handler is
  deferred per PRD D5f.
- **`MithrilErrorView.tsx` prettier discrepancy (pre-existing).** `prettier --check` flags the unrelated
  `ERROR_COPY_BY_STAGE` `Partial<Record<…>>` block — the local prettier-2.1.2 devDep vs the team's Nix
  `treefmt` form + `.editorconfig`-read-only-for-in-repo-files behaviour (proven by restoring HEAD
  content at the in-repo path: still fails). The only ErrorView change here is the 2 `?` markers
  (lines 28-29), themselves prettier-clean and in a different region. Per "smallest truthful change" the
  committed block was NOT reformatted (doing so would risk disagreeing with the authoritative treefmt
  form). Documented, not silently churned.
- **Storybook story update (beyond the plan's named files).** `MithrilPartialSyncOverlay.stories.tsx` is
  a tsc-compiled overlay consumer; making `onQuit` required broke `tsc` (TS2741), so
  `onQuit: action('onQuit')` was added (mirrors the spec default) — the minimal change to keep the TS
  gate green.
