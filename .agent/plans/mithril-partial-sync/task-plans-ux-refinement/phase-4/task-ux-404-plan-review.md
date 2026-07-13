# task-ux-404 — Plan Review Log

Append-only. Newest entries at EOF.

---

**Planner:** 2026-06-26T12:37:28Z

Authored `task-ux-404.md` to the small-model-implementable bar, covering all five renderer-only work items with line-anchored, re-verified seams. Interaction-mode classification: `autonomous` (`needsUserDecision = false`) — every item is functional safety/recovery wiring or functional copy fully governed by locked rules + PRD D5c/D5d/D5f/D8#33/D9; the two new strings (Quit label, stopping-node Cancel tooltip) are functional, with the tooltip text dictated verbatim by D5f. No blocking decision the locked rules/PRD cannot resolve.

Committed approach per item:

1. **Hide Cancel post-cutover (D5c/#9/#6).** `MithrilPartialSyncOverlay.tsx:123` `hideAction` → `['installing','finalizing','starting-node'].includes(status)`. Pre-cutover (`stopping-node`/`preparing`/`downloading`/`verifying`/`converting`) keep Cancel; `completed` keeps Continue. Backend post-cutover cancel rejection is untouched (lock #6).

2. **Defensive Quit (D5d/#8/#31).** The overlay's `actions[]` is built from the three booleans only (no `allowedRecoveryActions` prop). I compute `recoveryActions` from the existing spreads, then substitute a single Quit action ONLY when `recoveryActions.length === 0` (lock #5 — never inferred from status names, never mixed with a real recovery action). `quit` is NOT a backend `MithrilPartialSyncFailureAction` (`retry|restart-normal|wipe-and-full-sync`, `mithril-partial-sync.types.ts:17-20`), so it is renderer-only via a new `onQuit` prop.

   **EXACT app-quit seam pinned:** reuse the existing renderer→main app-close path — `App.tsx:97-123` wires `onQuit={() => actions.window.closeWindow.trigger()}` (`actions` already in scope at `App.tsx:42`; `window: new WindowActions()` at `actions/index.ts:67`; `closeWindow: Action<{}>` at `window-actions.ts:8`, `Action.trigger` is bound) → `WindowStore.ts:11,15-18` `ipcRenderer.send('close-window')` → **`source/main/windows/main.ts:79-82`** `ipcMain.on('close-window', …) → window.close()`. This is the only existing renderer-owned app-close channel; no new IPC channel is introduced (consistent with the reuse policy — the one genuinely new channel this sprint is the D9 finalize channel, already built).

3. **Disable Cancel during `stopping-node` + tooltip + resync (D5f/#39).** Add optional `actionDisabled`/`actionDisabledTooltip` props to the shared `MithrilProgressView`; render the Cancel `<Button>` once and wrap it in a react-polymorph `PopOver` (pattern from `SidebarCategory.tsx:6,39-49`) around a host `<span>` so the tooltip fires despite the disabled `<button>` swallowing hover. `disabled={isStartingNode || actionDisabled}` preserves bootstrap's `starting-node` disable (lock #11). Overlay passes both props only while `status === 'stopping-node'`. Store `cancelPartialSync` (`:263-266`) gets a try/finally that always `await this.syncStatus()` so the UI never sticks on the optimistic frame (including the stopping-node no-op and the post-cutover rejection).

4. **Remove dead props (#33).** Drop `onWipeRetry={onRetry}`/`onDecline={onRestartNormally}` from the overlay's `<MithrilErrorView>` (`:173-174`); make `onWipeRetry?`/`onDecline?` OPTIONAL in `MithrilErrorView.tsx:28-29` (do NOT delete — bootstrap `MithrilBootstrap.tsx:205-213` uses the default fallback LIVE; lock #11). `onRetry`/`onRestartNormally` remain used by the recovery array.

5. **Finalize on dismiss (D9 / lock #16 amended).** Store `dismissCompletedOverlay` (`:220-225`) becomes `async`: flip `isCompletedOverlayDismissed` first (keeps the success screen until the explicit Continue, lock #16), then `await mithrilPartialSyncFinalizeChannel.request()` (wrapper already exported `ipc/mithrilPartialSyncChannel.ts:58-61`; main handler `main/ipc/mithrilPartialSyncChannel.ts:127-129` → `controller.finalizePartialSync()`). Add the import to `MithrilPartialSyncStore.ts:15-22`. Documented #24: `retry` reuses `startPartialSync` (no dedicated channel) via a code comment above the action.

Two mandatory test-suite corrections flagged so the implementer does not read them as code bugs: the store-spec channel mock MUST add `mithrilPartialSyncFinalizeChannel` (else `dismissCompletedOverlay` rejects on `undefined.request`), and the "delegates recovery and lifecycle actions" test's `expect(mockStatusRequest).toHaveBeenCalledTimes(1)` must become `2` (cancel now resyncs). Overlay spec must add `onQuit={jest.fn()}` to defaults (now required) and a `react-polymorph/lib/components/PopOver` mock (no skin/theme context in that spec). Env: authoritative TS gate `node_modules/.bin/tsc --noEmit -p .`; overlay + bootstrap specs need the gitignored identity-obj-proxy scss sidecar via `--config`; i18n process `defineMessages !!!` → `yarn i18n:extract` → hand-edit en-US/ja-JP first-class → `yarn i18n:check` → stage `defaultMessages.json`.

Planning status: `in_review` (awaiting Critiquer). No source code modified in this phase.

---

**Critiquer:** 2026-06-26T12:41:05Z

One broad adversarial pass over `task-ux-404.md` against the tasks JSON, PRD D5c/D5d/D5f/D8#33/D9, research-19 gaps #8/#9/#24/#31/#33/#39, the grounding brief, and the LIVE working tree (every cited anchor re-verified, not trusted). No source code modified.

**Live re-verification (all anchors CONFIRMED):**
- Item 1 — `MithrilPartialSyncOverlay.tsx:123` `hideAction={status === 'starting-node'}`; status spellings match `PROGRESS_STATUSES` (`:41-51`) and the type union. Pre-cutover (`stopping-node`/`preparing`/`downloading`/`verifying`/`converting`) retain Cancel; `completed` retains Continue (`:125`). Backend post-cutover rejection untouched (lock #6/#9 preserved).
- Item 2 — recovery `actions[]` built from the three booleans `:133-172`; overlay has no `allowedRecoveryActions` prop (`:26-28`); empty-`[]` → `MithrilErrorView` `resolvedActions = actions || [...]` (`:97-108`, `[]` truthy → 0 buttons) is the real dead-end. **App-quit seam independently traced and CONFIRMED end-to-end:** `actions.window.closeWindow.trigger()` (`actions` in scope `App.tsx:43`; `window: new WindowActions()` `actions/index.ts:67`; `closeWindow: Action<{}>` `window-actions.ts:8`; `trigger` bound via `bindAll` in `actions/lib/Action.ts`) → `WindowStore.ts:11,15-18` `ipcRenderer.send('close-window')` → `source/main/windows/main.ts:79-82` `ipcMain.on('close-window', …) → window.close()`. This is a concrete, small-model-followable seam reusing the only renderer-owned app-close channel — NOT a guess. Quit substituted ONLY on `recoveryActions.length === 0`, never inferred from status names (lock #5 honored); `quit` is not a `MithrilPartialSyncFailureAction` (`mithril-partial-sync.types.ts:17-20`).
- Item 3 — `MithrilProgressView.tsx:207-215` Cancel `<Button disabled={isStartingNode}>`; `isStoppingNode` computed (`:96`) but unused for disable. `disabled={isStartingNode || actionDisabled}` keeps bootstrap's `starting-node` disable (lock #11). PopOver-on-host-`<span>` for the disabled button is a sound, in-repo pattern (react-polymorph PopOver). Store `cancelPartialSync` (`:263-266`) has no resync today; try/finally `await this.syncStatus()` is correct and `syncStatus` is `_isTornDown`-guarded (`:137-145`).
- Item 4 — `MithrilErrorView.tsx:28-29` `onWipeRetry(): void;`/`onDecline(): void;` REQUIRED today, consumed ONLY in the default fallback (`:97-108`); bootstrap `MithrilBootstrap.tsx:205-213` passes them with NO `actions` prop → LIVE. Making them OPTIONAL (not deleting) and dropping them from the overlay call site (`:173-174`) is correct and does NOT regress bootstrap (lock #11). `onRetry`/`onRestartNormally` remain used by the recovery array.
- Item 5 — store imports lack finalize (`:15-22`); `dismissCompletedOverlay` (`:220-225`) flag-only. Plan flips `isCompletedOverlayDismissed` first (keeps success visible until explicit Continue, lock #16/D9), then `await mithrilPartialSyncFinalizeChannel.request()`. Channel name + wrapper CONFIRMED (`ipc/mithrilPartialSyncChannel.ts:58-61`), main handler `.onRequest → controller.finalizePartialSync()` (`main/ipc/mithrilPartialSyncChannel.ts:127-129`) — `.request()` (not `.send()`) is the right pairing and matches store convention. `_updateStatus` resets the dismiss flag only when `status !== 'completed'` (`:178-180`), and `idle` is not an overlay status, so no re-show flicker after finalize resets to idle. #24 retry=start documented via comment.

**Gap/PRD/JSON coverage:** all five JSON `acceptance` lines and four of five `testCases` get explicit tests (overlay spec 9b 1-3, store spec 9a). The two mandatory store-spec assertion corrections (add `mithrilPartialSyncFinalizeChannel` mock; `mockStatusRequest` 1→2) and the `onQuit` required-prop / PopOver-mock spec updates are pre-flagged so they are not misread as code bugs. Env workarounds (authoritative `tsc --noEmit -p .`, identity-obj-proxy scss sidecar for the two scss-importing specs, i18n extract→hand-edit en-US/ja-JP→check→stage `defaultMessages.json`) carried over correctly. No hidden manual checkpoints; `autonomous` classification is justified (only functional safety wiring + functional copy, tooltip text dictated verbatim by D5f). Scope is the smallest truthful change and reuses existing seams (no new IPC channel, no new `allowedRecoveryActions` value, no backend change).

**Non-blocking observations (do not gate):**
1. `testCase 4` ("dead props removed") is mapped to structural/tsc coverage with no dedicated RTL assertion — acceptable since "a prop is not passed" is not directly assertable and the optional-prop change + unchanged behavior are guarded by tsc and the existing recovery/bootstrap specs.
2. `recoveryActions`/`errorActions` move to the top of the component, so they compute on progress-status renders too (previously only in the error branch) — negligible, harmless.
3. No explicit catch on a rejected `finalize` in `dismissCompletedOverlay` (the flag is already flipped, so the success screen stays dismissed) — consistent with every other store action's fire-and-await pattern and with the spec's own reject-path testing; acceptable.

These are minor and out of scope for a gate. The plan is concise, accurate, and small-model-implementable; the critical item-2 app-quit seam is pinned with real, independently-verified anchors.

Blockers: none.

Decision: approved
