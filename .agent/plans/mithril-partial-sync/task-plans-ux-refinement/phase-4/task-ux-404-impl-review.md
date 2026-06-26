# task-ux-404 — Implementation Review

## Implementation: 2026-06-26T12:57:35Z

Speaker: Implementer

### Summary

Implemented all five renderer-only work items from `task-ux-404.md` (overlay recovery-action safety + success-dismiss finalize wiring). Smallest-truthful-change edits to the task source files only; no backend/IPC/contract changes (the finalize channel already existed end-to-end from task-ux-202).

**Item 1 — Hide Cancel for all post-cutover phases (D5c / gap #9 / lock #6).**
`MithrilPartialSyncOverlay.tsx` `hideAction` extended from `status === 'starting-node'` to `['installing', 'finalizing', 'starting-node'].includes(status)`. Pre-cutover phases (`stopping-node`/`preparing`/`downloading`/`verifying`/`converting`) keep Cancel; `completed` keeps Continue. The backend post-cutover cancel hard-rejection is untouched.

**Item 2 — Defensive Quit fallback (D5d / gaps #8/#31 / lock #5, renderer-only).**
Added `onQuit(): void` to the overlay `Props`. The recovery `actions[]` is now computed into a `recoveryActions` array (logic unchanged: still strictly derived from `canRetry`/`canRestartNormally`/`canWipeAndFullSync`, i.e. from `allowedRecoveryActions`). A new `errorActions` substitutes a single `partialSyncQuit` action ONLY when `recoveryActions.length === 0`, closing the empty-`[]`-array dead-end (`MithrilErrorView`'s `actions || [...]` short-circuits on a truthy empty array → zero buttons). `quit` is NOT a backend `allowedRecoveryActions` value and is never inferred from a status name. Wired in `App.tsx`: `onQuit={() => actions.window.closeWindow.trigger()}` → `WindowStore.closeWindow` → `ipcRenderer.send('close-window')` → `main/windows/main.ts` `window.close()` (the existing, only renderer→main app-close seam; `Action.trigger(params?)` takes an optional param so `trigger()` is type-safe).

**Item 3 — Disable Cancel during `stopping-node` + tooltip + resync-after-cancel (D5f / gap #39).**
`MithrilProgressView.tsx`: imported react-polymorph `PopOver`; added optional props `actionDisabled?`/`actionDisabledTooltip?`; the action `<Button>` now `disabled={isStartingNode || actionDisabled}` (bootstrap `starting-node` disable preserved); when a tooltip is provided the button is wrapped in `<PopOver content={...}><span>{button}</span></PopOver>` (host `<span>` so the disabled button still surfaces the tooltip). The overlay passes `actionDisabled={status === 'stopping-node'}` and the `partialSyncCancelStoppingTooltip` text during `stopping-node`. Store `cancelPartialSync` wrapped in `try { request } finally { await this.syncStatus(); }` so the UI never sticks on the optimistic frame (including the stopping-node no-op and the post-cutover rejection — the original rejection still propagates after the resync).

**Item 4 — Remove dead `onWipeRetry`/`onDecline` from the overlay path (D8 #33).**
Dropped `onWipeRetry={onRetry}` / `onDecline={onRestartNormally}` from the overlay's `<MithrilErrorView>` call site (it always passes an explicit `actions` prop, so they were unreachable there). Made the two props OPTIONAL in `MithrilErrorView` Props (`onWipeRetry?(): void; onDecline?(): void;`) — NOT deleted. The empty-chain bootstrap (`MithrilBootstrap.tsx:205-213`) still passes them and still reaches the default `resolvedActions` fallback (lock #11 preserved). `MithrilBootstrap.spec.tsx` re-run unchanged → passes.

**Item 5 — `dismissCompletedOverlay` invokes the finalize channel (D9 dismiss half / lock #16 amended).**
Added `mithrilPartialSyncFinalizeChannel` to the store import block. `dismissCompletedOverlay` is now `async`: it early-returns unless `status === 'completed'`, flips `isCompletedOverlayDismissed = true` first (the only thing that drops the success screen, so visibility is preserved until the user's explicit "Continue"), then `await mithrilPartialSyncFinalizeChannel.request()` (backend reset-to-idle + remove staging + clear marker). `.request()` chosen to pair with the main-side `.onRequest` handler and the established store convention (not `.send()`).

**#24 doc.** Added a comment above `startPartialSync` noting `retry` reuses the start path with no dedicated retry IPC channel.

### Files changed

- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` — 2 new `defineMessages` entries (`partialSyncQuit`, `partialSyncCancelStoppingTooltip`).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` — items 1, 2, 3 (overlay half), 4 (call-site removal); `onQuit` prop.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx` — item 3 renderer half (PopOver + `actionDisabled`/`actionDisabledTooltip`).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx` — item 4 (props made optional; 2 lines).
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — items 3 (cancel resync), 5 (finalize import + async dismiss), #24 comment.
- `source/renderer/app/App.tsx` — `onQuit` wired to `actions.window.closeWindow.trigger()`.
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` — `onQuit: action('onQuit')` (required-prop consumer; tsc would otherwise fail).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` — PopOver mock + `onQuit` default; 3 new tests (hidden Cancel post-cutover/shown pre-cutover, defensive Quit only when empty, disabled-Cancel-with-tooltip during stopping-node).
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` — finalize channel mock; dismiss test made async + asserts finalize called 1×; lifecycle test status-request count 1→2 (cancel now resyncs); new cancel-rejection-still-resyncs test.
- i18n: `en-US.json`, `ja-JP.json` (first-class EN + JA for both keys, no `!!!`), `defaultMessages.json` (regenerated via `yarn i18n:check`), `translations/messages.json` (regenerated via `yarn i18n:extract`).

### Verification (exact results)

- **TS gate** `node_modules/.bin/tsc --noEmit -p .` → **exit 0** (clean). Caught one required-prop consumer (`MithrilPartialSyncOverlay.stories.tsx` missing `onQuit`), fixed, re-ran clean.
- **jest** (touched specs, via gitignored scss→identity-obj-proxy sidecar `--config <scratchpad>/jest.scss-override.js`, sidecar NOT staged): `MithrilPartialSyncStore.spec.ts`, `MithrilPartialSyncOverlay.spec.tsx`, `MithrilBootstrap.spec.tsx` → **3 suites passed, 43 tests passed, 0 failed**.
- **lint** `eslint` over the 9 touched TS/TSX files → **exit 0** (25 warnings, all pre-existing-style: unused mobx decorator imports flagged as false positives, `@ts-ignore`, `no-array-index-key`, 2 `no-explicit-any` in the new PopOver mock per the plan's mock shape; no errors).
- **prettier** `prettier --config .prettierrc --check` over all touched files → all clean EXCEPT `MithrilErrorView.tsx` (see Deviations — pre-existing, not introduced by this change). `MithrilPartialSyncOverlay.tsx` was `prettier --write`-formatted (my new code), verified clean in both editorconfig-present (real-path) and editorconfig-absent (/tmp) modes.
- **i18n** `yarn i18n:extract` + `yarn i18n:check` ran clean; both new ids present in `translations/messages.json` and `defaultMessages.json` (grep count 2); no `!!!` placeholders for the 2 new keys in `en-US.json`/`ja-JP.json`. The overlay spec's existing "ships polished runtime strings without placeholder markers" guard now covers both new keys (passes for EN and JA).
- **Bootstrap unaffected (lock #11):** `MithrilBootstrap.spec.tsx` passes unchanged; `MithrilBootstrap.tsx:205-213` still passes `onWipeRetry`/`onDecline` and still reaches `MithrilErrorView`'s default fallback. Optional-prop change is backward compatible.

### Deviations

1. **Storybook story update (necessary, beyond the plan's named files).** `MithrilPartialSyncOverlay.stories.tsx` is a tsc-compiled consumer of the overlay; making `onQuit` required broke `tsc` there (TS2741). Added `onQuit: action('onQuit')` — the minimal truthful fix to keep the TS gate green. Mirrors the spec's `onQuit` default.
2. **`MithrilErrorView.tsx` pre-existing prettier discrepancy left untouched.** `prettier --config .prettierrc --check` flags this file on the `ERROR_COPY_BY_STAGE` generic (`Partial<\n Record<...>\n>` vs prettier-2.1.2's desired `Partial<Record<\n...\n>>`). Proven pre-existing: the HEAD content at the real in-repo path fails identically (the local prettier-2.1.2 devDep disagrees with the form produced by the team's Nix `treefmt` formatter — cf. commit `47304d7d8 style: apply treefmt formatting` — and prettier reads `.editorconfig` for in-repo files, so byte-identical content is "clean" outside the repo and "dirty" inside). My only ErrorView change is the 2 `?` characters (lines 28-29), which are themselves prettier-clean and do not touch the flagged block. Per "smallest truthful change", I did NOT reformat the unrelated committed block (doing so would also risk disagreeing with the team's authoritative treefmt form). Documented rather than silently churned.
3. No backend, IPC contract, `api-endpoints.md`, or `MithrilErrorView` `ERROR_COPY_BY_STAGE`/bootstrap-fallback changes (per Non-goals / lock #11). The finalize channel, app-close seam, and PopOver pattern all reuse existing seams.

---

## Code Review: 2026-06-26T13:01:12Z

Speaker: Code Reviewer (pass 1)

### Scope reviewed
Broad pass over the working-tree diff vs `task-ux-404.md` (all five renderer-only items) and the four locked invariants (#5, #6/#9, #16/D9, #11). Re-ran every verification gate myself.

### Correctness vs plan — all five items implemented
- **Item 1 (hide Cancel post-cutover, D5c/#9/lock #6).** `MithrilPartialSyncOverlay.tsx:179-183` — `hideAction={['installing','finalizing','starting-node'].includes(status)}`. Pre-cutover phases (`stopping-node`/`preparing`/`downloading`/`verifying`/`converting`) keep Cancel; `completed` keeps Continue. Confirmed against the exact `MithrilPartialSyncStatus` spellings. Backend post-cutover cancel rejection untouched.
- **Item 2 (defensive Quit, D5d/#8/#31/lock #5).** `recoveryActions` (`:81-118`) built strictly from `canRetry`/`canRestartNormally`/`canWipeAndFullSync` (logic byte-identical to the prior inline array). `errorActions` (`:122-131`) substitutes a single `partialSyncQuit` action ONLY when `recoveryActions.length === 0` — never inferred from a status name, never mixed alongside a real recovery action. `onQuit` wired in `App.tsx:121` to `actions.window.closeWindow.trigger()`. App-quit seam verified END-TO-END: `window-actions.ts:8 closeWindow: Action<{}>` → `WindowStore.ts:11,15-17 listen → ipcRenderer.send('close-window')` → `main/windows/main.ts:79-81 ipcMain.on('close-window') → window.close()`; `actions.window` registered in `actions/index.ts:44,67`; `actions` in scope at `App.tsx:42`. Real seam, no new IPC.
- **Item 3 (disable Cancel during stopping-node + tooltip + resync, D5f/#39).** `MithrilProgressView.tsx` adds optional `actionDisabled?`/`actionDisabledTooltip?`; action `<Button disabled={isStartingNode || actionDisabled}>` (bootstrap `starting-node` disable preserved); disabled-tooltip hosted on a wrapping `<span>` inside `<PopOver>` (disabled buttons swallow hover) — matches the `SidebarCategory`/`StatusIcons` PopOver pattern. Overlay passes `actionDisabled={status==='stopping-node'}` + `partialSyncCancelStoppingTooltip`. Store `cancelPartialSync` (`:272-280`) now `try { request } finally { await this.syncStatus() }` — UI always resyncs; the original rejection still propagates (verified by the new reject-still-resyncs spec).
- **Item 4 (remove dead props, D8 #33).** Overlay no longer passes `onWipeRetry`/`onDecline` (call site is now `actions={errorActions}` only). `MithrilErrorView.tsx:28-29` made OPTIONAL (`onWipeRetry?()`/`onDecline?()`), NOT deleted; still consumed in the default fallback `:100,105`. Bootstrap (`MithrilBootstrap.tsx:210-211`) still passes them LIVE — lock #11 preserved.
- **Item 5 (finalize on dismiss, D9/lock #16).** Store imports `mithrilPartialSyncFinalizeChannel`; `dismissCompletedOverlay` (`:221-231`) now async, early-returns unless `status==='completed'`, flips `isCompletedOverlayDismissed = true` FIRST (sole driver of `shouldShowOverlay`, so visibility preserved until explicit dismiss), then `await ...finalize.request()` (pairs with main `.onRequest`). `#24` doc comment added above `startPartialSync` (`:238-239`).

### Locked-invariant check — all intact
- **#5** Recovery strictly from `allowedRecoveryActions`; Quit purely from `recoveryActions.length === 0`, never a status name. PASS.
- **#6/#9** No backend cancel change; renderer only hides/disables. Post-cutover hard-rejection intact; the new reject-still-resyncs test asserts `rejects.toThrow('post-cutover')`. PASS.
- **#16/D9** Flag flips before finalize; success screen stays until dismiss; finalize invoked on dismiss (store spec asserts 1×). PASS.
- **#11** `MithrilErrorView` props optional not deleted; bootstrap still passes + reaches the default fallback; `MithrilBootstrap.spec.tsx` green. PASS.

### Verification re-run (by reviewer)
- `node_modules/.bin/tsc --noEmit -p .` → **exit 0**.
- `eslint` (5 touched source files) → **0 errors**, 21 warnings (all pre-existing style: unused mobx decorator false-positives, `@ts-ignore`, `no-array-index-key`, the pre-existing `error as any` at overlay `:197`).
- `jest` `MithrilPartialSyncStore.spec.ts` + `MithrilPartialSyncOverlay.spec.tsx` + `MithrilBootstrap.spec.tsx` (scss sidecar) → **3 suites / 43 tests passed**.
- `prettier --check` → all touched files clean EXCEPT `MithrilErrorView.tsx`. **Confirmed pre-existing**: I restored HEAD content of that file at the real in-repo path and `prettier --check` still fails (exit 1); the working tree changed only 2 lines (the `?` markers, lines 28-29), which are prettier-clean and sit in a different region than the flagged `ERROR_COPY_BY_STAGE` block (`:41-43`). This is the treefmt-vs-prettier-2.1.2 + `.editorconfig` discrepancy already documented in Deviation 2 — NOT introduced by this task. Not a blocker.

### Other observations (non-blocking)
- `.gitignore` adds `.devcontainer` — unrelated to task-ux-404 but harmless dev-env hygiene; no functional impact.
- i18n: both new ids present in `defaultMessages.json` + `translations/messages.json`; first-class EN/JA with no `!!!`; the overlay spec's placeholder-marker guard covers both keys.
- No IPC/contract/backend drift — diff is renderer + i18n + storybook + `.gitignore` only.

### Decision
Decision: approved

Blockers: none.
