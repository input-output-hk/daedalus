# task-ux-302 — Implementation / Code-Review log (append-only)

## Implementation:

Timestamp: 2026-06-25T14:50:00Z

### Summary of changes

Built the D13 proactive fork prompt on the syncing screen: a self-contained two-state
(`choice` / `confirm`) component gated by the container on
`isPartialSyncEnabled && isSignificantlyBehind && !proactivePromptDismissedThisSession`.
The choice view shows the epochs body (or unknown fallback) + benefit sentence + handoff note,
with **"Mithril Sync (fast)"** (primary, emphasized) and **"Standard Sync (slow)"** (secondary).
The fast button only reveals the concise confirm view (never starts). The confirm view's
**"Start now"** is the single call site of the reused store action
`mithrilPartialSync.startPartialSync` (no new channel); **"Cancel"** returns to the choice view.
**"Standard Sync (slow)"** calls the new in-memory `dismissProactivePrompt` session flag. Epochs-only
copy (no sync-%), user-facing term is "Mithril" / "Mithril Sync" / "standard sync" (no "immutable
files", no "Mithril partial sync"). EN+JA i18n added first-class.

### Files changed

Created:
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` (NEW component, two-state, defineMessages, `_isMounted` guard, `isStarting` disable)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss` (NEW local layout; `.scss.d.ts` sidecar intentionally not staged — gitignored)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx` (NEW spec, 8 tests)

Modified:
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (added `@observable proactivePromptDismissedThisSession`, `@action dismissProactivePrompt`; reused `startPartialSync`; NOT reset in `_updateStatus`)
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` (extended: default-false + dismiss; session-scope persistence across `_updateStatus`)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnecting.tsx` (new Props `showMithrilPrompt`, `mithrilBehindByEpochs`, `onStartMithrilSync`, `onDismissMithrilPrompt`; import + mount inside `.content` after `LogosDisplay`)
- `source/renderer/app/containers/loading/SyncingConnectingPage.tsx` (read `mithrilPartialSync` store; destructure `networkTip`/`localTip`; compute `behindByEpochs` via the task-ux-304 block; compute `showMithrilPrompt` gate; wire `startPartialSync` / `dismissProactivePrompt`; added gate comment re: overlay handoff)
- `source/renderer/app/i18n/locales/en-US.json` (9 new `mithrilProactivePrompt*` keys, EN)
- `source/renderer/app/i18n/locales/ja-JP.json` (same 9 keys, first-class JA)
- `translations/messages.json` (regenerated via `yarn i18n:extract`; +50 lines, only the 9 new keys)

NOT touched (per plan): `defaultMessages.json` (an `i18n:check` side-effect rewrote it with unrelated
churn; reverted via `git checkout` so it stays unstaged/unchanged). No AppStore / Diagnostics /
`MithrilPartialSyncSection` / app-actions / backend edits. The unused `showConfirmationOnOpen`
deep-link seam left as-is.

### Verification results (run from /workspaces/mithril-partial-sync-ux)

- `yarn i18n:extract` — PASS (Done in 8.30s; 9 `mithrilProactivePrompt*` keys present in `translations/messages.json`).
- Terminology guards — PASS: no `!!!`, no `immutable`, no `partial sync` / `部分同期` in any new prompt key (EN+JA).
- `node_modules/.bin/tsc --noEmit` (authoritative TS gate) — PASS (exit 0, no errors). [`yarn compile` not run separately to avoid the Node v24 dart-sass `typedef:sass` precompile hook crash documented by task-ux-301/303/304; `tsc --noEmit` IS what `yarn compile` invokes.]
- `eslint` on the changed files — PASS (exit 0; only pre-existing-style warnings, no errors).
- `prettier --check` on changed files — PASS after `prettier --write` (also normalized a few pre-existing lines in the store/spec, as full-repo `prettier:check` would require).
- `yarn i18n:check` — PASS (exit 0; none of the new keys appear in its `!!!` untranslated report — the listed `!!!` entries are pre-existing unrelated keys).
- `jest` (the two new/extended specs) — PASS: 25/25 tests (8 component + 17 store). Required the documented env workaround: `.scss` mapped to `identity-obj-proxy` via a CLI-only `--moduleNameMapper`/`--transform` override (committed jest config unchanged) because `jest-css-modules-transform` can't find sass under this Node. `SyncingConnectingMithrilPrompt.tsx` coverage 100% stmts/funcs.

### Locked invariants — confirmed

1. Confirmation precedes start: the choice-view fast button only `setState({ view: 'confirm' })`; the ONLY `onStart`/`startPartialSync` call site is the confirm-view "Start now". No start on mount. (Test: "does NOT start when the fast button is clicked".)
2. No new start channel: "Start now" → reused `mithrilPartialSync.startPartialSync`.
3. No renderer threshold: gate is the backend booleans `isPartialSyncEnabled` (live observable, not `isEnabled`) + `isSignificantlyBehind`; `behindByEpochs` is display-only.
4. Kill-switch hides the prompt entirely (gate false → unmounted; never a disabled control).
5. In-memory dismissal only (mirrors `isCompletedOverlayDismissed`; not persisted; not reset by `_updateStatus`).
6. No "immutable files" and no "Mithril partial sync" reaches the user; epochs-only, no sync-%.

### Deviations / residual notes

- **`isEnabled` vs `isPartialSyncEnabled`:** gated on the live store observable `isPartialSyncEnabled` (the JSON/PRD wording `isEnabled` is only the IPC payload field). As the plan instructed.
- **`yarn compile` / `yarn test:jest` env workarounds** applied exactly as the plan's KNOWN ENV WORKAROUND prescribes (`tsc --noEmit` direct; `.scss` → `identity-obj-proxy` CLI override). Pre-existing Node v24 dart-sass defect, not a regression; committed config unchanged.
- **`defaultMessages.json`** was incidentally rewritten by `i18n:check`'s translation-runner (unrelated churn); reverted so it remains untouched per plan.
- **Prettier** reformatted a couple of pre-existing lines in `MithrilPartialSyncStore.ts` / `.spec.ts` (e.g. `as unknown` cast wrapping, an `await` line) to satisfy `prettier:check`; these are repo-standard and would be required by the full check regardless.
- The `.gitignore`, `…-prd.md`, `…-tasks.json` shown as modified in `git status` are pre-existing/orchestrator-owned, not part of this implementation.

## Code Review:

Timestamp: 2026-06-25T15:05:00Z

Reviewer: Code Reviewer (one broad pass over the working-tree diff vs the approved plan task-ux-302.md).
Scope reviewed: `SyncingConnectingMithrilPrompt.tsx`/`.scss`/`.spec.tsx` (NEW), `MithrilPartialSyncStore.ts`
(+`.spec.ts`), `SyncingConnecting.tsx`, `SyncingConnectingPage.tsx`, `en-US.json`, `ja-JP.json`,
`translations/messages.json`. The `.gitignore`/PRD `.md`/tasks `.json` were ignored per brief.

### Findings

1. **[non-blocker — confirmed correct] Lock #3 (confirmation precedes start) — HONORED.**
   `SyncingConnectingMithrilPrompt.tsx:120` is the ONLY call site of `this.props.onStart()`, reached only from
   `handleStart` (`:117`), which is wired only to the confirm-view "Start now" button
   (`:214 onClick={this.handleStart}`). The choice-view fast button (`:181 onClick={this.showConfirmation}`)
   only does `setState({ view: 'confirm', startError: null })` (`:105-107`) — it never starts.
   `componentDidMount` (`:97-99`) only sets `_isMounted` — no start on mount. The spec assertion is
   non-vacuous: `SyncingConnectingMithrilPrompt.spec.tsx:66-76` clicks "Mithril Sync (fast)", asserts the
   "Start now" button is now present AND `expect(onStart).not.toHaveBeenCalled()`. No fix needed.

2. **[non-blocker — confirmed correct] Start seam correctness — HONORED.** "Start now" `await`s the prop
   `onStart` (`:120`), which the container binds to `mithrilPartialSync.startPartialSync`
   (`SyncingConnectingPage.tsx:103`) — the existing store action (`MithrilPartialSyncStore.ts:207`), no new
   IPC channel. Start-error path present (`:126-142`, renders `state.startError` at `:198`); double-start
   guard via `disabled={isStarting}` (`:213`); `_isMounted` guard against setState-after-unmount
   (`:95-103,131-133`). Spec covers the start-error (`:114-137`) and late-reject-after-unmount
   (`:139-177`, asserts no React "unmounted component" console error). Correct.

3. **[non-blocker — confirmed correct] Gating — HONORED.** Gate at `SyncingConnectingPage.tsx:60-63` is
   `isPartialSyncEnabled && isSignificantlyBehind && !proactivePromptDismissedThisSession` — the LIVE
   observable `isPartialSyncEnabled` (not `isEnabled`), per the resolved conflict. Kill-switch off → gate
   false → `{showMithrilPrompt && (...)}` (`SyncingConnecting.tsx:210`) fully unmounts the prompt (not a
   disabled control). The session flag (`MithrilPartialSyncStore.ts:65`) is in-memory and is NOT reset by
   `_updateStatus` (`:147-160` resets only `isCompletedOverlayDismissed` on non-`completed` status); the
   `dismissProactivePrompt` action (`:203-205`) only sets it true. Correct.

4. **[non-blocker — confirmed correct] behindByEpochs — HONORED.** `SyncingConnectingPage.tsx:51-59` mirrors
   the task-ux-304 block exactly: guards `networkTip`/`localTip` with `Number.isFinite(.epoch)`, computes
   `Math.max(1, networkEpoch - localEpoch)`, and falls back to `undefined` when either tip/epoch is missing
   (renders the unknown-figure copy). No backend field, no sync-%, no per-network constant.

5. **[non-blocker — confirmed correct] Copy / i18n — HONORED.** All 9 `mithrilProactivePrompt*` keys present
   in BOTH `en-US.json` and `ja-JP.json` (9 each) with first-class JA (no `!!!` in runtime catalogs).
   No "immutable files", no "Mithril partial sync", no sync-% in any new string; user-facing terms are
   "Mithril"/"Mithril Sync"/"standard sync". `translations/messages.json` regenerated with the 9 new
   descriptors under the new component path; `defaultMessages.json` untouched (not in `git status`).
   Confirm body preserves the locked "verified … chain data … catch up faster" wording
   (`en-US.json` mithrilProactivePromptConfirmBody).

6. **[non-blocker — confirmed correct] Tests — HONORED.** `SyncingConnectingMithrilPrompt.spec.tsx` covers:
   known figure (`:42-55`), unknown fallback (`:57-64`), fast-button-does-not-start (`:66-76`), dismiss
   (`:78-87`), start-on-confirm called once (`:89-99`), cancel→choice (`:101-112`), start-error
   (`:114-137`), and late-reject-after-unmount (`:139-177`). `MithrilPartialSyncStore.spec.ts:497-525`
   covers default-false + dismiss, and session-scope persistence across a non-`completed` (`'idle'`)
   `_updateStatus` (non-vacuous — `'idle'` IS a status that resets `isCompletedOverlayDismissed`, so the
   test meaningfully distinguishes the two flags). Assertions are real and correct.

7. **[non-blocker — confirmed correct] Scope / regression — HONORED.** `git diff --name-only` shows NO
   changes to the confirmation modal, recommendation, overlay, `DaedalusDiagnostics`, `AppStore`,
   `MithrilPartialSyncSection`, or `app-actions`. The `showConfirmationOnOpen` seam is left untouched/unused.
   New `SyncingConnecting` props are additive; the prompt only renders behind `showMithrilPrompt &&`
   (`:210`), so when hidden the existing render is byte-for-byte unchanged. Bootstrap path
   (`MithrilBootstrap`) is not touched.

8. **[non-blocker — polish, not blocking] SCSS positioning.** `SyncingConnectingMithrilPrompt.scss:8-12`
   uses `position: fixed; top: 0; left: 50%; transform: translateX(-50%)` while `.component` also sets
   `margin: 24px auto 0` (the margin is inert under `position: fixed`). `top: 0` could overlap other
   syncing-screen chrome on small windows. This is explicitly visual polish deferred to task-ux-502 per the
   plan's Risks ("a functional card is the bar here … Do not block on .scss polish"). No reduced-motion or
   theme-token concern (all colors use `--theme-*` tokens; no animation). Optional fix later: drop the inert
   `margin` and reconsider `fixed`/`top:0` placement in task-ux-502. Not a blocker.

9. **[non-blocker — observation] Buttons are real buttons; accessibility OK.** Uses react-polymorph `Button`
   + `ButtonSkin` (renders a real `<button>`, proven by the spec's `getByRole('button', { name })` queries).
   `@observer` is correctly ABSENT from the prompt — it reads no observables (pure props-driven; gating lives
   in the `@observer` container `SyncingConnectingPage`). No missing React keys (no `.map`). No dead code.

### Decision: approved

All eight review axes from the brief are satisfied; the four locked invariants (confirmation-precedes-start,
no new start channel, no renderer threshold, kill-switch unmount, in-memory session dismissal, epochs-only
no-immutable copy) hold. The single non-correctness finding (#8 SCSS placement) is visual polish explicitly
owned by task-ux-502 and is not a regression or invariant break. No true blockers.
