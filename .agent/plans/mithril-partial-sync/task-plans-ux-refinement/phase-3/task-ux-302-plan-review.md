# task-ux-302 — Plan Review Log (append-only)

Each entry: speaker label, ISO-8601 UTC `Timestamp:`, outcome. Critiquer/Code-Review entries end with
`Decision: approved` or `Decision: requires_changes`. Append at end-of-file only; never rewrite history.

---

## Planner:
Timestamp: 2026-06-25T13:35:00Z

### What I planned
Wrote the canonical `task-ux-302.md` to the small-model-implementable bar. The task adds the **proactive,
session-dismissible prompt** on the SyncingConnecting screen (the unbuilt half of PRD D1) that deep-links into
the **existing** Diagnostics-owned confirmation modal. It builds on the deep-link seam pre-built by
task-ux-303/304 and reuses task-ux-304's canonical renderer-computed `behindByEpochs` figure + copy framing.

### Current-state trace (already-built vs DELTA) — verified against the working tree
The Diagnostics-side threading is fully built and **unchanged** by this task:
- `MithrilPartialSyncSection.tsx:17,38-43` — `showConfirmationOnOpen?` prop + `componentDidMount` opens the
  confirmation (respecting `isActionBlocked`). BUILT.
- `DaedalusDiagnostics.tsx:414,527,731-733` — threads `showMithrilPartialSyncConfirmationOnOpen` →
  `showConfirmationOnOpen`, and `behindByEpochs={behindByEpochs}`. BUILT.

The two missing links (the DELTA):
1. **The flag source is dead-ended.** `DaedalusDiagnosticsDialog.tsx:138` HARDCODES
   `showMithrilPartialSyncConfirmationOnOpen={false}`, and `AppStore.ts:50-53`'s
   `openDaedalusDiagnosticsDialog.listen(() => …)` callback **ignores its argument** — so the payload has no
   landing spot. DELTA: add `@observable showMithrilPartialSyncConfirmationOnOpen` to AppStore, capture the
   `{ showMithrilConfirmation }` payload, reset on close (and on no-payload opens), and have the container read
   the AppStore flag instead of `false`.
2. **The prompt + session state don't exist.** DELTA: a new pure
   `SyncingConnectingMithrilPrompt.tsx`, mounted by `SyncingConnecting.tsx`, fed by `SyncingConnectingPage.tsx`;
   an in-memory `proactivePromptDismissedThisSession` store flag (+ `dismissProactivePrompt` action) mirroring
   `isCompletedOverlayDismissed`; and a container Review handler that triggers
   `openDaedalusDiagnosticsDialog.trigger({ showMithrilConfirmation: true })`.

`app-actions.ts:16` already declares `openDaedalusDiagnosticsDialog: Action<any>` — it already carries a
payload; an optional type-narrow is non-blocking.

### Key decisions / assumptions
- **CONFLICT (live code wins):** the JSON/PRD say the gate flag is `isEnabled`; the live store observable is
  **`isPartialSyncEnabled`** (`MithrilPartialSyncStore.ts:65`; the IPC payload field is `isEnabled`). The plan
  gates on `mithrilPartialSync.isPartialSyncEnabled && mithrilPartialSync.isSignificantlyBehind &&
  !proactivePromptDismissedThisSession`. Flagged explicitly so the implementer does not invent a non-existent
  `isEnabled` getter.
- **`behindByEpochs` sourced the SAME way as the modal** (task-ux-304): renderer node-tip difference
  `Math.max(1, networkTip.epoch − localTip.epoch)`, computed in `SyncingConnectingPage` from the
  already-injected `networkStatus.networkTip`/`.localTip`; `undefined` → `behindUnknown`-style fallback. No
  backend field; no renderer threshold (gate stays backend-owned).
- **Prompt = new pure presentational component** co-located in `syncing-connecting/` (repo convention:
  `ReportIssue`/`StatusIcons` siblings). `SyncingConnecting` stays pure (props in); the container does all
  gating + store access. Kill-switch hides the prompt entirely when `isPartialSyncEnabled` is false.
- **Session dismissal in-memory only**, mirroring `isCompletedOverlayDismissed`; NOT reset on status changes
  (whole-session scope per D1).
- **sync-% line OPTIONAL (DD4): recommend OMIT** from the space-constrained prompt (the confirmation modal
  already shows `(N% synced)`); component accepts `formattedSyncPercentage` if the user opts in.
- **i18n:** new `mithrilPartialSyncProactivePrompt*` keys in `en-US.json` + `ja-JP.json`; regenerate
  `translations/messages.json` via `yarn i18n:extract`. Do NOT touch `defaultMessages.json` (established
  non-target by task-ux-304). First-class EN+JA, no `!!!`. JA mirrors task-ux-304's confirmation style
  (retains untranslated "Mithril partial sync"; benefit phrasing "通常の同期を待つよりも速く追いつけます").
- **Verification:** new `SyncingConnectingMithrilPrompt.spec.tsx`; extend `MithrilPartialSyncStore.spec.ts`
  (flag default + set + not-reset-on-status); add AppStore payload coverage (`AppStore.spec.ts`: payload →
  flag true; no-payload → false; close → reset). The existing `MithrilPartialSyncSection.spec.tsx` deep-link
  tests already cover `showConfirmationOnOpen` — no change there. Commands: `yarn i18n:extract`, `yarn compile`,
  `yarn lint`, focused `yarn test:jest`, `yarn prettier:check`; documented Node-v24 dart-sass workarounds.

### Locked invariants stated inline in the plan
Confirmation precedes start (prompt never starts; Review only deep-links); no renderer threshold (gate
backend-owned; `behindByEpochs` is display-only); in-memory session dismissal; no "immutable files" to the
user; kill-switch hides (never disables) the prompt; bootstrap path not regressed.

### Open copy-approval questions for the user (interactive_decision — blocks Steps 2/8)
1. **Prompt body** — approve the recommended PRD-example
   ("Your node is about {epochs} epochs behind. Mithril partial sync can catch it up faster than waiting for
   normal sync." / "ノードは約{epochs}エポック遅れています。Mithril partial syncを使えば、通常の同期を待つよりも速く追いつけます。"),
   or the "verified chain data" alternative that mirrors the confirmation modal's wording?
2. **Button labels** — approve **Review / Not now** (確認する / 後で)?
3. **Handoff note** — approve "You can start it anytime from the Diagnostics screen."
   ("Diagnostics画面からいつでも開始できます。"), or the more-explicit alternative naming Mithril partial sync?
4. **Sync-% line** — include "({syncPercentage}% synced)" / "（{syncPercentage}% 同期済み）" in the prompt, or
   **omit** (recommended, space-constrained)?

Until these are pinned into the plan's FINAL COPY block, no catalog/`defaultMessage` edits proceed.

### Planning status
`in_review` (build status `in_progress`).


## Critiquer:
Timestamp: 2026-06-25T13:50:00Z

One broad, rigorous pass. I verified every current-state claim against the working tree. The plan is
well-anchored, faithfully implements PRD D1 + UX Flow 1, honors all locked invariants, and reuses the
existing `showConfirmationOnOpen` seam without inventing a second start path or a new IPC channel. Most of
its delta claims check out at the exact line anchors cited. The blockers below are narrow.

### Current-state verifications (all correct)
- `AppStore.ts:50-53` — `openDaedalusDiagnosticsDialog.listen(() => {...})` ignores its argument. CONFIRMED.
- `DaedalusDiagnosticsDialog.tsx:138` — `showMithrilPartialSyncConfirmationOnOpen={false}` hardcoded; `app`
  destructured at `:51`. CONFIRMED.
- `MithrilPartialSyncStore.ts:65-67` — live observables are `isPartialSyncEnabled` / `isSignificantlyBehind`
  / `behindByImmutables`, set by `_applyAvailability` (`:184-193`) from `availability.isEnabled`. The
  `isEnabled` vs `isPartialSyncEnabled` conflict is real and the plan resolves it to live code. CONFIRMED.
  `isCompletedOverlayDismissed` at `:64`, `dismissCompletedOverlay` at `:195-200`, `_updateStatus` reset at
  `:152-154` — the mirror targets are accurate.
- `DaedalusDiagnostics.tsx:570-577` — the `behindByEpochs = max(1, networkEpoch − localEpoch)` block matches
  the plan's quoted code verbatim; `formattedSyncPercentage = formattedNumber(syncPercentage, 2)` at `:569`.
  CONFIRMED. `networkTip`/`localTip`/`syncPercentage` sourced from `networkStatus` (`DaedalusDiagnosticsDialog.tsx:63,70,71`).
- `MithrilPartialSyncSection.tsx:17,38-43,59-104` — `showConfirmationOnOpen?` prop, `componentDidMount` open
  respecting `isActionBlocked`, confirmation-as-local-state, `startFromConfirmation` only on explicit call.
  Lock #3 intact. CONFIRMED.
- `SyncingConnecting.tsx` — pure `@observer` (no store injection); `Props` `:23-52`; render destructure
  `:144-171`; `<LogosDisplay>` at `:200` inside `<div className={styles.content}>` (`:183`). CONFIRMED — the
  insertion seam the plan names is exact.
- `SyncingConnectingPage.tsx` — `@inject('stores','actions')` container; destructures
  `newsFeed/appUpdate/networkStatus/profile/app` at `:18-19`; `networkStatus` block `:20-36`; status-icon
  open helper `openDaedalusDiagnosticsDialog` at `:97-102` (passes NO payload), wired via
  `onStatusIconClick` at `:68`. `mithrilPartialSync`, `networkTip`, `localTip`, `syncPercentage` are NOT
  yet destructured — the plan correctly says ADD them. CONFIRMED.
- `app-actions.ts:16` — `openDaedalusDiagnosticsDialog: Action<any> = new Action();` already payload-capable.
  CONFIRMED.
- `stores/index.ts:41,65,119` — `mithrilPartialSync` store registered, readable via
  `this.props.stores.mithrilPartialSync`. CONFIRMED.
- i18n: `package.json:52` `i18n:extract` → `translations/messages.json`. The proposed
  `mithrilPartialSyncProactivePrompt*` keys sort alphabetically AFTER `…Confirmation*` (en-US.json:160-171)
  and BEFORE `…Recommendation` (`:172`) — the plan's stated placement is correct ('P' < 'R'). Existing
  `…ConfirmationBehindUnknown` (`:162`) = "Your node is behind the latest verified snapshot." — the plan's
  NEW `promptBodyUnknown` is a benefit-extended superset, fine as a distinct key. CONFIRMED.
- Reference patterns: `hasBehindFigure` at `MithrilPartialSyncConfirmation.tsx:114-115`; `contextTypes`
  at `MithrilPartialSyncRecommendation.tsx:50-52`. CONFIRMED.
- `DIALOGS.DAEDALUS_DIAGNOSTICS = 'DAEDALUS_DIAGNOSTICS_DIALOG'` (`common/ipc/constants.ts:5`) — the plan's
  AppStore-spec assertion value is correct.
- `MithrilPartialSyncSection.spec.tsx:193-229` already proves deep-link open-on-mount + isActionBlocked
  respect — plan's "no change needed there" is correct.

### Blocker list

1. **[non-blocker] AppStore spec construction is under-specified for a small model.** No `AppStore.spec.ts`
   exists today. Unlike `MithrilPartialSyncStore.spec.ts` (which uses a `jest.fn()` `ActionsMap` because it
   never `.trigger()`s an action), the proposed AppStore tests must actually `openDaedalusDiagnosticsDialog
   .trigger({...})` to exercise the `.listen` capture — that requires a REAL `ActionsMap` instance (so
   `.listen`/`.trigger` wire up) and `setup()` to be called explicitly (the base `Store` constructor does
   NOT call `setup()`; `initialize()` does — `lib/Store.ts:30-34`). `setup()` then fires `_getGpuStatus()`
   (`getGPUStatusChannel.request()`) and `showUiPartChannel.onReceive`/`toggleUiPartChannel.onReceive`, all
   of which need mocking. *Fix:* in Step 9 / Tests, spell out the AppStore harness: construct
   `new AppStore(api, new ActionsMap-equivalent, noopAnalyticsTracker)` using the real actions map (import
   the actions singleton or build it like the app does), `jest.mock` `../ipc/get-gpu-status.ipc`,
   `../ipc/control-ui-parts`, `../ipc/open-external-url`, `../ipc/show-file-dialog-channels`, call
   `store.setup()` (or `initialize()`), then `actions.app.openDaedalusDiagnosticsDialog.trigger({...})` and
   assert. If that harness is judged too heavy, the cheaper truthful alternative is to extract the capture
   into the `@action _handleOpenDaedalusDiagnosticsDialog` (the plan already proposes this method, DD5/Step
   5b) and unit-test that method directly on a constructed store WITHOUT `setup()` — call
   `store._handleOpenDaedalusDiagnosticsDialog({ showMithrilConfirmation: true })` and assert
   `showMithrilPartialSyncConfirmationOnOpen === true` and `activeDialog === 'DAEDALUS_DIAGNOSTICS_DIALOG'`,
   then `store._closeActiveDialog()` resets it. Pick ONE and pin it so the implementer makes no judgment.

2. **[non-blocker] `_handleOpenDaedalusDiagnosticsDialog` decorated-method swap is slightly more churn than
   needed; the existing `@ts-ignore` must be preserved.** DD5/Step 5b replaces the inline `:50-53` listener
   with a new `@action`-decorated arrow handler passed to `.listen`. This is fine and matches the store's
   decorated-handler convention, but (a) the existing `// @ts-ignore ts-migrate(2345) FIXME` on the
   `_updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS)` call (`:51`) MUST be carried onto the new method or
   `yarn compile` will fail on the same string-vs-`ApplicationDialog` type mismatch every sibling open-call
   suppresses — the plan shows the comment in the snippet but does not call it out as load-bearing; *fix:*
   add an explicit "retain the `@ts-ignore` exactly" note. (b) A leaner alternative that keeps the diff to
   two lines is to leave the inline `.listen((params) => {...})` arrow and just `@action`-wrap by assigning
   inside — but MobX requires the mutation to run in an action; the decorated-method approach is the correct
   minimal one. No change to the chosen approach required beyond the `@ts-ignore` callout.

3. **[non-blocker] DD5 / Step 5d "reset in the `showUiPart` DAEDALUS_DIAGNOSTICS branch" is good, but verify
   the anchor.** The plan cites `:128-136` for the system-menu `DAEDALUS_DIAGNOSTICS` branch; live code shows
   that `case` at `AppStore.ts:128-136` calling `_updateActiveDialog(DIALOGS.DAEDALUS_DIAGNOSTICS)` with an
   analytics event — anchor CONFIRMED. The defensive reset there is correct (a system-menu open must not
   inherit a stale `true`). Keep it; it is the difference between "resets on close only" and "never spuriously
   auto-opens." Non-blocking because close-reset (5c) already covers the common path, but recommended.

4. **[non-blocker] Copy: the FINAL COPY block is unresolved and is the single interactive_decision gate —
   correctly isolated.** The `interactive_decision` stop is cleanly fenced: only Steps 2 (component
   `defaultMessage`) and 8 (catalogs) depend on FINAL COPY; Steps 1, 3, 4, 5, 6, 7 (store flag, prop
   threading, container gating/handler, AppStore capture, container flag read, optional type-narrow) are
   copy-independent and can proceed autonomously. The four copy questions (body / buttons / handoff note /
   sync-% include-omit) are well-formed with first-class EN+JA options and a clear OMIT recommendation for
   sync-% (DD4). No hidden second checkpoint. **This blocker DOES touch user-approval copy** — it is the only
   one that does. *Fix:* none needed to the plan; the orchestrator must pin FINAL COPY before Step 2/8.

5. **[non-blocker] Minor: confirm the prompt's `promptBodyUnknown` does not collide with the existing
   `…ConfirmationBehindUnknown` key.** The plan's DD6 id is
   `…mithrilPartialSyncProactivePromptBodyUnknown` — distinct from `…ConfirmationBehindUnknown` (en-US.json
   :162). No collision. CONFIRMED; noted only so the implementer does not "reuse" the confirmation key by
   mistake — the plan already specifies new keys, so this is just a guard.

### Cross-checks (clean — no blocker)
- **PRD/gap coverage:** D1's proactive half + deep-link handoff is fully implemented — gate
  (`isPartialSyncEnabled && isSignificantlyBehind && !proactivePromptDismissedThisSession`), Review deep-link
  via `openDaedalusDiagnosticsDialog.trigger({ showMithrilConfirmation: true })` → AppStore flag →
  `showConfirmationOnOpen`, in-memory session dismissal, handoff copy pointing to Diagnostics. Matches PRD
  `:703-714` and Functional `:741-752`.
- **Locked invariants:** confirmation precedes start (prompt never calls `startPartialSync`; Review only
  deep-links); no renderer THRESHOLD (gate booleans backend-owned; `behindByEpochs` is display-only);
  kill-switch hides the prompt entirely (unmounted, not disabled); no 'immutable files' language
  (`behindByImmutables` never rendered); dismissal in-memory only, not reset on status change. All honored.
- **Convergence/simplicity:** smallest truthful change — reuses the existing seam, no new IPC channel, no
  second start path, one new pure component + one store flag + one AppStore observable. No over-engineering.
  The optional `app-actions.ts` type-narrow (DD5/Step 7) is correctly marked non-blocking polish.
- **Scope creep:** Storybook explicitly deferred to task-ux-502 (Non-goals + Step 2c "functional card is the
  bar"); cross-cutting tests are this task's own focused specs only; the i18n diagnostics-row label is
  deferred to task-ux-501. No absorption of out-of-scope work.
- **Docs:** correctly states no PRD edits needed; records the trace + conflict resolution in
  `task-ux-302-research.md`. Verification command set matches the repo (`yarn i18n:extract`, `tsc --noEmit`,
  `yarn lint`, focused `yarn test:jest`, `yarn prettier:check`) with the documented Node-v24 dart-sass
  workaround.

All five items are non-blockers; none requires re-planning. Items 1 and 2(a) are the only ones that could
cause an implementer to stumble (test harness ambiguity; a dropped `@ts-ignore`), and both have a concrete,
pinned fix above. The copy gate (item 4) is the single user-approval dependency and is cleanly isolated.

Decision: approved

---

## Planner:
Timestamp: 2026-06-25T14:05:00Z

### What changed (REVISION — flow + copy + scope redesign per the user's review)
The user reviewed the original deep-link plan and **materially redesigned the flow**. I revised `task-ux-302.md`
in place. All previously verified current-state findings are RETAINED (re-verified); only the FLOW, COPY, and
SCOPE changed. Recorded as PRD amendment **D13** (orchestrator-owned).

**New flow (D13):** the prompt is a self-contained **fork + concise inline confirm + DIRECT start**, NOT a
Diagnostics deep-link:
- Choice view: epochs body + primary **"Mithril Sync (fast)"** (→ concise confirm view, does NOT start) /
  secondary **"Standard Sync (slow)"** (→ session-dismiss). Handoff note tells the user they can start later
  from Diagnostics.
- Confirm view (on the syncing screen, NOT the full `MithrilPartialSyncConfirmation` modal): concise body +
  **"Start now"** (the ONLY start path) / **"Cancel"** (→ back to choice).
- "Start now" reuses the existing **`mithrilPartialSync.startPartialSync`** store action (no new channel).

### Verified start-action seam (the load-bearing new finding)
`MithrilPartialSyncStore.startPartialSync` (`:202-231`, `@action async`, sets `START_PENDING_STATUS`, awaits
`mithrilPartialSyncStartChannel.request()`, re-throws `toStartError` on failure) — the SAME action
`DaedalusDiagnosticsDialog.tsx:142` passes as `onStartMithrilPartialSync` and that `App.tsx:115` uses for retry.
`SyncingConnectingPage` binds it as the prompt's `onStart`; the confirm view `await`s it and shows a start-error
line on reject (mirroring `MithrilPartialSyncSection.startFromConfirmation` `:83-104`). No new start path.

### Dropped from the original plan (per D13)
The AppStore `{ showMithrilConfirmation }` payload capture, the `DaedalusDiagnosticsDialog.tsx` `false`→flag
change, the `openDaedalusDiagnosticsDialog.trigger({...})` payload, the `app-actions.ts` type-narrow, and all
`showConfirmationOnOpen` deep-link wiring are **DROPPED**. The prompt no longer touches Diagnostics. The
already-built `showConfirmationOnOpen` seam (`MithrilPartialSyncSection.tsx:17,38-43`;
`DaedalusDiagnostics.tsx:414,527,731-733`) is **left in place, unused** (fed `false`) — benign residual, flagged
for a later cleanup/task-ux-601, not this task's concern. Files-to-change shrank to:
`MithrilPartialSyncStore.ts`, NEW `SyncingConnectingMithrilPrompt.tsx` (+ optional `.scss`), `SyncingConnecting.tsx`,
`SyncingConnectingPage.tsx`, `en-US.json`, `ja-JP.json`, `translations/messages.json`, + specs. (DROPPED:
`AppStore.ts`, `DaedalusDiagnosticsDialog.tsx`, `app-actions.ts`, `DaedalusDiagnostics.tsx`,
`MithrilPartialSyncSection.tsx`.)

### Terminology + figure decisions (D13)
- User-facing term is **"Mithril" / "Mithril Sync" / "standard sync"**, never "Mithril partial sync". The
  sprint-wide rollout of this term to the modal/recommendation/overlay is **deferred to task-ux-601**.
- **Epochs-only, NO sync-%** in the prompt (dropped `…SyncContext` key). `behindByEpochs` still the renderer
  node-tip `max(1, networkTip.epoch − localTip.epoch)` computed in `SyncingConnectingPage` (mirror
  `DaedalusDiagnostics.tsx:570-577`); keep an epochs-unknown fallback body.
- The `isEnabled`-vs-`isPartialSyncEnabled` conflict resolution is unchanged: gate on the live observable
  `isPartialSyncEnabled`.

### FINAL COPY (RESOLVED — approved EN verbatim; first-class JA)
New i18n family `daedalus.diagnostics.dialog.mithrilProactivePrompt*` (9 keys):
- `promptBody` EN "Your node is about {epochs} epochs behind." / JA "ノードは約{epochs}エポック遅れています。"
- `promptBodyUnknown` EN "Your node is behind the blockchain tip." / JA "ノードはブロックチェーンの先端より遅れています。"
- `promptBodyBenefit` EN "Mithril can catch you up faster than the standard sync." / JA "Mithrilを使えば、標準同期よりも速く追いつけます。"
- `promptHandoffNote` EN "If skipped, you can still start the Mithril sync from the Diagnostics screen." / JA "スキップした場合でも、Diagnostics画面からMithril syncを開始できます。"
- `promptMithrilButton` EN "Mithril Sync (fast)" / JA "Mithril Sync（高速）"
- `promptStandardButton` EN "Standard Sync (slow)" / JA "標準同期（低速）"
- `promptConfirmBody` EN "Mithril will stop your Cardano node, restore verified chain data, and restart it — so you catch up faster." / JA "Mithrilはお使いのCardanoノードを停止し、検証済みのチェーンデータを復元してから再起動します。これにより、より速く追いつけます。"
- `promptConfirmStart` EN "Start now" / JA "今すぐ開始"
- `promptConfirmCancel` EN "Cancel" / JA "キャンセル"
Locked "verified … chain data … catch up faster" wording preserved in the confirm body.

### Locked invariants (restated inline in the plan)
Confirmation-precedes-start PRESERVED via the concise confirm ("Start now" is the only start call site; the
fast button only opens the confirm view); no second/unconfirmed backend start channel; no renderer threshold;
kill-switch hides (never disables); in-memory session dismissal; no "immutable files" AND no "Mithril partial
sync" term to the user; bootstrap not regressed.

### Critiquer refinements folded in
- The `// @ts-ignore ts-migrate(2345)` note no longer applies — the AppStore `_updateActiveDialog` edits are
  DROPPED (no AppStore change in this task).
- Store unit tests prefer testing the `@action` directly (no full `setup()` harness): the test surface is now
  the prompt component (choice/confirm two-state, fast-button-does-not-start, dismiss, start-on-confirm,
  cancel, start-error, late-reject-after-unmount) + the store session flag (default, set, not-reset-on-status).
  No AppStore/Diagnostics spec changes.

### Verification (revised)
`yarn i18n:extract`; `grep` for `!!!` / `immutable` / no "partial sync"/"部分同期" in new keys; `yarn compile`;
`yarn lint`; `yarn test:jest` on `SyncingConnectingMithrilPrompt.spec.tsx` + `MithrilPartialSyncStore.spec.ts`;
`yarn prettier:check`. Documented Node-v24 dart-sass workarounds retained.

### Interaction mode
`interactive_decision` — **RESOLVED** (flow + EN copy approved 2026-06-25, D13). No open copy-approval
questions remain. FINAL COPY is pinned in the plan.

### Planning status
`in_review` (build status `in_progress`).

---

## Critiquer:
Timestamp: 2026-06-25T14:20:00Z

Focused re-critique of the D13 redesign (fork prompt + concise inline confirm + DIRECT start; deep-link and all
its AppStore/Diagnostics/app-actions wiring DROPPED; epochs-only; user-facing "Mithril"). I re-verified the new
load-bearing claims against the working tree. The redesign is a genuine simplification: it touches FEWER files
(no AppStore, no DaedalusDiagnosticsDialog, no app-actions, no DaedalusDiagnostics, no MithrilPartialSyncSection),
reuses the existing `startPartialSync` store action with no new channel, and preserves confirmation-precedes-start
via the inline confirm view. My prior blockers #1–#3 (AppStore spec harness, `@ts-ignore`, system-menu reset) are
**dissolved** — those edits no longer exist. Prior #4 (copy gate) is **resolved** — FINAL COPY is pinned (D13).

### New current-state verifications (D13 load-bearing claims — all correct)
- **Start-action seam.** `MithrilPartialSyncStore.startPartialSync` at `:202-231` — `@action async`, sets
  `START_PENDING_STATUS` (`'stopping-node'`, `:33,205`), `await mithrilPartialSyncStartChannel.request()` (`:215`),
  re-throws `toStartError(startError)` on failure when status is still pending (`:226-230`). CONFIRMED. The start
  channel `mithrilPartialSyncStartChannel` is defined at `ipc/mithrilPartialSyncChannel.ts:28`. CONFIRMED.
- **Same action used elsewhere.** `DaedalusDiagnosticsDialog.tsx:142` `onStartMithrilPartialSync={mithrilPartialSync.startPartialSync}`
  and `App.tsx:115` `onRetry={mithrilPartialSync.startPartialSync}`. CONFIRMED — it is the canonical shared start
  entry. A repo-wide grep shows the ONLY current `startPartialSync` call sites are those two (both via the store
  action; no direct channel calls). The prompt becomes a third caller via the container handler — consistent.
- **Confirm-view error/late-reject reference pattern.** `MithrilPartialSyncSection.tsx:36` (`_isMounted`),
  `:38-44`/`:45-47` (set in mount/unmount), `:83-104` (`startFromConfirmation` awaits `onStart`, guards
  `_isMounted` before setState, shows `startError`). CONFIRMED — the plan's mirror is exact. The spec patterns the
  plan tells the implementer to mirror exist: `MithrilPartialSyncSection.spec.tsx` "does not update state when
  start resolves after unmount" (`:84-124`) and "keeps confirmation open and shows concrete start failure"
  (`:126-152`). CONFIRMED.
- **Gate flags + figure source** (re-confirmed from pass 1): `isPartialSyncEnabled`/`isSignificantlyBehind`
  (`MithrilPartialSyncStore.ts:65-66`, fed by `availability.isEnabled` at `:190`); `behindByEpochs` node-tip block
  (`DaedalusDiagnostics.tsx:570-577`); `SyncingConnecting` pure-component injection model; `<LogosDisplay>` mount
  seam at `:200`; `SyncingConnectingPage` destructure at `:18-36`. All unchanged and accurate.
- **No collision:** `mithrilProactivePrompt` / `SyncingConnectingMithrilPrompt` do not exist anywhere in the tree
  today. CONFIRMED — the nine new keys + new component are net-new.

### Blocker list

1. **[non-blocker] Confirmation-precedes-start (lock #1) holds, with ONE thing the implementer must not get
   wrong.** Verified the design has exactly one `onStart` call site (confirm-view "Start now" → `handleStart` →
   `await this.props.onStart()`, DD2/DD5/Step 2e). The choice-view "Mithril Sync (fast)" only does
   `setState({ view: 'confirm' })`, and there is no mount-time start. The lock is preserved. *The single
   load-bearing instruction for the implementer:* the fast button's `onClick` must NOT be wired to `onStart` — it
   must be wired to the local view transition only. The plan states this explicitly (Non-goals, DD2, DD5, lock #1)
   and the spec "Fast button does NOT start" test pins it. *Fix:* none required — flagged as the one place a
   careless edit could break the invariant; the test catches it. (Severity non-blocker because the plan + spec
   already guard it.)

2. **[non-blocker] Prompt self-unmount-on-success relies on an assumption that is true today but only loosely
   pinned.** DD2's note and the Risks section assert that after `startPartialSync` sets `START_PENDING_STATUS` the
   syncing screen "yields to the partial-sync overlay flow" so the prompt "naturally unmounts." Note the GATE
   itself does NOT go false on `stopping-node` — `isSignificantlyBehind` stays true and the prompt is not dismissed
   — so the prompt's removal depends on the LoadingPage/App routing swapping the syncing screen for the
   partial-sync overlay once status is a working status (that routing is real, so the assumption holds). The
   actual safety net is `isStarting` disabling "Start now" (DD2), which is correctly specified. *Fix (optional,
   recommended):* add one sentence to DD2/Risks confirming that even if the prompt lingers a beat, "Start now" is
   disabled (`isStarting`) so no double-start is possible — so worst case is a transient visual, not a correctness
   bug. Non-blocking; the `isStarting` guard already prevents the only real hazard (double start).

3. **[non-blocker] i18n placement note is directionally right but imprecise.** Step 5b says the new
   `…mithrilProactivePrompt*` family "sorts just after `…mithrilPartialSync*`". Strict lexicographic order: within
   the shared `mithrilP` prefix, `mithrilPa`rtialSync < `mithrilPr`oactivePrompt (`'a'` < `'r'`), so the new family
   lands AFTER the entire `mithrilPartialSync*` block (after en-US.json `:173`
   `…RecommendationWithProgress`), not interleaved. The plan's wording is correct in direction; just place the
   nine keys as a contiguous block after line 173 (and the matching position in ja-JP.json). *Fix:* cosmetic
   catalog hygiene — key order is not enforced by `i18n:extract`/`i18n:check`, not a build gate.

4. **[non-blocker] Confirm-body copy honors the locks — verified clean.** FINAL COPY `promptConfirmBody` =
   "Mithril will stop your Cardano node, restore verified chain data, and restart it — so you catch up faster."
   User-approved EN (D13); preserves the locked "verified … chain data … catch up faster" phrasing; avoids
   "immutable files" and "partial sync" (lock #4/#6). The verification plan's grep guard (`partial sync` /
   `部分同期` / `immutable` over the new keys, Step 5d) backstops it. No issue.

5. **[non-blocker] Residual unused deep-link seam — acceptable for this task; ensure cleanup is tracked.** The
   plan leaves `showConfirmationOnOpen` + the `DaedalusDiagnostics`/`Section` threading in the tree, fed the
   literal `false` (`DaedalusDiagnosticsDialog.tsx:138`). This compiles, is dead but harmless, and its existing
   tests (`MithrilPartialSyncSection.spec.tsx:193-229`) stay green. Removal is deferred to task-ux-601. *Fix:*
   none for task-ux-302 — but the orchestrator should ensure task-ux-601 (or a named cleanup task) actually
   carries the seam removal so it does not become permanent dead code. Within this task's scope, leaving it is the
   correct minimal choice (removing it now would be out-of-scope churn touching three more files).

### Cross-checks (clean — no blocker)
- **Lock #1 (confirmation precedes start):** PRESERVED via the inline confirm view — single `onStart` call site;
  fast button only transitions view; no mount-time start. Verified against the design + the dedicated spec.
- **No second/unconfirmed backend start channel:** reuses `mithrilPartialSync.startPartialSync` →
  `mithrilPartialSyncStartChannel` (the same path Diagnostics + retry use). No new channel. CONFIRMED.
- **Gating + kill-switch:** gate = `isPartialSyncEnabled && isSignificantlyBehind && !proactivePromptDismissedThisSession`;
  when `isPartialSyncEnabled` is false the prompt is fully unmounted (not disabled). Live observable name
  `isPartialSyncEnabled` correctly used (the `isEnabled` conflict is flagged + resolved). `SyncingConnectingPage`
  can read all three off `this.props.stores.mithrilPartialSync` (store registered `stores/index.ts:41,65,119`).
- **behindByEpochs / no sync-% / no forbidden terms:** mirrors task-ux-304 node-tip
  `max(1, networkTip.epoch − localTip.epoch)` with unknown fallback (`promptBodyUnknown`); epochs-only (no
  `…SyncContext` key); no 'immutable files'; no 'partial sync' user-facing string (D13). All honored; grep guards
  in the verification plan.
- **i18n:** nine `mithrilProactivePrompt*` keys, first-class EN+JA from FINAL COPY (no `!!!` in runtime catalogs),
  `{epochs}` preserved in JA; `translations/messages.json` regenerated via `yarn i18n:extract`
  (`package.json:52`); `defaultMessages.json` untouched. No collisions.
- **Tests:** prompt two-state spec (choice/known + unknown figure, fast-button-does-not-start, dismiss,
  confirm→start, confirm→cancel, start-error, late-reject-after-unmount) + store session-flag spec
  (default/set/not-reset-on-status). Store tests correctly test the `@action`s directly (no AppStore harness now
  needed — that whole concern is gone). Commands correct for the repo (`yarn i18n:extract`, `yarn compile` /
  `tsc --noEmit`, `yarn lint`, focused `yarn test:jest` on the two spec paths, `yarn prettier:check`) with the
  documented Node-v24 dart-sass workaround.
- **Convergence/scope:** strictly smaller than the approved deep-link plan — fewer files, no new IPC, no new
  channel, one new component + one store flag. Sprint-wide "Mithril" terminology rollout and the modal sync-%
  removal are deferred to task-ux-601; Storybook to task-ux-502; cross-cutting tests to task-ux-503. No scope
  creep. The dropped deep-link work is genuinely unneeded for the new flow (the prompt no longer opens
  Diagnostics), and the residual unused seam is a benign, compiling no-op.
- **Small-model implementability:** every step names exact files, symbols, the nine key ids + EN/JA values
  (verbatim FINAL COPY), pinned seams (insertion after `<LogosDisplay>` `:200`; reuse `startPartialSync`
  `:202-231`; mirror `_isMounted`/error pattern `:36,45-47,83-104`), and exact tests/commands. The
  interactive_decision copy stop is CLOSED (FINAL COPY pinned) — no remaining manual checkpoint blocks autonomous
  implementation.

### Build-stopper assessment
None of the five items is a build-stopper. The redesign removes the two items from my first pass that could have
tripped an implementer (the AppStore spec harness and the `@ts-ignore`), and closes the copy gate. Items 1 and 2
are correctness-adjacent but both are already guarded (the fast-button-does-not-start spec; the `isStarting`
disable). Items 3–5 are hygiene/tracking notes. The plan is mechanical, fully anchored, and ready.

Decision: approved
