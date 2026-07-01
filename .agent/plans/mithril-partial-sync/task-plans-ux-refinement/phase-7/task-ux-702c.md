# task-ux-702c — Mithril partial-sync UX cleanup (manual-testing wave)

> Canonical task plan + single source of truth for the **extensible manual-testing cleanup wave** on the shipped
> Mithril Sync UX. Six operator manual-testing findings are remediated as categories **CAT-A..CAT-F**: CAT-A
> (retry-after-cancel race + missing cleanup state — the only correctness fix), CAT-B (overlay-backdrop bleed),
> CAT-C (first-load diagnostics availability), CAT-D/CAT-E (proactive/process prompt chrome), CAT-F (dead
> "Before Mithril Sync begins" CSS). Authoritative inputs: `task-ux-702c-decisions.md` (locked decision record
> D-702c-0..13, unknowns U1–U4), `task-ux-702b.md`/`task-ux-702b-decisions.md` (predecessor wave, whose structure
> this mirrors), `mithril-partial-sync-prd.md` (status contract :147-160, Boundary A/B/C recovery model :176-194),
> `prompt-ux-refinement.md` (process rules, locked safety boundaries). All citations verified against live code
> (2026-07-01).
>
> Review logs: `task-ux-702c-decisions.md` (the locked decision record for this plan),
> `task-ux-702c-plan-review.md`, `task-ux-702c-impl-review.md`, and `task-ux-702c-research.md`.

## Execution status — IMPLEMENTED (awaiting operator validation)

Decisions remain locked in `task-ux-702c-decisions.md`. CAT-A, CAT-B, CAT-D + CAT-E, and CAT-F are now implemented on
the shared working tree; CAT-C was re-verified as a no-code-change slice. Every code-bearing category has an approved
per-category code review, and the focused automated verification record lives in `task-ux-702c-impl-review.md`
(touched Jest suites green, compile/typecheck green, targeted lint/stylelint clean of errors, touched-file Prettier
green, no editor diagnostics on the primary touched surfaces). This remains an **extensible manual-testing cleanup
wave** (D-702c-0), but the only outstanding items are the operator verification-only gates U1–U4 and the final
phase-7 completion/commit decision.

## Task id + title
- **Id:** task-ux-702c
- **Title:** Mithril partial-sync UX cleanup (manual-testing wave)

## Interaction mode (AUTHORITATIVE)
`interactive_validation`. Decisions are recorded in the decision record; the human-in-the-loop surfaces that keep
it non-autonomous are: (1) a per-category code-review pass after each CAT lands, and (2) operator visual/behavioral
re-test of the cancel→cleanup→retry flow and the CAT-B..CAT-F surfaces on a running app (verify-only, U1–U4). Do **not**
relabel autonomous — phase-7 tasks require operator-run validation.

## Why now
`task-ux-702` (the deployment QA gate) is held `pending` behind the remediation waves (`…702a → …702b`). Operator
manual testing of the shipped UX then surfaced a **functional defect** (finding #1 / CAT-A): after cancelling a
Mithril partial sync, the "Retry Mithril Sync (fast)" button drops straight back to the "Mithril Sync was cancelled"
prompt and only works after a wait — plus a **UX gap**: no cleanup state is shown before the terminal cancelled
prompt. A second manual-testing pass added five presentation findings (#2–#6 / CAT-B..CAT-F). 702c remediates all
six before the gate can re-run. The tasks JSON gains `task-ux-702c`; `task-ux-702` depends on it (chain:
`…702a → …702b → …702c → …702`).

## Scope
**CAT-A (correctness):** remediate **finding #1** (the retry-after-cancel race + the missing cleanup state) — a
**Full** fix (D-702c-2): a new `cancelling` state that gates Retry during teardown, a coordinator-owned run-promise
join so terminal `cancelled` is emitted only after the in-flight `start()` unwinds, and a renderer defense-in-depth
log for the swallowed rejection.

**CAT-B..CAT-F (presentation/renderer/i18n):** remediate findings **#2–#6**, each fixed by reusing an existing
app/bootstrap pattern (no new colors/classes/theme vars):
- **CAT-B (D-702c-8):** add the app-wide `overlay-backrop` frosted-glass blur (+ `opacity:1`) to the single shared
  `.backdrop` rule so the overlay stops bleeding the live syncing screen; keep the tint token.
- **CAT-C (D-702c-9 — verify-only):** **no code change.** Keep the Diagnostics gate coupled to
  `isSignificantlyBehind` (the sole offer signal, boundary #4); the figure-based OR-gate is rejected as unsound. The
  first-load-availability gap is a **separate backend probe-hardening follow-up** (not this wave).
- **CAT-D (D-702c-10) + CAT-E (D-702c-11):** re-align the proactive prompt (choice view) **and** the process
  (confirm view) to the MithrilBootstrap/MithrilErrorView reference — add the "Mithril Sync" / "Mithril Sync Process"
  titles, merge + left-align the choice-view body, de-dim the "Note:" line (own element), and swap in the shared
  `.primaryAction`/`.secondaryAction` button tokens. **One coordinated edit** (D-702c-13).
- **CAT-F (D-702c-12):** un-nest the dead descendant-scoped `.mithrilPartialSyncConfirmation*` CSS to flat selectors
  (the Dialog portals to `document.body`), add real inter-paragraph spacing (`p + p { margin-top: 20px }`), lift a
  dialog error class (+ one-line TSX swap), and drop the `.Body` opacity:0.85 to the "Before Mithril Sync begins"
  dialog.

CAT-A, CAT-B, and CAT-F each land as code subagents on the shared working tree; CAT-C is a validation-only
subagent (no source edit, no implementation diff); CAT-D + CAT-E land as one merged code subagent. Every
code-bearing category is followed by a per-category code-review/compile pass, while CAT-C records unchanged
gate/spec proof only.

## Non-goals
- **No change to the Boundary A/B/C recovery model** or the cutover-forbidden-cancel rule (`prd.md:190`). Cancel
  stays Boundary-A-only; `installing`/`finalizing` still throw (`MithrilPartialSyncService.ts:293-297`).
- **Do NOT reintroduce `wipe-and-full-sync` into the pre-cutover cancelled dialogue** — D-702a-2 removed it; terminal
  `cancelled` stays `['retry','restart-normal']`.
- **No new IPC channel.** Retry keeps reusing the start path (PRD D8 / gap #24); the fix adds a `finalizeCancel`
  **handler** on the existing dependency object, not a channel.
- **Do NOT make `cancelling` a generic shared teardown state** — it is scoped to the cancel path only.
  `restart-normal`/`wipe`/`finalize` paths are untouched.
- **No backend cleanup-before-reset reorder** (carried from D-702b-4).
- **Do NOT block/serialize cancel behind the mutation lock** as the fix — the long `start()` runs outside the lock,
  so the explicit run-promise join (D-702c-4), not the lock, is the coordination mechanism.
- **CAT-A only:** no unrelated copy edits, no behind-ness/epoch changes, no proactive-prompt changes.
- **CAT-B..CAT-F:** no change to the **backend behind-ness probe / offer signal** (boundary #4 — CAT-C makes no
  renderer or backend change either; the probe-hardening is a *separate* follow-up task); **no change to the
  proactive-prompt suppression logic** (`MithrilProactivePromptContainer.tsx:67-74,88` stays coupled to
  `isSignificantlyBehind`); **no per-theme backdrop-alpha edits** (CAT-B adds blur to the one shared rule, not ~10
  theme files); **no new colors, classes, or theme vars** (every fix reuses an existing token/mixin/reference); **no
  `cancelling`/state-machine or PRD status-contract change** (CAT-B..CAT-F are presentation-only).

## Dependencies
- **Depends on `task-ux-702b`** (the prior manual-testing cleanup round).
- **Blocks `task-ux-702`** (the deployment QA gate; its `dependencies` includes `task-ux-702c`).

## Consulted docs / workflows / skills
- **Docs consulted:** `prompt-ux-refinement.md`, `.agent/readme.md`, `.agent/system/architecture.md`,
  `mithril-partial-sync-ux-refinement-tasks.json`, `task-ux-702c.md`, `task-ux-702c-decisions.md`, and the live repo
  files cited throughout this plan.
- **Workflow docs consulted in this orchestration pass:** `.agent/workflows/frontend.md`,
  `.agent/workflows/ipc.md`, `.agent/workflows/test.md`, and `.agent/workflows/update-doc.md`.
- **Skills consulted in this orchestration pass:** none.

---

## Finding → Category traceability (nothing dropped)

| # | Finding (short) | Decision | CAT | Severity |
|---|---|---|---|---|
| #1 | **Retry after cancel silently no-ops** back to the "Mithril Sync was cancelled" prompt: terminal `cancelled`+Retry is emitted before the killed `start()` unwinds and clears the coordinator `_partialSyncInProgress` guard, so a quick Retry hits "already in progress" and the store swallows it (`MithrilPartialSyncStore.ts:395-397`). **No cleanup state** is shown first. | D-702c-1..6 | **CAT-A** | High |
| #2 | **Overlay backdrop bleeds the live syncing screen**: the shared `.backdrop` (`MithrilBootstrap.scss:10-16`) is translucent (token alpha 0.92 × `opacity:0.97` ≈ 0.89) and has **no `backdrop-filter`**, unlike the app-wide `overlay-backrop` frosted-glass mixin used by 6 other overlays, so the animated `SyncingConnectingPage` shows through the app-level overlay (`App.tsx:98-99`, over `LoadingPage.tsx:82`). | D-702c-8 | **CAT-B** | Medium |
| #3 | **Diagnostics "Mithril Sync" option coupled to the lagging behind-ness probe**: gate is `isMithrilPartialSyncEnabled && isSignificantlyBehind` (`DaedalusDiagnostics.tsx:724-727`), but `isSignificantlyBehind` fails safe to `false` on first-load-after-genesis (empty `immutable/` dir → `PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE`; probe catch `MithrilPartialSyncService.ts:751-758`) with a 30s→5min back-off, so the fallback entry point stays absent while the probe keeps failing safe. Recovery is **delayed by the back-off, not gated on a restart**: the poll never stops (`MithrilPartialSyncStore.ts:279-291`, cleared only on `teardown`) and re-arms the fast 30s cadence on the first behind reading, so the option reappears without a restart once the node is genuinely behind. A restart only resets the cadence *and* coincides with immutable files finally existing — **temporally correlated, not causally required**. **Outcome: no renderer fix** — the `behindByEpochs`-based gate is unsound; keep the AND-gate, file backend probe-hardening separately. | D-702c-9 | **CAT-C** | Medium |
| #4 | **Proactive prompt chrome diverges from the bootstrap reference**: no title, center-aligned text/actions, two split paragraphs, and a double-dimmed note (`.handoffNote` `--theme-mithril-secondary-text-color` **+ `opacity:0.8`**, `SyncingConnectingMithrilPrompt.scss:34-41`); the fast button uses app `'primary'` not the Mithril `.primaryAction` token. Reported "blur" is a **misdiagnosis** (no `backdrop-filter` on this surface). | D-702c-10 | **CAT-D** | Medium |
| #5 | **Process ("confirm") prompt has no title + hand-rolled buttons**: `renderConfirmView()` (`SyncingConnectingMithrilPrompt.tsx:189-211`, returned JSX block) opens on the shared `processSummary` body with no `.header`/h1 and uses `'primary'` + font-less `styles.actionButton` instead of the bootstrap `.primaryAction`/`.secondaryAction`. | D-702c-11 | **CAT-E** | Low |
| #6 | **"Before Mithril Sync begins" dialog CSS is dead**: `.mithrilPartialSyncConfirmation*` rules are authored as descendants of `.component` (`DaedalusDiagnostics.scss:28-481`) but the Dialog portals to `document.body` (react-modal `Modal.js:216-217,93`), so the descendant selectors never match — paragraphs fall back to the global reset (`themes/index.global.scss:92-93`) and render as a zero-gap, undifferentiated stack. | D-702c-12 | **CAT-F** | Medium |

---

## CAT-A — retry-after-cancel: `cancelling` state + gated Retry + backend join (self-contained)

Implementable from this section + `task-ux-702c-decisions.md` (D-702c-1..6). Land the edits in this order
(types → backend → controller/coordinator → renderer → store → tests):

### 1. State machine (`source/common/types/mithril-partial-sync.types.ts`)
- Add `'cancelling'` to `MithrilPartialSyncStatus` (`:3-15`).
- Add `'cancelling'` to `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES` (`:74-83`) so `isWorking` and the derived
  `MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES` (`:91-96`) include it. ⚠️ If omitted, `hasDisplayStatus` is false and the
  overlay **hides** during cleanup (blank screen). Leave the TERMINAL set unchanged (`isMithrilPartialSyncActiveStatus`
  = `!== 'idle'` already covers it).
- **C-A1 (REQUIRED explicit edit):** `isMithrilPartialSyncBlockingNodeStart` is a **standalone hardcoded array
  literal** (`:118-131`), NOT derived from `WORKING_STATUSES`. Add `'cancelling'` to that literal too, or the
  node-start guard (consumers `MithrilStartupGate.ts:488`, `MithrilController.ts:189`) lets the cardano node start
  mid-cleanup and collapse the "Cleaning up…" frame. Add a unit test asserting
  `isMithrilPartialSyncBlockingNodeStart('cancelling') === true`.

### 2. Backend service (`source/main/mithril/MithrilPartialSyncService.ts`)
- Split `cancel()` (`:276-336`): keep the no-op branch (`:280-291`, gap #39 — must NOT emit `cancelling`) and the
  Boundary throw (`:293-297`). In the active branch, **emit `cancelling`** at entry (`allowedRecoveryActions: []`,
  `transferProgress: {}`, a "cleaning up" progress item), set `_isCancelled = true`, kill the process (`:303`), and
  **stop**.
- **C-A2 (REQUIRED):** move BOTH the destructive `_cleanupPartialSyncArtifacts()` (today `:312`) **and** its
  `failed`-on-cleanup-error terminal (today inline `:324-331`) OUT of `cancel()` and into `finalizeCancel()`. As-is,
  that `failed` branch emits a terminal with `['retry','restart-normal']` **before** the coordinator join clears
  `_partialSyncInProgress` (re-opening the race via `failed`), and throws before the coordinator ever reaches
  `finalizeCancel`. `cancel()` must therefore emit NO terminal (neither `cancelled` nor `failed`).
- **C-A3 (REQUIRED):** do **NOT** call `_clearRuntimeWorkState()` in `cancel()`'s finally (today `:334`) — it nulls
  `_currentProcess`, making `forceKill()` a silent no-op. `_currentProcess` must survive until the bounded join has
  (possibly) escalated to SIGKILL.
- Add **`finalizeCancel()`** — mirror `finalizeWipeAndFullSync` (`:360-364`), **guarded on `status === 'cancelling'`**
  (no-op otherwise; idempotent like `finalizeCompletedPartialSync` `:366-377`). It is invoked **only on the settled
  branch** (`_awaitRunSettledBounded` returned `true`), so `_partialSyncInProgress` is **confirmed false** when it
  runs — do not rely on the `status` guard for that invariant (it does not check the flag): `await
  _cleanupPartialSyncArtifacts()` → on success emit terminal `cancelled` with `['retry','restart-normal']`; on cleanup
  error emit terminal `failed` with `['retry','restart-normal']` (the old inline behavior, now correctly sequenced —
  safe to advertise Retry because the guard is already clear). Run `_clearRuntimeWorkState()` here (moved from
  `cancel()`).
- Add **`forceKill()`** (`_currentProcess?.kill('SIGKILL')`) for the bounded-join escalation (D-702c-5) —
  `cancel()`'s `.kill()` is Node's default SIGTERM (`:303`, no explicit signal), and a SIGTERM-ignoring child would
  never emit `close`, leaving the run promise pending forever.
- Add **`abandonCancel()`** (D-702c-5 corrected floor) — the **non-retryable operational-failure** floor for the
  pathological no-settle branch (SIGKILL did not produce `close` within the bound). **Guarded on
  `status === 'cancelling'`** (idempotent no-op otherwise). Emit a terminal `failed` whose `allowedRecoveryActions`
  **exclude `'retry'`** (leave only an app-restart escape — e.g. `['restart-normal']`) with a distinct "couldn't
  finish cleaning up — restart Daedalus" detail, and **do NOT run `_cleanupPartialSyncArtifacts()`** (the
  possibly-still-live process must not race `fs.remove`). Because it is reached only when `_partialSyncInProgress` may
  still be `true`, it must never advertise Retry. (Verify the surviving `restart-normal` action does **not** itself
  re-enter `startPartialSync` against the still-set guard; if it does, drop it to a bare restart message with no
  in-app recovery action.)

### 3. Controller + coordinator handlers
- `MithrilController._getPartialSyncDependencies` (`:439-452`): add `finalizeCancel`, `forceKill`, and `abandonCancel`
  to the handlers object next to `finalizeWipeAndFullSync` (`:446-447`). Extend the `PartialSyncHandlers` type
  (`chainStorageCoordinator.ts`).
- `chainStorageCoordinator.ts`: add field `_partialSyncRunPromise: Promise<void> | null = null`. In
  `startPartialSync` (`:220-272`) capture the post-preflight run as a promise whose `finally` clears both
  `_partialSyncInProgress` and `_partialSyncRunPromise`, store it (synchronously, at/after the lock so
  `cancelPartialSync` observes it), then `await` it (see D-702c-4 snippet). **Load-bearing:** the captured promise
  must be the IIFE wrapping the WHOLE try/`finally { _partialSyncInProgress = false }` — NOT the inner
  `handlers.start()` promise (awaiting the inner promise races the coordinator's own `finally` continuation, so
  `finalizeCancel` could run with the flag still true). Rewrite `cancelPartialSync` (`:274-278`) to **gate the
  retry-enabled terminal on the join having actually settled**:
  `await handlers.cancel()` →
  `const settled = _partialSyncRunPromise ? await _awaitRunSettledBounded(_partialSyncRunPromise) : true` →
  `if (settled) await handlers.finalizeCancel()` **else** `await handlers.abandonCancel()`.
- Add `_awaitRunSettledBounded(run)` (D-702c-5) **returning a `settled` boolean**: race `run.catch(()=>{})` vs
  `PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS` (**15000**); on timeout call `handlers.forceKill()`, then **re-await
  `run.catch(()=>{})` (short second bound)**. Return `true` once the run settles (its `finally` has run ⇒
  `_partialSyncInProgress` is now false), or `false` if it is **still pending after the SIGKILL bound**. Always clear
  the timer.
- **Corrected floor — the pathological no-settle branch must NOT reach a retry-enabled terminal.** `finalizeCancel`
  emits terminal `cancelled`/`failed` with `['retry','restart-normal']`, and its **only** guard is
  `status === 'cancelling'` — which still *passes* when `settled === false`, so "proceed to `finalizeCancel` anyway"
  would re-advertise Retry while `_partialSyncInProgress` may still be `true`, re-opening the exact race CAT-A
  eliminates. (The `cancelling` empty-actions state is **not** a floor here: `finalizeCancel` overwrites it with a
  retry-enabled terminal. The old "proceed regardless … the empty-actions state keeps Retry hidden even then" claim was
  self-contradictory.) So `settled === false` routes to **`abandonCancel()`** (step 2) — a **non-retryable operational
  failure** floor that emits a terminal whose recovery actions **exclude `retry`** and runs **no** destructive cleanup.
  Retry is thus never offered while the guard could still be set; the user still gets an actionable (restart) terminal
  rather than a wedged spinner.

### 4. Renderer overlay + copy
- `MithrilPartialSyncOverlay.tsx`: add `'cancelling'` to `PROGRESS_STATUSES` (`:42-52`) so it renders
  `MithrilProgressView`; add `'cancelling'` to the `hideAction` set (`:210-215`) so no Cancel button shows.
- `MithrilProgressView.tsx`: add a `cancelling` special-case ("Cleaning up…") beside the
  `stopping-node`/`starting-node` cases (`:138-139`); no progress bar / indeterminate spinner. **C-A4:** also
  **suppress the step-indicator waterfall and the elapsed timer** for `cancelling` — both render unconditionally today
  (elapsed `timerDisplay` `:184-189`; `waterfallContainer` wrapping `MithrilStepIndicator` `:197-209`); `cancelling`
  maps to `getActiveStepIndex` = −1, and the store does not re-anchor `startedAt` on a working→working transition, so
  the default frame would show a reset waterfall + a timer still ticking from download start. Verify the frame in the
  planned `cancelling` Storybook story across themes.
- New EN + JA i18n keys (`MithrilBootstrap.messages.ts` + `en-US.json`/`ja-JP.json` + defaultMessages/translations)
  for the "Cleaning up…" title/detail (vocab guardrail #8; JA best-effort per 601 glossary).

### 5. Renderer store defense-in-depth (`MithrilPartialSyncStore.ts`)
- `cancelPartialSync` (`:402-411`) needs no logic change (the `cancelling`→`cancelled` pushes drive the UI; the IPC
  round-trip now covers the full sequence). `canRetry`/`canRestartNormally` (`:170-178`) are already false during
  `cancelling` (empty actions) — the UI-layer gate.
- `startPartialSync` (`:368-400`): in the swallow branch (`:395-397`), `logger.warn` the rejected start instead of
  returning silently (D-702c-6). Do not change the optimistic-frame happy path.

### 6. Tests + Storybook + PRD (see `task-ux-702c-decisions.md` "Test + verification plan")
Backend service + coordinator units (incl. the two fake-timers join units — **settled → `finalizeCancel`** and
**no-settle → `forceKill` → `abandonCancel`** [non-retryable, no cleanup, Retry never advertised] — and the
`cancelling`-blocks-node-start types unit placed in
`source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` beside the existing
`isMithrilPartialSyncOverlayStatus` helper block), renderer store + overlay units, a `cancelling` Storybook story in
`storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` (the canonical CAT-A story owner across
themes; do not split this requirement into `MithrilProgressView.stories.tsx`), the PRD status-contract update
(`prd.md:147-160` add `cancelling`; **reconcile the phantom `confirming`** while there), and the operator manual
re-test (U1).

**Rewrite (not just add to) the existing cancel specs — they encode the OLD contract and will fail CI first:**
- `MithrilPartialSyncService.spec.ts` has **two** cases that assert `cancel()` itself finishes cleanup and lands
  **directly** in a terminal state: `'cleans staging artifacts … when cancellation succeeds before cutover'`
  (`:859-886`, asserts `removeMock`/`clearMithrilPartialSyncMarkerMock` ran inside `cancel()` **and**
  `status: 'cancelled'`) and `'surfaces a boundary-a failure when cancellation cleanup fails'` (`:888-914`, asserts
  `cancel()` rejects and lands `status: 'failed'`). Under the new split (`cancel()` emits only `cancelling`; cleanup +
  terminal move to `finalizeCancel`) both **must be rewritten** to drive the full coordinator sequence
  (`cancel` → join → `finalizeCancel`/`abandonCancel`) and assert `cancel()` emits `cancelling` with **no** terminal.
  The 3 other cancel-touching cases (`:787`, `:805`, `:836` — post-cutover reject + the node-stop-window no-op re-emit)
  are the preserved Boundary/no-op branches and stay unchanged.
- `chainStorageCoordinator.spec.ts` (at `source/main/utils/`) does **not** encode the old terminal/cleanup contract —
  the existing `'cancels partial sync immediately through the registered handlers'` case (`:655-686`) only asserts
  `handlers.cancel` was called once, which still holds. But its mock handlers object (`:23-30`) lacks the new members,
  so **add `finalizeCancel`/`forceKill`/`abandonCancel` jest.fns to the mock** for the new sequence (mock setup, not a
  rewrite).

Node v24 renderer verify-env caveat applies (regen `.scss.d.ts` via typed-scss-modules + the gitignored
identity-obj-proxy jest sidecar before treating tsc/jest fails as regressions).

---

## CAT-B — overlay backdrop: add the app-wide frosted-glass blur to the shared `.backdrop` (self-contained)

Implementable from this section + `task-ux-702c-decisions.md` (D-702c-8). Styling-only; no TSX, i18n, store, IPC, or
backend change. Two references: the app-wide `overlay-backrop` mixin and the existing Mithril tint token.

### 1. SCSS (`source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss`)
- Add `@import '../../../themes/mixins/overlay-backdrop';` at the top of the file (import-depth precedent
  `MithrilStepIndicator.scss:3`).
- Inside `.backdrop` (`:10-16`) add `@include overlay-backrop;` (note the typo'd mixin name in
  `themes/mixins/overlay-backdrop.scss:1-2` = `backdrop-filter: blur(5px)`).
- Also set `opacity: 1;` on `.backdrop` (still translucent via the token's 0.92 alpha) to firm the scrim and avoid
  the opacity-group compositing nuance. Keep the tint token `--theme-mithril-overlay-backdrop-start`; do **not** edit
  per-theme alphas.
- Do **not** create an overlay-only class — this is intentionally a shared-rule change to the two component importers
  (`MithrilBootstrap.tsx`, `MithrilPartialSyncOverlay.tsx`). The mixin is already consumed by six overlays; the shared
  rule also firms the **bootstrap** tint (effective α ~0.89→0.92) — acceptance is "slightly-firmer bootstrap tint is
  acceptable," not "unchanged" (nothing busy renders behind the bootstrap card: `LoadingPage.tsx:72-77` replaces the
  syncing screen).

### i18n
- None (styling-only; vocab-neutral).

### Tests
- **On-device manual test is the load-bearing proof.** Trigger a partial sync on a behind node so
  `SyncingConnectingPage` is live behind the overlay; verify it is blurred/illegible through the backdrop during
  stopping-node/downloading and on the failed/cancelled prompt; repeat across light-blue, cardano, white. Elevate
  **U2** (blur over animated content — GPU jank) to a required low-end/Linux check.
- **Storybook is NOT valid proof here** — the story frame renders `.backdrop` over a solid canvas, so `blur(5px)` is
  imperceptible and the story looks identical pre/post-fix. Add a story variant with animated placeholder content
  *behind* the frame if Storybook coverage is wanted.
- Unit (regression): `MithrilPartialSyncOverlay.spec.tsx` + `MithrilBootstrap.spec.tsx` still render; `.backdrop` node
  still present for both surfaces.
- `yarn compile` / renderer build: confirm no error from the new `@import` path.

---

## CAT-C — diagnostics option: keep the AND-gate; file the first-load fix as a backend follow-up (verify-only)

Implementable from this section + `task-ux-702c-decisions.md` (D-702c-9). **CAT-C ships no renderer change.**

**Why the OR-gate is rejected:** `behindByEpochs` (`utils/mithrilBehindness.ts:45-66`) anchors on the **live**
`networkTip.epoch`, while `isSignificantlyBehind` measures the gap to the **lagging certified snapshot**
(`MithrilPartialSyncService.ts:738-747`). So `behindByEpochs≥1` is reachable while `isSignificantlyBehind=false`
(synced-past-certified-but-behind-live-tip, near-tip rollover, first-load empty-immutable, transient aggregator
failure). In those windows the OR-gate would offer a partial sync that **dead-ends** in
`PARTIAL_SYNC_NO_CERTIFIED_RANGE` / `PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE`
(`mithrilPartialSyncPreflight.ts:132-138,148-154`), and a premature Start **burns `mithrilAttemptStartedThisSession`**
(`MithrilPartialSyncStore.ts:373`, never reset) — permanently suppressing the *legitimate* proactive prompt, which
ANDs `!mithrilAttemptStartedThisSession` (`MithrilProactivePromptContainer.tsx:73`). `isSignificantlyBehind` is the
**sole offer signal** by locked boundary #4 (documented `mithrilBehindness.ts:13-15`).

### 1. In-wave change: NONE
- Leave `shouldShowRecommendation` (`DaedalusDiagnostics.tsx:724-727`) coupled to
  `isMithrilPartialSyncEnabled && isMithrilPartialSyncSignificantlyBehind` (parity with the proactive prompt; the
  shared blackout is thereby *preserved*). `behindByEpochs` (computed `:573`, passed `:728`) stays **display-only**
  (confirmation copy; the CTA gate is `MithrilPartialSyncSection.tsx:130-132`, on `shouldShowRecommendation` alone).
  No gate/prop/plumbing edit; no i18n/SCSS/theme change.

### 2. Follow-up task (separate, NOT this wave): backend offer-signal hardening
- The real first-load gap (option absent until immutable files exist — the restart correlation is incidental, the
  poll self-recovers without one) is a **backend** problem: the
  probe collapses every failure to `isSignificantlyBehind:false`. File a separate task to have
  `getPartialSyncBehindness` distinguish **"unknown/unavailable"** from **"not behind"**, so the renderer can surface
  the option on first-load *only when a partial sync is actually fulfillable*. This touches **locked offer-signal
  boundary #4** and must be scoped/grilled on its own — do NOT fold it into 702c.

### i18n
- None (no change).

### Tests (verify-only — assert the CURRENT behavior is intact)
- Unit: `MithrilProactivePromptContainer` returns null when `isSignificantlyBehind=false` (suppression intact); the
  Diagnostics option is hidden in the same state (still coupled — no divergence introduced).
- Regression: `DaedalusDiagnostics.spec.tsx:119-139` "shows … only when enabled and significantly behind" still passes
  unchanged (the rejected OR-gate would have broken it — default tips 100/101 ⇒ `behindByEpochs=1`).
- Manual (verify-only): confirm the first-load window is a **known, documented** limitation pending the backend
  follow-up — the Diagnostics option and proactive prompt both appear once the node is genuinely
  significantly-behind. (Record U3 first-load duration for the follow-up's context.)

---

## CAT-D — proactive prompt: re-align chrome to the MithrilBootstrap reference (merge with CAT-E)

Implementable from this section + `task-ux-702c-decisions.md` (D-702c-10, D-702c-13). Touches one component + SCSS +
3 locale files + specs + the existing story. **Land as one edit with CAT-E** — both edit the same file/buttons.

### 1. TSX (`source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx`)
- Choice view (`:142-183`): add a title element (prefer semantic `<h1 className={styles.title}>`) rendering the new
  title key **before** the body.
- Merge sentence 1 (`promptBody`/`promptBodyUnknown`) and sentence 2 (`promptBodyBenefit`) into **one**
  `<p className={styles.body}>` as **sibling inline `<span>`s** with an explicit `{' '}` between them (so each stays
  individually queryable — see test note; preserves the epochs/Unknown branch `:152-156`). Remove the separate
  `.benefit` paragraph.
- Note (`:161-163`): put **"Note:" in its own element** (a `<strong>` span — not in-copy, which would mutate the exact
  string the spec asserts) using the updated `promptHandoffNote`.
- Buttons: swap `'primary'` → `styles.primaryAction` on the fast button (`:175`); swap `styles.actionButton` →
  `styles.secondaryAction` on the Standard button (`:166`). (Apply the same swaps to the confirm view — CAT-E.) Keep
  `min-width:180px` on the buttons and `gap:12px` on `.actions` (else buttons lose spacing/min-width).

### 2. SCSS (`SyncingConnectingMithrilPrompt.scss`)
- Remove `text-align: center` from `.component` (`:12`) — **left-align both views**.
- Add `.title { color: var(--theme-mithril-heading-color); font-family: var(--font-medium); font-size: 18px;
  margin: 0 0 8px; }` (18px for the compact card).
- `.body`: `color: var(--theme-mithril-body-text-color); font-family: var(--font-regular); font-size: 15px;
  line-height: 1.5; margin: 0; text-align: left;`. Delete `.benefit`.
- `.handoffNote`: remove `opacity: 0.8`; keep 13px / `--theme-mithril-secondary-text-color`; set line-height 1.5 (was
  1.4, mirroring `MithrilProgressView.scss:54-55 .reassurance` — NOT `.errorHint`); add `text-align: left`.
- Add `.primaryAction`/`.secondaryAction` copied verbatim from `MithrilErrorView.scss:85-111`, and **augment the
  copied `.primaryAction` with `&[disabled]` (+ `:focus`)** (the confirm-view Start button is `disabled={isStarting}`;
  verbatim copy drops that affordance). Keep `min-width: 180px`.
- `.actions`: `justify-content: flex-end; gap: 12px;` (drop per-button `margin: 0 8px`). `flex-end` is a design choice
  (reference base `.actions` is flex-start; flex-end is the `.actionsRightAligned` modifier).
- Regenerate `SyncingConnectingMithrilPrompt.scss.d.ts` (typed-scss-modules) for the new classes.

### i18n (`en-US.json`, `ja-JP.json`, generated `defaultMessages.json`)
- New title key via `defineMessages`, e.g. `daedalus.diagnostics.dialog.mithrilProactivePromptTitle = "Mithril Sync"`;
  run `yarn i18n:manage` (defaultMessages.json is generated — do not hand-edit).
- Update `promptHandoffNote` (JA best-effort per the 601 glossary; keep vocab-clean — no "partial sync"/%/immutable;
  epochs-only).

### Tests
- Unit (`SyncingConnectingMithrilPrompt.spec.tsx`): assert the "Mithril Sync" title renders; assert both sentences
  render (as sibling spans — keep the exact-text queries at `:46-52` working); assert the note element contains
  "Note:" and the note body assertion (`:69-73`) still passes; keep the `/partial sync/i` + `/immutable/i` negative
  guards (`:53-54`); assert the fast button carries `styles.primaryAction` and the Standard button
  `styles.secondaryAction`.
- Unit (`MithrilProactivePromptContainer.spec.tsx` — **add to filesToTouch**): its exact `getByText` queries
  (`KNOWN_EPOCHS_TEXT` def `:13`, positive `:65`/`:159`, hardcoded literal `:148`) must still pass with the
  sibling-span merge.
- Storybook: extend `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` for
  `behindByEpochs` (known/undefined) and view (choice/confirm) across cardano / light-blue / white.
- Manual: on a behind node confirm the prompt shows the title, a left-aligned single paragraph, a readable (not
  greyed) "Note:" line, an action-colored fast button, right-aligned actions; across default + light-blue.

---

## CAT-E — process ("confirm") prompt: add title + bootstrap button chrome (merge with CAT-D)

Implementable from this section + `task-ux-702c-decisions.md` (D-702c-11, D-702c-13). **Land as one edit with
CAT-D** — same component/SCSS/buttons. Presentation + i18n only; no store/IPC/backend change.

### 1. TSX (`SyncingConnectingMithrilPrompt.tsx`, `renderConfirmView()` returned JSX `:189-211`)
- Add the title element "Mithril Sync Process" (reuse the **left-aligned** `.title` added in CAT-D — do not center
  it) before the `confirmBody` paragraph; keep the shared `processSummary`
  (`mithrilSyncProcessSummaryMessages.processSummary`) as the body. Set `.confirmBody` (and the lifted dialog error)
  to `text-align: left`.
- Buttons: Start (`:203`) → `styles.primaryAction`, Cancel (`:197`) → `styles.secondaryAction` (same swap as CAT-D;
  applied to both views). Keep `min-width:180px` + `gap:12px` on `.actions`.

### 2. SCSS (`SyncingConnectingMithrilPrompt.scss`)
- Reuse the `.title` and `.primaryAction`/`.secondaryAction` rules added in CAT-D (do not duplicate).
- Optional: normalize `.confirmBody` to 15px / `--theme-mithril-body-text-color` for parity with the choice-view `.body`
  (already 15px per CAT-D); do **not** leave `.confirmBody` at 16px (16px is only the pre-edit base state, never a valid
  post-edit outcome).
- Regenerate `.scss.d.ts` for any new classes.

### i18n (`en-US.json`, `ja-JP.json`, generated `defaultMessages.json`)
- New local title key via `defineMessages`, e.g.
  `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmTitle = "Mithril Sync Process"`. Do **NOT** fork the
  shared `mithrilSyncProcessSummary` key (`MithrilSyncProcessSummary.messages.ts:11` warns against redeclaring). JA
  best-effort; vocab-clean. *(Optional DRY: reuse the already-translated `mithrilPartialSyncConfirmationTitle` instead
  of minting a third title.)*

### Tests
- Unit (`SyncingConnectingMithrilPrompt.spec.tsx:76-89`): assert the confirm view renders the "Mithril Sync Process"
  title AND still renders the shared `processSummary` body; assert Start carries `styles.primaryAction` and Cancel
  `styles.secondaryAction` (no global `'primary'`).
- Unit (regression): "Start now" still calls `onStart` exactly once (`:113-124`); "Cancel" still returns to the choice
  view without starting (`:125-137`).
- Storybook: extend the `MithrilPartialSyncDialogue.stories.tsx` confirm view across light-blue + one dark theme,
  side-by-side with a `MithrilErrorView` story to confirm button parity.
- Checks: `yarn compile` + `yarn lint` + `yarn prettier:check` + touched jest green (apply the Node v24
  typed-scss-modules `.scss.d.ts` regen + identity-obj-proxy jest sidecar per the renderer verify-env memory).

---

## CAT-F — "Before Mithril Sync begins" dialog: un-nest the dead descendant-scoped CSS (self-contained)

Implementable from this section + `task-ux-702c-decisions.md` (D-702c-12). SCSS re-scope + one-line TSX swap. No
i18n/store/IPC/backend change.

### 1. SCSS (`source/renderer/app/components/status/DaedalusDiagnostics.scss`)
- **Lift** `.mithrilPartialSyncConfirmationDialog` (`:373`), `.mithrilPartialSyncConfirmationBody` (`:344`),
  `.mithrilPartialSyncConfirmationBehind` (`:377`), `.mithrilPartialSyncConfirmationRecovery` (`:386`),
  `.mithrilPartialSyncConfirmationPrimaryButton` (`:395`), `.mithrilPartialSyncConfirmationSecondaryButton` (`:399`)
  **out** of the top-level `.component { }` block (`:28`–`:481`) to flat top-level selectors — mirror
  `ExportWalletToFileDialog.scss` (flat content classes; `styles.*` passed as the Dialog className, already applied at
  `MithrilPartialSyncConfirmation.tsx:72`).
- Keep existing tokens (`--theme-dialog-text-color`, `--theme-dialog-title-color`, `var(--font-regular)`,
  `var(--font-medium)`). Do NOT import mithril-card vars.
- Replace the collapsing margins: set `.mithrilPartialSyncConfirmationBody { p { margin: 0 } p + p { margin-top: 20px } }`
  and **drop the `margin` declarations from `.Behind` and `.Recovery`** (keep only their font/color) so the
  higher-specificity Body rule does not clobber a leftover margin. Keep P1 emphasised (font-medium, title-color) and
  P2 de-emphasised (14px).
- **Also lift a dialog error class + swap one TSX classname.** The `.error` at `:180` is dead the same
  descendant-scoped way and the start-failure path renders it (`MithrilPartialSyncConfirmation.tsx:108`); it is shared
  with the live in-`.component` diagnostics table (`DaedalusDiagnostics.tsx:702`), so add a **new** flat
  `.mithrilPartialSyncConfirmationError` (using `--theme-dialog-*`, not `--theme-network-window-red-color`) and swap
  `styles.error` → `styles.mithrilPartialSyncConfirmationError` at `:108` (one-line TSX change).
- **DROP the wrapper `.Body` `opacity: 0.85`** — it compounds with `.Recovery` `opacity: 0.7` to ~0.6 effective on P2
  (a contrast regression vs today's full-opacity fallback). Express P2 de-emphasis via a single opacity/color-alpha.
- Delete the confirmed-unused `.mithrilPartialSyncConfirmationActions` (`:356-367`) and
  `.mithrilPartialSyncConfirmationCancelButton` (`:369-371`) — referenced only by the generated `.scss.d.ts`. Repair
  the stale in-file "restyled IN PLACE" comment (`:383-385`).
- Regenerate `DaedalusDiagnostics.scss.d.ts` (typed-scss-modules); drop the removed class exports.

### i18n
- None (pure SCSS scoping + one classname swap; the paragraphs are existing i18n strings, unchanged).

### Tests
- Storybook (strongest proof): via `storybook/stories/nodes/status/Diagnostics.stories.tsx`, render the confirmation
  directly for both `behindByEpochs` set and the `behindUnknown` fallback, **light and dark**. Before: paragraphs
  touch with identical font. After: P1 emphasised, P2 de-emphasised, ~20px gap, width ~560px, buttons ~180px
  min-width. Ensure the decorator/theme provider still supplies `--theme-dialog-*` vars to the portal (react-modal
  renders into `document.body`).
- Jest (`MithrilPartialSyncConfirmation.spec.tsx`, guard not CSS proof): assert both `<p>` carry
  `mithrilPartialSyncConfirmationBehind` / `mithrilPartialSyncConfirmationRecovery`, exactly two paragraphs, and the
  real `.ReactModal__Overlay` still mounts (`:36`). Do not assert computed margins (identity-obj-proxy).
- Manual: Diagnostics → Mithril Sync section → Start → verify (a) visible vertical gap between the epochs line and the
  processSummary line, (b) P1 heavier than P2, (c) width ~560px and ~180px buttons; repeat with tips/epoch unavailable
  to hit `behindUnknown`.

---

## Locked safety boundaries (honor while reasoning)
1. **Cancel is Boundary-A-only** (`prd.md:190`); `cancelling` is a Boundary-A transitional state cleaning only staged
   artifacts (`prd.md:192`). No cancel after cutover.
2. **Terminal `cancelled` recovery set stays `['retry','restart-normal']`** (D-702a-2; no wipe).
3. **Kill switch #1/#2, staged-only #3, latest-snapshot-only #5, no bootstrap regression #7, vocab #8** — all carried
   unchanged from `task-ux-702.md:112-127`.
4. **`cancelling` must never wedge, and Retry must never be re-advertised while the in-progress guard may still be
   set** — bounded join + SIGKILL; on **confirmed settle** → `finalizeCancel` (retry-enabled terminal, guard proven
   clear); on **no-settle** → `abandonCancel` (a **non-retryable** operational-failure floor, `retry` excluded, no
   destructive cleanup) (D-702c-5). A *cleanup* error on the settled branch still routes to the `failed` terminal with
   `['retry','restart-normal']` (safe there — the guard is already clear).
5. **CAT-B..CAT-F boundaries** — the backend behind-ness probe / offer signal (#4) is untouched (CAT-C makes no
   renderer or backend change; the probe-hardening is a *separate* follow-up); proactive-prompt *suppression* logic is
   untouched (CAT-D restyles chrome only); every fix reuses an existing token/mixin/reference (no new
   colors/classes/theme vars); no `cancelling`/state-machine or PRD status-contract change (CAT-B..CAT-F are
   presentation-only); vocab guardrail #8 holds (titles "Mithril Sync" / "Mithril Sync Process"; epochs-only; never
   "partial sync"/%/immutable).

## Implementation approach — ordered checklist (small-model-implementable)

**CAT-A:**
1. Types: add `cancelling` (enum + working set) **and, explicitly, the `isMithrilPartialSyncBlockingNodeStart` literal
   `:118-131` — C-A1** (not auto-covered by the working set).
2. Backend: split `cancel()` (emit `cancelling`, kill, **stop** — no cleanup, no terminal); add `finalizeCancel()`
   that **owns cleanup + the terminal `cancelled`/`failed` emit** (C-A2) and `_clearRuntimeWorkState()` (C-A3); add
   `forceKill()` (SIGKILL); add `abandonCancel()` (the **non-retryable** no-settle floor — terminal `failed`, `retry`
   excluded, no destructive cleanup). `cancel()` must NOT clear runtime state (keeps `_currentProcess` for `forceKill`).
3. Coordinator/controller: `_partialSyncRunPromise` capture (**IIFE wrapping the whole try/finally**) + join + handler
   wiring + bounded-join helper (**returns `settled`**; `settled` → `finalizeCancel`, no-settle → `abandonCancel` —
   never `finalizeCancel` while the guard may still be set).
4. Renderer: overlay routing + `hideAction` + progress-view "Cleaning up…" case **with waterfall/timer suppressed**
   (C-A4) + i18n.
5. Store: defense-in-depth `logger.warn` on swallowed start rejection.
6. Tests (incl. the two join units — **settled → `finalizeCancel`** and **no-settle → `forceKill` → `abandonCancel`**
   (non-retryable, no cleanup) — plus the `cancelling`-blocks-node-start unit, **and rewriting the two old-contract
   cases in `MithrilPartialSyncService.spec.ts` `:859-886`/`:888-914`**) + Storybook + PRD status-contract update
   (`prd.md:147-160` add `cancelling`; **reconcile the phantom `confirming`** while there).
7. Per-category code-review pass; `yarn compile` + touched specs; whole-task verify (compile/lint/prettier/stylelint/
   jest).

**CAT-B..CAT-F (independent per-category subagents, except CAT-D+CAT-E which merge):**
1. **CAT-B:** `@import` + `@include overlay-backrop;` (+ `opacity:1`) in `MithrilBootstrap.scss` `.backdrop`.
2. **CAT-C:** **NO code change** — run a validation-only subagent pass that keeps the AND-gate in
  `DaedalusDiagnostics.tsx:724-727`, proves proactive suppression + the Diagnostics gate stay coupled via the
  unchanged regression checks, and files the backend probe-hardening as a separate follow-up task.
3. **CAT-D + CAT-E (one edit, D-702c-13):** in `SyncingConnectingMithrilPrompt.tsx/.scss` add both titles, merge +
   **left-align both views**, de-dim the "Note:" (its own element), **copy** `.primaryAction`/`.secondaryAction`/
   `.title` into this component's SCSS once (augment `.primaryAction` with `[disabled]`/`:focus`), apply to both views,
  keep `min-width:180px` + `gap:12px`; new i18n title key(s) (EN + JA via `defineMessages` + `yarn i18n:manage`);
  update both specs (incl. `MithrilProactivePromptContainer.spec.tsx`); regen `.scss.d.ts`; extend
  `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx`.
4. **CAT-F:** un-nest `.mithrilPartialSyncConfirmation*` out of `.component` in `DaedalusDiagnostics.scss`, add
   `p + p { margin-top: 20px }`, drop the collapsing Behind/Recovery margins, **drop the `.Body` opacity:0.85**, delete
   the two dead classes, **lift a dialog `.mithrilPartialSyncConfirmationError` + swap `styles.error`→it at
   `MithrilPartialSyncConfirmation.tsx:108`** (one-line TSX), regen `.scss.d.ts`; QA the whole dialog light+dark.
5. Per-category code-review pass; Storybook/on-device proof per surface; `yarn compile` + touched specs; whole-task
   verify (compile/lint/prettier/stylelint/jest) with the Node v24 renderer verify-env caveat applied.

## Verification plan
- **Automated:** the unit/overlay/store suites in the decision-record test plan; per-CAT specs
  (`MithrilPartialSyncOverlay`, `DaedalusDiagnostics`, `SyncingConnectingMithrilPrompt` +
  `MithrilProactivePromptContainer`, `MithrilPartialSyncConfirmation`); `yarn check:all` green with no real
  regressions (baseline prettier caveat per 702b).
- **Storybook / on-device (styling proof):** overlay backdrop over animated content across light-blue/cardano/white
  (CAT-B, on-device load-bearing); proactive choice + confirm views (CAT-D/E); the "Before Mithril Sync begins" dialog
  for both `behindByEpochs` set and the `behindUnknown` fallback, light + dark (CAT-F); the `cancelling` overlay
  (CAT-A).
- **Operator (verify-only, preprod/Linux):** (CAT-A) cancel mid-download → "Cleaning up…" → immediate "Mithril Sync
  (fast)" (no drop-back), record teardown time (U1); (CAT-B) the overlay no longer bleeds the node-status page (U2
  jank); (CAT-C) confirm the first-load window is a known limitation pending the backend follow-up (U3); (CAT-D/E) the
  proactive/process prompts read as titled + left-aligned + action-colored + readable (U4); (CAT-F) the "Before
  Mithril Sync begins" paragraphs have a clear gap.

## Risks / open questions

**No product sign-off remains open — all decisions are LOCKED (`task-ux-702c-decisions.md`).** What remains is
**verification-only**:
- **Join-timeout value (15 s default):** confirm against real preprod/Linux teardown latency (U1). No IPC-layer
  timeout rejects the ~15 s cancel request first (`IpcConversation.request:65-96` has none), so the request stays
  pending safely.
- **U1** SIGTERM latency/compliance; **U2** backdrop-filter GPU jank over *animated* content (required low-end check);
  **U3** first-load window duration (context for the CAT-C backend follow-up); **U4** the "blur" complaint is a
  confirmed misdiagnosis (no CSS blur on that surface).
- **Regression guards:** the no-op cancel branch (D5f) and Boundary invariants are preserved by explicit guards;
  covered by unit tests (incl. the two new join coordinator tests — **settled → `finalizeCancel`** and
  **no-settle → `forceKill` → `abandonCancel`**, asserting Retry is never advertised on the no-settle floor).

## Review-log paths
- Decision record: `task-ux-702c-decisions.md` (LOCKED).
- Plan review: `task-ux-702c-plan-review.md` (Planner/Critiquer append-only transcript).
- Implementation review: `task-ux-702c-impl-review.md` (Implementation + Code Review + Scribe append-only log).
- Research notes: `task-ux-702c-research.md` (durable findings / operator follow-up context).

## Planning status / build status
- Planning status: **approved** — CAT-A (correctness) + CAT-B..CAT-F (presentation), decisions D-702c-0..13 in
  `task-ux-702c-decisions.md`. Every root cause verified against live code; all category edit-shape items (C-A1..C-A4,
  CAT-B/D/E/F) are part of the definitive fix shape. **No product sign-offs remain open** — only verification-only
  items (U1–U4, join-timeout value).
- Build status: **in_review** (all code-bearing CATs landed, per-category reviews are approved, and focused automated
  verification is green on task-owned files; operator validation U1–U4 is still pending before task completion).
- Tasks JSON: `task-ux-702c` entry `status: "pending"`, with CAT-A..CAT-F targetPaths / implementationNotes /
  testCases / acceptance; `task-ux-702` depends on `task-ux-702c`. No `completedAt`.

---

## Planner

Timestamp: 2026-07-01
Speaker: Planner (manual-testing cleanup wave)

Authored the canonical plan for `task-ux-702c` from the locked decision record (`task-ux-702c-decisions.md`,
D-702c-0..13). Six operator manual-testing findings are remediated as CAT-A..CAT-F. **CAT-A** (correctness): the
retry-after-cancel race — terminal `cancelled`+Retry is emitted un-coordinated with the still-unwinding `start()`
that owns the coordinator's `_partialSyncInProgress` guard, so a quick Retry hits "already in progress" and the store
silently swallows it. Locked the Full fix: a new `cancelling` working-status ("Cleaning up…", empty recovery actions)
gating Retry during teardown, a coordinator-owned `_partialSyncRunPromise` join so terminal `cancelled` fires only
after the guard clears, a guarded `finalizeCancel` handler that owns cleanup + the terminal emit, a bounded join with
SIGKILL escalation whose helper returns `settled` (settled → `finalizeCancel`; no-settle → an `abandonCancel`
**non-retryable** floor that never re-advertises Retry while the guard may still be set), and a renderer
defense-in-depth log — with the definitive edit shape C-A1..C-A4. **CAT-B**: add the app-wide `overlay-backrop` blur (+ `opacity:1`) to the single shared `.backdrop` rule.
**CAT-C**: keep the AND-gate coupled to `isSignificantlyBehind` (the sole offer signal, boundary #4) — the OR-gate is
rejected as unsound; file the real first-load fix as a separate backend follow-up. **CAT-D + CAT-E** (merged): re-align
the proactive/confirm prompt chrome to the bootstrap reference (titles, merged left-aligned body, de-dimmed "Note:"
element, `.primaryAction`/`.secondaryAction` copied in locally). **CAT-F**: un-nest the descendant-scoped
`.mithrilPartialSyncConfirmation*` CSS to flat selectors and add real `p + p` spacing + a lifted dialog error class.
Carried the locked invariants (Boundary-A-only cancel, no wipe in the cancelled dialogue, offer-signal boundary #4,
vocab guardrail). Held the deployment gate open: `task-ux-702` depends on `task-ux-702c`; no `completedAt`. No product
sign-off remains open — only operator verification-only items (U1–U4, the 15 s join-timeout value).

Decision: LOCKED.
