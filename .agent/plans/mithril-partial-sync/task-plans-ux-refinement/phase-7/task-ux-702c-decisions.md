# task-ux-702c — Grill Decision Record (manual-testing cleanup wave)

> Locked decision record for `task-ux-702c`, the phase-7 **extensible manual-testing cleanup wave** on the
> shipped Mithril Sync UX. It captures six operator manual-testing findings (#1–#6) as categories
> **CAT-A..CAT-F**, each root-caused and adversarially verified against live code. Every decision below is
> stated once in its **final, locked form** (LOCKED 2026-07-01); each carries an explicit
> `→ Locked path` line.
>
> Grounded by read-only exploration of live code; every load-bearing anchor re-verified against the working
> tree (2026-07-01). Predecessors: `task-ux-702b-decisions.md` (D-702b-0..10), `task-ux-702a-decisions.md`.
> Domain model: `mithril-partial-sync-prd.md` (status contract :147-160, Boundary A/B/C recovery model
> :176-194). Process rules: `prompt-ux-refinement.md`. Companion plan: `task-ux-702c.md` (implementable
> per-category tasking derived from this record).

---

## Scope (D-702c-0 + D-702c-7) — LOCKED

`task-ux-702c` is a **net-new phase-7 manual-testing cleanup wave** — the second remediation wave after the
702a copy/UX round and the 702b code-review round — structured as an **extensible wave** so additional
operator findings fold in as new categories without restructuring. Six findings are in hand:

| # | Finding (short) | CAT | Scope | Severity |
|---|---|---|---|---|
| #1 | Retry after cancel silently no-ops back to the cancelled prompt; no cleanup state shown first | **CAT-A** | state machine + backend + renderer | High |
| #2 | Overlay backdrop bleeds the live syncing screen through the app-level overlay | **CAT-B** | SCSS only | Medium |
| #3 | Diagnostics "Mithril Sync" option absent on first-load-after-genesis (lagging behind-ness probe) | **CAT-C** | verify-only + backend follow-up | Medium |
| #4 | Proactive prompt chrome diverges from the bootstrap reference (no title, centered, dimmed note) | **CAT-D** | renderer + i18n | Medium |
| #5 | Process ("confirm") prompt has no title + hand-rolled buttons | **CAT-E** | renderer + i18n | Low |
| #6 | "Before Mithril Sync begins" dialog CSS is dead (descendant-scoped under a portaled Dialog) | **CAT-F** | SCSS + 1-line TSX | Medium |

**CAT-A is the only correctness fix** (a race + a UX gap); **CAT-B..CAT-F are presentation / renderer / i18n
scope** — none touches the `cancelling` state machine, the Boundary A/B/C recovery model, or the backend
offer-signal boundary (#4). Sequencing: **CAT-A is independent; CAT-B, CAT-C, CAT-F each land as their own
subagent; CAT-D + CAT-E MUST be one coordinated edit** (both mutate `SyncingConnectingMithrilPrompt`;
D-702c-13). Every CAT-B..CAT-F fix reuses an existing token/mixin/reference component — **no new colors,
classes, or theme vars**.

Depends-on chain: **`…702a → …702b → …702c → …702`**. The deployment QA gate `task-ux-702` stays held behind
702c (its `dependencies` includes `task-ux-702c`).

→ **Locked path:** ship the wave as CAT-A (correctness) + CAT-B..CAT-F (presentation); hold the gate open
behind it; run CAT-A / CAT-B / CAT-C / CAT-F as independent subagents and CAT-D+CAT-E as one merged edit.

---

## CAT-A — retry-after-cancel: root cause, fix, and mechanism

### Root cause (D-702c-1) — LOCKED (verified against live code)

The failure is a **race between two un-coordinated async chains**, compounded by a renderer swallow:

1. **The start guard outlives the visible cancel.** `chainStorageCoordinator.startPartialSync` sets
   `_partialSyncInProgress = true` inside the mutation lock (`chainStorageCoordinator.ts:258`), **releases the
   lock**, and runs the long `await dependencies.handlers.start(preflightContext)` **outside** the lock
   (`:268`); the flag is cleared only in that call's `finally` (`:270`) — i.e. when `start()` fully unwinds.
2. **Cancel is a separate chain that advertises Retry immediately.** `cancelPartialSync` takes **no lock** and
   only calls `handlers.cancel()` (`:274-278`). `MithrilPartialSyncService.cancel()` kills the process
   (`MithrilPartialSyncService.ts:303`), awaits `_cleanupPartialSyncArtifacts()` (`:312`), then emits terminal
   **`status:'cancelled'` with `allowedRecoveryActions:['retry','restart-normal']`** (`:314-321`) — enabling the
   "Retry Mithril Sync (fast)" button. This emission is **not coordinated** with the still-unwinding `start()`
   promise that owns `_partialSyncInProgress`.
3. **The killed process resolves late.** The Mithril command runner resolves on the child's `close` event
   (`mithrilCommandRunner.ts:190-193`, exit code `null` when signal-killed). For a network download client
   mid-transfer, `close` (process exit + stdio pipe teardown) **lags** the `.kill()` — this is the window the
   operator observed. During it, the UI shows "cancelled / Retry" while `_partialSyncInProgress` is still `true`.
4. **A quick Retry is rejected, then silently swallowed.** Retry reuses the start path (`App.tsx:116` → store
   `startPartialSync`). With the flag still up, the backend rejects at `_assertPartialSyncStartAllowed()` —
   *"Mithril partial sync is already in progress."* (`chainStorageCoordinator.ts:381-383`). The store then
   swallows it: `finally { await this.syncStatus() }` pulls the still-`cancelled` backend status, and the guard
   `if (this.status !== START_PENDING_STATUS) return;` (`MithrilPartialSyncStore.ts:395-397`) returns **without
   surfacing the error** — the optimistic `stopping-node` frame collapses back to the cancelled prompt. Waiting
   lets the process exit, `start()` unwind, the flag clear, and Retry then succeed.

**Note — a second, service-level guard exists but is NOT the lingering blocker:** `cancel()`'s `finally` runs
`_clearRuntimeWorkState()` (`:334`, def `:607-613`) which nulls `_activeWorkDir`/`_currentProcess` promptly, so
`start()`'s own `if (this._activeWorkDir) throw` guard clears fast. The **coordinator `_partialSyncInProgress`**
is the flag that lingers.

→ **Locked path:** the emission of terminal `cancelled` + retry-enabled must be **gated on the in-flight
`start()` having fully unwound**. That invariant is what the fix enforces.

### Fix depth (D-702c-2) — LOCKED = **Full** (correctness race + cleanup UX)

The fix is **Full**: fix the race *and* add the cleanup state, with the state doubling as the gate.
Correctness-only (the coordinator join alone) is strictly less safe — the `cancelling` empty-actions state
(Retry/Restart structurally absent) is the **only** layer robust to the residual join edge cases, so it is
defense-in-depth, not gold-plating — and it leaves the reported "no cleanup state before the cancelled prompt"
UX gap live. Three coordinated changes:

- **(a) A `cancelling` working-status** (D-702c-3) shown "Cleaning up…", carrying **empty
  `allowedRecoveryActions`** so Retry/Restart are structurally absent during teardown — closes the race at the
  **UI** layer.
- **(b) A coordinator-owned backend join** (D-702c-4): emit terminal `cancelled` (retry-enabled) **only after**
  the in-flight `start()` has settled and `_partialSyncInProgress` is `false` — closes the race at the
  **backend** layer.
- **(c) Renderer defense-in-depth** (D-702c-6): stop the silent swallow so a rejected start is never lost.

→ **Locked path:** implement (a)+(b) together (they are the fix); (c) is a cheap independent safety net. All
three land in CAT-A, with the definitive edit shape below (**C-A1..C-A4** are part of the fix, not
options).

### State model (D-702c-3) — LOCKED = new `cancelling` working-status

Add a single **`cancelling`** value to `MithrilPartialSyncStatus`
(`source/common/types/mithril-partial-sync.types.ts:3-15`), scoped to the cancel path only (**not** a generic
shared teardown state). Semantics:

- **It is a working / transitional status, not terminal.** Add it to `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES`
  (`:74-83`) so `isWorking` and the **derived** `MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES` (`:91-96`, spread of the
  working set + non-idle terminal set) include it. **If omitted from the working set, `hasDisplayStatus` is false
  and the overlay HIDES during cleanup (blank screen).** Leave `MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES`
  unchanged (`isMithrilPartialSyncActiveStatus` = `!== 'idle'` already covers it).
- **C-A1 (REQUIRED explicit second edit):** `isMithrilPartialSyncBlockingNodeStart` is a **standalone hardcoded
  array literal** (`:118-131`), **NOT** derived from `WORKING_STATUSES`. Add `'cancelling'` to that literal too —
  otherwise the node-start guard (consumers `MithrilStartupGate.ts:488`, `MithrilController.ts:189`
  `isPartialSyncNodeStartBlocked`) lets the cardano node start mid-cleanup and collapse the "Cleaning up…" frame.
  Add a unit test asserting `isMithrilPartialSyncBlockingNodeStart('cancelling') === true`.
- **Overlay routes it to the progress view.** Add `'cancelling'` to the overlay's `PROGRESS_STATUSES`
  (`MithrilPartialSyncOverlay.tsx:42-52`) so it renders `MithrilProgressView` (spinner) not `MithrilErrorView`,
  and to the `hideAction` set (`:210-215`) so no Cancel button shows during cleanup.
- **C-A4 (REQUIRED):** in `MithrilProgressView.tsx` add a `cancelling` special-case ("Cleaning up…") beside the
  `stopping-node`/`starting-node` cases (`:138-139`), **and suppress the step-indicator waterfall and the elapsed
  timer** for `cancelling`. Both render **unconditionally** today (elapsed `timerDisplay` `:184-189`;
  `waterfallContainer` wrapping `MithrilStepIndicator` `:197-209`); `cancelling` maps to `getActiveStepIndex` = −1
  and the store does not re-anchor `startedAt` on a working→working transition, so the default frame would show a
  reset waterfall + a timer still ticking from download start. Use an indeterminate spinner, no progress bar.
- **Empty recovery actions during `cancelling`.** `allowedRecoveryActions: []` ⇒ `canRetry`/`canRestartNormally`
  are false (`MithrilPartialSyncStore.ts:170-178`) ⇒ no action buttons until terminal.
- **Terminal `cancelled` is unchanged** — still emitted with `['retry','restart-normal']` (D-702a-2 removed wipe
  from the pre-cutover cancelled dialogue; do **not** reintroduce it).
- **Copy (vocab guardrail #8):** user-facing "Cleaning up…" — never "partial sync". New i18n keys in EN + JA
  (JA best-effort per the 601 glossary), mirroring the bootstrap flow's existing cleanup label under the
  partial-sync namespace.

**Domain note:** the implemented enum has neither `confirming` nor `cancelling`; the PRD contract
(`prd.md:147-160`) lists a phantom `confirming` (`:149`) the enum never had. Adding `cancelling` is a
**sanctioned additive extension** (an ADR, like D-702a-1/-2), not a regression. Cancel remains
**Boundary-A-only** (`prd.md:190`); `cancelling` is a Boundary-A transitional state cleaning only
staged/downloaded files (`prd.md:192`).

→ **Locked path:** the single `cancelling` value + the C-A1/C-A4 wiring above. Update the PRD status contract
(`:147-160`) to add `cancelling` **and reconcile the phantom `confirming`** in the same edit, so doc and code
agree.

### Join mechanism (D-702c-4) — LOCKED

Restructure the cancel path so terminal `cancelled` is emitted only after the in-flight run has joined. Keep the
**coordinator as the orchestrator** and the **service as the executor** (existing layering).

**Coordinator (`chainStorageCoordinator.ts`):**
- Add a field `_partialSyncRunPromise: Promise<void> | null = null` (verified: no such field exists yet).
- In `startPartialSync` (`:220-272`), capture the post-preflight run as a promise so cancel can join it. The
  captured promise **must be the IIFE wrapping the WHOLE `try/finally { _partialSyncInProgress = false }`** — not
  the inner `handlers.start()` promise (awaiting the inner promise races the coordinator's own `finally`
  continuation, so `finalizeCancel` could run with the flag still true):
  ```ts
  const preflightContext = await this._withMutationLock('startPartialSync', /* unchanged; sets flag */);
  const runPromise = (async () => {
    try { await dependencies.handlers.start(preflightContext); }
    finally { this._partialSyncInProgress = false; this._partialSyncRunPromise = null; }
  })();
  this._partialSyncRunPromise = runPromise;   // assign synchronously so cancelPartialSync observes it
  await runPromise;
  ```
- Rewrite `cancelPartialSync` (`:274-278`) as an ordered sequence that **gates the retry-enabled terminal on the
  join having actually settled**:
  ```ts
  await dependencies.handlers.cancel();                          // service: emit 'cancelling', kill, STOP
  const run = this._partialSyncRunPromise;
  const settled = run ? await this._awaitRunSettledBounded(run) : true;  // join (D-702c-5); returns whether it settled
  if (settled) {
    await dependencies.handlers.finalizeCancel();               // flag proven false: cleanup + terminal cancelled/failed (retry OK)
  } else {
    await dependencies.handlers.abandonCancel();                // flag maybe still true: non-retryable failed, NO cleanup, NO retry
  }
  ```
  On the **settled** branch, because `runPromise` resolves *after* its own `finally` runs, the join guarantees
  `_partialSyncInProgress === false` before `finalizeCancel` advertises Retry (robust — does not rely on microtask
  registration order). On the **no-settle** branch the flag may still be `true`, so a retry-enabled terminal must NOT
  fire — hence `abandonCancel` (D-702c-5).

**Service (`MithrilPartialSyncService.ts`):**
- Split `cancel()` (`:276-336`): keep the **no-op branch** (`:280-291`, gap #39 — must NOT emit `cancelling`) and
  the **Boundary throw** (`:293-297`, `installing`/`finalizing`). In the active branch: **emit `cancelling`** at
  entry (`allowedRecoveryActions: []`, `transferProgress: {}`, a "cleaning up" progress item), set `_isCancelled`,
  kill the process (`:303`) — and **STOP** (no cleanup, no terminal emission).
- **C-A2 (REQUIRED):** move **both** the destructive `_cleanupPartialSyncArtifacts()` (today `:312`) **and** the
  `failed`-on-cleanup-error terminal (today `:324-331`, inline in `cancel()`) **out** of `cancel()` and into
  `finalizeCancel()`. As-is, that `failed` branch emits a terminal with `['retry','restart-normal']` **before**
  the coordinator join clears `_partialSyncInProgress` (re-opening the exact race via `failed`) and throws before
  the coordinator ever reaches `finalizeCancel`. `cancel()` must therefore emit **no** terminal.
- **C-A3 (REQUIRED):** do **NOT** run `_clearRuntimeWorkState()` in `cancel()`'s finally (today `:334`) — it nulls
  `_currentProcess`, making `forceKill()` a silent no-op. `_currentProcess` must survive until the bounded join
  has (possibly) escalated to SIGKILL. Move runtime-state clearing into `finalizeCancel`.
- Add **`finalizeCancel()`** — mirror `finalizeWipeAndFullSync` (`:360-364`), **guarded on
  `status === 'cancelling'`** (no-op otherwise; idempotent like `finalizeCompletedPartialSync` `:366-377`). It is
  invoked **only on the settled branch** (`_awaitRunSettledBounded` returned `true`), so the flag is **confirmed
  false** when it runs — note the `status` guard does **not** itself check the flag, so this ordering, not the guard,
  is what makes advertising Retry safe: `await _cleanupPartialSyncArtifacts()` → on success emit terminal `cancelled`
  with `['retry','restart-normal']`; on cleanup error emit terminal `failed` with `['retry','restart-normal']` (the
  old inline `failed` behavior, now correctly sequenced). Run `_clearRuntimeWorkState()` here.
- Add **`forceKill()`** (`_currentProcess?.kill('SIGKILL')`) for the bounded-join escalation — necessary because
  `cancel()`'s `.kill()` is Node's default SIGTERM (`:303`, no explicit signal arg) and a SIGTERM-ignoring child
  would never emit `close`, leaving the run promise pending forever.
- Add **`abandonCancel()`** — the **non-retryable operational-failure** floor for the pathological no-settle branch
  (SIGKILL did not yield `close` within the bound), **guarded on `status === 'cancelling'`** (idempotent no-op
  otherwise). Emit terminal `failed` with `allowedRecoveryActions` that **exclude `'retry'`** (restart-only, e.g.
  `['restart-normal']`) and a distinct "couldn't finish cleaning up — restart Daedalus" detail; **do NOT** run
  `_cleanupPartialSyncArtifacts()` (the possibly-still-live process must not race `fs.remove`). It is reached only
  when `_partialSyncInProgress` may still be `true`, so it must never advertise Retry.

**Controller (`MithrilController.ts`):** add `finalizeCancel`, `forceKill`, and `abandonCancel` to the handlers
object returned by `_getPartialSyncDependencies` (`:439-452`), next to `finalizeWipeAndFullSync` (`:446-447`); extend
the `PartialSyncHandlers` type in `chainStorageCoordinator.ts`.

→ **Locked path:** coordinator-owned join via `_partialSyncRunPromise` (IIFE around the whole try/finally) + a
`settled`-returning bounded join + guarded `finalizeCancel` (settled branch; **owns cleanup + the terminal emit**,
C-A2) + `abandonCancel` (no-settle branch; **non-retryable** floor, no cleanup) + `forceKill` for escalation. Cleanup
is preserved on the no-op branch by the `if (run)` + `status === 'cancelling'` guards making the join/finalize
safe no-ops there.

### Anti-wedge / bounded join (D-702c-5) — LOCKED

The join introduces a hang risk: if the killed download **ignores SIGTERM**, `close` never fires and the user is
stranded in `cancelling`. Bound it (same philosophy as 702b D-702b-6's anti-pin request timeout):

`_awaitRunSettledBounded(run)` **returns a `settled` boolean**: race `run.catch(() => {})` against
`PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS`. On timeout, escalate — call `handlers.forceKill()` (SIGKILL), then **re-await
`run.catch(() => {})` once more (short second bound)** (SIGKILL guarantees `close`, so this normally settles at once).
Return `true` once the run settles (its `finally` has run ⇒ `_partialSyncInProgress` is now false), or `false` if it
is **still pending** after the SIGKILL bound. Always clear the timer.

**`cancelPartialSync` gates the retry-enabled terminal on `settled` — this replaces the earlier "proceed anyway"
floor, which was unsound:** `if (settled) await handlers.finalizeCancel()` **else** `await handlers.abandonCancel()`.
The old floor claimed the `cancelling` empty-actions state kept Retry hidden even when the run never settled — but
that is **self-contradictory**: `finalizeCancel` emits a terminal `cancelled`/`failed` carrying
`['retry','restart-normal']`, and its **only** guard is `status === 'cancelling'` (which still passes in the
no-settle branch), so proceeding would **overwrite** the empty-actions state and re-advertise Retry while
`_partialSyncInProgress` may still be `true` — re-opening the exact race (principle (b) above requires the
retry-enabled terminal fire *only after* the guard clears). So the no-settle branch instead routes to
**`abandonCancel()`**: a **non-retryable operational-failure** floor — terminal `failed`, `allowedRecoveryActions`
**excluding `retry`** (restart-only), and **no** destructive cleanup (the possibly-still-live process must not race
`fs.remove`). Retry is thereby never offered while the guard could still be set, and the user gets an actionable
restart terminal rather than a wedged spinner. (Verify the surviving `restart-normal` does not itself re-enter
`startPartialSync` against the still-set guard; if it does, degrade to a bare restart message.)

- **Timeout value:** **15000 ms** (network-client teardown can take several seconds; headroom before SIGKILL).
  No IPC-layer timeout rejects the ~15 s cancel request first (`IpcConversation.request` `:65-96` has none, so the
  request stays pending safely). Operator-tunable during QA (U1) — a bounded fail-safe, not the happy path.
- **Cleanup slowness (accepted residual):** `_cleanupPartialSyncArtifacts()` (`fs.remove` of the staging tree) can
  be slow on large ranges; `cancelling` uses an indeterminate spinner, no progress bar. The pre-join `await
  cancel()` is not itself timeout-bounded, but with C-A2 moving cleanup into `finalizeCancel` (after the bounded
  join) the wedge surface shrinks to a hung `fs.remove`; document a hung-cleanup watchdog/log as accepted residual.

→ **Locked path:** bounded join with SIGKILL escalation + a **`settled`-gated** floor — settled →
`finalizeCancel` (retry-enabled), no-settle → `abandonCancel` (**non-retryable**, `retry` excluded, no cleanup);
never re-advertise Retry while the guard may still be set. 15 s default; verify real teardown latency in operator
QA (U1).

### Renderer defense-in-depth (D-702c-6) — LOCKED (secondary)

Independent of the backend fix: the store's silent swallow (`MithrilPartialSyncStore.ts:395-397`) means a rejected
start is lost whenever the resync pulls a non-`stopping-node` status. With the `cancelling` gate (Retry hidden
during cleanup) the *primary* race can no longer be user-triggered, so this is belt-and-suspenders — but cheap.

**Minimal change:** when `startError` is set and the status is not the optimistic pending frame, `logger.warn` the
swallowed rejection (today it vanishes with no trace) in `startPartialSync` (`:368-400`). Do **not** change the
happy-path optimistic-frame behavior. `cancelPartialSync` (`:402-411`) needs no logic change — the
`cancelling`→`cancelled` status pushes drive the UI; `canRetry`/`canRestartNormally` (`:170-178`) are already false
during `cancelling`.

→ **Locked path:** land the log-at-minimum change in CAT-A; "surface to UI" is optional polish, not a blocker.

---

## CAT-B — overlay backdrop bleed (D-702c-8) — LOCKED

**Root cause (confirmed):** the app-level `MithrilPartialSyncOverlay` shares the reference bootstrap stylesheet
(`MithrilPartialSyncOverlay.tsx:18` `import styles from './MithrilBootstrap.scss'`) and its single `.backdrop`
rule (`MithrilBootstrap.scss:10-16`) is **translucent** (theme token alpha 0.92 × element `opacity: 0.97` ≈ 0.89
effective ⇒ ~11 % bleed) **and carries no `backdrop-filter`**. Because the overlay mounts as a sibling of
`<Router>` (`App.tsx:98-99`, gated by `shouldShowOverlay`, store getter `MithrilPartialSyncStore.ts:166-168`), it
layers over whichever route is live — during partial sync that is the animated `SyncingConnectingPage`
(`LoadingPage.tsx:82`) — which shows through the faint, unblurred scrim and reads as unfinished.

**Fix = shared-rule blur, keep the tint token.** In `MithrilBootstrap.scss` add
`@import '../../../themes/mixins/overlay-backdrop';` at the top (import-depth precedent
`MithrilStepIndicator.scss:3`) and `@include overlay-backrop;` inside `.backdrop` (`:10-16`). This reuses the
exact app-wide frosted-glass convention — the `overlay-backrop` mixin (`themes/mixins/overlay-backdrop.scss:1-2`
= `backdrop-filter: blur(5px)`; note the typo'd name) is already consumed by **six** overlays
(`AppUpdateOverlay.scss:19`, `AlertsOverlay.scss:17`, `RTSFlagsRecommendationOverlay.scss:20`,
`IncidentOverlay.scss:17`, `ModalOverrides.scss:5`, `DaedalusDiagnosticsDialog.scss:12`; `AboutDialog.scss:1`
imports it dead). Also set **`opacity: 1;`** on `.backdrop` (still translucent via the token's 0.92 alpha) to firm
the scrim and avoid the opacity-group compositing nuance. Keep the `--theme-mithril-overlay-backdrop-start` tint;
do **not** touch the per-theme 0.92 alphas.

**Notes:** (1) the reference overlays put blur on the same viewport-sized element that carries the tint; here it
lands on the separate `.backdrop` child — functionally equivalent (`backdrop-filter` samples what paints behind
`.backdrop`; `.content`/`.card` paint after and stay crisp) — worth a one-line reviewer note. (2) Bootstrap is
provably unaffected: `LoadingPage.tsx:72-77` **replaces** (not layers) the syncing screen with `MithrilBootstrapPage`
over a solid `CenteredLayout`, so nothing busy renders behind the bootstrap card; `opacity 0.97→1` firms the
bootstrap tint too (effective α ~0.89→0.92) — acceptance is **"slightly-firmer bootstrap tint is acceptable,"** not
"unchanged." (3) The sibling token `--theme-mithril-overlay-backdrop-end` is currently dead (only `-start` is
consumed) — a future gradient opportunity, out of scope. (4) `MithrilBootstrap.scss` has **three** importers
(`MithrilBootstrap.tsx`, `MithrilPartialSyncOverlay.tsx`, and the storybook `LoadingOverlayStoryFrame`).

**Rejected:** per-theme alpha edits (~10-file blast radius, unjustified once blur lands); an overlay-only divergent
class (violates the align-to-reference guardrail).

→ **Locked path:** add the existing blur mixin + `opacity: 1` to the single shared `.backdrop` rule; keep the tint
token; styling-only, vocab-neutral. **Proof is the on-device manual test** (bleed over a live `SyncingConnectingPage`)
— Storybook is **not** valid proof here: its frame renders `.backdrop` over a solid canvas, so `blur(5px)` is
imperceptible and the story looks identical pre/post-fix (add a story variant with animated placeholder content
*behind* the frame if Storybook coverage is wanted). Elevate **U2** (blurring continuously-animated content every
frame — a new GPU profile; all six existing consumers sit over static content) to a required low-end/Linux jank check.

---

## CAT-C — diagnostics option first-load availability (D-702c-9) — LOCKED = keep AND-gate + backend follow-up

**Root cause (confirmed):** the Diagnostics option gates on
`isMithrilPartialSyncEnabled && isMithrilPartialSyncSignificantlyBehind` (`DaedalusDiagnostics.tsx:724-727`,
section wrapper `:720`). `isSignificantlyBehind` is the backend probe
(`MithrilPartialSyncService.getPartialSyncBehindness` `:727-759`), which needs a local immutable position; on
first-load-after-genesis the `immutable/` dir has no parseable chunk, so `resolveLocalImmutableNumber` throws
`PARTIAL_SYNC_IMMUTABLE_POSITION_UNAVAILABLE` (`mithrilPartialSyncPreflight.ts:132-138`) and the probe fails safe to
`{ isSignificantlyBehind: false }` (`:751-758`). A throw never populates the local-read cache (only success does,
`:706-709`); and because not-behind makes availability "stable" (`MithrilPartialSyncStore.ts:245`) the poll actively
**backs off from 30 s to 5 min** (`:279-284`) — so the option appears to "need a restart" (a restart resets the fast
cadence *and* coincides with immutable files finally existing — temporally correlated, not causally required). The
same observable feeds the proactive-prompt gate (`MithrilProactivePromptContainer.tsx:70`), so both entry points
share the blackout.

**Fix = keep the AND-gate; NO renderer change.** CAT-C ships **no renderer gate edit** — the Diagnostics option
stays coupled to `isSignificantlyBehind` (parity with the proactive prompt; the "shared blackout" is thereby
*preserved*, which is correct — see below). Its only in-wave work is **verify-only** assertions.

**Rejected — the figure-based OR-gate `isEnabled && (isSignificantlyBehind || behindByEpochs !== undefined)` is
unsound (do not re-propose):**
- `computeBehindByEpochs` (`utils/mithrilBehindness.ts:45-66`) anchors on the **live** `networkTip.epoch`; the
  backend probe measures the gap to the **lagging certified snapshot** (`MithrilPartialSyncService.ts:738-747`,
  ~20-immutable threshold). A node synced up to the latest certified snapshot but ≥1 epoch behind the live tip has
  `isSignificantlyBehind = false` yet `behindByEpochs ≥ 1` ⇒ the OR-gate would offer a partial sync that
  **dead-ends** in `PARTIAL_SYNC_NO_CERTIFIED_RANGE` (`mithrilPartialSyncPreflight.ts:148-154`) — same on first-load
  (`IMMUTABLE_POSITION_UNAVAILABLE`) and transient aggregator failure. `isSignificantlyBehind` is the **sole offer
  signal** by locked boundary #4 (documented in `mithrilBehindness.ts:13-15`); the OR-gate violates it.
- **Session regression:** a premature Start burns `mithrilAttemptStartedThisSession`
  (`MithrilPartialSyncStore.ts:373`, set unconditionally, **never reset**); the proactive prompt ANDs
  `!mithrilAttemptStartedThisSession` (`MithrilProactivePromptContainer.tsx:73`), so the **legitimate proactive
  prompt would be suppressed for the rest of the session** — the OR-gate is *not* decoupling-safe.
- It breaks `DaedalusDiagnostics.spec.tsx:119-139` (default tips 100/101 ⇒ `behindByEpochs = 1`; asserts the button
  is null when `significantlyBehind = false`), and the resulting dead-click can be **silent** (the store's
  post-`syncStatus()` guard returns without rethrow).

**Backend follow-up (separate task, NOT this wave):** the first-load-availability gap is real but the only honest
fix is **backend-side** — have `getPartialSyncBehindness` distinguish **"unknown/unavailable"** from **"not behind"**
so the renderer can offer the option only when a partial sync is actually fulfillable. That touches **locked
offer-signal boundary #4**, so it must be scoped and grilled on its own — file it as a separate follow-up task; do
**NOT** fold it into 702c.

→ **Locked path:** leave `shouldShowRecommendation` (`:724-727`) coupled to
`isMithrilPartialSyncEnabled && isMithrilPartialSyncSignificantlyBehind`; no gate/prop/plumbing/i18n/SCSS change.
`behindByEpochs` (computed `:573`, passed `:728`) stays **display-only** (confirmation copy; the CTA gate is
`MithrilPartialSyncSection.tsx:130-132`, on `shouldShowRecommendation` alone). Verify-only: proactive suppression
(`MithrilProactivePromptContainer.tsx:67-74,88`) and the confirm-dialog copy stay intact; record the first-load
window duration (U3) as context for the backend follow-up.

---

## CAT-D — proactive prompt chrome re-alignment (D-702c-10) — LOCKED

**Root cause (confirmed):** `SyncingConnectingMithrilPrompt` was built as a bespoke compact card
(`SyncingConnectingMithrilPrompt.scss:1-16`) rather than reusing the bootstrap reference chrome, diverging on four
axes: (a) **no title** (choice view opens on a body paragraph, `tsx:142-183`); (b) **center-aligned** text/actions
(`.component text-align: center` `scss:12`; `.actions justify-content: center` `scss:51-55`); (c) **two split
paragraphs** — `.body` 16px (`scss:18-24`) + `.benefit` 15px (`scss:26-32`); (d) a **double-dimmed** note —
`.handoffNote` 13px `--theme-mithril-secondary-text-color` **plus** `opacity: 0.8` (`scss:34-41`), no "Note:" prefix.
The fast button uses the app-wide `'primary'` class (`tsx:174-179`); the Standard button (`tsx:165-170`) has only
`styles.actionButton`. **The reported "blur" is a MISDIAGNOSIS** — there is no `backdrop-filter`/blur anywhere on this
prompt or the syncing screen behind it (U4); the readability problem is the double-dim + centering + low-contrast copy.

**Fix = full re-align to the MithrilBootstrap / MithrilErrorView reference:**
1. **Title:** add an i18n key + a title element ("Mithril Sync") before the body. Prefer a semantic `<h1>` for
   a11y/parity with the reference. `.title` = `--theme-mithril-heading-color` / `var(--font-medium)` / **18px** /
   `margin: 0 0 8px` (compact-card size; the reference h1 is 22px in a full-viewport dialog).
2. **Merge sentences:** render sentence 1 (`promptBody`/`promptBodyUnknown`) and sentence 2 (`promptBodyBenefit`) as
   **sibling inline `<span>`s inside one `<p className={styles.body}>`**, with an explicit `{' '}` between them
   (RTL's normalizer hides a missing space from the spec but not from the eye). This preserves the epochs/Unknown
   branch (`tsx:152-156`) **and** keeps each sentence individually queryable (spec note below). Delete the separate
   `.benefit` paragraph; set `.body` = `--theme-mithril-body-text-color` / `var(--font-regular)` / 15px /
   line-height 1.5 / `text-align: left`.
3. **Note:** put **"Note:" in its own element** (e.g. a `<strong>` span — **not** in-copy, which would mutate the
   exact string the spec asserts), remove `opacity: 0.8`, keep 13px `--theme-mithril-secondary-text-color`. Reference
   is `MithrilProgressView.scss:54-55 .reassurance` (**not** `.errorHint`, which uses the body-text token).
4. **Alignment:** remove `text-align: center` from `.component` (`scss:12`) — **left-align both views** (see D-702c-13).
5. **Buttons:** swap `'primary'` → `styles.primaryAction` on the fast button (`tsx:175`) and
   `styles.actionButton` → `styles.secondaryAction` on the Standard button (`tsx:166`). **Copy
   `.primaryAction`/`.secondaryAction` verbatim from `MithrilErrorView.scss:85-111`** — they do **not** exist in
   `SyncingConnectingMithrilPrompt.scss` (referencing them without copying yields `undefined`/unstyled buttons) —
   and **augment the copied `.primaryAction` with `&[disabled]` (+ `:focus`)** (verbatim copy drops the
   disabled/focus affordance the confirm-view Start button needs). Regenerate `SyncingConnectingMithrilPrompt.scss.d.ts`.
6. **Actions:** `justify-content: flex-end; gap: 12px; ` and keep `min-width: 180px` on the buttons (non-optional —
   `.primaryAction`/`.secondaryAction` set no spacing and `.actions` has no gap, so the buttons otherwise touch).
   `flex-end` is a **design choice** (the reference base `.actions` is `flex-start` `MithrilErrorView.scss:78`;
   `flex-end` is the `.actionsRightAligned` modifier `:81-83`).

**Critical spec note:** the sentence merge breaks exact-text queries. `SyncingConnectingMithrilPrompt.spec.tsx:46-52`
queries each sentence exactly, and `MithrilProactivePromptContainer.spec.tsx` uses exact `getByText` (the
`KNOWN_EPOCHS_TEXT` constant def `:13`, positive queries `:65`/`:159`, plus a hardcoded literal at `:148`). Sibling
inline `<span>`s inside one styled `<p>` keep each individually queryable; **add
`MithrilProactivePromptContainer.spec.tsx` to filesToTouch.** Keep the note assertion (`:69-73`) working by putting
"Note:" in its own element, and keep the `/partial sync/i` + `/immutable/i` negative guards (`:53-54`).

**Cross-theme note:** switching the fast button off `'primary'` changes its hue only where
`--theme-mithril-button-primary-background` ≠ `--theme-button-primary-background-color` — visually neutral in default
cardano (#2cbb69 == #2cbb69), shifts to mithril-teal in light-blue. Intended per reference.

→ **Locked path:** full re-align (title + merged left-aligned body + de-dimmed "Note:" element +
`.primaryAction`/`.secondaryAction` buttons + right-aligned actions), sentences as sibling spans. Title size 18px;
fast button `.primaryAction` (teal in light-blue). Vocab: title "Mithril Sync", epochs-only, never "partial sync".

---

## CAT-E — process ("confirm") prompt title + button chrome (D-702c-11) — LOCKED (merge with CAT-D)

**Root cause (confirmed):** `renderConfirmView()` (`SyncingConnectingMithrilPrompt.tsx:189-211`, returned JSX block) is a bare body +
buttons card with **no `.header`/h1** — it opens straight on `<p className={styles.confirmBody}>` rendering the
shared `mithrilSyncProcessSummary` key (imported from `../../status/MithrilSyncProcessSummary.messages`, values
`:14-20`; shared byte-for-byte with the diagnostics modal `MithrilPartialSyncConfirmation.tsx:102-106`). Its buttons
hand-roll the global `'primary'` + a font-less `styles.actionButton` (`scss:57-60`). The reviewer's literal
complaint ("button fonts differ from the proactive prompt") is **not reproducible** — choice and confirm views use
byte-identical button classing; the actionable intent is "align this prompt to the bootstrap pattern." Net divergence
vs reference: the secondary button renders Medium where bootstrap secondary is Regular
(`MithrilErrorView.scss:103`), and shape/color diverge.

**Fix = title + `.primaryAction`/`.secondaryAction` on BOTH views (merged with CAT-D):**
1. Add a title element ("Mithril Sync Process") to `renderConfirmView()` before the `confirmBody` paragraph; keep
   the shared `processSummary` as the body. Reuse the **left-aligned** `.title` added in CAT-D (see D-702c-13 —
   do **not** center it; that guidance is stale once CAT-D removes `text-align:center`). Set `.confirmBody` (and the
   lifted dialog error, below) to `text-align: left`.
2. Swap the classNames on **both** the confirm view (Start `:203` → `styles.primaryAction`, Cancel `:197` →
   `styles.secondaryAction`) **and** the choice view (they share identical classing — fixing one alone creates a new
   intra-component divergence). Keep `min-width: 180px` and the `.actions` `gap: 12px` (both required — else the
   buttons touch). The confirm-view Start button is `disabled={isStarting}`, so the `&[disabled]` augmentation on the
   copied `.primaryAction` (CAT-D step 5) is required for it.
3. **i18n:** add a **new local title key** via `defineMessages` (e.g.
   `daedalus.diagnostics.dialog.mithrilProactivePromptConfirmTitle = "Mithril Sync Process"`) in en-US + ja-JP; run
   `yarn i18n:manage` (defaultMessages.json is generated — do **not** hand-edit). Do **not** fork the shared
   `mithrilSyncProcessSummary` key (`MithrilSyncProcessSummary.messages.ts:11` warns against redeclaring).
   *(Optional DRY: reuse the already-translated `mithrilPartialSyncConfirmationTitle` "Before Mithril Sync begins"
   instead of minting a third title — implementer's call.)*
4. If normalizing `.confirmBody` to 15px/body-text-color for parity, note the choice-view `.body` is already 15px (per
   CAT-D); do not leave `.confirmBody` at 16px — 16px is only the pre-edit base state, never a valid post-edit outcome.

**Cross-surface copy note:** this confirm view shows "Mithril Sync Process" while the diagnostics modal shows "Before
Mithril Sync begins" (`MithrilPartialSyncConfirmation.tsx:12`) over the **same** body key — two context-appropriate
titles; accept the intentional divergence (do not unify — out of scope).

→ **Locked path:** title + full button-token swap on both views, **merged with CAT-D** (D-702c-13). Reuse only
existing `--theme-mithril-*` / `--font-*` tokens. Vocab-compliant ("Mithril Sync Process", never "partial sync").

## CAT-D + CAT-E share the same prompt Buttons/SCSS (D-702c-13) — LOCKED = one merged edit

CAT-D and CAT-E both edit `SyncingConnectingMithrilPrompt.tsx` + `.scss` and both consume the same
`.primaryAction`/`.secondaryAction` rules (choice view for CAT-D, confirm view for CAT-E). Landing them as two
subagents risks conflicting edits and a half-applied button treatment (one view teal, the other app-blue).

→ **Locked path:** treat CAT-D + CAT-E as a **single coordinated edit** — add `.primaryAction`/`.secondaryAction`/
`.title` **once** (copied from `MithrilErrorView.scss`, `.primaryAction` augmented with `[disabled]`/`:focus`), apply
to both views, add both titles, **left-align both views**, merge the choice-view sentences, de-dim the "Note:"
element — with **one** spec update covering both views (`SyncingConnectingMithrilPrompt.spec.tsx` +
`MithrilProactivePromptContainer.spec.tsx`), one `.scss.d.ts` regen, and one extension of the existing
`storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` (which already renders the choice view —
extend it for the confirm view; do **not** create a new file).

---

## CAT-F — "Before Mithril Sync begins" dialog dead CSS (D-702c-12) — LOCKED

**Root cause (confirmed — scope mismatch between where the CSS lives and where the DOM lives):** the confirmation
dialog's classes are authored as **descendants of `.component`** — all `.mithrilPartialSyncConfirmation*` rules sit
between `.component {` (`DaedalusDiagnostics.scss:28`) and its close (`:481`): Body `:344`, Actions `:356-367`,
CancelButton `:369-371`, Dialog `:373`, Behind `:377`, Recovery `:386`, PrimaryButton `:395`, SecondaryButton `:399`
(plus the shared `.error` `:180`) — compiling to `.component .mithrilPartialSyncConfirmation*` descendant selectors.
But the shared `Dialog` is a react-polymorph `Modal` → react-modal that **portals to `document.body`** (react-modal
`Modal.js` default `parentSelector = document.body` `:216-217`, `appendChild`s the portal `:93`). Although
`MithrilPartialSyncSection` is a **React-tree** descendant of `.component`, descendant selectors match the **DOM
tree**, where the portaled paragraphs are siblings of `.component` — so the rules never fire. The paragraphs fall
back to the global Meyer reset (`themes/index.global.scss:21,92-93` `p { margin:0 }`), so P1/P2 touch with only
line-height between them and the intended font hierarchy, 560px width, and 180px buttons are all dead. Confirmed
empirically by `MithrilPartialSyncConfirmation.spec.tsx:36` asserting a real `.ReactModal__Overlay`.

**Fix = un-nest to flat top-level selectors + a deliberate single-sided gap:**
- **Lift** `.mithrilPartialSyncConfirmationDialog` (`:373`), `Body` (`:344`), `Behind` (`:377`), `Recovery` (`:386`),
  `PrimaryButton` (`:395`), `SecondaryButton` (`:399`) **out** of the `.component { }` block (`:28-481`) to flat
  top-level selectors, mirroring `ExportWalletToFileDialog.scss` (flat content classes; `styles.*` passed as the
  Dialog className — the markup already applies them, `MithrilPartialSyncConfirmation.tsx:72`). Keep existing tokens
  (`--theme-dialog-*`, `var(--font-regular)`, `var(--font-medium)`); do **not** import mithril-card vars.
- **Also lift a dialog-scoped error class + swap one TSX classname.** The `.error` at `:180` is dead the same
  descendant-scoped way, and the confirmation's start-failure path renders it (`MithrilPartialSyncConfirmation.tsx:108`)
  — so leaving it closes only **6 of 7** dead rules. `.error` is shared with the live in-`.component` diagnostics
  table (`DaedalusDiagnostics.tsx:702`), so it can't be lifted as-is: add a **new** flat
  `.mithrilPartialSyncConfirmationError` (using `--theme-dialog-*`, **not** the dark `--theme-network-window-red-color`)
  and swap `styles.error` → `styles.mithrilPartialSyncConfirmationError` at `:108` (**one-line TSX change** — CAT-F is
  **not** "no TSX change").
- **Gap:** `.mithrilPartialSyncConfirmationBody { p { margin: 0 } p + p { margin-top: 20px } }` and **drop the
  `margin` declarations from `.Behind`/`.Recovery`** (keep only their font/color). The `p + p` rule under `.Body`
  (specificity 0,1,2) beats the reset and a leftover per-element margin, and single-sided avoids margin-collapsing;
  20px matches `Dialog.scss:53-56` (`.contentWithActions .content` margin-bottom). Keep P1 emphasised
  (font-medium, title-color) and P2 de-emphasised (14px).
- **DROP the wrapper `.Body` `opacity: 0.85`** — it compounds with `.Recovery` `opacity: 0.7` to ~**0.6 effective**
  on P2 once the rules activate, *fainter* than today's full-opacity fallback (a contrast regression). Express P2
  de-emphasis via a single opacity/color-alpha, not stacked opacity.
- **Delete** the confirmed-unused `.mithrilPartialSyncConfirmationActions` (`:356-367`) and
  `.mithrilPartialSyncConfirmationCancelButton` (`:369-371`) — referenced only by the generated `.scss.d.ts`, never
  any `.tsx`. Regenerate `DaedalusDiagnostics.scss.d.ts` (drop the removed exports). Move/repair the stale in-file
  "restyled IN PLACE" comment (`:383-385`), which describes a currently-dead rule.

**Whole-dialog regression note:** un-nesting **activates several previously-dead rules at once** (width 580→560px,
180px buttons, P1 font-medium/title-color, P2 14px). Treat the **entire dialog** as the visual-regression surface and
QA it in Storybook for both `behindByEpochs` set **and** the `behindUnknown` fallback, **light and dark**. Jest cannot
prove the CSS fix (SCSS maps through identity-obj-proxy); it guards only the markup contract (two `<p>`, correct class
names, real `.ReactModal__Overlay`).

**Rejected:** in-place value edits (rules stay dead); a dedicated `MithrilPartialSyncConfirmation.scss` (fine
long-term cleanup, out of scope — log as follow-up).

→ **Locked path:** un-nest to flat selectors, `p + p { margin-top: 20px }` (drop the collapsing Behind/Recovery
margins), lift the dialog error class + swap `:108`, drop the `.Body` opacity:0.85, delete the two dead classes,
regen the `.scss.d.ts`, re-verify the whole dialog in Storybook. Styling + one-line TSX; vocab-neutral.

---

## Locked safety boundaries (honor while reasoning)
1. **Cancel is Boundary-A-only** (`prd.md:190`); `cancelling` is a Boundary-A transitional state cleaning only
   staged artifacts (`prd.md:192`). No cancel after cutover; `installing`/`finalizing` still throw
   (`MithrilPartialSyncService.ts:293-297`).
2. **Terminal `cancelled` recovery set stays `['retry','restart-normal']`** (D-702a-2; no wipe).
3. **Kill switch #1/#2, staged-only #3, latest-snapshot-only #5, no bootstrap regression #7, vocab #8** — all
   carried unchanged from `task-ux-702.md:112-127`.
4. **`cancelling` must never wedge, and Retry must never be re-advertised while the in-progress guard may still be
   set** — bounded join + SIGKILL, `settled`-gated (settled → `finalizeCancel`; no-settle → `abandonCancel`, a
   **non-retryable** floor with `retry` excluded and no cleanup) (D-702c-5); a *cleanup* error on the settled branch
   routes to the `failed` terminal with recovery actions (safe — the guard is already clear).
5. **Backend offer signal is untouched (#4).** `isSignificantlyBehind` remains the sole offer signal; CAT-C makes
   no renderer or backend change (the probe-hardening is a separate follow-up). Proactive-prompt *suppression* logic
   is untouched. Every CAT-B..CAT-F fix reuses an existing token/mixin/reference — no new colors/classes/theme vars.
   No `cancelling`/state-machine or PRD status-contract change from CAT-B..CAT-F (they are presentation-only).
   Vocab guardrail #8 holds (titles "Mithril Sync" / "Mithril Sync Process"; "Cleaning up…"; epochs-only; never
   "partial sync"/%/immutable).

---

## Unknowns / verification (operator QA — no product decision pending)
- **U1 — SIGTERM compliance + teardown latency.** How long does the download client take to hit `close` after
  `.kill()` mid-download on preprod/Linux, and does it ever ignore SIGTERM? Sizes `PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS`
  (15 s default) and validates the SIGKILL escalation. → start partial sync, cancel mid-download, observe "Cleaning
  up…" duration and that immediate Retry works; note worst-case teardown time.
- **U2 — backdrop-filter GPU cost (CAT-B).** `blur(5px)` over the continuously-animated `MithrilProgressView` /
  `SyncingConnectingPage` is a new GPU profile (all six existing consumers sit over static content). → required
  low-end/Linux jank check during downloading/cleanup frames.
- **U3 — first-load window duration (CAT-C).** Wall-clock length of the first-load blackout is not determinable from
  source; not needed for the locked fix (no renderer change), but record as context for the backend follow-up.
- **U4 — perceived "blur" on the proactive prompt (CAT-D).** No CSS blur exists on that surface (confirmed
  misdiagnosis); the "blur" is the double-dimmed low-contrast copy. → confirm the de-dim + left-align + merge resolves
  the readability complaint in operator re-test.

**Regression guards (covered by unit tests):** the no-op cancel branch (D5f / gap #39) is preserved by the `if (run)`
+ `status === 'cancelling'` guards; the Boundary invariant (post-cutover cancel-rejection) is unchanged; add two
coordinator units (fake timers) — **settled → `finalizeCancel`** and **no-settle → `forceKill` → `abandonCancel`**
(assert the no-settle floor emits a terminal with **no `retry`** and never re-advertises Retry).

---

## Test + verification plan (mirrors 702b's per-category discipline)
- **Backend units (`MithrilPartialSyncService.spec.ts`):** cancel emits `cancelling` (empty actions) then, after
  join, terminal `cancelled` (`['retry','restart-normal']`) — in that order; `finalizeCancel` is a no-op unless status
  is `cancelling`; `abandonCancel` emits terminal `failed` with **no `retry`** and runs **no** cleanup; the no-op
  cancel branch is unaffected. **Rewrite (do not merely add to) the two existing old-contract cases** — `'cleans
  staging artifacts … when cancellation succeeds before cutover'` (`:859-886`, asserts `cancel()` itself ran
  cleanup + landed `status: 'cancelled'`) and `'surfaces a boundary-a failure when cancellation cleanup fails'`
  (`:888-914`, asserts `cancel()` rejects + lands `status: 'failed'`): both encode the pre-split contract and will
  **fail CI first** under the new `cancel()` (emit `cancelling`, no terminal). Drive the new sequence
  (`cancel` → join → `finalizeCancel`/`abandonCancel`) instead. The 3 Boundary/no-op cases (`:787`, `:805`, `:836`)
  stay unchanged.
- **Coordinator units (`chainStorageCoordinator.spec.ts`, `source/main/utils/`):** `_partialSyncInProgress` is
  `false` before `finalizeCancel`/terminal `cancelled`; a start after the full cancel sequence is accepted (no
  "already in progress"); **two join branches (fake timers)** — settled → `finalizeCancel`, and no-settle →
  `forceKill` → `abandonCancel`. The existing `'cancels partial sync immediately …'` case (`:655-686`) still holds
  (only asserts `handlers.cancel` called once) but its mock handlers (`:23-30`) need the new
  `finalizeCancel`/`forceKill`/`abandonCancel` jest.fns **added** (mock setup, not a rewrite).
- **Types unit:** `isMithrilPartialSyncBlockingNodeStart('cancelling') === true` (C-A1).
- **Renderer store (`MithrilPartialSyncStore.spec.ts`):** during `cancelling`, `canRetry`/`canRestartNormally` are
  false; after terminal `cancelled` they are true; the swallowed-rejection log fires.
- **Overlay (`MithrilPartialSyncOverlay.spec.tsx`):** `cancelling` renders the progress ("Cleaning up…") view with no
  action buttons and no waterfall/timer; `cancelled` still renders the retry/restart prompt.
- **CAT-C (verify-only):** `MithrilProactivePromptContainer` returns null when `isSignificantlyBehind = false`;
  `DaedalusDiagnostics.spec.tsx:119-139` still passes unchanged (the option hidden in the same state).
- **CAT-D/E (`SyncingConnectingMithrilPrompt.spec.tsx` + `MithrilProactivePromptContainer.spec.tsx`):** title renders;
  both sentences render (sibling spans, exact-text queries intact); "Note:" element present; negative
  `/partial sync/i` + `/immutable/i` guards hold; fast/Start carry `styles.primaryAction`, Standard/Cancel
  `styles.secondaryAction`; onStart-once and cancel-returns regressions pass.
- **CAT-F (`MithrilPartialSyncConfirmation.spec.tsx`):** both `<p>` carry the Behind/Recovery classes, exactly two
  paragraphs, real `.ReactModal__Overlay` mounts; do not assert computed margins.
- **Storybook:** `cancelling` overlay story (CAT-A); overlay backdrop over animated placeholder content (CAT-B);
  proactive choice + confirm views (CAT-D/E); the confirmation dialog for `behindByEpochs`-set and `behindUnknown`,
  light + dark (CAT-F).
- **Checks:** `yarn check:all` (compile/lint/prettier/stylelint/jest) green with no real regressions (baseline
  prettier caveat per 702b). Node v24 renderer verify-env caveat: regen the `.scss.d.ts` via typed-scss-modules + the
  gitignored identity-obj-proxy jest sidecar before treating tsc/jest fails as regressions.
- **Operator (verify-only, preprod/Linux):** (CAT-A) cancel mid-download → "Cleaning up…" → immediate Retry works (no
  drop-back), record teardown time (U1); (CAT-B) overlay no longer bleeds the node-status page (U2 jank check);
  (CAT-C) confirm the first-load window is a known limitation pending the backend follow-up (U3); (CAT-D/E) prompts
  read as titled + left-aligned + action-colored + readable (U4); (CAT-F) the "Before Mithril Sync begins" paragraphs
  have a clear gap, P1 heavier than P2, ~560px width, ~180px buttons.

---

## Decision record — LOCKED 2026-07-01

Speaker: Grill (grill-with-docs, manual-testing cleanup wave)

`task-ux-702c` is the extensible phase-7 manual-testing cleanup wave; six operator findings are folded in as
CAT-A..CAT-F. Every root cause was grounded against live code and adversarially re-verified (a per-category
verification pass re-confirmed all six root causes and every load-bearing citation; #4's "blur" is a confirmed
misdiagnosis). **CAT-A (correctness):** the retry-after-cancel race — terminal `cancelled`+Retry is emitted
un-coordinated with the still-unwinding `start()` that owns the coordinator's `_partialSyncInProgress` guard.
Fix = **Full**: a `cancelling` empty-actions working-status ("Cleaning up…") that gates Retry during teardown, a
coordinator-owned `_partialSyncRunPromise` join so terminal `cancelled` fires only after the guard clears, a guarded
`finalizeCancel` that owns cleanup + the terminal emit, a bounded join with SIGKILL escalation whose helper returns
`settled` (settled → `finalizeCancel`; no-settle → an `abandonCancel` **non-retryable** floor that never re-advertises
Retry while the guard may still be set), and a renderer defense-in-depth log — with the definitive edit shape C-A1
(blocking-node literal is standalone),
C-A2 (cleanup + `failed`-terminal move behind the join), C-A3 (don't clear runtime state in `cancel()`), C-A4
(suppress the waterfall + timer in the `cancelling` frame). **CAT-B:** add the app-wide `overlay-backrop` blur mixin
+ `opacity:1` to the single shared `.backdrop` rule (six existing consumers), keep the tint token. **CAT-C:** keep the
AND-gate coupled to `isSignificantlyBehind` (the sole offer signal, boundary #4) — the figure-based OR-gate is unsound
(offers an unfulfillable sync, burns the proactive session flag) and is rejected; the real first-load gap is a
**backend** probe-hardening follow-up, filed separately. **CAT-D + CAT-E (merged, D-702c-13):** re-align the proactive
choice view and the confirm/process view to the bootstrap reference — titles "Mithril Sync" / "Mithril Sync Process",
merged left-aligned body (sibling spans), de-dimmed "Note:" element, `.primaryAction`/`.secondaryAction` copied in
locally (augmented with `[disabled]`/`:focus`) on both views. **CAT-F:** un-nest the descendant-scoped
`.mithrilPartialSyncConfirmation*` CSS to flat selectors (the Dialog portals to `document.body`), add
`p + p { margin-top: 20px }`, lift a dialog error class + one-line TSX swap at `:108`, drop the `.Body` opacity:0.85,
delete the two dead classes. All invariants preserved (Boundary-A-only cancel, no wipe in the cancelled dialogue,
offer-signal boundary #4, vocab #8). The deployment gate `task-ux-702` stays held behind 702c. **No product sign-off
remains open** — only verification-only items (U1–U4, the 15 s join-timeout value).

Decision: **LOCKED** — CAT-A = Full (with C-A1..C-A4); CAT-C = keep-AND-gate + backend follow-up; CAT-B/D/E/F as
above. Remaining items are operator verification-only.
