# task-ux-702c — Independent Code Review

> Independent, adversarial code review of the uncommitted 702c working tree (manual-testing cleanup wave,
> CAT-A..CAT-F). Distinct from `task-ux-702c-impl-review.md` (the implementer's own build/review log).
> Plan: `task-ux-702c.md`. Decisions: `task-ux-702c-decisions.md` (D-702c-0..13). Reviewed: 2026-07-01.

---

## Method

- One **grounding** subagent read the four-file 702c task surface and produced a per-CAT review dispatch plan
  (CAT inventory, per-CAT scope, cross-cutting concerns, acceptance gates) before any code was judged.
- **Six per-CAT review subagents** then ran in parallel, each pulling the live `git diff HEAD` for its files,
  re-confirming the plan's load-bearing line anchors against the working tree, verifying conformance to the locked
  decisions, and adversarially hunting for correctness defects, regressions, i18n parity gaps, and scope violations.
- CAT-D and CAT-E share one merged edit (D-702c-13); they were reviewed by two complementary agents (choice view vs
  confirm view) over the same diff.

## Headline verdict

**No High or Medium issues found in any category.** All six CATs conform to their locked decisions
(D-702c-8..12 for the presentation slices; D-702c-1..6 + C-A1..C-A4 for the correctness slice). The only
correctness-bearing change — CAT-A's retry-after-cancel race fix — is verified correct and safe. Every finding
surfaced is **Low** (cosmetic / test-representativeness / diff-hygiene) and none blocks the task.

The task correctly remains **pending / interactive_validation**: automated gates are green and all code-bearing CATs
are conformant, but the operator manual gates **U1–U4** are still open and are the true remaining blockers.

## Per-CAT results

| CAT | Decision | Original sev. | Conformance | High | Med | Low | Reviewer verdict |
|-----|----------|---------------|-------------|:----:|:---:|:---:|------------------|
| **A** — retry-after-cancel `cancelling` state + gated Retry + bounded join | D-702c-1..6, C-A1..C-A4 | High (correctness) | ✅ all 13 items | 0 | 0 | 3 | Correct & safe; race guarantee holds |
| **B** — overlay backdrop frosted-glass blur | D-702c-8 | Medium | ✅ all 5 items | 0 | 0 | 2 | Minimal, exact, styling-only |
| **C** — keep AND-gate; file backend follow-up (verify-only) | D-702c-9 | Medium | ✅ no-op confirmed | 0 | 0 | 1 | Correct no-op; AND-gate byte-for-byte intact |
| **D** — proactive prompt (choice view) chrome | D-702c-10, D-702c-13 | Medium | ✅ all 7 items | 0 | 0 | 2 | Conformant; 23/23 affected specs pass |
| **E** — process (confirm view) chrome | D-702c-11, D-702c-13 | Low | ✅ all 5 items | 0 | 0 | 1 | Conformant; behavioral specs pass |
| **F** — un-nest dead descendant-scoped dialog CSS | D-702c-12 | Medium | ✅ all 7 items | 0 | 0 | 1 | Conformant; no dangling refs, spacing coherent |

## High / Medium issues

**None.** No High or Medium severity issues exist across CAT-A through CAT-F.

## Low findings (tracked, non-blocking)

The most substantive item is the first (CAT-A); the rest are hygiene/informational.

1. **[Low · CAT-A] `_cancelFallbackErrorStage` is dead-in-production for `finalizeCancel`, and its unit asserts a
   value production never yields.** `MithrilPartialSyncService.ts:349-352` reads
   `this._cancelFallbackErrorStage ?? this._getCurrentRecoveryStage()` in the cleanup-error branch, but on the real
   coordinator settled path `start()`'s `finally` runs `_clearRuntimeWorkState()` (`:669`), nulling
   `_cancelFallbackErrorStage` *before* `finalizeCancel()` runs — so the fallback always resolves to `'preparing'`
   for status `cancelling` (`:1044`), never the actual pre-cancel stage. The rewritten unit
   (`MithrilPartialSyncService.spec.ts:909-917`) calls `service.finalizeCancel()` directly (bypassing the join), so
   the field is never cleared and it asserts `stage: 'downloading'` — a value the production join path cannot produce.
   *Impact:* cosmetic only (the `error.stage` metadata on an already-correct `failed` terminal; the load-bearing
   status and `['retry','restart-normal']` actions are correct), but a regression in real stage propagation would not
   be caught by this test. The field remains meaningful only for `abandonCancel` (where `start()` has not unwound).
   Confidence: CONFIRMED.

2. **[Low · CAT-A] `abandonCancel` emits a hardcoded English `error.message`.**
   `MithrilPartialSyncService.ts:382-383` ("Daedalus couldn't finish cleaning up Mithril Sync. Restart Daedalus to
   continue safely.") is not i18n-keyed. Consistent with the existing un-localized backend error channel
   (`_buildError`, `:1085-1090`) and vocab-clean (no "partial sync"/%/immutable), so **not** a guardrail violation —
   but it is a net-new user-visible English string on a JA build. Confidence: CONFIRMED (un-localized); PLAUSIBLE
   (whether it matters given the pre-existing channel).

3. **[Low · CAT-D] Plan says `promptHandoffNote` was "updated"; it is byte-identical to HEAD.** The pre-edit copy
   never carried a "Note:" prefix, so nothing needed stripping when "Note:" moved into its own `<strong>` element.
   No functional impact; spec assertion for the current string passes. Plan-vs-impl wording mismatch only.

4. **[Low · CAT-C] Incidental Prettier-only reformats in the CAT-A file touch (but do not alter) the offer-signal
   helper.** `MithrilPartialSyncService.ts:754` reflow sits inside `_readLocalImmutableNumber` (feeds
   `getPartialSyncBehindness`); `getPartialSyncBehindness`/`isSignificantlyBehind` logic (`:783-815`) is **not** in
   the diff. Not a boundary #4 scope violation — cosmetic file-wide Prettier noise only. (Also observed by the CAT-A
   reviewer as diff-hygiene across `MithrilController.ts`, `chainStorageCoordinator.ts`.)

5. **[Low · CAT-B] Mixin adds only `backdrop-filter` (no property clobbering); effective bootstrap tint firms
   ~0.89→0.92.** Both verified within the plan's explicitly accepted bounds ("slightly-firmer bootstrap tint is
   acceptable"); `sass` + `stylelint` clean. Informational.

6. **[Low · CAT-D] Extra responsive `@media (max-width:720px)` stacking block carried over from
   `MithrilErrorView.scss`.** Not called out in the plan but a faithful copy from the same reference stylesheet;
   harmless. Informational.

7. **[Low · CAT-E] Both `renderChoiceView` and `renderConfirmView` emit `<h1 className={styles.title}>`.** They are
   mutually exclusive at render time, so there is never a duplicate landmark in the DOM. Informational.

8. **[Low · CAT-F] Dialog error text uses `--theme-dialog-title-color` (not network-red), differentiated only by
   italic/14px.** This is the *decided* behavior — D-702c-12 explicitly rejects `--theme-network-window-red-color` on
   the white dialog. Flagged only so QA confirms the start-failure line reads as an error. Not a code defect.

9. **[Info · CAT-C] Doc path nit:** plan/decisions cite `mithrilPartialSyncPreflight.ts` under `source/main/utils/`;
   it actually lives at `source/main/mithril/`. Cited constants and line ranges are otherwise accurate. No code impact.

## Cross-cutting confirmations

- **CAT-A race guarantee (the core of the wave):** the captured `_partialSyncRunPromise` wraps the **whole**
  `try/finally { _partialSyncInProgress = false }` IIFE (`chainStorageCoordinator.ts:271-296`), not the inner
  `handlers.start()` — so `settled === true` provably implies `_partialSyncInProgress === false` before
  `finalizeCancel` advertises Retry. The no-settle branch routes to `abandonCancel` (non-retryable, no cleanup),
  so **Retry is never advertised while the guard could still be set**. `forceKill` operates on a `_currentProcess`
  that is only nulled by `start()`'s own finally after the child closes. The `cancelling` frame cannot be clobbered
  by a late progress line (`applyProgressUpdate` early-returns on `_isCancelled`, set before the emit). No double
  terminal is possible.
- **i18n parity:** all new keys (CAT-A "Cleaning up…", CAT-D "Mithril Sync", CAT-E "Mithril Sync Process") present
  1:1:1 across `en-US.json` / `ja-JP.json` / generated `defaultMessages.json`; all vocab-clean (no "partial
  sync"/%/immutable; epochs-only); negative `/partial sync/i` + `/immutable/i` spec guards still hold.
- **`.scss.d.ts` regeneration:** CAT-D/E and CAT-F module typings correctly reflect added/removed classes on disk
  (gitignored, so absent from `git diff` by design); no TSX references a class missing from its `.scss.d.ts`.
- **No scope violations:** CAT-B..CAT-F are presentation-only — no change to the `cancelling`/state machine, the PRD
  status contract, the Boundary A/B/C recovery model, or the backend offer-signal boundary #4
  (`isSignificantlyBehind` remains the sole offer signal; the Diagnostics option stays coupled to the proactive
  prompt). CAT-C introduces no OR-gate anywhere.
- **Spec discipline:** the two old-contract CAT-A service specs were **rewritten** (not appended) to drive
  cancel→join→`finalizeCancel`/`abandonCancel` and assert `cancel()` emits `cancelling` with no terminal; the three
  Boundary/no-op cases are preserved; the coordinator mock gained the three new handler fns plus two genuine
  fake-timer join units.

## Outstanding (operator gates — the real blockers)

These are verify-only and cannot be closed by code review; they gate task completion:

- **U1 (CAT-A):** cancel mid-download → "Cleaning up…" → immediate Retry works (no drop-back); record teardown
  latency; validate the 15 s `PARTIAL_SYNC_CANCEL_JOIN_TIMEOUT_MS` + SIGKILL escalation on a real node.
- **U2 (CAT-B):** overlay no longer bleeds the live syncing screen; required low-end/Linux `backdrop-filter`
  GPU-jank check over animated content (Storybook is explicitly **not** valid proof for the blur).
- **U3 (CAT-C):** confirm the first-load window is a known, documented limitation pending the separate backend
  offer-signal follow-up; record window duration for that task's context.
- **U4 (CAT-D/E):** proactive/process prompts read as titled + left-aligned + action-colored + readable "Note:".
- **CAT-F visual:** clear vertical paragraph gap, P1 heavier than P2, ~560px width, ~180px buttons, light + dark.

## Recommendation

The 702c code is **ready for the operator validation pass (U1–U4)** with no code changes required to proceed.
Optionally, before or alongside the operator pass, the CAT-A implementer may wish to address Low #1 (make the
`finalizeCancel` cleanup-error unit drive the real join so its asserted `error.stage` reflects production, or drop
the dead `_cancelFallbackErrorStage` read from `finalizeCancel`) and Low #2 (i18n-key the `abandonCancel` message)
— both are polish, not blockers.
