# task-ux-702b-cat-d — Completion finalize: renderer-only robustness

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-D is the **fourth** category in the collision-safe order (`… CAT-C → CAT-D → CAT-E → …`). It lands
**after CAT-C** and **before CAT-E**. CAT-D edits two files shared with other categories: (1)
`MithrilPartialSyncStore.ts`, which is also edited by CAT-B, CAT-A and CAT-E — these four store-file
editors serialize **B → A → D → E** to avoid edit collisions (CAT-D rewrites `dismissCompletedOverlay`);
and (2) `MithrilPartialSyncOverlay.tsx`, also edited by CAT-C — serialize **C → D** (CAT-C owns the
`errorActions` region `:101-151`/`:216-223`; CAT-D owns the completed-overlay `useEffect`/timeout region
`:84-91`). The shared spec `MithrilPartialSyncOverlay.spec.tsx` is touched by CAT-C, CAT-D and CAT-G; run
order **C → D → G**, each touching its own distinct test case.

> **Full collision-safe order:** `CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`, each followed by `yarn compile` + the touched specs + a per-category code-review pass. Store-file editors (`MithrilPartialSyncStore.ts`) serialize **B → A → D → E**. Overlay-file editors (`MithrilPartialSyncOverlay.tsx`) serialize **C → D**; the overlay spec is also touched by **G** (run order C → D → G, each its own test case). **CAT-B before CAT-A** — the container near-tip gate consumes CAT-B's `computeBehindByEpochs`. **CAT-H is backend-only** (collision-free), placed after CAT-E. **CAT-F is standalone.** **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by safe degradation** — an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only (= today, no regression).

## Findings closed & decisions implemented

| # | Finding (short) | Decision | Severity |
|---|---|---|---|
| #2 | Completed-overlay finalize is fire-and-forget (no `.catch`) and `dismissCompletedOverlay` flips `isCompletedOverlayDismissed=true` **before** awaiting finalize → unhandled rejection + staging-dir/`.lock` leak | D-702b-4 | High |

**CORRECTED framing (D-702b-4, 2026-06-30 plan review) — read before implementing.** Because the backend
already runs `_resetToIdleStatus()` **before** the failed `fs.remove(stagingRoot)`
(`MithrilPartialSyncService.ts:351-362`), the `finally { await this.syncStatus() }` pulls `idle`; `idle`
is not an overlay status, so **on failure the overlay hides anyway** (status-driven). This change therefore
does **NOT** keep the overlay up for a retry and does **NOT** remediate the staging-dir/`.lock` leak (that
leak is a backend `fs.remove` failure the renderer cannot reach). Its **two genuine deliverables** are:
(1) **no unhandled promise rejection** (awaited finalize + the timeout `.catch`); (2) **no premature
*optimistic* hide on the success path** (the flag flips only after finalize resolves). The staging/marker
leak is **deferred** to the out-of-scope backend cleanup-before-reset reorder. Future-proofing: if that
backend reorder later leaves `status==='completed'` on failure, the no-blind-flip + resync would correctly
keep the overlay up — but with today's backend it hides. **Renderer-only; NO backend change.**

## Locked invariants this change must NOT break

> - **#9 / D9 success-overlay model.** The success overlay follows the lifecycle through `completed`; the finalize stays the auto-fired reset-idle + remove-staging + clear-marker. CAT-D defers (never skips) the dismiss.

## Exact files (full repo-relative paths)

- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — rewrite `dismissCompletedOverlay` (`:229-241`): await finalize before flipping the flag, catch, resync in `finally`.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` — `.catch` the completed auto-dismiss timeout (`:84-91`); widen the `onDismissCompleted` Prop type.
- specs: `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`, `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`.
- `source/main/mithril/MithrilPartialSyncService.ts` — **NOT modified** (backend reorder is out of scope; cited only for grounding: `_resetToIdleStatus()` `:358` runs before `await fs.remove(stagingRoot)` `:359`).

## Implementation steps (ordered, mechanical)

- [ ] **`dismissCompletedOverlay` — await before hide + catch + resync.** Rewrite `:229-241` to await
  the finalize **before** flipping `isCompletedOverlayDismissed`, and resync on any outcome, reusing the
  store's `try { … } finally { await this.syncStatus(); }` pattern (as in
  `cancelPartialSync`/`startPartialSync`):
  ```ts
  dismissCompletedOverlay = async () => {
    if (this.status !== 'completed') return;
    try {
      await mithrilPartialSyncFinalizeChannel.request();
      this.isCompletedOverlayDismissed = true; // flip ONLY on success
    } catch (error) {
      logger.warn('MithrilPartialSyncStore: failed to finalize completed overlay', { error });
      // keep the overlay up; the resync below reflects the true backend state
    } finally {
      await this.syncStatus();
    }
  };
  ```
  Note in the code comment: the backend `_resetToIdleStatus()` runs before `fs.remove`, so on failure
  the backend is already idle and `syncStatus()` will pull `idle` (overlay hides via status); the
  renderer no longer **optimistically** hides nor leaks an unhandled rejection, and if a future backend
  reorder leaves status at `completed` on failure, the overlay correctly persists (no blind flip).
- [ ] **`.catch` the overlay timeout (no fire-and-forget).** In `MithrilPartialSyncOverlay.tsx:84-91`,
  wrap the `onDismissCompleted()` call so a finalize rejection can never become an unhandled rejection:
  ```ts
  const timer = setTimeout(() => {
    Promise.resolve(onDismissCompleted()).catch(() => {});
  }, COMPLETED_AUTO_DISMISS_DELAY_MS);
  ```
  Widen the `onDismissCompleted` Prop type to `(): void | Promise<void>` so the `Promise.resolve(...)`
  wrap type-checks. Keep `COMPLETED_AUTO_DISMISS_DELAY_MS`, the `status !== 'completed'` guard, and the
  `clearTimeout` cleanup.

## New symbols

None. The `onDismissCompleted` Prop type widens to `(): void | Promise<void>`; no new exported symbol.

## Acceptance

- On finalize success: `isCompletedOverlayDismissed` flips **after** the awaited finalize, `syncStatus()`
  runs, overlay hides cleanly (no premature optimistic hide).
- On finalize failure: `isCompletedOverlayDismissed` **stays `false`** and there is **no unhandled promise
  rejection**; `syncStatus()` resyncs the true backend state. **Note (plan-review correction):** today's
  backend resets to `idle` *before* the failed `fs.remove`, so that resync pulls `idle` and the overlay
  **hides anyway** — this change does **NOT** keep the overlay up and does **NOT** fix the staging/`.lock`
  leak (deferred to the out-of-scope backend reorder). The two genuine deliverables are: no unhandled
  rejection + no premature optimistic hide on success.
- No backend file is modified.

## Tests (add/update)

- `MithrilPartialSyncStore.spec.ts`: finalize-success path (flag flips, `syncStatus` called) and a
  finalize-rejection path (flag stays false, no unhandled rejection, `syncStatus` called). Mock the
  finalize channel to resolve / reject.
- **Existing store-spec test must stay green** (`MithrilPartialSyncStore.spec.ts:~639`, "live completed
  push keeps overlay shown; `dismissCompletedOverlay` flips it off and calls finalize once"): after this
  rewrite the flag flips **after** the awaited finalize (not before), and the new `finally { await
  this.syncStatus(); }` issues an **extra** status-channel request on the finalize path. Ensure that
  test's status-channel mock **tolerates** the added finalize-path `syncStatus()` call (resolves a status
  response) and that its finalize-called-once assertion still holds.
- `MithrilPartialSyncOverlay.spec.tsx`: the completed auto-dismiss test (fake timers) asserts the
  timeout-fired `onDismissCompleted` is invoked and that a rejecting `onDismissCompleted` does not throw
  out of the timer (no unhandled rejection).
- **Failure-path resync (plan-review):** mock the finalize channel to reject AND the status channel to
  resolve an `idle` snapshot; assert `dismissCompletedOverlay` does not throw (no unhandled rejection),
  `isCompletedOverlayDismissed` stays `false`, `syncStatus()` is called, and the resulting
  `shouldShowOverlay === false` (overlay hides because the backend is idle — documents the real behavior
  so a later reviewer does not "fix" the test toward a keep-overlay-up intent that today's backend cannot
  deliver).

## Verify commands

- `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `yarn compile`
- `yarn lint`

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest failure as a regression (memory: `mithril-ux-renderer-verify-env`).

## Operator / verify-only gates

None.

## Cross-category coupling notes

- **`MithrilPartialSyncStore.ts` is shared with CAT-B, CAT-A and CAT-E.** Serialize **B → A → D → E**.
  CAT-D edits only `dismissCompletedOverlay` (`:229-241`); avoid touching the `certifiedEpoch` observable
  (CAT-B/CAT-A), `mithrilAttemptStartedThisSession` / `_applyAvailability` (CAT-A), or `_refreshAvailability`
  / the back-off (CAT-E).
- **`MithrilPartialSyncOverlay.tsx` is shared with CAT-C.** Serialize **C → D**. CAT-C owns the
  `errorActions` ordering region (`:101-151`/`:216-223`); CAT-D owns the completed-overlay
  `useEffect`/timeout region (`:84-91`). Do not touch the error branch (CAT-C owns it).
- **`MithrilPartialSyncOverlay.spec.tsx` is shared with CAT-C and CAT-G.** Run order **C → D → G**, each
  touching its own distinct test case with a compile/review pass between.
- **Backend `MithrilPartialSyncService.ts` is OUT OF SCOPE here** — the `_resetToIdleStatus()`-before-`fs.remove`
  order stays; the staging/`.lock` leak fix is deferred to the (out-of-scope) backend reorder.
