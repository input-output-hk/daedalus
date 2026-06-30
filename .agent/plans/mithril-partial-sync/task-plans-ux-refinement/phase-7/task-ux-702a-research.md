# task-ux-702a — Research Log (durable findings)

> Durable, append-only research notes surfaced while implementing the 702a
> categories. Each entry carries a speaker label, an ISO-8601 UTC `Timestamp:`,
> and the category it was found under. All citations verified against live code
> at the noted commit/working tree.

---

## CAT-A — backend availability probe coupling (flag, not fixed here)

Timestamp: 2026-06-29T17:49:33Z
Speaker: Implementation (CAT-A)

Finding (verified against live code):
`MithrilController.getPartialSyncAvailability` (`source/main/mithril/MithrilController.ts:167-174`)
reads the static `isEnabled` synchronously (`:168`,
`chainStorageCoordinator.getPartialSyncAvailability()`), but then **awaits the slow
behind-ness probe** `this._partialSyncService.getPartialSyncBehindness()` (`:172`) **before
returning the whole response** (`:173`). So on a cold start the renderer's availability read can
block on the behind-ness probe even though `isEnabled` is already known — contributing to the
first-load lag where the Diagnostics Mithril section does not appear until later (ISSUE-1).

Scope decision: CAT-A is **renderer-only and collision-safe**, so the backend is NOT edited here.
The renderer fix (CAT-A) makes the availability poll re-fetch on **every** 30s tick (dropping the
old `if (this.isWorking)` gate) plus a re-entrancy guard, so the Diagnostics section self-corrects
within ≤30s on first load without a reload — independent of the backend timing.

OPTIONAL backend hardening (flagged for the backend-correctness track, **out of CAT-A scope**):
return `isEnabled` immediately and refresh behind-ness independently (e.g. resolve availability with
`isSignificantlyBehind: false` first, then update once the probe settles), so the static enable flag
is never gated behind the slow probe. Escalate to the backend track if the ≤30s renderer
self-correction is still judged too slow on cold start.

No backend file was modified by CAT-A.

---

## CAT-H — Storybook sidebar folder-vs-component icon (report-only root cause)

Timestamp: 2026-06-29T19:29:11Z
Speaker: Implementation (CAT-H)

Finding (verified against live config + dependency tree): the inconsistent sidebar glyph
(ISSUE-12 side-question — some Mithril entries show a folder icon, others a component/document
icon) is **stock Storybook 6.4 behavior, not a Daedalus mis-configuration**. Versions:
`@storybook/react@6.4.22`, `@storybook/addon-knobs@6.4.0` (`package.json`). In Storybook 6.4 the
sidebar tree derives each node's icon purely from the slash-delimited `storiesOf(...)` kind path:
**any kind segment that is a parent of a deeper kind (a grouping node with children) renders the
folder glyph; only leaf kinds that directly own stories render the component/document glyph.** There
is no per-story icon API in `storiesOf` and no sidebar/icon configuration in
`storybook/main.ts` (`source: storybook/main.ts:1-142` — only `core.builder`, `stories` glob,
addons, and a `webpackFinal` for ProvidePlugin/SCSS/SVG; zero `manager`/`sidebar`/`renderLabel`
options), nor any `.storybook/manager.{js,ts}` (none exists in the repo).

Why the reorg keeps the folder icons by design: the requested grouping deliberately introduces
**nested parent kinds** — `Nodes / Diagnostic` is a parent of `Nodes / Diagnostic / Mithril Partial
Sync Confirmation`, and `Loading / Mithril` is a parent of `Loading / Mithril / Mithril Partial Sync
Dialogue` (and of `Loading / Mithril / Partial Sync Overlay`, etc.). Those intermediate parents are
grouping nodes, so Storybook necessarily paints them with the folder glyph. The only way to get a
uniform component glyph would be to **flatten away the grouping** (single-segment kinds), which
contradicts ISSUE-10/ISSUE-11's requested NODES→Diagnostic and LOADING→Mithril→Dialogue hierarchy.

Conclusion: **no trivial fix preserves the requested grouping** — the folder icons on the parent
kinds are inherent to having parent kinds in Storybook 6.4. A non-trivial alternative (custom
`renderLabel`/sidebar manager theming) was out of CAT-H scope and not pursued. Reported per the plan
as report-only; no code change for the icon. CAT-H touched zero app code/IPC/store/.scss.

---

## Scribe (finalization) — durable root causes + sanctioned ADR amendments

Timestamp: 2026-06-29T19:51:40Z
Speaker: Scribe (task-ux-702a finalization)

Consolidated durable findings worth keeping past the working tree. The CAT-A backend
`getPartialSyncAvailability` probe-delay (the deferred ISSUE-2 backend item) and the CAT-H Storybook
sidebar folder-icon root cause are recorded in full in the two sections above and are NOT repeated here.

### CAT-A — first-load Diagnostics invisibility root cause (ISSUE-1, verified, FIXED renderer-side)
The Mithril option being hidden on first load and only appearing after a manual reload was **not** a
config/kill-switch difference — it was a renderer **freshness** bug. `MithrilPartialSyncStore` fetched
availability exactly once at `setup()` and then re-fetched on the 30s interval **only `if (this.isWorking)`**.
During normal Cardano-node sync `isWorking` is `false`, so the one-shot read never self-corrected and the
section stayed hidden until a full reload re-ran `setup()`. Fix (CAT-A, renderer-only): drop the
`if (this.isWorking)` gate so `_refreshAvailability()` fires on **every** 30s tick, plus a
`_isRefreshingAvailability` re-entrancy guard (set before the request, cleared in `finally`) to prevent
probe pile-up. Net: the section self-corrects within ≤30s on first load with no reload, independent of the
backend probe timing flagged above. ISSUE-2's "Last network block"/behind-ness-known availability is
exposed as the reactive `NetworkStatusStore.isBehindnessKnown` computed (both tips present with finite
epochs — an availability boolean, NOT a renderer threshold; backend `isSignificantlyBehind` stays the sole
offer signal).

### CAT-F — proactive prompt unmount-on-transition root cause (ISSUE-7, verified, FIXED)
The prompt vanished on the loading → Wallet Summary transition because it was mounted **inside the loading
screen** (`SyncingConnecting.tsx`), which unmounts the instant Root returns the routed app. It also flashed
during the early connecting/"verifying blockchain" window because the gate lacked any known-behind /
status-idle guard (the displayed epochs figure is `undefined` until `networkTip` arrives). Fix (CAT-F):
hoist the prompt into a new app-level `MithrilProactivePromptContainer` mounted in `App.tsx` as a sibling
of `<Router>` (the established persist surface, beside `MithrilPartialSyncOverlay` /
`RTSFlagsRecommendationOverlayContainer`), so it survives the route swap into Wallet Summary; and gate it on
`status==='idle' && isPartialSyncEnabled && isSignificantlyBehind && networkStatus.isBehindnessKnown &&
!proactivePromptDismissedThisSession`. The gate consumes **CAT-A's `isBehindnessKnown` computed directly**
(not a local `behindByEpochs !== undefined` re-derivation), which is what keeps that CAT-A deliverable
load-bearing and kills the anti-flash early-render. The `status==='idle'` gate makes the prompt mutually
exclusive with the overlay (`MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES` explicitly excludes `'idle'`), so they
never co-render, and during empty-chain bootstrap there is no synced local tip so `isBehindnessKnown` is
false → the prompt stays hidden (#11 preserved).

### Sanctioned ADR amendments — how cleanup/safety was preserved
- **D-702a-1 (CAT-G) — auto-fire D9 finalize on the completed-overlay timeout.** Removed the "Continue to
  Daedalus" button; the completed overlay now shows a loading-style hand-off (spinner + "Returning to
  Daedalus...") and the existing finalize IPC fires **automatically** when the completed-timeout elapses
  (`COMPLETED_AUTO_DISMISS_DELAY_MS = 4000`, a component `useEffect setTimeout` calling the unchanged
  `store.dismissCompletedOverlay → mithrilPartialSyncFinalizeChannel`). The amendment swaps the **trigger**
  (click → timeout) only; the backend `finalizeCompletedPartialSync()` is byte-for-byte untouched and still
  does `_resetToIdleStatus()` + `fs.remove(stagingRoot)` (staging-folder deletion) + `clearMithrilPartial
  SyncMarker()` + `_clearRuntimeWorkState()`. No backend/IPC-contract/store-logic change to the finalize
  path. This is the explicit amendment to locked #16-as-amended-by-D9 (success overlay had cleared only on a
  manual dismiss); the 601-touched `completed.subtitle` reword is a deliberate override, not a regression.
- **D-702a-2 (CAT-E) — cancelled-state recovery trim (pre-cutover only).** The backend stops emitting
  `wipe-and-full-sync` for the two **cancel-originated** arrays (`status:'cancelled'` ~:301 and the
  cancel-cleanup `status:'failed'` ~:311), trimming both to `['retry','restart-normal']`. Cancellation is
  only possible pre-cutover (Boundary A) where the chain DB is intact, so `restart-normal` is the viable
  slow path and wipe is unnecessary. **Preserved untouched:** post-cutover `['wipe-and-full-sync']` in
  `mithrilPartialSyncNodeStartup.ts` (there the old DB is already emptied, so wipe is the only safe recovery;
  removing it would create a Quit-only dead-end — locked #6/D5b), AND the active-sync **failure** path
  `_deriveAllowedRecoveryActions` inline `['retry','restart-normal','wipe-and-full-sync']` (~:552). The
  resulting cancelled(2-action)/failure(3-action) asymmetry is **intentional** per the literal D-702a-2
  citation. The renderer still renders strictly from `allowedRecoveryActions` (locked #5 intact; no renderer
  logic change), and `cancel()` still hard-rejects post-cutover for installing/finalizing (locked #6 intact).
