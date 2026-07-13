# task-ux-402 — Live activity affordances: rotating active step, ticking elapsed, reassurance, and populated stopping-node frame

- **Phase:** phase-4 (Renderer Progress And Error Overlay Honesty)
- **Implements:** PRD **D4 (UX6/UX7)** — animated active step + renderer-side ticking elapsed in BOTH flows — and **D8 #19 (UX5)** — populated in-dialogue node-stop frame. Closes research-19 **gap #5 (non-download "looks stalled")**, **gap #19 (status / stopping-node)**, **gap #36 (active circle never rotates)**, **gap #37 (elapsed freezes between events)**, **gap #38 (node-stop invisible in dialogue)**.
- **Planning status:** `approved`
- **Build status:** `completed`
- **Interaction mode:** `autonomous`
- **Single commit subject:** `feat(mithril): task-ux-402 add live activity affordances to the Mithril progress overlay in both flows`

---

## Why now

task-ux-401 made the download readout truthful. The remaining dishonesty is **apparent deadness**: during the long non-download phases and the node-stop window, the shared overlay looks frozen.

- The active top-level step renders a **static dot** (`.activeCircle`), so nothing animates while a phase runs (gap #36 / UX7).
- Partial-sync elapsed is **backend-snapshot-stamped**, so it freezes between status events and reads `0:00` during the node stop (gaps #37/#5 / UX6). The reporter saw it stick at `6:46` and `0:00`.
- The node-stop window has **no in-dialogue frame** — only greyed steps behind the opaque backdrop for a couple of minutes (gaps #38/#19 / UX5).

D4/D8#19 close these by reusing seams the bootstrap flow already proves: the unused `.iconSpinner`/`loading-spin`, the `bootstrapStartedAt` renderer-side ticking timer, and the `isStartingNode` reassurance block. This task is the activity-affordance layer on top of task-ux-401's honest counts. It is the **last** shared-component change before task-ux-403 (error copy) and task-ux-404 (recovery-action safety) layer on.

---

## Interaction mode + justification

**`autonomous`.** The only net-new copy is **operational reassurance** ("This can take several minutes…", "Stopping Cardano node… this can take a couple of minutes") taken almost verbatim from PRD D4 (lines 210-212) and D8 #19 (lines 398-401). There is **no behind-ness figure, no benefit-vs-waiting framing, and no blocking user decision** (those were task-ux-304 / the proactive-prompt tasks, which were `interactive_decision`). All five new strings are mechanical phase reassurance with first-class EN+JA. `needsUserDecision = false`. If the implementer feels the reassurance wording needs sign-off, that is **not** a blocker — proceed; the holistic JA/wording pass is task-ux-601.

---

## Scope

1. **Rotating active top-level step (both flows).** Replace the static `.activeCircle` dot with the existing `.iconSpinner`/`loading-spin` spinner for the **active** top-level step in `MithrilStepIndicator`. Reduced-motion stays honored (the scss reduced-motion block already names `.iconSpinner`).
2. **Renderer-side ticking elapsed (both flows).** Add `startedAt` to `MithrilPartialSyncStore`, anchored exactly like the bootstrap store's `bootstrapStartedAt`. Pass it through the overlay as `bootstrapStartedAt` into `MithrilProgressView`'s already-proven ticking effect, and **stop passing the frozen `elapsedSeconds` prop**. Elapsed advances from `0:00` immediately and never freezes between events.
3. **Long-phase reassurance.** For `verifying`/`unpacking`/`converting`/`installing`/`finalizing`, render a one-line "this can take several minutes" reassurance under the elapsed timer (the active indicator = the spinner; elapsed already shown).
4. **Populated, in-dialogue stopping-node frame.** Map `stopping-node` to the **`preparing`** top-level step so it renders as an active (spinning) step instead of three greyed placeholders, and add an `isStoppingNode` reassurance block mirroring `isStartingNode`.

### Non-goals (do NOT do here)

- **No** Cancel-disable-during-`stopping-node` tooltip and **no** always-resync-after-cancel — that is **task-ux-404** (gap #39). Leave `MithrilProgressView`'s `disabled={isStartingNode}` and the overlay's `hideAction`/`onCancel` wiring **unchanged**.
- **No** backend emit of `stopping-node` — optional polish owned by task-ux-203. `stopping-node` stays the renderer-optimistic `START_PENDING_STATUS`.
- **No** error-copy map / cancelled-vs-failed copy (task-ux-403); **no** finalize-channel / recovery-action changes (task-ux-404).
- **No** new IPC, no backend change, **no** synthetic throughput / remaining-time / overall-% (locked invariant **#18 = prompt boundary #8**). Elapsed is a **renderer clock anchored to a timestamp the store holds**, not a backend-streamed number.
- **No** rename/removal of the `elapsedSeconds` prop on `MithrilProgressView` (keep it to avoid storybook TS ripple — storybook is task-ux-502); just stop **passing** it from the overlay.
- **No** change to task-ux-401's file-count work (`filesDownloaded`/`filesTotal`/`snapshotSizeBytes`, `formatFileCount`, `progressCombinedDetail`, `progressSnapshotSizeContext`).
- **No** snapshot-selection or storage-location picker.

---

## Dependencies

- JSON `dependencies: ["task-ux-401"]` — **task-ux-401 is `completed` (shipped 2026-06-26)**. Its prop renames (`filesDownloaded`/`filesTotal`/`snapshotSizeBytes`) are already on the shared components this task edits; do not clobber them.

---

## Research / docs / workflows / skills consulted

- PRD `mithril-partial-sync-ux-refinement-prd.md`: **D4** long-phase reassurance (lines 210-212), animated active step **UX7** (lines 219-224), ticking elapsed **UX6** (lines 227-234); **D8 #19** populated node-stop frame (lines 394-401); components-impact notes (lines 897-902); store note "add `startedAt`" (line 881); acceptance checklist (lines 819-823).
- research-19 `19-ux-refinement-state-and-gaps.md`: **gap #5** (line 132), **#36** (line 389), **#37** (line 390), **#38** (line 391), **#19/stopping-node renderer-optimistic** (lines 85, 243, 278). §5 locked invariant **#18** (no synthetic figures).
- prompt `prompt-ux-refinement.md` — locked boundaries **#8** (no synthetic figures), **#11** (do not regress empty-chain bootstrap); "smallest truthful change, reuse seams".
- **task-ux-401** four docs (`task-ux-401.md`, `-research.md`, `-impl-review.md`, `-plan-review.md`) — continuity on the shared components, the live-seam table, and the Node v24 dart-sass env workaround.
- Workflows: `.agent/workflows/frontend.md`, `.agent/workflows/test.md`. Skill: `i18n-messaging` (the `!!!` default convention; `yarn i18n:extract` writes only `translations/messages.json`; `en-US.json`/`ja-JP.json` are hand-edited first-class catalogs).

---

## Live-seam verification table (verified against the working tree 2026-06-26)

| Anchor (JSON / PRD) | Expected | Live finding | Status |
|---|---|---|---|
| `MithrilStepIndicator.tsx:386-388` `.activeCircle` static dot | the active top-level icon | `TopLevelIcon` active branch is **L403-405**: `if (state === 'active') { return <div className={styles.activeCircle} />; }` | **DRIFT (+17)** — use L403-405 |
| `MithrilStepIndicator.scss:79-84` `.activeCircle` (no animation) | the static dot style | `.activeCircle` is **L79-84** | CONFIRMED |
| `MithrilStepIndicator.scss:299-311` reduced-motion | guards `.iconSpinner` | media block **L299-312**; `.iconSpinner, .subItemIconSpinner { :global { animation: none } }` at **L306-311** — already names `.iconSpinner` | CONFIRMED — **no scss change needed** |
| `.iconSpinner` rule | unused spinner style ready to reuse | **L71-77**: `.iconSpinner { color: …; :global { animation: loading-spin 1.2s linear infinite } }` — identical structure to the working `.subItemIconSpinner` (L215-225); `loading-spin` is the global keyframe from `@import '…/mixins/loading-spinner'` (scss L3) | CONFIRMED — proven by the live sub-item spinner |
| `MithrilStepIndicator.spec.tsx:59-60` locks `.iconSpinner` null | the spec to flip | assertions at **L61-62** (`activeCircle` not-null L61, `iconSpinner` null L62); whole test **L38-64** | **DRIFT (+2)** — flip L61-62 |
| `MithrilProgressView.tsx:97-109` bootstrap ticking path | the proven elapsed effect | `useEffect` body **L93-111**; `elapsedSecondsProp` precedence **L94-97**; tick/`setInterval` **L104-110**; deps `[bootstrapStartedAt, elapsedSecondsProp, status]` L111; `TERMINAL_STATUSES = Set(['failed','cancelled'])` **L43-45** | CONFIRMED (effect spans 93-111) |
| `MithrilProgressView.tsx:142-163` `isStartingNode` block | the block to mirror | `isStartingNode = status === 'starting-node'` **L86**; block **L145-166** (`completionBlock`, `completionTitle`, `completionDetail`, `completionSpinner`) | **DRIFT (+3)** — mirror L145-166 |
| `MithrilPartialSyncOverlay.tsx:93` frozen `elapsedSeconds` prop | the line to drop | **L93** `elapsedSeconds={transferProgress?.elapsedSeconds}` | CONFIRMED exact |
| store `startedAt` | "the store already can hold" | NOT present yet; `MithrilPartialSyncStore` already holds `elapsedSeconds`/`filesDownloaded`/… and `_updateStatus` (L146-166) is the single mutation point; `START_PENDING_STATUS = 'stopping-node'` (L33) | add `@observable startedAt` |
| bootstrap-proven anchor logic | the pattern to copy | `MithrilBootstrapStore._updateStatus` **L128-137**: on first working frame, `bootstrapStartedAt = (finite backendElapsed > 0) ? Date.now() - backendElapsed*1000 : Date.now()`; reset to `null` on terminal→fresh-start (L126) and decision-cycle (L148) | copy verbatim, partial-sync-typed |
| overlay mount | where store→overlay props are mapped | `App.tsx:97-123` renders `<MithrilPartialSyncOverlay>` with store fields; transferProgress built L101-109 (incl. dead-after-this `elapsedSeconds: …` L104) | thread `startedAt` here |
| consumers passing `elapsedSeconds` to `MithrilProgressView` | none besides the overlay | grep: only `App.tsx:104` (into transferProgress) + `MithrilPartialSyncOverlay.tsx:93`; bootstrap page does **not** pass it | CONFIRMED — safe to stop passing |

**Net reconciliations:** every numeric anchor in TASK_CONTEXT/PRD drifted by a few lines (the components grew during task-ux-401). Use the LIVE anchors above. The most consequential reconciliation: **`MithrilStepIndicator.scss` needs NO edit** — `.iconSpinner` already exists and reduced-motion already guards it (scss L306-311), so the spinner is a pure TSX swap. The scss target listed in the JSON is satisfied by verification, not edit.

---

## Locked invariants this change MUST NOT break (inline)

- **#11 — do not regress the empty-chain bootstrap flow.** This edits the SHARED `MithrilStepIndicator` / `MithrilProgressView`. The spinner, ticking elapsed, and reassurance apply to bootstrap too. Re-run `MithrilBootstrap.spec.tsx`, `MithrilStepIndicator.spec.tsx`, `MithrilProgressView.spec.tsx`; the bootstrap **success/handoff** path (`starting-node` block, all-completed checkmarks) and the combined-bar percentages must be unchanged. `stopping-node` is not a `MithrilBootstrapStatus`, so the `STATUS_TO_STEP['stopping-node']` map entry is never consulted for bootstrap.
- **#8 / #18 — no synthetic figures.** Elapsed is `Math.floor((Date.now() - startedAt)/1000)` in the renderer — NOT a backend-streamed number, NOT throughput/ETA/overall-%. No new IPC channel or field. The reassurance is static copy, not a computed estimate.
- **Reduced-motion accessibility (scss L299-312) must remain effective for the new spinner.** Because the active top-level icon uses `styles.iconSpinner`, the existing `@media (prefers-reduced-motion: reduce) { .iconSpinner { animation: none } }` already suppresses it. Do **not** invent a new animation class that would escape this guard.
- **#4 — preserve the locked "verified … chain data … catch up faster" wording.** Do not alter `partialSyncProgressSubtitle`. New reassurance copy uses "verified chain data" consistently but adds no behind-ness claim.
- **Reuse existing seams.** `.iconSpinner`/`loading-spin`, the `bootstrapStartedAt` ticking effect, and the `isStartingNode` block. Do not build a new timer/animation system.

---

## Implementation approach — ordered, mechanical steps

> File paths are relative to repo root `/workspaces/mithril-partial-sync-ux`. All line anchors are LIVE (verified above).

### Step 1 — Rotating active top-level step (`MithrilStepIndicator.tsx`)

In `TopLevelIcon` (live **L393-416**), replace the active branch (**L403-405**):
```tsx
  if (state === 'active') {
    return <div className={styles.activeCircle} />;
  }
```
with the spinner (mirrors the `.iconCheck` pattern just above it, and the `SubItemIcon` spinner at L428-435):
```tsx
  if (state === 'active') {
    return (
      <SVGInline
        svg={spinnerIcon}
        aria-hidden="true"
        className={classNames(styles.icon, styles.iconSpinner)}
      />
    );
  }
```
`spinnerIcon` is already imported (L5); `SVGInline` (L4) and `classNames` (L1) are already imported. **Do not** touch `SubItemIcon` (its `.subItemIconSpinner` already animates). **No scss edit** — `.iconSpinner` (L71-77) + reduced-motion (L306-311) already exist.

### Step 2 — Populated `stopping-node` step (`MithrilStepIndicator.tsx`)

In `STATUS_TO_STEP` (live **L44-55**) add a leading entry so the node-stop window lights up the first visible step instead of leaving all three greyed:
```ts
const STATUS_TO_STEP: Partial<Record<
  MithrilBootstrapStatus | MithrilPartialSyncStatus,
  StepId
>> = {
  'stopping-node': 'preparing',
  preparing: 'preparing',
  downloading: 'downloading',
  …
};
```
Effect: `getActiveStepIndex('stopping-node')` now returns `0`, so `deriveTopLevelState` makes `preparing` **active** (rendering the Step-1 spinner from Step 1) while `downloading`/`finalizing` stay pending. Type-safe (the record is keyed by the union). Bootstrap never has `status === 'stopping-node'`, so this is inert for bootstrap (#11).

### Step 3 — Add the renderer-side `startedAt` anchor (`MithrilPartialSyncStore.ts`)

3a. Add the observable next to the other progress observables (after L58 `elapsedSeconds`):
```ts
  @observable startedAt: number | null = null;
```

3b. In `_updateStatus` (live **L146-166**), capture the previous status FIRST and add the anchor logic. Replace the opening of the method:
```ts
  @action
  _updateStatus = (update: MithrilPartialSyncStatusSnapshot) => {
    if (this._isTornDown) {
      return;
    }

    this.status = update.status;
```
with:
```ts
  @action
  _updateStatus = (update: MithrilPartialSyncStatusSnapshot) => {
    if (this._isTornDown) {
      return;
    }

    const previousStatus = this.status;
    this.status = update.status;

    // Renderer-side elapsed anchor (mirrors MithrilBootstrapStore). A fresh
    // working run re-anchors to THIS run; we then stamp the anchor on the first
    // working frame (honoring any backend elapsedSeconds so a re-attach to an
    // in-flight op shows the true elapsed); the anchor is released only when
    // fully idle so terminal overlays keep their frozen elapsed value.
    const isWorkingNow = isMithrilPartialSyncWorkingStatus(this.status);
    if (isWorkingNow && !isMithrilPartialSyncWorkingStatus(previousStatus)) {
      this.startedAt = null;
    }
    if (isWorkingNow && this.startedAt == null) {
      const backendElapsed = update.transferProgress.elapsedSeconds;
      this.startedAt =
        typeof backendElapsed === 'number' &&
        Number.isFinite(backendElapsed) &&
        backendElapsed > 0
          ? Date.now() - backendElapsed * 1000
          : Date.now();
    }
    if (this.status === 'idle') {
      this.startedAt = null;
    }
```
`isMithrilPartialSyncWorkingStatus` is already imported (L8-13). Leave the rest of `_updateStatus` (L152-166) unchanged. **Rationale for each branch:** (i) idle/terminal→working resets so elapsed starts at `0:00` for the new run; (ii) the first working frame stamps the anchor; (iii) working→completed/failed/cancelled keeps `startedAt` non-null so `MithrilProgressView` freezes the final value (see Step 5a); (iv) →idle (the dismiss-driven reset) releases it.

### Step 4 — Thread `startedAt` overlay → progress view (`MithrilPartialSyncOverlay.tsx`)

4a. Add to `Props` (after `transferProgress?: …`, L22):
```ts
  startedAt?: number | null;
```
4b. Destructure it in the function body (with the other props, L52-66): add `startedAt,`.
4c. In the `<MithrilProgressView …>` element: **delete L93** `elapsedSeconds={transferProgress?.elapsedSeconds}` and add `bootstrapStartedAt={startedAt}` plus the two stopping-node overrides (mirrors the existing `startingNodeTitle`/`startingNodeDetail` at L107-112):
```tsx
              filesDownloaded={transferProgress?.filesDownloaded}
              filesTotal={transferProgress?.filesTotal}
              ancillaryBytesDownloaded={
                transferProgress?.ancillaryBytesDownloaded
              }
              ancillaryBytesTotal={transferProgress?.ancillaryBytesTotal}
              bootstrapStartedAt={startedAt}
              …
              startingNodeTitle={…}      // unchanged
              startingNodeDetail={…}     // unchanged
              stoppingNodeTitle={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncNodeStoppingTitle
              )}
              stoppingNodeDetail={intl.formatMessage(
                MithrilBootstrapMessages.partialSyncNodeStoppingDetail
              )}
```
Do **not** touch `hideAction={status === 'starting-node'}` or `onAction` (task-ux-404). Leave the ancillary props and `showDownloadProgressBar` as-is.

### Step 5 — Progress-view affordances (`MithrilProgressView.tsx`)

5a. **Freeze elapsed on the completed frame (BOTH flows).** Add `'completed'` to `TERMINAL_STATUSES` (live **L43-45**) so the renderer clock stops once the run reaches the `completed` frame:
```ts
const TERMINAL_STATUSES = new Set<
  MithrilBootstrapStatus | MithrilPartialSyncStatus
>(['failed', 'cancelled', 'completed']);
```
**#11 truthful rationale — the freeze DOES apply to bootstrap; it is explicitly ACCEPTED, not inert.** `completed` IS a `MithrilBootstrapStatus` and a `WORKING_STATUS` (`MithrilBootstrap.tsx:67-76`), and bootstrap renders this SHARED `MithrilProgressView` for it with `status==='completed'` + `bootstrapStartedAt` (`MithrilBootstrap.tsx:190-204`). `MithrilBootstrapStore` keeps `bootstrapStartedAt` non-null through `completed` — it is cleared only on a decision-cycle status or on `preparing`-after-terminal (`MithrilBootstrapStore.ts:119-148`). So **today the elapsed clock ticks on the bootstrap `completed` frame; after this change it FREEZES** at its last value. This is accepted as **correct, not a regression**: (i) a success/handoff frame must not keep climbing an elapsed counter; (ii) `completed` is transient — `starting-node` (NOT terminal, same un-cleared anchor) immediately follows and resumes the live tick from the same `bootstrapStartedAt`, so the only visible effect is a momentary freeze on the brief completed frame. (The pre-existing `MithrilProgressView.spec.tsx:84,106` completed tests pass no `bootstrapStartedAt`, so they read `0:00` and never asserted the timer — unaffected.) The freeze is proven by the new `MithrilProgressView.spec.tsx` completed-freeze test (Step 10b), which renders the exact shared component with `status:'completed'` + the `bootstrapStartedAt` bootstrap mounts. The `MithrilBootstrap.spec.tsx` completed-handoff re-run (`MithrilBootstrap.spec.tsx:207-213`) remains the integration regression (it passes no anchor, so it stays `0:00`).

5b. **Add the long-phase set** near `TERMINAL_STATUSES`:
```ts
const LONG_RUNNING_STATUSES = new Set<
  MithrilBootstrapStatus | MithrilPartialSyncStatus
>(['verifying', 'unpacking', 'converting', 'installing', 'finalizing']);
```

5c. **Add the two new props** to `interface Props` (after `startingNodeDetail?: string;`, L33):
```ts
  stoppingNodeTitle?: string;
  stoppingNodeDetail?: string;
```
and destructure them (after `startingNodeDetail,`, L80): `stoppingNodeTitle, stoppingNodeDetail,`.

5d. **Add the derived flags** next to `isStartingNode` (L86):
```ts
  const isStartingNode = status === 'starting-node';
  const isStoppingNode = status === 'stopping-node';
  const isLongRunningPhase = LONG_RUNNING_STATUSES.has(status);
```

5e. **Reassurance line** — directly after the `timerDisplay` block (closes at L129), before `waterfallContainer` (L131):
```tsx
      {isLongRunningPhase && (
        <p className={styles.reassurance} aria-live="polite">
          {intl.formatMessage(messages.progressLongPhaseReassurance)}
        </p>
      )}
```
(Use `aria-live="polite"` WITHOUT `role="status"` so it never collides with the `getByRole('status')` used by the starting/stopping blocks — those never co-occur with a long phase.)

5f. **Stopping-node block** — mirror the `isStartingNode` block (L145-166) and place it immediately before it:
```tsx
      {isStoppingNode && (
        <div
          className={styles.completionBlock}
          role="status"
          aria-live="polite"
          aria-atomic="true"
        >
          <h2 className={styles.completionTitle}>
            {stoppingNodeTitle ||
              intl.formatMessage(messages.nodeStoppingTitle)}
          </h2>
          <p className={styles.completionDetail}>
            {stoppingNodeDetail ||
              intl.formatMessage(messages.nodeStoppingDetail)}
          </p>
          <SVGInline
            svg={spinnerIcon}
            className={styles.completionSpinner}
            aria-hidden="true"
          />
        </div>
      )}
```
`spinnerIcon` (L11) and `SVGInline` (L3) are already imported. Reuse the existing `completionBlock`/`completionTitle`/`completionDetail`/`completionSpinner` styles. Do **not** change `disabled={isStartingNode}` on the Cancel button (task-ux-404 owns disabling during stop).

### Step 6 — Reassurance style (`MithrilProgressView.scss`)

Add a `.reassurance` rule (place after `.timerDisplay`/`.timerValue`, ~L52). Keep it minimal and theme-consistent:
```scss
.reassurance {
  color: var(--theme-mithril-secondary-text-color);
  font-size: 13px;
  line-height: 1.5;
  margin: -8px 0 16px;
}
```
> This file is **not** in the JSON `targetPaths` (which listed `MithrilStepIndicator.scss`), but the reassurance `<p>` lives in `MithrilProgressView`, so its style belongs here. This mirrors task-ux-401 touching `MithrilBootstrapPage.tsx`/storybook beyond the JSON list when the seam required it. `MithrilStepIndicator.scss` itself needs no edit (Step 1 reconciliation).

### Step 7 — Thread `startedAt` at the overlay mount (`App.tsx`)

In the `<MithrilPartialSyncOverlay …>` element (L98-122):
- Add a prop: `startedAt={mithrilPartialSync.startedAt}` (e.g. next to `progressItems` L100).
- **Delete** the now-dead `elapsedSeconds: mithrilPartialSync.elapsedSeconds,` line inside the `transferProgress={{…}}` literal (L104). The overlay no longer reads it; the store observable may remain (harmless; removing it is out of scope).
> `App.tsx` is an `@observer`, so reading `mithrilPartialSync.startedAt` is reactive; it changes only at status transitions, while the per-second tick stays local to `MithrilProgressView` (no app-wide re-render each second).

### Step 8 — Messages (`MithrilBootstrap.messages.ts`)

Add **five** new entries inside the `defineMessages({ … })` object (keep `!!!` defaults per the live convention). Group the bootstrap-scoped ones near `nodeStartingTitle`/`nodeStartingDetail` (L247-259) and the partial-sync-scoped ones near `partialSyncNodeStartingDetail` (L279-285):
```ts
  progressLongPhaseReassurance: {
    id: 'loading.mithrilBootstrap.progress.longPhaseReassurance',
    defaultMessage:
      '!!!This can take several minutes — Daedalus is still working.',
    description:
      'Reassurance line shown during long non-download Mithril phases (verifying/unpacking/converting/installing/finalizing) so the dialogue never looks frozen.',
  },
  nodeStoppingTitle: {
    id: 'loading.mithrilBootstrap.progress.nodeStoppingTitle',
    defaultMessage: '!!!Stopping Cardano node...',
    description:
      'Title shown for the in-dialogue node-stop frame (default; bootstrap fallback).',
  },
  nodeStoppingDetail: {
    id: 'loading.mithrilBootstrap.progress.nodeStoppingDetail',
    defaultMessage:
      '!!!Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes.',
    description:
      'Detail copy shown for the in-dialogue node-stop frame (default; bootstrap fallback).',
  },
  partialSyncNodeStoppingTitle: {
    id: 'loading.mithrilPartialSync.progress.nodeStoppingTitle',
    defaultMessage: '!!!Stopping Cardano node...',
    description:
      'Title shown while the Cardano node is being stopped before Mithril partial sync restores chain data.',
  },
  partialSyncNodeStoppingDetail: {
    id: 'loading.mithrilPartialSync.progress.nodeStoppingDetail',
    defaultMessage:
      '!!!Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes.',
    description:
      'Detail copy shown while the Cardano node is being stopped before Mithril partial sync restores chain data.',
  },
```
> The bootstrap-scoped `nodeStopping*` are the `MithrilProgressView` fallbacks (Step 5f `|| intl.formatMessage(messages.nodeStopping*)`), mirroring the `nodeStarting*` defaults. They are effectively partial-sync-only today (bootstrap never reaches `stopping-node`) but are kept for symmetry with the proven `isStartingNode` block; task-ux-501 (i18n hygiene) may prune if it confirms they are dead.

### Step 9 — i18n catalogs

9a. Run `yarn i18n:extract` to regenerate `translations/messages.json` with the five new ids (this is the ONLY file extract writes).

9b. Add the five first-class **EN** strings to `source/renderer/app/i18n/locales/en-US.json`, inserted in alphabetical key order among the `loading.mithrilBootstrap.progress.*` / `loading.mithrilPartialSync.progress.*` keys (the catalog is key-sorted; neighbors: `…progress.elapsedLabel` L320, `…progress.nodeStartingDetail` L322 for the bootstrap block; `…mithrilPartialSync.progress.nodeStartingTitle` L356 for the partial-sync block):
```
"loading.mithrilBootstrap.progress.longPhaseReassurance": "This can take several minutes — Daedalus is still working.",
"loading.mithrilBootstrap.progress.nodeStoppingDetail": "Daedalus is stopping the Cardano node so it can restore verified chain data. This can take a couple of minutes.",
"loading.mithrilBootstrap.progress.nodeStoppingTitle": "Stopping Cardano node...",
"loading.mithrilPartialSync.progress.nodeStoppingDetail": "Daedalus is stopping the Cardano node before restoring verified Mithril chain data. This can take a couple of minutes.",
"loading.mithrilPartialSync.progress.nodeStoppingTitle": "Stopping Cardano node...",
```

9c. Add the five first-class **JA** strings to `source/renderer/app/i18n/locales/ja-JP.json` (same alphabetical slots; no `!!!`):
```
"loading.mithrilBootstrap.progress.longPhaseReassurance": "数分かかることがあります。Daedalusは引き続き処理しています。",
"loading.mithrilBootstrap.progress.nodeStoppingDetail": "Daedalusは検証済みのチェーンデータを復元するために、Cardanoノードを停止しています。数分かかることがあります。",
"loading.mithrilBootstrap.progress.nodeStoppingTitle": "Cardanoノードを停止しています...",
"loading.mithrilPartialSync.progress.nodeStoppingDetail": "Daedalusは検証済みのMithrilチェーンデータを復元する前に、Cardanoノードを停止しています。数分かかることがあります。",
"loading.mithrilPartialSync.progress.nodeStoppingTitle": "Cardanoノードを停止しています...",
```
Provide first-class JA (no `!!!`); the holistic JA pass is task-ux-601. Validate ids + ordering with `yarn i18n:check` (or `yarn i18n:manage`).

9d. **Stage the regenerated i18n artifact.** `yarn i18n:check` / `yarn i18n:manage` runs `react-intl-translations-manager`, which regenerates `source/renderer/app/i18n/locales/defaultMessages.json` (and may touch the empty `whitelist_en-US.json` / `whitelist_ja-JP.json`) with the five new ids — task-ux-304 listed `defaultMessages.json` as an explicit i18n target. Commit the regenerated `defaultMessages.json` alongside `translations/messages.json` + `en-US.json` + `ja-JP.json` so the catalogs stay consistent; `git diff --stat` should show `defaultMessages.json` gaining the five ids. Do **not** hand-edit it (it is generated).

### Step 10 — Specs

10a. `MithrilStepIndicator.spec.tsx` — **flip** the locking test (L38-64). Rename it and replace the two assertions at **L61-62**:
```tsx
  it('renders a rotating spinner for the active top-level step while subitems keep their spinner', () => {
    // …same progressItems + renderComponent('downloading', { … }) …
    expect(downloadingStep).not.toBeNull();
    expect(downloadingStep?.querySelector('.iconSpinner')).not.toBeNull();
    expect(downloadingStep?.querySelector('.activeCircle')).toBeNull();
    expect(downloadingDetails.querySelector('svg')).not.toBeNull();
  });
```
(`.iconSpinner` selects only the top-level icon — the token differs from the sub-item `.subItemIconSpinner`.) Then **ADD** a populated-stopping-node test:
```tsx
  it('renders the preparing step as active during stopping-node (no greyed placeholders)', () => {
    renderComponent('stopping-node', { progressItems: [] });
    const preparingStep = screen
      .getByText(/preparing$/i)
      .closest('[role="listitem"]');
    expect(preparingStep).toHaveClass('stepActive');
    expect(preparingStep).not.toHaveClass('stepPending');
    expect(preparingStep?.querySelector('.iconSpinner')).not.toBeNull();
  });
```
(`renderComponent` types `status` as `MithrilBootstrapStatus`; cast the call `renderComponent('stopping-node' as any, …)` or widen the param type to `MithrilBootstrapStatus | MithrilPartialSyncStatus` — prefer widening the local `renderComponent` signature.)

10b. `MithrilProgressView.spec.tsx` — ADD three tests:
```tsx
  afterEach(() => jest.useRealTimers());

  it('ticks elapsed every second from the start anchor (advances from 0:00 immediately)', () => {
    jest.useFakeTimers();           // Jest 27 modern timers mock Date.now
    jest.setSystemTime(0);
    renderComponent({ status: 'verifying', bootstrapStartedAt: 0 });
    expect(screen.getByText('0:00')).toBeInTheDocument();
    act(() => { jest.advanceTimersByTime(3000); });
    expect(screen.getByText('0:03')).toBeInTheDocument();
  });

  it('shows the long-phase reassurance for verifying/finalizing', () => {
    renderComponent({ status: 'finalizing', bootstrapStartedAt: Date.now() });
    expect(
      screen.getByText(/this can take several minutes/i)
    ).toBeInTheDocument();
  });

  it('shows the in-dialogue node-stop frame while stopping-node', () => {
    renderComponent({ status: 'stopping-node', bootstrapStartedAt: Date.now() });
    expect(
      screen.getByRole('heading', { name: /stopping cardano node/i })
    ).toBeInTheDocument();
    expect(screen.getByRole('status')).toHaveAttribute('aria-live', 'polite');
  });

  // Covers BOTH flows' completed frame: bootstrap mounts this SAME shared
  // component with status:'completed' + bootstrapStartedAt (MithrilBootstrap.tsx:190-204),
  // so this is the bootstrap completed-frame timer-freeze proof for #11 (Step 5a).
  it('freezes elapsed on the completed frame instead of ticking', () => {
    jest.useFakeTimers();
    jest.setSystemTime(0);
    renderComponent({ status: 'completed', bootstrapStartedAt: 0 });
    expect(screen.getByText('0:00')).toBeInTheDocument();
    act(() => { jest.advanceTimersByTime(5000); });
    expect(screen.getByText('0:00')).toBeInTheDocument();      // no tick on terminal frame
    expect(screen.queryByText('0:05')).not.toBeInTheDocument();
  });
```
Import `act` from `@testing-library/react` and widen the `renderComponent` `status` union to include `'stopping-node'` and `'verifying'`. These statuses already pass cleanly into the component (`MithrilBootstrapStatus | MithrilPartialSyncStatus`).

10c. `MithrilPartialSyncStore.spec.ts` — ADD anchoring tests (the file already calls `jest.useFakeTimers()` at module top, so `jest.setSystemTime` is available):
```ts
  it('anchors a renderer-side startedAt when entering a working status', () => {
    const store = setupStore();
    jest.setSystemTime(10_000);
    store._updateStatus({
      status: 'stopping-node',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBe(10_000);
  });

  it('anchors startedAt to the backend elapsed when present', () => {
    const store = setupStore();
    jest.setSystemTime(100_000);
    store._updateStatus({
      status: 'downloading',
      allowedRecoveryActions: [],
      transferProgress: { elapsedSeconds: 12 },
      progressItems: [],
      error: null,
    });
    expect(store.startedAt).toBe(100_000 - 12_000);
  });

  it('keeps startedAt stable across a run, frozen on terminal, released on idle', () => {
    const store = setupStore();
    jest.setSystemTime(5_000);
    store._updateStatus({ status: 'stopping-node', allowedRecoveryActions: [], transferProgress: {}, progressItems: [], error: null });
    jest.setSystemTime(9_000);
    store._updateStatus({ status: 'downloading', allowedRecoveryActions: [], transferProgress: {}, progressItems: [], error: null });
    expect(store.startedAt).toBe(5_000);                 // stable through the run
    store._updateStatus({ status: 'completed', allowedRecoveryActions: [], transferProgress: {}, progressItems: [], error: null });
    expect(store.startedAt).toBe(5_000);                 // frozen on terminal
    store._updateStatus({ status: 'idle', allowedRecoveryActions: [], transferProgress: {}, progressItems: [], error: null });
    expect(store.startedAt).toBeNull();                  // released on idle
    jest.setSystemTime(20_000);
    store._updateStatus({ status: 'stopping-node', allowedRecoveryActions: [], transferProgress: {}, progressItems: [], error: null });
    expect(store.startedAt).toBe(20_000);                // re-anchors next run
  });
```

10d. `MithrilPartialSyncOverlay.spec.tsx` — **EDIT REQUIRED (this spec WILL break otherwise).** Its first test (`MithrilPartialSyncOverlay.spec.tsx:42-49`, "renders the progress overlay with partial sync copy") renders the default `transferProgress={{ …, elapsedSeconds: 65 }}` and asserts `screen.getByText('1:05')` (L48). Step 4c **deletes** the overlay's `elapsedSeconds={transferProgress?.elapsedSeconds}` pass-through and the test passes no `startedAt`, so `MithrilProgressView` would receive `elapsedSecondsProp=undefined` + `bootstrapStartedAt=undefined` and render `0:00`, not `1:05` → assertion fails. Fix by anchoring the timer through the NEW prop so the test now proves the overlay→`MithrilProgressView` `startedAt`→`bootstrapStartedAt` threading end-to-end: in the `renderComponent` default props (L15-34), **add** `startedAt={Date.now() - 65_000}` and **drop** the now-dead `elapsedSeconds: 65` from the default `transferProgress` literal (L21). Keep the `getByText('1:05')` assertion at L48.
```tsx
        <MithrilPartialSyncOverlay
          status="downloading"
          progressItems={[]}
          startedAt={Date.now() - 65_000}        // NEW: anchors the renderer clock
          transferProgress={{
            filesDownloaded: 3,
            filesTotal: 9,
            // elapsedSeconds removed — overlay no longer passes it (Step 4c)
            ancillaryBytesDownloaded: 1,
            ancillaryBytesTotal: 2,
          }}
          …
```
(Real timers are fine: the effect's synchronous `tick()` computes `floor((Date.now() − (Date.now()−65_000)) / 1000) === 65` → `1:05` on mount; the two `Date.now()` calls differ by sub-millisecond. The other tests in this file override `status` to `failed`/`completed`/`cancelled` and never assert the timer, so the default `startedAt` is inert for them.)

10e. Re-run (no edits expected): `App.spec.tsx` (overlay is mocked at `App.spec.tsx:109-112`; the new `startedAt` prop just flows into the mock and the assertion uses `expect.objectContaining` at L236-246, so it is tolerated — **optional fidelity polish**: add `startedAt: undefined` to the idle store mock at L144-162 and a `startedAt` value to the failed store mock at L201-219, non-blocking), `MithrilBootstrap.spec.tsx` (bootstrap regression — its completed-handoff test at L207-213 passes no anchor so stays `0:00`; the completed-frame timer freeze is proven by the Step 10b `MithrilProgressView.spec.tsx` test on the same shared component).

---

## Acceptance criteria (VERBATIM from the tasks JSON)

- Active step circle animates (rotating spinner) in both flows; the locking spec is updated.
- Elapsed ticks continuously via a renderer-side timer anchored to startedAt/bootstrapStartedAt.
- Long phases and the node-stop window never look frozen.
- No new IPC throughput math; no bootstrap regression.

### testCases (VERBATIM from the tasks JSON)

- Active top-level step renders the rotating spinner in both flows (spec updated).
- Elapsed ticks continuously via the renderer-side timer (no freeze between events; advances from 0:00 immediately).
- Long phases show an active indicator + elapsed + reassurance.
- stopping-node shows a populated step + reassurance block, not greyed placeholders.
- Reduced-motion preference still suppresses animation.

---

## Verification plan (exact commands, from repo root `/workspaces/mithril-partial-sync-ux`)

```bash
cd /workspaces/mithril-partial-sync-ux
yarn i18n:extract            # regenerate translations/messages.json with the 5 new ids
yarn i18n:check              # validate ids exist in en-US/ja-JP + ordering; regenerates defaultMessages.json
git status --porcelain source/renderer/app/i18n/locales/defaultMessages.json
                            # → defaultMessages.json shows as modified; STAGE it (5 new ids, generated — do not hand-edit)
grep -c 'progress.nodeStopping\|progress.longPhaseReassurance' source/renderer/app/i18n/locales/defaultMessages.json
                            # → 5 (longPhaseReassurance + 4 nodeStopping* ids — 2 bootstrap-scoped + 2 partial-sync-scoped — landed in the generated catalog)
grep -n '!!!' source/renderer/app/i18n/locales/en-US.json source/renderer/app/i18n/locales/ja-JP.json
                            # → no NEW !!! for longPhaseReassurance / nodeStopping* keys
yarn compile                # tsc --noEmit must pass (allow up to 600s)
yarn lint                   # ESLint clean on touched files
yarn test:jest \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx \
  source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx \
  source/renderer/app/stores/MithrilPartialSyncStore.spec.ts \
  source/renderer/app/App.spec.tsx
yarn prettier:check         # (or prettier --check on the touched files)
grep -n "activeCircle" source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx
                            # → no match in TopLevelIcon (spinner replaced the dot)
grep -n "elapsedSeconds=" source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx
                            # → no match (frozen prop no longer passed)
```

> **KNOWN ENV WORKAROUND (recorded by task-ux-301/303/304/401):** `yarn compile`'s `typedef:sass` precompile hook and jest's css-modules transform crash under **Node v24 dart-sass** ("Invalid or unexpected token"). If hit:
> - Run the authoritative TS gate directly: `node_modules/.bin/tsc --noEmit -p .` (allow up to 600s).
> - Run jest with `.scss` mapped to `identity-obj-proxy` via a **CLI-only** `--moduleNameMapper` override (the gitignored sidecar config the prior tasks used; do NOT stage it). The spec selectors `.iconSpinner` / `.activeCircle` / `.stepActive` rely on `identity-obj-proxy` mapping `styles.X → 'X'`, which is how the existing `MithrilStepIndicator.spec.tsx` already runs green.
> No new stories are added (task-ux-502 owns Storybook), so storybook is not run here. All new `MithrilProgressView` props are OPTIONAL, so the storybook `MithrilProgressView.stories.tsx` continues to compile unchanged.

### Tests mapped to JSON testCases

- **Active spinner, both flows** → `MithrilStepIndicator.spec.tsx` flipped test (`.iconSpinner` not null, `.activeCircle` null); applies to bootstrap and partial-sync statuses (shared component). The `MithrilBootstrap.spec.tsx` re-run is the bootstrap-side regression proof (#11).
- **Elapsed ticks from 0:00, no freeze** → `MithrilProgressView.spec.tsx` fake-timer tick test (`0:00` → `0:03`) + `MithrilPartialSyncStore.spec.ts` anchoring tests (anchor on working frame, backend-elapsed anchor, stable/frozen/released lifecycle).
- **Long phases show indicator + elapsed + reassurance** → `MithrilProgressView.spec.tsx` reassurance test (the spinner = indicator, timer = elapsed, reassurance text asserted).
- **stopping-node populated step + reassurance block** → `MithrilStepIndicator.spec.tsx` `stepActive` test + `MithrilProgressView.spec.tsx` node-stop-frame test.
- **Reduced-motion suppresses animation** → static assertion: the active icon uses `styles.iconSpinner`, which the existing `@media (prefers-reduced-motion: reduce)` block (scss L306-311) already sets to `animation: none`. No JS toggle to test; the reviewer confirms no new animation class escapes the guard (jsdom does not evaluate `@media`, so this is a code-review check, not a unit assertion).

---

## Risks / open questions

- **Bootstrap regression (#11).** Shared components change. Mitigations: `stopping-node` is not a `MithrilBootstrapStatus` (the `STATUS_TO_STEP['stopping-node']` map entry is never consulted for bootstrap); the spinner swap is symmetric with the proven sub-item spinner; all new `MithrilProgressView` props are optional. **The `completed` freeze (Step 5a) is NOT inert for bootstrap** — bootstrap renders the shared `MithrilProgressView` for `status==='completed'` with a non-null `bootstrapStartedAt` (`MithrilBootstrap.tsx:67-76,190-204`; `MithrilBootstrapStore.ts:119-148`), so its elapsed clock, which ticks on the completed frame today, will FREEZE there after this change. This is explicitly **accepted as correct** (a success/handoff frame should not climb an elapsed counter; `completed` is transient and `starting-node` resumes the live tick from the same anchor) — see Step 5a. Guard = the Step 10b `MithrilProgressView.spec.tsx` completed-freeze test (shared component, the bootstrap completed-frame proof) + re-running `MithrilBootstrap.spec.tsx` + `MithrilStepIndicator.spec.tsx` + `MithrilProgressView.spec.tsx` green.
- **Fake-timer tick test brittleness.** Relies on Jest 27 **modern** fake timers (default) mocking `Date.now()` so `advanceTimersByTime(3000)` advances the clock. Confirmed: `jest@27.5.1`; `NetworkStatusStore` already uses `setSystemTime`. If a future Jest config forces legacy timers, the tick test must `jest.spyOn(Date, 'now')` instead.
- **`completed` keeps `MithrilProgressView` ticking?** No — Step 5a adds `completed` to `TERMINAL_STATUSES`, so the success/handoff frame freezes elapsed at its last value (the partial-sync store keeps `startedAt`, and the bootstrap store keeps `bootstrapStartedAt`, non-null through terminal; released only on `idle`/decision-cycle respectively). Without 5a the timer would keep incrementing on the success screen. This applies to **both** flows — including the bootstrap `completed` frame (accepted, see Step 5a / the #11 risk bullet above) — and is covered by the Step 10b completed-freeze test.
- **`completionSpinner` (the big stop/start block spinner) is NOT reduced-motion-guarded** in `MithrilProgressView.scss` (its local `spin` keyframe, L92-93/169-177, has no `@media reduce`). This is **pre-existing** (the `isStartingNode` block already uses it) and out of scope; the task's reduced-motion requirement is specifically the top-level **step** spinner (`.iconSpinner`), which IS guarded. Flag for task-ux-601/502 if a holistic a11y pass wants it.
- **Two bootstrap-scoped `nodeStopping*` defaults may be dead** (bootstrap never reaches `stopping-node`). Kept for `isStartingNode` symmetry; task-ux-501 may prune. Not blocking.

---

## Required doc / research updates

- **No** `.agent/system/api-endpoints.md` change (no IPC/contract change; elapsed is renderer-derived).
- At completion: fill "Final outcome", set the JSON task `status: completed` (+ `completedAt`), and record durable findings in `task-ux-402-research.md` (esp. the anchor drift table, the `startedAt` anchor mirroring bootstrap, and the reduced-motion-already-handled reconciliation that removed the expected `MithrilStepIndicator.scss` edit).

## Review-log paths

- Planning review: `task-plans-ux-refinement/phase-4/task-ux-402-plan-review.md`
- Implementation review: `task-plans-ux-refinement/phase-4/task-ux-402-impl-review.md`
- Research note: `task-plans-ux-refinement/phase-4/task-ux-402-research.md`

## Final outcome

**Status: COMPLETED 2026-06-26. Planning `approved`, build `completed`. Code review: approved (impl-review log, 2026-06-26T11:20:22Z). Single commit subject as planned.**

### What was built (all four affordances, in both flows, pure seam reuse)

1. **Rotating active top-level step.** `MithrilStepIndicator.TopLevelIcon` active branch
   (`MithrilStepIndicator.tsx:404-409`) now renders the existing `spinnerIcon` via `SVGInline` with
   `classNames(styles.icon, styles.iconSpinner)` instead of the static `.activeCircle` dot. Shared
   component → applies to bootstrap AND partial-sync. **No scss edit** — `.iconSpinner` (scss L71-77)
   and the reduced-motion guard naming `.iconSpinner` (scss L306-311) already existed, so reduced-motion
   stays honored with no new animation class.
2. **Populated stopping-node step.** `STATUS_TO_STEP['stopping-node'] = 'preparing'` added as the leading
   entry (`MithrilStepIndicator.tsx:48`), so `getActiveStepIndex('stopping-node') = 0` lights the
   `preparing` step active/spinning instead of three greyed placeholders. Inert for bootstrap (bootstrap
   never has `status === 'stopping-node'`).
3. **Renderer-side ticking elapsed.** `@observable startedAt: number | null` added to
   `MithrilPartialSyncStore` (L59) with anchor logic in `_updateStatus` (L153-177) mirroring
   `MithrilBootstrapStore`: reset on non-working→working, stamp on first working frame honoring backend
   `elapsedSeconds`, release on `idle`. Threaded store `startedAt` → `App.tsx`
   (`startedAt={mithrilPartialSync.startedAt}`) → overlay `startedAt` prop →
   `MithrilProgressView bootstrapStartedAt` (the proven `setInterval` ticking effect). The frozen
   `elapsedSeconds={transferProgress?.elapsedSeconds}` overlay pass-through and the dead `App.tsx`
   `elapsedSeconds:` literal were removed. Elapsed advances from 0:00 immediately and never freezes
   between backend events.
4. **Freeze elapsed on completed.** `'completed'` added to `TERMINAL_STATUSES` in `MithrilProgressView`
   (L46-48) so the success/handoff frame freezes elapsed at its last value (both flows; accepted per
   Step 5a — `completed` is transient and `starting-node` resumes the live tick from the same anchor).
5. **Long-phase reassurance.** `LONG_RUNNING_STATUSES` (verifying/unpacking/converting/installing/
   finalizing) drives a one-line `<p aria-live="polite">` reassurance under the timer (no `role="status"`
   to avoid colliding with the start/stop blocks' `getByRole('status')`); new `.reassurance` style in
   `MithrilProgressView.scss`.
6. **Stopping-node reassurance block.** `isStoppingNode` block mirrors the `isStartingNode` block
   (`role="status"`, `aria-live="polite"`, `aria-atomic`, completion spinner) with two optional override
   props (`stoppingNodeTitle`/`stoppingNodeDetail`) wired from the overlay. Cancel `disabled`/`hideAction`
   wiring left untouched (task-ux-404 owns disabling during stop).
7. **i18n.** Five new message ids (EN+JA first-class, no `!!!` in runtime catalogs): bootstrap-scoped
   `progress.longPhaseReassurance`, `progress.nodeStoppingTitle`, `progress.nodeStoppingDetail`; and
   partial-sync-scoped `progress.nodeStoppingTitle`, `progress.nodeStoppingDetail`. `yarn i18n:extract` +
   `yarn i18n:check` regenerated `translations/messages.json` and `defaultMessages.json`.

### Files changed (16 in working tree; `.gitignore` env line is pre-existing/out-of-scope)

- `MithrilStepIndicator.tsx` — spinner active branch; `STATUS_TO_STEP['stopping-node'] = 'preparing'`.
- `MithrilProgressView.tsx` — `completed`→TERMINAL_STATUSES; LONG_RUNNING_STATUSES;
  `stoppingNodeTitle`/`stoppingNodeDetail` props; `isStoppingNode`/`isLongRunningPhase` flags;
  reassurance line; stopping-node block.
- `MithrilProgressView.scss` — `.reassurance` rule.
- `MithrilPartialSyncOverlay.tsx` — `startedAt` prop; `bootstrapStartedAt={startedAt}`; dropped
  `elapsedSeconds=`; stopping-node title/detail overrides.
- `MithrilPartialSyncStore.ts` — `startedAt` observable + anchor logic.
- `App.tsx` — `startedAt={mithrilPartialSync.startedAt}`; removed dead `elapsedSeconds:` from the
  `transferProgress` literal.
- `MithrilBootstrap.messages.ts` — 5 new messages.
- `en-US.json`, `ja-JP.json` — 5 first-class strings each; `defaultMessages.json`,
  `translations/messages.json` — regenerated.
- Specs: `MithrilStepIndicator.spec.tsx`, `MithrilProgressView.spec.tsx`,
  `MithrilPartialSyncStore.spec.ts`, `MithrilPartialSyncOverlay.spec.tsx`.

### Verification results (actual)

- `node_modules/.bin/tsc --noEmit -p .` → **EXIT 0** (the authoritative TS gate; `yarn compile`'s
  `typedef:sass` hook crashes under Node v24 dart-sass — single-file `typed-scss-modules` regen of the
  stale gitignored `MithrilProgressView.scss.d.ts` was needed first).
- Focused jest (`MithrilStepIndicator`, `MithrilProgressView`, `MithrilPartialSyncOverlay`,
  `MithrilBootstrap`, `MithrilPartialSyncStore`, `App`) → **6 suites / 64 tests PASS, EXIT 0**
  (reviewer reproduced on the committed config; no sidecar needed).
- `eslint` on touched files → **0 errors** (pre-existing mobx-decorator/`@ts-ignore`/`any` warnings only).
- `prettier --check` on touched ts/scss + i18n catalogs → **clean**.
- Grep gates: no `activeCircle` in `MithrilStepIndicator.tsx`; no `elapsedSeconds=` in
  `MithrilPartialSyncOverlay.tsx`; `defaultMessages.json` has 5 new ids; no NEW `!!!` for the new keys in
  en-US/ja-JP.

### testCase → evidence

- Active spinner both flows → `MithrilStepIndicator.spec.tsx` flipped lock (`.activeCircle` gone,
  `.iconContainer svg` present); shared component covers bootstrap + partial-sync.
- Elapsed ticks from 0:00, no freeze → `MithrilProgressView.spec.tsx` fake-timer tick test (0:00→0:03) +
  `MithrilPartialSyncStore.spec.ts` anchor tests (anchor on working frame, backend-elapsed anchor,
  stable/frozen/released lifecycle).
- Long phases show indicator + elapsed + reassurance → `MithrilProgressView.spec.tsx` reassurance test.
- stopping-node populated step + reassurance block → `MithrilStepIndicator.spec.tsx` `stepActive` test +
  `MithrilProgressView.spec.tsx` node-stop-frame test.
- Reduced-motion suppresses animation → static code-review check: active icon uses the already-guarded
  `styles.iconSpinner` (jsdom does not evaluate `@media`, so no unit assertion).

### Locked invariants — all preserved

- **#11 (no bootstrap regression):** `MithrilBootstrap.spec.tsx` + `MithrilStepIndicator.spec.tsx` +
  `MithrilProgressView.spec.tsx` green; `stopping-node` map entry inert for bootstrap. The only
  bootstrap-visible change is the `completed`-frame elapsed freeze, explicitly accepted (Step 5a) and
  proven by the new completed-freeze test on the shared component.
- **#8/#18 (no synthetic figures / no new IPC):** elapsed is `Math.floor((Date.now()-startedAt)/1000)` in
  the renderer; no new channel/field; reassurance is static copy.
- **Reduced-motion:** active icon uses `styles.iconSpinner`, already covered by the existing
  `@media (prefers-reduced-motion: reduce)` block — no new animation class.
- **#4:** `partialSyncProgressSubtitle` unchanged.

### Accepted deviation (documented in impl-review)

- The plan (Step 10a) pinned `querySelector('.iconSpinner')` for the active-spinner assertions, but
  `svg-jest`/`react-svg-inline` renders a bare `<svg>` with no className (same as the proven sub-item
  spinner). The two assertions were made truthful instead — assert `.activeCircle` is gone and the active
  step's `.iconContainer` renders an `<svg>`; the stopping-node test asserts `stepActive` + no
  `.pendingCircle`. The component change matches the plan verbatim.

### Residual (non-blocking, owned elsewhere)

- Two bootstrap-scoped `nodeStopping*` defaults are likely dead (bootstrap never reaches `stopping-node`);
  kept for `isStartingNode` symmetry — task-ux-501 may prune.
- The big `completionSpinner` (start/stop block) is pre-existing-unguarded for reduced-motion — flagged
  for task-ux-601/502, out of scope (the task's reduced-motion requirement is the top-level step spinner,
  which IS guarded).
- Backend emit of `stopping-node` remains optional polish owned by task-ux-203; it stays
  renderer-optimistic here.
