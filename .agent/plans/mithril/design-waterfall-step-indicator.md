# Design Spec: Vertical Waterfall Step Indicator

**Feature:** Mithril bootstrap waterfall step indicator  
**Created:** 2026-03-18  
**Revised:** 2026-03-23  
**Status:** implemented  
**Related design:** [Mithril Progress View Composition](design-progress-view-composition.md)

### Design Changelog

| Date | Change |
|---|---|
| 2026-04-15 | Changed combined progress bar weighting from fixed 85/15 to dynamic weights based on actual snapshot size and ancillary `bytes_total`; falls back to 95/5 when ancillary size is unavailable. Colors now reference per-theme `mithrilBootstrap` tokens instead of a single fixed dark palette. |
| 2026-03-23 | Merged the two progress bars ("Snapshot files" + "Fast State Sync") into a single combined "Snapshot Files and Fast Sync" bar (85 % snapshot / 15 % ancillary weighting). Extracted `InlineProgressBar` to its own component. Added 500 ms verification-handoff delay before synthesizing the verifying-digests active row. Top-level active icon changed from spinner to 12 px open dot (`activeCircle`). Updated completion delay from 3 s to 6 s. Colors now reference `mithrilBootstrap` theme tokens. |
| 2026-03-20 | Updated the spec to match the shipped waterfall: progress bars render only while `Downloading snapshot data` is active, verification uses sub-items without bars, and remaining-time copy was removed from the inline metadata rows. |
| 2026-03-18 | Established the 3-step vertical waterfall structure for Preparing, Downloading, and Finalizing, including connector rules, icon states, and inline progress bars. |
| 2026-03-18 | Finalized the component-level behavior and implemented it in `MithrilStepIndicator.tsx` through task-024h. |
| 2026-03-18 | Parent composition into `MithrilProgressView` landed through task-024i, so the waterfall design now reflects the live feature rather than a staged child-only component. |

---

## 1. Layout Diagram

```
┌─ MithrilStepIndicator (full width of MithrilProgressView content area) ──────┐
│                                                                               │
│  ┌──────────────────────────────────────────────────────────────────────┐    │
│  │ [●] Preparing                                          ← top-level   │    │
│  │  │                                                      step row      │    │
│  │  │  (no sub-items; no sub-content area)                              │    │
│  │  │                                                                    │    │
│  │ [◌] Downloading                               ← active/pending       │    │
│  │  │                                                                    │    │
│  │  │   ┌──────────────────────────────────────────┐  ← sub-content    │    │
│  │  │   │ [◌] Checking local disk info             │                    │    │
│  │  │   │ [◌] Fetching certificate chain           │                    │    │
│  │  │   │ [⟳] Downloading snapshot data  ← active (spinner)            │    │
│  │  │   │                                                                │    │
│  │  │   │  Snapshot Files and Fast Sync                                  │    │
│  │  │   │  ┤█████████████████░░░░░░░░░░░├  56%                           │    │
│  │  │   │  Snapshot files: 1.2 GB / 1.9 GB | Fast sync: 24 MB / 195 MB  │    │
│  │  │   │                                                               │    │
│  │  │   │ [○] Verifying digests                                        │    │
│  │  │   │ [○] Verifying database                                       │    │
│  │  │   │ [○] Computing verification message                           │    │
│  │  │   │ [○] Verifying snapshot signature                             │    │
│  │  │   └──────────────────────────────────────────┘                   │    │
│  │  │                                                                    │    │
│  │ [○] Finalizing                                                        │    │
│  │     (sub-items appear when Finalizing becomes active)                 │    │
│  └──────────────────────────────────────────────────────────────────────┘    │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘

Legend:
  [●]  completed checkmark (green)
  [⟳]  active spinner (white, rotating)
  [○]  pending grey circle
  [✕]  error red X (SVGInline close/error icon, tinted red)
```

### Column Grid

```
│← 32px →│← 12px gap →│←── remaining content width ──────────────────→│
   icon                   labels, sub-content, progress bars
   col

Sub-content indent from outer left:
  44px  (32px icon col + 12px gap = aligns with label start)

Sub-item icon column within sub-content:
  18px  icon col + 8px gap = 26px total before sub-item label

Connector line x-position:
  15px from outer left  (centered on 32px icon column)
```

---

## 2. Spacing and Sizing

### Component Outer

| Property | Value |
|---|---|
| Component width | 100% of MithrilProgressView content area |
| Layout basis | 720px card `max-width` (MithrilBootstrap `.card`) with 32px padding → ~656px content width. The waterfall consumes 100% of this available width. Below 720px viewport the card fills the viewport with 24px padding. |
| Max component width | none (bounded by card content area) |
| Margin bottom | 0 (MithrilProgressView drives inter-section gaps) |

### Top-Level Step Row

| Property | Value |
|---|---|
| Min-height | 36px |
| Vertical alignment | `align-items: center` |
| Gap (icon → label) | 12px |
| Padding top (first step) | 0 |
| Padding bottom (last step before finalize) | 0 |

### Icon Column (top-level)

| Property | Value |
|---|---|
| Container width | 32px |
| Container height | 32px |
| Container flex-shrink | 0 |
| Icon size (SVGInline) | 20px × 20px (set via font-size: 20px on .icon) |
| Pending circle diameter | 12px |
| Pending circle border-radius | 6px (50%) |

### Connector Line

| Property | Value |
|---|---|
| Width | 2px |
| Min-length | fills full distance between icon-col bottom and next icon-col top |
| X position | `left: 15px` (absolute within step wrapper, centered on icon) |
| Top | bottom of icon container (16px + 8px = 24px from step top, i.e., `top: 32px`) |
| Bottom | top of next step icon container |
| Includes sub-content | Yes — runs continuously through the sub-content area |

### Sub-Content Area

| Property | Value |
|---|---|
| Left indent | 44px (aligns with label column) |
| Padding top | 10px (space between step label row and first sub-item) |
| Padding bottom | 14px (space between last sub-item and next step icon) |
| Gap between sub-items | 7px |
| Gap between sub-items and progress bars | 12px |
| Combined progress bar bottom margin | 10px |

### Sub-Item Row

| Property | Value |
|---|---|
| Min-height | 24px |
| Vertical alignment | `align-items: center` |
| Gap (icon → label) | 8px |
| Sub-item icon size | 16px × 16px (font-size: 16px) |
| Sub-item pending circle diameter | 8px (border-radius: 4px) |

### Inline Progress Bars

| Property | Value |
|---|---|
| Track height | 6px |
| Track border-radius | 3px |
| Fill border-radius | 3px |
| Progress bar label row height | 18px |
| Margin between label row and bar | 4px |
| Bar bottom margin (inside each bar block) | 2px |
| Metadata row height (below bar) | 16px |

---

## 3. Typography

### Top-Level Step Label

| State | Font size | Weight | Color |
|---|---|---|---|
| pending | 14px | regular (`var(--font-regular)`) | `rgba(158,173,189,0.75)` |
| active | 15px | medium (`var(--font-medium)`) | `rgba(233,237,242,0.98)` |
| error | 15px | medium | `rgba(220,60,60,0.95)` |
| completed | 14px | regular | `rgba(30,164,126,0.85)` |

Line-height: 1.35  
White-space: nowrap (desktop). Wraps normally below responsive breakpoint — see §10.

### Sub-Item Label

| State | Font size | Weight | Color |
|---|---|---|---|
| pending | 13px | regular | `rgba(158,173,189,0.72)` |
| active | 13px | regular | `rgba(210,220,232,0.90)` |
| error | 13px | regular | `rgba(220,60,60,0.85)` |
| completed | 13px | regular | `rgba(140,175,161,0.80)` |

Line-height: 1.4

### Progress Bar Labels

| Element | Font size | Weight | Color |
|---|---|---|---|
| Bar title (e.g. "Snapshot files") | 12px | regular | `rgba(172,182,195,0.85)` |
| Percent value | 12px | bold (`var(--font-bold)`) | `rgba(233,237,242,0.92)` |
| Bytes label (e.g. "1.2 GB / 1.9 GB") | 12px | regular | `rgba(172,182,195,0.80)` |

---

## 4. Icons

All icons use `SVGInline` with `currentColor` fill. Color is set via CSS `color` property.

### Top-Level Step Icons

| State | Icon | Size | Color | Notes |
|---|---|---|---|---|
| completed | `check-mark-universal.inline.svg` | 20px | `rgba(30,164,126,0.95)` | |
| active | `<div>` (no SVG) | — | — | Solid 12px open dot (`.activeCircle`), same size as pending circle but using the active label color `rgba(233,237,242,0.98)` as border.
| error | `close-cross.inline.svg` | 20px | `rgba(220,60,60,0.95)` | Asset: `source/renderer/app/assets/images/close-cross.inline.svg`. Tinted red via `color` |
| pending | `<div>` (no SVG) | — | — | Solid 12px circle |

**Pending circle styling:**
```scss
background: rgba(92, 110, 128, 0.35);
border-radius: 50%;
height: 12px;
width: 12px;
```

### Sub-Item Icons

| State | Icon | Size | Color |
|---|---|---|---|
| completed | `check-mark-universal.inline.svg` | 16px | `rgba(30,164,126,0.80)` |
| active | `spinner-universal.inline.svg` | 16px | `rgba(210,220,232,0.88)` |
| error | `close-cross.inline.svg` | 16px | `rgba(220,60,60,0.88)` |
| pending | `<div>` circle | — | — |

**Sub-item pending circle:**
```scss
background: rgba(92, 110, 128, 0.30);
border-radius: 50%;
height: 8px;
width: 8px;
```

### Icon Vertical Positioning

Top-level icon column (`32px × 32px`): icon centered via `align-items: center; justify-content: center; display: flex;`

Sub-item icon column (`18px × 18px`): same flex centering.

---

## 5. Connector Lines

### Structure

The connector is a `2px`-wide vertical line centered under the top-level icon column. It is a separate `<div>` child of each step wrapper (except the last step), absolutely positioned.

```scss
.connector {
  position: absolute;
  left: 15px;          /* center of 32px icon col */
  top: 32px;           /* bottom edge of icon container */
  bottom: 0;           /* stretches to end of step wrapper */
  width: 2px;
  transform: translateX(-50%);
}
```

The step wrapper itself is `position: relative` with no fixed height (grows with sub-content).

### Color States (Phase 1 — Solid Colors Only)

All connectors use **solid** background colors based on the state of the step they originate from. No gradients.

| Connector from step in state… | Color | CSS class |
|---|---|---|
| completed | `rgba(30, 164, 126, 0.55)` | `.connectorCompleted` |
| active / pending | `rgba(92, 110, 128, 0.22)` | `.connector` (default) |
| error | `rgba(220, 60, 60, 0.35)` | `.connectorError` |

Implementation: apply one class per connector `<div>` based on its parent step's state. No `linear-gradient` or cross-step gradient logic.

---

## 6. Progress Bars

A single combined inline progress bar appears inside the Downloading sub-content area immediately below the `Downloading snapshot data` row (`step-3`). It renders only while `step-3` is the active sub-item. The bar dynamically weights snapshot-file download progress and fast-sync ancillary progress based on actual byte sizes when both totals are known, falling back to 95 % snapshot / 5 % ancillary when ancillary size is unavailable. When both transfers complete or the backend transitions into `verifying`, the combined bar shows 100 % and, after a **500 ms** verification-handoff delay, the renderer synthesizes the `Verifying snapshot digests` row as active and hides the bar.

The `InlineProgressBar` component has been extracted to its own file (`InlineProgressBar.tsx`) for reuse.

### Combined Percentage Formula

```
// When both byte totals are known (snapshotSize > 0 and ancillaryBytesTotal > 0):
totalBytes = snapshotSize + ancillaryBytesTotal
snapshotWeight = (snapshotSize / totalBytes) × 100
ancillaryWeight = (ancillaryBytesTotal / totalBytes) × 100
combinedPercent = (snapshotPercent / 100) × snapshotWeight + (ancillaryPercent / 100) × ancillaryWeight

// Fallback when ancillary size is unavailable:
combinedPercent = (snapshotPercent / 100) × 95 + (ancillaryPercent / 100) × 5
```

Clamps to 100 when either transfer is complete or the status is `verifying` or later.

### Bar Layout

```
┌─ bar wrapper ──────────────────────────────────────────────────┐
│  [label row]  Snapshot Files and Fast Sync           56%      │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │ ████████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ │  │  ← 6px track
│  └──────────────────────────────────────────────────────────┘  │
│  [details row]  Snapshot files: 1.2 GB / 1.9 GB |             │
│                 Fast sync: 24 MB / 195 MB                     │
└─────────────────────────────────────────────────────────────────┘
```

**Label row** (displayed above bar):
```
[bar title]                                [percent%]
```
Both on same line, space-between.

**Details row** (displayed below bar):
```
Snapshot files: {downloaded} / {total} | Fast sync: {downloaded} / {total}
```
Single line on desktop; wraps naturally on narrow widths.

### Bar Visual Styling

```scss
.inlineBarTrack {
  background: rgba(14, 24, 38, 0.75);
  border: 1px solid rgba(46, 64, 82, 0.45);
  border-radius: 3px;
  height: 6px;
  overflow: hidden;
  position: relative;
  width: 100%;
}

.inlineBarFill {
  border-radius: 3px;
  height: 100%;
  transition: width 0.24s ease-out;
}

/* Active downloading — animated stripes reuse ProgressBarLarge dark mode vars */
.inlineBarFillActive {
  animation: animatedStripesBackground 2s linear infinite;
  background: repeating-linear-gradient(
    -63deg,
    var(--theme-progress-bar-large-progress-dark-stripe1),
    var(--theme-progress-bar-large-progress-dark-stripe1) 10px,
    var(--theme-progress-bar-large-progress-dark-stripe2) 10px,
    var(--theme-progress-bar-large-progress-dark-stripe2) 20px
  );
  background-size: 23px 6px !important;
}

/* Completed — solid green, no animation */
.inlineBarFillComplete {
  background: rgba(30, 164, 126, 0.70);
}
```

### Fallback / Unavailable State

When bytes are not yet available (bytes_total = 0 or null):
- Show the bar track in loading state: use `loading` styles from ProgressBarLarge (`opacity: 0.3`, animated diagonal stripes)
- Show "—" for the byte values

---

## 7. State Transitions (Waterfall Scenarios)

### Scenario A — Early Preparing

```
[⟳] Preparing          ← active spinner, bright white label
 │
[○] Downloading         ← pending, muted grey circle + label
 │
[○] Finalizing          ← pending
```
No sub-items visible. No progress bars.

---

### Scenario B — Mid-Download (steps 1–3 visible, step 3 active)

```
[●] Preparing           ← completed, green check
 │
[⟳] Downloading         ← active
 │
 │   [●] Checking local disk info      ← completed (appeared, animated in)
 │   [●] Fetching certificate chain    ← completed
 │   [⟳] Downloading snapshot data    ← active
 │
 │   Snapshot Files and Fast Sync  56%
 │   ┤█████████░░░░░░░░░░░├
 │   Snapshot files: 1.2 GB / 1.9 GB | Fast sync: 24 MB / 195 MB
 │
 │   [○] Verifying digests
 │   [○] Verifying database
 │   [○] Computing verification message
 │   [○] Verifying snapshot signature
 │
[○] Finalizing
```

---

### Scenario C — Verification Phase (all download sub-items completed, verification active)

```
[●] Preparing
 │
[⟳] Downloading         ← still active (verification is part of download phase)
 │
 │   [●] Checking local disk info
 │   [●] Fetching certificate chain
 │   [●] Downloading snapshot data
 │
 │   [●] Verifying digests
 │   [⟳] Verifying database   ← active
 │   [○] Computing verification message
 │   [○] Verifying snapshot signature
 │
[○] Finalizing
```

---

### Scenario D — Finalizing (post-download, with conversion)

```
[●] Preparing
 │
[●] Downloading
 │
[⟳] Finalizing          ← active
 │
 │   [●] Converting snapshot format   ← only if conversion step ran
 │   [⟳] Moving snapshot to storage   ← active
 │   [○] Cleaning up files
```

---

### Scenario E — Finalizing (no conversion)

```
[●] Preparing
 │
[●] Downloading
 │
[⟳] Finalizing
 │
 │   [⟳] Moving snapshot to storage   ← active immediately when finalizing starts
 │   [○] Cleaning up files
```

---

### Scenario F — Completed

```
[●] Preparing
 │
[●] Downloading
 │
[●] Finalizing
 │
 │   [●] Moving snapshot to storage
 │   [●] Cleaning up files
```

All steps completed. Green checkmarks throughout. No pending items, all connector lines green. (MithrilProgressView then shows the 6-second node-starting delay — outside this component's scope.)

---

### Scenario G — Error During Download

```
[●] Preparing
 │
[✕] Downloading         ← red X, red label
 │
 │   [●] Checking local disk info
 │   [●] Fetching certificate chain
 │   [✕] Downloading snapshot data   ← failed sub-item, red
 │
 │   Snapshot Files and Fast Sync  37%  ← bar remains frozen at last value
 │   ┤████░░░░░░░░░░░░░░░├
 │   Snapshot files: 720 MB / 1.9 GB | Fast sync: — / —
 │
[○] Finalizing
```

Connector from Preparing (completed): solid green (`connectorCompleted`).  
Connector from Downloading (error): solid red (`connectorError`).  
No connector from Finalizing (last step).

---

## 8. Animation Specs

### Sub-Item Enter Animation

Each sub-item entering the DOM for the first time (appearing progressively) animates in:

```scss
@keyframes waterfallItemEnter {
  from {
    opacity: 0;
    transform: translateY(-8px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.subItem {
  animation: waterfallItemEnter 0.30s cubic-bezier(0.4, 0, 0.2, 1) forwards;
}
```

| Property | Value |
|---|---|
| Duration | 300ms |
| Easing | `cubic-bezier(0.4, 0, 0.2, 1)` (standard Material ease-in-out) |
| Properties animated | `opacity`, `transform: translateY` |
| translateY range | −8px → 0 |
| opacity range | 0 → 1 |

Sub-items already in `completed` state on initial mount (fast sub-second steps) render without the animation to avoid flicker.

### Progress Bar Fill

```scss
transition: width 0.24s ease-out;
```

Matches the existing ProgressBarLarge transition for visual consistency.

### Connector Line

No animation on the connector itself. Color changes (pending → completed) are instantaneous via class swap. A `transition: background-color 0.3s ease` is acceptable if desired but not required.

### Spinner

Existing global keyframe `loading-spin` (from `themes/mixins/loading-spinner.scss`):

```scss
:global {
  animation: loading-spin 1.5s linear infinite;
}
```

Applied to both top-level (20px) and sub-item (16px) spinners.

### Auto-Scroll

When a new sub-item becomes `active`, call:
```js
activeItemRef.current?.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
```

The MithrilProgressView wraps the waterfall in a `max-height: 480px; overflow-y: auto;` scrollable container. The waterfall items themselves are not inside a separate scroll container — the view-level scroll handles it.

**Scroll container styling:**

```scss
/* on MithrilProgressView's .stepIndicator wrapper */
max-height: 480px;
overflow-y: auto;
overflow-x: hidden;
scroll-behavior: smooth;

/* custom scrollbar (dark theme) */
scrollbar-width: thin;
scrollbar-color: rgba(92, 110, 128, 0.35) transparent;

&::-webkit-scrollbar {
  width: 4px;
}
&::-webkit-scrollbar-track {
  background: transparent;
}
&::-webkit-scrollbar-thumb {
  background: rgba(92, 110, 128, 0.35);
  border-radius: 2px;
}
```

### Reduced Motion

All animations must respect `prefers-reduced-motion`. Apply the following overrides:

```scss
@media (prefers-reduced-motion: reduce) {
  /* Disable sub-item entrance translateY — instant opacity only */
  .subItem {
    animation: none;
    opacity: 1;
    transform: none;
  }

  /* Disable spinner rotation */
  .iconSpinner,
  .subItemIcon :global(.loading-spin) {
    animation: none;
  }

  /* Disable progress bar stripe animation */
  .inlineBarFillActive {
    animation: none;
  }

  /* Disable progress bar fill transition */
  .inlineBarFill {
    transition: none;
  }

  /* Instant scroll instead of smooth */
  .stepIndicatorWrapper {
    scroll-behavior: auto;
  }
}
```

In JavaScript, auto-scroll calls must also respect reduced-motion:

```ts
const prefersReducedMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
activeItemRef.current?.scrollIntoView({
  behavior: prefersReducedMotion ? 'auto' : 'smooth',
  block: 'nearest',
});
```

---

## 9. Accessibility

### ARIA Structure

```html
<div role="list" aria-label="Bootstrap progress steps">
  <!-- Each top-level step -->
  <div role="listitem"
       aria-current="step" <!-- only on active step -->
       data-state="active|completed|pending|error">

    <!-- Sub-items list (only rendered when step has sub-items) -->
    <div role="list" aria-label="Downloading steps">
      <div role="listitem"
           aria-current="step" <!-- only on active sub-item -->
           data-state="active|completed|pending|error">
      </div>
    </div>

    <!-- Progress bar -->
    <div role="progressbar"
         aria-valuenow="{percent}"
         aria-valuemin="0"
         aria-valuemax="100"
         aria-label="Snapshot Files and Fast Sync: {percent}%">
    </div>
  </div>
</div>
```

### Live Region

The step indicator itself does **not** own a live region. In the shipped renderer, `MithrilProgressView` only live-announces the completion handoff block; the waterfall contributes list semantics and per-bar `progressbar` roles/labels. Broader phase-by-phase announcements remain deferred to the accessibility follow-up.

### Keyboard Considerations

The waterfall is read-only and non-interactive — no keyboard focus management required. The Cancel button in MithrilProgressView remains the only interactive element.

### Color Contrast Ratios

| Element | Foreground | Background | Ratio (approx.) | WCAG AA |
|---|---|---|---|---|
| Active step label | `rgba(233,237,242,0.98)` | `rgba(15,24,34,0.96)` | ~13:1 | ✓ Pass |
| Completed step label | `rgba(30,164,126,0.85)` | `rgba(15,24,34,0.96)` | ~4.5:1 | ✓ Pass |
| Pending step label (14px) | `rgba(158,173,189,0.75)` | `rgba(15,24,34,0.96)` | ~4.8:1 | ✓ Pass (normal text) |
| Pending sub-item label (13px) | `rgba(158,173,189,0.72)` | `rgba(15,24,34,0.96)` | ~4.6:1 | ✓ Pass (normal text) |
| Error state label | `rgba(220,60,60,0.95)` | `rgba(15,24,34,0.96)` | ~4.8:1 | ✓ Pass |
| Progress bar percent | `rgba(233,237,242,0.92)` | sub-content bg | ~12:1 | ✓ Pass |
| Progress bar bytes | `rgba(172,182,195,0.80)` | sub-content bg | ~5.2:1 | ✓ Pass |

All text elements meet WCAG AA 4.5:1 minimum for normal-sized text (< 18px regular / < 14px bold).

---

## Summary SCSS Class Map

```
.root                          — outer <div role="list">, flex-col, full width
.step                          — top-level step wrapper, position: relative
  .stepCompleted / .stepActive / .stepPending / .stepError — state modifiers
  .stepRow                     — flex row: iconContainer + labelContainer
  .iconContainer               — 32×32px, flex center, flex-shrink: 0
  .icon                        — font-size: 20px, SVGInline wrapper
  .iconCheck / .iconError      — color overrides
  .activeCircle                — 12px dot div for active top-level step
  .pendingCircle               — 12px circle div
  .labelContainer              — flex col, min-height: 32px, justify-center
  .label                       — 14–15px text, state-colored
  .connector                   — absolute 2px line, left:15px, top:32px, bottom:0
    .connectorCompleted        — green tint override
    .connectorError            — red tint override
  .subContent                  — indent 44px, padding-top 10px, padding-bottom 14px
    .subItem                   — flex row, min-height 24px, animates in
      .subItemCompleted / .subItemActive / .subItemPending / .subItemError
      .subItemIconContainer    — 18×18px, flex center
      .subItemIcon             — font-size: 16px
      .subItemPendingCircle    — 8px circle div
      .subItemLabel            — 13px text, state-colored
    .progressBars              — flex col, gap 10px, margin-top 12px
      .inlineBar               — single bar block
        .inlineBarHeader       — flex row, space-between
        .inlineBarTitle        — 12px muted label
        .inlineBarPercent      — 12px bold
        .inlineBarTrack        — 6px tall track
          .inlineBarFill       — transition: width
            .inlineBarFillActive / .inlineBarFillComplete
        .inlineBarMeta         — flex row, space-between, 12px muted
```

---

## Implementation Notes for Coder

- Reuse `spinnerIcon` and `checkMarkIcon` imports already in `MithrilStepIndicator.tsx`
- For the error icon: use `source/renderer/app/assets/images/close-cross.inline.svg`; tint red via `color` CSS property
- Sub-items enter without animation if they arrive in `completed` state (fast sub-second steps); detect via initial render vs. subsequent prop update using `useRef` or class component `prevProps`
- Auto-scroll: `useRef` on each sub-item, trigger `scrollIntoView` inside `componentDidUpdate`/`useEffect` when the `active` sub-item id changes
- Connector color: solid color per-step based on step state (completed → green, error → red, active/pending → grey). No gradients in phase 1
- Progress bars are extracted into `InlineProgressBar.tsx`, not `ProgressBarLarge` (the 24px tall large bar would be too heavy in sub-content)
- The `animatedStripesBackground` keyframe from `ProgressBarLarge.scss` and the `loading-spin` keyframe from mixins are already global; reference with `:global {}` blocks
- Colors reference `mithrilBootstrap` theme tokens from `createTheme.ts` (e.g. `var(--theme-mithril-progress-track-color)`) rather than raw `rgba()` values

---

## Storybook Story Acceptance Matrix

Each scenario from §7 maps to a required Storybook story. Story implementation is tracked in task-025+ but this spec defines the target coverage.

| Scenario | Story Name | Key Visual Checkpoint |
|---|---|---|
| A — Early Preparing | `Preparing Active` | Spinner on Preparing, all others pending grey circles, no sub-items |
| B — Mid-Download | `Downloading Mid Progress` | Preparing completed, Download active with mixed sub-item states, one combined progress bar with partial fill + animated stripes |
| C — Verification Phase | `Download Verification Active` | All download sub-items exist, download bars are hidden, one verification sub-item is active |
| D — Finalizing (with conversion) | `Finalizing With Conversion` | Preparing + Downloading completed, Finalizing active with conversion sub-item completed |
| E — Finalizing (no conversion) | `Finalizing No Conversion` | Same as D but no conversion sub-item; first Finalizing sub-item is active |
| F — Completed | `All Steps Completed` | All green checkmarks, all connectors green, no pending items |
| G — Error During Download | `Download Error` | Red X on Downloading + failed sub-item, frozen combined progress bar, red connector from Download step |

### Additional Stories (edge cases)

| Story Name | Description |
|---|---|
| `Reduced Motion` | Same as Scenario B but rendered with `prefers-reduced-motion: reduce` — verify no translateY animation, no spinner rotation, instant scroll |
| `Loading Bars Unknown Size` | Scenario B variant where `bytes_total` is null — bar shows loading state with "—" byte values |
| `Overflow Scroll` | All steps expanded with many sub-items — verify scrollable container and custom scrollbar styling |
| `Narrow Viewport` | Render at 360px width — verify labels wrap, metadata stacks, icon column stays 32px, no horizontal overflow |

---

## 10. Responsive Behavior

The MithrilBootstrap `.card` has `max-width: 720px; padding: 32px`. Below the `720px` viewport breakpoint, the card switches to `padding: 24px` and fills the screen. The waterfall step indicator uses the same `@media (max-width: 720px)` breakpoint already established in `MithrilStepIndicator.scss` and all sibling Mithril components.

### Breakpoint: `@media (max-width: 720px)`

All overrides are scoped under `.root` to avoid global bleed.

#### Top-Level Step Row

| Property | Desktop | ≤ 720px |
|---|---|---|
| `.root` gap | 12px | 8px |
| `.step` gap | 12px | 8px |
| `.label` font-size | 14–15px (state-dependent) | 14px (all states) |
| `.label` white-space | nowrap | normal (allows wrapping) |
| `connector` min-width | — | 12px |
| Icon column width | 32px | 32px (unchanged — stays fixed) |

#### Sub-Item Row

| Property | Desktop | ≤ 720px |
|---|---|---|
| `.subItemLabel` font-size | 13px | 12px |
| `.subItemLabel` white-space | — | normal |
| `.subContent` left indent | 44px | 38px |
| Sub-item gap | 7px | 6px |

#### Progress Bars

| Property | Desktop | ≤ 720px |
|---|---|---|
| `.inlineBarHeader` layout | flex row, space-between | flex row, space-between (unchanged) |
| `.inlineBarMeta` layout | flex row, space-between | flex-direction: column; gap: 2px |
| `.inlineBarMeta` items | single line: transferred bytes detail | stacked vertically for narrow widths |
| `.inlineBarTitle` font-size | 12px | 11px |
| `.inlineBarPercent` font-size | 12px | 11px |
| `.inlineBarMeta` font-size | 12px | 11px |

### SCSS Skeleton

```scss
@media (max-width: 720px) {
  .root {
    gap: 8px;
  }

  .step {
    gap: 8px;
  }

  .connector {
    min-width: 12px;
  }

  .label {
    font-size: 14px;
    white-space: normal;
  }

  .subContent {
    padding-left: 38px;
  }

  .subItemLabel {
    font-size: 12px;
    white-space: normal;
  }

  .subItem {
    gap: 6px;
  }

  .inlineBarTitle,
  .inlineBarPercent {
    font-size: 11px;
  }

  .inlineBarMeta {
    flex-direction: column;
    font-size: 11px;
    gap: 2px;
  }
}
```

### Design Rationale

- **Labels wrap (`white-space: normal`)**: On a 360px viewport, the card has ~312px content width (`360 - 24*2 padding`). After the 32px icon column and 12px gap, the label column gets ~268px — enough for most labels, but long i18n strings (especially Japanese) need wrapping.
- **Icon column stays fixed at 32px**: Provides visual anchor for the vertical connector line regardless of viewport width.
- **Inline bar metadata stacks vertically**: At narrow widths, both the label/percent row and the transferred-bytes row benefit from vertical stacking to avoid crowding.
- **Sub-item font scales to 12px**: Dense sub-item rows at narrow widths benefit from the slightly smaller type, matching the information-density reduction pattern used elsewhere in the Mithril SCSS.

## Implementation Status

- Status: Implemented (2026-03-18)
- Renderer: Implemented in `MithrilStepIndicator.tsx` and composed into `MithrilProgressView.tsx`
- Follow-up: Unit and integration expansion remains tracked in the Mithril task ledger
- ReviewerCouncil: PASS (Sonnet 0.92, Gemini 0.95, Codex 0.89 FAST-FIX)
