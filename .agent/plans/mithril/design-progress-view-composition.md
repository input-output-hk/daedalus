# Design Spec: Mithril Progress View Composition

**Feature:** Mithril bootstrap progress view composition  
**Created:** 2026-03-18  
**Revised:** 2026-03-23  
**Status:** implemented  
**Related design:** [Vertical Waterfall Step Indicator](design-waterfall-step-indicator.md)

### Design Changelog

| Date | Change |
|---|---|
| 2026-03-23 | Added subtitle under header; changed completion delay from 3 s to 6 s; enlarged completion spinner to 64 px; moved elapsed timer from store-driven `overallElapsedSeconds` to renderer-local state derived from `bootstrapStartedAt`; added `progressSubtitle` i18n message; noted theme-token migration (no longer deferred). |
| 2026-03-20 | Updated the spec to match the shipped renderer: `verifying` stays mapped to the Downloading visual phase, the view exposes only the elapsed timer plus pass-through waterfall data, the completion spinner sits below the copy, and the cancel action is centered. |
| 2026-03-18 | Established the progress-view layout around a header, single elapsed timer, scrollable waterfall container, completion handoff block, and cancel action. |
| 2026-03-18 | Aligned the container sizing and non-interactive waterfall wrapper with the related step-indicator design. |
| 2026-03-18 | Implemented the composed view through task-024i; broader phase-announcement accessibility remains deferred to the accessibility phase. |

---

## 1. Layout Diagram

The recomposed `MithrilProgressView` renders inside the existing `.card` (720px max-width, 32px padding, `rgba(15,24,34,0.96)` background). All widths below are relative to the ~656px content area (720 − 32×2).

### Active Progress State (preparing / downloading / verifying / finalizing)

```
┌─ .card ──────────────────────────────────────────────────────────────┐
│                                                                       │
│  ┌─ .header ──────────────────────────────────────────────────────┐  │
│  │  Fast sync with Mithril                          22px medium   │  │
│  │  <p> Snapshot download and verification time can vary based    │  │
│  │      on your network connection and storage performance.       │  │
│  │      (14px regular, secondary-text color)                      │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                               margin-bottom: 24px    │
│  ┌─ .timerDisplay ────────────────────────────────────────────────┐  │
│  │  ELAPSED   0:42                               ← inline display │  │
│  │  (13px uppercase label)  (18px mono value)                    │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                               margin-bottom: 16px    │
│  ┌─ .waterfallContainer ──────────────────────────────────────────┐  │
│  │  ┌───────────────────────────────────────────────────────┐     │  │
│  │  │                                                       │     │  │
│  │  │  < MithrilStepIndicator />                            │     │  │
│  │  │                                                       │     │  │
│  │  │  (see the related step-indicator design for inner layout) │  │  │
│  │  │                                                       │  ▐  │  │
│  │  │                                                       │  ▐  │  │
│  │  │                                                       │     │  │
│  │  └───────────────────────────────────────────────────────┘     │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                            max-height: 480px         │
│                                            overflow-y: auto          │
│                                            margin-bottom: 18px       │
│  ┌─ .actions ─────────────────────────────────────────────────────┐  │
│  │                       [ Cancel ]                               │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                                                       │
└───────────────────────────────────────────────────────────────────────┘
```

### Completed State (6-second delay)

When `status === 'completed'`, the completion block appears between the waterfall and the cancel button. The cancel button becomes disabled.

```
┌─ .card ──────────────────────────────────────────────────────────────┐
│                                                                       │
│  ┌─ .header ──────────────────────────────────────────────────────┐  │
│  │  Fast sync with Mithril                                        │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                               margin-bottom: 24px    │
│  ┌─ .timerDisplay ────────────────────────────────────────────────┐  │
│  │  ELAPSED   1:47 (frozen)                                       │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                               margin-bottom: 16px    │
│  ┌─ .waterfallContainer ──────────────────────────────────────────┐  │
│  │  [●] Preparing                                                 │  │
│  │   │                                                            │  │
│  │  [●] Downloading                                               │  │
│  │   │                                                            │  │
│  │  [●] Finalizing          ← all green checkmarks                │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                               margin-bottom: 18px    │
│  ┌─ .completionBlock ────────────────────────────────────────────┐  │
│  │                                                                │  │
│  │  Starting cardano-node                                         │  │
│  │  (16px medium, bright white)                                   │  │
│  │                           4px gap                              │  │
│  │  The Mithril snapshot has been restored.                       │  │
│  │  Cardano-node is starting up to complete the remaining sync.   │  │
│  │  (14px regular, muted)                                         │  │
│  │                           12px gap                             │  │
│  │  [spinner] 64px × 64px, centered below text                    │  │
│  │                                                                │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                               margin-bottom: 24px    │
│  ┌─ .actions ─────────────────────────────────────────────────────┐  │
│  │                       [ Cancel ] ← disabled, 50% opacity       │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                                                       │
└───────────────────────────────────────────────────────────────────────┘
```

---

## 2. State Matrix

### 2.1 Element Visibility by Status

| Element | preparing | downloading | verifying | unpacking | converting | finalizing | completed |
|---|---|---|---|---|---|---|---|
| **Header** ("Fast sync with Mithril") | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible |
| **Subtitle** (network/storage note) | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible |
| **Timer row** (Elapsed + value) | ✓ ticking | ✓ ticking | ✓ ticking | ✓ ticking | ✓ ticking | ✓ ticking | ✓ frozen |
| **Waterfall container** | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible | ✓ visible (collapsed — all steps completed) |
| **Completion block** | ✗ hidden | ✗ hidden | ✗ hidden | ✗ hidden | ✗ hidden | ✗ hidden | ✓ visible (fade-in) |
| **Cancel button** | ✓ enabled | ✓ enabled | ✓ enabled | ✓ enabled | ✓ enabled | ✓ enabled | ✓ visible, **disabled** |
| **Completion live region** | — | — | — | — | — | — | announces "Starting cardano-node" |

### 2.2 Status → Visual Phase Mapping

The cardano-wallet backend emits working statuses that `MithrilStepIndicator` maps to a 3-step visual model internally. This table is the canonical reference.

| Backend status | Visual step (StepIndicator) | Notes |
|---|---|---|
| `preparing` | **Preparing** step active | First phase |
| `downloading` | **Downloading** step active | Waterfall sub-items and progress bars visible |
| `verifying` | **Downloading** step active | Verification stays inside the Downloading visual phase; download bars are hidden while steps 4–7 advance |
| `unpacking` | **Finalizing** step active | Mapped internally by `MithrilStepIndicator` — no separate visual step |
| `converting` | **Finalizing** step active | Mapped internally by `MithrilStepIndicator` — no separate visual step |
| `finalizing` | **Finalizing** step active | Final processing before completion |
| `completed` | All steps completed (green checkmarks) | Completion block appears, cancel disabled |

`cancelled` does not render this view. The outer loading page keeps the Mithril overlay mounted, and `MithrilBootstrap` routes `cancelled` back into the storage/decision cycle.

### Timer behavior by state

| State | Timer | Notes |
|---|---|---|
| preparing | Ticking from 0:00 | Renderer derives elapsed seconds locally from `bootstrapStartedAt` via `useState` + `setInterval` |
| downloading | Ticking | Continues from preparing |
| verifying | Ticking | Continues while verification sub-items advance under the Downloading step |
| finalizing | Ticking | Continues from downloading |
| completed | **Frozen** | Renderer stops the local interval when `status === 'completed'`. The frozen value is the elapsed seconds at the moment of status change. |

---

## 3. Element Specifications

### 3.1 Header

Title followed by a subtitle `<p>` element.

| Property | Value |
|---|---|
| Tag | `<h1>` |
| Content | `messages.title` ("Fast sync with Mithril") |
| Font size | 22px |
| Font weight | medium (`var(--font-medium)`) |
| Color | `var(--theme-mithril-heading-color)` |
| Margin bottom | 6px |

**Subtitle:**

| Property | Value |
|---|---|
| Tag | `<p>` (child of `.header`) |
| Content | `messages.progressSubtitle` ("Snapshot download and verification time can vary based on your network connection and storage performance.") |
| Font size | 14px |
| Font weight | regular (`var(--font-regular)`) |
| Color | `var(--theme-mithril-secondary-text-color)` |
| Line height | 1.5 |
| Margin bottom | 24px (on the header block) |

### 3.2 Timer Display

A compact inline row showing the elapsed label and formatted duration together.

```
┌──────────────────────────────────────────────────────────────┐
│  ELAPSED                                             1:47    │
│  ↑ label                                        value ↑     │
└──────────────────────────────────────────────────────────────┘
```

| Property | Value |
|---|---|
| Container display | `flex` |
| Container gap | `8px` |
| Container align-items | `baseline` |
| Container margin-bottom | `16px` |

**Label (left side):**

| Property | Value |
|---|---|
| Content | `messages.progressElapsedLabel` ("Elapsed") |
| Font size | 13px |
| Font weight | regular (`var(--font-regular)`) |
| Letter spacing | `0.04em` |
| Text transform | `uppercase` |
| Color | `rgba(172, 182, 195, 0.90)` |

**Value (right side):**

| Property | Value |
|---|---|
| Content | Formatted duration from renderer-local `elapsedSeconds` state (derived from `bootstrapStartedAt` prop) |
| Format | `H:MM:SS` when ≥1 hour, `M:SS` when <1 hour (reuse existing `formatDuration` helper) |
| Font size | 18px |
| Font weight | medium (`var(--font-medium)`) |
| Font family | `var(--font-mono, 'SF Mono', 'Menlo', 'Monaco', 'Consolas', monospace)` |
| Font variant numeric | `tabular-nums` (prevents layout shift as digits change) |
| Color | `rgba(236, 241, 247, 0.95)` |

**Frozen state (completed):**
- The renderer stops its local `setInterval` when `status === 'completed'`
- Visual appearance is identical — no gray-out or special treatment. The frozen value is the final elapsed time

### 3.3 Waterfall Container

The scrollable wrapper around `<MithrilStepIndicator />`.

| Property | Value |
|---|---|
| Max height | `480px` |
| Overflow-y | `auto` |
| Margin bottom | `18px` |
| Padding right | `4px` |

The waterfall is read-only and non-interactive, with auto-scroll handled by the step-indicator component. Cancel remains the only interactive control in the active progress state.

**Custom scrollbar (dark theme):**

```scss
.waterfallContainer {
  scrollbar-color: rgba(92, 110, 128, 0.5) transparent;
  scrollbar-width: thin;

  &::-webkit-scrollbar {
    width: 6px;
  }
  &::-webkit-scrollbar-track {
    background: transparent;
  }
  &::-webkit-scrollbar-thumb {
    background: rgba(92, 110, 128, 0.5);
    border-radius: 3px;

    &:hover {
      background: rgba(129, 147, 166, 0.7);
    }
  }
}
```

**Scroll fade hint (top/bottom) — optional enhancement:**

> This is an optional UX polish that may be added in a future pass. It is not part of the implemented baseline design.

When content overflows, a 20px gradient fade at the top/bottom edges can indicate scrollability. Would be implemented via `::before` / `::after` pseudo-elements on `.waterfallContainer`:

```scss
&::after {
  background: linear-gradient(to top, rgba(15, 24, 34, 0.96), transparent);
  bottom: 0;
  content: '';
  height: 20px;
  left: 0;
  pointer-events: none;
  position: sticky;
  right: 0;
  z-index: 1;
}
```

The top fade would only appear when scrolled down (managed via a `data-scrolled="true"` attribute toggled by a scroll listener). The bottom fade only when there is content below the fold. **Skip this for initial implementation.**

### 3.4 Completion Block

Appears only when `status === 'completed'`. Centered content with a spinner above the title.

| Property | Value |
|---|---|
| Container display | `flex` |
| Container flex-direction | `column` |
| Container align-items | `center` |
| Container text-align | `center` |
| Container padding | `24px 20px` |
| Container background | `rgba(10, 18, 28, 0.52)` |
| Container border | `1px solid rgba(46, 64, 82, 0.55)` |
| Container border-radius | `12px` |
| Container margin-bottom | `24px` |
| Enter animation | fade-in 300ms ease (see §5) |

**Spinner (top):**

| Property | Value |
|---|---|
| Icon | `spinner-universal.inline.svg` (same as waterfall active spinner) |
| Size | 64px × 64px (`font-size: 64px`) |
| Color | `rgba(122, 210, 188, 0.88)` (green-tinted, matching the completion theme) |
| Animation | `loading-spin 1.5s linear infinite` (existing global keyframe) |
| Margin bottom | `8px` |

**Title:**

| Property | Value |
|---|---|
| Content | `messages.nodeStartingTitle` ("Starting cardano-node") |
| Tag | `<h2>` |
| `tabindex` | `-1` (allows programmatic focus on completed transition; not in natural tab order) |
| Font size | 16px |
| Font weight | medium (`var(--font-medium)`) |
| Color | `rgba(243, 247, 251, 0.98)` |
| Line height | 1.35 |
| Margin | `0 0 4px` |

**Detail:**

| Property | Value |
|---|---|
| Content | `messages.nodeStartingDetail` ("The Mithril snapshot has been restored. Cardano-node is starting up to complete the remaining sync.") |
| Tag | `<p>` |
| Font size | 13px |
| Font weight | regular (`var(--font-regular)`) |
| Color | `rgba(187, 198, 210, 0.82)` |
| Line height | 1.55 |
| Max width | `480px` (prevents overly long lines) |
| Margin | `0` |

### 3.5 Cancel Button

Reuses the existing `.secondaryAction` button style. Adds a disabled-state visual.

**Normal state** (preparing / downloading / finalizing):
- Unchanged from current design
- Border: `1px solid rgba(92, 110, 128, 0.6)`
- Color: `rgba(225, 233, 242, 0.92)`
- Hover: border-color brightens, text brightens

**Disabled state** (completed):

| Property | Value |
|---|---|
| Opacity | `0.40` |
| Cursor | `not-allowed` |
| Border color | unchanged from base disabled button state |
| Color | unchanged from base disabled button state |
| `disabled` attribute | `true` (HTML native for accessibility) |
| Hover | disabled override preserves the base colors and border |

```scss
.secondaryAction {
  &:disabled {
    cursor: not-allowed;
    opacity: 0.5;

    &:hover {
      border-color: rgba(92, 110, 128, 0.6);
      color: rgba(225, 233, 242, 0.92);
    }
  }
}
```

---

## 4. Typography Summary

| Element | Size | Weight | Color | Line height | Notes |
|---|---|---|---|---|---|
| Header h1 | 22px | medium | `var(--theme-mithril-heading-color)` | — | Unchanged |
| Subtitle p | 14px | regular | `var(--theme-mithril-secondary-text-color)` | 1.5 | Below title |
| Timer label | 13px | regular | `rgba(172,182,195,0.90)` | — | Uppercase, 0.04em spacing |
| Timer value | 18px | medium | `rgba(236,241,247,0.95)` | — | `tabular-nums` |
| Completion title | 16px | medium | `rgba(243,247,251,0.98)` | 1.35 | |
| Completion detail | 14px | regular | `rgba(187,198,210,0.92)` | 1.55 | Max-width 480px |
| Cancel button | 14px | regular | `rgba(225,233,242,0.92)` | — | Unchanged |
| Cancel disabled | 14px | regular | `rgba(225,233,242,0.50)` | — | 50% opacity on container |

---

## 5. Animations

### Completion Block Enter

```scss
@keyframes completionFadeIn {
  from {
    opacity: 0;
    transform: translateY(6px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.completionBlock {
  animation: completionFadeIn 0.3s cubic-bezier(0.4, 0, 0.2, 1) forwards;
}
```

### Completion Spinner

Uses the local `spin` keyframe defined in `MithrilProgressView.scss`:

```scss
.completionSpinner {
  animation: spin 1s linear infinite;
}
```

### Reduced Motion

```scss
@media (prefers-reduced-motion: reduce) {
  .completionBlock {
    animation: none;
    opacity: 1;
    transform: none;
  }

  .completionSpinner :global {
    animation: none;
  }
}
```

---

## 6. Color Palette Reference

Colors reference the `mithrilBootstrap` theme token map defined in `createTheme.ts` and all per-theme output files (e.g. `--theme-mithril-heading-color`). Raw `rgba()` values below are the dark-theme defaults; the actual runtime values come from CSS custom properties.

| Usage | Color | Source |
|---|---|---|
| Card background | `rgba(15, 24, 34, 0.96)` | MithrilBootstrap `.card` |
| Header text | `rgba(255, 255, 255, 0.98)` | Existing `.header h1` |
| Timer row bg | `rgba(10, 18, 28, 0.45)` | Derived from existing metadata item bg (`0.52`) — slightly lighter for single-row element |
| Timer row border | `rgba(46, 64, 82, 0.50)` | Matches existing metadata item border family |
| Timer label | `rgba(172, 182, 195, 0.85)` | Matches `.metadataLabel` |
| Timer value | `rgba(233, 237, 242, 0.95)` | Matches `.metadataValue` family |
| Completion block bg | `rgba(10, 18, 28, 0.52)` | Matches existing `.metadataItem` bg |
| Completion block border | `rgba(46, 64, 82, 0.55)` | Matches existing border family |
| Completion spinner | `rgba(233, 237, 242, 0.92)` | Neutral spinner shown below the completion copy |
| Completion title | `rgba(243, 247, 251, 0.98)` | Matches `.statusTitle` |
| Completion detail | `rgba(187, 198, 210, 0.82)` | Matches `.statusDetail` family |
| Scrollbar thumb | `rgba(92, 110, 128, 0.50)` | Matches the implemented waterfall container |
| Button disabled text | `rgba(225, 233, 242, 0.40)` | Existing button color at 40% opacity |
| Scroll fade gradient | `rgba(15, 24, 34, 0.96) → transparent` | Matches card bg |

---

## 7. Removed Elements

The following elements from the current `MithrilProgressView` are removed in the recomposition:

| Element | Reason |
|---|---|
| **ProgressBarLarge** | Replaced by inline progress bars inside the waterfall step indicator |
| **Status panel** (`.statusPanel`) | Stage title/detail copy is now communicated via active waterfall step labels. The completion state uses the dedicated completion block. |
| **Metadata grid** (`.metadataGrid`) | Overall progress, transfer-rate, and remaining-time fields were removed. Determinate bar percent + transferred bytes live inline in the waterfall, and elapsed time is the only top-level timer row. |
| **Status label** ("CURRENT ACTIVITY") | No longer needed — waterfall makes active step visually obvious |

---

## 8. Responsive Behavior

Breakpoint: `@media (max-width: 720px)` — consistent with all Mithril components.

### Layout changes

| Element | Desktop (>720px) | Narrow (≤720px) |
|---|---|---|
| **Header margin-bottom** | 24px | 24px |
| **Timer display** | Inline label + value | Inline label + value |
| **Waterfall max-height** | 480px | 480px |
| **Waterfall margin-bottom** | 18px | 18px |
| **Completion block padding** | `20px` | `20px` |
| **Completion title font-size** | 16px | 16px |
| **Completion detail font-size** | 14px | 14px |
| **Cancel button** | Centered within the action row | Centered within a vertical action stack |

### SCSS skeleton

```scss
@media (max-width: 720px) {
  .actions {
    align-items: center;
    flex-direction: column;
  }
}
```

---

## 9. Accessibility

### Current behavior

| Element | Attribute | Value |
|---|---|---|
| Completion block | `role` | `status` |
| Completion block | `aria-live` | `polite` |
| Completion block | `aria-atomic` | `true` |
| Completion heading | `tabIndex` | `-1` |
| Cancel button (completed) | `disabled` | `true` |
| Waterfall container | `role` | none (read-only wrapper) |

### Phase announcements

Broader top-level phase announcements are deferred to the accessibility phase. The implemented view only live-announces the completion block so the final handoff is spoken once without re-announcing each waterfall sub-item or progress-bar update.

### Focus management

When `status` transitions to `completed`, focus moves programmatically to the completion heading (`<h2>`). The heading uses `tabIndex="-1"` so it can receive focus without entering the normal tab order. Cancel remains the only interactive control during active progress.

### Reduced motion

See §5 for animation overrides under `prefers-reduced-motion: reduce`.

---

## 10. Behavior Notes

### 6-Second Completion Delay

1. When `status` changes to `'completed'`, the renderer freezes the local elapsed timer
2. The view renders the completion block with fade-in animation
3. The waterfall shows all steps as completed (green checkmarks)
4. The cancel button becomes disabled
5. After 6 seconds, the main-process handoff helper in `handleDiskSpace.ts` emits Mithril idle and yields to the normal node-sync loading screen

The 6-second delay is a UX grace period so the user sees confirmation that Mithril restore succeeded before the overlay disappears. The component does not own the timeout — it simply renders the completed state while the main process delays the idle handoff.

### Timer Format

- `0:00` → `0:01` → ... → `9:59` → `10:00` → ... → `59:59` → `1:00:00`
- Format: `M:SS` (no leading zero on minutes) below 1 hour
- Format: `H:MM:SS` (no leading zero on hours, zero-padded minutes) at/above 1 hour
- Reuses existing `formatDuration()` helper already in the file

### Auto-scroll

The waterfall container's auto-scroll is driven by the related `MithrilStepIndicator` component internally. The parent view provides the scrollable container with `max-height` and `overflow-y: auto`. The child calls `scrollIntoView({ behavior: 'smooth', block: 'nearest' })` on newly active items.

---

## 11. SCSS Class Map

New/changed classes in `MithrilProgressView.scss`:

```
.root                          — flex-col container (unchanged)
.header                        — title block (margin-bottom adjusted)
  h1                           — unchanged
  p                            — subtitle (progressSubtitle message)
.timerDisplay                  — NEW: inline elapsed label/value row
  .timerLabel                  — NEW: uppercase muted label
  .timerValue                  — NEW: monospace-like digit value, tabular-nums
.waterfallContainer            — Scrollable wrapper around the step indicator
.completionBlock               — NEW: centered card, fade-in animation
  .completionSpinner           — NEW: 64px spinning icon
  .completionTitle             — NEW: 16px medium h2, tabindex=-1 for programmatic focus
  .completionDetail            — NEW: 14px muted paragraph
.actions                       — unchanged
  .secondaryAction             — unchanged, with disabled pseudo-state

REMOVED:
  .progressBar
  .statusPanel
  .statusCopy
  .statusLabel
  .statusTitle
  .statusDetail
  .metadataGrid
  .metadataItem
  .metadataLabel
  .metadataValue
```

---

## 12. i18n Messages Used

| Message key | Existing? | Usage |
|---|---|---|
| `messages.title` | ✓ | Header h1 |
| `messages.progressElapsedLabel` | ✓ | Timer label "Elapsed" |
| `messages.nodeStartingTitle` | ✓ | Completion block title |
| `messages.nodeStartingDetail` | ✓ | Completion block detail |
| `messages.cancel` | ✓ | Cancel button label |
| `messages.progressSubtitle` | ✓ | Subtitle below header |

---

## 13. Out of Scope

The following items are explicitly deferred and not part of this design spec:

| Item | Deferred to | Notes |
|---|---|---|
| **Storybook stories** | Phase 8 | Visual regression stories for all MithrilProgressView states (preparing, downloading, unpacking, converting, finalizing, completed, narrow viewport, overflow scroll, keyboard focus). Story acceptance matrix will be defined in the Phase 8 design task. |
| **Theme variable migration** | ✅ Done | Migrated to CSS custom properties via `mithrilBootstrap` and `chainStorage` token maps in `createTheme.ts` and all per-theme output files. |
| **Scroll fade hints** | Future enhancement | Gradient fade pseudo-elements on waterfall overflow edges (see §3.3 optional note). |

---

## 14. Implementation Notes

1. **Remove** `ProgressBarLarge` import and `<ProgressBarLarge>` JSX
2. **Remove** the entire `.statusPanel` section and `.metadataGrid` section from JSX and SCSS
3. **Remove** the `getStageDetail()` helper, `metadataItems` array construction, `downloadedLabel`, `transferRateLabel`, `remainingLabel` logic (this now lives in the waterfall component)
4. **Keep** `formatDuration()` — still used for the timer display
5. **Add** timer display JSX between header and waterfall
6. **Rename** `.stepIndicator` class to `.waterfallContainer` and add scroll styles (max-height **480px**, scrollbar per §3.3)
7. **Add** completion block JSX conditionally rendered when `status === 'completed'`
8. **Add** disabled state to cancel button when `status === 'completed'`
9. **Keep** the completion block as the only live-announced status region for this phase; broader phase-announcement a11y is deferred
10. **Props simplification**: The view receives `status`, `bootstrapStartedAt`, `onCancel`, plus pass-through props for `MithrilStepIndicator` (progress items and determinate progress data). Timer is derived locally from `bootstrapStartedAt` via `useState` + `setInterval`. The view does not own any overall percentage, throughput, or remaining-time UI.
11. **Skip** scroll fade pseudo-elements (§3.3 optional enhancement) — not required for initial implementation
12. **Focus management**: On completed transition, move focus to the completion heading (`<h2>`)
13. **Keep** the 6-second delay orchestration outside the component; it belongs to the main-process handoff path
