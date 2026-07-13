# task-ux-702d-cat-c — partial-sync overlay backdrop: make it a solid per-theme surface

> Per-category doc, decomposed from the canonical plan `task-ux-702d.md` (task-ux-702d = Mithril Sync UX
> finalization cleanup, ad-hoc wave). **Implementation record — as landed (2026-07-01).** Parent task:
> `task-ux-702d`. Plan review: `task-ux-702d-plan-review.md`. Research: `task-ux-702d-research.md`. If this doc ever
> disagrees with live code, prefer live code and reconcile here.

## Sequencing / status position
Third landed CAT of the wave. Presentation/token-only; touches the same nine theme files + `createTheme.ts` as CAT-A
but different token lines (no collision) — landed A → C in practice.

## Findings closed & decisions implemented
| Finding # | Finding (short) | CAT | Severity |
|---|---|---|---|
| #3 | **Mithril Partial Sync download-status + failure prompts show the Cardano Node status page through the backdrop**: `MithrilPartialSyncOverlay` reuses `MithrilBootstrap.scss` `.backdrop` (`:14` → `background: var(--theme-mithril-overlay-backdrop-start)`), whose token is alpha **`0.92`** in every theme. Because the overlay is mounted at `App.tsx:99` over the live `SyncingConnectingPage` (unlike fast-sync, which `LoadingPage:72-77` renders *instead of* the syncing page), the 8% transparency bleeds the node status page through both the download/progress view and the failure/error prompts. Fast-sync uses the *same* token but has nothing meaningful behind it, so it reads as a clean per-theme backdrop. Distinct from CAT-A (the proactive **card** fill, a different token) and 702c CAT-B (which firmed this backdrop's structure but left it translucent). | **CAT-C** | Medium |

## Locked invariants this change must NOT break
- **Vocab guardrail #8** holds — token-alpha change only; no copy introduced.
- **Offer-signal boundary #4** untouched — no gating/suppression change.
- **No SCSS / structure / mount change** — the partial-sync overlay is not re-parented or given a new background
  element, and `MithrilBootstrap.scss` (including the `.backdrop` `blur(5px)`) is untouched; token-only — no
  partial-sync-specific backdrop token is forked (restated inline from the master's Non-goals, which remain
  authoritative).

## Exact files (full repo-relative paths)
- `source/renderer/app/themes/daedalus/yellow.ts`
- `source/renderer/app/themes/daedalus/white.ts`
- `source/renderer/app/themes/daedalus/dark-blue.ts`
- `source/renderer/app/themes/daedalus/shelley-testnet.ts`
- `source/renderer/app/themes/daedalus/cardano.ts`
- `source/renderer/app/themes/daedalus/dark-cardano.ts`
- `source/renderer/app/themes/daedalus/flight-candidate.ts`
- `source/renderer/app/themes/daedalus/light-blue.ts`
- `source/renderer/app/themes/daedalus/incentivized-testnet.ts`
- `source/renderer/app/themes/utils/createTheme.ts`

Implementable from this section alone. Styling/token-only; no TSX, i18n, store, IPC, or backend change.

### Rationale
The **Mithril Partial Sync overlay** (`MithrilPartialSyncOverlay` — the download-status/progress view and the
failure/error prompts) and the **fast-sync bootstrap** (`MithrilBootstrap`) render the *same* JSX shell and import the
*same* stylesheet (`MithrilBootstrap.scss`). Its `.backdrop` (`:12-19`) is a full-screen scrim:
`@include overlay-backrop` (a `backdrop-filter: blur(5px)` — see `themes/mixins/overlay-backdrop.scss`) plus
`background: var(--theme-mithril-overlay-backdrop-start)` (`:14`). That token is defined at alpha **`0.92`** in every
theme, so the scrim is 8% transparent.

The two flows differ only in **what is mounted behind the scrim**:
- **Fast-sync:** `LoadingPage` (`:72-77`) returns `<CenteredLayout><MithrilBootstrapPage /></CenteredLayout>`
  *instead of* `SyncingConnectingPage`. Nothing but the solid `CenteredLayout` background
  (`--theme-loading-background-color`) sits behind the scrim, so it reads as a clean, solid per-theme backdrop.
- **Partial-sync:** `MithrilPartialSyncOverlay` is mounted at `App.tsx:99` as a **sibling of `<Router>`**, layered
  *over* whatever route is active. During catch-up that route is `LoadingPage → SyncingConnectingPage` (the Cardano
  Node status/syncing page), which stays mounted underneath. The same 0.92-alpha scrim lets that node status page show
  through the download and failure prompts — the reported defect.

There is no dedicated solid backdrop element for the partial-sync surface; it relies entirely on the shared
`.backdrop` token's alpha. So the correct, minimal fix — mirroring CAT-A's card-fill fix — is to make the backdrop
token **opaque**. Setting alpha to `1` keeps each theme's chosen hue and simply stops the bleed-through, making the
partial-sync overlay read as the *same* solid backdrop the fast-sync flow already shows. (The user's "light blue" is
the light-blue theme's own backdrop hue `rgba(14, 48, 87, …)`; the fix is per-theme, each theme keeping its color.)

**Why prior 702 cleanup didn't close this ("still not fixed"):** 702c CAT-B firmed the *element* side of this same
`.backdrop` — it set `opacity: 1` (`MithrilBootstrap.scss:16`) and added the `blur(5px)` mixin — but left the
**token's own alpha at `0.92`**. Element `opacity: 1` doesn't remove the transparency that lives in the token's alpha
channel, so 8% of the node status page still shows through. CAT-C closes it at the token, the one place the residual
transparency actually is.

### Reference details (verified against the live post-702c tree, 2026-07-01)
- **Presentational components (share the SCSS, unchanged by CAT-C):**
  `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (partial-sync;
  `.component > .backdrop + .content > .card > MithrilProgressView | MithrilErrorView`, `:166-249`) and
  `MithrilBootstrap.tsx` (fast-sync; identical shell `:216-232`).
- **Mount points:** partial-sync overlay at `source/renderer/app/App.tsx:99` (sibling of `<Router>`, over the live
  `SyncingConnectingPage`); fast-sync at `source/renderer/app/containers/loading/LoadingPage.tsx:72-77` (renders
  `MithrilBootstrapPage` *instead of* `SyncingConnectingPage`, so nothing shows through).
- **SCSS (references the token — unchanged by CAT-C):**
  `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss:14`
  (`background: var(--theme-mithril-overlay-backdrop-start)`), full-screen via `.component`
  `position: absolute; inset 0; z-index: 2600` (`:3-10`) + `.backdrop` `position: absolute; 100%×100%` (`:12-19`).
- **Token definition sites (the actual transparency — both stops at alpha `0.92` before the fix):**
  - `source/renderer/app/themes/daedalus/light-blue.ts:377-378` — `rgba(14, 48, 87, 0.92)` / `rgba(24, 58, 97, 0.92)`
  - `source/renderer/app/themes/daedalus/white.ts:365-366` — `rgba(240, 240, 240, 0.92)` / `rgba(248, 248, 248, 0.92)`
  - `source/renderer/app/themes/daedalus/yellow.ts:366-367` — `rgba(240, 235, 228, 0.92)` / `rgba(246, 241, 234, 0.92)`
  - `source/renderer/app/themes/daedalus/dark-blue.ts:381-382` — `rgba(24, 37, 55, 0.92)` / `rgba(34, 47, 65, 0.92)`
  - `source/renderer/app/themes/daedalus/cardano.ts:379-380` — `rgba(18, 20, 23, 0.92)` / `rgba(28, 30, 33, 0.92)`
  - `source/renderer/app/themes/daedalus/dark-cardano.ts:365-366` — `rgba(40, 41, 63, 0.92)` / `rgba(50, 51, 73, 0.92)`
  - `source/renderer/app/themes/daedalus/shelley-testnet.ts:365-366` — `rgba(40, 41, 63, 0.92)` / `rgba(50, 51, 73, 0.92)`
  - `source/renderer/app/themes/daedalus/flight-candidate.ts:365-366` — `rgba(40, 41, 63, 0.92)` / `rgba(50, 51, 73, 0.92)`
  - `source/renderer/app/themes/daedalus/incentivized-testnet.ts:366-367` — `rgba(28, 29, 46, 0.92)` / `rgba(38, 39, 56, 0.92)`
  - `source/renderer/app/themes/utils/createTheme.ts:588-595` — dynamic:
    `chroma(background.secondary.regular).darken(0.5).alpha(0.92)` (start) and `chroma(...).alpha(0.92)` (end).
- **Shared-token consumer (intentionally in scope):** the fast-sync bootstrap backdrop uses the same token via the
  shared `MithrilBootstrap.scss`. Making it solid there is **harmless** (nothing but the solid
  `--theme-loading-background-color` sits behind it) and is in fact the point — the two Mithril backdrops become
  visually identical. **Accepted, not a regression.**

### Implementation (as landed)
Flip the token alpha `0.92 → 1` at every definition site, preserving the rgb hue, for **both** the `-start` and
`-end` stops:
- Nine static themes: `'--theme-mithril-overlay-backdrop-start'` and `'--theme-mithril-overlay-backdrop-end'` set to
  `rgba(<r>, <g>, <b>, 1)` (files/lines above).
- Dynamic generator (`createTheme.ts:592,595`): both `.alpha(0.92)` → `.alpha(1)`.

Only `-start` is currently referenced by any SCSS (`MithrilBootstrap.scss:14`); `-end` is defined but unused. It is
flipped too so the backdrop gradient-stop pair stays internally consistent (a future gradient wiring won't
reintroduce a translucent stop). The `.backdrop` SCSS rule — including the now-moot `blur(5px)` — is left untouched
(it correctly references the token). Sibling tokens (`--theme-mithril-card-background`, `--theme-mithril-card-shadow`,
`--theme-mithril-panel-border-color`) are **not** touched — only the overlay backdrop.

Diff shape: 10 files (9 static themes × 2 lines + `createTheme.ts` × 2 stops). Verified post-edit that no
`--theme-mithril-overlay-backdrop-*` definition retains `0.92` (`grep` clean).

### i18n
- None (styling/token-only; vocab-neutral).

### Tests
- **On-device / Storybook is the load-bearing proof.** On a significantly-behind node, drive the Mithril Partial Sync
  overlay through the **download/progress** states and a **failure/error** state and confirm the backdrop is a fully
  solid per-theme surface with **no** Cardano Node status page visible through it — matching the fast-sync flow. Repeat
  across light-blue plus at least one other light (white) and one dark (cardano / dark-blue) theme. Storybook:
  `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` renders these views across themes.
- **No unit/snapshot coverage exists for this token** (`grep` for `overlay-backdrop` across specs/snapshots is empty),
  so no test needed updating; nothing asserts the old `0.92`.
- Checks: `yarn compile` + `yarn lint` + touched-file `yarn prettier:check` green (theme files are plain TS object
  literals — trivial-risk edits). Node v24 renderer verify-env caveat applies if running renderer jest/tsc suites —
  though CAT-C touches no SCSS class names or types.

## Risks (category-specific)
- **Shared-token blast radius (CAT-C):** the `--theme-mithril-overlay-backdrop-start` backdrop token is shared by the
  fast-sync bootstrap and the partial-sync overlay (same `MithrilBootstrap.scss`). Making it opaque affects both;
  that is deliberate (the two Mithril backdrops should look identical) and harmless for fast-sync (nothing meaningful
  sits behind its backdrop). The `blur(5px)` on `.backdrop` becomes a no-op once the fill is opaque — left in place as
  a no-op, not removed (SCSS untouched, token-only). If a future finding needs the partial-sync backdrop translucent
  while fast-sync stays solid (or vice versa), split a dedicated token then. No product sign-off is open for CAT-C.

## Operator / verify-only gates
(Moved from the master Verification plan's shared **Operator (verify-only)** bullet; "the same node" = the
significantly-behind node of the CAT-A check.)
- **CAT-C:** on the same node, the Mithril Partial Sync overlay's download-status **and** failure prompts read as a
  solid per-theme backdrop with no Cardano Node status page showing through, matching the fast-sync flow, across
  light-blue + a light + a dark theme.

## Cross-category coupling notes
- Same nine theme files + `createTheme.ts` as CAT-A, but different tokens (CAT-A: `--theme-mithril-card-background`;
  CAT-C: `--theme-mithril-overlay-backdrop-start`/`-end`).
- The backdrop token is shared with the fast-sync bootstrap via the shared `MithrilBootstrap.scss` `.backdrop` —
  making both solid is deliberate (the two Mithril backdrops become visually identical).
