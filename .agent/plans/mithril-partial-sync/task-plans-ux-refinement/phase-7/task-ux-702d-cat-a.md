# task-ux-702d-cat-a — proactive-prompt card: make the fill fully opaque

> Per-category doc, decomposed from the canonical plan `task-ux-702d.md` (task-ux-702d = Mithril Sync UX
> finalization cleanup, ad-hoc wave). **Implementation record — as landed (2026-07-01).** Parent task:
> `task-ux-702d`. Plan review: `task-ux-702d-plan-review.md`. Research: `task-ux-702d-research.md`. If this doc ever
> disagrees with live code, prefer live code and reconcile here.

## Sequencing / status position
First landed CAT of the wave. Presentation/token-only; no file collisions with CAT-B/CAT-C (CAT-C edits different
token lines in the same nine theme files + `createTheme.ts`) — landed A → C in practice.

## Findings closed & decisions implemented
| Finding # | Finding (short) | CAT | Severity |
|---|---|---|---|
| #1 | **Proactive-prompt card is translucent and hard to read**: the card (`SyncingConnectingMithrilPrompt.scss:2` → `background-color: var(--theme-mithril-card-background)`) is a `position: fixed` float over the wallet summary with **no dimming backdrop** (`:11`), and the token `--theme-mithril-card-background` is set at alpha **`0.96`** in every theme, so the live wallet summary bleeds through the card. Distinct from 702c CAT-B (overlay backdrop) and 702c CAT-D (chrome; the "blur" report there was a misdiagnosis — no `backdrop-filter` on this surface). | **CAT-A** | Medium |

## Locked invariants this change must NOT break
- **Vocab guardrail #8** holds — CAT-A is a token-alpha change only; no copy, no "partial sync"/%/immutable strings
  introduced.
- **Offer-signal boundary #4** untouched — proactive-prompt suppression/gating logic is not modified.
- **No new colors, classes, or theme vars** — CAT-A only flips the existing token's alpha; it does not introduce a
  proactive-prompt-specific token (restated inline from the master's Non-goals, which remain authoritative).

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
The proactive prompt is **not** a full-screen dimmed modal. It is a floating card:
`SyncingConnectingMithrilPrompt.scss` `.component` uses `position: fixed` (`:11`) with only its own
`background-color: var(--theme-mithril-card-background)` (`:2`) between the reader and the wallet summary underneath —
there is no `.backdrop`/scrim element for this surface (unlike the `MithrilPartialSyncOverlay` bootstrap card, which
sits on the 702c-firmed backdrop). Because the card's own fill carries alpha `0.96`, the summary bleeds through and
the copy is hard to read.

The transparency is **not** in the SCSS — `.component` has no `opacity`/`rgba`/backdrop rule. It comes entirely from
the value of the CSS variable, which is defined per-theme (static rgba literals) and in the dynamic generator
(`chroma(...).alpha(0.96)`). There is no single shared rule to flip (contrast with 702c CAT-B's shared `.backdrop`),
so a per-theme edit of the static token values plus the generator is the correct and only way to make this token
solid. Setting alpha to `1` keeps each theme's chosen hue and simply removes the bleed-through.

### Reference details (verified against the live post-702c tree, 2026-07-01)
- **Presentational component:** `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx`
  — card wrapper `<div className={styles.component}>` in both views (`renderChoiceView` `:167`, `renderConfirmView`
  `:213`).
- **Container / gating:** `source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx` (visibility
  gated on `isSignificantlyBehind` `:70` + session-attempt guard; `return null` `:77,:89`), mounted at
  `source/renderer/app/App.tsx:130` (sibling of `<Router>`; distinct from `MithrilPartialSyncOverlay` at `App.tsx:99`).
- **SCSS (references the token — unchanged by CAT-A):**
  `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss:2`
  (`background-color: var(--theme-mithril-card-background)`), floats via `position: fixed` (`:11`).
- **Token definition sites (the actual transparency — all at alpha `0.96` before the fix):**
  - `source/renderer/app/themes/daedalus/yellow.ts:368` — `rgba(252, 249, 243, 0.96)`
  - `source/renderer/app/themes/daedalus/white.ts:367` — `rgba(255, 255, 255, 0.96)`
  - `source/renderer/app/themes/daedalus/dark-blue.ts:383` — `rgba(30, 43, 61, 0.96)`
  - `source/renderer/app/themes/daedalus/shelley-testnet.ts:367` — `rgba(46, 47, 69, 0.96)`
  - `source/renderer/app/themes/daedalus/cardano.ts:381` — `rgba(24, 26, 29, 0.96)`
  - `source/renderer/app/themes/daedalus/dark-cardano.ts:367` — `rgba(46, 47, 69, 0.96)`
  - `source/renderer/app/themes/daedalus/flight-candidate.ts:367` — `rgba(46, 47, 69, 0.96)`
  - `source/renderer/app/themes/daedalus/light-blue.ts:379` — `rgba(20, 54, 93, 0.96)`
  - `source/renderer/app/themes/daedalus/incentivized-testnet.ts:368` — `rgba(34, 35, 52, 0.96)`
  - `source/renderer/app/themes/utils/createTheme.ts:597-599` — dynamic:
    `chroma(background.secondary.regular).darken(0.3).alpha(0.96)`.
- **Shared-token consumer (intentionally in scope):**
  `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss:32`
  (`background: var(--theme-mithril-card-background)`) — the post-"Start now" bootstrap/working overlay card also uses
  this token. It already sits on a dimmed backdrop, so making it solid is harmless and keeps the two Mithril cards
  visually consistent. **Accepted, not a regression** — this is a deliberate shared-token change, not a
  proactive-prompt-only edit. (If a future finding requires the bootstrap card to stay translucent, split a dedicated
  proactive-prompt token then; do not fork it pre-emptively.)

### Implementation (as landed)
Flip the token alpha `0.96 → 1` at every definition site, preserving the rgb hue:
- Nine static themes: `'--theme-mithril-card-background': 'rgba(<r>, <g>, <b>, 1)'` (files/lines above).
- Dynamic generator (`createTheme.ts:599`): `.alpha(0.96)` → `.alpha(1)`.

The SCSS `.component` rule is left untouched (it correctly references the token). Sibling tokens
(`--theme-mithril-card-shadow`, `--theme-mithril-panel-border-color`, the overlay-backdrop tokens) are **not** touched
— only the card fill.

Diff shape: 10 files, one line each (9 static themes + `createTheme.ts`). Verified post-edit that no
`--theme-mithril-card-background` definition retains `0.96` (`grep` clean).

### i18n
- None (styling/token-only; vocab-neutral).

### Tests
- **On-device / Storybook is the load-bearing proof.** On a significantly-behind node, trigger the proactive prompt
  over a populated wallet summary and confirm the card is fully solid (no summary visible through it) and the copy is
  legible; repeat across light (white, yellow) and dark (cardano, dark-blue) themes. Storybook:
  `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` renders both views across themes — verify
  the card reads as a solid, per-theme surface.
- **No unit/snapshot coverage exists for this token** (`grep` for `mithril-card-background` across specs/snapshots is
  empty), so no test needed updating; nothing asserts the old `0.96`.
- Checks: `yarn compile` + `yarn lint` + touched-file `yarn prettier:check` green (theme files are plain TS object
  literals — trivial-risk edits). Node v24 renderer verify-env caveat applies if running the renderer jest/tsc suites
  (regen `.scss.d.ts` via typed-scss-modules + the gitignored identity-obj-proxy jest sidecar before treating
  tsc/jest fails as regressions) — though CAT-A touches no SCSS class names or types.

## Risks (category-specific)
- **Shared-token blast radius (CAT-A):** the card token also fills the bootstrap/working overlay card
  (`MithrilBootstrap.scss:32`). Making it solid there is accepted as harmless/consistent (that card sits on a dimmed
  backdrop). If a future finding needs the bootstrap card translucent while the proactive card stays solid, split a
  dedicated token then. No product sign-off is open for CAT-A.

## Operator / verify-only gates
(Moved from the master's wave-level Verification plan.)
- **Automated:** `yarn compile` + touched-file lint/prettier green; no spec references the token, so no suite change.
- **Storybook / on-device (styling proof):** proactive choice + confirm views across light-blue / cardano / white /
  yellow (CAT-A card solid, copy legible); confirm the shared bootstrap card still renders correctly.
- **Operator (verify-only):** on a significantly-behind node, the proactive prompt reads as a solid, legible card with
  no wallet-summary bleed-through, across default + at least one light and one dark theme.

## Cross-category coupling notes
- The token is shared with the bootstrap/working-overlay card (`MithrilBootstrap.scss:32`) — making it solid there is
  a deliberate, accepted shared-token change (see Reference details above).
- Distinct token from CAT-C's overlay-backdrop pair (`--theme-mithril-overlay-backdrop-start`/`-end`); the two CATs
  touch different lines in the same nine theme files + `createTheme.ts`.
