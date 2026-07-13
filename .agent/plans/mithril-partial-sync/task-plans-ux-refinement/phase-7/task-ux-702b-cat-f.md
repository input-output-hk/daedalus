# task-ux-702b-cat-f — Shared `<CompletionBlock>`; remove dead `.primaryButton` CSS (Findings #12, #13)

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-F is **standalone**. It lands **after CAT-H** and **before CAT-G** in the collision-safe order. It
touches `MithrilProgressView.tsx` + `SyncingConnectingMithrilPrompt.scss`/`.tsx` — **no overlap with
CAT-A…E** (none of which edit these files) and **no overlap with the backend (CAT-H)** or the overlay
spec (CAT-G). There are no shared files to serialize against, so its only ordering constraint is the
per-category compile/review cadence below. Implement it, then run `yarn compile` + the touched specs +
a code-review pass before CAT-G.

> **Full collision-safe order:** `CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`, each
> followed by `yarn compile` + the touched specs + a per-category code-review pass. Store-file editors
> (`MithrilPartialSyncStore.ts`) serialize **B → A → D → E**. Overlay-file editors
> (`MithrilPartialSyncOverlay.tsx`) serialize **C → D**; the overlay spec is also touched by **G** (run
> order C → D → G, each its own test case). **CAT-B before CAT-A** — the container near-tip gate consumes
> CAT-B's `computeBehindByEpochs`. **CAT-H is backend-only** (collision-free), placed after CAT-E.
> **CAT-F is standalone.** **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by
> safe degradation** — an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only
> (= today, no regression).

## Findings closed & decisions implemented

| Finding # | Short desc | Decision | Severity |
|---|---|---|---|
| #12 | Three near-identical completion blocks in `MithrilProgressView.tsx` (stopping / starting / completed) | D-702b-7 | Cleanup |
| #13 | Dead `.primaryButton` CSS rule + its two `styles.primaryButton` refs in `SyncingConnectingMithrilPrompt` | D-702b-7 | Cleanup |

## Locked invariants this change must NOT break

> - **#11 Do not regress the empty-chain Mithril bootstrap.** CAT-F leaves bootstrap's completed frame
>   byte-for-byte unchanged: bootstrap never passes `completedTransitionLabel`, so the shared
>   `<CompletionBlock>` reproduces the current stopping/starting frames exactly (spinner bottom, with
>   detail) and only the partial-sync overlay uses the spinner-top/no-detail completed frame.
> - **No copy / i18n changes (D-702b-7).** Call sites keep their own `prop || intl.formatMessage(...)`
>   title/detail resolution; rendered DOM/ARIA/widths are unchanged.

## Exact files (full repo-relative paths)

- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx` — extract the shared
  `CompletionBlock` helper; replace the three inline completion blocks (#12).
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss`
  (+ regen `.scss.d.ts`) — delete the dead `.primaryButton` rule (#13).
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` — drop
  the two `styles.primaryButton` refs (#13).

## Grounding (re-verified against live code 2026-06-30)

- `MithrilProgressView.tsx` — completion blocks at `:174` (stopping), `:197` (starting), `:220`
  (completed-transition); shared `div.completionBlock` + `role=status` / `aria-live=polite` /
  `aria-atomic=true` + `SVGInline` spinner (`styles.completionSpinner`). Stopping/starting render
  `<h2 completionTitle>` → `<p completionDetail>` → spinner; the completed block renders spinner **FIRST**,
  then `<h2 completionTitle>`, and **no** detail `<p>`. Gate `isCompletedTransition` `:108-109`.
- `SyncingConnectingMithrilPrompt.scss` — `.actionButton` `:57-60` (`min-width:180px`), dead
  `.primaryButton` `:62-68` (only `min-width:180px`); `.tsx` references `styles.primaryButton` in two
  `classNames([...])` at `:174-179` (choice view "Mithril Sync (fast)") and `:206-211` (confirm view
  "Start now"), always co-applied with `'primary'` + `styles.actionButton`.

## Implementation steps (ordered, mechanical)

- [ ] **Extract `CompletionBlock` (#12).** In `MithrilProgressView.tsx`, add a small local helper
  component (or function returning JSX) `CompletionBlock` with props
  `{ title: string; detail?: string; spinnerPosition?: 'top' | 'bottom' }` (default `'bottom'`). It
  renders the shared wrapper `div.completionBlock` + `role="status"` + `aria-live="polite"` +
  `aria-atomic="true"`, the `SVGInline` spinner (`styles.completionSpinner`, `aria-hidden`),
  `<h2 className={styles.completionTitle}>{title}`, and — only when `detail` is provided —
  `<p className={styles.completionDetail}>{detail}</p>`. When `spinnerPosition==='top'` the spinner
  renders **before** the title (and there is no detail); otherwise title → detail → spinner (matching
  the current stopping/starting layout). Replace the three inline blocks:
  - stopping (`:174-195`): `<CompletionBlock title={stoppingNodeTitle || intl.formatMessage(
    messages.nodeStoppingTitle)} detail={stoppingNodeDetail || intl.formatMessage(
    messages.nodeStoppingDetail)} />` (spinner bottom).
  - starting (`:197-218`): same shape with the `nodeStarting*` resolutions (spinner bottom).
  - completed-transition (`:220-234`): `<CompletionBlock title={completedTransitionLabel}
    spinnerPosition="top" />` (no detail).
  **Call sites keep their own `prop || intl.formatMessage(...)` resolution.** No scss change in this file
  (reuse `.completionBlock` / `.completionSpinner` / `.completionTitle` / `.completionDetail`). Bootstrap
  never passes `completedTransitionLabel`, so its completed frame is byte-for-byte unchanged (#12).
- [ ] **Remove dead `.primaryButton` (#13).** In `SyncingConnectingMithrilPrompt.scss`, delete the
  `.primaryButton` rule (`:62-68`). In `SyncingConnectingMithrilPrompt.tsx`, drop `styles.primaryButton`
  from both `classNames([...])` arrays (`:174-179` and `:206-211`) → `classNames(['primary',
  styles.actionButton])`. Rendered width is unchanged (`.actionButton` already sets `min-width:180px`).
  **Regen `SyncingConnectingMithrilPrompt.scss.d.ts`.**

  > **WARNING (do NOT touch the unrelated class):** an UNRELATED `.primaryButton` class exists in
  > `NotificationActions.scss` / `.tsx` (a different component) — do **NOT** touch it. Only remove the
  > `.primaryButton` in `SyncingConnectingMithrilPrompt.scss:66-68` (+ its comment `:62-65`) and the two
  > `styles.primaryButton` refs in `SyncingConnectingMithrilPrompt.tsx:178` / `:210`. Grep confirmed no
  > test/story references the SyncingConnecting one.

## New symbols

- local `CompletionBlock` component in `MithrilProgressView.tsx` — props
  `{ title: string; detail?: string; spinnerPosition?: 'top' | 'bottom' }` (default `'bottom'`). **Not
  exported.**

## Acceptance

- The three completion blocks render identically to before (same DOM/ARIA/spinner position) via the
  shared helper; bootstrap's completed frame is unchanged.
- `.primaryButton` is gone from the scss + `.d.ts` + both `classNames` calls; the two buttons render at
  the same width; no test/story referenced `.primaryButton` (grep-verified).

## Tests (add/update)

- Existing `MithrilProgressView` / `MithrilStepIndicator` specs stay **green** (same DOM/ARIA/spinner
  position via the shared `CompletionBlock`).
- The `SyncingConnectingMithrilPrompt` spec stays **green** (button widths unchanged after dropping
  `styles.primaryButton`).
- `yarn compile` after the `.scss.d.ts` regen.

## Verify commands

1. **Regen `SyncingConnectingMithrilPrompt.scss.d.ts`** (the `.primaryButton` class is removed).
2. `yarn test:jest` on the prompt + progress-view specs.
3. `yarn compile`
4. `yarn lint`
5. `yarn prettier:check`

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and
> ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest
> failure as a regression (memory: `mithril-ux-renderer-verify-env`).

## Operator / verify-only gates

None.

## Cross-category coupling notes

CAT-F is **standalone** — it touches `MithrilProgressView.tsx` + `SyncingConnectingMithrilPrompt.scss` /
`.tsx`, which no other category (A–E backend/renderer, H backend, G overlay-spec) edits. There are no
shared files to watch. The one trap is the **unrelated `.primaryButton` in `NotificationActions.scss` /
`.tsx`** (a different component): do **NOT** remove or rename it — only the `SyncingConnectingMithrilPrompt`
copy of the class is dead.
