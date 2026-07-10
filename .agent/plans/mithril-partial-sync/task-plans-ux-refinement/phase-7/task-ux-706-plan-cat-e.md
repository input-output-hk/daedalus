# task-ux-706 · CAT-E — Button copy-overflow layout fix

Resolves resolution #7 / Extra "UI fix": long labels (exemplar: "Wipe chain data and do full Mithril
sync") overflow the button border in the Mithril partial-sync UI. CSS/layout only. **Run after CAT-A** —
the vocabulary changes lengthen some labels ("Restart Blockchain Sync (slow)", "Blockchain Sync from
Genesis"), so verify against the final strings.

## Diagnosis (from recon)

- Recovery buttons are shared react-polymorph `Button` + `ButtonSkin`, rendered in
  `MithrilErrorView.tsx:172-184` from `MithrilPartialSyncOverlay.tsx:134-171`.
- The theme forces fixed button geometry via CSS vars: `--rp-button-height: 50px`,
  `--rp-button-line-height: 20px`, `--rp-button-width: 360px` (`themes/utils/createTheme.ts:80-86`, applied
  by `react-polymorph/.../SimpleButton.scss`).
- The mithril overrides `.primaryAction` (`MithrilErrorView.scss:85-97`) and `.secondaryAction`
  (`:99-111`) set only padding + font/color — **no** `white-space`, `height`, `min-height`, `max-width`,
  or `flex` control.
- `.actions` (`MithrilErrorView.scss:75-79`) is a `display:flex` row with **no `flex-wrap`**; it only
  stacks to a full-width column below 720px (`:113-118`). Card is `max-width:720px`
  (`MithrilBootstrap.scss:34`).
- Net: 2–3 fixed 360px-basis buttons shrink narrow, the long label wraps to 3+ lines (3×20px = 60px) and
  spills past the fixed 50px border. Neither wrap-to-fit nor ellipsis is handled today.

## Fix

Make the buttons accommodate their label instead of clipping it. Recommended approach (implementer may
tune visually):

1. On the mithril button overrides (`.primaryAction` / `.secondaryAction`), the **primary technique**
   (plan-review inversion) is setting the react-polymorph geometry custom properties **on the module
   rule**: `--rp-button-height: auto; --rp-button-width: auto;` (the earlier example repeated the
   height var where the width var was meant). Custom properties on the rule win deterministically over
   the `documentElement`-inherited theme values (`ThemeManager.tsx:24`); by contrast a class-level
   `height: auto` fights `.root` at equal specificity (0,1,0 — `ButtonSkin` composes both classes on
   the same element) and wins only by injection order, which is statically indeterminable. Add the
   supporting declarations on the same rule: `height: auto; min-height: 50px; white-space: normal;
   --rp-button-line-height: 1.3; width: auto; max-width: 100%;` (line-height goes through the custom
   property too — a class-level `line-height` has the same injection-order indeterminacy vs `.root`).
2. On `.actions`, add `flex-wrap: wrap;` so buttons reflow rather than over-shrinking (a `gap: 12px` is
   already present — `MithrilErrorView.scss:75-79` — don't re-add it), and consider dropping the ≥720px
   single-row assumption for the 3-button error case.
3. Keep the existing <720px column stack; ensure the new auto-height doesn't double-apply there.

Do not change label text to "fix" overflow — the labels are locked by CAT-A/DD-706-1.

## Scope / audit beyond the recovery buttons

Reported scope: "mostly in the mithril partial sync ui." Primary target is `MithrilErrorView` recovery buttons.
Also visually verify — and fix if they clip — the buttons whose labels grew under CAT-A:
- Bootstrap decline `Blockchain Sync from Genesis` — renders in **`MithrilDecisionView.tsx:101-114`**,
  styled by **`MithrilDecisionView.scss`** (`.actions:72-76`, `.secondaryAction:92-104`) — plan-review
  redirect: not a "`MithrilBootstrap` action row" (that file has none; its `failed` state reuses
  `MithrilErrorView`, which the main fix covers for free).
- Proactive prompt `Blockchain Sync (slow)` / `Mithril Sync (fast)` (`SyncingConnectingMithrilPrompt.tsx`
  button row `:177-193`; its scss `:62-97` adds `min-width: 180px`).
- Confirmation modal `Start Mithril Sync` / `Back to Daedalus Diagnostics` — these render via the
  **`Dialog` widget's `actions` array** (`MithrilPartialSyncConfirmation.tsx:72-85`; `Dialog.scss:83-94`
  sets `width: 50%` + margins, `DaedalusDiagnostics.scss:466-472` adds `min-width: 180px`) — a
  different fix shape than the flex-row recipe.

Full inventory (plan-review): four scss modules independently restyle the same fixed-geometry skin with
no height/wrap control — `MithrilErrorView.scss`, `MithrilDecisionView.scss`,
`SyncingConnectingMithrilPrompt.scss`, and the confirmation's Dialog-widget buttons. Only touch the scss
for rows that actually overflow; don't restyle healthy buttons — but the visual verification must cover
all four, minding how the `min-width: 180px` rules interact with any wrap recipe.

## Acceptance

- No label overflows its button border at the default card width and when buttons are shrunk in the
  multi-button error row — tested with the **longest** post-CAT-A labels in EN and JA.
- Buttons either grow in height to fit wrapped text or reflow via `flex-wrap`; no ellipsis-clipping that
  hides words.
- <720px column-stack behavior preserved.
- Storybook `MithrilPartialSyncOverlay` / `MithrilErrorView` error stories visually verified across the
  recovery-button permutations; `yarn compile` + lint + prettier clean; `.scss.d.ts` regenerated if class
  names change.
