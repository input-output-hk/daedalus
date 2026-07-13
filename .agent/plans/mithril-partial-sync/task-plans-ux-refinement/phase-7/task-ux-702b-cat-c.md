# task-ux-702b-cat-c — Order-agnostic shared error view; ordering + opt-in right-align in the overlay caller; fix decision view; restore bootstrap default; correct the doc note

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-C is the **third** category in the collision-safe order (`CAT-B → CAT-A → CAT-C → CAT-D → …`). It
lands **after CAT-A** and **before CAT-D**. CAT-C and CAT-D both edit
`MithrilPartialSyncOverlay.tsx`, so they are serialized **C before D**: CAT-C reworks the `errorActions`
ordering/right-align region (`:101-151` / `:216-223`) while CAT-D touches the completed-overlay
`useEffect`/timeout region (`:84-91`) — distinct regions of the same file. The shared spec
`MithrilPartialSyncOverlay.spec.tsx` is touched by **CAT-C** (cancelled DOM-order assertion), **CAT-D**
(completed auto-dismiss) and **CAT-G** (new live wipe test); the serialized order **C → D → G** has each
category touch its own distinct test case with a compile/review pass between, so there is no real
collision on the spec. CAT-C touches no store file, so it does not participate in the B → A → D → E
store-file serialization.

> **Full collision-safe order:** `CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`, each followed by `yarn compile` + the touched specs + a per-category code-review pass. Store-file editors (`MithrilPartialSyncStore.ts`) serialize **B → A → D → E**. Overlay-file editors (`MithrilPartialSyncOverlay.tsx`) serialize **C → D**; the overlay spec is also touched by **G** (run order C → D → G, each its own test case). **CAT-B before CAT-A** — the container near-tip gate consumes CAT-B's `computeBehindByEpochs`. **CAT-H is backend-only** (collision-free), placed after CAT-E. **CAT-F is standalone.** **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by safe degradation** — an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only (= today, no regression).

## Findings closed & decisions implemented

| # | Finding (short) | Decision | Severity |
|---|---|---|---|
| #1 | Shared `MithrilErrorView.orderedActions` "primary-last" filter silently moved the destructive **"Wipe chain & retry"** primary to the far right of the empty-chain bootstrap default (no `actions` prop) | D-702b-5 | High |
| #5 | `MithrilDecisionView.scss:75` flipped to `flex-end` but its `.tsx` DOM order (primary first) was not, leaving its primary on the **left** (mirror image of the error view) | D-702b-5 | Med |

## Locked invariants this change must NOT break

> - **#2 Recovery actions render strictly from `allowedRecoveryActions`.** CAT-C's reorder is a **stable sort, not a filter** — it must not drop/add/filter any action; membership stays from `allowedRecoveryActions`.
> - **#11 Do not regress the empty-chain Mithril bootstrap.** CAT-C restores the bootstrap error-view default (primary-first, left-aligned).

## Exact files (full repo-relative paths)

- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx` — make render order-agnostic; add the opt-in right-align Prop.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.scss` — scope `flex-end` to an opt-in modifier (**regen `MithrilErrorView.scss.d.ts`** — a new class is added).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` — the overlay caller owns ordering (secondary-first, primary-last) + right-align opt-in for the error/cancelled view.
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilDecisionView.tsx` — swap the two `<Button>`s so secondary precedes primary (#5).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilDecisionView.scss` — **verify-only** (keep `:72-76` `flex-end` untouched ⇒ primary on the right after the DOM swap).
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.tsx` — **verify-only** (renders `MithrilErrorView` at `:207-213` with **no** `actions` / **no** `rightAlignActions`; confirm the default restores, no edit).
- specs: `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`, and the bootstrap `MithrilErrorView` / `MithrilDecisionView` spec(s) if present.
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-702a-impl-review.md` — correct the inaccurate "visual-only" note at `:533-537`.

## Implementation steps (ordered, mechanical)

- [ ] **Make `MithrilErrorView` render-order-agnostic.** In `MithrilErrorView.tsx`: delete the
  `orderedActions` filter (`:111-116`); map `resolvedActions` **verbatim** in the `.actions` block
  (`:168-182`). The default `resolvedActions` (`:99-110`, `[wipeAndRetry(primary), decline(secondary)]`)
  is rendered in caller order — primary first — so the bootstrap default returns to **primary-on-left**.
- [ ] **Add opt-in right-alignment.** Add a Prop `rightAlignActions?: boolean` (or `actionsAlignment?:
  'start' | 'end'`). Apply it as a className modifier on the `.actions` container, e.g.
  `className={classNames([styles.actions, rightAlignActions && styles.actionsRightAligned])}` (import
  `classnames`). Default (no prop) = left-aligned.
- [ ] **Scope the `.scss` `flex-end` (D-702b-5(3)).** In `MithrilErrorView.scss`: change `.actions`
  `justify-content: flex-end` → `flex-start` (`:75-79`); add an opt-in modifier
  `.actionsRightAligned { justify-content: flex-end; }`. **Keep `.hintBody` (`:31-36`) untouched** (it is
  CAT-E's live cancelled-hint class). **Regen `MithrilErrorView.scss.d.ts`** (new class).
- [ ] **Overlay caller owns ordering + right-align.** In `MithrilPartialSyncOverlay.tsx`: build
  `errorActions` (currently primary-first, `:101-151`) into display order **secondary-first,
  primary-last** before passing it down — move the old `orderedActions` filter logic here (sort
  non-primary then primary), OR construct the array in that order. Pass `rightAlignActions` (or
  `actionsAlignment="end"`) on the `MithrilErrorView` usage (`:216-223`). This keeps the cancelled/error
  overlay **primary-on-the-right** (per manual-assessment D-702a-2). Do **not** touch the completed
  branch (CAT-D owns it). **GUARDRAIL (locked invariant #2):** this is a **stable reorder** of the SAME
  action set (secondary-first, primary-last) — it must NOT filter, drop, or add any action;
  `allowedRecoveryActions` still determines membership. Reorder by partitioning into non-primary then
  primary (preserving each group's relative order), exactly as the old `orderedActions` did — only the
  location moves (to the caller).
- [ ] **Restore the bootstrap default (verify).** `MithrilBootstrap.tsx:207-213` renders
  `MithrilErrorView` with **no** `actions` and **no** `rightAlignActions` → default
  `[Wipe chain & retry (primary), Decline]`, left-aligned, primary-first — exactly as before 702a. **No
  edit** beyond confirming this.
- [ ] **Fix `MithrilDecisionView` DOM order (#5).** In `MithrilDecisionView.tsx:97-112`, swap the two
  `<Button>`s so `secondaryAction` (decline) is rendered **before** `primaryAction` (accept). Keep
  `MithrilDecisionView.scss:72-76` `flex-end` ⇒ primary renders on the right, matching the corrected
  error view + the `widgets/Dialog.tsx` convention (caller lists primary last → right).
- [ ] **Correct the doc note.** In `task-ux-702a-impl-review.md:533-537`, correct the "Shared-error-view
  note (#11, sanctioned visual-only … No logic/handler change)" claim: the 702a `orderedActions` change
  was a **real DOM child reorder** of a destructive action in the bootstrap default that moved
  "Wipe chain & retry" to the far right — **not** visual-only, and it contradicted
  `task-ux-702a-decisions.md:167-168` ("visual-only … do not regress the empty-chain bootstrap flow").
  Note the remediation: 702b makes the error view order-agnostic and restores the bootstrap default.

## New symbols

- `MithrilErrorView` Prop `rightAlignActions?: boolean` (or `actionsAlignment?: 'start' | 'end'`).
- `MithrilErrorView.scss` class `.actionsRightAligned`.

## Acceptance

- Empty-chain bootstrap error dialog: `[Wipe chain & retry (primary), Decline]`, **left-aligned,
  primary-first** (restored).
- Partial-sync overlay error/cancelled dialog: secondary-first, **primary on the right**, right-aligned
  (opt-in).
- `MithrilDecisionView`: `[Decline (secondary), Accept (primary)]` in the DOM, primary on the right.
- The 702a impl-review note is corrected.

## Tests (add/update)

- `MithrilPartialSyncOverlay.spec.tsx`: the overlay cancelled/error spec keeps its DOM-order assertion
  (`['Restart Node Sync (slow)','Retry Mithril Sync (fast)']`) green.
- Bootstrap error-view spec: add/adjust an assertion that the default (no `actions` prop) renders the
  wipe primary **first** (left).
- `MithrilDecisionView` spec (if present): assert decline-before-accept DOM order.
- No i18n change.

## Verify commands

- `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- the bootstrap error-view spec; `MithrilDecisionView` spec (if present)
- `yarn compile`
- `yarn lint`
- **Regen the `MithrilErrorView.scss.d.ts`** (a new opt-in modifier class `.actionsRightAligned` is added).

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest failure as a regression (memory: `mithril-ux-renderer-verify-env`).

## Operator / verify-only gates

- **D-702b-5 initial-bootstrap-dialog realignment (verify-only).** Whether the *initial Mithril
  bootstrap process* dialogs should also adopt primary-on-right is a **separate, code-reviewed
  decision**. Making the error view order-agnostic already protects the bootstrap default
  (left-aligned, primary-first). Do NOT blind-align the bootstrap.
- **`MithrilBootstrap.tsx:207-213` restore (verify, no edit).** Confirm it still renders `MithrilErrorView`
  with no `actions` / no `rightAlignActions` so the default returns to `[Wipe chain & retry (primary),
  Decline]`, left-aligned, primary-first.

## Cross-category coupling notes

- **`MithrilPartialSyncOverlay.tsx` is shared with CAT-D.** CAT-C edits the `errorActions`
  ordering/right-align region (`:101-151` / `:216-223`); CAT-D edits the completed-overlay
  `useEffect`/timeout region (`:84-91`). Serialize **C before D**; do not touch the completed branch (CAT-D
  owns it).
- **`MithrilPartialSyncOverlay.spec.tsx` is shared with CAT-D and CAT-G.** Run order **C → D → G**, each
  category touching its own distinct test case with a compile/review pass between.
- **`MithrilErrorView.scss.d.ts` regen** is required by CAT-C (new `.actionsRightAligned`). Unrelated to
  CAT-F's `SyncingConnectingMithrilPrompt.scss.d.ts` regen.
