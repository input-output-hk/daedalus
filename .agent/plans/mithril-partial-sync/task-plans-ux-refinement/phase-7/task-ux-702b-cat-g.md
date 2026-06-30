# task-ux-702b-cat-g — Live wipe-and-full-sync render + handler test (Finding #8)

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-G **runs last** in the collision-safe order. It touches **only**
`MithrilPartialSyncOverlay.spec.tsx`. That spec is shared with **CAT-C** (cancelled DOM-order assertion)
and **CAT-D** (completed auto-dismiss / no-unhandled-rejection), so the three categories are serialized
**C → D → G**, each touching its own distinct test case in turn with a compile/review pass between —
there is no real collision on the spec. By the time CAT-G runs, CAT-C and CAT-D have already landed
their cases; CAT-G adds the new live wipe case and keeps the existing cancelled (wipe-absent) test green.

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
| #8 | No live test exercises the real overlay's `canWipeAndFullSync:true` render + `onWipeAndFullSync` handler (the one positive test was rewritten to assert absence) | D-702b-8 | Test gap |

## Locked invariants this change must NOT break

> - **#6 Cancellation forbidden after cutover (CAT-G).** The live wipe test drives from `failed`
>   (post-cutover), NOT `cancelled` — D-702a-2 removed wipe from the pre-cutover cancelled dialogue.

## Exact files (full repo-relative paths)

- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` — add the
  live wipe-and-full-sync render + handler test; keep the existing cancelled (wipe-absent) test green.

## Grounding (re-verified against live code 2026-06-30)

- `MithrilPartialSyncOverlay.spec.tsx` — `renderComponent(overrides)` helper `:40-67` defaults
  `canWipeAndFullSync:false`; the cancelled test asserts wipe **ABSENT**; no remaining real-component
  test drives `canWipeAndFullSync:true`. `act` already imported `:4`.
- `MithrilPartialSyncOverlay.tsx` — the wipe branch is part of `recoveryActions` (`:124-137`); the live
  wipe render branch + its `onWipeAndFullSync` wiring is the **uncovered** code this test exercises.
  `MithrilErrorView` is rendered `:216-223` with `actions={errorActions}`,
  `hintAsBody={status==='cancelled'}`.

## Implementation steps (ordered, mechanical)

- [ ] Add a spec that renders the **real** `MithrilPartialSyncOverlay` with a **post-cutover** status
  where wipe is legitimately offered (drive from `failed`, **NOT** `cancelled` — D-702a-2 removed wipe
  from the pre-cutover cancelled dialogue):

  ```tsx
  renderComponent({
    status: 'failed',
    canRetry: false,
    canRestartNormally: false,
    canWipeAndFullSync: true,
    onWipeAndFullSync,
  })
  ```

  The wipe variant resolves to `variant: 'primary'` from the **BOOLEAN combo** (`canRetry:false`,
  `canRestartNormally:false`, `canWipeAndFullSync:true`), **not** from the status string — `failed` is
  the realistic driver.
  - Assert the wipe button is **present** (query by its label, e.g.
    `/wipe chain data and do full mithril sync/i`).
  - `fireEvent.click` it and assert the injected `onWipeAndFullSync` fired **exactly once**.
- [ ] Keep the existing cancelled test (asserts wipe **ABSENT** with `canWipeAndFullSync:false`) green.

## New symbols

None.

## Acceptance

- The live wipe render branch (`MithrilPartialSyncOverlay.tsx:124-137`) + its `onWipeAndFullSync` wiring
  are covered by a passing test driven from `failed`.

## Tests (add/update)

- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`:
  - **New case:** real overlay rendered via `renderComponent({ status: 'failed', canRetry: false,
    canRestartNormally: false, canWipeAndFullSync: true, onWipeAndFullSync })`; assert the wipe button
    (`/wipe chain data and do full mithril sync/i`) is present, `fireEvent.click` it, assert
    `onWipeAndFullSync` fired exactly once.
  - **Existing case stays green:** the cancelled test asserting wipe **ABSENT** with
    `canWipeAndFullSync:false`.
- The new case + the full existing overlay spec pass.

## Verify commands

1. `yarn test:jest source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
2. `yarn compile`
3. `yarn lint`

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and
> ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest
> failure as a regression (memory: `mithril-ux-renderer-verify-env`).

## Operator / verify-only gates

None.

## Cross-category coupling notes

CAT-G shares the overlay spec `MithrilPartialSyncOverlay.spec.tsx` with **CAT-C** (cancelled DOM-order
assertion) and **CAT-D** (completed auto-dismiss / no-unhandled-rejection) — **run order C → D → G**,
each its own distinct test case with a compile/review pass between, so there is no real collision. CAT-G
must land **after** C and D so it appends its case to a spec already carrying their changes, and it must
not disturb the existing cancelled (wipe-absent) test that CAT-C owns. CAT-G touches **no** production
source — only the spec.
