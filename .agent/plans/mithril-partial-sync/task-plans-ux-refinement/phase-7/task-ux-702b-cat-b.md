# task-ux-702b-cat-b — Extract `computeBehindByEpochs`; treat ≤ 0 as undefined; beacon-epoch fallback (Findings #11, #7, #16)

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-B is the **first** category in the collision-safe order and the **first of the four
`MithrilPartialSyncStore.ts` editors** (B → A → D → E). It must **land before CAT-A** because both edit
`MithrilProactivePromptContainer.tsx`: CAT-B switches the container's display math to
`computeBehindByEpochs` (and switches `DaedalusDiagnostics.tsx`); CAT-A then reorders the render so the
gate short-circuits first and consumes the `computeBehindByEpochs` result for the near-tip hide. CAT-B
also makes a **declaration-only** one-line add to the store (`@observable certifiedEpoch`), which is why
it goes first among the store editors — the add is collision-safe and the field stays `undefined` until
CAT-A populates it and CAT-H produces it. Shared files: `MithrilPartialSyncStore.ts` (with A/D/E),
`MithrilProactivePromptContainer.tsx` (with A), and `DaedalusDiagnostics.tsx` /
`DaedalusDiagnosticsDialog.tsx` (CAT-B only).

> **Full collision-safe order:** `CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`, each followed by `yarn compile` + the touched specs + a per-category code-review pass. Store-file editors (`MithrilPartialSyncStore.ts`) serialize **B → A → D → E**. Overlay-file editors (`MithrilPartialSyncOverlay.tsx`) serialize **C → D**; the overlay spec is also touched by **G** (run order C → D → G, each its own test case). **CAT-B before CAT-A** — the container near-tip gate consumes CAT-B's `computeBehindByEpochs`. **CAT-H is backend-only** (collision-free), placed after CAT-E. **CAT-F is standalone.** **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by safe degradation** — an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only (= today, no regression).

## Findings closed & decisions implemented

| Finding # | short desc | Decision | Severity |
|---|---|---|---|
| #7 | Misleading "about 1 epochs behind" from `Math.max(1, …)` clamp at diff ≤ 0 | D-702b-2 | Med |
| #11 | Byte-identical behind-by-epochs math duplicated in `MithrilProactivePromptContainer.tsx` and `DaedalusDiagnostics.tsx` | D-702b-2 | Cleanup |
| #16 | Behind-ness gate + displayed figure anchor on cardano-wallet's late-resolving `networkTip.epoch`; re-anchor on the Mithril certified-beacon epoch (hybrid: prefer `networkTip.epoch` when finite, else `certifiedEpoch`). **CAT-B half = the `certifiedEpoch` fallback param + the store-observable declaration.** | D-702b-10 | High |

## Locked invariants this change must NOT break

> - **#4 Backend owns the OFFER** (`isSignificantlyBehind`). The renderer can only **suppress** an offer (near-tip hide) or set anti-flash known-ness (`certifiedKnown`), **never create** an offer or compute a threshold.
> - **Gate-vs-display decoupling (D-702b-2/D-702b-10):** `isMithrilBehindnessKnown` / `NetworkStatusStore.isBehindnessKnown` stay **byte-identical** and clamp-free (true at equal epochs). The `≤ 0 ⇒ undefined` logic lives **only** in the display-only `computeBehindByEpochs`, consumed in the container render; the `certifiedKnown` OR is composed **only** in the container — never baked into the named gate util.
> - **Epochs-only user-facing vocabulary (D13 / `ux-copy-cardano-vocabulary`):** never surface immutable/file numbers or sync-% to the user; `certifiedEpoch` is a parsed integer used as an epochs figure.

## Exact files (full repo-relative paths)

- `source/renderer/app/utils/mithrilBehindness.ts` — add the new `computeBehindByEpochs` export; keep `isMithrilBehindnessKnown` byte-identical.
- `source/renderer/app/utils/mithrilBehindness.spec.ts` — add `computeBehindByEpochs` cases (incl. #16 hybrid-fallback).
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — replace duplicated math copy B with the util; add a `certifiedEpoch?: number | null` prop.
- `source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx` — replace `Math.max(1, …)` display block with the util (leave it above the gate; CAT-A reorders later).
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — pass `certifiedEpoch={mithrilPartialSync.certifiedEpoch}` next to `localTip`/`networkTip` at `:144-145`.
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — **declaration only:** add `@observable certifiedEpoch: number | null | undefined = undefined;` (populated by CAT-A, produced by CAT-H).

## Implementation steps (ordered, mechanical)

1. **`mithrilBehindness.ts` — add `computeBehindByEpochs`.** Add
   `export const computeBehindByEpochs = (localTip, networkTip, certifiedEpoch?: number | null | undefined): number | undefined`.
   Logic (**hybrid, D-702b-10**):
   - if `!Number.isFinite(localTip?.epoch)` → return `undefined`;
   - pick the **network-side anchor** =
     `Number.isFinite(networkTip?.epoch) ? networkTip.epoch : (Number.isFinite(certifiedEpoch) ? certifiedEpoch : undefined)`;
   - if the anchor is `undefined` → return `undefined`;
   - else `diff = anchor − localTip.epoch`; return `diff > 0 ? diff : undefined`.

   **DISPLAY-ONLY.** Doc comment: returns `undefined` for diff ≤ 0 (node level/ahead) and that it is NOT
   the gate (the gate is the unchanged `isMithrilBehindnessKnown`); **`certifiedEpoch` (the Mithril
   certified-beacon epoch) is the early-sync fallback for the late-resolving `networkTip.epoch` — when
   omitted/undefined the helper behaves exactly as the networkTip-only version (no regression).**

2. **Keep `isMithrilBehindnessKnown` (`:18-22`) BYTE-IDENTICAL (D-702b-10 §4 / D-702b-2).** Do **NOT**
   add `certifiedEpoch` to it — it remains the **networkTip** sub-signal, clamp-free, returns `true` at
   equal epochs. The certified-known OR is composed at the **container** (CAT-A), never baked into this
   util — preserving the gate-vs-display decoupling.

3. **`mithrilBehindness.spec.ts` — add `computeBehindByEpochs` cases:** both tips finite & diff > 1 →
   that diff; diff == 1 → 1; diff == 0 → `undefined`; diff < 0 → `undefined`; missing / non-finite tip →
   `undefined`. **#16 hybrid-fallback cases:** `networkTip` null/non-finite **but** `certifiedEpoch`
   finite & `> localTip.epoch` → that diff (early-sync path); `networkTip` finite → result uses
   `networkTip.epoch` and **ignores** `certifiedEpoch` even when they differ (prefer-networkTip); both
   `networkTip` and `certifiedEpoch` absent → `undefined`; `certifiedEpoch` finite but `≤ localTip.epoch`
   → `undefined`; `localTip.epoch` non-finite (regardless of either anchor) → `undefined`. Keep the
   existing `isMithrilBehindnessKnown` cases unchanged (it still takes only `localTip`/`networkTip`).

4. **Declare the store observable (compile-order, D-702b-10).** CAT-B runs **before** CAT-A, but both
   call sites below now read `mithrilPartialSync.certifiedEpoch`, so add
   `@observable certifiedEpoch: number | null | undefined = undefined;` to `MithrilPartialSyncStore.ts`
   (**declaration only**, default `undefined`). **CAT-A populates it** (in `_applyAvailability`) and adds
   the `certifiedKnown` OR to the container gate; **CAT-H produces it** in the backend probe + IPC
   payload. Until A+H land it stays `undefined` ⇒ `computeBehindByEpochs` degrades to networkTip-only (no
   regression). CAT-B is the first of the four `MithrilPartialSyncStore` editors (B → A → D → E), so this
   one-line add is collision-safe.

5. **`DaedalusDiagnostics.tsx:570-577`** — replace the `networkEpoch`/`localEpoch`/`behindByEpochs =
   Math.max(1, …)` block with
   `const behindByEpochs = computeBehindByEpochs(localTip, networkTip, certifiedEpoch);` (add the import;
   add a `certifiedEpoch?: number | null` prop). Delete the two intermediates. **NOTE:** only the Mithril
   behind-by-epochs *figure* (`:574-577`) changes; the raw network-tip/local-tip diagnostic dump rows
   (`:742-779`) are untouched (they legitimately show "—" when `networkTip` is null). The downstream
   consumer (`MithrilPartialSyncConfirmation.tsx:67-99`) already renders `behindUnknown` when
   `behindByEpochs` is undefined, so the confirmation modal's near-tip "1 epochs" is fixed too.

6. **`DaedalusDiagnosticsDialog.tsx:144-145`** — wire the prop next to `localTip`/`networkTip`:
   `certifiedEpoch={mithrilPartialSync.certifiedEpoch}`. The sibling Mithril prop already passed there is
   named `isMithrilPartialSyncSignificantlyBehind` (`:135-137`); add `certifiedEpoch` next to the
   existing `localTip={localTip}` (`:144`) / `networkTip={networkTip}` (`:145`). The dialog already
   injects `mithrilPartialSync`, so no new inject is required.

7. **`MithrilProactivePromptContainer.tsx:45-52`** — replace the `Math.max(1, …)` display block with
   `const behindByEpochs = computeBehindByEpochs(localTip, networkTip, mithrilPartialSync.certifiedEpoch);`
   (import added). **Leave the block above the gate for now — CAT-A reorders it.** The prompt consumer
   fallback (`SyncingConnectingMithrilPrompt.tsx:146-156`, message key `promptBodyUnknown` — not
   literally "behindUnknown") renders the unknown-behind fallback when undefined.

## New symbols

- `computeBehindByEpochs(localTip: TipInfo | null | undefined, networkTip: TipInfo | null | undefined, certifiedEpoch?: number | null | undefined): number | undefined`
  — exported from `utils/mithrilBehindness.ts` (**#16** adds the third param).
- `certifiedEpoch: number | null | undefined` (`@observable`, default `undefined`) on
  `MithrilPartialSyncStore` — **declared** here, **populated** in CAT-A.
- `DaedalusDiagnostics` prop `certifiedEpoch?: number | null` (**#16**).

## Acceptance

- Both call sites use the util; the `networkEpoch`/`localEpoch` intermediates are gone.
- `computeBehindByEpochs` returns `undefined` at diff ≤ 0; `isMithrilBehindnessKnown` is byte-for-byte
  unchanged. The confirmation modal and prompt show the `behindUnknown` fallback at the tip instead of
  "about 1 epochs behind."
- **#16:** with `networkTip` null but a finite `certifiedEpoch > localTip.epoch`, the util returns the
  certified diff (the figure is available early); with `networkTip` finite it prefers `networkTip.epoch`.
  When `certifiedEpoch` is `undefined` (the store field not yet populated by CAT-A/produced by CAT-H) the
  util behaves exactly as the networkTip-only version (no regression). The store observable exists and
  defaults `undefined`; `tsc` is clean at the end of CAT-B (no dangling reference to an unpopulated
  field).
- `mithrilBehindness.spec.ts` passes; `tsc`/`lint` clean.

## Tests (add/update)

- `source/renderer/app/utils/mithrilBehindness.spec.ts` — new `computeBehindByEpochs` cases (diff > 1,
  == 1, == 0, < 0, non-finite tip) **plus the #16 hybrid-fallback cases** listed in Implementation step
  3; keep the existing `isMithrilBehindnessKnown` cases unchanged.
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx` — must still pass
  (undefined → `behindUnknown`).

## Verify commands

1. Regen the relevant `.scss.d.ts` via `typed-scss-modules` (none changed here, but ensure the sidecar
   is present so the new util import type-checks).
2. `yarn test:jest source/renderer/app/utils/mithrilBehindness.spec.ts`
3. `yarn test:jest source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx`
4. `yarn compile`
5. `yarn lint`

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest failure as a regression (memory: `mithril-ux-renderer-verify-env`).

## Operator / verify-only gates

None. (The #16 end-to-end behavior depends on the **CAT-H** operator beacon-epoch verify and CAT-A's
population, but CAT-B itself ships a self-contained, safely-degrading util + declaration.)

## Cross-category coupling notes

- **`MithrilPartialSyncStore.ts`** is also edited by CAT-A (populate `certifiedEpoch` + re-pop guard),
  CAT-D (`dismissCompletedOverlay`), CAT-E (`_refreshAvailability` + back-off) — land in store order
  **B → A → D → E**. CAT-B's contribution is the single declaration-only line.
- **`MithrilProactivePromptContainer.tsx`** is also edited by CAT-A. CAT-B only swaps the display math to
  `computeBehindByEpochs` and **leaves the block above the gate**; CAT-A then reorders the render so the
  gate short-circuits first and consumes the helper for the near-tip hide. Land **CAT-B before CAT-A**.
- `DaedalusDiagnostics.tsx` / `DaedalusDiagnosticsDialog.tsx` are CAT-B-only (no other category touches
  them).
