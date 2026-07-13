# task-ux-702b-cat-a — Proactive-prompt lifecycle: node-loaded trigger, near-tip hide, re-pop guard, perf reorder, drop dead inject, beacon-epoch gate (Findings #3, #4, #10, #14, #16)

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-A is the **second** category and the **second of the four `MithrilPartialSyncStore.ts` editors**
(B → A → D → E). It must **land after CAT-B**: CAT-B declares the `@observable certifiedEpoch` and
switches the container's display math to `computeBehindByEpochs`; CAT-A then (1) populates that field in
`_applyAvailability`, (2) reorders the container `render()` so the cheap boolean gate short-circuits
first and consumes `computeBehindByEpochs` for the near-tip hide, and (3) adds the `certifiedKnown` OR to
the gate. Shared files: `MithrilPartialSyncStore.ts` (with B/D/E) and
`MithrilProactivePromptContainer.tsx` (with B). **Keeps the app-level mount + cross-screen persistence
(REVERSES the review's route-scoping).**

> **Full collision-safe order:** `CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`, each followed by `yarn compile` + the touched specs + a per-category code-review pass. Store-file editors (`MithrilPartialSyncStore.ts`) serialize **B → A → D → E**. Overlay-file editors (`MithrilPartialSyncOverlay.tsx`) serialize **C → D**; the overlay spec is also touched by **G** (run order C → D → G, each its own test case). **CAT-B before CAT-A** — the container near-tip gate consumes CAT-B's `computeBehindByEpochs`. **CAT-H is backend-only** (collision-free), placed after CAT-E. **CAT-F is standalone.** **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by safe degradation** — an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only (= today, no regression).

## Findings closed & decisions implemented

| Finding # | short desc | Decision | Severity |
|---|---|---|---|
| #3 | Proactive prompt flashes during connecting / "verifying blockchain", and shows the misleading near-tip offer; fix trigger/persist/near-tip-hide (route-scoping is **reversed**) | D-702b-1 | High |
| #4 | Prompt re-pops after a Mithril attempt ends (completed/cancelled/failed/restart-normal) because only the dismiss flag — written nowhere on those paths — gates it | D-702b-3 | High |
| #10 | Dead `actions` inject in `MithrilProactivePromptContainer` (`@inject('stores','actions')`, `actions` unused) | D-702b-1 (CAT-A bundle) | Cleanup |
| #14 | Observer-perf: the container reads the tip observables and computes the figure **above** the gate (figure computed even when the prompt is hidden) | D-702b-1 (CAT-A bundle) | Cleanup |
| #16 | Behind-ness gate + display anchor on cardano-wallet's late-resolving `networkTip.epoch`; re-anchor on the beacon. **CAT-A half = populate `certifiedEpoch` in `_applyAvailability`; broaden the gate's anti-flash known-term to `(isBehindnessKnown \|\| certifiedKnown)`; feed `certifiedEpoch` to `computeBehindByEpochs`.** | D-702b-10 | High |

## Locked invariants this change must NOT break

> - **#4 Backend owns the OFFER** (`isSignificantlyBehind`). The renderer can only **suppress** an offer (near-tip hide) or set anti-flash known-ness (`certifiedKnown`), **never create** an offer or compute a threshold.
> - **Gate-vs-display decoupling (D-702b-2/D-702b-10):** `isMithrilBehindnessKnown` / `NetworkStatusStore.isBehindnessKnown` stay **byte-identical** and clamp-free (true at equal epochs). The `≤ 0 ⇒ undefined` logic lives **only** in the display-only `computeBehindByEpochs`, consumed in the container render; the `certifiedKnown` OR is composed **only** in the container — never baked into the named gate util.
> - **Epochs-only user-facing vocabulary (D13 / `ux-copy-cardano-vocabulary`):** never surface immutable/file numbers or sync-% to the user; `certifiedEpoch` is a parsed integer used as an epochs figure.

**KEEP BOTH `isBehindnessKnown` AND the near-tip `behindByEpochs === undefined` early-return — do
NOT collapse them (reviewer ruling F2).** Although `behindByEpochs === undefined` logically subsumes
`isBehindnessKnown` (both require finite tips), the two-stage structure is **required, not optional**:
(1) collapsing breaks the existing anti-flash test at `MithrilProactivePromptContainer.spec.tsx:98-110`,
which sets `isBehindnessKnown:false` with **finite** tips (100/97) and expects no render — a lone
`undefined` check would render there (97 < 100 ⇒ diff 3); (2) it violates the D-702b-2 gate-vs-display
decoupling invariant (the gate must not depend on the display helper); (3) it loses the #14 perf
short-circuit (`isBehindnessKnown` is the cheap named boolean that returns BEFORE `computeBehindByEpochs`
is evaluated). `isBehindnessKnown` is the named anti-flash short-circuit; the `undefined` early-return is
the near-tip display-availability hide. Both stay.
  - **#16 leaves F2 intact.** The first stage becomes `(isBehindnessKnown || certifiedKnown)` — still a
    cheap boolean short-circuit before `computeBehindByEpochs`. The `:98-110` mock keeps `certifiedEpoch`
    **undefined** ⇒ `certifiedKnown` false ⇒ the OR is false ⇒ still no render; and a *lone* `undefined`
    check would STILL render there (the figure prefers the finite `networkTip` ⇒ `100 − 97 = 3`, defined),
    so both stages remain required. (Add `certifiedEpoch: undefined` to the default mock — see Tests.)

## Exact files (full repo-relative paths)

- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — re-pop guard `mithrilAttemptStartedThisSession` + **#16** `certifiedEpoch` population in `_applyAvailability`.
- `source/common/types/mithril-partial-sync.types.ts` — **#16** add optional `certifiedEpoch?: number | null` to the `MithrilPartialSyncAvailability` payload type (CAT-H supplies the value).
- `source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx` — drop dead `actions` inject; gate + near-tip hide + perf reorder; `certifiedKnown` OR.
- `source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx` — extend mocks + add cases.
- `source/renderer/app/App.tsx` — **verify-only.** The mount already exists at `:130`; **no edit expected** unless the comment at `:126-129` needs a one-line update to mention the node-loaded gate.
- `NetworkStatusStore.ts` — **verify-only.** `isConnected`/`isBehindnessKnown` already exist; **no edit**.

## Implementation steps (ordered, mechanical)

1. **Store re-pop guard (D-702b-3).** In `MithrilPartialSyncStore.ts`, after
   `proactivePromptDismissedThisSession` (`:67`) add
   `@observable mithrilAttemptStartedThisSession = false;`. In `startPartialSync` (`:250-279`), set
   `this.mithrilAttemptStartedThisSession = true;` **before the optimistic
   `_updateStatus({ status: START_PENDING_STATUS, … })` call** (which is an object-literal snapshot, not
   a bare constant; the literal first statement in `startPartialSync` is `let startError: unknown;`, so
   insert the flag set right after that `let` and before the `_updateStatus` call). Session-scoped,
   in-memory, **never reset** (no idle reset). **Keep `proactivePromptDismissedThisSession`
   single-purpose** (still written only by `dismissProactivePrompt`).

2. **Populate `certifiedEpoch` from the availability payload (#16, D-702b-10).** The observable is
   **declared in CAT-B** (`@observable certifiedEpoch: number | null | undefined = undefined;`); here
   CAT-A (1) adds `certifiedEpoch?: number | null` to the `MithrilPartialSyncAvailability` IPC payload
   type (`source/common/types/mithril-partial-sync.types.ts` — **optional**, so the renderer type-checks
   before CAT-H's backend production lands), and (2) sets
   `this.certifiedEpoch = availability.certifiedEpoch;` in `_applyAvailability` (`:219-227`, next to
   `behindByImmutables` `:226`). **Compile-order:** the consumer owns the *optional type field* (added
   here) so the read type-checks pre-CAT-H; CAT-H supplies the *value*. Until CAT-H lands the field is
   `undefined` ⇒ the gate's `certifiedKnown` is false and the figure degrades to networkTip-only (no
   regression).

3. **Drop dead `actions` inject (#10).** In `MithrilProactivePromptContainer.tsx`, change
   `@inject('stores', 'actions')` (`:31`) → `@inject('stores')`; remove `actions: null` from
   `defaultProps` (`:34-37`); narrow `Props` so it no longer requires `actions` (e.g. a
   `{ stores: StoresMap }` shape, or keep `InjectedProps` but stop referencing `actions`). The container
   never used `actions`.

4. **Gate + near-tip hide + perf reorder (#3, #14).** Rewrite `render()` so the **cheap boolean gate
   short-circuits first** and the figure is computed **after** the early return (#14), then a
   **second-stage near-tip hide** uses the CAT-B display helper:

   ```tsx
   render() {
     const { networkStatus, mithrilPartialSync } = this.props.stores;
     const { localTip, networkTip, isConnected, isBehindnessKnown } = networkStatus;
     const { certifiedEpoch } = mithrilPartialSync; // #16 (D-702b-10): early-sync beacon anchor

     // #16: combined known-ness = local epoch finite AND (live network tip finite OR certified epoch
     // finite). The named NetworkStatusStore.isBehindnessKnown stays the networkTip sub-signal; the
     // certified OR is composed HERE (keeps the util/gate decoupling — D-702b-2/§4 of D-702b-10).
     const certifiedKnown =
       Number.isFinite(localTip?.epoch) && Number.isFinite(certifiedEpoch);

     const isGated =
       mithrilPartialSync.status === 'idle' &&
       mithrilPartialSync.isPartialSyncEnabled &&
       mithrilPartialSync.isSignificantlyBehind &&   // backend offer signal (near-tip ⇒ false)
       isConnected &&                                 // D-702b-1(a): node loaded, past verifying
       (isBehindnessKnown || certifiedKnown) &&       // #16: anti-flash known-gate, now beacon-aware
       !mithrilPartialSync.mithrilAttemptStartedThisSession && // D-702b-3 re-pop guard
       !mithrilPartialSync.proactivePromptDismissedThisSession;

     if (!isGated) return null;

     // Display figure + near-tip hide (D-702b-1(c)): undefined when local ≥ chosen anchor.
     // #16: hybrid anchor — prefer networkTip.epoch when finite, else certifiedEpoch.
     const behindByEpochs = computeBehindByEpochs(localTip, networkTip, certifiedEpoch);
     if (behindByEpochs === undefined) return null;

     return (
       <SyncingConnectingMithrilPrompt
         behindByEpochs={behindByEpochs}
         onStart={mithrilPartialSync.startPartialSync}
         onDismiss={mithrilPartialSync.dismissProactivePrompt}
       />
     );
   }
   ```

   - **`isConnected`** is the node-loaded / past-verifying pin (`NetworkStatusStore.ts:836-839`;
     `isVerifyingBlockchain` is `!isConnected && …`, so `isConnected` implies past-verifying).
   - **`isBehindnessKnown`** stays as the cheap, named anti-flash short-circuit (it tracks the tip
     observables so the observer re-renders when the network tip arrives, and it short-circuits BEFORE
     `computeBehindByEpochs` is evaluated — satisfying #14).
   - **`behindByEpochs === undefined ⇒ return null`** is the near-tip hide tied to `computeBehindByEpochs
     ≤ 0` (D-702b-1(c)). With the backend `isSignificantlyBehind` also flipping false near tip (kept
     fresh by the CAT-E always-on poll), the prompt dismisses automatically once the node catches up, AND
     the misleading "about 1 epochs behind" is eliminated at the source.
   - Update the top-of-file comment to also state the **node-loaded (`isConnected`)** trigger and the
     **near-tip hide**; keep the persistence + mutual-exclusion narrative.

## New symbols

- `mithrilAttemptStartedThisSession: boolean` (`@observable`, default `false`) on
  `MithrilPartialSyncStore`; set `true` in `startPartialSync`.
- **#16:** `MithrilPartialSyncAvailability.certifiedEpoch?: number | null` (IPC payload type); the
  `certifiedEpoch` observable populated in `_applyAvailability`; local `certifiedKnown` in the container
  `render()`.

## Decoupling / anti-regression invariants (must hold)

- `isMithrilBehindnessKnown` / `NetworkStatusStore.isBehindnessKnown` are **unchanged** and stay
  clamp-free (true at equal epochs). The near-tip ≤ 0 logic lives only in `computeBehindByEpochs`
  (display) and is consumed in the **container** render, never baked into the gate computed.
- **#16 (D-702b-10):** the container's anti-flash known-term broadens from `isBehindnessKnown` to
  `(isBehindnessKnown || certifiedKnown)`, where `certifiedKnown = finite(localTip.epoch) &&
  finite(certifiedEpoch)`. The **named** util `isMithrilBehindnessKnown(localTip, networkTip)` and
  `NetworkStatusStore.isBehindnessKnown` remain byte-identical (still the networkTip sub-signal); the OR
  is composed only in the container, so the gate-vs-display decoupling (D-702b-2) holds. The display
  helper's hybrid anchor (`networkTip.epoch ?? certifiedEpoch`) lives only in `computeBehindByEpochs`.
- (See **Locked invariants** above for the verbatim F2 "KEEP BOTH gate stages" reviewer ruling.)

## Acceptance

- The prompt does **not** flash during connecting / "verifying blockchain"; it appears only once
  `isConnected && (isBehindnessKnown || certifiedKnown)` AND `computeBehindByEpochs > 0`; it persists
  across every route (app-level mount unchanged) until the user picks Standard or Mithril this session.
- **#16:** during early/mid sync — `networkTip` not yet resolved but a finite `certifiedEpoch >
  localTip.epoch` — the prompt now **appears** (with the certified-anchored figure) instead of staying
  suppressed; this is the core defect fixed. When `networkTip` later resolves the figure may tick **up**
  by the certified-frontier lag (commonly ~1 epoch, occasionally more); never down — accepted, not
  clamped.
- After any Mithril attempt begins this session, the prompt never re-offers regardless of the terminal
  outcome (completed/cancelled/failed/restart-normal).
- The prompt hides automatically once the node reaches the tip (near-tip hide + backend signal).
- The container no longer injects `actions`; the figure is computed only when the prompt renders.

## Tests (add/update)

**`MithrilProactivePromptContainer.spec.tsx`:** extend the `renderContainer` default `networkStatus`
mock to add `isConnected: true`, and the default `mithrilPartialSync` mock to add
`mithrilAttemptStartedThisSession: false` **and `certifiedEpoch: undefined` (#16)** so the existing
networkTip-based cases (incl. the `:98-110` anti-flash case) are unaffected. **Also drop
`actions={{} as any}` from the `renderContainer` render helper (`:48-52`):** CAT-A narrows the
container's inject to `@inject('stores')` and drops `actions` from `Props`, so the spec must stop passing
`actions` to the rendered container — confirm with `yarn compile`. Add cases:

- `isConnected: false` → renders nothing (node-loaded gate).
- near-tip: `localTip`/`networkTip` with diff ≤ 0 (e.g. equal epochs) → renders nothing (near-tip hide),
  even though `isBehindnessKnown` is true.
- `mithrilAttemptStartedThisSession: true` → renders nothing (re-pop guard).
- **#16 early-sync render:** `isConnected: true`, `isSignificantlyBehind: true`, `networkTip: null` (so
  `isBehindnessKnown: false`), `localTip.epoch` finite, **`certifiedEpoch` finite and >
  `localTip.epoch`** → the prompt **renders** with the certified-anchored figure (the defect-fix case
  that previously stayed suppressed).
- **#16 prefer-networkTip:** both `networkTip.epoch` and `certifiedEpoch` finite but different → the
  figure uses `networkTip.epoch` (assert the rendered behind-by value reflects the networkTip diff, not
  the certified diff).
- keep the existing all-pass / non-idle / disabled / not-significantly-behind / `isBehindnessKnown:
  false` (now with `certifiedEpoch: undefined`) / dismissed cases green.

**Store-spec cases (`MithrilPartialSyncStore.spec.ts`):** `startPartialSync` sets
`mithrilAttemptStartedThisSession = true`; **#16: `_applyAvailability` with a payload carrying
`certifiedEpoch` sets the observable, and a payload without it leaves `certifiedEpoch` `undefined`.**

## Verify commands

1. `yarn test:jest source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx`
2. `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
3. `yarn compile`
4. `yarn lint`

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest failure as a regression (memory: `mithril-ux-renderer-verify-env`).

## Operator / verify-only gates

- **D-702b-1 fixed-banner visual caveat (verify-only).** The prompt is a `position:fixed; top:84px;
  z-index:10` banner (`SyncingConnectingMithrilPrompt.scss:1-16`) that now persists over
  Settings/Staking/Voting/Wallet sub-pages. **Operator must verify it does not obscure the top
  navigation or page content** on those routes; adjust positioning/offset **only if it collides** (no
  blind change). This is the one residual risk of the "persist everywhere" decision.
- **`App.tsx` (verify-only):** the mount already exists at `:130`; no edit expected unless the comment at
  `:126-129` needs a one-line update to mention the node-loaded gate.
- **`NetworkStatusStore.ts` (verify-only):** `isConnected`/`isBehindnessKnown` already exist; no edit.
- **#16 end-to-end is live only once CAT-H lands** (and its operator beacon-epoch verify passes). Until
  then `certifiedEpoch` is `undefined` ⇒ the gate/figure degrade to networkTip-only (= today, no
  regression).

## Cross-category coupling notes

- **`MithrilPartialSyncStore.ts`** is also edited by CAT-B (declares `certifiedEpoch`), CAT-D
  (`dismissCompletedOverlay`), CAT-E (`_refreshAvailability` + back-off) — land in store order
  **B → A → D → E**. CAT-A populates the field CAT-B declared and adds the re-pop guard.
- **`MithrilProactivePromptContainer.tsx`** is also edited by CAT-B (display math → `computeBehindByEpochs`).
  Land **CAT-B before CAT-A**; CAT-A reorders the render so the gate short-circuits first and consumes
  CAT-B's `computeBehindByEpochs` for the near-tip hide.
- **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by safe degradation.** CAT-A
  owns the optional IPC-type field (so the read type-checks pre-CAT-H) and the store population + gate OR;
  CAT-H supplies the value. An absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only.
