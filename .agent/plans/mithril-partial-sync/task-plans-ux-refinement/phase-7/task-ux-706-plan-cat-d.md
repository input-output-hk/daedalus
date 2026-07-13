# task-ux-706 · CAT-D — At/past-snapshot guidance (variant split) + moving-stage caution (new copy)

Covers Extra items #2 (diagnostic tooltip when the node is at/past the latest snapshot), #3 (the same
message on the confirmation prompt that appears when the DD "Mithril Sync" button is clicked), and #4
(moving-stage caution). Implements **DD-706-6**. **EN strings are LOCKED (product sign-off 2026-07-09);
JA remains a first-pass draft for the translator's round-2 review.** See each step for the locked EN.

## What "at/past the latest snapshot" means (the intent behind Extra #2/#3)

The two extra items were logged loosely ("diagnostic tooltip when it will not find a snapshot / latest
snapshot is…", "same when clicking the DD Mithril button"). The concrete intent (DD-706-6): **when the
node's tip is at or past the latest certified snapshot** — backend `gap <= 0`, i.e. there are **no
epochs left to catch up** — the copy must tell the user they are **ahead of the latest snapshot**, that
they **don't need Mithril Sync**, and that **Blockchain Sync will complete the remaining block sync** on
its own. This is a reassurance/redirection, not a block — the button stays enabled (behavior unchanged).
*(Kept as the original intent record — the 2026-07-09 resolution below deliberately softened the
"don't need Mithril Sync" claim; the shipped copy is the LOCKED hedge in Step D1, and the acceptance
checklist matches the locked strings, not this paragraph.)*

## Why this is a variant split, not a reword (critical)

`getPartialSyncBehindness` (`MithrilPartialSyncService.ts:955-992`; gap branches `:969-981`) computes
`gap = latest - local` and:
- `gap <= 0` → returns `{ isSignificantlyBehind: false }` and **omits `behindByImmutables`** (the
  at/past-snapshot case).
- `0 < gap < threshold` (threshold ≈ 20 immutables) → `{ isSignificantlyBehind: false, behindByImmutables: gap }`
  (still a few chunks behind — Mithril Sync *can* still restore them).
- `gap >= threshold` → `isSignificantlyBehind: true`.

But the UI collapses **both** not-significantly-behind cases into a single `near-tip` variant:
`MithrilPartialSyncSection.tsx:130-135` sets `availabilityVariant = 'near-tip'` unless
`isSignificantlyBehind` (→ `behind`) or `isProbeFailed` (→ `availability-unknown`). The store
(`MithrilPartialSyncStore.ts:255-257`) only surfaces `isSignificantlyBehind` / `isProbeFailed` /
`certifiedEpoch` — it **drops** `behindByImmutables`, so the two states are indistinguishable downstream.

Rewording the one `near-tip` variant to "you're ahead of the latest snapshot" would be **false** for a
node 1–19 immutables behind (that node *is* behind and Mithril *would* help). So CAT-D must **split** the
variant on the `gap <= 0` signal and give the at/past-snapshot case its own copy — the existing near-tip
copy stays for `0 < gap < threshold`.

The proactive loading-screen prompt is **not** a surface here: it only renders when
`isSignificantlyBehind` (`MithrilProactivePromptContainer.tsx:33`), which is always false at `gap <= 0`.
The two surfaces are exactly the DD tooltip and the DD confirmation prompt (Extra #2 and #3).

## ✅ Resolved 2026-07-09 — hedge as a non-diagnostic offer (option a)

`gap <= 0` is **not** proof of a healthy node — it is the exact condition task-705 WI-3 targets. A node
with recent immutables but **missing/corrupted/incompatible ledger state** also sits at `gap <= 0`,
`isSignificantlyBehind` stays false, and it drops into a slow from-genesis **ledger replay** that
Mithril's ledger-only restore short-circuits. The behind-ness probe knows only the immutable gap, not
ledger health, so it **cannot** distinguish "healthy & ahead" from "stuck replaying" — both are `gap <= 0`.
This user can genuinely land on this copy: `SyncingConnectingPage.tsx:68` opens the DD dialog from the
replay screen via the status icon.

**Investigated and rejected — auto-detecting the stuck case (option c).** We checked whether the DD
surface could split the `gap <= 0` copy on an actual replay-in-progress signal instead of hedging. It
cannot. The only candidate, `blockSyncProgress[BlockSyncType.replayedBlock]` (the same signal the
loading-screen button uses), is **log-derived** (`handleCheckBlockReplayProgress.ts` tails `node.log` for
`Replayed block … Progress: X%`), starts at `0`, and only advances when the node emits replay lines *this
session*. A healthy node that loaded its ledger from a snapshot and never replayed emits none, so its
counter **stays at 0 → `< 100` reads true for healthy nodes too**; and per task-705 commit 5 a genuinely
stuck node also sits at `0` at the start of its replay. The value is ambiguous at both ends and cannot
drive copy. The DD-706-6 "indistinguishable" claim is therefore confirmed, not merely assumed.

**Decision (option a):** hedge, framed as a **non-diagnostic offer** — never assert "your node is stuck"
(false for a healthy fast-replay node) and drop the flat "you don't need Mithril Sync" (discouraging to
the stuck user). State the fact + the reassuring path, then offer Mithril as a remedy for slow/failing
sync. The button stays enabled on both surfaces regardless (behavior unchanged); the copy just no longer
talks a stuck user out of the fix. Rationale: the downside is asymmetric — the offer clause costs a
healthy user one harmless sentence, whereas an un-hedged claim tells the stuck user "you don't need the
fix" on the very page they opened to get it. The offer wording is honest for **both** node types, so no
false claim is made in either direction. Locked strings in Step D1 below.

## Step D1 — Thread the `gap <= 0` signal and add the `at-or-past-snapshot` variant

**Signal (backend → type → store → dialog → presentational page → section):**
1. `getPartialSyncBehindness` already knows `gap <= 0`. Surface it explicitly — add
   `isAtOrPastSnapshot: gap <= 0` to its return (preferred; robust to future refactors — confirmed the
   right call by plan-review: a `behindByImmutables`-absence fallback would misclassify probe-failed).
   Do **not** infer it from `behindByEpochs` absence (a significantly-behind node can also lack an
   epoch anchor).
2. Type (`source/common/types/mithril-partial-sync.types.ts:89-101`): add `isAtOrPastSnapshot?: boolean`
   to **`MithrilPartialSyncAvailability`** — the typed IPC payload the store consumes (plan-review
   addition; without it the field is dropped at the type seam and the variant never fires).
3. Store (`MithrilPartialSyncStore.ts:255-257`): add an observable for the signal (e.g.
   `@observable isAtOrPastSnapshot = false`), set from the behindness result. The default **must** be
   `false` deliberately: the disabled/working short-circuits (`MithrilController.ts:151-160`) return no
   flag at all.
4. Dialog container (`DaedalusDiagnosticsDialog.tsx:136-139`): passes the flags to the presentational
   **`DaedalusDiagnostics.tsx`** — the middle hop that actually renders the section (plan-review
   addition; the container does not render it directly). Declare the new prop alongside the existing
   mithril flags (`:411-414`), destructure it (`:525-528`), and pass it where
   `MithrilPartialSyncSection` renders (`:718-731`).
5. Section (`MithrilPartialSyncSection.tsx:130-135`): extend the variant ladder —
   `if (isSignificantlyBehind) 'behind'; else if (isProbeFailed) 'availability-unknown'; else if
   (isAtOrPastSnapshot) 'at-or-past-snapshot'; else 'near-tip'`. Pass the new variant to
   `MithrilPartialSyncRecommendation` and pass the same signal into `MithrilPartialSyncConfirmation`.

**Tooltip — new message `mithrilPartialSyncRecommendationAtOrPastSnapshot`** (Extra #2). Map it in
`MithrilPartialSyncRecommendation.tsx:64-69` for `variant === 'at-or-past-snapshot'`. LOCKED EN
(product sign-off 2026-07-09; offer-framed per the resolution above), JA still round-2 draft:
- EN: `Your node is at or past the latest Mithril snapshot. Blockchain Sync will finish the remaining blocks on its own. If sync seems slow or runs into verification issues, Mithril Sync can restore a verified ledger state.`
- JA (draft, round 2): `ノードは最新のMithrilスナップショットに到達しているか、それを超えています。残りのブロックはブロックチェーン同期が自動的に同期します。同期が遅い場合や検証に問題がある場合は、Mithril同期で検証済みの台帳状態を復元できます。`

**Confirmation body — new variant `atOrPastSnapshot`** (Extra #3). In `MithrilPartialSyncConfirmation.tsx`
the body currently chooses `behind` (epochs finite) vs `behindUnknown` (`:89-93`). At `gap <= 0` today it
renders the misleading `behindUnknown` = *"Your node is behind the latest verified snapshot."* —
confirmed by plan-review: a "behind by 0 epochs" cannot render, since `computeBehindByEpochs` returns
`undefined` for diff ≤ 0 (`mithrilBehindness.ts:38-39`) — the **wrong** claim for a node that is not
behind.
Add an `atOrPastSnapshot` message and select it when the new signal is set, **regardless of epochs**
(keep `behindUnknown` for the genuinely-behind-but-no-epoch case). LOCKED EN (2026-07-09) — same
offer framing as the tooltip, but the final clause describes the action since the user has already
clicked "Mithril Sync"; do **not** re-open a "you don't need this" argument on a dialog the user opened
on purpose. JA still round-2 draft:
- EN: `Your node is at or past the latest Mithril snapshot, so Blockchain Sync can finish the remaining blocks on its own. If sync seems slow or runs into verification issues, continuing will restore a verified ledger state at the snapshot position.`
- JA (draft, round 2): `ノードは最新のMithrilスナップショットに到達しているか、それを超えているため、残りのブロックはブロックチェーン同期が同期できます。同期が遅い場合や検証に問題がある場合は、続行するとスナップショット位置の検証済み台帳状態が復元されます。`

**Reconciliation with CAT-B B3 and the existing `near-tip` copy:** `recommendationNearTip` (line 164)
now stays scoped to `0 < gap < threshold` and keeps its "close to the blockchain tip, you can still use
Mithril Sync" meaning — apply CAT-B B3's 先端→最新ブロック edit there (it no longer collides with a CAT-D
rewrite of that line). Do **not** put the "ahead of the snapshot / don't need it" claim on
`recommendationNearTip`; that wording lives only on the new `atOrPastSnapshot` strings.

`mithrilPartialSyncRecommendationUnknown` (line 165, availability-unknown) — leave as-is unless product
wants the same softening; note it as an optional round-2 item.

Files: `MithrilPartialSyncService.ts` (surface `gap <= 0`),
`source/common/types/mithril-partial-sync.types.ts` (`MithrilPartialSyncAvailability`),
`MithrilPartialSyncStore.ts`, `DaedalusDiagnosticsDialog.tsx`, **`DaedalusDiagnostics.tsx`** (the
presentational middle hop — plan-review addition), `MithrilPartialSyncSection.tsx` (variant ladder +
prop threading), `MithrilPartialSyncRecommendation.tsx` (new tooltip variant),
`MithrilPartialSyncConfirmation.tsx` (new body variant), plus the three locale files. No change to the
`gap <= 0` backend logic itself — only the signal is newly surfaced.

## Step D2 — Moving-stage "don't close Daedalus" caution (Extra #4; locked shutdown voice — no "corrupt")

The file-move work is the `install-snapshot` sub-item ("Moving snapshot to storage",
`MithrilBootstrap.messages.ts:216-221`), synthesized/kept active during `finalizing`
(`MithrilStepIndicator.tsx:271-301`). Add a caution line attached to that stage on **both** the bootstrap
and partial-sync overlays (both pass through `finalizing`).

New message, e.g. `loading.mithrilBootstrap.progress.moveCaution`. **LOCKED EN (2026-07-09)** — matches
the existing shutdown copy's voice deliberately (see below); JA still round-2 draft:
- EN: `To preserve data integrity, please don't close Daedalus until this step is complete.`
- JA (draft, round 2): `データの完全性を保つために、この手順が完了するまでDaedalusを閉じないでください。`

**Voice decision (2026-07-09):** this caution **mirrors the existing "Stopping Cardano node" description**
(`SyncingConnectingStatus.tsx:31-36` / `loading.screen.stoppingCardanoDescription`: *"…To preserve data
integrity, please wait until this process is complete."*), which is Daedalus's established register for an
integrity-sensitive "don't interrupt" moment. We rejected the stronger "…can corrupt your chain data"
draft precisely because the shutdown screen **deliberately avoids the word "corrupt"** — matching its
"preserve data integrity" framing keeps the two integrity-sensitive screens consistent. The JA draft reuses
the existing JA rendering of that phrase (`データの完全性を保つために…`, from
`ja-JP.json:405`) so the translator sees a consistent first pass.

Placement: render it attached to the `install-snapshot` sub-item — note this is a **new DOM element +
SCSS class**, not a message swap: `renderSubItem` (`MithrilStepIndicator.tsx:560-591`) renders only
icon + label today (plan-review correction to the earlier "detail/subtext" wording). Alternatively
render it as the `finalizing` step detail while that sub-item is active (guarded by
`keepInstallingActiveDuringFinalizing`, `:271-301`). Prefer attaching to the sub-item so it appears
exactly during the move, and disappears once `finalizing` completes. Style it as a caution (reuse an
existing warning/subtext style; no new severe-error styling).

Add the message def to `MithrilBootstrap.messages.ts`, the three locale files, and reference it from the
sub-item render. Ensure it is spoken once, not per-tick.

## Acceptance

- At/past the latest snapshot (`gap <= 0`, no epochs to catch up), both the DD tooltip and the DD
  confirmation prompt render the LOCKED hedge copy: the node is at or past the latest snapshot,
  Blockchain Sync will finish the remaining blocks on its own, and Mithril Sync is offered as a remedy
  if sync is slow or hits verification issues. Per the 2026-07-09 resolution, **no** flat "you don't
  need Mithril Sync" sentence appears (plan-review corrected this bullet — the earlier "isn't needed"
  wording contradicted the locked strings). The button remains enabled (no behavior change).
- The confirmation prompt no longer shows "behind the latest verified snapshot" when `gap <= 0`; the
  discriminator is the `gap <= 0` signal, not epochs-absence, so a genuinely-behind node with no epoch
  anchor still reads "behind."
- The `0 < gap < threshold` near-tip copy is unchanged (still "you can still use Mithril Sync") and picks
  up CAT-B B3's 最新ブロック edit; the strong "ahead of snapshot" claim never renders there.
- During the move/finalize step, the LOCKED "To preserve data integrity, please don't close Daedalus until
  this step is complete." caution (shutdown-voice, no "corrupt") is visible on both overlays and clears when
  the step finishes.
- The stuck-replay hedge decision is **resolved: option (a) — hedge as a non-diagnostic offer**
  (decided 2026-07-09; option c auto-detect investigated and rejected — see the resolution section). The
  shipped EN wording matches the LOCKED strings above; neither surface asserts "your node is stuck" nor a
  flat "you don't need Mithril Sync."
- All new/changed strings flagged in the round-2 copy table for the JA translator; no placeholders here.
- i18n-messaging validation clean; scoped jest green with the corrected spec inventory (plan-review):
  variant→copy selection is tested in **`MithrilPartialSyncSection.spec.tsx:240,251`** (there is no
  `MithrilPartialSyncRecommendation.spec.tsx`), plus the confirmation and step-indicator specs.
  Adding `isAtOrPastSnapshot` to the probe's return breaks exact-shape `toEqual` assertions in
  `MithrilPartialSyncService.spec.ts:1565` and the behindness describe — `:2153` to end of file:
  in-range assertions `:2175/:2188/:2201/:2213/:2228` plus `:2364/:2377/:2391/:2404` past the
  plan-review's originally cited span — and the `MithrilPartialSyncStore.spec.ts` `_applyAvailability`
  coverage (`:656+`). Update them all. Per seam
  **S8**: apply CAT-F's top-level `beforeEach` `pathExists.mockResolvedValue(true)` fix **with or
  before** these spec additions (D lands first in A→G order), so CAT-F verifies rather than re-fixes.
  Storybook stories updated for the new caution/variant.
