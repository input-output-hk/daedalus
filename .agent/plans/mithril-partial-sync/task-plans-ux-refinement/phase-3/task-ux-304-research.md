# task-ux-304 — Research Note (durable findings)

Sprint: Mithril Partial Sync UX Refinement · phase-3 · task-ux-304
Branch: `feat/mithril-partial-sync-ux-refinement` · Planner pass: 2026-06-25T10:35:57Z

---

## 1. The D11 → D12 figure-source supersession (the central decision)

- **D11 (2026-06-24)** locked the *framing*: replace "N immutable files behind" with Cardano-native
  **epochs (primary sentence) + sync-% (separate trailing sentence)**, never show "immutable files",
  convey the **benefit vs waiting**, keep the gate backend-owned (D2). D11's *figure source* was a
  **backend** immutable→epochs conversion (`behindByEpochs` availability field + a per-network
  `filesPerEpoch` constant: mainnet/preprod ≈ 20, preview ≈ 4, unknown → 20).
- **D12 (2026-06-25)** supersedes **only the figure source**: the displayed figure is now the **exact
  node-tip epoch difference computed in the RENDERER** —
  `behindByEpochs = Math.max(1, networkTip.epoch − localTip.epoch)`, whole-epoch subtraction, floored at 1.
  D11's gate decision, the epochs-lead + sync-% framing, the never-show-immutable-files rule, and the
  benefit framing all **stand**. The backend `behindByEpochs` field and the per-network constant are
  **dropped** from this task's scope.
- **Why node-tip, not the conversion (D12 rationale):** (1) the data is already exact and already plumbed to
  both consumers (`DaedalusDiagnostics` holds `localTip`/`networkTip`; `SyncingConnectingPage` injects
  `networkStatus`), so no new backend field is needed; (2) no constant — the two integers are the same epoch
  numbers already shown in the "Last network block / Last synchronized block" rows, so the figure is visually
  consistent with what the user already sees; (3) it cannot undercut the offer — the network tip is
  at-or-ahead of the latest certified snapshot, so node-tip distance ≥ the certified-immutable gap; whenever
  the gate fires (gap ≥ ~1 epoch) the displayed epochs is ≥ that.
- **Whole-epoch over slot-fractional** (`networkTip.epoch − localTip.epoch` vs `round(Δabsolute-slot ÷
  epochLength)`) is an explicit user decision (2026-06-25) for visual consistency with the displayed tip
  rows; sub-epoch coarseness (±<1 epoch) is benign under the "about" hedge.

## 2. Verified live-code anchors (re-read 2026-06-25)

All confirmed against the working tree; drifts recorded.

- `DaedalusDiagnostics.tsx`
  - Props: `syncPercentage: number` `:403`; `localTip`/`networkTip: TipInfo | null | undefined` `:409-410`;
    `mithrilPartialSyncBehindByImmutables?: number` `:414` (display prop to drop).
  - render: destructures `syncPercentage` `:519`, `localTip`/`networkTip` `:523-524`; computes
    `formattedSyncPercentage = formattedNumber(syncPercentage, 2)` at **`:571`** (← insert `behindByEpochs`
    computation here).
  - Renders `<MithrilPartialSyncSection>` at **`:714-733`** (gate `:714`, element `:715-732`); passes
    `formattedSyncPercentage` `:716` (already) and `behindByImmutables={mithrilPartialSyncBehindByImmutables}`
    **`:724`** (← replace with `behindByEpochs`). **Anchor drift:** brief said ~716-724; live render block is
    714-733.
  - Tip rows guard `networkTip && networkTip.epoch` (`:738`) and `localTip && localTip.epoch` (`:761`) — the
    existing null/falsy-0 convention to mirror.
- `MithrilPartialSyncSection.tsx`
  - Props `:10-20`: `formattedSyncPercentage: string` **`:11`** (already received), `behindByImmutables?:
    number` **`:16`** (← replace with `behindByEpochs?: number`).
  - render `isShowingConfirmation` branch **`:116-126`** passes `behindByImmutables={this.props.behindByImmutables}`
    at **`:121`** (← replace with `behindByEpochs` + `formattedSyncPercentage`). **Anchor drift:** brief said
    118-124; live is 116-126.
- `MithrilPartialSyncConfirmation.tsx`
  - `defineMessages`: `behind` **`:27-33`** (immutable files), `behindUnknown` **`:34-40`** (fallback).
  - Props `{ isActionBlocked, startError, behindByImmutables?, onCancel, onConfirm }` **`:82-88`**.
  - render: `hasBehindFigure` guard `:105-107`; behind `<p>` **`:133-139`** (← add trailing sync-% `<p>`).
- `DaedalusDiagnosticsDialog.tsx` (container): injects `syncPercentage` `:128`, `localTip`/`networkTip`
  `:147-148`; passes `mithrilPartialSyncBehindByImmutables={mithrilPartialSync.behindByImmutables}`
  **`:138-140`** (← remove the display pass).
- i18n: EN `…ConfirmationBehind` `en-US.json:160`, `…BehindUnknown` `:161`; JA `…ConfirmationBehind`
  `ja-JP.json:160` (literally "約{count}個のimmutableファイル" — MUST be replaced), `…BehindUnknown` `:161`;
  `translations/messages.json` `…Behind` defaultMessage+id `:1866-1868`. Canonical id confirmed
  `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind`; fallback id
  `…mithrilPartialSyncConfirmationBehindUnknown`.

## 3. NetworkStatusStore tip/epoch field shape (confirmed)

- `TipInfo = { epoch: number; slot: number; absoluteSlotNumber: number }` (`api/network/types.ts:1-5`).
  **`epoch` is a non-nullable `number`.** The tip OBJECT is `TipInfo | null | undefined`
  (`DaedalusDiagnostics.tsx:409-410`).
- Flow: `NetworkStatusStore` → `DaedalusDiagnosticsDialog` (`networkStatus.networkTip` / `.localTip`,
  `:70-71`) → `DaedalusDiagnostics` props (`:409-410`). A missing tip presents as `null`/`undefined`.
- **Guard convention:** guard the tip OBJECT (`tip && Number.isFinite(tip.epoch)`), not the epoch number;
  `undefined` `behindByEpochs` when either tip is missing → modal renders the `behindUnknown` fallback (no
  fabricated number). This matches the existing `tip && tip.epoch` guard the Diagnostics tip rows already use.
- **No epoch field exists on the main process / availability read model** — corroborates D12's rationale that
  the figure must be renderer-derived. `epochLength` lives only in `NetworkStatusStore`, never in main.

## 4. Existing copy/tone references reused

- Locked benefit wording (#4) appears verbatim in `mithrilPartialSyncRecommendation` /
  `…WithProgress` (`en-US.json:171-172`): "restore verified chain data to help it catch up faster". The
  approved variant must preserve this.
- The sync-% trailing sentence reuses the existing `…WithProgress` phrasing for consistency:
  EN "Cardano node is currently {syncPercentage}% synced." / JA "Cardanoノードは現在 {syncPercentage}% 同期済みです。"
- JA "verified chain data" is established as "検証済みのチェーンデータ" (`ja-JP.json:171-172`); "epoch" renders as
  "エポック"; "blockchain tip" → "ブロックチェーンの先端". Use these for catalog consistency.

## 5. Live-vs-doc conflicts / reconciliations

- **D11 vs D12 (figure source):** the PRD already records D11 as "partially superseded by D12" in-line
  (`…-prd.md:531-536`), and the tasks JSON `task-ux-304` already encodes the D12 (renderer node-tip) plan.
  **No conflict** — D12 is authoritative for the figure source; D11 governs the framing. The earlier D11
  backend `behindByEpochs` field + `filesPerEpoch` constant are explicitly out of scope.
- **task-ux-303 NOTE** (`task-ux-303.md:353`, and the live `behind` message description) already flags that
  the immutable-files behind copy is "REVISED by task-ux-304". The 303 modal STRUCTURE is unchanged; 304
  swaps only the figure prop + copy. No conflict.
- **`behindByImmutables` on the store/availability model:** D12 says leaving it as an internal/debug field OR
  removing it is non-blocking cleanup. **Recommendation: LEAVE it.** The gate (`isSignificantlyBehind`) and
  its types still reference the immutable gap backend-side; removing the renderer-facing observable would
  churn the store/types for no user-facing benefit and risk the gate. Flag removal (if desired) to the
  task-ux-501 hygiene pass, not this task.

## 6. Residual gaps / follow-ups

- **task-ux-302** (proactive prompt, pending) reuses this canonical figure (renderer node-tip difference,
  computed in `SyncingConnectingPage` from the injected `networkStatus`) and copy. The sync-% sentence is
  optional in the space-constrained prompt (PRD UX Flow 1). 302 depends on 304.
- **task-ux-601** (holistic EN+JA copy pass) may optionally adopt the epochs lead in the diagnostics
  recommendation teaser (`mithrilPartialSyncRecommendation*`); NOT required here (D11 `:577-581`).
- **task-ux-501** (i18n hygiene) explicitly must NOT re-touch the behind-ness keys (those are 304-owned);
  may later remove `behindByImmutables` from the store if the team decides to.
- **Plural "epochs":** the approved copy uses "{epochs} epochs" unconditionally (no ICU plural) per #34
  "keep copy simple"; floored at 1 so "1 epochs" is possible — benign under the "about" hedge, consistent
  with the 303 "files" precedent. Flag only if QA objects.

## 7. Open decision pending user (interactive_decision)

The final EN+JA behind-ness sentences (benefit-vs-waiting framing) — 3 variants proposed in
`task-ux-304-plan-review.md`, **variant-a recommended**. The implementer pastes the approved variant into the
FINAL COPY block in `task-ux-304.md` before editing any catalog.

## 8. Finalization confirmation (completed 2026-06-25)

Sections 1–6 are confirmed accurate against the shipped implementation (verified anchors, the
`TipInfo.epoch`-is-non-nullable-`number` shape, the D11→D12 figure-source supersession, the
`behindByImmutables`-left-on-store recommendation). The two findings below resolve/correct earlier-pending
items; recorded here as durable (not duplicated above):

- **i18n-target correction (durable).** The runtime/extract targets are exactly `en-US.json`, `ja-JP.json`,
  and the regenerated `translations/messages.json` (via `yarn i18n:extract`, whose `--out-file` is
  `translations/messages.json` per `package.json`). **Do NOT edit
  `source/renderer/app/i18n/locales/defaultMessages.json`** — verified it is a separate/stale generated
  snapshot, is not the `i18n:extract` output target, is not imported at runtime, and does not even contain the
  `…ConfirmationBehind` / `…ConfirmationBehindUnknown` / new `…ConfirmationBehindSyncContext` ids. Confirmed
  during implementation: `defaultMessages.json` was untouched (absent from `git status --porcelain`).

- **User's compact-parenthetical sync-% decision (2026-06-25) — supersedes the "separate trailing sentence"
  framing in §1/§7.** The `interactive_decision` (§7) is CLOSED: the user picked variant A primary and
  directed the sync-% to be a **compact parenthetical reference line below the primary**, NOT a full trailing
  sentence. Shipped: `behindSyncContext` (EN `({syncPercentage}% synced)`, JA `（{syncPercentage}% 同期済み）`),
  id `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehindSyncContext`, rendered as its own
  de-emphasized `<p>` directly below the primary behind-ness line and **always** (it depends only on
  `formattedSyncPercentage`, independent of whether `behindByEpochs` is known). Where §1/§4 say "sync-%
  (separate trailing sentence)", read "compact parenthetical reference line below the primary" — the
  never-interpolated-mid-sentence rule and the epochs-primary framing are unchanged.

No other new research. Outcome shipped 2026-06-25; code review approved.
