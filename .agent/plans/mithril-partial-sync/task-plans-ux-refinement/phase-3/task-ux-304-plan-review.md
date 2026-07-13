# task-ux-304 — Plan Review Log (append-only)

Each entry: speaker label, ISO-8601 UTC `Timestamp:`, outcome. Critiquer/Code-Review entries end with
`Decision: approved` or `Decision: requires_changes`. Append at end-of-file only; never rewrite history.

---

## Planner:
Timestamp: 2026-06-25T10:35:57Z

### What I planned
Wrote the canonical `task-ux-304.md` to the small-model-implementable bar. The task revises ONLY the
user-facing behind-ness figure + copy in the partial-sync confirmation modal (303 owns the modal structure):

- **Figure source (D12 supersedes D11):** compute `behindByEpochs = Math.max(1, networkTip.epoch −
  localTip.epoch)` in the RENDERER inside `DaedalusDiagnostics.render()` (next to `formattedSyncPercentage`,
  `:571`), whole-epoch subtraction, floored at 1, `undefined` when a tip/epoch is missing. NO backend field,
  NO per-network `filesPerEpoch` constant, NO change to `getPartialSyncBehindness` or the gate (D2 reaffirmed).
- **Props:** drop the `behindByImmutables` DISPLAY prop chain; add `behindByEpochs?: number` +
  `formattedSyncPercentage: string` to `MithrilPartialSyncConfirmation`; thread `behindByEpochs` through
  `MithrilPartialSyncSection` (which already receives `formattedSyncPercentage`); compute + pass from
  `DaedalusDiagnostics`; drop the container's `mithrilPartialSyncBehindByImmutables` pass.
- **Copy:** epochs PRIMARY sentence (`behind`, `{epochs}`) + a NEW `behindSyncContext` SEPARATE trailing
  sentence (`{syncPercentage}`); `behindUnknown` fallback retained. Canonical id
  `daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind` (unchanged) + new
  `…mithrilPartialSyncConfirmationBehindSyncContext`. All four catalogs updated (incl. the JA
  "約{count}個のimmutableファイル" → epochs+sync-%); no `!!!` leakage.

Verified every live seam (table in the canonical doc). Anchor drifts recorded: the `MithrilPartialSyncSection`
render in `DaedalusDiagnostics` is `:714-733` (brief said ~716-724); the confirmation render in the Section is
`:116-126` (brief said 118-124); the `formattedSyncPercentage` compute + insertion point is `:571`. Confirmed
`TipInfo.epoch` is a non-nullable `number` while the tip object is `| null | undefined`, so the guard is on the
tip object (matching the existing `tip && tip.epoch` convention in the Diagnostics tip rows). Confirmed there
is no epoch field on the main process / availability read model — corroborating D12's renderer-derived figure.

Left the build-status `in_progress` and planning-status `in_review` per the brief; the FINAL COPY block in the
canonical doc is the single gate the implementer fills after user sign-off.

### Proposed EN+JA copy variants (epochs lead + separate sync-% trailing sentence + benefit-vs-waiting)
All keep the locked "verified … chain data … catch up faster" wording (#4), keep copy simple (#34), never say
"immutable files", and put sync-% in its own trailing sentence. JA reuses the established catalog phrasing
("検証済みのチェーンデータ", "Cardanoノードは現在 {syncPercentage}% 同期済みです。", "より速く追いつけるように") for consistency with
`mithrilPartialSyncRecommendation*` (`ja-JP.json:171-172`).

**variant-a — "behind + benefit" (RECOMMENDED).** Plain, states the gap then the time benefit; tightest fit
with the existing recommendation tone.
- behind (EN): `Your node is about {epochs} epochs behind the blockchain tip. Mithril partial sync can restore verified chain data to help it catch up faster than waiting for normal sync.`
- behindSyncContext (EN): `Cardano node is currently {syncPercentage}% synced.`
- behind (JA): `ノードはブロックチェーンの先端より約{epochs}エポック遅れています。Mithril partial syncで検証済みのチェーンデータを復元すれば、通常の同期を待つよりも速く追いつけます。`
- behindSyncContext (JA): `Cardanoノードは現在 {syncPercentage}% 同期済みです。`
- rationale: Calm/factual; gap-then-benefit ordering makes the value-vs-waiting explicit; reuses locked wording.

**variant-b — "two short sentences, benefit-forward."** Splits the epochs primary into two crisp sentences so
the benefit reads as its own beat; slightly more persuasive tone.
- behind (EN): `Your node is about {epochs} epochs behind the blockchain tip. Restoring verified chain data with Mithril partial sync is faster than waiting for normal sync to catch up.`
- behindSyncContext (EN): `Cardano node is currently {syncPercentage}% synced.`
- behind (JA): `ノードはブロックチェーンの先端より約{epochs}エポック遅れています。Mithril partial syncで検証済みのチェーンデータを復元する方が、通常の同期で追いつくのを待つより速く済みます。`
- behindSyncContext (JA): `Cardanoノードは現在 {syncPercentage}% 同期済みです。`
- rationale: Benefit-forward; "faster than waiting" stated as the headline value; preserves verified-data lock.

**variant-c — "minimal primary + benefit in trailing block."** Keeps the epochs sentence very short; folds the
benefit into the recovery-note region rather than the behind line (leans on the existing modal recovery copy).
- behind (EN): `Your node is about {epochs} epochs behind the blockchain tip.`
- behindSyncContext (EN): `Cardano node is currently {syncPercentage}% synced. Mithril partial sync restores verified chain data to help it catch up faster than waiting for normal sync.`
- behind (JA): `ノードはブロックチェーンの先端より約{epochs}エポック遅れています。`
- behindSyncContext (JA): `Cardanoノードは現在 {syncPercentage}% 同期済みです。Mithril partial syncは検証済みのチェーンデータを復元し、通常の同期を待つより速く追いつけるようにします。`
- rationale: Cleanest epochs primary; benefit rides with the sync-% sentence. NOTE: this puts the sync-% first
  in the trailing block then the benefit — still its own sentence, but the percentage and benefit share the
  trailing `<p>`. Offered for the "shortest primary" preference; a-/b- keep the benefit attached to the gap.

**Recommendation:** **variant-a**. It leads with the concrete epochs gap, states the benefit-vs-waiting in the
same primary block (so the figure's *reason for existing* is immediate), keeps the locked "restore verified
chain data … catch up faster" wording verbatim, and keeps the sync-% as a clean standalone trailing sentence.

### One user-blocking question
Which copy variant (a / b / c) should ship for the partial-sync confirmation behind-ness line — and is the
benefit-vs-waiting framing in the chosen variant worded the way you want for both EN and JA? (variant-a
recommended: "Your node is about {epochs} epochs behind the blockchain tip. Mithril partial sync can restore
verified chain data to help it catch up faster than waiting for normal sync." + trailing "Cardano node is
currently {syncPercentage}% synced.")

Outcome: plan written; awaiting user copy sign-off (interactive_decision stop condition). No build started.

---

## Critiquer:
Timestamp: 2026-06-25T10:43:23Z

One broad pass over `task-ux-304.md`, the tasks-JSON entry, PRD D11/D12, the live seams, and the four i18n
artifacts. The plan implements **D11-as-amended-by-D12 correctly** on the substance: renderer whole-epoch
node-tip subtraction `Math.max(1, networkTip.epoch − localTip.epoch)` in `DaedalusDiagnostics.render()`
adjacent to `formattedSyncPercentage` (`:571`); **no** backend `behindByEpochs` field, **no** `filesPerEpoch`
constant, **no** change to `getPartialSyncBehindness` or the `isSignificantlyBehind`/`behindByImmutables`
gate (D2 reaffirmed); epochs as the primary sentence + sync-% as its **own** trailing `behindSyncContext`
sentence; `behindUnknown` fallback retained; no renderer threshold; figure floored at 1 so it can't undercut
the offer. Closes gap #29 (copy half) and the surfaced half of #26. **No new IPC field and no
api-endpoints.md change** — figure is renderer-derived (correct). All proposed EN+JA variants (a/b/c) are
**compliant** (epochs lead, sync-% as its own trailing sentence, locked "verified … chain data … catch up
faster" #4 preserved, never "immutable files", simple #34, benefit-vs-waiting framing). The
`interactive_decision` copy stop is correctly surfaced as the single user gate (FINAL COPY block), with
concrete variants ready. No immutable source files touched in scope.

### Live-seam verification (all re-checked against the working tree)
- VERIFIED: `MithrilPartialSyncConfirmation.tsx` `behind` msg `:27-33`, `behindUnknown` `:34-40`, Props
  `:82-88` (`behindByImmutables?`), `hasBehindFigure` `:105-107`, behind `<p>` `:133-139`. Swap is mechanical.
- VERIFIED: `MithrilPartialSyncSection.tsx` `formattedSyncPercentage` Prop `:11`, `behindByImmutables?` `:16`,
  `<MithrilPartialSyncConfirmation … behindByImmutables>` `:118-124` (anchor `:116-126` in doc — accurate).
- VERIFIED: `DaedalusDiagnostics.tsx` Props `:403/:409-410/:414`, render destructure `:519/:523-524/:528`,
  `formattedSyncPercentage` `:571`, `<MithrilPartialSyncSection … behindByImmutables={…}>` `:714-733`
  (`behindByImmutables` at `:724`), tip-row epoch guards `:738/:761`. All match.
- VERIFIED: `DaedalusDiagnosticsDialog.tsx` `mithrilPartialSyncBehindByImmutables` pass `:138-140`,
  `localTip`/`networkTip` `:147-148`, `syncPercentage` `:128`.
- VERIFIED: `TipInfo = { epoch: number; slot; absoluteSlotNumber }` (`api/network/types.ts:1-5`) — `epoch`
  is non-nullable; tip object is `| null | undefined`. Guard-the-tip convention is correct.
- VERIFIED: gate lives at `source/main/mithril/MithrilPartialSyncService.ts` (`getPartialSyncBehindness` at
  `:677`, file is 1022 lines). The doc only ever writes the bare filename `MithrilPartialSyncService.ts:677-706`
  (never a wrong `cardano/` dir), so the non-goal reference is functionally fine — but it omits the
  `source/main/mithril/` directory; recommend adding the full path so a reader doesn't grep `cardano/`.
- VERIFIED: the three spec files exist; `MithrilPartialSyncConfirmation.spec.tsx:53-58` asserts exactly the
  `behindByImmutables: 42` → "42 immutable files" line the plan swaps. Test plan is accurate & mechanical.

### Blockers (require changes)

1. **`defaultMessages.json` is the WRONG i18n target and does NOT contain these keys — remove it from scope.**
   `package.json:52` defines `i18n:extract` as `formatjs extract … --out-file='translations/messages.json'`.
   The single extraction artifact is **`translations/messages.json`** (the `behind` entry is there at
   `:1864-1868`, `behindUnknown` description "immutable-file gap" at `:1872`). A programmatic scan of
   `source/renderer/app/i18n/locales/defaultMessages.json` finds **0** `mithrilPartialSync*` keys — the
   confirmation keys are NOT in that file. Yet the plan lists `defaultMessages.json` as an in-scope catalog
   to update in **three** places: Scope item 4 ("update … in `defaultMessages.json`, `en-US.json`, …"), DD4
   ("All three present in `defaultMessages.json` (auto), …"), and the Commit stage-list (`:493`,
   "`defaultMessages.json`, `en-US.json`, …"). A small-model implementer following the doc literally would
   hunt for an absent key or hand-add the new `behindSyncContext` entry to the wrong file. **Fix:** drop
   `defaultMessages.json` from scope item 4, DD4, Step 6, and the commit stage-list; the catalogs to edit are
   `en-US.json`, `ja-JP.json`, and (regenerated by `yarn i18n:extract`) `translations/messages.json` only.
   If `defaultMessages.json` is genuinely a separate hand-maintained artifact, the plan must say what it is
   and where these keys would live in it — but evidence says it is not the target for these ids.

### Non-blocking corrections / nits (fix opportunistically)

2. **Live-seam row `:157` mischaracterizes the reuse string.** It claims `en-US.json:172` / `ja-JP.json:172`
   is the standalone string "Cardano node is currently {syncPercentage}% synced." In fact line 172 is
   `mithrilPartialSyncRecommendationWithProgress`, whose value is "Cardano node is currently {syncPercentage}%
   synced. If catch-up is taking longer than you want, …" — the target phrase is only the FIRST sentence of a
   longer message, not a standalone catalog key. The proposed `behindSyncContext` copy reuses the phrasing
   correctly (new key), so this is doc-accuracy only; reword the row to "first sentence of
   `…RecommendationWithProgress`" rather than "exact phrasing / reuse this exact string."

3. **State the gate-service full path.** Add `source/main/mithril/MithrilPartialSyncService.ts` (the bare
   filename appears twice; the directory is never given). Low-severity since it's a do-not-touch non-goal.

4. **DD4's `behindUnknown` description update is warranted** (confirmed `translations/messages.json:1872`
   reads "immutable-file gap"). Keep that instruction; just ensure it's applied in `translations/messages.json`
   (regenerated) rather than `defaultMessages.json`.

### Coverage / consistency confirmations (no action needed)
- Locked invariants 1-7 are each stated inline and the change honors all of them; the `{count}`→`{epochs}`
  rename is correctly flagged as needing consistent application at the `defineMessages` call site + catalogs.
- Stories correctly deferred to task-ux-502; backend gate/threshold correctly untouched and asserted-by-absence.
- The "leave `behindByImmutables` on the store as internal/debug" recommendation matches D12's non-blocking note.

**Small-model-implementable:** YES once blocker #1 is fixed — every prop/key/anchor is concrete and verified;
the only human input is the user-pinned FINAL COPY block (by design).

Decision: requires_changes

---

## Planner:
Timestamp: 2026-06-25T10:46:42Z

One fix pass applied to `task-ux-304.md` in response to the critique. Re-verified every claim against the live
working tree before editing (prefer-live-over-docs), and the central evidence both confirmed and *refined* the
blocker.

### Blocker #1 (i18n scope — `defaultMessages.json` is the wrong target) — RESOLVED

What I verified live (and a correction to the critique's own evidence):
- `package.json:52` `i18n:extract` = `formatjs extract … --out-file='translations/messages.json'`. The single
  extraction artifact for these ids is **`translations/messages.json`** (the `behind` entry is at `:1866-1868`,
  `behindUnknown` description "immutable-file gap" at `:1872`). Confirmed.
- The critique stated "a programmatic scan of `defaultMessages.json` finds ZERO `mithrilPartialSync*` keys."
  That is **factually inaccurate** — `defaultMessages.json` contains **25** `mithrilPartialSync*` keys,
  including **six** `…Confirmation*` ids (`Title`, `Intro`, `Success`, `Recovery`, `Cancel`, `Confirm` at
  `:1877-1904`). BUT the critique's *conclusion is correct*: `defaultMessages.json` does **not** contain the two
  ids this task touches (`…ConfirmationBehind` / `…ConfirmationBehindUnknown` are absent), it is **not** the
  `i18n:extract` `--out-file` target, and it is **not** imported at runtime (no `source/**` file imports
  `defaultMessages`). It is a separate/stale generated snapshot in the same `descriptors`/`id`/`path` format as
  `translations/messages.json`. So hand-editing it is both wrong and unnecessary. I recorded the accurate
  evidence in the doc rather than repeating the "zero keys" claim.

Edits (all four flagged locations + safety reinforcement):
1. **Scope item 4** — retitled to "the runtime catalogs + the extraction artifact"; the edit set is now
   `en-US.json`, `ja-JP.json`, and (regenerated) `translations/messages.json`. Added a blockquote: do NOT
   hand-edit `defaultMessages.json`, with the `package.json:52` evidence, the not-imported-at-runtime fact, and
   the absent-ids fact (verified 2026-06-25).
2. **DD4** — replaced "All three present in `defaultMessages.json` (auto), …" with the runtime-catalogs +
   `translations/messages.json` set and an explicit "Do NOT touch `defaultMessages.json`." Also added the
   concrete `behindUnknown` `description` rewrite (see blocker #4 below) here.
3. **Step 6** — added a leading "edit ONLY en-US.json / ja-JP.json / regenerated translations/messages.json; do
   NOT touch defaultMessages.json" note. (Step 6a–6d already only edited those three; reinforced 6a's wording so
   the `description` changes are described as coming from the `defineMessages` edits and written to
   `translations/messages.json` per `package.json:52`.)
4. **Commit stage-list** — removed `defaultMessages.json` from "Stage ONLY"; added "(Do not stage
   `defaultMessages.json` — it is not edited)".

Net: zero remaining instructions to edit `defaultMessages.json`; the four remaining mentions are all explicit
do-NOT-edit / do-NOT-stage guards.

### Non-blocking nits #2–#4 (critique-flagged) — also fixed in this pass

- **#2 reuse-string mischaracterization (live-seam row :157).** Verified `en-US.json:172` / `ja-JP.json:172` is
  `mithrilPartialSyncRecommendationWithProgress` ("Cardano node is currently {syncPercentage}% synced. If
  catch-up is taking longer…"), i.e. the target phrase is the FIRST sentence of a longer message, not a
  standalone key. Reworded the row to say so and to instruct reusing only the first-sentence *phrasing* in the
  new `behindSyncContext` value.
- **#3 gate-service full path.** Replaced both bare `MithrilPartialSyncService.ts:677-706` references (Why-now
  and Non-goals) with `source/main/mithril/MithrilPartialSyncService.ts:677-706` so no reader greps the wrong
  `cardano/` dir. (Confirmed both occurrences updated; no bare references remain.)
- **#4 `behindUnknown` description.** Confirmed the live `defineMessages` description
  (`MithrilPartialSyncConfirmation.tsx:38-39`) and `translations/messages.json:1872` both read "…when the
  immutable-file gap figure is unavailable." Made Step 1c a definite `description`-only edit ("…when the
  epochs-behind figure is unavailable (tips/epoch missing)") and noted in DD4 that the change is made at the
  `defineMessages` call site and propagates to `translations/messages.json` on re-extract; runtime EN/JA values
  stay verbatim.

### Not changed (and why)
- No copy/variant edits: all three EN+JA variants remain compliant per both the critique and the copy review;
  the FINAL COPY block remains the single `interactive_decision` user gate.
- No PRD/research-note edits: no finding *changed* the domain model — the `defaultMessages.json` finding is a
  build-pipeline scope correction local to this plan, not a PRD/research fact. (The `task-ux-304-research.md`
  durable-findings doc is authored at implementation time per the plan's "Required doc / research updates"
  section; the corrected i18n-target fact belongs there and is already captured in the plan's Scope/DD4 for the
  implementer to carry over.)
- No user-blocking decision was required by any blocker; all were resolvable from live evidence.

Outcome: blocker #1 resolved with corrected evidence; nits #2–#4 resolved; plan remains
small-model-implementable with the user-pinned FINAL COPY block as the only human input. Awaiting user copy
sign-off (interactive_decision stop condition).
