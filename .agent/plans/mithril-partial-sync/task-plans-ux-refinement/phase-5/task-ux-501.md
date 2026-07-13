# task-ux-501 — i18n and label hygiene, dead-copy cleanup, and doc re-baseline

## Task id + title
`task-ux-501` — "i18n and label hygiene, dead-copy cleanup, and doc re-baseline".

## Why now
Lowest-ID unblocked pending task (dep `task-ux-303` is completed); blocks `task-ux-601`
(holistic copy pass), which needs final, non-stale strings to review. Implements PRD **D8**
gaps **#10, #15-doc, #20, #21, #24-doc, #25, #27**.

## Interaction mode
`autonomous` — no user-blocking decision. Copy is hygiene-level (correcting a stale description,
localizing one already-visible English label, and clarifying one sentence's *placement* meaning);
the user-facing wording of the new label is unchanged ("Mithril Partial Sync").

## Scope
Hygiene + docs only:
- Correct one stale i18n **description** (not user copy).
- Localize the one hardcoded diagnostics **row label**.
- Confirm the orphaned key is already gone.
- Reword the confirmation **recovery** sentence so it is clear the options appear **in the
  progress/error overlay if the attempt fails**, not at confirmation time.
- Document (doc-only) the kept `_isTornDown` teardown guard (#15) and `retry`=start reuse (#24).
- Re-baseline 300-series doc references to the live 3-component structure.
- Add a partial-sync pointer section to the historical UX plan.

## Non-goals / scope guard
- **No** new IPC channels, types, or backend logic.
- **No** full copy audit — that is `task-ux-601`. Stay on #10, #15-doc, #20, #21, #24-doc, #25, #27.
- **Do NOT** touch the behind-ness copy keys `mithrilPartialSyncConfirmationBehind` /
  `...BehindUnknown` / `...BehindSyncContext` — owned by `task-ux-304` (D11/D12).
- **Do NOT** ship `!!!` placeholders into `en-US.json` / `ja-JP.json`.
- **Do NOT** edit the original `task-plans/` tree (orchestrator prompt hard rule) — see anchor G.

## Locked invariants this change must not break (inline)
- Confirmation precedes start; the confirmation modal is the only start path.
- Recovery actions render strictly from `allowedRecoveryActions` — never inferred from status names.
  (The reworded recovery copy must therefore say "the available options", not promise all three.)
- No renderer-computed behind-ness threshold; no synthetic throughput/remaining-time/overall-%.
- Staged-only restore. Bootstrap (empty-DB) flow must not regress (this task touches no bootstrap code).

## Dependencies
`task-ux-303` (completed).

## Research / docs / workflows / skills consulted
- PRD: `mithril-partial-sync-ux-refinement-prd.md` D8 (lines ~410–425), #25 (lines 410–411),
  #10 doc re-baseline (line 422).
- Research-19: `research/19-ux-refinement-state-and-gaps.md` lines 79–89 (stale/placeholder cleanup),
  88–89 (300-series plans stale vs the refactor).
- Tasks JSON entry `task-ux-501` (lines 592–628).
- Workflows: `.agent/workflows/frontend.md`, `.agent/workflows/update-doc.md`; skill `i18n-messaging`.
- Memory: diagnostics keys keep the **English** "partial sync" term for now; `task-ux-601` owns the
  single canonical ja-JP rendering. So new/updated JA here mirrors existing diagnostics JA tone.

---

## Verified live state (re-verify before editing — line numbers may drift)

| Anchor | File:line | Live fact |
|---|---|---|
| A | `i18n/locales/defaultMessages.json:1927` | `buttonLabel` description = `"Disabled placeholder CTA label for Mithril partial sync"` — **stale** (button is functional). |
| A' | `MithrilPartialSyncRecommendation.tsx:25` | same stale description in source `defineMessages`. |
| B | `MithrilPartialSyncSection.tsx:136` | renders literal `Mithril Partial Sync` then `globalMessages.punctuationColon`. |
| C | `MithrilPartialSyncRecommendation.tsx:7–40` | `defineMessages` pattern to follow. |
| F | `MithrilPartialSyncConfirmation.tsx:68–74` (recovery), runtime `en-US.json:165`, `ja-JP.json:165`, `defaultMessages.json:~1896` | recovery copy present; reword. |
| Orphan | grep `mithrilPartialSyncButtonHint` (bare) | **No matches** — already removed. Only `...ButtonHintBlocked` / `...ButtonHintReady` exist (those are live + correct). |
| Hint descs | `MithrilPartialSyncRecommendation.tsx:30–38` | descriptions say "disabled … button" / "before confirmation opens" — **accurate, leave as-is**. Only `buttonLabel` is stale. |

`translations/messages.json` and `defaultMessages.json` are byte-identical except a trailing newline
(both are formatjs-extract output). `yarn i18n:extract` rewrites `translations/messages.json` from the
source `defineMessages`; mirror the same descriptor edits into `defaultMessages.json` so the two stay
in sync. `en-US.json` / `ja-JP.json` are managed by `yarn i18n:check`
(react-intl-translations-manager) and are alphabetically sorted; add/update runtime strings in
alpha order.

---

## Implementation approach (ordered, mechanical)

### Step 1 — Fix stale `buttonLabel` description (#20) — `MithrilPartialSyncRecommendation.tsx`
At the `buttonLabel` descriptor (~line 25), change:
```
description: 'Disabled placeholder CTA label for Mithril partial sync',
```
to:
```
description: 'CTA label that opens the Mithril partial sync confirmation from diagnostics',
```
Do **not** change its `id` or `defaultMessage`.

### Step 2 — Localize the hardcoded row label (#21) — `MithrilPartialSyncSection.tsx`
2a. Add `defineMessages` to the existing import:
```
import { defineMessages, intlShape } from 'react-intl';
```
2b. Add a `messages` const (after the imports, before `type Props`), following the
`Recommendation.tsx` pattern:
```
const messages = defineMessages({
  sectionLabel: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel',
    defaultMessage: '!!!Mithril Partial Sync',
    description:
      'Row label for the Mithril partial sync section in the diagnostics dialog',
  },
});
```
2c. Replace the hardcoded label (line ~136). Change:
```
          Mithril Partial Sync
          {intl.formatMessage(globalMessages.punctuationColon)}
```
to:
```
          {intl.formatMessage(messages.sectionLabel)}
          {intl.formatMessage(globalMessages.punctuationColon)}
```
`intl` is already destructured from `this.context` at line ~114. Keep `globalMessages` import (still
used for `punctuationColon`).

**New key (exact):**
- id: `daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel`
- EN runtime (`en-US.json`): `"Mithril Partial Sync"`
- JA runtime (`ja-JP.json`): `"Mithril Partial Sync"`  (diagnostics keys keep the English term per
  `task-ux-601` ownership; this mirrors the existing `mithrilPartialSyncButtonLabel` JA value).

### Step 3 — Reword the recovery hand-off copy (#25) — `MithrilPartialSyncConfirmation.tsx`
At the `recovery` descriptor (~lines 70–71), change `defaultMessage` to make the **placement**
explicit (options live in the progress/error overlay, only on failure) while honoring
`allowedRecoveryActions` (say "available options", do not promise all three always):

**EN (`defaultMessage`, keep the `!!!` prefix in source):**
```
'!!!You don\'t need to choose now. If the sync fails, the progress screen will then offer the available recovery options — such as retrying, restarting normally on your current data, or wiping chain data and running a full Mithril sync.'
```
Leave its `id`, `description`, and the JSX usage unchanged. ("You don't *need* to choose now."
is the grammatical EN form and matches the JA "ここで選択する必要はありません" =
"you don't *need* to choose here".)

### Step 4 — Regenerate + mirror i18n catalogs
4a. Run `yarn i18n:extract` → rewrites **only** `translations/messages.json` (picks up the Step 1
description fix, the new `sectionLabel` descriptor, and the Step 3 recovery `defaultMessage`).
`yarn i18n:extract` does **not** regenerate `defaultMessages.json`, so the manual mirror in Step 4b
below is required (not optional).
4b. Mirror the same three edits into `source/renderer/app/i18n/locales/defaultMessages.json` (Edit the
matching descriptor blocks so only those lines change; preserve the file's no-trailing-newline):
   - `buttonLabel` description → the Step 1 text.
   - add the new `mithrilPartialSyncSectionLabel` descriptor block (place it next to the other
     `mithrilPartialSync*` descriptors).
   - `mithrilPartialSyncConfirmationRecovery` `defaultMessage` → the Step 3 EN text.
4c. Update runtime catalogs (drop the `!!!`, keep alpha order):
   - `en-US.json`: add `"daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel": "Mithril Partial Sync",`
     (between `...mithrilPartialSyncRecommendationWithProgress` and `...mithrilPartialSyncStart*` per
     alpha order; if no neighbor, place adjacent to the other diagnostics keys). Update
     `...mithrilPartialSyncConfirmationRecovery` value to:
     `"You don't need to choose now. If the sync fails, the progress screen will then offer the available recovery options — such as retrying, restarting normally on your current data, or wiping chain data and running a full Mithril sync."`
   - `ja-JP.json`: add `"daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel": "Mithril Partial Sync",`
     in alpha order. Update `...mithrilPartialSyncConfirmationRecovery` value to:
     `"ここで選択する必要はありません。同期に失敗した場合は、進行状況画面で利用可能な復旧オプション（再試行、現在のデータでの通常再開、またはチェーンデータを消去して完全なMithril syncを実行など）が表示されます。"`
4d. Run `yarn i18n:check` (or `yarn i18n:manage`) — must report **no missing** keys and **no `!!!`**
   placeholders for the new key. Confirm the bare `mithrilPartialSyncButtonHint` key is absent.

### Step 5 — DOC ONLY: `_isTornDown` teardown guard (#15) — NO code change
**No code change.** Document in this doc + research note that the guard is intentional and benign:
- `MithrilPartialSyncStore.ts:72` field `_isTornDown`; `:90` set `true` in `teardown()`.
- Early-return guards at `:140–142` (`syncStatus`), `:150–152` (`_updateStatus`), `:196–198`
  (`_refreshAvailability`), `:212–214` (`_applyAvailability`).
- Rationale: the store has no IPC-abstraction `unsubscribe`, so the `onReceive` listener is retained
  after teardown; `_isTornDown` makes any late frame a no-op (no MobX writes on a dead store). The
  retained listener is benign. **Keep as-is.**

### Step 6 — DOC ONLY: `retry` = start reuse (#24) — NO code change
**No code change.** The comment + wiring already exist:
- `MithrilPartialSyncStore.ts:238–241` comment: "`retry` reuses this start path — there is no
  dedicated retry IPC channel (PRD D8 / gap #24). onRetry in the overlay wires straight to
  startPartialSync." Preserve verbatim. Cross-reference it in this canonical doc + research note.

### Step 7 — 300-series doc re-baseline (#10) — anchor G
Live structure is the **3-component diagnostics trio**:
`MithrilPartialSyncSection.tsx` → `MithrilPartialSyncRecommendation.tsx` /
`MithrilPartialSyncConfirmation.tsx` (all under `source/renderer/app/components/status/`); the
in-session overlay is `MithrilPartialSyncOverlay.tsx` under
`source/renderer/app/components/loading/mithril-bootstrap/`.
- **Verify** the ux-refinement `task-plans-ux-refinement/phase-3/` docs already cite this live trio.
  (At plan time they do — every path ref is `components/status/MithrilPartialSync{Section,
  Recommendation,Confirmation}` and the overlay path is correct. Record "already aligned; no edit
  needed" if still true; otherwise correct any drifted path.)
- The genuinely stale plans (UX described "inline in `DaedalusDiagnostics.tsx`") are in the
  **original `task-plans/`** tree (`task-300…304`). The orchestrator prompt **forbids writing into
  `task-plans/`**. Reconciliation (record in research): leave `task-plans/` untouched as an immutable
  pre-refactor record; capture the live 3-component baseline here and in the historical UX plan
  (Step 8) instead. This honors both the acceptance criterion and the hard rule.

### Step 8 — Historical UX plan pointer (#27) — `.agent/plans/mithril/mithril-snapshot-ux.md`
> **SUPERSEDED — reverted post-completion. See "Post-completion reconciliation" below; the historical
> plan was left untouched and this step was not kept.**
That doc today covers only the empty-DB **bootstrap** flow. Add a brief new section (e.g. after
`## Overview` or near `## References`) titled **"Partial Sync (catch-up) — see UX-refinement PRD"**
that: (a) states partial sync reuses the shared Mithril progress components for a **non-empty** DB
catch-up; (b) names the live diagnostics trio + the overlay (Step 7 paths); (c) points to
`.agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-prd.md` as the detailed source —
**do not duplicate** the PRD content.

---

## Files expected to change
1. `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` (Step 1).
2. `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (Step 2).
3. `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` (Step 3).
4. `source/renderer/app/i18n/locales/defaultMessages.json` (Step 4b).
5. `translations/messages.json` (Step 4a — regenerated).
6. `source/renderer/app/i18n/locales/en-US.json` (Step 4c).
7. `source/renderer/app/i18n/locales/ja-JP.json` (Step 4c).
8. ~~`.agent/plans/mithril/mithril-snapshot-ux.md` (Step 8).~~ — superseded/reverted (see reconciliation).
9. Phase-5 canonical/review/research docs (this task's own four files).
10. `mithril-partial-sync-ux-refinement-tasks.json` — status → `completed` (scribe step).

No code change for Steps 5 and 6 (doc-only).

## Acceptance criteria (from tasks.json)
1. Orphaned/stale i18n removed/corrected; diagnostics row label localized.
2. Confirmation hand-off copy clarified; #15 `_isTornDown` guard and #24 retry=start documented.
3. 300-series doc references re-baselined to the live 3-component structure.

## Test cases
- No orphaned `mithrilPartialSyncButtonHint` key in any catalog (already true — assert via grep).
- Row label renders from i18n (`messages.sectionLabel`), not a hardcoded string.
- No `!!!` placeholders for the new key in `en-US.json` / `ja-JP.json`.

## Verification plan
Apply the Node-v24 env fix FIRST (typed-scss-modules regen of `.scss.d.ts` + the gitignored
identity-obj-proxy jest sidecar) so tsc/jest failures are not phantom regressions, then run:
- `yarn i18n:extract && yarn i18n:check` (or `yarn i18n:manage`) — clean, no missing/`!!!`.
- `grep -rn "mithrilPartialSyncButtonHint\"" source/ translations/` → only `...Blocked` / `...Ready`.
- `yarn lint` (or `yarn lint:js:fix`).
- `yarn tsc`.
- `yarn test:unit`, then **verify** `MithrilPartialSyncSection.spec.tsx` /
  `MithrilPartialSyncConfirmation.spec.tsx` still pass (expected: **no spec change needed**). The row
  label's English text is unchanged (`"Mithril Partial Sync"`); `Section.spec.tsx` matches the
  *button* via `getByRole(... name: 'Mithril Partial Sync')` (from `buttonLabel`, not the row) and its
  `:190` negative assert (`not.toMatch(/Mithril Partial Sync/)` when the recommendation is hidden)
  still holds because the row and button disappear together; `Confirmation.spec.tsx` does not assert
  the recovery sentence. Only edit a spec if it unexpectedly asserts a literal string that moved.

## Risks / open questions
- Test risk is low and expected to be **no-op**: the row label's English text is unchanged
  (`"Mithril Partial Sync"`), `Section.spec.tsx` matches the button (not the row) via `getByRole` and
  its `:190` negative assert still passes, and `Confirmation.spec.tsx` does not assert the recovery
  sentence. Verify the specs still pass; only touch one if it unexpectedly asserts a moved literal.
- `defaultMessages.json` no-trailing-newline must be preserved to avoid a noisy diff — edit
  descriptor blocks in place rather than `cp`.
- Conflict (tasks.json wants 300-series plans re-baselined vs prompt forbids touching `task-plans/`):
  resolved in Step 7 — re-baseline lands in the ux-refinement phase-3 docs (already aligned) + the
  historical UX plan; original `task-plans/` left as immutable record. Recorded in research note.
- Pre-existing (NOT fixed here — out of scope): the new section row label
  (`mithrilPartialSyncSectionLabel`) and the CTA button (`buttonLabel`) share identical EN text
  "Mithril Partial Sync". Acceptable for this hygiene pass; flagged for `task-ux-601` to disambiguate
  if desired.

## Required doc / research updates
- Finalize this doc's outcome; append `Implementation:` to `task-ux-501-impl-review.md`.
- Record in `task-ux-501-research.md`: the `_isTornDown`/retry=start engineering notes, the
  `task-plans/` re-baseline reconciliation, and the byte-identical messages.json/defaultMessages.json
  relationship.

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-5/task-ux-501-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-5/task-ux-501-impl-review.md`

## Engineering notes (DOC ONLY — captured at implementation, no code change)

### #15 — `_isTornDown` teardown guard (intentional / benign) — `MithrilPartialSyncStore.ts`
Confirmed live at implementation:
- `:72` field `_isTornDown = false;`
- `:90` set `this._isTornDown = true;` inside `teardown()`.
- Early-return guards: `:140-142` (`syncStatus`), `:150-152` (`_updateStatus`), `:196-198`
  (`_refreshAvailability`), `:212-214` (`_applyAvailability`).

Rationale: the store has no IPC-abstraction `unsubscribe`, so the `onReceive` listener is retained
after teardown. `_isTornDown` makes any late frame a no-op so no MobX writes land on a dead store.
The retained listener is benign. **Kept as-is — no code change in this task.**

### #24 — `retry` reuses the start path (no dedicated retry IPC channel) — `MithrilPartialSyncStore.ts`
Confirmed live at implementation (`:238-241`), comment preserved verbatim:
```
// `retry` reuses this start path — there is no dedicated retry IPC channel
// (PRD D8 / gap #24). onRetry in the overlay wires straight to startPartialSync.
startPartialSync = async () => {
```
The overlay's `onRetry` wires straight to `startPartialSync`; there is no separate retry channel
(PRD #24). **Preserved — no code change in this task.** Cross-referenced in
`task-ux-501-research.md`.

### #10 — 300-series doc re-baseline reconciliation
The ux-refinement `phase-3/` docs are append-only chronological review transcripts that already
reference the live structure: the diagnostics parent `DaedalusDiagnostics.tsx` plus the live trio
`MithrilPartialSyncSection.tsx` / `MithrilPartialSyncRecommendation.tsx` /
`MithrilPartialSyncConfirmation.tsx` (all under `source/renderer/app/components/status/`), and the
in-session overlay `MithrilPartialSyncOverlay.tsx` under
`source/renderer/app/components/loading/mithril-bootstrap/`. **Already aligned — no edit needed.**
The genuinely-stale "inline in `DaedalusDiagnostics.tsx`" plans live in the original `task-plans/`
tree, which the orchestrator prompt forbids editing; that tree is left as an immutable pre-refactor
record. The live 3-component baseline is captured here in this canonical doc instead.

---

## Final outcome (scribe — shipped)

Status flipped to `completed` after code-review APPROVAL (impl-review log
`task-ux-501-impl-review.md`: Implementer 2026-06-26; Code Reviewer 2026-06-26 =
APPROVED). `mithril-partial-sync-ux-refinement-tasks.json` task-ux-501 `status` → `completed`,
`completedAt` = `2026-06-26`. All three acceptance criteria met; locked safety invariants
and the bootstrap (empty-DB) flow untouched.

### What shipped (code — hygiene only, no IPC/types/backend)
- `MithrilPartialSyncRecommendation.tsx` (#20) — corrected the stale `buttonLabel` `description`
  ("Disabled placeholder CTA label…" → "CTA label that opens the Mithril partial sync confirmation
  from diagnostics"); `id`/`defaultMessage` unchanged.
- `MithrilPartialSyncSection.tsx` (#21) — added `defineMessages`; new key
  `daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel`; replaced the hardcoded literal row
  label with `intl.formatMessage(messages.sectionLabel)`; `punctuationColon` retained.
- `MithrilPartialSyncConfirmation.tsx` (#25) — reworded the
  `mithrilPartialSyncConfirmationRecovery` `defaultMessage` so the recovery options are framed as
  appearing in the progress/error overlay only on failure, phrased "the available recovery options"
  to honor the `allowedRecoveryActions` render contract; `id`/`description`/JSX unchanged.
- i18n catalogs — `translations/messages.json` regenerated via `yarn i18n:extract`;
  `defaultMessages.json` mirrored (no-trailing-newline preserved); `en-US.json` / `ja-JP.json`
  updated recovery value + added `mithrilPartialSyncSectionLabel` in alpha order, no `!!!`
  placeholders.

### What shipped (docs)
- **#27 reverted post-completion** — the historical bootstrap plan
  `.agent/plans/mithril/mithril-snapshot-ux.md` was left **untouched**. See
  "Post-completion reconciliation" below. The live 3-component baseline is captured in this canonical
  doc only; all partial-sync research/information stays inside `.agent/plans/mithril-partial-sync/`.
- Phase-5 docs: this canonical doc, `task-ux-501-research.md`, and the append-only review logs.

### Decisions recorded (doc-only — no code change)
- **#15 `_isTornDown` teardown guard** — intentional/benign. The store has no IPC-abstraction
  `unsubscribe`, so the `onReceive` listener is retained after `teardown()`; `_isTornDown`
  (`MithrilPartialSyncStore.ts:72`, set `:90`, early-return guards `:140-142`, `:150-152`,
  `:196-198`, `:212-214`) makes any late frame a no-op so no MobX writes land on a dead store.
  Kept as-is.
- **#24 `retry` = start reuse** — no dedicated retry IPC channel. The overlay `onRetry` wires
  straight to `startPartialSync` (`MithrilPartialSyncStore.ts:238-241` comment preserved verbatim).
  Kept as-is; cross-referenced in `task-ux-501-research.md`.
- **#10 300-series re-baseline** — ux-refinement `phase-3/` transcripts already cite the live
  3-component trio + overlay (already aligned, no edit; append-only logs not rewritten); the stale
  pre-refactor "inline in `DaedalusDiagnostics.tsx`" plans live only in the immutable `task-plans/`
  tree (forbidden to edit), left untouched; the live baseline is captured here in this canonical doc.

### Handoff
task-ux-601 (holistic copy pass) is unblocked. Pre-existing, out-of-scope here: the section row
label (`mithrilPartialSyncSectionLabel`) and the CTA button (`buttonLabel`) share identical EN text
"Mithril Partial Sync" — flagged for task-ux-601 to disambiguate if desired.

## Post-completion reconciliation (2026-06-26)
Per maintainer direction after completion, **all partial-sync research/information must stay inside
`.agent/plans/mithril-partial-sync/`**; plans outside that folder must not be touched. The original
Step 8 / gap **#27** (adding a "Partial Sync (catch-up)" pointer to
`.agent/plans/mithril/mithril-snapshot-ux.md`) violated that principle, so it was **reverted** — that
file is restored to its pre-task state and the feature is not referenced from the historical bootstrap
plan. This does **not** affect the task's three acceptance criteria (i18n/label hygiene; hand-off copy +
#15/#24 doc; 300-series re-baseline), which never required editing the historical plan. The component
architecture map that the reverted pointer carried already lives in this canonical doc (Engineering
notes / #10) and the authoritative `mithril-partial-sync-ux-refinement-prd.md`, so no relocation was
needed. The Step 8 / item-8 references in the Plan section above are superseded by this note.

## Status
- Planning status: `approved`
- Build status: `completed`
- Final status: `completed` (`completedAt` 2026-06-26)
- Post-completion: `#27 reverted` — historical plan left untouched; all docs kept in-folder
  (2026-06-26)
