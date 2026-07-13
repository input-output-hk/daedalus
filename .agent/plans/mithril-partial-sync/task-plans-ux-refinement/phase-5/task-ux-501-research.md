# task-ux-501 — Research notes

Captured during implementation of i18n/label hygiene, dead-copy cleanup, and doc re-baseline
(PRD D8: #10, #15-doc, #20, #21, #24-doc, #25, #27).

## i18n catalog relationship (verified)
- `translations/messages.json` and `source/renderer/app/i18n/locales/defaultMessages.json` are
  **byte-identical except a trailing newline** (`messages.json` ends with `\n`; `defaultMessages.json`
  has no trailing newline). Both are formatjs-extract output.
- `yarn i18n:extract` (`@formatjs/cli` 4.8.3) rewrites **only** `translations/messages.json` from the
  source `defineMessages`. It does **not** regenerate `defaultMessages.json`. The implementation
  therefore regenerated `defaultMessages.json` from the freshly-extracted `messages.json` with the
  trailing newline stripped (`printf '%s' "$(cat translations/messages.json)" > defaultMessages.json`),
  yielding a clean 3-change diff and preserving the no-trailing-newline.
- `en-US.json` / `ja-JP.json` are managed by `yarn i18n:check`
  (`react-intl-translations-manager` 5.0.3) and are alphabetically sorted. The new key
  `mithrilPartialSyncSectionLabel` sorts between `...mithrilPartialSyncRecommendationWithProgress`
  and `...mithrilProactivePromptBody` (PartialSync < Proactive: 'a' < 'r').

## Orphaned key (#20 hygiene) — verified absent
`grep -rn "mithrilPartialSyncButtonHint"` (bare) → **zero matches**. Only the live, correctly-described
`...ButtonHintBlocked` and `...ButtonHintReady` keys exist. No change needed.

## #15 — `_isTornDown` teardown guard (doc-only)
`MithrilPartialSyncStore.ts`: field `:72`, set in `teardown()` `:90`, early-return guards at
`:140-142`, `:150-152`, `:196-198`, `:212-214`. The store has no IPC-abstraction `unsubscribe`, so the
`onReceive` listener is retained after teardown; `_isTornDown` makes late frames no-ops (no MobX writes
on a dead store). Intentional and benign — kept as-is.

## #24 — `retry` = start reuse (doc-only)
`MithrilPartialSyncStore.ts:238-241` comment confirms `retry` reuses the `startPartialSync` path with
no dedicated retry IPC channel (PRD D8 / #24). The overlay `onRetry` wires straight to
`startPartialSync`. Preserved verbatim; cross-referenced in the canonical doc.

## #10 — 300-series re-baseline reconciliation (conflict resolved)
tasks.json wants "300-series doc references re-baselined to the live 3-component structure"; the
orchestrator prompt **forbids editing the original `task-plans/` tree**. Resolution:
- The ux-refinement `phase-3/` review transcripts already cite the live structure
  (`DaedalusDiagnostics.tsx` + `MithrilPartialSyncSection/Recommendation/Confirmation.tsx` +
  `MithrilPartialSyncOverlay.tsx`) — **already aligned, no edit**. They are append-only logs and are
  not rewritten.
- The genuinely-stale pre-refactor plans ("inline in `DaedalusDiagnostics.tsx`") live only in the
  immutable `task-plans/` tree, left untouched.
- The live 3-component baseline is captured in `task-ux-501.md` (Engineering notes). (The
  historical-plan pointer originally added under #27 was reverted post-completion — see below.)

## #25 — confirmation hand-off copy
Reworded the `mithrilPartialSyncConfirmationRecovery` copy (EN + JA) to make explicit that the
recovery options (retry / restart normally / wipe + full Mithril sync) appear **in the progress/error
overlay only if the attempt fails**, not at confirmation time. Phrased as "the available recovery
options" to honor the locked `allowedRecoveryActions` render contract (never promise all three).
The EN now matches the JA "ここで選択する必要はありません" (= "you don't *need* to choose here").

## Pre-existing item flagged for task-ux-601 (out of scope here)
The new section row label (`mithrilPartialSyncSectionLabel`) and the CTA button (`buttonLabel`) share
identical EN text "Mithril Partial Sync". Acceptable for this hygiene pass; task-ux-601 may
disambiguate if desired.

## Scribe finalization (2026-06-26)
No new research surfaced at finalize beyond the above. Code-review APPROVED (Code Reviewer
2026-06-26). tasks.json task-ux-501 → `completed`, `completedAt` 2026-06-26.
The doc-only #15 / #24 anchors and the #10 `task-plans/`-immutability reconciliation are the durable
findings carried forward for task-ux-601.

## Post-completion correction (2026-06-26)
Maintainer direction: **all partial-sync research/information stays inside
`.agent/plans/mithril-partial-sync/`; do not touch plans outside that folder.** Gap **#27** (a pointer
section in `.agent/plans/mithril/mithril-snapshot-ux.md`) was therefore **reverted** and that file
restored to its pre-task state. The three acceptance criteria are unaffected (they never required the
historical plan). The architecture map is fully covered by `task-ux-501.md` and
`mithril-partial-sync-ux-refinement-prd.md`, so the maintainer chose not to relocate it. Durable rule
for future tasks: keep every feature doc/reference in-folder; treat `#27`-style cross-links into legacy
plans as out of bounds.
