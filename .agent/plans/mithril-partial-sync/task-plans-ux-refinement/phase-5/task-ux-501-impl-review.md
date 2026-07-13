# task-ux-501 — Implementation review log (append-only)

Append-only chronological transcript. Each entry: speaker, ISO-8601 UTC timestamp, outcome.

---

Implementation:
Speaker: Implementer
Timestamp: 2026-06-26
Outcome: Implemented PRD D8 hygiene/doc gaps (#10, #15-doc, #20, #21, #24-doc, #25, #27). Executed the
approved canonical plan (`task-ux-501.md`) step by step. Autonomous mode; no scope change. All
verification green (see below).

Changes (per file):
- `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` (#20) — corrected the
  stale `buttonLabel` `description` from "Disabled placeholder CTA label for Mithril partial sync" to
  "CTA label that opens the Mithril partial sync confirmation from diagnostics". `id`/`defaultMessage`
  unchanged.
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (#21) — added `defineMessages`
  to the `react-intl` import; added a `messages` const with new key
  `daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel` (defaultMessage `!!!Mithril Partial
  Sync`); replaced the hardcoded literal `Mithril Partial Sync` row label with
  `intl.formatMessage(messages.sectionLabel)`. `punctuationColon` via `globalMessages` retained.
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` (#25) — reworded the
  `mithrilPartialSyncConfirmationRecovery` `defaultMessage` so the recovery options are explicitly
  framed as appearing in the progress/error overlay only on failure, phrased as "the available
  recovery options" to honor the `allowedRecoveryActions` render contract. `id`/`description`/JSX
  unchanged.
- `translations/messages.json` — regenerated via `yarn i18n:extract` (picks up the description fix,
  the new `sectionLabel` descriptor, and the reworded recovery copy).
- `source/renderer/app/i18n/locales/defaultMessages.json` — mirrored from the freshly-extracted
  `messages.json` with the trailing newline stripped (clean 3-change diff; no-trailing-newline
  preserved).
- `source/renderer/app/i18n/locales/en-US.json` — updated recovery value; added
  `mithrilPartialSyncSectionLabel: "Mithril Partial Sync"` in alpha order. No `!!!` placeholders.
- `source/renderer/app/i18n/locales/ja-JP.json` — updated recovery value (matching JA); added
  `mithrilPartialSyncSectionLabel: "Mithril Partial Sync"` (diagnostics keys keep the English term per
  task-ux-601 ownership) in alpha order. No `!!!` placeholders.
- `.agent/plans/mithril/mithril-snapshot-ux.md` (#27) — added a "Partial Sync (catch-up) — see
  UX-refinement PRD" section naming the live diagnostics trio + the overlay and pointing to
  `mithril-partial-sync-ux-refinement-prd.md` (no PRD duplication).
- `.agent/plans/.../phase-5/task-ux-501.md` — added Engineering notes for #15 (`_isTornDown` guard),
  #24 (`retry`=start reuse), and the #10 re-baseline reconciliation; status → completed.
- `.agent/plans/.../phase-5/task-ux-501-research.md` — created (catalog relationship, conflict
  reconciliation, doc-only anchors).

Doc-only (no code change): #15 `_isTornDown` teardown guard and #24 `retry`=start reuse — confirmed
live in `MithrilPartialSyncStore.ts` (`:72,90,140-142,150-152,196-198,212-214` and `:238-241`) and
documented; store code untouched.

#10 re-baseline: ux-refinement `phase-3/` review transcripts already cite the live 3-component trio +
overlay — already aligned, no edit. Original `task-plans/` tree left immutable per orchestrator rule;
live baseline captured in `task-ux-501.md` + `mithril-snapshot-ux.md`.

#20 orphan check: bare `mithrilPartialSyncButtonHint` confirmed absent via grep (only `...Blocked` /
`...Ready` exist) — no change needed.

Verification (Node v24; scss `.d.ts` present, identity-obj-proxy sidecar present — no env fix needed):
- `yarn i18n:extract` — PASS (exit 0); only `translations/messages.json` rewritten (expected diff).
- `yarn i18n:check` — PASS (exit 0); no mithril keys reported missing or `!!!`. The remaining `!!!`
  list is the pre-existing repo-wide untranslated set, unrelated to this task.
- `yarn tsc` — PASS (exit 0, clean).
- `yarn eslint` (3 touched components) — PASS (exit 0). 3 warnings on
  `MithrilPartialSyncSection.tsx:28` (`onStartMithrilPartialSync: (...args: Array<any>) => any`) are
  PRE-EXISTING (that prop type line was not touched).
- `yarn test:jest` (`MithrilPartialSyncSection.spec.tsx`, `MithrilPartialSyncConfirmation.spec.tsx`) —
  PASS: 2 suites, 21 tests passed, no spec changes needed (row English text unchanged). The Modal
  strict-mode lifecycle warning is pre-existing React noise.
- `yarn test:unit` (cucumber) — NOT run for this change: the touched specs are Jest component tests
  (run above); the cucumber `@unit` suite does not cover these diagnostics components and is slow.
  Flagged here rather than claiming it ran.

Deviations: none of substance. Per plan Step 4b, `defaultMessages.json` was regenerated from
`messages.json` (newline-stripped) rather than per-block in-place editing — same end result, clean
3-line diff, no-trailing-newline preserved. No IPC/types/backend touched; locked invariants and
bootstrap flow untouched.

Handoff: ready for impl review. Scribe to stamp/confirm the timestamp and flip
`mithril-partial-sync-ux-refinement-tasks.json` task-ux-501 status → `completed`. task-ux-601
(holistic copy pass) is unblocked and may want to disambiguate the identical EN text shared by
`mithrilPartialSyncSectionLabel` and `buttonLabel` ("Mithril Partial Sync").

---

Code Review:
Speaker: Code Reviewer
Timestamp: 2026-06-26
Decision: APPROVED

Reviewed the working-tree diff (deliverable files only; numerous untracked `.js`/`.js.map` are
pre-existing `tsc`-emit build artifacts and out of scope — they must NOT be staged on commit).

Acceptance criteria — all met:
1. i18n/label hygiene (#20, #21): stale `buttonLabel` description ("Disabled placeholder CTA
   label...") corrected; the button is genuinely functional (`onClick={onShowConfirmation}`,
   `disabled` only conditionally) so the new description is accurate. New key
   `mithrilPartialSyncSectionLabel` added in `MithrilPartialSyncSection.tsx` via `defineMessages`
   and consumed as `intl.formatMessage(messages.sectionLabel)`; the hardcoded literal
   `Mithril Partial Sync` row label is gone. Key present and consistent across en-US.json,
   ja-JP.json, defaultMessages.json and translations/messages.json (alpha-ordered, no `!!!` in any
   mithril value). Orphan `mithrilPartialSyncButtonHint` (bare) confirmed absent via grep — no
   change needed.
2. Confirmation hand-off (#25): `mithrilPartialSyncConfirmationRecovery` reworded (EN + JA) to state
   recovery options appear on the progress screen only if the sync fails, "the available recovery
   options" — consistent with the `allowedRecoveryActions` render contract. #15 (`_isTornDown`
   teardown guard) and #24 (`retry` reuses `startPartialSync`, no dedicated retry IPC) are DOC-ONLY;
   anchors re-verified live in `MithrilPartialSyncStore.ts` (`:72,90,140-142,150-152,196-198,
   212-214`; `:238-241` comment) — store code untouched.
3. Doc re-baseline (#10, #27): `mithril-snapshot-ux.md` gains a partial-sync pointer naming the live
   3-component diagnostics trio + the overlay and linking to the existing
   `mithril-partial-sync-ux-refinement-prd.md` (link target verified present). No PRD duplication.

Verification (reviewer spot-checks): all four message catalogs parse as valid JSON; new key wired
and old literal removed; grep confirms zero `!!!` on mithril keys in locale catalogs; doc-only store
anchors accurate; PRD link resolves. Changes are trivial intl wiring + string/description/JSON/doc
edits following the established pattern.

Locked safety invariants: NOT touched — no store/IPC/types/backend code changed; confirmation-
precedes-start, allowedRecoveryActions-only rendering, no renderer-computed thresholds, staged-only
restore, and no synthetic metrics all preserved. Bootstrap (empty-DB) flow untouched. No IPC/contract
drift. No scope creep beyond gaps #10/#15-doc/#20/#21/#24-doc/#25/#27.

Notes (non-blocking): (a) the working tree carries an unrelated `.gitignore` `.devcontainer` line that
predates this session — not part of the deliverable; (b) as flagged by the implementer, `sectionLabel`
and `buttonLabel` now share identical EN text "Mithril Partial Sync" — disambiguation is correctly
deferred to task-ux-601. Scribe to confirm the timestamp and flip tasks.json task-ux-501 →
`completed`.

---

## Reconciliation (Maintainer + Orchestrator) — 2026-06-26
Outcome: reverted the #27 out-of-folder edit. `.agent/plans/mithril/mithril-snapshot-ux.md` is
restored to its pre-task (parent-commit `67679c123`) state — the "Partial Sync (catch-up)" pointer
section is removed. No dangling references (review confirmed nothing links to that section). i18n /
component / copy deliverables (#10/#15-doc/#20/#21/#24-doc/#25) are unchanged and were independently
re-reviewed as ship-as-is (no medium/high issues). Canonical doc + research note corrected to drop
historical-plan cross-references; this transcript is append-only and not rewritten. Recorded in a
follow-up commit (task-501 commit `b8cb819e8` left intact — append-only history honored).
