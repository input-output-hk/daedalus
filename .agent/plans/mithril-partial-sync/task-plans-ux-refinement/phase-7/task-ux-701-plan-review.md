# task-ux-701 — Plan Review Log (append-only)

> Append-only chronological transcript. Each entry carries a speaker label
> (Planner | Critiquer), an ISO-8601 UTC `Timestamp:`, and an outcome; Critiquer entries end with
> `Decision: approved` or `Decision: requires_changes`. Append at end-of-file only; never rewrite history.
>
> The Planner sign-off for this task lives at the foot of the canonical doc `task-ux-701.md`
> (`## Planner`, `Decision: approved`). This log awaits the Critiquer pass.

---

## Critiquer

Timestamp: _(awaiting — the critique stage appends here)_
Speaker: Adversarial Critiquer

_Awaiting one broad critique pass over `task-ux-701.md` for: gap/PRD coverage (gaps #16/#17/#18/#19,
PRD D7), consistency with tasks.json:748-779 + the locked invariants, correctness of the corrected
line-anchors (Concern B throw `:191-202`, Concern C `_emptyManagedContents` `:442-447`), the
autonomous-eligible vs operator-owned `manual_execution` split, hidden manual checkpoints, missing
tests/docs, and whether the lower detail is concise and small-model-implementable._

Decision: _(superseded by the entry below)_

---

## Critiquer

Timestamp: 2026-06-29T15:42:00Z
Speaker: Adversarial Critiquer

Summary:
One broad critique pass over `task-ux-701.md`, with the three concern seams re-opened against live source.

(a) Gap / PRD coverage — PASS. Concern A = gap #16 (cleanup), Concern B = gap #17 (drift), Concern C =
gaps #18/#19 (immutable-merge / `00000.primary`); each maps to the backend half of PRD D7 with the
research/02, /18, /19 evidence anchors cross-linked. All four gaps are covered.

Anchor verification (I re-read the live files, did not take the plan on faith):
- Concern A: `_cleanupPartialSyncArtifacts()` is exactly `fs.remove(stagingRoot)` then
  `clearMithrilPartialSyncMarker()` at `MithrilPartialSyncService.ts:579-582`; cancel call site `:297`,
  restart-normal `:335`, wipe (direct `fs.remove`) `:346`, `try/finally` → `_clearRuntimeWorkState()` at
  `:327` all confirmed. Post-cancel `allowedRecoveryActions = ['retry','restart-normal',
  'wipe-and-full-sync']` at `:301-305` confirmed (boundary #2 — literal, not status-inferred). CORRECT.
- Concern B: constant `:60` confirmed; the drift re-resolve + `!== partialSyncRange.end` guard + throw of
  `PARTIAL_SYNC_LATEST_DRIFT_CODE` with the "Please retry with the refreshed range" message sit at
  `:191-202`, in the `preparing` stage (pre-cutover, staged-only). The plan's correction away from the
  gap-cited `:169-180` is ACCURATE.
- Concern C: layout merge at `chainStorageManagerLayout.ts:588-629` confirmed line-for-line — validation
  `:588-595`, empty-excluding-immutable `:597-599`, per-file immutable MOVE-merge `:601-618`,
  non-immutable wholesale move `:619-624`, staged-root cleanup `:627-629`. The corrected anchors
  `_emptyManagedContents` `chainStorageManager.ts:442-447`, `migrateData(...,{preserveSourceRoot})`
  `:298-332`, `_movePath` `:457-459` are ALL ACCURATE. The grounding's `:302-330`/`preserveSourceRoot`
  mislabel was correctly caught. The `00000.primary` structural argument (immutable excluded from empty,
  then per-file merged in) is sound.

(b) Interaction-mode deferral — PASS. The doc holds `interactive_validation`/`manual_execution`, states
explicitly it MUST NOT set JSON `completed`/`completedAt` or commit, and fences OP-A/B/C (live-chain,
mainnet/preprod/preview, packaged build) as operator-owned for task-ux-702. The JSON-sync + commit are
deferred until operator evidence exists. Build status left `in_progress`, not done.

(c) Locked boundaries + vocab — PASS. Boundaries #1 (staged-only), #2 (allowedRecoveryActions-
authoritative), #3 (latest-snapshot-only), #4 (no cancel after cutover), #5 (networks), #7 (no bootstrap
regression) are carried inline and tied to the right concern. Vocab guardrail (Mithril/Mithril Sync,
epochs-only behind-ness, "immutable files" internal-only) is stated and respected — "partial sync" only
appears in internal code-identifier / staging-path context.

(d) Small-model executability — PASS. Part 1 (static re-trace A.1–C.3) and Part 2 (spec run) carry exact
files + line ranges; the spec list resolves to real files on disk (all eight `*.spec.ts` present). Part 3
is correctly fenced as not-runnable-here. The targeted-fix gate is defect-conditional with "no
re-architecture" repeated.

Non-blocking concerns (record, do not gate the build):
- Checklist step B.2 attributes the "RETRIABLE" classification to the task-ux-403 error-copy map. Verified
  `partialSyncErrorCopy.ts:19-22,51` only carries `title`/`hint` copy for the `PARTIAL_SYNC_LATEST_DRIFT`
  key; the actual retriability is governed by the backend `allowedRecoveryActions` (boundary #2), not the
  copy map. The cross-check is still valid (it confirms the code is a handled, user-facing case and the
  hint copy guides retry) — but the implementer should phrase the evidence as "copy-map handles the code +
  backend recovery actions confer retriability", not "the copy map classifies it RETRIABLE".
- Part 2 lists the full backend spec set; for the fast loop the load-bearing specs are
  `MithrilPartialSyncService.spec.ts` (A+B) and `chainStorageManager.spec.ts` + the layout/validation
  specs (C). Fine as written; no change required.

No blocking items. The plan covers all four gaps with verified seams + evidence, correctly defers the JSON
status flip and commit to post-operator-evidence, honors every locked boundary and the vocab guardrail,
and the automated subset is precisely executable.

Decision: approved
