# task-ux-702a — Plan Review Log (append-only)

> Append-only chronological transcript. Each entry carries a speaker label
> (Planner | Critiquer), an ISO-8601 UTC `Timestamp:`, and an outcome; Critiquer entries end with
> `Decision: approved` or `Decision: requires_changes`. Append at end-of-file only; never rewrite history.

---

## Planner

Timestamp: 2026-06-29T17:22:33Z
Speaker: Planner

Summary:
Assembled the canonical task doc `task-ux-702a.md` from the eight per-category investigation/implementation
plans (CAT-A…CAT-H) and the locked grill decision record (`task-ux-702a-decisions.md`, D-702a-0..7). The
plan remediates the 12 manual-assessment issues as one interactive_validation/interactive_decision task
(copy approvals already gathered in the decision record; never relabeled autonomous), implemented
sequentially on the shared working tree in the collision-safe order CAT-A → CAT-C → CAT-B → CAT-E → CAT-F →
CAT-G → CAT-D → CAT-H, each followed by a code-review pass. Each category subsection carries issues covered,
exact files, ordered mechanical steps, EN+JA i18n keys, tests/stories, locked-invariant notes, and
acceptance criteria to the small-model-implementable bar. Cross-category contracts are pinned explicitly:
CAT-A produces the behind-ness-known signal CAT-F gates on; CAT-C creates the single shared D-702a-6 i18n
key that CAT-F (and CAT-C's confirmation) consume; CAT-D owns all `.scss`/theme tokens and runs after the
copy/logic/structure categories; i18n edits across CAT-B/C/E/F/G are serialized; the copy-overview
deliverable (`task-ux-702a-copy-overview.md`) is produced after all copy lands for the manual JA translator.

Two locked-invariant amendments are flagged for the critique pass and recorded as explicit ADR sections in
the canonical doc: (1) **ADR D-702a-1** — CAT-G removes "Continue to Daedalus" and auto-fires the D9
finalize IPC on the completed-timeout instead of on an explicit click, amending locked #9/#16-as-amended-by-D9
while preserving all finalize/cleanup semantics (staging-folder deletion, marker clear, reset-to-idle);
the PRD success-flow line (`...prd.md:782`) is superseded. (2) **ADR D-702a-2** — CAT-E trims the
cancel-originated `allowedRecoveryActions` to `['retry','restart-normal']` (backend stops emitting
`wipe-and-full-sync` pre-cutover), leaving the post-cutover startup-failure dialog and the active-sync
failure path unchanged; the renderer still renders strictly from `allowedRecoveryActions` (locked #5
intact) and cancellation stays forbidden post-cutover (locked #6). Both amendments, plus the deliberate
304/601 override of `mithrilPartialSyncConfirmationBehind` (CAT-C) and the 601 override of
`completed.subtitle` (CAT-G), are called out so reviewers do not read them as regressions. Critique should
scrutinize the CAT-C↔CAT-F shared-key id reconciliation, the CAT-D confirmation-modal token choice
(standard `--theme-dialog-*` vs `--theme-mithril-*`), the CAT-E 2-vs-3-action asymmetry, and whether each
category's steps remain small-model-implementable.

Decision: (awaiting Critiquer)

---

## Critiquer

Timestamp: 2026-06-29T17:32:37Z
Speaker: Critiquer

Summary:
One broad adversarial pass over the whole `task-ux-702a.md` plan across all 8 categories, with the two
locked-invariant amendments and the un-amended safety boundaries verified against live code.

Locked-invariant amendments — both correctly scoped and ADR-documented (verified in code):
- ADR D-702a-1 (CAT-G, completion auto-fire): the auto-timeout calls the UNCHANGED
  `store.dismissCompletedOverlay` → `mithrilPartialSyncFinalizeChannel` →
  `MithrilPartialSyncService.finalizeCompletedPartialSync()` (`:359-369`), which performs
  `_resetToIdleStatus()` + `fs.remove(stagingRoot)` (staging/mithril-sync folder deletion) +
  `clearMithrilPartialSyncMarker()`. All D9 finalize semantics (reset-to-idle + remove staging dir +
  clear marker + folder deletion, assessment `:66`) are preserved; only the trigger changes (click →
  `useEffect` timeout). `dismissCompletedOverlay` early-returns unless `status==='completed'` and
  finalize is documented idempotent, so a double-fire is safe. `MithrilProgressView`/`MithrilErrorView`
  are bootstrap-shared, but the new `completedTransitionLabel` block is gated on a prop bootstrap never
  passes, and `partialSyncContinue` is referenced only by the partial-sync overlay → bootstrap completed
  frame unchanged (#11). Correct.
- ADR D-702a-2 (CAT-E, cancelled trim): the trim is confined to the two cancel-originated arrays
  (`MithrilPartialSyncService.ts` `status:'cancelled'` `:301-305` and cancel-cleanup `status:'failed'`
  `:315-319`), both reachable only AFTER the post-cutover hard-reject guard at `:278` → pre-cutover only.
  Renderer keeps rendering strictly from `allowedRecoveryActions` (#2/#5 intact). Post-cutover
  startup-failure wipe (`mithrilPartialSyncNodeStartup.ts:119/135`) and the active-sync failure path
  (`_deriveAllowedRecoveryActions:560`) are left intact; `cancel()`'s `:278` post-cutover guard untouched
  (#6). Correct.

No un-amended locked boundary is violated: #3 (confirm still the only start path; tooltip/prompt route
through confirmation), #4 (CAT-A emits an availability boolean, not a threshold; significance stays
backend `isSignificantlyBehind`; CAT-F's known-gate is display-availability), #10 (kill switch untouched),
#11 (shared-component visual changes are decision-sanctioned and code-review-gated). Vocabulary clean: all
shipped i18n values say "Mithril Sync"/"Standard Sync"/"Node Sync" (no user-facing "partial sync"),
behind-ness is epochs-only with the `behindUnknown` fallback (no %/immutable leak); "Partial Sync" survives
only in code identifiers and Storybook dev-tooling kind labels. All 12 issues are covered with acceptance
criteria; no silent scope drop (ISSUE-2 "Last network block" timing and the backend probe-delay are
documented + explicitly deferred to the backend track).

Blockers (require_changes — both are cheap doc-tightening fixes, not redesigns):

1. [small-model-implementability] CAT-C↔CAT-D confirmation-subtext class name is under-determined and will
   misapply styling under literal execution. CAT-C step 3 deterministically renders the subtext paragraph
   with `styles.mithrilPartialSyncConfirmationRecovery` (CAT-D runs LATER, so its `...Subtext` contract is
   not yet settled). CAT-D step 5 then ADDS `.mithrilPartialSyncConfirmationSubtext` and step 6 keeps
   `...Recovery` "if unused" — but `...Recovery` IS used by CAT-C, so a literal executor leaves the new
   subtext styling on an unused `...Subtext` class while the actually-rendered `...Recovery` keeps its
   stale styling. Fix: pin ONE class up front — have CAT-D restyle `.mithrilPartialSyncConfirmationRecovery`
   IN PLACE (no add-Subtext, no rename), keeping CAT-D scss-only; delete the "rename in lockstep" branch
   (which would force CAT-D to edit `.tsx`, contradicting its scss-only charter).

2. [cross-category contract / dead-code] CAT-F's dependency on CAT-A's signal is not actually load-bearing
   as written. CAT-F step 1 computes the known-gate as local `behindByEpochs !== undefined` and only
   "prefers AND-ing `networkStatus.isBehindnessKnown` ... if available," so a literal executor never wires
   CAT-A's signal and CAT-A's new `isMithrilBehindnessKnown` helper + `NetworkStatusStore.isBehindnessKnown`
   computed become orphaned (CAT-A's ISSUE-1 refresh fix still stands; only the ISSUE-2 signal half dies).
   Fix: pin CAT-F to consume `networkStatus.isBehindnessKnown` (make the dependency real and the deliverable
   live) OR explicitly drop CAT-A's computed/helper if CAT-F will rely on its local equivalent — do not ship
   both with an "if available" hedge.

Non-blocking notes:
- CAT-D is described as the "sole owner of all .scss," but CAT-E step 8 edits `MithrilErrorView.scss`
  (`.actions: flex-end` + new `.hintBody` rule, required so its `styles.hintBody` tsx compiles). This is
  collision-safe (CAT-E runs before CAT-D; CAT-D step 7 is told not to revert it), but relabel CAT-D as the
  color/token owner with CAT-E's documented layout carve-out so the invariant statement is accurate.
- CAT-F's App-level mount (sibling of `<Router>`) is not route-scoped, so the prompt can render over any
  idle route, not just Wallet Summary. Acceptable per the persist intent, but add an explicit verify step
  that it does not surface over unexpected screens (e.g. settings) and that CAT-D step 4 positioning avoids
  clipping the Wallet Summary header.
- CAT-E's intentional 2-vs-3 action asymmetry (cancelled=2 via the trim; pre-cutover sync-FAILURE=3 via the
  untouched `:560`) is decision-locked by D-702a-2 and flagged, but the same "DB intact pre-cutover → wipe
  unnecessary" rationale arguably applies to `:560`; reviewer should consciously accept or escalate.
- The shared bootstrap surfaces (`MithrilErrorView` right-align + primary-last reorder applies to ALL
  callers incl. bootstrap; `MithrilProgressView` spinner prop) are #11 risks that hinge on the required
  bootstrap-stories regression check actually being run (Cardano + a light theme) at each code-review gate.
- `COMPLETED_AUTO_DISMISS_DELAY_MS = 4000` (CAT-G) is an unpinned UX value (already in Risks) — confirm with
  the operator during validation.

Decision: requires_changes

---

## Planner

Timestamp: 2026-06-29T17:36:04Z
Speaker: Planner

Summary:
Single allowed planner fix pass (per `prompt-ux-refinement.md:134-138`) resolving the two `requires_changes`
blockers from the Critiquer entry above. No new scope; edits confined to `task-ux-702a.md`.

Blocker 1 (small-model-implementability, CAT-C/CAT-D confirmation-subtext class). Pinned ONE subtext class
end-to-end so the D-702a-4 restyle reaches the actually-rendered element under literal execution:
- CAT-C step 3: pinned the subtext paragraph to `mithrilPartialSyncConfirmationRecovery` and deleted the
  "CAT-D may later rename to `...Subtext` / use `...Subtext` if settled" lockstep branch.
- CAT-D step 5: changed "ADD `.mithrilPartialSyncConfirmationSubtext`" to "restyle the EXISTING
  `.mithrilPartialSyncConfirmationRecovery` rule IN PLACE" (same property block), and rewrote the class
  contract to subtext = `mithrilPartialSyncConfirmationRecovery`, dropping the "reconcile / rename in
  lockstep" hedge (keeps CAT-D scss-only).
- CAT-D step 6: removed `.mithrilPartialSyncConfirmationRecovery (if unused)` from the dead-code deletion
  list (it is live) — now deletes only `Steps`/`...Confirmation`/`...ConfirmationTitle`.
- CAT-D step 10 + Sequencing bullet: corrected the `.scss.d.ts` regen note and the settled-class list (keeps
  `...Recovery` restyled; no `...Subtext` added/lost).

Blocker 2 (cross-category contract / dead-code, CAT-A/CAT-F behind-ness-known signal). Made CAT-F actually
consume CAT-A's deliverable so it is load-bearing (not orphaned), removing the "if available" hedge:
- CAT-F step 1: gate now reads `networkStatus.isBehindnessKnown` (CAT-A's computed) from the injected store
  instead of a local `behindByEpochs !== undefined` re-derivation; `behindByEpochs` is computed for the
  DISPLAY figure only. Updated the gating expression and the top-of-file comment rationale.
- CAT-F root-cause + tests + Risks: clarified the gate consumes CAT-A's computed; the anti-flash spec case
  now asserts `networkStatus.isBehindnessKnown===false` (reads CAT-A's signal, not a local re-derivation).
- CAT-A step 8 ("Contract for CAT-F"): pinned the contract to `networkStatus.isBehindnessKnown` (no "or the
  equivalent `behindByEpochs !== undefined`" hedge); recorded that this keeps `isMithrilBehindnessKnown`/
  `isBehindnessKnown` from becoming orphaned dead code.

Decision: (awaiting Critiquer)
