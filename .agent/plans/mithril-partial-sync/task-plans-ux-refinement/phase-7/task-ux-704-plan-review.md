# task-ux-704 — plan review log

## 2026-07-04 — plan validation pass (2 reviewer agents) + fix pass (5 author agents)

Scope: `task-ux-704.md` (master) + `task-ux-704-plan-cat-a.md` … `task-ux-704-plan-cat-h.md`.
Method: every load-bearing anchor spot-checked against HEAD `7c9f7de20`; implementability judged
against a literal mid-size-model executor; locked-decision screen against DD-703-1…14, the PRD
locked boundaries, ADR D-702a-1/2, and D-702b-9; seam contracts S1–S4 checked cross-doc; per-CAT
out-of-scope lists reconciled with the master's deferred/refuted registers. A key review fact:
`yarn compile` (`tsc --noEmit`) typechecks specs AND `storybook/**`, which drove three of the five
blockers.

### Verdicts (initial → after fix pass)

| Doc | Initial verdict | Blockers | Majors | Resolution |
| --- | --- | --- | --- | --- |
| task-ux-704.md (master) | APPROVED | 0 | 0 | 4 minors applied directly (C-1 behavior exception, S2 completeness, gate-4 D12 exception, D3-refuted register entry) |
| cat-a | REQUIRES-CHANGES | 2 | 0 | fixed + re-verified |
| cat-b | REQUIRES-CHANGES | 1 | 0 | fixed + re-verified (4 minors incl. the accepted cancel-window equivalence note) |
| cat-c | REQUIRES-CHANGES | 0 | 1 | fixed (launcherConfig import keep-note; warn-line count; multi-line quote; C1-partial row) |
| cat-d | REQUIRES-CHANGES | 2 | 0 | D-1 rewritten against CAT-B's end state; new Step 1.7 for the four storybook fixture codes |
| cat-e | APPROVED | 0 | 0 | 2 recommended minors applied (comment hygiene; falsified fixture comment deleted in-step) |
| cat-f | REQUIRES-CHANGES | 0 | 1 | Step 3.2 split to also delete the stale `@ts-ignore ts-migrate(2339)`; scope guard widened to two lines |
| cat-g | APPROVED | 0 | 0 | no changes |
| cat-h | REQUIRES-CHANGES | 0 | 1 | mooted list extended (CAT-F `:60-65`, CAT-D `:136-139` hand-offs) + 2 relocation notes + CAT-A narrative sync |

### The five blockers (all doc-level; none reached source)

1. **cat-a / A-4** — deleting the two listener arrays without also deleting the direct field
   assignments in `handleDiskSpace.spec.ts:155-156` (`resetMithrilControllerForTests`) → TS2339.
2. **cat-a / A-2** — deleting the `getPendingBootstrapDecision` mock key while the suite
   `beforeEach` (`mithrilBootstrapChannel.spec.ts:66`) re-primes it → every test crashes.
3. **cat-b / B-1.4** — same class: `broadcastPartialSyncStatus` prime at
   `mithrilPartialSyncChannel.spec.ts:76-78`.
4. **cat-d / D-1** — drafted against HEAD, not CAT-B's end state (seam S2): the factory threading
   D-1 planned to retype no longer exists post-B; `createPartialSyncStageError`'s `code?: string`
   param needed the union too or `mithrilErrors.ts` itself fails compile.
5. **cat-d / Step 1.1(b)** — union-typing `MithrilPartialSyncError.code` breaks `tsc` at four
   storybook fixtures carrying story-invented codes
   (`MithrilPartialSyncOverlay.stories.tsx` `'MITHRIL_PARTIAL_SYNC_CANCELLED'` et al.); resolved by
   a new Step 1.7 (drop `code` from the four fixtures — copy-neutrality argued per fixture; the
   widened-fixture-type alternative rejected for colliding with CAT-F).

Common failure class for 1–3: a "now-unreferenced" deletion predicate falsified by a spec
`beforeEach` prime or direct private-field assignment. Future deletion plans must grep spec
`beforeEach`/setup helpers for the deleted symbol, not just call sites.

### Locked-decision screen

No doc reopens DD-703-1…14, the PRD locked boundaries, ADR D-702a-1/2, or D-702b-9. Notables:
CAT-B preserves the stage-error wire shape (`name`/`message`/`stage`/`code`) so T16/DD-703-12
mapping is untouched; CAT-C's de-fork leaves the D-702b-9 TTL caches intact (comment-text updates
only); CAT-D's five `COPY_BY_CODE` additions are provably copy-neutral (all five emit stage
`'preparing'`, which `COPY_BY_STAGE` omits → generic `FAILED` fallthrough before and after);
CAT-E's E-1 deletion is consistent with PRD D13 and includes the D13 note update.

### Documented behavior deltas (accepted, recorded in the plans)

- CAT-C C-1: probe-failure error path now resolves the correct custom path instead of silently
  defaulting — strict correctness improvement (master header notes it).
- CAT-B B-3: transient `'already in progress'` rejection window for a concurrent
  `startBootstrap()` during a cancel's cleanup await — guard tightening on an already-cancelled
  run (equivalence note in cat-b).
- CAT-D Step 1.7: the four error stories' technical-details line no longer shows a fictional code
  (story-visible only).

### Outcome

All fixes were applied by the original author agents from reviewer-specified replacement text,
with anchors re-verified at HEAD before editing. Planning status: **approved**.

## 2026-07-05 — independent grounding verification (9 verifier agents) + fix pass (4 fix agents)

Scope: all nine docs, verified against the working tree at HEAD `7c9f7de20` (clean source tree —
the same commit the plans were authored against). Method: one adversarial verifier per CAT doc
plus one for the master + cross-doc seams. Every load-bearing anchor, quoted snippet, and
zero-caller predicate re-checked (greps included spec `beforeEach`/setup primes and direct
private-field assignments, per the 2026-07-04 failure class); byte-identity claims mechanically
diffed; CAT-D's post-CAT-B targeting cross-read against the CAT-B doc; CAT-H's mooted list checked
exhaustively and its inventory sampled ~250 of ~414 entries; CAT-F's typed-story claim re-proven
by scratch compile against the installed @storybook typings. Fix agents re-verified every finding
before editing (all reproduced; zero false findings).

### Verdicts (verification → after fix pass)

| Doc | Verification verdict | Resolution |
| --- | --- | --- |
| cat-a | GROUNDED, 2 minors | fixed (type-import justification; :772 comment updated for A-8's deletions) |
| cat-b | 1 BLOCKER + 2 minors | fixed (B-4.1 now covers the `resetOnDirectoryChange()` field resets at `MithrilStartupGate.ts:221-222` — TS2339 as written; `startBootstrap()` naming; branch-count phrasing) |
| cat-c | 1 major | fixed (Step 1.4 extended to the FOURTH fork-falsified comment, `MithrilPartialSyncService.ts:1032-1034`, adopting CAT-H's replacement text verbatim; ALL-CAPS emphasis lowercased in replacement texts) |
| cat-d | 2 minors | fixed (false "story async wrapper" evidence removed; approximate anchors made exact) |
| cat-e | 2 minors | fixed ("CAT-D already landed" made wave-relative; overbroad i18n-artifact rationale narrowed) |
| cat-f | 1 major + 2 minors | fixed (Staking "plain registration" precedent is guarded by a stale `@ts-ignore` and proves nothing — replaced with the csf-typings mechanism, conclusion independently re-proven by scratch compile; fixture-importer count; jest pattern's covered-spec list) |
| cat-g | 1 minor | fixed (Step 1.1 expected `git grep` output replaced with the `.agent/`-only invariant — git grep sees tracked files only) |
| cat-h | 1 BLOCKER + 2 majors + 6 minors | fixed (see below) |
| master | 2 majors (duplicates of the H-side conflicts) + 3 minors | fixed (four comments; A/B/C co-touch of `handleDiskSpace.ts` + multi-CAT file map; header now enumerates all four accepted behavior deltas) |

### The two blockers

1. **cat-b / B-4.1** — the `_declineInFlight` record migration missed that both boolean fields are
   also reset in `resetOnDirectoryChange()` (`MithrilStartupGate.ts:221-222`, a live path) →
   `yarn compile` fails with TS2339. Same failure class as the 2026-07-04 blockers (unswept
   reference site falsifying a replacement predicate), this time in production code, not a spec.
   Deletion/replacement sweeps must include field writes outside the methods being replaced.
2. **cat-h / mooted list** — the CAT-C hand-off was entirely missing: H's live REWRITE texts for
   `:157-159`, `:979-981`, spec `:2380-2381`, and `:1032-1034` re-state the fork that CAT-C's
   de-fork makes false; executing H after C would re-falsify C's fixes. Also missing: the CAT-B
   hand-offs (`:386-388` rewritten by B-5.2; spec `:1028-1029` deleted; `:1060-1061` trimmed).
   All now recorded as handled-by-CAT-C / handled-by-CAT-B bullets. B-5.2's replacement text was
   itself reworded to comply with H's conventions (no ALL-CAPS, no defending-the-change prose).

### Other notable fixes

- cat-h Part C: ~133 DELETE entries carry bare line numbers, unexecutable under the doc's
  relocate-by-quote mandate — a post-A–G re-anchor + rubric-classify procedure is now specified
  (test-file default DELETE; comment-only rule bounds damage), with explicit re-anchoring in the
  three spec files CATs A/B rework.
- cat-h hazard #2 synced with the mooted list (the `:1374-1377` tripwire site is deleted by A-5);
  retitle attribution corrected to CAT-B B-1.4(2); `Diagnostics.stories.tsx:76-77` added to the
  CAT-E bullet; Overlay/Service.spec block tallies corrected (14, ~57); two ±1 anchors fixed;
  density-gate arithmetic now acknowledges comment lines A–G ADD.
- Pre-existing drift noted, no action: `MithrilPartialSyncService.ts:90` TTL comment ("only the
  aggregator query is cached") is stale at HEAD — already covered by CAT-H's accuracy-fix list.

All fixes doc-only; no source/spec/storybook file touched. Planning status: **approved**
(re-affirmed).
