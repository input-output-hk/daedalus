# task-ux-703 — plan review log (append-only)

## Planner

- Timestamp: 2026-07-03T10:55:00Z
- Wrote `task-ux-703.md` from a full digest of PR #3337 review feedback: 33 inline comments in 32
  unresolved threads (self multi-agent review, Copilot, AndrewWestberg) plus 9 nit cleanups listed
  only in the multi-agent review body. Every thread is mapped in the Finding → Category table
  (CAT-A–G) and every judgment call was resolved with the user in an interactive grilling session
  (2026-07-03) before writing:
  - DD-703-1 packaging: one wave, all threads + nits, single commit.
  - DD-703-2 (T11): Diagnostics section always renders when enabled; state-adaptive copy.
  - DD-703-3 (T2): keep `mithrilPartialSyncThresholdImmutables = 20` as prompt gate; manual offer
    unconditional via DD-703-2; checklist reply to AndrewWestberg, no criterion rewrite.
  - DD-703-4 (T13): keep ADR D-702a-1 auto-finalize; retry once then existing error view with
    retry action.
  - DD-703-5 (T7): `requiredBytes = max(snapshot − chainDir, 0) + 0.2 × snapshot`, 4 GB floor,
    whole-snapshot fallback on measurement failure (user-authored formula).
  - DD-703-6 (T14): stop detaching on POSIX; PID-persist fallback recorded.
  - DD-703-7: automated verification only; smoke-test cheat sheet updated for behavior changes.
  - DD-703-8: no PR writes; deliver `task-ux-703-pr-comment-checklist.md` for manual replies.
- Cross-checked claims against live code before decisions: confirmed ADR D-702a-1 exists and the
  auto-finalize swallow is real (`MithrilPartialSyncOverlay.tsx:59,84-93`); confirmed the T2
  constant is immutables-based, not a percentage (`launcher-config.nix:410`); confirmed the
  metadata normalizer parses only whole-DB size fields (`mithrilSnapshotMetadata.ts:17-21`);
  confirmed both detached spawn sites (`mithrilCommandRunner.ts:162,258`).
- Outcome: canonical plan drafted to the small-model bar; requesting critique.

## Critiquer

- Timestamp: 2026-07-03T11:55:00Z
- One broad pass over `task-ux-703.md`, the PR-comment checklist, and the tasks-JSON entry, with
  ~30 anchor spot-checks against live code. Blockers found:
  1. T5 step anchored to the wrong module — `MithrilController` never emits partial-sync status;
     the cross-session snapshots come from `startInstalledNode`
     (`mithrilPartialSyncNodeStartup.ts:116,132`) via `emitMithrilPartialSyncStatus`
     (`mithrilPartialSyncChannel.ts:132`).
  2. T8 step unimplementable as written — no injected quit handler exists in the startup module
     (real seam: `safeExitWithCode`), and the "non-quit branch re-emits failed" clause contradicts
     the live two-branch dialog (the continue branch wipes and resumes a healthy boot).
  3. Two "verified" file paths wrong: `chainStorageCoordinator.ts` lives in `source/main/utils/`,
     `MithrilPartialSyncConfirmation.tsx` in `components/status/` (plan + JSON `targetPaths`).
  4. Step 4 (T9 wipe restore) silently reversed recorded grill decision D-702a-2 with no
     reconciliation.
  5. Planning status claimed an approved critique pass before one had run.
  Non-blockers: build-status `pending` outside the prompt enum (annotate); T3 categorized CAT-F in
  the plan but filed under CAT-E in the checklist; minor anchor drifts (scss `:13`, messages
  `:16`, service `:964`, `_assertRecoveryActionAllowed:801`); `yarn storybook` line is a soft
  manual checkpoint in an autonomous task; T12's "error/terminal state" predicate left fuzzy.
  Verified sound: minimum-contents coverage, full T1–T32/N1–N9 traceability, DD-703-5 formula
  consistency across plan/checklist/JSON, ADR D-702a-1's existence (boundary-9 restatement
  legitimate), T18/N6 claims, worktree HEAD `7723298239`.
- Decision: requires_changes

## Planner (fix pass)

- Timestamp: 2026-07-03T12:25:00Z
- Blocker 4 was a genuine decision conflict, so it went back to the user (grill continuation)
  rather than being self-resolved: user chose **best-effort cancel cleanup, no wipe restore,
  D-702a-2 preserved** — recorded as DD-703-9; step 4, traceability row, non-goals, checklist T9
  response, and the JSON entry all rewritten to match.
- Blocker 1: step 1 re-anchored to the real emission sites; fix now specified as
  `adoptRecoverySnapshot(getMithrilPartialSyncStatus())` seeding in `MithrilController`'s
  recovery-action IPC wirings (`:495` area), with `_assertRecoveryActionAllowed:801` anchor.
- Blocker 2: step 3 rewritten — Quit branch calls `safeExitWithCode`
  (`source/main/utils/safeExitWithCode.ts`); continue branch explicitly left unchanged; checklist
  T8 response updated to acknowledge the comment's second half was already sound.
- Blocker 3: both paths corrected in the plan files-list and JSON `targetPaths`;
  `mithrilPartialSyncChannel.ts` added to both.
- Blocker 5: status lines now state the true sequence (one critique pass, requires_changes, one
  fix pass) — planning status `approved` per the workflow's one-critique + one-fix-pass cap.
- Non-blockers all applied: `pending` annotated, checklist T3 moved to CAT-F, anchors corrected,
  storybook check converted to story/spec assertions + optional author-run note, T12 predicate
  pinned to the exact `'failed'` status check with `MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES`
  reference (`mithril-partial-sync.types.ts:87,106`).
- Outcome: all blockers resolved; plan `approved`; build not started.

## Reviewer (verification grill)

- Timestamp: 2026-07-03T14:45:00Z
- Independent validity/diminishing-returns/actionability review of the approved plan: four parallel
  read-only verification agents re-checked all 41 items (CAT-A; CAT-B/C/D; CAT-E; CAT-F/G) against
  live code, then a grilling session resolved the surfaced decision points with the user (each with
  a recommended option; all recommendations adopted).
- Verdicts: 38/41 items confirmed valid and actionable. Refuted or materially amended:
  1. **T14 refuted** — `killProcessTree` POSIX kill is `process.kill(-pid)` and depends on
     `detached` group leadership (documented in its docstring); POSIX children reparent to init on
     parent crash regardless of `detached`. DD-703-6 revised to **no code change + checklist
     reply**.
  2. **T11 gap** — probe failure is collapsed into `{ isSignificantlyBehind: false }` before IPC;
     DD-703-2's third copy state was unimplementable. New DD-703-10 adds an `isProbeFailed` flag
     through availability type → service catch → controller → store.
  3. **T15 underspecified** — no renderer-set "overlay-visible error state" exists (`toStartError`
     yields a plain `Error`; the overlay picks the error view from backend status). New DD-703-11:
     catch + resync + log, no new UI surface; also covers the missed `App.tsx:116` `onRetry`
     wiring that T12's broader rethrow would otherwise leave unhandled.
  4. **T16 incomplete** — additional user-reachable hardcoded-prose throw sites found (`:180`,
     `:355-356`, `:556-568`, `:805-813`, `:901-903`, `:1080-1082` with banned "immutable",
     `_buildError` fallback `:1290`; `_buildError` copies `error.message` verbatim into
     `status.error`). New DD-703-12 widens T16 to all of them.
  5. **N1 refuted** — the two kill implementations are not equivalent (group kill of a live
     `ChildProcess` vs polled single-pid kill of a possibly prior-session PID); unification would
     alter CardanoNode shutdown semantics for a nit. New DD-703-13 declines it.
  6. **N4 amended** — 11 props (not ~13); feasible (view already holds intl fallbacks) but touches
     the locked bootstrap boundary. New DD-703-14 keeps it under explicit bootstrap guards.
- Factual corrections applied without a decision: T6/T9 "startup-reclaim path" does not exist for
  cancel-orphaned staging (no marker pre-cutover; reclaim happens at the next partial-sync start's
  staging prepare) — DD-703-9, steps 2/4, and checklist T9 reworded; T7 must reuse the existing
  recursive `getPathSizeBytes` helper (`chainStorageManagerShared.ts:499-520`) instead of new du
  logic; T12 flag reset needs `runInAction`; T13 needs local component state to leave the progress
  view; T28 must keep `_getCachedCertifiedEpoch()` after the joined await; T23 helper must absorb
  the string-vs-Error and fallback-wording divergences (and fixes the prompt's own hardcoded
  fallback at `:151-154`); T26 helper parameterization pinned (args prefix, binary path, async env,
  stdin). Verification section gained Node-24 env prep notes (scss typings regen; prettier 2.1.2
  oscillation on `mithrilCommandRunner` specs).
- Files-expected list updated (App.tsx added; killProcessTree/CardanoNode removed; T11 plumbing
  annotated). Acceptance criterion "answered with no code change" now covers T2, T14, N1.
- Decision: plan remains `approved` (amended in place; DD-703-6 revised, DD-703-10…14 added);
  build not started.
