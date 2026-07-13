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

## Planner (CAT-A per-section plan)

- Timestamp: 2026-07-03T16:59:24Z
- Wrote the CAT-A per-section implementation doc `task-ux-703-plan-cat-a.md`: T5 service `adoptRecoverySnapshot` + controller recovery-wiring seeding with repro/guard specs; T6/T8/T9/T10 startup/cancel/staging hardening with quoted anchors and verbatim edits + unit tests; T14 recorded as checklist-only no-op (anchors verified).
- All brief anchors re-verified against the working tree — no drift; no i18n work in CAT-A (log lines only); dialog "partial sync" copy explicitly left to CAT-E.
- One interpretation flagged (plan escalation E1): the startup wipe branch reclaims staging marker-only — no session resolver exists in `mithrilPartialSyncNodeStartup.ts`; escalate-on-mismatch noted.
- Outcome: CAT-A plan written (idempotent overwrite); no source files modified; build not started.

## Critiquer (CAT-A per-section plan)

- Timestamp: 2026-07-03T17:04:41Z
- One broad pass over `task-ux-703-plan-cat-a.md` against the CAT-A brief; spot-checked anchors in live code: `mithrilPartialSyncNodeStartup.ts` (:66-68, :82-106, :116/:132-135; no `_getStagingRootPath` in the module — E1's deviation is correct), `MithrilPartialSyncService.ts` (`finalizeCancel`:403-450, `wipeAndFullSync`:523-532, `_assertRecoveryActionAllowed`:801, `_cleanupPartialSyncArtifacts`:817; logger/snapshot-type/marker-reader imports present), `MithrilController.ts`:494-495 handler lines byte-exact, `safeExitWithCode.ts`:4, and all three spec files' quoted titles/mock handles (`stateDirectoryPath` mocked `/tmp/daedalus-state`; marker mock primed `null` in `beforeEach`:144).
- Coverage complete: T5/T6/T8/T9/T10 each with quoted locate anchors, exact edits, and named jest tests; T14 recorded as verify-only no-op with the E3 tripwire. `this.getPartialSyncStatus()` in place of the channel export is a faithful circular-import-safe mechanic.
- No boundary violations (adoption copies backend-emitted snapshots only; `abandonCancel` keeps `failed`; no wipe restored into cancel; bootstrap untouched); no copy/i18n added; cross-section contracts honored; no task IDs in comments/test titles; no git/GitHub instructions.
- Two non-blocking nits recorded: Step 2B.3 loses the `finally`-guaranteed `_clearRuntimeWorkState()` on a listener-throw edge; Step 2B.4(c) re-declares a marker-mock handle the spec already exposes (`readMithrilPartialSyncMarkerMockTop`:99-101).
- Decision: approved — implementable as written; build not started.

## Planner (CAT-B per-section plan)

- Timestamp: 2026-07-03T17:07:02Z
- Wrote `task-ux-703-plan-cat-b.md` — mechanical steps for T11+N7 (isProbeFailed through type →
  service catch → controller spread → store → Diagnostics; three tooltip variants, 2 new EN+JA
  keys `…RecommendationNearTip`/`…RecommendationUnknown`), T12/T31 (swallow only on resynced
  `failed`; idle resync re-arms the prompt flag in `runInAction`), T15 (catch+resync+log on all
  three recovery actions; App.tsx onRetry wrap), T21 (new service `onChainDirectoryChanged()`
  hooked into `resetStartupGateOnDirectoryChange`), T28 (`Promise.all`, certified-epoch read kept
  after the join), T2 recorded as no-op. All 20+ anchors re-verified against the working tree;
  JA near-tip first sentence flagged as no-exact-table-precedent (derived from row 3 pattern).
- Outcome: CAT-B plan drafted to the small-model bar; build not started.

- Timestamp: 2026-07-03T17:11:45Z
- Critiquer for the CAT-B plan: single broad pass against the section brief with live-code
  spot-checks of 10+ quoted anchors (section early-return/fallback, service probe catch + return
  type + sequential awaits + `_invalidateBehindnessCaches`, store swallow block/flag comment/
  recovery actions, controller spread + `resetStartupGateOnDirectoryChange`, N7 block, App.tsx
  `onRetry`, availability type, locale keys, Recommendation messages, `handleDiskSpace.ts:444`
  wiring, spec helpers/test titles incl. `createLatestSnapshot` defaulting `certifiedEpoch: null`).
  All match byte-for-byte; coverage, boundaries, vocabulary, i18n (EN+JA + `yarn i18n:manage`),
  and cross-section contracts all hold. One cosmetic nit: step 4 cites "step 13" for the
  controller pass-through test that actually lives in step 18 (fully specified there).
- Decision: approved (nit is non-blocking; no plan amendment required).

## Planner (CAT-C section plan)

- Timestamp: 2026-07-03T17:02:09Z
- Wrote `task-ux-703-plan-cat-c.md` — per-section plan for CAT-C (T13 / DD-703-4): retry-once
  auto-finalize (2 s), local `finalizeFailed` state swapping the progress view for
  `MithrilErrorView` with a renderer-local retry, new
  `loading.mithrilPartialSync.error.finalizeFailed.{title,hint,retry}` keys EN+JA, two overlay
  spec tests, and an auto-playing storybook story. Brief anchors (`:59`, `:85-94`, `:91`) verified
  live — no drift; all edit steps locate by quoted content.
- JA flagged: title rendered with 後処理 (cleanup framing, copy-table rows 28–29) against EN
  "Finishing Mithril Sync failed"; "Try again" → 再試行 extrapolated from row 47.
- Escalation recorded: `dismissCompletedOverlay` intentionally never rejects today
  (`MithrilPartialSyncStore.spec.ts:301`), so the failure frame hardens the typed
  `onDismissCompleted` contract; plan stays overlay-only per DD-703-4 — no store change.
- Outcome: CAT-C plan written; build not started.

## Critiquer (CAT-C plan)

- Timestamp: 2026-07-03T17:06:01Z
- One broad pass over `task-ux-703-plan-cat-c.md` with anchor spot-checks against live code: overlay constant/effect/swallowed catch (`MithrilPartialSyncOverlay.tsx:59,85-94,91`) and every Step 6–9 locate block match verbatim; messages-file insertion point (`MithrilBootstrap.messages.ts:353-355`), EN/JA locale neighbors and alphabetical-sort claim, spec describe-close (`:357-361` — the prefix-scanning locale test sits in a separate describe, so the insertion point is correct), storybook interface/spread/tail anchors, `yarn i18n:manage` script, and Escalation 1's store/spec claims (`MithrilPartialSyncStore.ts:337`, swallow-pinning spec) all verified — no drift.
- Boundaries and contracts hold: 4000 ms auto-finalize trigger untouched (D-702a-1); retry action renderer-local on the same footing as the existing Quit fallback while `allowedRecoveryActions` membership stays backend-driven; `error={null}` keeps backend text out; no edits to shared views, bootstrap specs, or `partialSyncErrorCopy.ts`; N7/N4/T18 correctly deferred; JA copy derived from pinned copy-table rows; no task IDs in comments or test titles; tests/prettier/i18n commands all named. Overlay-only scope with the dead-path store-swallow nuance escalated, not silently absorbed — the right call.
- Decision: approved

## Planner (CAT-D per-section plan)

- Timestamp: 2026-07-03T17:06:22Z
- Wrote `task-ux-703-plan-cat-d.md` covering T7 (DD-703-5 delta formula via existing
  `_getPathSizeBytes`, explicit whole-snapshot fallback branch, 5 new + 2 pinned preflight unit
  tests) and T18 (append-only union member, exhaustive `COPY_BY_CODE` entry, EN+JA
  `insufficientDiskSpace` keys, copy-resolver spec row); cheat-sheet Low-disk row and JA copy-table
  rows included per DD-703-7. All anchors re-verified by content; edits shown verbatim.
- Flagged: thrown disk message is user-visible (`MithrilErrorView:130`) and said "partial sync" —
  reworded within the T7 rewrite (T16's list delegates that site here); context path threaded
  instead of re-calling `getManagedChainPath()` (avoids a re-forked probe); JA strings derived from
  copy-table patterns (no disk row exists) — native review welcome.
- Outcome: CAT-D plan written to the small-model bar; no source files modified.

- Timestamp: 2026-07-03T17:10:41Z
- Critiquer (CAT-D plan, `task-ux-703-plan-cat-d.md`): one broad pass against the DD-703-5 brief; spot-checked
  10+ quoted anchors live (service const/comment + `_assertSufficientDiskSpace` + `start()` call site, types
  union, `COPY_BY_CODE`, messages.ts adjacency, spec mocks/titles/`createContext`, locale :353 lines,
  cheat-sheet block, copy-table row 55) — all exact. Formula/fallback/floor and GB-figure consistency match
  the brief; union edit append-only; all five new tests' arithmetic re-derived correct; `safeLstat` confirms
  the Step-11 lstat→0-byte degradation; the sole other `_assertSufficientDiskSpace` reference (spec :1291,
  full mockImplementation) survives the signature change. Escalations 1–3 are legitimate flags, not open
  decisions; no boundary, vocabulary, i18n, or cross-section contract violations found.
- Decision: `approved` — CAT-D plan is autonomously implementable as written.

## Planner (CAT-E section plan)

- Timestamp: 2026-07-03T17:14:55Z
- Wrote `task-ux-703-plan-cat-e.md` — per-section mechanical plan for CAT-E (i18n/copy/vocabulary): chunk 1 = T16 stable error codes (7 union members appended; rejection-path throws use code-as-message since Electron structured clone drops custom Error props; prose → logger.warn; metadata stage errors gain `PARTIAL_SYNC_METADATA_UNAVAILABLE` + bespoke EN/JA copy; coordinator disabled/start/layout throws recoded) + T19 error-view body from intl maps only (raw message confined to collapsed details behind a localized "Technical details" header); chunk 2 = T1/T17 stage-id labels (verifying/converting/installing mapped + set membership), T4 shut down, T20/T32 ICU plurals, T22 shared `partialSyncStartFailureMessage` export/consumption, locale JSON + `yarn i18n:manage` + spec updates.
- Anchors re-verified live; drift corrected: disabled throw is a const at coordinator `:41-42` thrown at `:426` with an unused exported getter at `:590`; the "sole T22 seam" phrasing is per-file — the prompt's own fallback (`:151-154`) is CAT-F/T23 per the verification-grill log, left untouched.
- Escalations recorded: coordinator scope widened to all four start-path prose throws (LAYOUT_UNSUPPORTED example only matches the recovery-fallback throw) with an explicit narrow-back instruction; INSUFFICIENT_DISK_SPACE composition with CAT-D ordering; JA flagged as table-pattern-derived (「技術的な詳細」, metadata pair, stage labels, start-failure fallback) — native review suggested.
- Outcome: CAT-E plan written (idempotent overwrite); no source files modified; build not started.

- Timestamp: 2026-07-03T17:20:25Z
- Critiquer for the CAT-E plan (i18n/copy/vocabulary: T1/T17, T4, T16, T19, T20, T22, T32): spot-checked 20+ quoted
  anchors against live code — service throws (:180/:354/:556-568/:801-815/:903/:1082), coordinator guards + disabled
  export, MithrilErrorView :105/:130/:147-159, step-indicator maps + STRUCTURAL_IDS, section fallback :107, union tail,
  locale insertion points, spec test names/helpers — all match. Verified supporting claims: Logger accepts null data,
  CollapsibleSection renders children only when expanded (new overlay assertion valid), exit-code throw already coded,
  `yarn i18n:manage` exists, intl context available in MithrilPartialSyncSection, no bootstrap-spec assertions on the
  changed error surfaces. Coverage complete incl. explicit no-op list; boundaries 2/6/11 and T22/T23 + union
  append-only contracts honored; vocabulary clean ("certified immutable" reworded); JA flagged, table-patterned.
  Coordinator scope widening (start-path guards) is brief-grounded (LAYOUT_UNSUPPORTED example) and escalation-fenced.
- Decision: approved — build may start.

## Planner (CAT-F per-section plan)

- Timestamp: 2026-07-03T17:08:43Z
- Wrote `task-ux-703-plan-cat-f.md` covering T23/T24/T25/T29/T30/T3 (renderer) and T26/T27 (main),
  with every anchor re-verified against live code and all edits given verbatim.
- Key mechanics: T23 helper `utils/mithrilErrorMessage.ts` consumes CAT-E's fallback export
  (escalation if absent; empty-message Error is the store→component "use fallback" sentinel);
  T26 extracts the FULL duplicated promise pipeline (wider than the briefed spawn hunks, per
  approved item 26) with the POSIX detach comment and `detached: !environment.isWindows` preserved;
  T27 delegates the no-production-caller public methods to the raw pipeline (inert delta documented,
  caller-appears escalation); T3 confirmed a visual no-op on the shared bootstrap stylesheet.
- No new i18n keys in F; no source files modified during planning. Outcome: CAT-F plan ready-to-implement.

- Timestamp: 2026-07-03T17:11:56Z
- Critiquer for the CAT-F plan (T23–T27, T29/T30, T3): one broad pass; 11 quoted anchors spot-checked against the working tree, all verbatim — T26 dual spawn blocks + POSIX comments + stdinInput/origin-tag/env deltas; T24 inline array element-identical to the working-status list; T25 PROGRESS_STATUSES and its sole use; T23's three sites (both components carry contextTypes intl); T29/T30 declarations, writes, surviving backendElapsed; T3 scss import/include on the opaque backdrop; T27's four methods.
- T27 inert-delta claim independently confirmed: the live listSnapshots route resolves to MithrilBootstrapService via chainStorageCoordinator; the partial-sync service's showSnapshot has no production callers. New spec fixture shapes match the normalizer's key paths.
- Contracts honored: CAT-E fallback consumed with STOP-escalation if absent and no second fallback string (empty-message sentinel, component-side intl substitution); DD-703-6 detached + rationale comment preserved verbatim; boundary 11 guarded by content-identical .scss.d.ts regen with escalation; zero new i18n keys; comments/test names ID-free; deltas documented as intentional/inert with escalations.
- All cited existing test names and App.spec mock lines verified present; verification commands named per file; guardrails forbid gh/.gitignore/.agent-skills touches; zero unresolved implementer decisions.
- Decision: `approved` — no changes required.

## Planner (CAT-G section plan)

- Timestamp: 2026-07-03T17:06:04Z
- Wrote `task-ux-703-plan-cat-g.md` — the CAT-G per-section plan for review-body nits N1–N9
  (N1 recorded as DD-703-13 declined no-op; N7 deferred to CAT-B T11). All eight anchors
  re-verified against the working tree; zero drift found; every step locates by quoted content.
- Chunk 1: mechanical cleanups N2/N3/N5/N6/N8/N9 with verbatim edits, spec updates (controller
  mock → boolean getter, new `mithril-partial-sync.types.spec.ts`) and a fixtures-hoisted
  `snapshotFilesTotal`. Chunk 2: N4 `variant` flag per DD-703-14 — bootstrap defaults resolve
  the identical message descriptors, so the bootstrap completed frame stays byte-identical
  (boundary 11); overlay drops all 11 pre-formatted intl props; 3 new view-spec tests.
- No new i18n keys anywhere in CAT-G (N4 reuses existing EN+JA messages). Four
  escalate-on-mismatch triggers recorded for seams CAT-B/C/F edit first. Build not started.

- Reviewer: Critiquer — CAT-G plan (`task-ux-703-plan-cat-g.md`)
- Timestamp: 2026-07-03T17:06:35Z
- Spot-checked anchors against live code: N2 comment+dead paths (`mithrilSnapshotMetadata.ts:94-108`), N3 fabricated shape (`chainStorageCoordinator.ts:75-83`; type import has no other use), N4 overlay 11 props (`MithrilPartialSyncOverlay.tsx:177-234`) and view Props/guard/fallback frames (`MithrilProgressView.tsx`), N5 three idle literals, N6 static+dynamic import pair, N8 filter, N9 both story constants + fixture anchor — all match verbatim.
- Verified byte-identical-bootstrap claim independently: no consumer besides the overlay passes any of the 11 removed props; all referenced message ids exist in the messages file, en-US.json, and ja-JP.json; new spec regexes match actual EN copy; jest roots cover the new `source/common` spec; coordinator-spec `loadModule` + pinned-flag anchors and all four controller-spec mock anchors present.
- Contracts honored: N1 recorded as declined no-op (DD-703-13); N7 excluded (CAT-B T11); no touch of `PARTIAL_SYNC_DISABLED_ERROR` (CAT-E T16) or `mithrilCommandRunner.ts` (CAT-F T26); Step 1.5 escalation trigger covers the DD-703-10 `isProbeFailed` drift risk at the coordinator/controller seam.
- No new i18n, no locked-boundary violations (boundary 2/4 intact under N3; boundary 11 guarded by unmodified bootstrap tests under N4), no task-ID citations in comments/test titles; command table carries the Node-24/prettier env caveats.
- Issues: none.
- Decision: approved — ready to implement.

## Planner (T23 amendment after implementation escalation)

- Timestamp: 2026-07-03T19:23:44Z
- Conflict: the approved T23 had `getMithrilStartErrorMessage` PREFER the concrete extracted rejection message and anchored the spec test `keeps confirmation open and shows concrete start failure`; landed CAT-E formats `partialSyncStartFailureMessage` unconditionally, bans surfacing the raw rejection message, and rewrote that test — and post-T16 rejections carry stable codes as `error.message`, so "prefer concrete" would render raw codes like `PARTIAL_SYNC_DISABLED` as user copy.
- Binding decision encoded (not relitigated): code-to-copy mapping — the helper resolves a known code through `COPY_BY_CODE` via a new additive `resolvePartialSyncErrorCopyByCode` export on `partialSyncErrorCopy.ts`, rendering that entry's `title` at both one-line seams (hints reference overlay-only recovery affordances), otherwise the shared `partialSyncStartFailureMessage`; raw prose/code text never renders; the store keeps throwing via `toMithrilStartError` (extracted message or empty-string sentinel).
- Rewrote §1.1 in full with every anchor re-verified verbatim against the post-CAT-E tree (both catch blocks, `toStartError`, the copy map, the IPC rejection path, both landed spec tests); verified the landed section test stubs un-coded prose so it stays green UNMODIFIED (Step 1c pre-check + new Escalation 6); the prompt's concrete-prose test is replaced by a coded/un-coded pair asserting raw text absent.
- Zero new i18n keys (mapped copy reuses landed EN+JA descriptors, e.g. `loading.mithrilPartialSync.error.failed.title`); guardrails/i18n/verification/files-touched/T23 acceptance row/Escalations 1+5 updated in step; T24–T27, T29/T30, T3, and chunk 2 untouched; no source files modified during planning.

- Reviewer: Critiquer (T23 amendment re-check) — CAT-F plan (`task-ux-703-plan-cat-f.md`, §1.1 as amended 2026-07-03)
- Timestamp: 2026-07-03T19:28:18Z
- Decision-encoding verified against the binding orchestrator resolution: helper file exposes store normalization (`toMithrilStartError`, empty-message convention → component-side fallback) plus `getMithrilStartErrorMessage` mapping trimmed known codes through the new additive `resolvePartialSyncErrorCopyByCode` export to `title` copy (one field, all codes, hint-exclusion rationale documented) and otherwise the shared `partialSyncStartFailureMessage`; raw message/code text never returned; both catch seams routed; landed invariant comment kept at the copy module and restated in the helper; deltas (i)-(iv) and the T23 acceptance row encode coded-copy-at-both-seams / fallback-everywhere / no-raw-prose.
- Spot-checked all §1.1 anchors against the live post-CAT-E tree: `partialSyncErrorCopy.ts` fallback export + two-line comment verbatim, private `COPY_BY_CODE` with `PARTIAL_SYNC_DISABLED → FAILED.title = partialSyncFailedTitle`; both catch blocks, store `toStartError`, throw site, and all import anchors byte-identical to the plan's quotes; both components carry `contextTypes` with `intl` so the prompt seam's `this.context.intl` is valid; message ids and EN/JA locale values match (`startFailure` EN `Unable to start Mithril Sync.`, `failed.title` EN `Mithril Sync failed`); `Intl` type and `strict: false` confirm the Step 3 compile caveat; helper file absent today; sibling spec precedent exists.
- CAT-E test safety: Step 1c pre-check + Escalation 6 pin that the landed test `keeps confirmation open and shows the localized start-failure fallback` stubs prose outside the `MithrilPartialSyncErrorCode` union (verified: `'Mithril partial sync is disabled by launcher configuration.'` is not a member) and stays green UNMODIFIED; the new coded section test is purely additive after it. Prompt rewrite fully specified: existing test body matches the plan's REPLACE quote byte-for-byte; both replacement tests carry exact helpers, button labels, logger.warn signature, and locale literals verified in the tree; store-spec additions reuse the anchor test's exact payload shape (anchor present).
- Scope containment: amendment touches only §1.1 plus the declared feed lines (guardrails, Verification, Files touched, T23 acceptance row, Escalations 1/5/6, first Out-of-scope bullet); T24/T25/T26/T27/T29/T30/T3 and chunk 2 show no bleed; no second fallback string, zero new copy, no i18n edits, no task-ID citations in quoted comments/test titles, no locked-boundary violations, zero unresolved implementer decisions.
- Issues: none.
- Decision: approved — amended T23 is ready to implement.
