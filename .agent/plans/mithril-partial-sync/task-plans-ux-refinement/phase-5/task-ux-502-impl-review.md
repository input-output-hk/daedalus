# task-ux-502 — Implementation review log (append-only)

Append-only chronological transcript. Each entry: speaker, ISO-8601 UTC timestamp, outcome.

---

Implementation:
Speaker: Implementer
Timestamp: 2026-06-26T14:52:55Z
Outcome: Implemented PRD D8 (#13, #28, #35) Storybook coverage. Executed the approved canonical plan
(`task-ux-502.md`) step by step. Autonomous, stories-only; no component/i18n/scss/test edits. All
verification green (see below).

Root cause of the broken story (fixed): `Diagnostics.stories.tsx` wrapped `DaedalusDiagnostics` in a
ref-based `PartialSyncConfirmationStory` that called
`instance.setState({ isShowingMithrilPartialSyncConfirmation: true })`. That key exists nowhere —
`DaedalusDiagnostics` State is only `{ isNodeRestarting }` (`DaedalusDiagnostics.tsx:424-426`), and the
real confirmation state lives on `MithrilPartialSyncSection` as `isShowingConfirmation`
(`MithrilPartialSyncSection.tsx:31-34`). The modal therefore never opened. Fix routes through the real
seam: the class is deleted and the story renders
`<DaedalusDiagnostics {...baseProps} showMithrilPartialSyncConfirmationOnOpen />`, which flows
`showMithrilPartialSyncConfirmationOnOpen` (`DaedalusDiagnostics.tsx:414,731-733`) →
`MithrilPartialSyncSection.showConfirmationOnOpen` → `componentDidMount` → `showConfirmation()` →
`setState({ isShowingConfirmation: true })`. `baseProps` keeps `isMithrilBootstrapActive:false`
(→ `isActionBlocked:false`) so the open is not suppressed.

Changes (per file):
- `storybook/stories/nodes/status/Diagnostics.stories.tsx` —
  - Added 3 imports: `MithrilPartialSyncRecommendation`, `MithrilPartialSyncConfirmation` (both
    `../../../../source/renderer/app/components/status/`), `SyncingConnectingMithrilPrompt`
    (`.../components/loading/syncing-connecting/`).
  - Deleted the broken `PartialSyncConfirmationStory` class; the `Partial Sync Confirmation` story now
    renders `<DaedalusDiagnostics {...baseProps} showMithrilPartialSyncConfirmationOnOpen />`.
  - Added `storiesOf('Nodes / Status / Mithril Partial Sync Recommendation')`: Behind (CTA Ready)
    [isSynced=false], Synced [isSynced], Blocked [isActionBlocked].
  - Added `storiesOf('Nodes / Status / Mithril Partial Sync Confirmation')`: Known Epochs Behind
    [behindByEpochs=42], Unknown Behind [behindByEpochs omitted], Start Error [startError set,
    vocabulary-compliant "Mithril sync" wording].
  - Added `storiesOf('Nodes / Status / Mithril Proactive Prompt')`: Known Epochs Behind
    [behindByEpochs=120], Unknown Behind. `onStart` returns a Promise (`async () => { action(...)(); }`)
    so the confirm-view "Start now" await resolves like the real store call.
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` —
  - Added `getStoppingProgressItems(intl)` (all steps → pending), `getDownloadingProgressItems(intl)`
    (prepare completed, download active, rest pending), and selector
    `getProgressItemsForStory(intl, status, completed)`; all reuse `getActiveProgressItems(intl)` so
    labels are built at render (intl seam preserved — never captured at module scope).
  - Added 4 error fixtures: `downloadingError` (code `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED`),
    `convertingError` (`PARTIAL_SYNC_CONVERSION_FAILED`), `installingError`
    (`PARTIAL_SYNC_STAGED_DB_INVALID`), `finalizingError` (code `PARTIAL_SYNC_FINALIZE_FAILED` —
    intentionally NOT in `COPY_BY_CODE` and `finalizing` intentionally NOT in `COPY_BY_STAGE`
    per `partialSyncErrorCopy.ts:45-68`, so it exercises the generic FAILED title/hint fallthrough).
  - Swapped the factory's `progressItems` JSX to `getProgressItemsForStory(intl, props.status,
    props.completed)` (behavior-preserving for existing Active Progress/Completed) and removed the two
    now-unused locals (`activeProgressItems`, `completedProgressItems`).
  - Appended 6 stories: Downloading File Count (status=downloading, 4/9 files), Stopping Node
    (status=stopping-node → populated completion block + Cancel disabled with stopping tooltip), and
    Failed - Downloading/Converting/Installing/Finalizing (All Recovery Actions), each status=failed
    with canRetry+canRestartNormally+canWipeAndFullSync (full 3-action layout). Cancelled-vs-failed =
    the existing Cancelled story (same can* flags, cancelled copy) vs the new Failed-* stories.

Locked invariants honored: (1) locale-at-render seam preserved — every story pulls intl from context
at render via the factory `contextTypes` / each component's `static contextTypes={intl}`; the
auto-applied `StoryWrapper` (`StoryWrapper.tsx:70-82`) remounts on locale via `key={locale}`. (2)
Recovery actions render strictly from the can* booleans (`MithrilPartialSyncOverlay.tsx:79-118`);
3-action stories set all three true; no 4th action fabricated; the defensive Quit path is left to the
existing no-action fixtures. (3) Confirm-precedes-start — fix uses the real state seam, not a 2nd start
path. (4) Stopping-node keeps the component's real `actionDisabled`+tooltip. (5) No bootstrap/shared-
component edits (MithrilProgressView/MithrilStepIndicator consumed only via the overlay factory);
bootstrap file-count coverage already lives in MithrilBootstrap/MithrilProgressView stories. (6) No new
i18n keys — all referenced runtime keys already populated in en-US.json + ja-JP.json with zero `!!!`
placeholders (re-verified by grep). Vocabulary: new story prose uses "Mithril sync", epochs (not %),
no immutable-file references.

Verification (Node v24.16.0; scss `.d.ts` regenerated via the typed-scss-modules script — env caveat,
not a regression):
- `yarn typedef:sass` (typed-scss-modules) — PASS (exit 0; `.scss.d.ts` regenerated, gitignored).
- `yarn compile` (`precompile: typedef:sass` then `tsc --noEmit`) — PASS (exit 0, clean). Whole-project
  typecheck confirms the shared bootstrap stories (MithrilBootstrap/MithrilProgressView) still
  typecheck.
- `yarn eslint` (scoped to the 2 changed files) — PASS (exit 0). 1 warning at
  `Diagnostics.stories.tsx:76:6` (`} as any;`) is PRE-EXISTING on the untouched `baseProps` const, not
  introduced here.
- `prettier --check` (scoped to the 2 changed files via the binary, avoiding the yarn script's default
  globs over unrelated pre-existing `tests/` files) — PASS (exit 0, "All matched files use Prettier
  code style"). `Diagnostics.stories.tsx` was `prettier --write`-formatted (one long confirmation-story
  JSX line wrapped) before the final clean re-check.
- `yarn storybook` / `yarn dev` — NOT run (they hang, per process rules). `storybook:build` skipped
  (time-boxed/optional; compile+lint+prettier cover correctness).

Deviations: none of substance. (a) Error-fixture `logPath` values reuse the existing fixtures'
`mithril-partial-sync.log` / `cardano-node.log` convention for consistency (filesystem artifact names,
not prose copy); new prose `message` strings are vocabulary-compliant ("Mithril sync", no "partial
sync"). (b) Confirmation "Start Error" story uses a vocabulary-compliant fixture string rather than the
component's internal fallback literal (which is owned by copy task task-ux-601, out of scope).

Handoff: ready for impl review. Scribe to confirm the timestamp and flip
`mithril-partial-sync-ux-refinement-tasks.json` task-ux-502 status → `completed` (+completedAt).

---

Code-Review:
Speaker: Code-Review (REQUIRED gate, iteration 1)
Timestamp: 2026-06-26T15:02:02Z
Outcome: One broad pass over the working-tree diff (`git diff`) vs the approved plan. APPROVED. Findings:

Broken story fix — VERIFIED correct. The ref-based `PartialSyncConfirmationStory` (which set the
non-existent `isShowingMithrilPartialSyncConfirmation` key) is deleted; the `Partial Sync Confirmation`
story now renders `<DaedalusDiagnostics {...baseProps} showMithrilPartialSyncConfirmationOnOpen />`.
Confirmed the seam against live code: `DaedalusDiagnostics.tsx:414` declares the prop, `:731-733`
passes it to `MithrilPartialSyncSection.showConfirmationOnOpen` → `componentDidMount` (line 47-52) →
`showConfirmation()` → `setState({ isShowingConfirmation: true })` (the real state key,
`MithrilPartialSyncSection.tsx:31-34`). `baseProps` keep `isMithrilPartialSyncWorking:false` and
`isMithrilBootstrapActive:false`, so `isMithrilActionBlocked` (`DaedalusDiagnostics.tsx:581-582`) is
false and the open is not suppressed. Modal now renders.

Coverage — COMPLETE. Recommendation (Behind/Synced/Blocked); Confirmation modal (Known Epochs Behind
[behindByEpochs=42] / Unknown Behind / Start Error) plus the real-seam diagnostics story; Proactive
Prompt (Known/Unknown, `onStart` returns a Promise); all four error stages
(downloading/converting/installing/finalizing) each with the full retry+restart-normally+wipe 3-action
layout; cancelled-vs-failed (existing Cancelled story, cancelled copy, vs the new Failed-* at the same
can* flags); partial-sync file-count (Downloading File Count 4/9); populated Stopping Node frame.
Bootstrap file-count coverage correctly left to MithrilBootstrap/MithrilProgressView stories (shared
component, invariant 5).

Props/types — VERIFIED against live components. Recommendation/Confirmation/Prompt prop shapes match
fixtures. `finalizingError.code='PARTIAL_SYNC_FINALIZE_FAILED'` typechecks because
`MithrilPartialSyncError.code` is `code?: string` (mithril-partial-sync.types.ts:41), not the narrow
union; and it is absent from both COPY_BY_CODE and COPY_BY_STAGE (`partialSyncErrorCopy.ts:46-68`), so
it exercises the generic FAILED fallthrough as intended. All `stage`/`status` values used are valid
union members.

Locked invariants — NO regressions. (1) locale-at-render seam preserved: every story pulls intl from
context at render (factory/component `contextTypes`); module-scope `*BaseProps` hold only
callbacks/booleans/raw values (`formattedSyncPercentage:'62.5'`), never formatted/translated strings;
progress labels built via `getActiveProgressItems(intl)` at render. (2) recovery actions render strictly
from can* booleans (`MithrilPartialSyncOverlay.tsx:79-118`); 3-action stories set exactly the three;
no 4th fabricated. (3) confirm-precedes-start via the real state seam. (4) stopping-node keeps the
component's real `actionDisabled` + tooltip. (5) BOOTSTRAP REGRESSION check: `git diff --name-only`
shows ZERO `source/` edits — only the 2 story files (+ pre-existing unrelated `.gitignore`); shared
MithrilProgressView/MithrilStepIndicator untouched and consumed only via the overlay factory. (6) no
new i18n keys — re-verified by grep that every referenced mithrilPartialSync*/mithrilProactivePrompt*/
section-label key and the overlay error/tooltip ids (`loading.mithrilPartialSync.error.*`,
`...progress.cancelStoppingTooltip`, `...error.failed.*`) are populated in both en-US.json and ja-JP.json
with ZERO `!!!` placeholders.

Vocabulary — COMPLIANT for strings this task introduces (Start Error fixture and overlay `message`
fixtures use "Mithril sync"/"Mithril data"/"chain data", epochs not %, no immutable-file references).
The pre-existing component/catalog "Mithril partial sync" copy is out of scope (owned by task-ux-601).

Complexity — appropriate; the 2 new progress helpers + `getProgressItemsForStory` selector reuse
`getActiveProgressItems` with no duplication or over-engineering.

Verification — RE-RUN, genuinely green (Node v24, scss-typedef caveat honored):
`yarn typedef:sass` exit 0; `tsc --noEmit` exit 0 (clean, whole-project — bootstrap stories still
typecheck); eslint scoped to the 2 files exit 0 (only the PRE-EXISTING `@typescript-eslint/no-explicit-any`
warning at `Diagnostics.stories.tsx:76:6`, the untouched `baseProps` `} as any;` close — not introduced
here); `prettier --check` scoped to the 2 files exit 0 ("All matched files use Prettier code style").

Non-blocking notes for scribe/orchestrator: (a) `task-ux-502-research.md` is not yet created and the
single task-commit has not been made — both correctly deferred to the scribe/commit step per the 501
precedent; not code defects. (b) The pre-existing `.gitignore` change must NOT be committed.

Decision: approved

---
