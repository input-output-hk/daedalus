# task-ux-502 — Planning Review Log

Append-only chronological transcript. Each entry: speaker, ISO-8601 UTC timestamp, outcome.
Critiquer entries end with `Decision: approved | requires_changes`.

---

## Planner
Timestamp: 2026-06-26T14:36:14Z
Outcome: Plan drafted (small-model-implementable). Awaiting Critiquer pass.

Verified every anchor against live files (see the canonical doc's "Verified live state" table):
- A/B/C: the broken story (`Diagnostics.stories.tsx:75-87`) sets `isShowingMithrilPartialSync
  Confirmation` on the `DaedalusDiagnostics` instance, whose only state is `isNodeRestarting`
  (`DaedalusDiagnostics.tsx:424-426`). The real key is `isShowingConfirmation`
  (`MithrilPartialSyncSection.tsx:31-34`), reachable through the supported prop
  `showMithrilPartialSyncConfirmationOnOpen` → `showConfirmationOnOpen` → `componentDidMount` →
  `showConfirmation()` (`DaedalusDiagnostics.tsx:414,731-733`; `MithrilPartialSyncSection.tsx:
  26,49-51`). `baseProps` keep `isActionBlocked:false`, so the open is not suppressed. Fix = pass
  the prop on `DaedalusDiagnostics` and delete the ref-hack class (no second start path → confirm-
  precedes-start invariant honored).
- E/F/G: confirmed props for the three isolated components
  (`MithrilPartialSyncRecommendation` `{formattedSyncPercentage,isActionBlocked,isSynced,
  onShowConfirmation}`; `MithrilPartialSyncConfirmation` `{isActionBlocked,startError,behindByEpochs?,
  formattedSyncPercentage,onCancel,onConfirm}`; `SyncingConnectingMithrilPrompt`
  `{behindByEpochs?,onStart():Promise<void>,onDismiss()}`). All three carry
  `static contextTypes={intl}`, so they honor the global locale switch.
- H/I/J/K/L/M: the overlay factory already resolves `intl` from context at render
  (`MithrilPartialSyncOverlay.stories.tsx:113-114,140-142`) and renders recovery actions strictly
  from `can*` booleans (`MithrilPartialSyncOverlay.tsx:79-118`). Error copy resolves per
  `partialSyncErrorCopy.ts:46-88` — downloading/converting/installing map to bespoke copy, and
  **finalizing intentionally falls back to generic FAILED** (recorded so it is not "fixed"). The
  stopping-node completion block renders from `status==='stopping-node'`
  (`MithrilProgressView.tsx:99-100,166-187`) with Cancel disabled + tooltip in the overlay.
- N: the locale-at-render-time seam is the global `StoryWrapper` `<IntlProvider key={locale}>`
  (`StoryWrapper.tsx:70-82`) — `key={locale}` forces remount on switch; the plan reuses it by
  building all labels at render via the factory's `intl` and the components' own intl, never at
  import. Two new helpers (`getStoppingProgressItems`, `getDownloadingProgressItems`) only remap
  `state` over `getActiveProgressItems(intl)`, preserving the seam.

Coverage decisions:
- Stories split by domain: Diagnostics file gets recommendation + confirmation + proactive prompt
  (decision surfaces, diagnostics i18n namespace); overlay file gets the file-count progress,
  stopping-node frame, and the four failed error-stage 3-action stories. cancelled-vs-failed is
  satisfied by contrasting the existing `Cancelled` story with the new `Failed - *` stories at the
  same full 3-action layout.
- Bootstrap not regressed: no component and no bootstrap story touched; the Step B3 factory swap is
  behavior-preserving for the existing `Active Progress`/`Completed` stories; the "unified" file-
  count requirement is met by adding the partial-sync side while bootstrap coverage stays in
  `MithrilBootstrap.stories.tsx`/`MithrilProgressView.stories.tsx`.
- Confirmed no new i18n keys are needed: all referenced `mithrilPartialSync*` /
  `mithrilProactivePrompt*` runtime keys are already populated in en-US.json and ja-JP.json with
  zero `!!!` placeholders.

Plan written to the small-model bar: exact files, exact line anchors, exact code blocks for every
add/delete, the locale-at-render invariant inline, the recovery-action and bootstrap-safety
invariants inline, and a non-interactive verification plan (yarn typedef:sass → compile → lint →
prettier:check; never run storybook/dev).

---

## Critiquer
Timestamp: 2026-06-26T14:44:21Z
Outcome: Broad single-pass review of the plan and canonical doc against live code. Every load-bearing
anchor re-verified by reading the source; the plan is accurate, complete, and small-model
implementable. No blockers.

Verified live (re-read, not trusted):
- **Broken-state-key root cause — CONFIRMED.** `Diagnostics.stories.tsx:75-87` `class
  PartialSyncConfirmationStory` calls `instance.setState({ isShowingMithrilPartialSyncConfirmation:
  true })` on the `DaedalusDiagnostics` ref; that component's only state is `isNodeRestarting`
  (`DaedalusDiagnostics.tsx:424-426`), so the key is inert and the modal never opens. The real key is
  `isShowingConfirmation` on `MithrilPartialSyncSection` (`:31-34`). Registration is at line 95. Class
  is referenced ONLY at line 95 → safe to delete.
- **Fix seam — CONFIRMED truthful.** `DaedalusDiagnostics` declares
  `showMithrilPartialSyncConfirmationOnOpen?` (`:414`), destructures it (`:527`), forwards it as
  `showConfirmationOnOpen` (`:731-733`); `MithrilPartialSyncSection.componentDidMount` (`:47-52`)
  calls `showConfirmation()` when `showConfirmationOnOpen && !isActionBlocked`. `baseProps` give
  `isMithrilPartialSyncWorking:false` + `isMithrilBootstrapActive:false` → `isMithrilActionBlocked`
  (`:581-582`) false → open not suppressed. Remount on locale switch re-fires componentDidMount, so
  the modal re-opens per locale — seam-safe. No second start path → confirm-precedes-start honored.
- **Component prop shapes — CONFIRMED** for `MithrilPartialSyncRecommendation` (`:43-48`),
  `MithrilPartialSyncConfirmation` (`:89-96`), `SyncingConnectingMithrilPrompt` (`:70-74`,
  `onStart(): Promise<void>`; the `async () => action(...)()` fixture satisfies it). All three carry
  `static contextTypes = { intl }`; the prompt's fast button → `showConfirmation` (view='confirm'),
  `handleStart` is the sole `onStart` caller → confirm precedes start (`:105-143,180-181`).
- **Error fixtures / copy resolution — CONFIRMED, incl. the intentional gap.**
  `MithrilPartialSyncStatus` and `MithrilPartialSyncErrorStage` unions include `stopping-node`,
  `downloading`, `converting`, `installing`, `finalizing` (`mithril-partial-sync.types.ts:3-30`).
  `MithrilPartialSyncError.code` is typed `string` (`:41`), so `PARTIAL_SYNC_FINALIZE_FAILED`
  (absent from the `MithrilPartialSyncErrorCode` union) compiles. `partialSyncErrorCopy.ts`: download/
  conversion/staged-db codes map via `COPY_BY_CODE` (`:46-55`); `finalizing` is in NEITHER
  `COPY_BY_CODE` nor `COPY_BY_STAGE` (`:60-68`) → falls through to generic `FAILED` (`:87`). Plan's
  "intentionally unmapped" note and its `:57-68` anchor are correct — do not "fix".
- **Recovery-action + stopping-node invariants — CONFIRMED.** Overlay builds recovery actions
  strictly from `canRetry/canRestartNormally/canWipeAndFullSync` (`:79-118`); 3-action stories set all
  three true; defensive Quit only when none (`:119-131`) — left to no-action fixtures. stopping-node:
  `actionDisabled = status==='stopping-node'` + tooltip (`:184-191`). Invariants 2 & 4 honored.
- **Locale-at-render seam — CONFIRMED.** New helpers `getStoppingProgressItems`/
  `getDownloadingProgressItems`/`getProgressItemsForStory` only remap `state` over
  `getActiveProgressItems(intl)`; the factory pulls `intl` from context (`:113-114,140-142`). No
  formatted strings captured at module scope.
- **No new i18n keys / zero placeholders — CONFIRMED in BOTH catalogs.** `loading.mithrilPartialSync.*`
  = 27 keys, 0 `!!!`; `mithrilProactivePrompt*` = 9, 0 `!!!`; `mithrilPartialSyncConfirmation*` = 12,
  0 `!!!`; recommendation/button + bootstrap step labels populated (already rendered by existing
  stories). en-US and ja-JP both clean.
- **Bootstrap non-regression + "both file-count flows" — CONFIRMED.** No component or bootstrap-story
  edit. Bootstrap file-count coverage already exists (`MithrilBootstrap.stories.tsx:118-121`,
  `MithrilProgressView.stories.tsx:71-100`, both with `filesDownloaded`/`filesTotal` at status
  `downloading`); the plan adds the partial-sync side (`Downloading File Count`), so the unified
  both-flows requirement is genuinely met. Step B3 swap is behavior-preserving for `Active Progress`
  (converting→active set) and `Completed` (props.completed→completed set).
- **B3 unused-locals are NOT a hazard.** `tsconfig.json` has `noUnusedLocals` commented out (line 88);
  `.eslintrc` sets `no-unused-vars: "warn"` and the `lint` script has no `--max-warnings 0`. Leaving
  `activeProgressItems`/`completedProgressItems` after the swap neither fails `tsc` nor `lint`; the
  plan's conditional-delete note is harmless either way.
- **Governance/scribe wiring — CONFIRMED.** Canonical docs live under
  `task-plans-ux-refinement/phase-5/` mirroring `task-ux-501.*` (the plan's doc-location note is
  correct; the prompt's flat path is stale). `tasks.json` `task-ux-502` is `status: pending`,
  `completedAt: null`, deps `[302,303,304,401,402,403,404]` — scribe flip is correct and accurate.

Coverage vs acceptance criteria: broken confirmation fix (A2); recommendation (A3 Behind/Synced/
Blocked); confirmation modal (A4 Known/Unknown/StartError + fixed integration); proactive prompt (A5
Known/Unknown); all four error stages with full 3-action layout (B4); cancelled-vs-failed (existing
Cancelled vs new Failed-* at same layout); file-count for both flows; populated stopping-node frame
(B4). All present and truthful.

Non-blocking observations (no change required):
1. The fixed integration confirmation story (A2) derives `behindByEpochs = max(1, 513-512) = 1` from
   `baseProps` tips, so it reads "about 1 epochs behind" (awkward singular). Truthful; the isolated A4
   stories carry the rich `behindByEpochs=42`/unknown copy, so primary coverage is unaffected.
2. B3's "leave or delete the now-unused locals" is genuinely optional (verified above); could be
   stated as a single deterministic instruction, but not required.

Decision: approved
