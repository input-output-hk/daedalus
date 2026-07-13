# task-ux-502 — Research notes

Captured during implementation of Storybook coverage for the recommendation, confirmation modal,
proactive prompt, progress, and error states (PRD D8: #13 stories, #28 progress states, #35
locale-at-render fixture seams). Stories-only; no component/i18n/scss/test edits.

## Broken-state-key root cause (the bug this task fixed)
- `Diagnostics.stories.tsx` (pre-fix, ~lines 75-87) wrapped `DaedalusDiagnostics` in a ref-based
  `class PartialSyncConfirmationStory` whose `setDiagnosticsRef` called
  `instance.setState({ isShowingMithrilPartialSyncConfirmation: true })`.
- That key exists **nowhere**. `DaedalusDiagnostics` State is only `{ isNodeRestarting }`
  (`DaedalusDiagnostics.tsx:424-426`), so the `setState` was inert and the confirmation modal **never
  rendered**.
- The real confirmation state lives one component down: `isShowingConfirmation` on
  `MithrilPartialSyncSection` (`MithrilPartialSyncSection.tsx:31-34`).
- **Fix (truthful seam, not a second start path):** delete the ref-hack class and render
  `<DaedalusDiagnostics {...baseProps} showMithrilPartialSyncConfirmationOnOpen />`. The prop flows
  `showMithrilPartialSyncConfirmationOnOpen` (`DaedalusDiagnostics.tsx:414`, forwarded `:731-733`) →
  `MithrilPartialSyncSection.showConfirmationOnOpen` → `componentDidMount` (`:47-52`) →
  `showConfirmation()` → `setState({ isShowingConfirmation: true })`. The open is gated by
  `!isActionBlocked`; `baseProps` keep `isMithrilPartialSyncWorking:false` +
  `isMithrilBootstrapActive:false` → `isMithrilActionBlocked` (`DaedalusDiagnostics.tsx:581-582`)
  false → not suppressed. On locale switch the `StoryWrapper` remount re-fires `componentDidMount`,
  so the modal re-opens per locale — seam-safe.

## Story-coverage seams (decision-surface split)
- **Two target files, three decision surfaces grouped by domain.** `Diagnostics.stories.tsx` (the
  diagnostics-namespace decision surfaces) hosts the recommendation, the confirmation modal, and the
  proactive prompt. `SyncingConnectingMithrilPrompt` physically lives under
  `loading/syncing-connecting/`, but its i18n is in the `daedalus.diagnostics.dialog.mithrilProactive
  Prompt*` namespace and it is a partial-sync decision surface, so grouping it with the other two in
  the Diagnostics stories file keeps file + storybook title coherent within the task's two-file
  constraint.
- `MithrilPartialSyncOverlay.stories.tsx` hosts the in-session progress + error states: file-count
  progress, the populated stopping-node frame, and the four failed error-stage 3-action stories.
- Isolated component prop shapes (verified): `MithrilPartialSyncRecommendation`
  `{ formattedSyncPercentage, isActionBlocked, isSynced, onShowConfirmation }`;
  `MithrilPartialSyncConfirmation`
  `{ isActionBlocked, startError, behindByEpochs?, formattedSyncPercentage, onCancel, onConfirm }`;
  `SyncingConnectingMithrilPrompt` `{ behindByEpochs?, onStart(): Promise<void>, onDismiss() }`. All
  three carry `static contextTypes = { intl }`, so each honors the global locale switch on its own.
- `SyncingConnectingMithrilPrompt.onStart` is `(): Promise<void>`; the story fixture
  `async () => action('onStart')()` satisfies the signature, and the prompt's fast button reveals the
  internal confirm view in-story (confirm precedes start — no second start path).

## Locale-at-render seam (#35) — location and reuse
- The seam is the global `StoryWrapper`: `<IntlProvider key={locale} messages={translations[locale]}>`
  + `ThemeManager` (`storybook/stories/_support/StoryWrapper.tsx:70-82`). The `key={locale}` forces a
  full remount on locale switch, so any component pulling `intl` at render re-renders translated.
- **Reuse rule honored:** never capture `intl`/formatted strings at module load. The overlay factory
  pulls `intl` from context (`MithrilPartialSyncOverlay.stories.tsx:113-114,140-142`); the new
  helpers `getStoppingProgressItems` / `getDownloadingProgressItems` / `getProgressItemsForStory`
  only remap the `state` field over `getActiveProgressItems(intl)`, so labels are still built fresh at
  render. Module-scope `*BaseProps` hold only callbacks/booleans/raw values
  (e.g. `formattedSyncPercentage: '62.5'`), never translated strings.

## Error-copy resolution table (`partialSyncErrorCopy.ts:46-88`) — incl. the intentional gap
Copy resolves in order: (1) `status === 'cancelled'` short-circuits to CANCELLED; (2) by `error.code`
via `COPY_BY_CODE`; (3) by `error.stage` via `COPY_BY_STAGE`; else (4) generic FAILED.

| Story fixture | stage | code | Resolves to |
|---|---|---|---|
| `downloadingError` | downloading | `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED` | DOWNLOAD_FAILED (by code) |
| `convertingError` | converting | `PARTIAL_SYNC_CONVERSION_FAILED` | CONVERSION_FAILED (by code) |
| `installingError` | installing | `PARTIAL_SYNC_STAGED_DB_INVALID` | STAGED_DB_INVALID (by code) |
| `finalizingError` | finalizing | `PARTIAL_SYNC_FINALIZE_FAILED` | **generic FAILED (fallthrough)** |

- **`finalizing` is intentionally absent from both `COPY_BY_CODE` and `COPY_BY_STAGE`** → it falls
  through to the generic FAILED title/hint. This is the correct, intended behaviour; the Failed -
  Finalizing story demonstrates it. **Do not "fix" it.**
- Type note: `MithrilPartialSyncError.code` is `code?: string`
  (`mithril-partial-sync.types.ts:41`), not the narrow `MithrilPartialSyncErrorCode` union, so
  `PARTIAL_SYNC_FINALIZE_FAILED` typechecks even though it is not a union member. All `stage`/`status`
  values used in fixtures are valid union members (`stopping-node`, `downloading`, `converting`,
  `installing`, `finalizing` all present in the unions).

## Recovery-action + stopping-node invariants (consumed, not changed)
- The overlay builds recovery actions strictly from `canRetry` / `canRestartNormally` /
  `canWipeAndFullSync` (`MithrilPartialSyncOverlay.tsx:79-118`); the full-3-action stories set exactly
  those three true. The defensive **Quit** button appears only when *no* recovery action is available
  (`:119-131`) — left to the existing no-action fixtures, not the 3-action stories.
- Cancelled-vs-failed contrast is achieved without a new cancelled story: the existing **Cancelled**
  story (CANCELLED copy, same `can*` layout) vs the new **Failed-*** stories (stage-specific/generic
  copy, same layout). The differing copy at the same layout is the deliverable.
- Stopping-node: `status === 'stopping-node'` renders the populated completion block
  (`MithrilProgressView.tsx:99-100,166-187`); the overlay disables Cancel + shows the stopping tooltip
  (`MithrilPartialSyncOverlay.tsx:184-191`).

## Bootstrap non-regression (shared components)
`MithrilProgressView` / `MithrilStepIndicator` are shared by bootstrap and partial-sync. This task
edits **no** component and **no** bootstrap story — they are consumed only via the partial-sync
overlay factory. Bootstrap file-count coverage already lives in `MithrilBootstrap.stories.tsx` and
`MithrilProgressView.stories.tsx` and was left untouched. The "unified file-count for both flows"
requirement is met by adding the partial-sync side (`Downloading File Count`) while the bootstrap side
stays as-is. The Step B3 selector swap is behaviour-preserving for the existing `Active Progress`
(converting → active set) and `Completed` (props.completed → completed set) stories.

## No new i18n keys / zero placeholders (verified in BOTH catalogs)
All referenced `mithrilPartialSync*` / `mithrilProactivePrompt*` / overlay error+tooltip runtime keys
are already populated in `en-US.json` and `ja-JP.json` with **zero `!!!` placeholders**
(re-verified by grep at implementation and code-review). This task introduces no new key.

## Scribe finalization (2026-06-26)
No new research surfaced at finalize beyond the above. Plan-review APPROVED (Critiquer
2026-06-26T14:44:21Z) and impl-review APPROVED (Code-Review 2026-06-26T15:02:02Z).
`mithril-partial-sync-ux-refinement-tasks.json` `task-ux-502` → `completed`, `completedAt`
`2026-06-26T15:03:51Z`. Durable findings carried forward: the broken-state-key root cause and the
real `showMithrilPartialSyncConfirmationOnOpen` seam; the decision-surface story split; the
locale-at-render seam location; and the error-copy resolution table with the intentional finalizing →
generic FAILED fallback.
