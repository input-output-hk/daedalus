# task-ux-502 — Storybook coverage for recommendation, confirmation modal, prompt, progress, and error states

## Task id + title
`task-ux-502` — "Storybook coverage for recommendation, confirmation modal, prompt, progress, and error states".

## Why now
Lowest-ID unblocked pending task; all deps completed (`task-ux-302`, `303`, `304`, `401`,
`402`, `403`, `404`). Implements PRD **D8** gaps **#13, #28, #35**. Stories-only — the feature
code and focused tests already shipped in prior tasks.

## Interaction mode
`autonomous` — stories only. No user-blocking decision. Copy approval is `task-ux-601` (out of
scope). This task introduces **no** user-facing copy and **no** new i18n keys.

## Scope
Storybook-only, two files:
- **Fix** the broken "Partial Sync Confirmation" story so the confirmation modal actually renders.
- **Add isolated stories** for: the recommendation, the confirmation modal, the proactive prompt,
  every error stage (downloading / converting / installing / finalizing) with the full
  retry + restart + wipe 3-action layout, cancelled-vs-failed, and the unified bootstrap +
  partial-sync file-count progress including the populated stopping-node frame.
- Reuse the existing **locale-at-render-time fixture seams** so every new story renders truthfully
  under en-US and ja-JP and both themes.

## Non-goals / scope guard
- **No** new i18n keys, **no** scss changes, **no** component/Jest/Cucumber tests (those live with
  the feature tasks; `task-ux-503` owns cross-cutting automated coverage).
- **No** user-facing copy changes (that is `task-ux-601`). Do not edit any `defineMessages` block.
- **No** changes to the feature components themselves (`MithrilPartialSyncOverlay`,
  `MithrilProgressView`, `MithrilStepIndicator`, `MithrilPartialSyncSection/Recommendation/
  Confirmation`, `SyncingConnectingMithrilPrompt`, `DaedalusDiagnostics`) — stories consume them.
- **Do NOT** `git add -A`; do **not** commit the pre-existing `.gitignore` change.

## Locked invariants this change must not break (inline)
- **Locale-at-render-time seam:** the storybook `StoryWrapper` wraps every story in
  `<IntlProvider key={locale} messages={translations[locale]}>` (`storybook/stories/_support/
  StoryWrapper.tsx:70-82`) — the `key={locale}` forces a full remount on locale switch. Stories
  must therefore pull `intl` at **render time** (via the story factory's `contextTypes.intl`, or via
  each component's own `static contextTypes = { intl }`), never capture `intl`/formatted strings at
  module load. Keep this pattern; do not precompute labels at import.
- **Recovery actions render strictly from `allowedRecoveryActions`** (here the
  `canRetry`/`canRestartNormally`/`canWipeAndFullSync` booleans) — never inferred from status names
  (`MithrilPartialSyncOverlay.tsx:79-118`). The "full 3-action layout" stories set all three `can*`
  true; do not fabricate a 4th action. The defensive **Quit** button only appears when *no* recovery
  action is available (`:119-131`) — leave that path to the existing/Quit-only fixtures, not the
  3-action stories.
- **Confirmation precedes start.** The fixed confirmation story must reach the modal through the real
  state seam (`showConfirmationOnOpen` → `componentDidMount` → `showConfirmation()` →
  `isShowingConfirmation: true`), not a second start path.
- **Cancellation forbidden after cutover / disabled during stop.** The stopping-node story must keep
  the overlay's real `actionDisabled` + tooltip behaviour (driven by `status === 'stopping-node'`).
- **No synthetic throughput / remaining-time / overall-%.** Progress fixtures use only file counts +
  elapsed seconds, matching the existing `transferProgress` shape.
- **Do NOT regress the empty-chain Mithril bootstrap flow.** `MithrilProgressView` and
  `MithrilStepIndicator` are shared by bootstrap and partial-sync. This task edits **no** component
  and **no** bootstrap story; the bootstrap file-count + progress coverage already lives in
  `MithrilBootstrap.stories.tsx` and `MithrilProgressView.stories.tsx` and must be left untouched and
  passing. The "unified" requirement is satisfied by adding the partial-sync side (overlay file)
  while the bootstrap side stays as-is.
- **Vocabulary:** this task writes no user copy, but any new story title/label text must honor the
  glossary (user-facing copy is "Mithril Sync" / "standard sync", never "partial sync"; behind-ness
  in epochs only; never "immutable files"). Story titles in storybook navigation are dev-facing, so
  internal "Partial Sync" group names are acceptable; do not introduce user-facing copy.

## Dependencies
`task-ux-302`, `task-ux-303`, `task-ux-304`, `task-ux-401`, `task-ux-402`, `task-ux-403`,
`task-ux-404` — all `completed`.

## Research / docs / workflows / skills consulted
- PRD: `mithril-partial-sync-ux-refinement-prd.md` D8 (#13 stories, #28 progress states, #35
  locale-at-render fixture seams).
- Tasks JSON entry `task-ux-502` (lines 631-658).
- Workflows: `.agent/workflows/storybook.md`, `.agent/workflows/frontend.md`; skill
  `storybook-creation`.
- Memory: renderer/scss verify env (Node v24 needs `typed-scss-modules` regen + identity-obj-proxy
  jest sidecar — treat those as env setup, not regressions).

---

## Verified live state (re-verify before editing — line numbers may drift)

| Anchor | File:line | Live fact |
|---|---|---|
| A | `Diagnostics.stories.tsx:75-87` | `class PartialSyncConfirmationStory` uses a `ref` to call `instance.setState({ isShowingMithrilPartialSyncConfirmation: true })` on the **`DaedalusDiagnostics`** instance. That component's only state is `isNodeRestarting` (`DaedalusDiagnostics.tsx:424-426`); the key does not exist anywhere → the modal never opens. **This is the bug.** |
| A' | `Diagnostics.stories.tsx:95` | registered via `.add('Partial Sync Confirmation', () => <PartialSyncConfirmationStory />)`. |
| B | `MithrilPartialSyncSection.tsx:31-34` | real state key is **`isShowingConfirmation`** (not `isShowingMithrilPartialSyncConfirmation`). |
| B' | `MithrilPartialSyncSection.tsx:26,49-51,68-78` | the supported, truthful open path is the prop `showConfirmationOnOpen` → `componentDidMount` → `showConfirmation()` (sets `isShowingConfirmation: true` when not action-blocked). |
| C | `DaedalusDiagnostics.tsx:414, 527, 731-733` | `DaedalusDiagnostics` accepts prop **`showMithrilPartialSyncConfirmationOnOpen?: boolean`** and forwards it to `MithrilPartialSyncSection` as `showConfirmationOnOpen`. So passing this prop on `DaedalusDiagnostics` opens the modal through real wiring. `baseProps` has `isMithrilPartialSyncWorking:false` + `isMithrilBootstrapActive:false` → `isActionBlocked:false` → the open is not suppressed. |
| D | `Diagnostics.stories.tsx:33-73` | `baseProps` includes `isMithrilPartialSyncEnabled:true`, `isMithrilPartialSyncSignificantlyBehind:true`, `isMithrilPartialSyncWorking:false`, `isMithrilBootstrapActive:false`, `onStartMithrilPartialSync: action(...)`. |
| E | `MithrilPartialSyncRecommendation.tsx:43-48` | props `{ formattedSyncPercentage, isActionBlocked, isSynced, onShowConfirmation }`; `isSynced` toggles `recommendation` vs `recommendationWithProgress` copy; `isActionBlocked` toggles hint + disables the CTA. `static contextTypes = { intl }`. |
| F | `MithrilPartialSyncConfirmation.tsx:89-96` | props `{ isActionBlocked, startError, behindByEpochs?, formattedSyncPercentage, onCancel, onConfirm }`; renders `Dialog`; `behindByEpochs` finite → `behind` copy else `behindUnknown`; `startError` shows the error line. `static contextTypes = { intl }`. |
| G | `SyncingConnectingMithrilPrompt.tsx:70-74` | props `{ behindByEpochs?, onStart(): Promise<void>, onDismiss() }`; internal `view` state ('choice'→'confirm'); `static contextTypes = { intl }`. i18n in the `daedalus.diagnostics.dialog.mithrilProactivePrompt*` namespace. |
| H | `MithrilPartialSyncOverlay.stories.tsx:16-50` | `getActiveProgressItems(intl)` / `getCompletedProgressItems(intl)` build `MithrilProgressItem[]` with intl-formatted labels (states completed/active/pending). |
| I | `MithrilPartialSyncOverlay.stories.tsx:52-76` | error fixtures `cancelledError` (stage preparing), `restartAllowedError` (stage verifying), `wipeOnlyError` (stage starting-node). |
| J | `MithrilPartialSyncOverlay.stories.tsx:78-142` | `baseProps` (line 78), `StoryProps` interface (101-111), `MithrilPartialSyncOverlayStory(props, context)` factory pulling `intl` from `context` (113-114), `contextTypes` (140-142). progressItems chosen by `props.completed` at :133-135. |
| K | `MithrilPartialSyncOverlay.stories.tsx:144-185` | existing stories: Active Progress (converting), Cancelled, Failed With Restart Allowed, Failed With Wipe-Only Recovery, Completed. |
| L | `partialSyncErrorCopy.ts:46-88` | copy resolves: 1st by `error.code` (COPY_BY_CODE), 2nd by `error.stage` (COPY_BY_STAGE = downloading→DOWNLOAD_FAILED, verifying→STAGED_DB_INVALID, converting→CONVERSION_FAILED, installing→STAGED_DB_INVALID), else generic FAILED; `status==='cancelled'` short-circuits to CANCELLED. **`finalizing` is intentionally NOT mapped → generic FAILED copy.** |
| M | `MithrilProgressView.tsx:99-100,166-187` | `status === 'stopping-node'` renders the populated stopping-node completion block (title/detail/spinner). Overlay disables Cancel during stopping-node and shows a tooltip (`MithrilPartialSyncOverlay.tsx:184-191`). |
| N | `StoryWrapper.tsx:70-82` | global `IntlProvider key={locale}` + `ThemeManager`; the locale/theme switch is the render-time seam. |

---

## Implementation approach (ordered, mechanical)

### FILE B — `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`

#### Step B1 — add two truthful progress-item builders (reuse the intl seam)
Immediately AFTER `getCompletedProgressItems` (currently ends at line 50), add:
```tsx
const getStoppingProgressItems = (intl: Intl): Array<MithrilProgressItem> =>
  getActiveProgressItems(intl).map((item) => ({
    ...item,
    state: 'pending' as const,
  }));

const getDownloadingProgressItems = (intl: Intl): Array<MithrilProgressItem> =>
  getActiveProgressItems(intl).map((item) => {
    if (item.id === 'prepare') return { ...item, state: 'completed' as const };
    if (item.id === 'download') return { ...item, state: 'active' as const };
    return { ...item, state: 'pending' as const };
  });

function getProgressItemsForStory(
  intl: Intl,
  status: MithrilPartialSyncStatus,
  completed?: boolean
): Array<MithrilProgressItem> {
  if (completed) return getCompletedProgressItems(intl);
  if (status === 'stopping-node') return getStoppingProgressItems(intl);
  if (status === 'downloading') return getDownloadingProgressItems(intl);
  return getActiveProgressItems(intl);
}
```
These reuse `getActiveProgressItems(intl)` (intl resolved at render → locale-aware) and only remap
`state`, so no labels are captured at import. `Intl`, `MithrilProgressItem`, and
`MithrilPartialSyncStatus` are already imported (lines 5-12).

#### Step B2 — add four error-stage fixtures
Immediately AFTER `wipeOnlyError` (currently ends at line 76), add:
```tsx
const downloadingError: MithrilPartialSyncError = {
  stage: 'downloading',
  code: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
  message:
    'Daedalus could not finish downloading and verifying the Mithril snapshot before cutover, so your existing database is untouched and all recovery actions remain available.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const convertingError: MithrilPartialSyncError = {
  stage: 'converting',
  code: 'PARTIAL_SYNC_CONVERSION_FAILED',
  message:
    'The verified snapshot was downloaded but could not be prepared for the local node before cutover, so recovery can still retry, restart normally, or wipe and full-sync.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const installingError: MithrilPartialSyncError = {
  stage: 'installing',
  code: 'PARTIAL_SYNC_STAGED_DB_INVALID',
  message:
    'The staged database failed validation while being installed, so Daedalus kept the existing chain data and left every recovery action available.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const finalizingError: MithrilPartialSyncError = {
  stage: 'finalizing',
  code: 'PARTIAL_SYNC_FINALIZE_FAILED',
  message:
    'Finalizing the restored chain data did not complete cleanly; this stage has no bespoke copy and intentionally falls back to the generic failed copy.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};
```
**Truthfulness note (do not "fix"):** `downloadingError`→DOWNLOAD_FAILED copy,
`convertingError`→CONVERSION_FAILED copy, `installingError`→STAGED_DB_INVALID copy (via
COPY_BY_CODE), `finalizingError`→**generic FAILED copy** (finalizing is intentionally omitted from
both COPY_BY_CODE and COPY_BY_STAGE per `partialSyncErrorCopy.ts:57-68`). This generic fallback is
the correct, intended behaviour — the finalizing story demonstrates it.

#### Step B3 — make the factory pick truthful progress items
In `MithrilPartialSyncOverlayStory` replace the JSX progress-items expression (currently
lines 133-135):
```tsx
      progressItems={
        props.completed ? completedProgressItems : activeProgressItems
      }
```
with:
```tsx
      progressItems={getProgressItemsForStory(intl, props.status, props.completed)}
```
The local `activeProgressItems` / `completedProgressItems` consts at lines 115-116 may stay (still
referenced indirectly through the helper) — leave them as-is to keep the diff minimal; they are not
unused (the helper calls the getters fresh). If lint flags them as unused after the swap, delete
those two `const` lines (115-116). Existing stories are unaffected: `converting` → active set,
`completed` (props.completed) → completed set.

#### Step B4 — add the new overlay stories
Append to the `storiesOf('Loading / Mithril / Partial Sync Overlay', module)` chain (after the
existing `.add('Completed', ...)` block at line 177-185), keeping the existing 5 stories intact:
```tsx
  .add('Downloading File Count', () => (
    <MithrilPartialSyncOverlayStory
      status="downloading"
      filesDownloaded={4}
      filesTotal={9}
      elapsedSeconds={210}
    />
  ))
  .add('Stopping Node', () => (
    <MithrilPartialSyncOverlayStory status="stopping-node" />
  ))
  .add('Failed - Downloading (All Recovery Actions)', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={downloadingError}
      canRetry
      canRestartNormally
      canWipeAndFullSync
    />
  ))
  .add('Failed - Converting (All Recovery Actions)', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={convertingError}
      canRetry
      canRestartNormally
      canWipeAndFullSync
    />
  ))
  .add('Failed - Installing (All Recovery Actions)', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={installingError}
      canRetry
      canRestartNormally
      canWipeAndFullSync
    />
  ))
  .add('Failed - Finalizing (All Recovery Actions)', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={finalizingError}
      canRetry
      canRestartNormally
      canWipeAndFullSync
    />
  ));
```
Move the trailing semicolon: the existing chain ends with `;` after the `Completed` story — append
the new `.add(...)` calls BEFORE that final `;` so the chain stays one expression.

**Coverage mapping:**
- Error stages downloading/converting/installing/finalizing each with the full retry+restart+wipe
  3-action layout (#13/#28).
- **cancelled-vs-failed:** existing `Cancelled` story (CANCELLED copy, all three actions) vs the new
  `Failed - *` stories (FAILED/stage-specific copy, all three actions) — same 3-action layout, copy
  differs; the contrast is the deliverable. No new cancelled story required.
- file-count progress: `Downloading File Count` (download active, file counts, download bar) +
  existing `Active Progress`.
- populated stopping-node frame: `Stopping Node` (status `stopping-node` → completion block +
  disabled Cancel + tooltip).

### FILE A — `storybook/stories/nodes/status/Diagnostics.stories.tsx`

#### Step A1 — add imports
After the existing import of `DaedalusDiagnostics` (line 5), add:
```tsx
import MithrilPartialSyncRecommendation from '../../../../source/renderer/app/components/status/MithrilPartialSyncRecommendation';
import MithrilPartialSyncConfirmation from '../../../../source/renderer/app/components/status/MithrilPartialSyncConfirmation';
import SyncingConnectingMithrilPrompt from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';
```
(`React` and `action` are already imported at lines 1-2; `storiesOf` at line 3; `StoryDecorator` at
line 4.)

#### Step A2 — fix the broken confirmation story (delete the ref hack)
Delete the entire `class PartialSyncConfirmationStory` block (lines 75-87). Then change the
registration (line 95) from:
```tsx
  .add('Partial Sync Confirmation', () => <PartialSyncConfirmationStory />);
```
to:
```tsx
  .add('Partial Sync Confirmation', () => (
    <DaedalusDiagnostics {...baseProps} showMithrilPartialSyncConfirmationOnOpen />
  ));
```
This opens the modal through the real seam (`showMithrilPartialSyncConfirmationOnOpen` →
`showConfirmationOnOpen` → `componentDidMount` → `showConfirmation()` → `isShowingConfirmation:true`)
instead of mutating a non-existent state key. `React` stays imported (still used by JSX).

#### Step A3 — add isolated recommendation stories
After the existing `storiesOf('Nodes / Status', module)...` chain (ends at line 95 after Step A2),
add a new chain:
```tsx
storiesOf('Nodes / Status / Mithril Partial Sync Recommendation', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Behind (CTA Ready)', () => (
    <MithrilPartialSyncRecommendation
      formattedSyncPercentage="62.50"
      isActionBlocked={false}
      isSynced={false}
      onShowConfirmation={action('onShowConfirmation')}
    />
  ))
  .add('Synced', () => (
    <MithrilPartialSyncRecommendation
      formattedSyncPercentage="100.00"
      isActionBlocked={false}
      isSynced
      onShowConfirmation={action('onShowConfirmation')}
    />
  ))
  .add('Blocked', () => (
    <MithrilPartialSyncRecommendation
      formattedSyncPercentage="62.50"
      isActionBlocked
      isSynced={false}
      onShowConfirmation={action('onShowConfirmation')}
    />
  ));
```

#### Step A4 — add isolated confirmation-modal stories
```tsx
storiesOf('Nodes / Status / Mithril Partial Sync Confirmation', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Known Epochs Behind', () => (
    <MithrilPartialSyncConfirmation
      isActionBlocked={false}
      startError={null}
      behindByEpochs={42}
      formattedSyncPercentage="62.50"
      onCancel={action('onCancel')}
      onConfirm={action('onConfirm')}
    />
  ))
  .add('Unknown Behind', () => (
    <MithrilPartialSyncConfirmation
      isActionBlocked={false}
      startError={null}
      formattedSyncPercentage="62.50"
      onCancel={action('onCancel')}
      onConfirm={action('onConfirm')}
    />
  ))
  .add('Start Error', () => (
    <MithrilPartialSyncConfirmation
      isActionBlocked={false}
      startError="Unable to start Mithril partial sync."
      behindByEpochs={42}
      formattedSyncPercentage="62.50"
      onCancel={action('onCancel')}
      onConfirm={action('onConfirm')}
    />
  ));
```

#### Step A5 — add isolated proactive-prompt stories
```tsx
storiesOf('Nodes / Status / Mithril Proactive Prompt', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Known Epochs Behind', () => (
    <SyncingConnectingMithrilPrompt
      behindByEpochs={120}
      onStart={async () => action('onStart')()}
      onDismiss={action('onDismiss')}
    />
  ))
  .add('Unknown Behind', () => (
    <SyncingConnectingMithrilPrompt
      onStart={async () => action('onStart')()}
      onDismiss={action('onDismiss')}
    />
  ));
```
The prompt's "Mithril Sync (fast)" button reveals the internal confirm view in-story (no second
start path; confirm precedes start). `onStart` must return a `Promise` — the `async () =>
action('onStart')()` arrow satisfies the `(): Promise<void>` signature.

**Prompt placement rationale:** `SyncingConnectingMithrilPrompt` lives under
`loading/syncing-connecting/`, but its i18n is in the `daedalus.diagnostics.dialog.mithrilProactive
Prompt*` namespace and it is a partial-sync **decision surface** like the recommendation and
confirmation. The task constrains output to the two target files; grouping the three decision
surfaces in `Diagnostics.stories.tsx` keeps file + storybook title coherent.

---

## Files expected to change
1. `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` (Steps B1-B4).
2. `storybook/stories/nodes/status/Diagnostics.stories.tsx` (Steps A1-A5).
3. Phase-5 canonical/review/research docs (this task's own four files).
4. `mithril-partial-sync-ux-refinement-tasks.json` — `task-ux-502` `status` → `completed`
   (+ `completedAt`) (scribe step).

No component, scss, i18n, or test-file changes.

## Acceptance criteria (from tasks.json)
1. The broken confirmation story is fixed (renders the modal).
2. Isolated stories cover recommendation, confirmation modal, proactive prompt, all error stages,
   cancelled-vs-failed, and unified file-count progress (including the stopping-node frame).
3. Locale-at-render fixture seams are reused.

## Test cases
- Confirmation story renders the modal (no longer broken) — exercises the real
  `isShowingConfirmation` seam via `showMithrilPartialSyncConfirmationOnOpen`.
- Stories exist for recommendation, prompt, every error stage (downloading/converting/installing/
  finalizing), cancelled-vs-failed, and file-count progress for both flows (bootstrap pre-existing;
  partial-sync added).
- Stories render under en-US and ja-JP without `!!!` placeholder strings (all referenced
  `mithrilPartialSync*` / `mithrilProactivePrompt*` runtime keys are already populated in both
  catalogs — verified: 0 `!!!` placeholders).

## Verification plan
Inspect `package.json` scripts first (done: `compile` = `tsc --noEmit` with `precompile` =
`typedef:sass`; `lint` = eslint over `source storybook utils`; `prettier:check`;
`typedef:sass` = `typed-scss-modules source/renderer/app`; `storybook:build` =
`build-storybook`). Then, non-interactively:
1. **Env first (Node v24):** run `yarn typedef:sass` to (re)generate `.scss.d.ts`; ensure the
   identity-obj-proxy jest sidecar is present. scss-typedef-only failures are env setup, NOT task
   regressions — regenerate and retry before treating any tsc failure as real.
2. `yarn compile` (`tsc --noEmit`) — must pass; the two story files are the only changed sources.
3. `yarn lint` (or scope eslint to the two changed files) — clean.
4. `yarn prettier:check` (or scope to the two changed files) — clean.
5. **Do NOT run `yarn storybook` or `yarn dev`** (they hang). Optionally `yarn storybook:build`
   capped/time-boxed; **skip if slow or it hangs** — do not block the task on it.
6. Spot-check the two files for the locale-at-render seam (no `intl.formatMessage` captured at module
   scope) and that no new i18n key / `!!!` string was introduced.

## Risks / open questions
- **Bootstrap regression:** none expected — no component or bootstrap-story file is edited; the
  shared `MithrilProgressView`/`MithrilStepIndicator` are only consumed via the partial-sync overlay
  factory. The Step B3 swap preserves the existing `Active Progress` (converting) and `Completed`
  stories exactly.
- **Unused-const lint:** after Step B3, `activeProgressItems`/`completedProgressItems` locals
  (factory lines 115-116) may become unused. If eslint flags them, delete those two lines; otherwise
  leave them.
- **finalizing → generic copy** is intentional (anchor L); not a bug. Documented so the implementer
  and reviewer do not "correct" it.
- **Dialog/portal in isolation:** `MithrilPartialSyncConfirmation` renders a `Dialog` (ReactModal
  portal). It already renders in storybook through the fixed integration story; the isolated stories
  render the same component, so no extra harness is needed.

## Required doc / research updates
- Finalize this doc's outcome; append `Implementation:` to `task-ux-502-impl-review.md`.
- Record in `task-ux-502-research.md` (or "no new research"): the locale-at-render seam location
  (`StoryWrapper.tsx:70-82`, `key={locale}`), the confirmation real-seam fix
  (`showMithrilPartialSyncConfirmationOnOpen`), and the error-copy resolution table (esp. the
  finalizing → generic FAILED fallback).

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-5/task-ux-502-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-5/task-ux-502-impl-review.md`

---

## Final outcome (scribe — shipped)

Status flipped to `completed` after code-review APPROVAL (plan-review log `task-ux-502-plan-review.md`:
Planner 2026-06-26T14:36:14Z; Critiquer 2026-06-26T14:44:21Z = **approved**. Impl-review log
`task-ux-502-impl-review.md`: Implementer 2026-06-26T14:52:55Z; Code-Review 2026-06-26T15:02:02Z =
**approved**). `mithril-partial-sync-ux-refinement-tasks.json` `task-ux-502` `status` → `completed`,
`completedAt` = `2026-06-26T15:03:51Z`. All three acceptance criteria met; the locked safety
invariants and the empty-DB Mithril bootstrap flow are untouched. **Stories-only — no component,
i18n, scss, or test edits.**

### What shipped (stories only — two files)
- `storybook/stories/nodes/status/Diagnostics.stories.tsx` —
  - **Broken confirmation story fixed.** Deleted the ref-based `PartialSyncConfirmationStory` class
    that called `instance.setState({ isShowingMithrilPartialSyncConfirmation: true })` — a key that
    exists nowhere (`DaedalusDiagnostics` State is only `{ isNodeRestarting }`; the real key is
    `isShowingConfirmation` on `MithrilPartialSyncSection`). The story now renders
    `<DaedalusDiagnostics {...baseProps} showMithrilPartialSyncConfirmationOnOpen />`, opening the
    modal through the real seam (prop → `showConfirmationOnOpen` → `componentDidMount` →
    `showConfirmation()` → `setState({ isShowingConfirmation: true })`). `baseProps` keep
    `isMithrilBootstrapActive:false` so the open is not suppressed.
  - Added 3 imports (`MithrilPartialSyncRecommendation`, `MithrilPartialSyncConfirmation`,
    `SyncingConnectingMithrilPrompt`) and three isolated `storiesOf` chains: **Mithril Partial Sync
    Recommendation** (Behind [CTA-Ready] / Synced / Blocked), **Mithril Partial Sync Confirmation**
    (Known Epochs Behind / Unknown Behind / Start Error), and **Mithril Proactive Prompt** (Known
    Epochs Behind / Unknown Behind; `onStart` returns a Promise so the confirm-view "Start now" await
    resolves like the real store call).
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` —
  - Added `getStoppingProgressItems` / `getDownloadingProgressItems` helpers + a
    `getProgressItemsForStory(intl, status, completed)` selector (all reuse
    `getActiveProgressItems(intl)` so labels build at render — intl seam preserved).
  - Added 4 per-stage error fixtures: downloading / converting / installing mapped by code, and
    **finalizing intentionally unmapped** → generic FAILED fallthrough (per
    `partialSyncErrorCopy.ts`).
  - Swapped the factory's `progressItems` JSX to the selector and removed the two now-unused locals.
  - Appended **Downloading File Count** (4/9), **Stopping Node** (populated completion block +
    disabled Cancel with stopping tooltip), and **Failed - Downloading / Converting / Installing /
    Finalizing**, each with the full retry + restart + wipe 3-action layout. Cancelled-vs-failed =
    the existing **Cancelled** story (same `can*` flags, cancelled copy) vs the new **Failed-***
    stories (stage-specific/generic copy, same layout).

### Locked invariants honored (verified at code-review)
- Locale-at-render seam preserved — no `intl.formatMessage` captured at module scope; every story
  pulls `intl` from context at render via the factory / component `contextTypes`; the global
  `StoryWrapper` remounts on locale via `key={locale}`.
- Recovery actions render strictly from the `can*` booleans; the 3-action stories set exactly the
  three; no fabricated 4th action; the defensive Quit path is left to the no-action fixtures.
- Confirm-precedes-start via the real state seam (no second start path).
- Stopping-node keeps the overlay's real `actionDisabled` + tooltip.
- No bootstrap / shared-component (`MithrilProgressView`, `MithrilStepIndicator`) edits; bootstrap
  file-count coverage stays in `MithrilBootstrap.stories.tsx` / `MithrilProgressView.stories.tsx`.
- No new i18n keys — all referenced runtime keys already populated in en-US.json + ja-JP.json with
  zero `!!!` placeholders. New story prose is vocabulary-compliant (Mithril sync, epochs not %, no
  immutable-file references).

### Verification results (Node v24.16.0; scss `.d.ts` regenerated — env caveat, not a regression)
- `yarn typedef:sass` (typed-scss-modules) — PASS (exit 0).
- `yarn compile` (`precompile: typedef:sass` then `tsc --noEmit`) — PASS (exit 0, clean,
  whole-project; shared bootstrap stories still typecheck).
- `yarn eslint` scoped to the two changed files — PASS (exit 0; the lone
  `@typescript-eslint/no-explicit-any` warning at `Diagnostics.stories.tsx:76:6` is pre-existing on
  the untouched `baseProps`, not introduced here).
- `prettier --check` scoped to the two changed files — PASS (exit 0).
- `yarn storybook` / `yarn dev` — NOT run (they hang, per process rules); `storybook:build` skipped
  (time-boxed/optional; compile + lint + prettier cover correctness).

### Final review decision
**approved** (Code-Review iteration 1, `task-ux-502-impl-review.md`, 2026-06-26T15:02:02Z). No
requires_changes round was needed.

### Handoff
- `task-ux-601` (holistic copy pass) is unaffected by this stories-only task; the "Start Error"
  fixture uses a vocabulary-compliant placeholder rather than the component's internal fallback
  literal, which `task-ux-601` owns.
- Durable findings recorded in `task-ux-502-research.md` (broken-state-key root cause, the
  story-coverage decision-surface split, the error-copy resolution table incl. the intentional
  finalizing → generic FAILED fallback, and the locale-at-render seam location).

## Status
- Planning status: `approved`
- Build status: `completed`
- Final status: `completed` (`completedAt` `2026-06-26T15:03:51Z`)
- Final review decision: `approved`
