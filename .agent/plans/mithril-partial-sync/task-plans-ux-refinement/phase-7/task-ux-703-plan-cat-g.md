# task-ux-703 — CAT-G implementation plan (review-body nits N1–N9)

- Status: ready-to-implement
- Section: CAT-G (runs last, after CAT-A…F)
- Base: branch `feat/mithril-partial-sync-ux-refinement`, PR base `develop`
- Anchors verified 2026-07-03 against the working tree. **All locates are by quoted content, never
  by line number** — earlier sections (especially CAT-B on `MithrilController`/coordinator seams,
  CAT-C on `MithrilPartialSyncOverlay.tsx`, CAT-F on `MithrilPartialSyncService.ts`) may have
  shifted lines. If a quoted snippet cannot be found verbatim, search for its distinctive
  substring; if still absent, stop and escalate (see Escalations).

## Constraints that bind every step

- Never cite task/finding/thread IDs (N2, CAT-G, DD-703-x, …) in source comments or test titles.
  Plain rationale comments only, matching surrounding density.
- User-facing copy says "Mithril Sync" — never "partial sync", never percentages, never
  "immutable". Internal identifiers keep their `partialSync` names.
- **No new i18n messages are introduced by CAT-G.** N4 reuses only existing message
  descriptors already defined in `MithrilBootstrap.messages.ts` and already present in
  `en-US.json` / `ja-JP.json`. Do not add, rename, or delete any locale key.
- Locked boundary 11 (N4): the bootstrap flow must not regress — bootstrap specs stay green and
  the bootstrap-variant `MithrilProgressView` completed frame stays byte-identical.
- Do not commit, push, or run any `gh` command. Never run `git add -A`/`-u`. Never stage or edit
  `.gitignore` or anything under `.agent/skills/`.
- N1 is DECLINED by decision DD-703-13 — record as a no-op; **no code change** (checklist answer
  is owned by the orchestrator's PR-comment checklist, not this plan).
- N7 was absorbed by CAT-B's T11 — **do not** touch the `DaedalusDiagnostics.tsx` inner
  `isMithrilPartialSyncEnabled` re-check here.

---

# 1. Mechanical cleanups — N2, N3, N5, N6, N8, N9

(N1 checklist-only, no code. N7 done in CAT-B.)

## N2 — drop dead epoch paths in `mithrilSnapshotMetadata.ts`

### Step 1.1 — replace the dead-path comment and path list

File: `source/main/mithril/mithrilSnapshotMetadata.ts`

Locate this exact comment + array (directly above/inside `extractCertifiedEpoch`):

```ts
// Extract the Mithril certified-beacon epoch. Mirrors
// extractLatestCertifiedImmutableNumber (multi-path, undefined-safe). `['beacon','epoch']` is the
// confirmed upstream key (CardanoDbBeacon `required: [epoch, immutable_file_number]`, snake_case),
// so it is FIRST; the `epoch_number`/`epochNumber` paths are dead defense-in-depth and the bare
// top-level `['epoch']` stays LAST so it cannot shadow the beacon epoch.
```

Replace the comment with:

```ts
// Extract the Mithril certified-beacon epoch. Mirrors
// extractLatestCertifiedImmutableNumber (multi-path, undefined-safe).
// `['beacon','epoch']` is the confirmed upstream key (CardanoDbBeacon
// `required: [epoch, immutable_file_number]`, snake_case);
// `['cardano_db_beacon','epoch']` matches the production listing shape.
```

Then, inside `extractCertifiedEpoch`, locate:

```ts
  const explicitPaths = [
    ['beacon', 'epoch'],
    ['cardano_db_beacon', 'epoch'],
    ['beacon', 'epoch_number'],
    ['cardanoDbBeacon', 'epochNumber'],
    ['epoch'],
  ];
```

Replace with:

```ts
  const explicitPaths = [
    ['beacon', 'epoch'],
    ['cardano_db_beacon', 'epoch'],
  ];
```

Do NOT touch the sibling `explicitPaths` array in `extractLatestCertifiedImmutableNumber`
(the one containing `['last_immutable_file_number']`).

### Step 1.2 — update the spec that exercised the bare-epoch fallback

File: `source/main/mithril/mithrilSnapshotMetadata.spec.ts`

Locate:

```ts
  it('prefers beacon.epoch over the bare top-level epoch path', () => {
    expect(extractCertifiedEpoch({ epoch: 7, beacon: { epoch: 320 } })).toBe(
      320
    );
  });
```

Replace with:

```ts
  it('ignores the bare top-level epoch key and non-beacon epoch spellings', () => {
    expect(extractCertifiedEpoch({ epoch: 7 })).toBeNull();
    expect(extractCertifiedEpoch({ beacon: { epoch_number: 9 } })).toBeNull();
    expect(extractCertifiedEpoch({ epoch: 7, beacon: { epoch: 320 } })).toBe(
      320
    );
  });
```

The other `extractCertifiedEpoch` tests ('extracts the certified beacon epoch from the production
beacon shape', 'parses a string-valued certified epoch via toPositiveInteger', 'returns null when
no epoch key is present…') only use the two kept paths — leave them unchanged.

## N3 — replace the fabricated availability shape with a boolean getter

### Step 1.3 — coordinator: `getPartialSyncAvailability()` → `isPartialSyncEnabled()`

File: `source/main/utils/chainStorageCoordinator.ts`

Locate:

```ts
  getPartialSyncAvailability(): MithrilPartialSyncAvailability {
    return {
      isEnabled: launcherConfig.mithrilPartialSyncEnabled === true,
      // Behind-ness is computed by MithrilController + MithrilPartialSyncService;
      // these defaults are only the disabled-path / fallback shape.
      isSignificantlyBehind: false,
      behindByImmutables: undefined,
    };
  }
```

Replace with:

```ts
  isPartialSyncEnabled(): boolean {
    return launcherConfig.mithrilPartialSyncEnabled === true;
  }
```

### Step 1.4 — coordinator: remove the now-unused type import

Same file. Locate:

```ts
import type { MithrilPartialSyncAvailability } from '../../common/types/mithril-partial-sync.types';
```

Delete this line (line ~8; `MithrilPartialSyncAvailability` had exactly one use in this file —
the method removed in Step 1.3). Do NOT touch any other import in this file, and do NOT touch
`PARTIAL_SYNC_DISABLED_ERROR` / `_assertPartialSyncFeatureEnabled` — CAT-E's T16 owns that seam.

### Step 1.5 — sole consumer: `MithrilController.ts`

File: `source/main/mithril/MithrilController.ts`

Locate (inside `async getPartialSyncAvailability()` — the CONTROLLER keeps its method name and
its `MithrilPartialSyncAvailability` return type; only the coordinator call changes):

```ts
    const { isEnabled } = chainStorageCoordinator.getPartialSyncAvailability();
```

Replace with:

```ts
    const isEnabled = chainStorageCoordinator.isPartialSyncEnabled();
```

Note: CAT-B (T11/T21) edited nearby controller code — if the destructuring line reads slightly
differently (e.g., extra destructured fields such as `isProbeFailed`), keep every OTHER consumed
field working: the mechanical rule is "the coordinator now returns only a boolean; the controller
builds the availability object itself, as it already does in its return statements". If the line
consumes more than `isEnabled` from the coordinator call, escalate (see Escalations).

### Step 1.6 — controller spec: mock the new getter

File: `source/main/mithril/MithrilController.spec.ts`

Four sub-edits, all mechanical renames from an object-returning mock to a boolean-returning mock:

a. Locate `const mockGetPartialSyncAvailability = jest.fn();` → replace with
   `const mockIsPartialSyncEnabled = jest.fn();`

b. Locate:

```ts
    getPartialSyncAvailability: (...args) =>
      mockGetPartialSyncAvailability(...args),
```

Replace with:

```ts
    isPartialSyncEnabled: (...args) => mockIsPartialSyncEnabled(...args),
```

c. Locate `mockGetPartialSyncAvailability.mockReturnValue({ isEnabled: true });` (in the
   top-level `beforeEach`) → replace with `mockIsPartialSyncEnabled.mockReturnValue(true);`

d. Locate `mockGetPartialSyncAvailability.mockReturnValue({ isEnabled: false });` (in the test
   `'short-circuits to disabled before the status guard or the behind-ness probe'`) → replace
   with `mockIsPartialSyncEnabled.mockReturnValue(false);`

All existing assertions in this spec keep their expected shapes (`{ isEnabled: …,
isSignificantlyBehind: … }`) because the controller still builds that object — do not change any
`resolves.toEqual` payload.

### Step 1.7 — coordinator spec: cover the boolean getter

File: `source/main/utils/chainStorageCoordinator.spec.ts`

Locate:

```ts
  it('exposes the singleton-backed manager and Mithril service accessors', () => {
```

Insert IMMEDIATELY BEFORE it (same indentation, blank line after the inserted block):

```ts
  it('reports the launcher-config partial-sync flag as a boolean', () => {
    const { chainStorageCoordinator } = loadModule();

    expect(chainStorageCoordinator.isPartialSyncEnabled()).toBe(true);
  });
```

(The spec's module-level `jest.mock('../config', …)` pins `mithrilPartialSyncEnabled: true`; the
false branch is covered by the controller spec's disabled short-circuit test from Step 1.6d.)

## N5 — one shared idle-status factory

### Step 1.8 — add the factory to the common types module

File: `source/common/types/mithril-partial-sync.types.ts`

Locate the end of the snapshot type:

```ts
export type MithrilPartialSyncStatusSnapshot = {
  status: MithrilPartialSyncStatus;
  allowedRecoveryActions: MithrilPartialSyncFailureAction[];
  transferProgress: MithrilPartialSyncTransferProgress;
  progressItems: MithrilProgressItem[];
  error: MithrilPartialSyncError | null;
  logPath?: string;
};
```

Insert IMMEDIATELY AFTER it (blank line before and after):

```ts
// Fresh idle snapshot per call so no two holders ever share the nested
// transferProgress / progressItems references.
export const makeIdlePartialSyncStatus =
  (): MithrilPartialSyncStatusSnapshot => ({
    status: 'idle',
    allowedRecoveryActions: [],
    transferProgress: {},
    progressItems: [],
    error: null,
  });
```

### Step 1.9 — `MithrilController.ts` uses the factory

File: `source/main/mithril/MithrilController.ts`

a. Locate and DELETE the constant:

```ts
const DEFAULT_PARTIAL_SYNC_STATUS: MithrilPartialSyncStatusSnapshot = {
  status: 'idle',
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
};
```

(Leave the sibling `DEFAULT_BOOTSTRAP_STATUS` — bootstrap types are out of scope.)

b. Locate the field initializer:

```ts
  _partialSyncStatus: MithrilPartialSyncStatusSnapshot = {
    ...DEFAULT_PARTIAL_SYNC_STATUS,
  };
```

Replace with:

```ts
  _partialSyncStatus: MithrilPartialSyncStatusSnapshot =
    makeIdlePartialSyncStatus();
```

c. Extend the existing VALUE import from the types module. Locate:

```ts
import {
  isMithrilPartialSyncBlockingNodeStart,
  isMithrilPartialSyncWorkingStatus,
} from '../../common/types/mithril-partial-sync.types';
```

Replace with:

```ts
import {
  isMithrilPartialSyncBlockingNodeStart,
  isMithrilPartialSyncWorkingStatus,
  makeIdlePartialSyncStatus,
} from '../../common/types/mithril-partial-sync.types';
```

### Step 1.10 — `MithrilPartialSyncService.ts` uses the factory

File: `source/main/mithril/MithrilPartialSyncService.ts` (CAT-F restructured this file — locate
by content).

a. Locate and DELETE the constant:

```ts
const DEFAULT_STATUS: MithrilPartialSyncStatusSnapshot = {
  status: 'idle',
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
};
```

b. Locate the field initializer:

```ts
  _status: MithrilPartialSyncStatusSnapshot = { ...DEFAULT_STATUS };
```

Replace with:

```ts
  _status: MithrilPartialSyncStatusSnapshot = makeIdlePartialSyncStatus();
```

c. Add a value import. The service currently imports from the types module with `import type {`
   only. Locate:

```ts
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncErrorStage,
  MithrilPartialSyncStatusSnapshot,
} from '../../common/types/mithril-partial-sync.types';
```

(If CAT-A/CAT-F added names to this `import type` list, keep them.) Insert IMMEDIATELY AFTER the
closing line of that import:

```ts
import { makeIdlePartialSyncStatus } from '../../common/types/mithril-partial-sync.types';
```

d. Verify `DEFAULT_STATUS` has no remaining references in this file
   (`grep -n "DEFAULT_STATUS" source/main/mithril/MithrilPartialSyncService.ts` → no hits). If a
   CAT-F edit introduced another reference, replace each with `makeIdlePartialSyncStatus()`.

### Step 1.11 — `MithrilPartialSyncStore.ts` uses the factory

File: `source/renderer/app/stores/MithrilPartialSyncStore.ts`

a. Locate:

```ts
const DEFAULT_STATUS: MithrilPartialSyncStatusSnapshot = {
  status: 'idle',
  allowedRecoveryActions: [],
  transferProgress: {},
  progressItems: [],
  error: null,
};
```

Replace with (the store reads `DEFAULT_STATUS.status` and `DEFAULT_STATUS.error` in observable
initializers, so the local const stays but is now factory-sourced):

```ts
const DEFAULT_STATUS: MithrilPartialSyncStatusSnapshot =
  makeIdlePartialSyncStatus();
```

b. Extend the store's existing VALUE import from the types module. Locate:

```ts
import {
  isMithrilPartialSyncActiveStatus,
  isMithrilPartialSyncOverlayStatus,
  isMithrilPartialSyncTerminalStatus,
  isMithrilPartialSyncWorkingStatus,
} from '../../../common/types/mithril-partial-sync.types';
```

Replace with:

```ts
import {
  isMithrilPartialSyncActiveStatus,
  isMithrilPartialSyncOverlayStatus,
  isMithrilPartialSyncTerminalStatus,
  isMithrilPartialSyncWorkingStatus,
  makeIdlePartialSyncStatus,
} from '../../../common/types/mithril-partial-sync.types';
```

## N6 — static import instead of `await import` in `_runBinary`

### Step 1.12 — `MithrilPartialSyncService.ts`

a. Locate:

```ts
import { runCommand } from './mithrilCommandRunner';
```

Replace with:

```ts
import { runBinary, runCommand } from './mithrilCommandRunner';
```

b. Locate (inside `async _runBinary(`):

```ts
    const { runBinary } = await import('./mithrilCommandRunner');
    return runBinary(
```

Replace with:

```ts
    return runBinary(
```

(No circularity risk: the same module is already statically imported for `runCommand`. The spec's
module-level `jest.mock('./mithrilCommandRunner', …)` keeps working — its `runBinary: jest.fn()`
now resolves through the static binding. `_runBinary` stays `async`.)

## N8 — remove the no-op `.filter` on the terminal-status spread

### Step 1.13 — `mithril-partial-sync.types.ts`

Locate:

```ts
const MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES: MithrilPartialSyncStatus[] = [
  ...MITHRIL_PARTIAL_SYNC_WORKING_STATUSES,
  ...MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES.filter(
    (status) => status !== 'idle'
  ),
];
```

Replace with:

```ts
const MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES: MithrilPartialSyncStatus[] = [
  ...MITHRIL_PARTIAL_SYNC_WORKING_STATUSES,
  ...MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES,
];
```

(`MITHRIL_PARTIAL_SYNC_TERMINAL_STATUSES` is exactly `['completed', 'failed', 'cancelled']` —
`'idle'` is not in it, so this is a behavioral no-op.)

### Step 1.14 — new spec pinning the N5 factory and the N8 no-op

Create NEW file: `source/common/types/mithril-partial-sync.types.spec.ts` with exactly:

```ts
import {
  isMithrilPartialSyncOverlayStatus,
  makeIdlePartialSyncStatus,
} from './mithril-partial-sync.types';

describe('mithril-partial-sync types helpers', () => {
  it('makeIdlePartialSyncStatus returns the idle snapshot with fresh nested references per call', () => {
    const first = makeIdlePartialSyncStatus();
    const second = makeIdlePartialSyncStatus();

    expect(first).toEqual({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(second).not.toBe(first);
    expect(second.transferProgress).not.toBe(first.transferProgress);
    expect(second.progressItems).not.toBe(first.progressItems);
    expect(second.allowedRecoveryActions).not.toBe(
      first.allowedRecoveryActions
    );
  });

  it('treats every working and terminal status as an overlay status, but never idle', () => {
    const overlayStatuses = [
      'stopping-node',
      'cancelling',
      'preparing',
      'downloading',
      'verifying',
      'converting',
      'installing',
      'finalizing',
      'starting-node',
      'completed',
      'failed',
      'cancelled',
    ] as const;

    overlayStatuses.forEach((status) => {
      expect(isMithrilPartialSyncOverlayStatus(status)).toBe(true);
    });
    expect(isMithrilPartialSyncOverlayStatus('idle')).toBe(false);
  });
});
```

## N9 — hoist the duplicated `SNAPSHOT_FILES_TOTAL` story constant

### Step 1.15 — add the fixture export

File: `storybook/stories/loading/_support/mithrilFixtures.ts`

Locate:

```ts
export const snapshotSize = latestSnapshot.size;
```

Insert IMMEDIATELY AFTER it:

```ts
export const snapshotFilesTotal = 980;
```

### Step 1.16 — `MithrilProgressView.stories.tsx`

File: `storybook/stories/loading/mithril/MithrilProgressView.stories.tsx`

a. Locate the fixtures import list:

```ts
import {
  ancillaryBytesTotal,
  bootstrapActions,
  createBootstrapStartedAt,
  getProgressItemsPreset,
  progressPresetOptions,
  snapshotSize,
} from '../_support/mithrilFixtures';
```

Replace with (alphabetical — `snapshotFilesTotal` before `snapshotSize`):

```ts
import {
  ancillaryBytesTotal,
  bootstrapActions,
  createBootstrapStartedAt,
  getProgressItemsPreset,
  progressPresetOptions,
  snapshotFilesTotal,
  snapshotSize,
} from '../_support/mithrilFixtures';
```

b. Locate and DELETE:

```ts
const SNAPSHOT_FILES_TOTAL = 980;
```

c. Replace every remaining `SNAPSHOT_FILES_TOTAL` identifier in this file with
   `snapshotFilesTotal` (four usages: the `filesDownloaded={Math.round(` expression, the
   `filesTotal={SNAPSHOT_FILES_TOTAL}` prop in the interactive story, and the
   `filesDownloaded`/`filesTotal` pair in the 'Starting Node Handoff' story).

### Step 1.17 — `MithrilBootstrap.stories.tsx`

File: `storybook/stories/loading/mithril/MithrilBootstrap.stories.tsx`

a. In the fixtures import list, locate the adjacent lines:

```ts
  progressPresetOptions,
  snapshotSize,
```

Replace with:

```ts
  progressPresetOptions,
  snapshotFilesTotal,
  snapshotSize,
```

b. Locate and DELETE:

```ts
const SNAPSHOT_FILES_TOTAL = 980;
```

c. Replace every remaining `SNAPSHOT_FILES_TOTAL` identifier in this file with
   `snapshotFilesTotal` (two usages: the `filesDownloaded={Math.round(` expression and
   `filesTotal={SNAPSHOT_FILES_TOTAL}`).

---

# 2. N4 — `MithrilProgressView` bootstrap/partial-sync variant flag

Decision DD-703-14 mechanics: replace the 11 pre-formatted intl props the partial-sync overlay
passes into `MithrilProgressView` with a single `variant` flag. **Locked boundary 11 invariant:
`variant` defaults to `'bootstrap'`, and for that default every rendered string resolves to the
exact same message descriptor the old `x || intl.formatMessage(messages.y)` fallbacks already
produced (bootstrap callers never passed any of the 11 props), the completed-transition frame
stays partial-sync-only, and the tooltip stays `undefined` — so the bootstrap-variant output,
including the completed frame, is byte-identical.** No new i18n: every message referenced below
already exists in `MithrilBootstrap.messages.ts` (imported in this file as `messages`) with EN
and JA locale entries.

CAT-C (T13) edited `MithrilPartialSyncOverlay.tsx` earlier — locate by content; if the quoted
overlay props differ beyond formatting (e.g., a message id was swapped), escalate.

### Step 2.1 — Props: add `variant`, drop the 11 string props

File: `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx`

Locate the full `interface Props { … }` block (starts `status: MithrilBootstrapStatus |
MithrilPartialSyncStatus;`, ends `onAction(): void;`). Replace the whole block with:

```tsx
interface Props {
  status: MithrilBootstrapStatus | MithrilPartialSyncStatus;
  variant?: 'bootstrap' | 'partial-sync';
  progressItems?: MithrilProgressItem[];
  filesDownloaded?: number;
  filesTotal?: number;
  snapshotSizeBytes?: number;
  ancillaryBytesDownloaded?: number;
  ancillaryBytesTotal?: number;
  ancillaryProgress?: number;
  bootstrapStartedAt?: number | null;
  elapsedSeconds?: number;
  hideAction?: boolean;
  actionDisabled?: boolean;
  showDownloadProgressBar?: boolean;
  onAction(): void;
}
```

(Removed: `title`, `subtitle`, `actionLabel`, `startingNodeTitle`, `startingNodeDetail`,
`stoppingNodeTitle`, `stoppingNodeDetail`, `cancellingTitle`, `cancellingDetail`,
`completedTransitionLabel`, `actionDisabledTooltip`. Verified today: no consumer other than the
partial-sync overlay passes any of them — `MithrilBootstrap.tsx`, both stories files, and both
component specs pass none.)

### Step 2.2 — destructuring

Same file. Locate the destructuring block beginning `const {` / `status,` and ending
`onAction,` / `} = props;`. Replace the whole block with:

```tsx
  const {
    status,
    variant = 'bootstrap',
    progressItems,
    filesDownloaded,
    filesTotal,
    snapshotSizeBytes,
    ancillaryBytesDownloaded,
    ancillaryBytesTotal,
    ancillaryProgress,
    bootstrapStartedAt,
    elapsedSeconds: elapsedSecondsProp,
    hideAction,
    actionDisabled,
    showDownloadProgressBar,
    onAction,
  } = props;
```

### Step 2.3 — variant-derived flags and the completed-transition guard comment

Same file. Locate:

```tsx
  const isLongRunningPhase = LONG_RUNNING_STATUSES.has(status);
  // The partial-sync overlay passes `completedTransitionLabel` to
  // turn the 'completed' frame into a loading-style hand-off (spinner +
  // "Returning to Daedalus...") while the finalize auto-timeout runs. Bootstrap
  // never passes the prop, so its 'completed' frame is byte-for-byte unchanged.
  const isCompletedTransition =
    status === 'completed' && !!completedTransitionLabel;
```

Replace with:

```tsx
  const isLongRunningPhase = LONG_RUNNING_STATUSES.has(status);
  const isPartialSync = variant === 'partial-sync';
  // The partial-sync variant turns the 'completed' frame into a loading-style
  // hand-off (spinner + "Returning to Daedalus...") while the finalize
  // auto-timeout runs. The default bootstrap variant keeps its own 'completed'
  // frame byte-for-byte unchanged.
  const isCompletedTransition = status === 'completed' && isPartialSync;
```

### Step 2.4 — variant-resolved subtitle and tooltip, computed before `return`

Same file. Locate:

```tsx
  const elapsedLabel = formatDuration(elapsedSeconds) ?? '0:00';

  return (
```

Replace with:

```tsx
  const elapsedLabel = formatDuration(elapsedSeconds) ?? '0:00';

  let subtitleMessage = messages.progressSubtitle;
  if (isPartialSync) {
    subtitleMessage =
      status === 'completed'
        ? messages.partialSyncCompletedSubtitle
        : messages.partialSyncProgressSubtitle;
  }
  // A disabled Cancel needs an explanation; only the partial-sync flow
  // disables it (while the node is still stopping).
  const actionDisabledTooltip =
    isPartialSync && isStoppingNode
      ? intl.formatMessage(messages.partialSyncCancelStoppingTooltip)
      : undefined;

  return (
```

### Step 2.5 — header resolves by variant

Same file. Locate:

```tsx
        <h1 id={MITHRIL_PROGRESS_HEADING_ID}>
          {title || intl.formatMessage(messages.title)}
        </h1>
        <p>{subtitle || intl.formatMessage(messages.progressSubtitle)}</p>
```

Replace with:

```tsx
        <h1 id={MITHRIL_PROGRESS_HEADING_ID}>
          {intl.formatMessage(
            isPartialSync ? messages.partialSyncTitle : messages.title
          )}
        </h1>
        <p>{intl.formatMessage(subtitleMessage)}</p>
```

### Step 2.6 — node-stop frame

Same file. Locate:

```tsx
      {isStoppingNode && (
        <CompletionBlock
          title={
            stoppingNodeTitle || intl.formatMessage(messages.nodeStoppingTitle)
          }
          detail={
            stoppingNodeDetail ||
            intl.formatMessage(messages.nodeStoppingDetail)
          }
        />
      )}
```

Replace with:

```tsx
      {isStoppingNode && (
        <CompletionBlock
          title={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStoppingTitle
              : messages.nodeStoppingTitle
          )}
          detail={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStoppingDetail
              : messages.nodeStoppingDetail
          )}
        />
      )}
```

### Step 2.7 — cancelling frame (both variants already used the partial-sync messages)

Same file. Locate:

```tsx
      {isCancelling && (
        <CompletionBlock
          title={
            cancellingTitle ||
            intl.formatMessage(messages.partialSyncCancellingTitle)
          }
          detail={
            cancellingDetail ||
            intl.formatMessage(messages.partialSyncCancellingDetail)
          }
        />
      )}
```

Replace with:

```tsx
      {isCancelling && (
        <CompletionBlock
          title={intl.formatMessage(messages.partialSyncCancellingTitle)}
          detail={intl.formatMessage(messages.partialSyncCancellingDetail)}
        />
      )}
```

### Step 2.8 — node-start frame

Same file. Locate:

```tsx
      {isStartingNode && (
        <CompletionBlock
          title={
            startingNodeTitle || intl.formatMessage(messages.nodeStartingTitle)
          }
          detail={
            startingNodeDetail ||
            intl.formatMessage(messages.nodeStartingDetail)
          }
        />
      )}
```

Replace with:

```tsx
      {isStartingNode && (
        <CompletionBlock
          title={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStartingTitle
              : messages.nodeStartingTitle
          )}
          detail={intl.formatMessage(
            isPartialSync
              ? messages.partialSyncNodeStartingDetail
              : messages.nodeStartingDetail
          )}
        />
      )}
```

### Step 2.9 — completed-transition frame

Same file. Locate:

```tsx
      {isCompletedTransition && (
        <CompletionBlock
          title={completedTransitionLabel || ''}
          spinnerPosition="top"
        />
      )}
```

Replace with:

```tsx
      {isCompletedTransition && (
        <CompletionBlock
          title={intl.formatMessage(messages.partialSyncCompletedTransition)}
          spinnerPosition="top"
        />
      )}
```

### Step 2.10 — action button label

Same file. Locate:

```tsx
                label={actionLabel || intl.formatMessage(messages.cancel)}
```

Replace with:

```tsx
                label={intl.formatMessage(messages.cancel)}
```

(The overlay passed `MithrilBootstrapMessages.cancel` — the identical descriptor — so this is
output-identical for both variants. The `actionDisabledTooltip ? <PopOver …` wrapper below stays
untouched; the variable now comes from Step 2.4.)

### Step 2.11 — overlay passes `variant="partial-sync"` instead of the 11 props

File: `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`

Locate the whole `<MithrilProgressView` element (starts `<MithrilProgressView`, first prop
`status={status}`, ends `onAction={onCancel}` / `/>`). Replace the element with:

```tsx
            <MithrilProgressView
              status={status}
              variant="partial-sync"
              progressItems={progressItems}
              filesDownloaded={transferProgress?.filesDownloaded}
              filesTotal={transferProgress?.filesTotal}
              ancillaryBytesDownloaded={
                transferProgress?.ancillaryBytesDownloaded
              }
              ancillaryBytesTotal={transferProgress?.ancillaryBytesTotal}
              bootstrapStartedAt={startedAt}
              hideAction={[
                'cancelling',
                'installing',
                'finalizing',
                'starting-node',
                'completed',
              ].includes(status)}
              actionDisabled={status === 'stopping-node'}
              showDownloadProgressBar
              onAction={onCancel}
            />
```

(If CAT-C's T13 changed any of the KEPT props — e.g. `hideAction`'s status list — preserve
CAT-C's version of those props verbatim and only remove the 11 string props + add
`variant="partial-sync"`.) Leave the overlay's `MithrilBootstrapMessages` import in place — it is
still used by the error view and recovery actions.

### Step 2.12 — view spec: variant pass-through + new assertions

File: `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx`

a. Locate the `renderComponent` helper parameter list and JSX:

```tsx
  const renderComponent = ({
    status = 'unpacking' as const,
    filesDownloaded,
    filesTotal,
    bootstrapStartedAt,
  }: {
```

Replace with:

```tsx
  const renderComponent = ({
    status = 'unpacking' as const,
    variant,
    filesDownloaded,
    filesTotal,
    bootstrapStartedAt,
  }: {
```

Then locate (further down in the same type annotation):

```tsx
    filesDownloaded?: number;
    filesTotal?: number;
    bootstrapStartedAt?: number | null;
  } = {}) =>
```

Replace with:

```tsx
    variant?: 'bootstrap' | 'partial-sync';
    filesDownloaded?: number;
    filesTotal?: number;
    bootstrapStartedAt?: number | null;
  } = {}) =>
```

Then locate:

```tsx
        <MithrilProgressView
          status={status}
          filesDownloaded={filesDownloaded}
```

Replace with:

```tsx
        <MithrilProgressView
          status={status}
          variant={variant}
          filesDownloaded={filesDownloaded}
```

b. Locate the existing test:

```tsx
  it('does not show completion block for restore-complete state alone', () => {
```

Insert IMMEDIATELY BEFORE it the three new tests (same indentation):

```tsx
  it('renders the hand-off frame on completed for the partial-sync variant', () => {
    renderComponent({ status: 'completed', variant: 'partial-sync' });

    expect(screen.getByText(/returning to daedalus/i)).toBeInTheDocument();
    expect(
      screen.getByRole('heading', { name: /^mithril sync$/i })
    ).toBeInTheDocument();
    expect(
      screen.getByText(/mithril sync completed successfully/i)
    ).toBeInTheDocument();
  });

  it('keeps the bootstrap completed frame free of the hand-off transition', () => {
    renderComponent({ status: 'completed' });

    expect(
      screen.queryByText(/returning to daedalus/i)
    ).not.toBeInTheDocument();
    expect(
      screen.getByRole('heading', { name: /fast sync with mithril/i })
    ).toBeInTheDocument();
  });

  it('shows the partial-sync node-stop copy for the partial-sync variant', () => {
    renderComponent({
      status: 'stopping-node',
      variant: 'partial-sync',
      bootstrapStartedAt: Date.now(),
    });

    expect(
      screen.getByText(
        /stopping the cardano node before restoring verified mithril chain data/i
      )
    ).toBeInTheDocument();
  });
```

c. Leave every existing test untouched — they all render the default (bootstrap) variant and are
   the byte-identical guard for boundary 11 (notably 'does not show completion block for
   restore-complete state alone', 'renders the header and timer', 'shows the in-dialogue
   node-stop frame while stopping-node', 'keeps the cancel button enabled in completed restore
   state').

### Step 2.13 — stories/specs re-run (no story content changes for N4)

Neither story file passes any of the removed props (verified today), so N4 needs no story edits
beyond N9's constant hoist. Re-run gates:

- Bootstrap + partial-sync component specs and overlay spec must pass unchanged (commands below).
- `MithrilPartialSyncOverlay.spec.tsx` needs NO edits: its assertions ('returning to daedalus',
  auto-finalize timing, recovery buttons) resolve through `en-US.json` and the view now resolves
  the identical message ids internally.
- Optional author-run visual check: `yarn storybook`, stories under `Loading / Mithril /
  Progress` and `Loading / Mithril` render identically (bootstrap frames unchanged; knobs work).

---

## i18n

**No new or changed i18n messages in CAT-G.** All message ids referenced by N4
(`loading.mithrilBootstrap.title`, `loading.mithrilBootstrap.progress.subtitle`,
`loading.mithrilBootstrap.progress.nodeStoppingTitle`/`Detail`,
`loading.mithrilBootstrap.progress.nodeStartingTitle`/`Detail`,
`loading.mithrilPartialSync.title`, `loading.mithrilPartialSync.progress.subtitle`,
`loading.mithrilPartialSync.progress.nodeStartingTitle`/`Detail`,
`loading.mithrilPartialSync.progress.nodeStoppingTitle`/`Detail`,
`loading.mithrilPartialSync.progress.cancellingTitle`/`Detail`,
`loading.mithrilPartialSync.progress.cancelStoppingTooltip`,
`loading.mithrilPartialSync.completed.subtitle`,
`loading.mithrilPartialSync.completed.transition`, and the shared `cancel` key) already exist in
`MithrilBootstrap.messages.ts` and in `en-US.json`/`ja-JP.json`. Do not touch locale files or run
defaultMessages regeneration.

## Tests and exact commands

Run from the repo root. Known environment prep (apply BEFORE treating failures as regressions):
on Node 24, regenerate SCSS module typings with `yarn typedef:sass` if `yarn compile` fails on
`.scss` imports, and use the existing gitignored identity-obj-proxy jest sidecar if jest fails on
SCSS module resolution.

| Command | Covers | Key assertions |
| --- | --- | --- |
| `yarn test:jest --testPathPattern="mithrilSnapshotMetadata"` | N2 | bare `{ epoch: 7 }` and `{ beacon: { epoch_number: 9 } }` → `null`; beacon shapes still parse |
| `yarn test:jest --testPathPattern="chainStorageCoordinator"` | N3 | new boolean-getter test passes; all existing coordinator tests green |
| `yarn test:jest --testPathPattern="MithrilController"` | N3, N5 | probe-guard suite green with boolean mock; disabled short-circuit green |
| `yarn test:jest --testPathPattern="mithril-partial-sync.types"` | N5, N8 | factory shape + fresh references; overlay-status membership incl. all terminals, never idle |
| `yarn test:jest --testPathPattern="MithrilPartialSyncService"` | N5, N6 | conversion-tracking tests green via statically imported `runBinary` mock |
| `yarn test:jest --testPathPattern="MithrilPartialSyncStore"` | N5 | store suite green with factory-sourced defaults |
| `yarn test:jest --testPathPattern="MithrilProgressView"` | N4 | 3 new variant tests; ALL pre-existing bootstrap tests unchanged and green (boundary 11) |
| `yarn test:jest --testPathPattern="MithrilPartialSyncOverlay"` | N4 | overlay suite green with zero spec edits |
| `yarn test:jest --testPathPattern="MithrilBootstrap"` | boundary 11 | bootstrap specs green |
| `yarn lint && yarn compile` | all | no new ESLint/TS errors in touched files |
| `yarn prettier:check` | all | if only newly touched files fail, run `npx prettier --write <those files>`; do NOT reformat `mithrilCommandRunner*` files (pre-existing drift) |

## Files touched

- `source/main/mithril/mithrilSnapshotMetadata.ts` (N2)
- `source/main/mithril/mithrilSnapshotMetadata.spec.ts` (N2)
- `source/main/utils/chainStorageCoordinator.ts` (N3)
- `source/main/utils/chainStorageCoordinator.spec.ts` (N3)
- `source/main/mithril/MithrilController.ts` (N3, N5)
- `source/main/mithril/MithrilController.spec.ts` (N3)
- `source/common/types/mithril-partial-sync.types.ts` (N5, N8)
- `source/common/types/mithril-partial-sync.types.spec.ts` (NEW — N5, N8)
- `source/main/mithril/MithrilPartialSyncService.ts` (N5, N6)
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` (N5)
- `storybook/stories/loading/_support/mithrilFixtures.ts` (N9)
- `storybook/stories/loading/mithril/MithrilProgressView.stories.tsx` (N9)
- `storybook/stories/loading/mithril/MithrilBootstrap.stories.tsx` (N9)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx` (N4)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx` (N4)
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (N4)

## Out of scope (owned by other sections)

- N1: declined by DD-703-13 — no code; checklist reply owned by the orchestrator's
  `task-ux-703-pr-comment-checklist.md`.
- N7 (`DaedalusDiagnostics.tsx` inner enabled re-check): absorbed by CAT-B T11.
- `PARTIAL_SYNC_DISABLED_ERROR` / `_assertPartialSyncFeatureEnabled` coded-error conversion:
  CAT-E T16 (do not touch even though N3 edits the same file).
- All copy keys: finalize-failed (CAT-C), insufficient-disk-space (CAT-D), start-failure fallback
  in `partialSyncErrorCopy.ts` + remaining copy (CAT-E); `mithrilErrorMessage.ts` helper (CAT-F).
- `MithrilPartialSyncErrorCode` union additions (CAT-D/E, append-only).
- `mithrilCommandRunner.ts` spawn-helper extraction and its process-group comments/`detached`
  flags (CAT-F T26) — N6 only changes the IMPORT side in the service.

## Acceptance checks

- **N1**: no diff anywhere for this nit; recorded as declined no-op (DD-703-13).
- **N2**: `extractCertifiedEpoch` consults only `['beacon','epoch']` and
  `['cardano_db_beacon','epoch']`; bare top-level `epoch` and `epoch_number`/`epochNumber`
  spellings return `null`; comment no longer declares dead paths; metadata spec green.
- **N3**: `chainStorageCoordinator` exposes `isPartialSyncEnabled(): boolean`; no fabricated
  `MithrilPartialSyncAvailability` literal remains in the coordinator; `MithrilController` (sole
  consumer) still returns the full availability object it builds itself (renderer still receives
  backend-computed behind-ness only — boundary 2/4 untouched); controller + coordinator specs
  green.
- **N4**: `MithrilProgressView` accepts `variant?: 'bootstrap' | 'partial-sync'` defaulting to
  `'bootstrap'`; the overlay passes `variant="partial-sync"` and zero pre-formatted intl strings;
  the completed-transition frame renders only for the partial-sync variant; all pre-existing
  bootstrap tests pass UNMODIFIED and bootstrap stories render unchanged (boundary 11); no new
  i18n keys; no user-visible copy change.
- **N5**: exactly one idle-snapshot literal remains in the codebase — inside
  `makeIdlePartialSyncStatus()` in `mithril-partial-sync.types.ts`; controller, service, and
  store all consume the factory; new types spec pins shape + fresh references.
- **N6**: no `await import('./mithrilCommandRunner')` remains in
  `MithrilPartialSyncService.ts`; `runBinary` rides the existing static import; service spec
  green.
- **N8**: `MITHRIL_PARTIAL_SYNC_OVERLAY_STATUSES` is the plain concat of working + terminal
  arrays; new types spec pins membership (all working + `completed`/`failed`/`cancelled` true,
  `idle` false).
- **N9**: `980` appears once, as `snapshotFilesTotal` in `mithrilFixtures.ts`; both story files
  import it; no `SNAPSHOT_FILES_TOTAL` identifier remains.

## Escalations

- None found today — every anchor in the section brief matched the working tree (11 props
  confirmed at the overlay call site; guard comment present; `runBinary` dynamic import present;
  all three idle literals present; filter no-op present; both story constants present).
- Escalate-on-mismatch triggers for the implementer (earlier sections run first and may drift
  these seams):
  1. Step 1.5: if the controller consumes anything besides `isEnabled` from
     `chainStorageCoordinator.getPartialSyncAvailability()` (CAT-B may have added fields such as
     a probe-failure flag), stop — the coordinator boolean getter may need to carry more than a
     boolean, which is a design change beyond this plan.
  2. Step 2.11: if CAT-C's edits replaced any of the 11 string props with DIFFERENT message ids
     than quoted here (e.g., a new completed-subtitle key), stop — internalizing the wrong id
     would silently change copy.
  3. Step 1.10/1.12: if CAT-F renamed `_runBinary`/`_status` or moved `DEFAULT_STATUS`, re-locate
     by the quoted fragments (`await import('./mithrilCommandRunner')`, `status: 'idle',`); if
     the dynamic import is already gone, record N6 as already-done and skip.
  4. Any failing PRE-EXISTING bootstrap test after Step 2.x is an automatic stop (boundary 11) —
     do not amend bootstrap test expectations to make them pass.
