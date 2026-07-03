# CAT-E — i18n, copy, and vocabulary (T1/T17, T4, T16, T19, T20, T22, T32)

**Section:** CAT-E of task-ux-703 (PR #3337 remediation).
**Decisions honored (never relitigate):** DD-703-12 (T16 widened to all user-reachable hardcoded-prose throw sites); T22/T23 contract (E adds + uses the shared start-failure fallback message exported from `partialSyncErrorCopy.ts`; CAT-F's helper reuses it and also fixes the prompt's own fallback — E must NOT touch the prompt catch); CAT-D owns `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` (union member + copy key) — E never adds it; CAT-C owns finalize-failed copy.
**Anchors verified 2026-07-03 against the working tree.** All line numbers below are approximate — earlier sections (A–D) run first and may shift them. **Locate every edit by the quoted content, never by line number.**
**Vocabulary invariant (applies to every string below):** user-facing copy says "Mithril Sync" — never "partial sync", never percentages, never the word "immutable". Internal identifiers keep their `partialSync` names.
**Comment/test-title invariant:** never cite task/finding/thread IDs (T16, CAT-E, DD-703-x …) in source comments or test titles; where context is needed, write a plain rationale comment matching surrounding density.
**Locked boundary 11 (bootstrap):** `MithrilErrorView` is shared with the bootstrap flow — every edit there must keep `MithrilBootstrap.spec.tsx` green (verified: it has no assertions on the error body paragraph or details header). `MithrilProgressView.tsx` is NOT touched by this section.
**i18n conventions:** every new key gets (a) a `defineMessages` descriptor with `!!!`-prefixed `defaultMessage` + `description`, (b) a polished EN entry in `source/renderer/app/i18n/locales/en-US.json` (alphabetical order, no `!!!` — a spec in `MithrilPartialSyncOverlay.spec.tsx` fails on `!!!` in any `loading.mithrilPartialSync.*` key in EITHER locale), (c) a polished JA entry in `ja-JP.json`, (d) `yarn i18n:manage` regeneration (updates `translations/messages.json` and `locales/defaultMessages.json`).

All file paths are repo-relative to `/workspaces/mithril-partial-sync-ux`.

---

# 1. T16 stable error codes replacing hardcoded English throws + T19 error-view body from intl maps only

Design (fixed, no implementer decisions): user-reachable throws get **stable codes**.

- Errors that reject an IPC request (start/cancel/recovery guards in the service and coordinator) are plain `Error`s whose **message IS the code** (Electron's structured clone drops custom `Error` properties, so a `code` field would not survive IPC; the message does). The English prose moves into a `logger.warn` line immediately before the throw.
- Errors inside the run (stage errors) keep their prose `message` (after T19 it is only ever visible inside the collapsed technical-details section, which is allowed) and **gain a `code`** via the existing third argument of `_createStageError`.
- Already-coded stage throws (`PARTIAL_SYNC_LATEST_DRIFT` at the drift re-resolve, `PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED` at the exit-code throw, `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` at the disk preflight — CAT-D's) need **no change**. `_buildError` also needs **no change**: it copies `error.message` verbatim into `status.error.message`, which after T19 is confined to the collapsed details (a raw technical surface, allowed by T19), and its code/stage fields are what the renderer copy maps key on.

## Step 1.1 — Extend the error-code union (append-only)

**File:** `source/common/types/mithril-partial-sync.types.ts`

**Locate:**

```ts
export type MithrilPartialSyncErrorCode =
```

**Edit:** append the following seven members at the END of the union, immediately before the terminating `;`. Today the last member is `| 'PARTIAL_SYNC_CONVERSION_FAILED';`; if CAT-D has already landed, the last member is `| 'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE';` — either way, append after whatever the current last member is and move the `;`:

```ts
  | 'PARTIAL_SYNC_DISABLED'
  | 'PARTIAL_SYNC_ALREADY_RUNNING'
  | 'PARTIAL_SYNC_START_NOT_ALLOWED'
  | 'PARTIAL_SYNC_LAYOUT_UNSUPPORTED'
  | 'PARTIAL_SYNC_CANCEL_NOT_ALLOWED'
  | 'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED'
  | 'PARTIAL_SYNC_METADATA_UNAVAILABLE';
```

Do NOT add `'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE'` — CAT-D owns that member.

## Step 1.2 — New intl messages for chunk 1 (metadata-unavailable copy + technical-details header)

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`

**Edit A — metadata-unavailable pair.** Locate the start of the retry-action descriptor:

```ts
  partialSyncRetry: {
```

Insert immediately BEFORE that line:

```ts
  partialSyncErrorMetadataUnavailableTitle: {
    id: 'loading.mithrilPartialSync.error.metadataUnavailable.title',
    defaultMessage: '!!!Checking the latest Mithril snapshot failed',
    description:
      'Title shown when Daedalus cannot resolve the latest verified Mithril snapshot metadata before or during Mithril Sync.',
  },
  partialSyncErrorMetadataUnavailableHint: {
    id: 'loading.mithrilPartialSync.error.metadataUnavailable.hint',
    defaultMessage:
      '!!!Daedalus could not read the latest verified snapshot details. Check your internet connection, then choose how to continue below.',
    description:
      'Hint shown when Daedalus cannot resolve the latest verified Mithril snapshot metadata before or during Mithril Sync.',
  },
```

**Edit B — technical-details header (shared error view, bootstrap namespace).** Locate:

```ts
  progressDiskCheck: {
```

Insert immediately BEFORE that line:

```ts
  errorDetailsHeader: {
    id: 'loading.mithrilBootstrap.errorDetailsHeader',
    defaultMessage: '!!!Technical details',
    description:
      'Collapsed technical-details section header on the Mithril error view (bootstrap and Mithril Sync overlays).',
  },
```

## Step 1.3 — Locale entries for chunk-1 keys

**File:** `source/renderer/app/i18n/locales/en-US.json`

Locate (line ~355; CAT-C/CAT-D may have inserted `error.finalizeFailed.*` / `error.insufficientDiskSpace.*` lines nearby — anchor on this exact line, which none of them touch):

```json
  "loading.mithrilPartialSync.error.latestDrift.title": "The verified Mithril snapshot moved on",
```

Insert immediately AFTER that line (alphabetical: `metadataUnavailable` sorts between `latestDrift` and `noCertifiedRange`):

```json
  "loading.mithrilPartialSync.error.metadataUnavailable.hint": "Daedalus could not read the latest verified snapshot details. Check your internet connection, then choose how to continue below.",
  "loading.mithrilPartialSync.error.metadataUnavailable.title": "Checking the latest Mithril snapshot failed",
```

Locate:

```json
  "loading.mithrilBootstrap.errorTitle": "Mithril bootstrap failed",
```

Insert immediately BEFORE that line (`errorDetailsHeader` sorts after `error.*` dotted keys, before `errorTitle`):

```json
  "loading.mithrilBootstrap.errorDetailsHeader": "Technical details",
```

**File:** `source/renderer/app/i18n/locales/ja-JP.json` — same two anchors (JA values shown):

After `"loading.mithrilPartialSync.error.latestDrift.title": "検証済みMithrilスナップショットが更新されました",` insert:

```json
  "loading.mithrilPartialSync.error.metadataUnavailable.hint": "最新の検証済みスナップショットの詳細を読み取れませんでした。インターネット接続を確認してから、下のオプションから続行方法を選択してください。",
  "loading.mithrilPartialSync.error.metadataUnavailable.title": "最新のMithrilスナップショットの確認に失敗しました",
```

Before `"loading.mithrilBootstrap.errorTitle": "Mithrilブートストラップに失敗しました",` insert:

```json
  "loading.mithrilBootstrap.errorDetailsHeader": "技術的な詳細",
```

## Step 1.4 — Map the new codes in `partialSyncErrorCopy.ts`

**File:** `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`

**Edit A.** Locate:

```ts
const CONVERSION_FAILED: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorConversionFailedTitle,
  hint: messages.partialSyncErrorConversionFailedHint,
};
```

Insert immediately AFTER that block:

```ts
const METADATA_UNAVAILABLE: PartialSyncErrorCopy = {
  title: messages.partialSyncErrorMetadataUnavailableTitle,
  hint: messages.partialSyncErrorMetadataUnavailableHint,
};
```

**Edit B.** Locate the entry:

```ts
    PARTIAL_SYNC_CONVERSION_FAILED: CONVERSION_FAILED,
```

Insert immediately AFTER it (and after CAT-D's `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE: …` entry if present — order inside the object is irrelevant; the `Record<MithrilPartialSyncErrorCode, …>` type enforces totality). NOTE: `FAILED` is declared a few lines below `CONVERSION_FAILED` in this file — the reference is legal (module-scope `const` evaluated before the map). The rejection-path codes are defensive entries only: those errors reject IPC requests and normally never reach the failed-status overlay, but the exhaustive `Record` requires them and generic failed copy is the correct fallback if one ever rides a status error:

```ts
    PARTIAL_SYNC_METADATA_UNAVAILABLE: METADATA_UNAVAILABLE,
    PARTIAL_SYNC_DISABLED: FAILED,
    PARTIAL_SYNC_ALREADY_RUNNING: FAILED,
    PARTIAL_SYNC_START_NOT_ALLOWED: FAILED,
    PARTIAL_SYNC_LAYOUT_UNSUPPORTED: FAILED,
    PARTIAL_SYNC_CANCEL_NOT_ALLOWED: FAILED,
    PARTIAL_SYNC_RECOVERY_NOT_ALLOWED: FAILED,
```

If TypeScript then complains that `COPY_BY_CODE` is missing `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`, CAT-D's union member landed without its copy entry — that is CAT-D's defect; escalate, do not add the entry.

## Step 1.5 — Service: code constants

**File:** `source/main/mithril/MithrilPartialSyncService.ts`

**Locate:**

```ts
const PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE =
  'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE';
```

Insert immediately AFTER:

```ts
const PARTIAL_SYNC_ALREADY_RUNNING_CODE = 'PARTIAL_SYNC_ALREADY_RUNNING';
const PARTIAL_SYNC_START_NOT_ALLOWED_CODE = 'PARTIAL_SYNC_START_NOT_ALLOWED';
const PARTIAL_SYNC_CANCEL_NOT_ALLOWED_CODE = 'PARTIAL_SYNC_CANCEL_NOT_ALLOWED';
const PARTIAL_SYNC_RECOVERY_NOT_ALLOWED_CODE =
  'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED';
const PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE =
  'PARTIAL_SYNC_METADATA_UNAVAILABLE';
```

## Step 1.6 — Service: start() re-entry guard (~:178)

**Locate:**

```ts
    if (this._activeWorkDir) {
      throw new Error('Mithril partial sync is already in progress.');
    }
```

**Replace with** (prose moves to the log; the thrown message is the stable code that rides the IPC rejection):

```ts
    if (this._activeWorkDir) {
      logger.warn(
        'MithrilPartialSyncService: rejecting start; a partial sync run is already in progress',
        { status: this._status.status }
      );
      throw new Error(PARTIAL_SYNC_ALREADY_RUNNING_CODE);
    }
```

## Step 1.7 — Service: cancel-after-cutover guard (~:354)

Invariant (boundary 6): no cancel after cutover — this guard's BEHAVIOR is unchanged; only the message becomes a code.

**Locate:**

```ts
    if (['installing', 'finalizing'].includes(this._status.status)) {
      throw new Error(
        'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
      );
    }
```

**Replace with:**

```ts
    if (['installing', 'finalizing'].includes(this._status.status)) {
      logger.warn(
        'MithrilPartialSyncService: rejecting cancel; live chain cutover has already started',
        { status: this._status.status }
      );
      throw new Error(PARTIAL_SYNC_CANCEL_NOT_ALLOWED_CODE);
    }
```

## Step 1.8 — Service: `assertStartAllowed()` (~:553-571)

**Locate:**

```ts
        throw new Error(
          'Mithril partial sync cannot retry from the current recovery boundary.'
        );
```

**Replace with:**

```ts
        logger.warn(
          'MithrilPartialSyncService: rejecting start; retry is not an allowed recovery action',
          { allowedRecoveryActions: this._status.allowedRecoveryActions }
        );
        throw new Error(PARTIAL_SYNC_START_NOT_ALLOWED_CODE);
```

**Locate:**

```ts
      throw new Error(
        'Mithril partial sync cannot start from the current state.'
      );
```

**Replace with:**

```ts
      logger.warn(
        'MithrilPartialSyncService: rejecting start; a run cannot start from the current status',
        { status: this._status.status }
      );
      throw new Error(PARTIAL_SYNC_START_NOT_ALLOWED_CODE);
```

## Step 1.9 — Service: `_assertRecoveryActionAllowed()` (~:801-815)

Invariant (boundary 2): recovery actions still come strictly from backend `allowedRecoveryActions`; this only recodes the guard's rejection.

**Locate:**

```ts
    if (this._activeWorkDir || this._currentProcess) {
      throw new Error(
        `Cannot ${action} while Mithril partial sync is still in progress.`
      );
    }

    if (!this._status.allowedRecoveryActions.includes(action)) {
      throw new Error(
        `Mithril partial sync cannot ${action} from the current recovery boundary.`
      );
    }
```

**Replace with:**

```ts
    if (this._activeWorkDir || this._currentProcess) {
      logger.warn(
        'MithrilPartialSyncService: rejecting recovery action; a partial sync run is still in progress',
        { action }
      );
      throw new Error(PARTIAL_SYNC_RECOVERY_NOT_ALLOWED_CODE);
    }

    if (!this._status.allowedRecoveryActions.includes(action)) {
      logger.warn(
        'MithrilPartialSyncService: rejecting recovery action; not allowed from the current recovery boundary',
        { action, allowedRecoveryActions: this._status.allowedRecoveryActions }
      );
      throw new Error(PARTIAL_SYNC_RECOVERY_NOT_ALLOWED_CODE);
    }
```

## Step 1.10 — Service: metadata stage errors gain a code (~:900, ~:1080)

**Locate:**

```ts
      throw this._createStageError(
        'preparing',
        'Unable to resolve the latest Mithril snapshot metadata.'
      );
```

**Replace with:**

```ts
      throw this._createStageError(
        'preparing',
        'Unable to resolve the latest Mithril snapshot metadata.',
        PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE
      );
```

**Locate** (this message carries the banned word — reword it while adding the code; the reworded prose stays technically accurate and only ever renders inside collapsed details/logs):

```ts
      throw this._createStageError(
        'preparing',
        'Unable to determine the latest certified immutable number from Mithril snapshot metadata.'
      );
```

**Replace with:**

```ts
      throw this._createStageError(
        'preparing',
        'Unable to determine the latest certified range from Mithril snapshot metadata.',
        PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE
      );
```

Explicit no-change list for this file: the latest-drift throw (`PARTIAL_SYNC_LATEST_DRIFT_CODE`), the exit-code throw (`'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED'`), the disk-space throw (`PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE`, CAT-D's), and `_buildError` (all three branches) stay exactly as they are.

## Step 1.11 — Coordinator: disabled/start/layout throws become coded

**File:** `source/main/utils/chainStorageCoordinator.ts`

**Edit A.** Locate:

```ts
const PARTIAL_SYNC_DISABLED_ERROR =
  'Mithril partial sync is disabled by launcher configuration.';
```

**Replace with:**

```ts
const PARTIAL_SYNC_DISABLED_CODE = 'PARTIAL_SYNC_DISABLED';
const PARTIAL_SYNC_ALREADY_RUNNING_CODE = 'PARTIAL_SYNC_ALREADY_RUNNING';
const PARTIAL_SYNC_LAYOUT_UNSUPPORTED_CODE = 'PARTIAL_SYNC_LAYOUT_UNSUPPORTED';
```

**Edit B.** Locate:

```ts
  _assertPartialSyncStartAllowed(): void {
    if (this._bootstrapInProgress) {
      throw new Error(
        'Cannot start Mithril partial sync while Mithril bootstrap is in progress.'
      );
    }

    if (this._partialSyncInProgress) {
      throw new Error('Mithril partial sync is already in progress.');
    }
  }
```

**Replace with:**

```ts
  _assertPartialSyncStartAllowed(): void {
    if (this._bootstrapInProgress) {
      logger.warn(
        '[MITHRIL] Rejecting partial sync start: Mithril bootstrap is in progress',
        null
      );
      throw new Error(PARTIAL_SYNC_ALREADY_RUNNING_CODE);
    }

    if (this._partialSyncInProgress) {
      logger.warn(
        '[MITHRIL] Rejecting partial sync start: a partial sync run is already in progress',
        null
      );
      throw new Error(PARTIAL_SYNC_ALREADY_RUNNING_CODE);
    }
  }
```

**Edit C.** Locate:

```ts
    if (launcherConfig.mithrilPartialSyncEnabled !== true) {
      throw new Error(PARTIAL_SYNC_DISABLED_ERROR);
    }
```

**Replace with:**

```ts
    if (launcherConfig.mithrilPartialSyncEnabled !== true) {
      logger.warn(
        '[MITHRIL] Rejecting partial sync action: partial sync is disabled by launcher configuration',
        null
      );
      throw new Error(PARTIAL_SYNC_DISABLED_CODE);
    }
```

**Edit D.** Locate (the preceding `logger.warn('[MITHRIL] Rejecting partial sync start on recovery fallback layout', …)` block already logs the prose — keep it unchanged):

```ts
          throw new Error(
            'Cannot start Mithril partial sync while chain storage is using recovery fallback state.'
          );
```

**Replace with:**

```ts
          throw new Error(PARTIAL_SYNC_LAYOUT_UNSUPPORTED_CODE);
```

**Edit E.** Locate:

```ts
export const getMithrilPartialSyncDisabledError = (): string =>
  PARTIAL_SYNC_DISABLED_ERROR;
```

**Replace with:**

```ts
export const getMithrilPartialSyncDisabledError = (): string =>
  PARTIAL_SYNC_DISABLED_CODE;
```

(No other consumer of this export exists in `source/` or `tests/` — verified by grep.)

Explicit no-change list for this file: `_assertNodeStopped` / `_ensureNodeStoppedForPartialSyncAction` prose, the bootstrap-side guards ("Cannot start Mithril bootstrap …", "Cannot change chain storage directory …", "Cannot wipe chain storage …"), and the "startup handler is not configured" throw stay as they are — they are either bootstrap-owned or their rejections are no longer user-surfaced after Step 2.7 (the renderer shows the localized fallback for every start rejection).

## Step 1.12 — T19: `MithrilErrorView` body comes only from intl maps

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx`

**Edit A.** Locate:

```ts
  const detailsHeader = error?.message || error?.code || '';
```

**Replace with:**

```ts
  const hasTechnicalDetails = Boolean(error?.message || error?.code);
```

**Edit B.** Locate (the raw-message body leak) and DELETE the whole line:

```tsx
        {error?.message && <p>{error.message}</p>}
```

**Edit C.** Locate:

```tsx
      {detailsHeader && (
        <div className={styles.detailsSection}>
          <CollapsibleSection
            header={detailsHeader}
```

**Replace with:**

```tsx
      {hasTechnicalDetails && (
        <div className={styles.detailsSection}>
          <CollapsibleSection
            header={intl.formatMessage(messages.errorDetailsHeader)}
```

Leave the collapsed-details body untouched — the `{error?.code && …}` div and the `<pre className={styles.errorMessage}>{error.message}</pre>` block are the allowed raw-technical surface (`CollapsibleSection` renders children only when expanded, so the raw message is hidden until the user opens the section).

## Step 1.13 — Chunk-1 test updates

**File:** `source/main/mithril/MithrilPartialSyncService.spec.ts`

- Locate BOTH occurrences of:

  ```ts
      'Mithril partial sync cancellation is no longer allowed after live chain cutover has started.'
  ```

  Replace each occurrence (they sit inside `rejects.toThrow(…)` in the tests named `'rejects cancellation once live cutover has started'` and `'does not emit a status when post-cutover cancel hard-rejects'`) with:

  ```ts
      'PARTIAL_SYNC_CANCEL_NOT_ALLOWED'
  ```

- Locate:

  ```ts
    expect(() => service.assertStartAllowed()).toThrow(
      'Mithril partial sync cannot retry from the current recovery boundary.'
    );
  ```

  Replace the message with `'PARTIAL_SYNC_START_NOT_ALLOWED'`.

- The assertion `'Unable to resolve the latest Mithril snapshot metadata.'` stays valid (the prose message was kept). No edit.

**File:** `source/main/utils/chainStorageCoordinator.spec.ts`

- Locate:

  ```ts
    ).rejects.toThrow(
      'Cannot start Mithril partial sync while chain storage is using recovery fallback state.'
    );
  ```

  Replace the message with `'PARTIAL_SYNC_LAYOUT_UNSUPPORTED'`.

- Locate:

  ```ts
    ).rejects.toThrow(
      'Cannot start Mithril partial sync while Mithril bootstrap is in progress.'
    );
  ```

  Replace the message with `'PARTIAL_SYNC_ALREADY_RUNNING'`.

- Do NOT touch the bootstrap-side assertions ("Cannot start Mithril bootstrap …", "Cannot change chain storage directory …", "Cannot wipe chain storage …", "while cardano-node is stopped") — those throws were not recoded.

**File:** `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts`

- Add to the `it.each` table (after the `PARTIAL_SYNC_CONVERSION_FAILED` row):

  ```ts
    [
      'PARTIAL_SYNC_METADATA_UNAVAILABLE',
      'loading.mithrilPartialSync.error.metadataUnavailable.title',
    ],
  ```

- Append a new test after the `it.each` block:

  ```ts
    it('maps start/cancel/recovery rejection codes to the generic failed copy', () => {
      [
        'PARTIAL_SYNC_DISABLED',
        'PARTIAL_SYNC_ALREADY_RUNNING',
        'PARTIAL_SYNC_START_NOT_ALLOWED',
        'PARTIAL_SYNC_LAYOUT_UNSUPPORTED',
        'PARTIAL_SYNC_CANCEL_NOT_ALLOWED',
        'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED',
      ].forEach((code) => {
        expect(resolvePartialSyncErrorCopy('failed', err(code)).title.id).toBe(
          'loading.mithrilPartialSync.error.failed.title'
        );
      });
    });
  ```

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`

Locate (inside the test `'shows bespoke copy for a mapped error code and never the raw backend message as the title'`):

```ts
    expect(
      screen.queryByRole('heading', { level: 1, name: /mithril-client json/i })
    ).not.toBeInTheDocument();
```

Insert immediately AFTER:

```ts
    // the raw backend message is confined to the collapsed technical-details
    // section (hidden until expanded); the visible header is localized
    expect(
      screen.queryByText('{"raw":"mithril-client json"}')
    ).not.toBeInTheDocument();
    expect(screen.getByText(/technical details/i)).toBeInTheDocument();
```

---

# 2. T1/T17 stage-id labels, T4 shut down typo, T20/T32 ICU plurals, T22 intl fallback message — locale JSON updates and test/snapshot updates

## Step 2.1 — New intl messages: stage labels + shared start-failure fallback

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`

Locate the END of the message table (the tooltip descriptor is the last entry today; if CAT-C/CAT-D appended entries after it, still insert before the closing `});`):

```ts
  partialSyncCancelStoppingTooltip: {
    id: 'loading.mithrilPartialSync.progress.cancelStoppingTooltip',
    defaultMessage: '!!!Cancellation available once the node has stopped',
    description:
      'Tooltip explaining why the Cancel button is disabled while the Cardano node is still stopping (stopping-node phase).',
  },
});
```

Insert the four new descriptors between the final `},` and `});`:

```ts
  partialSyncStageVerifying: {
    id: 'loading.mithrilPartialSync.progress.stageVerifying',
    defaultMessage: '!!!Verifying snapshot...',
    description:
      'Step-indicator label for the verifying stage emitted during Mithril Sync.',
  },
  partialSyncStageConverting: {
    id: 'loading.mithrilPartialSync.progress.stageConverting',
    defaultMessage: '!!!Converting snapshot format...',
    description:
      'Step-indicator label for the converting stage emitted during Mithril Sync.',
  },
  partialSyncStageInstalling: {
    id: 'loading.mithrilPartialSync.progress.stageInstalling',
    defaultMessage: '!!!Installing snapshot...',
    description:
      'Step-indicator label for the installing stage emitted during Mithril Sync.',
  },
  partialSyncStartFailure: {
    id: 'loading.mithrilPartialSync.error.startFailure',
    defaultMessage: '!!!Unable to start Mithril Sync.',
    description:
      'Fallback message shown when a Mithril Sync start request is rejected.',
  },
```

## Step 2.2 — T1/T17: map the real service stage ids in `MithrilStepIndicator`

The partial-sync service emits progress items with ids `verifying` (`_activateProgressStage('verifying')` — upserted with id = label = `'verifying'`), and `converting` / `installing` (`_activatePostVerificationStage`). None of these are in the indicator's maps today, so `renderSubItem` falls through to `item.label` and renders the raw id. The bootstrap service never emits items with these ids (its sub-item ids are `step-1..7`, `install-snapshot`, `cleanup`, `conversion` — all already mapped), so these edits cannot change any bootstrap frame (boundary 11 safe).

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`

**Edit A.** Locate:

```ts
const DOWNLOAD_SUB_IDS = new Set<string>([
  'step-1',
  'step-2',
  'step-3',
  'step-4',
  'step-5',
  'step-6',
  'step-7',
]);
```

**Replace with** (the `verifying` STATUS maps to the `downloading` step in `STATUS_TO_STEP`, so the `verifying` ITEM belongs to the downloading step's sub-ids — this also stops it leaking into the finalizing step's catch-all sub-item group):

```ts
const DOWNLOAD_SUB_IDS = new Set<string>([
  'step-1',
  'step-2',
  'step-3',
  'step-4',
  'step-5',
  'step-6',
  'step-7',
  'verifying',
]);
```

**Edit B.** Locate:

```ts
const FINALIZE_SUB_IDS = new Set<string>([
  'install-snapshot',
  'cleanup',
  'conversion',
]);
```

**Replace with:**

```ts
const FINALIZE_SUB_IDS = new Set<string>([
  'install-snapshot',
  'cleanup',
  'conversion',
  'converting',
  'installing',
]);
```

**Edit C.** Locate:

```ts
  'install-snapshot': 'progressInstallSnapshot',
  cleanup: 'progressCleanup',
  conversion: 'progressConversion',
};
```

**Replace with:**

```ts
  'install-snapshot': 'progressInstallSnapshot',
  cleanup: 'progressCleanup',
  conversion: 'progressConversion',
  verifying: 'partialSyncStageVerifying',
  converting: 'partialSyncStageConverting',
  installing: 'partialSyncStageInstalling',
};
```

## Step 2.3 — T4: "shutdown" → "shut down"

**File:** `source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts`

Locate:

```ts
      '!!!For this process to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks.',
```

Replace `be shutdown.` with `be shut down.` (only that word pair changes).

**File:** `source/renderer/app/i18n/locales/en-US.json` — locate the line starting with:

```json
  "daedalus.diagnostics.dialog.mithrilSyncProcessSummary": "For this process to begin your Cardano node will need to be shutdown.
```

Apply the same `be shutdown.` → `be shut down.` fix. The JA entry is already a proper translation — unchanged.

**Spec updates** — the canonical sentence is asserted verbatim in two specs; apply the same one-word fix in each:

- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx` — locate `'For this process to begin your Cardano node will need to be shutdown.` and fix.
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx` — same locate, same fix.

## Step 2.4 — T20: ICU plural in the proactive prompt body

**File:** `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx`

Locate:

```ts
    defaultMessage: '!!!Your node is about {epochs} epochs behind.',
```

Replace with:

```ts
    defaultMessage:
      '!!!Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind.',
```

(The call site already passes `{ epochs: behindByEpochs }` — no component-logic change.)

**File:** `source/renderer/app/i18n/locales/en-US.json` — locate:

```json
  "daedalus.diagnostics.dialog.mithrilProactivePromptBody": "Your node is about {epochs} epochs behind.",
```

Replace the value with:

```json
  "daedalus.diagnostics.dialog.mithrilProactivePromptBody": "Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind.",
```

**File:** `source/renderer/app/i18n/locales/ja-JP.json` — locate:

```json
  "daedalus.diagnostics.dialog.mithrilProactivePromptBody": "ノードは約{epochs}エポック遅れています。",
```

Replace the value with (Japanese has no singular/plural distinction — the plural collapses to a single `other` branch):

```json
  "daedalus.diagnostics.dialog.mithrilProactivePromptBody": "ノードは約{epochs, plural, other {#エポック}}遅れています。",
```

## Step 2.5 — T32: ICU plural in the diagnostics confirmation body

**File:** `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx`

Locate:

```ts
    defaultMessage:
      '!!!Your node is about {epochs} epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.',
```

Replace with:

```ts
    defaultMessage:
      '!!!Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind. Mithril Sync will restore verified chain data to help your node sync faster.',
```

**File:** `source/renderer/app/i18n/locales/en-US.json` — locate:

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind": "Your node is about {epochs} epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.",
```

Replace the value with:

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind": "Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind. Mithril Sync will restore verified chain data to help your node sync faster.",
```

**File:** `source/renderer/app/i18n/locales/ja-JP.json` — locate:

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind": "ノードは約{epochs}エポック遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。",
```

Replace the value with:

```json
  "daedalus.diagnostics.dialog.mithrilPartialSyncConfirmationBehind": "ノードは約{epochs, plural, other {#エポック}}遅れています。Mithril同期で検証済みのチェーンデータを復元し、ノードの同期を高速化します。",
```

## Step 2.6 — Locale entries: stage labels + start-failure fallback

**File:** `source/renderer/app/i18n/locales/en-US.json`

Locate:

```json
  "loading.mithrilPartialSync.error.stagedDbInvalid.title": "The Mithril snapshot could not be verified",
```

Insert immediately AFTER (alphabetical: `startFailure` sorts between `stagedDbInvalid` and `wipeAndFullSync`):

```json
  "loading.mithrilPartialSync.error.startFailure": "Unable to start Mithril Sync.",
```

Locate:

```json
  "loading.mithrilPartialSync.progress.nodeStoppingTitle": "Stopping Cardano node...",
```

Insert immediately AFTER (alphabetical: `stage*` sorts between `nodeStoppingTitle` and `subtitle`):

```json
  "loading.mithrilPartialSync.progress.stageConverting": "Converting snapshot format...",
  "loading.mithrilPartialSync.progress.stageInstalling": "Installing snapshot...",
  "loading.mithrilPartialSync.progress.stageVerifying": "Verifying snapshot...",
```

**File:** `source/renderer/app/i18n/locales/ja-JP.json`

After `"loading.mithrilPartialSync.error.stagedDbInvalid.title": "Mithrilスナップショットを検証できませんでした",` insert:

```json
  "loading.mithrilPartialSync.error.startFailure": "Mithril同期を開始できませんでした。",
```

After `"loading.mithrilPartialSync.progress.nodeStoppingTitle": "Cardanoノードを停止しています...",` insert:

```json
  "loading.mithrilPartialSync.progress.stageConverting": "スナップショットの形式を変換しています...",
  "loading.mithrilPartialSync.progress.stageInstalling": "スナップショットをインストールしています...",
  "loading.mithrilPartialSync.progress.stageVerifying": "スナップショットを検証しています...",
```

## Step 2.7 — T22: shared start-failure fallback (export + sole consumer)

**File:** `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`

Locate the end of the file:

```ts
  return FAILED;
}
```

Insert immediately AFTER (T22/T23 contract: this export is THE single start-failure fallback; CAT-F's `mithrilErrorMessage.ts` helper must reuse it and never define a second fallback string):

```ts
// Single shared fallback for a rejected Mithril Sync start request; the
// renderer must never surface the raw rejection message.
export const partialSyncStartFailureMessage: MessageDescriptor =
  messages.partialSyncStartFailure;
```

**File:** `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`

**Edit A.** Locate:

```ts
import MithrilPartialSyncConfirmation from './MithrilPartialSyncConfirmation';
```

Insert immediately AFTER:

```ts
import { partialSyncStartFailureMessage } from '../loading/mithril-bootstrap/partialSyncErrorCopy';
```

**Edit B.** Locate (the sole hardcoded fallback):

```ts
      this.setState({
        startError:
          error instanceof Error
            ? error.message
            : 'Unable to start Mithril partial sync.',
      });
```

**Replace with** (rejection messages are now stable codes, not user copy — always show the localized fallback; CAT-F's follow-up helper maps specific codes to specific copy on top of this message):

```ts
      this.setState({
        startError: this.context.intl.formatMessage(
          partialSyncStartFailureMessage
        ),
      });
```

Do NOT touch the analogous catch in `SyncingConnectingMithrilPrompt.tsx` (`'Unable to start Mithril sync.'` at its `handleStart`) — that seam is owned by CAT-F's T23 helper.

## Step 2.8 — Chunk-2 test updates

**File:** `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx`

Append two tests before the closing `});` of the top-level `describe('MithrilStepIndicator', …)` block (after the test `'keeps the install substep active while top-level status remains finalizing'`). These feed the REAL partial-sync service ids (id === label, exactly as `_activateProgressStage`/`_activatePostVerificationStage` upsert them):

```tsx
  it('renders a localized label for the verifying stage item instead of its raw id', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'preparing', state: 'completed' },
      { id: 'downloading', label: 'downloading', state: 'completed' },
      { id: 'verifying', label: 'verifying', state: 'active' },
    ];

    renderComponent('verifying', { progressItems });

    expect(screen.getByText('Verifying snapshot...')).toBeInTheDocument();
    expect(screen.queryByText(/^verifying$/)).not.toBeInTheDocument();
  });

  it('renders localized labels for the converting and installing stage items', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'preparing', state: 'completed' },
      { id: 'downloading', label: 'downloading', state: 'completed' },
      { id: 'verifying', label: 'verifying', state: 'completed' },
      { id: 'converting', label: 'converting', state: 'completed' },
      { id: 'installing', label: 'installing', state: 'active' },
    ];

    renderComponent('installing', { progressItems });

    expect(
      screen.getByText('Converting snapshot format...')
    ).toBeInTheDocument();
    expect(screen.getByText('Installing snapshot...')).toBeInTheDocument();
    expect(screen.queryByText(/^converting$/)).not.toBeInTheDocument();
    expect(screen.queryByText(/^installing$/)).not.toBeInTheDocument();
  });
```

**File:** `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx`

Locate the end of the test `'renders the epochs body and benefit line for a known figure'` (its closing `});`) and insert after it:

```tsx
  it('uses singular epoch phrasing when exactly one epoch behind', () => {
    renderComponent({ behindByEpochs: 1 });

    expect(
      screen.getByText('Your node is about 1 epoch behind.')
    ).toBeInTheDocument();
  });
```

(The existing 3-epochs assertions stay byte-identical — the plural `other` branch reproduces the old string.)

**File:** `source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx`

Locate the end of the test `'renders the epochs-only behind-ness line without any sync-% reference'` and insert after it:

```tsx
  it('uses singular epoch phrasing when exactly one epoch behind', () => {
    renderComponent({ behindByEpochs: 1 });

    expect(
      screen.getByText(
        'Your node is about 1 epoch behind. Mithril Sync will restore verified chain data to help your node sync faster.'
      )
    ).toBeInTheDocument();
  });
```

**File:** `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx`

Locate:

```ts
  it('keeps confirmation open and shows concrete start failure', async () => {
```

Replace the title line with:

```ts
  it('keeps confirmation open and shows the localized start-failure fallback', async () => {
```

Then, inside that test, locate:

```ts
    expect(
      screen.getByText(
        'Mithril partial sync is disabled by launcher configuration.'
      )
    ).toBeInTheDocument();
```

Replace with (the mock's rejection message is intentionally left as-is — the assertion proves it is no longer surfaced):

```ts
    expect(
      screen.getByText('Unable to start Mithril Sync.')
    ).toBeInTheDocument();
    expect(
      screen.queryByText(
        'Mithril partial sync is disabled by launcher configuration.'
      )
    ).not.toBeInTheDocument();
```

Leave `MithrilPartialSyncStore.spec.ts` (mock passthrough of a rejection message — still valid), `MithrilProactivePromptContainer.spec.tsx` and `DaedalusDiagnostics.spec.tsx` (3/8-epoch strings unchanged under the plural `other` branch), and `SyncingConnectingMithrilPrompt.spec.tsx`'s start-rejection test (prompt catch untouched until CAT-F) unmodified.

## Step 2.9 — Regenerate i18n catalogs

Run:

```bash
yarn i18n:manage
```

This re-extracts `translations/messages.json` and regenerates `source/renderer/app/i18n/locales/defaultMessages.json` (both will pick up the new descriptors and the T4/T20/T32 wording changes). Because Steps 1.3/2.3–2.6 added polished locale entries FIRST, the manager must not seed any `!!!` placeholder into `en-US.json`/`ja-JP.json`; if it reports missing keys, fix the locale entry (typo in the key) rather than accepting a seeded placeholder.

## Step 2.10 — Update the JA copy table doc

**File:** `.agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md` (docs, not source — thread IDs still not needed here)

- Row 2 and row 18: update the EN/JA cells to the new ICU-plural strings from Steps 2.4/2.5.
- Row 12: update the EN cell to "… will need to be shut down. …" (JA cell unchanged).
- Append new rows (numbering continues) for: the three stage labels, "Technical details", "Unable to start Mithril Sync.", and the metadata-unavailable title/hint pair, with the JA values from Steps 1.3/2.6 and screen column values "Sync overlay — progress (stage item)", "Sync overlay / bootstrap — error (details header)", "Diagnostics — confirmation modal (start-failure fallback)", "Sync overlay — error (metadata unavailable, title/hint)".

---

# Verification

Run from the repo root (narrow patterns; do not run the full suite):

```bash
yarn test:jest --testPathPattern "MithrilStepIndicator|MithrilErrorView|MithrilPartialSyncOverlay|partialSyncErrorCopy|MithrilBootstrap"
yarn test:jest --testPathPattern "MithrilPartialSyncSection|MithrilPartialSyncConfirmation|SyncingConnectingMithrilPrompt|MithrilProactivePromptContainer|DaedalusDiagnostics|MithrilPartialSyncStore"
yarn test:jest --testPathPattern "MithrilPartialSyncService|chainStorageCoordinator"
yarn i18n:manage
yarn compile
yarn lint
```

Environment note: on Node v24 this repo needs the known local prep before `yarn compile`/jest results are meaningful (regenerate `.scss.d.ts` via typed-scss-modules; gitignored identity-obj-proxy jest sidecar). Apply it before treating a tsc/jest failure as a regression. Do not reformat the two `mithrilCommandRunner` files if prettier oscillates — they carry pre-existing HEAD drift.

Grep gates (all must return no hits):

```bash
grep -rn "epochs behind" source/renderer/app/i18n/locales/en-US.json | grep -v "plural"
grep -n "be shutdown" source/renderer/app/i18n/locales/en-US.json source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts
grep -n "Unable to start Mithril partial sync" source/renderer/app/components/status/MithrilPartialSyncSection.tsx
grep -rn "T16\|CAT-E\|DD-703" source/ --include=*.ts --include=*.tsx | grep -v spec.data
```

# Files touched

- `source/common/types/mithril-partial-sync.types.ts` (union append)
- `source/main/mithril/MithrilPartialSyncService.ts` + `MithrilPartialSyncService.spec.ts`
- `source/main/utils/chainStorageCoordinator.ts` + `chainStorageCoordinator.spec.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` + `partialSyncErrorCopy.spec.ts`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx` + `MithrilStepIndicator.spec.tsx`
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
- `source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts`
- `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` + `MithrilPartialSyncConfirmation.spec.tsx`
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` + `MithrilPartialSyncSection.spec.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` + `SyncingConnectingMithrilPrompt.spec.tsx`
- `source/renderer/app/i18n/locales/en-US.json`, `ja-JP.json`; regenerated: `locales/defaultMessages.json`, `translations/messages.json`
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md`

Never stage/modify `.gitignore` or anything under `.agent/skills/`. No commits, no pushes, no `gh`.

# Out of scope (owned elsewhere)

- N7 redundant `isMithrilPartialSyncEnabled` re-check in `DaedalusDiagnostics.tsx` — CAT-B (T11).
- Finalize-failed copy key (EN+JA) and finalize-failure UI — CAT-C (T13).
- `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` union member + insufficient-disk-space copy key — CAT-D (T18).
- T23 `source/renderer/app/utils/mithrilErrorMessage.ts` helper, including the prompt's own hardcoded fallback in `SyncingConnectingMithrilPrompt.tsx` (`'Unable to start Mithril sync.'`) and code→message mapping of start rejections — CAT-F (which reuses `partialSyncStartFailureMessage` added here).
- T26 spawn-helper extraction (process-group comment + `detached` preservation) — CAT-F.
- N3/N4 — CAT-G.
- Coordinator/service prose throws NOT cited by T16 and not surfaced to users after Step 2.7 (`_assertNodeStopped`, node-stop ensure prose, bootstrap-side guards, "startup handler is not configured"), and `_buildError`'s verbatim message copy (confined to collapsed details by T19) — deliberate no-change.
- `MithrilProgressView.tsx` — untouched (bootstrap completed frame stays byte-identical).

# Acceptance checks

- **T1/T17:** `verifying`/`converting`/`installing` progress items render localized EN labels ("Verifying snapshot...", "Converting snapshot format...", "Installing snapshot...") with JA entries; raw stage ids never render; new indicator tests feed the real service ids; bootstrap fixtures/specs untouched and green.
- **T4:** canonical process-summary sentence reads "shut down" in the descriptor, `en-US.json`, regenerated catalogs, and both verbatim spec assertions.
- **T16 (DD-703-12):** every cited user-reachable throw carries a stable code from the extended `MithrilPartialSyncErrorCode` union; rejection-path throws use code-as-message with prose demoted to `logger.warn`; `chainStorageCoordinator`'s disabled throw is coded (`PARTIAL_SYNC_DISABLED`); metadata stage errors carry `PARTIAL_SYNC_METADATA_UNAVAILABLE` with bespoke intl copy in `partialSyncErrorCopy.ts`; the "certified immutable" wording is gone from the thrown message; no raw mithril-client JSON in any copy (boundary 8).
- **T19:** `MithrilErrorView` body renders only intl-derived title/hint; `error.message` appears only inside the collapsed technical-details section whose header is the localized "Technical details"; overlay spec asserts the raw message is not visible un-expanded; bootstrap specs stay green (boundary 11).
- **T20:** prompt body uses `{epochs, plural, one {# epoch} other {# epochs}}`; "about 1 epoch behind" verified by a new spec; JA collapses to the `other` branch.
- **T22:** the sole hardcoded fallback in `MithrilPartialSyncSection.tsx` is replaced by the shared `partialSyncStartFailureMessage` ("Unable to start Mithril Sync.") exported from `partialSyncErrorCopy.ts`; spec asserts the localized fallback and the non-surfacing of the raw rejection message.
- **T32:** confirmation behind-line uses the same ICU plural; singular spec added; existing 3-epoch assertions unchanged.

# Escalations / brief-code mismatches found

1. **Second hardcoded start-failure fallback exists** at `SyncingConnectingMithrilPrompt.tsx` `handleStart` (`'Unable to start Mithril sync.'`). The brief calls the `MithrilPartialSyncSection` seam "the sole T22 seam" — that phrasing corrects an earlier in-file anchor (`:115`), and the plan-review log assigns the prompt's fallback to CAT-F's T23 helper. Planned accordingly: E does not touch the prompt catch. Escalate only if CAT-F's plan does not cover it.
2. **`PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE` already exists in the service** (CAT-D work in flight) but not yet in the union/copy map at planning time. E's union/copy edits are anchored so they compose with either ordering; if `COPY_BY_CODE` totality fails because D landed the union member without its copy entry, escalate (D's defect) instead of adding D's entry.
3. **Coordinator scope widened beyond `:41-42`:** the brief's example code `PARTIAL_SYNC_LAYOUT_UNSUPPORTED` only matches the coordinator's recovery-fallback throw, and `_assertPartialSyncStartAllowed`'s two prose throws are equally start-path user-reachable, so all four coordinator start-path throws are recoded here. Escalate-on-mismatch: if the orchestrator intended the disabled throw only, drop Edits B/D of Step 1.11 and the two coordinator spec message swaps.
4. **JA uncertainty (flagged, table-pattern-derived, not free guesses):** 「技術的な詳細」 (Technical details — no table analogue), the metadata-unavailable pair (patterned on table rows 43–44), the three stage labels (patterned on rows 24/26 「〜しています...」), and 「Mithril同期を開始できませんでした。」 (patterned on rows 33/44 「〜できませんでした」). Native review recommended at the next JA copy pass.
