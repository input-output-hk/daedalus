import React from 'react';
import { action } from '@storybook/addon-actions';
import { withKnobs } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import type { MithrilProgressItem } from '../../../../source/common/types/mithril-bootstrap.types';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncStatus,
} from '../../../../source/common/types/mithril-partial-sync.types';
import MithrilPartialSyncOverlay from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';
import {
  loadingBooleanKnob,
  loadingNumberKnob,
  loadingRadiosKnob,
  loadingSelectKnob,
} from '../_support/loadingKnobs';

// The partial-sync service emits one cumulative progress item per stage, in
// this order, with label always equal to the id (the renderer's
// MithrilStepIndicator owns the user-facing copy, keyed by id). Stories must
// send this wire format so a missing id→copy mapping fails loudly as a raw id.
const PARTIAL_SYNC_STAGES = [
  'preparing',
  'downloading',
  'verifying',
  'converting',
  'installing',
  'finalizing',
] as const;

type PartialSyncStage = (typeof PARTIAL_SYNC_STAGES)[number];

const getStageItems = (
  reachedStage: PartialSyncStage,
  reachedState: MithrilProgressItem['state']
): Array<MithrilProgressItem> => {
  const reachedIndex = PARTIAL_SYNC_STAGES.indexOf(reachedStage);
  return PARTIAL_SYNC_STAGES.slice(0, reachedIndex + 1).map((stage, index) => ({
    id: stage,
    label: stage,
    state: index < reachedIndex ? ('completed' as const) : reachedState,
  }));
};

const isPartialSyncStage = (value: string): value is PartialSyncStage =>
  (PARTIAL_SYNC_STAGES as ReadonlyArray<string>).includes(value);

// Mirrors the backend's per-status item state: nothing before start(),
// stage-cumulative while working, carried forward unchanged into
// starting-node/completed, the reached stage marked error on failure, a lone
// cleanup item while cancelling, and reset to empty on cancelled.
const getProgressItemsForStory = (
  status: MithrilPartialSyncStatus,
  error?: MithrilPartialSyncError | null
): Array<MithrilProgressItem> => {
  if (status === 'stopping-node' || status === 'cancelled') {
    return [];
  }
  if (status === 'cancelling') {
    return [{ id: 'cleanup', label: 'cleanup', state: 'active' }];
  }
  if (status === 'starting-node' || status === 'completed') {
    return getStageItems('finalizing', 'active');
  }
  if (status === 'failed') {
    const stage =
      error?.stage && isPartialSyncStage(error.stage)
        ? error.stage
        : 'preparing';
    return getStageItems(stage, 'error');
  }
  if (isPartialSyncStage(status)) {
    return getStageItems(status, 'active');
  }
  return [];
};

const cancelledError: MithrilPartialSyncError = {
  stage: 'preparing',
  message:
    'Partial sync stopped before live chain data was replaced. Your existing database is still available for the selected recovery actions.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const restartAllowedError: MithrilPartialSyncError = {
  stage: 'verifying',
  message:
    'Verification failed before cutover completed, so Daedalus can safely retry Mithril Sync or restart Cardano node normally on the current database.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const wipeOnlyError: MithrilPartialSyncError = {
  stage: 'starting-node',
  message:
    'The staged database was installed but the first Cardano node start did not succeed, so Daedalus must keep recovery on the wipe-and-full-sync path.',
  logPath: '/home/ada/.local/share/Daedalus/mainnet/Logs/cardano-node.log',
};

// Per-stage failure fixtures. Codes resolve through partialSyncErrorCopy.ts:
// downloading/converting/installing map to bespoke title+hint copy by code;
// finalizing carries no code and `finalizing` is absent from COPY_BY_STAGE,
// so it exercises the generic FAILED fallthrough.
const downloadingError: MithrilPartialSyncError = {
  stage: 'downloading',
  code: 'PARTIAL_SYNC_DOWNLOAD_COMMAND_FAILED',
  message:
    'Downloading verified Mithril data failed before any chain data was replaced, so your current database is still intact.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const convertingError: MithrilPartialSyncError = {
  stage: 'converting',
  code: 'PARTIAL_SYNC_CONVERSION_FAILED',
  message:
    'Converting the downloaded snapshot failed before cutover completed, so your current database is still intact.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const installingError: MithrilPartialSyncError = {
  stage: 'installing',
  code: 'PARTIAL_SYNC_STAGED_DB_INVALID',
  message:
    'The staged database failed its integrity check during installation, so Daedalus stopped before replacing your current data.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const finalizingError: MithrilPartialSyncError = {
  stage: 'finalizing',
  message:
    'Finalizing the restored database failed, so Daedalus kept your previous chain data in place.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const baseProps = {
  filesDownloaded: 7,
  filesTotal: 9,
  elapsedSeconds: 645,
  ancillaryBytesDownloaded: 850 * 1024 * 1024,
  ancillaryBytesTotal: 2200 * 1024 * 1024,
  error: null,
  canRetry: false,
  canRestartNormally: false,
  canWipeAndFullSync: false,
  onCancel: action('onCancel'),
  onRetry: action('onRetry'),
  onRestartNormally: action('onRestartNormally'),
  onWipeAndFullSync: action('onWipeAndFullSync'),
  onDismissCompleted: action('onDismissCompleted'),
  onQuit: action('onQuit'),
  onOpenExternalLink: action('onOpenExternalLink'),
};

interface StoryProps {
  status: MithrilPartialSyncStatus;
  error?: MithrilPartialSyncError | null;
  canRetry?: boolean;
  canRestartNormally?: boolean;
  canWipeAndFullSync?: boolean;
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  ancillaryComplete?: boolean;
  onDismissCompleted?: () => void | Promise<void>;
}

function MithrilPartialSyncOverlayStory(props: StoryProps) {
  const { ancillaryBytesTotal } = baseProps;
  const error = props.error || null;

  return (
    <MithrilPartialSyncOverlay
      {...baseProps}
      status={props.status}
      error={error}
      canRetry={props.canRetry || false}
      canRestartNormally={props.canRestartNormally || false}
      canWipeAndFullSync={props.canWipeAndFullSync || false}
      onDismissCompleted={
        props.onDismissCompleted || baseProps.onDismissCompleted
      }
      // The real store pins startedAt once per run and the progress view ticks
      // its own timer from it; wiring the knob through startedAt (instead of
      // the wire-only transferProgress.elapsedSeconds, which the overlay
      // ignores) reproduces the live ticking timer.
      startedAt={
        Date.now() -
        loadingNumberKnob(
          'elapsedSeconds',
          props.elapsedSeconds ?? baseProps.elapsedSeconds
        ) *
          1000
      }
      transferProgress={{
        filesDownloaded: loadingNumberKnob(
          'filesDownloaded',
          props.filesDownloaded ?? baseProps.filesDownloaded
        ),
        filesTotal: loadingNumberKnob(
          'filesTotal',
          props.filesTotal ?? baseProps.filesTotal
        ),
        ancillaryBytesDownloaded: props.ancillaryComplete
          ? ancillaryBytesTotal
          : baseProps.ancillaryBytesDownloaded,
        ancillaryBytesTotal,
      }}
      progressItems={getProgressItemsForStory(props.status, error)}
    />
  );
}

const interactiveStatusOptions: Record<string, MithrilPartialSyncStatus> = {
  'Stopping Node': 'stopping-node',
  Cancelling: 'cancelling',
  Preparing: 'preparing',
  Downloading: 'downloading',
  Verifying: 'verifying',
  Converting: 'converting',
  Installing: 'installing',
  Finalizing: 'finalizing',
  'Starting Node': 'starting-node',
  Completed: 'completed',
  Failed: 'failed',
  Cancelled: 'cancelled',
};

const interactiveErrorPresets: Record<string, MithrilPartialSyncError | null> =
  {
    none: null,
    cancelled: cancelledError,
    'restart-allowed': restartAllowedError,
    'wipe-only': wipeOnlyError,
    downloading: downloadingError,
    converting: convertingError,
    installing: installingError,
    finalizing: finalizingError,
  };

const interactiveErrorOptions = {
  None: 'none',
  Cancelled: 'cancelled',
  'Restart Allowed': 'restart-allowed',
  'Wipe Only': 'wipe-only',
  Downloading: 'downloading',
  Converting: 'converting',
  Installing: 'installing',
  Finalizing: 'finalizing',
};

storiesOf('Loading / Mithril / Partial Sync Overlay', module)
  .addDecorator((story, context) => (
    <StoryDecorator>
      <LoadingOverlayStoryFrame>
        {withKnobs(story, context)}
      </LoadingOverlayStoryFrame>
    </StoryDecorator>
  ))
  .add('Interactive', () => (
    <MithrilPartialSyncOverlayStory
      status={loadingRadiosKnob(
        'status',
        interactiveStatusOptions,
        'converting'
      )}
      error={
        interactiveErrorPresets[
          loadingSelectKnob('errorPreset', interactiveErrorOptions, 'none')
        ]
      }
      canRetry={loadingBooleanKnob('canRetry', false)}
      canRestartNormally={loadingBooleanKnob('canRestartNormally', false)}
      canWipeAndFullSync={loadingBooleanKnob('canWipeAndFullSync', false)}
    />
  ))
  .add('Active Progress', () => (
    <MithrilPartialSyncOverlayStory status="converting" />
  ))
  .add('Cancelled', () => (
    <MithrilPartialSyncOverlayStory
      status="cancelled"
      error={cancelledError}
      canRetry
      canRestartNormally
    />
  ))
  .add('Cancelling', () => (
    <MithrilPartialSyncOverlayStory status="cancelling" />
  ))
  .add('Failed With Restart Allowed', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={restartAllowedError}
      canRetry
      canRestartNormally
    />
  ))
  .add('Failed With Wipe-Only Recovery', () => (
    <MithrilPartialSyncOverlayStory
      status="failed"
      error={wipeOnlyError}
      canWipeAndFullSync
    />
  ))
  .add('Completed', () => (
    <MithrilPartialSyncOverlayStory
      status="completed"
      filesDownloaded={9}
      filesTotal={9}
      elapsedSeconds={845}
      ancillaryComplete
    />
  ))
  .add('Downloading File Count', () => (
    <MithrilPartialSyncOverlayStory
      status="downloading"
      filesDownloaded={4}
      filesTotal={9}
    />
  ))
  .add('Download Progress Bar (Partial)', () => (
    <MithrilPartialSyncOverlayStory
      status="downloading"
      filesDownloaded={6}
      filesTotal={9}
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
  ))
  .add('Completed - Finalize Failed (auto-plays)', () => (
    <MithrilPartialSyncOverlayStory
      status="completed"
      filesDownloaded={9}
      filesTotal={9}
      elapsedSeconds={845}
      ancillaryComplete
      onDismissCompleted={() => {
        action('onDismissCompleted')();
        return Promise.reject(new Error('finalize failed'));
      }}
    />
  ));
