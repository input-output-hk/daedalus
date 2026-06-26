import React from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import { intlShape } from 'react-intl';
import type { MithrilProgressItem } from '../../../../source/common/types/mithril-bootstrap.types';
import type {
  MithrilPartialSyncError,
  MithrilPartialSyncStatus,
} from '../../../../source/common/types/mithril-partial-sync.types';
import MithrilPartialSyncOverlay from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay';
import MithrilBootstrapMessages from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages';
import type { Intl } from '../../../../source/renderer/app/types/i18nTypes';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';

const getActiveProgressItems = (intl: Intl): Array<MithrilProgressItem> => [
  {
    id: 'prepare',
    label: intl.formatMessage(MithrilBootstrapMessages.stepPreparing),
    state: 'completed',
  },
  {
    id: 'download',
    label: intl.formatMessage(MithrilBootstrapMessages.stepDownloading),
    state: 'completed',
  },
  {
    id: 'verify',
    label: intl.formatMessage(
      MithrilBootstrapMessages.progressVerifyingDatabase
    ),
    state: 'completed',
  },
  {
    id: 'convert',
    label: intl.formatMessage(MithrilBootstrapMessages.progressConversion),
    state: 'active',
  },
  {
    id: 'install',
    label: intl.formatMessage(MithrilBootstrapMessages.progressInstallSnapshot),
    state: 'pending',
  },
];

const getCompletedProgressItems = (intl: Intl): Array<MithrilProgressItem> =>
  getActiveProgressItems(intl).map((item) => ({
    ...item,
    state: 'completed' as const,
  }));

// stopping-node frame: nothing has started yet, so every waterfall step is
// pending while the populated stopping-node completion block spins above it.
const getStoppingProgressItems = (intl: Intl): Array<MithrilProgressItem> =>
  getActiveProgressItems(intl).map((item) => ({
    ...item,
    state: 'pending' as const,
  }));

// downloading frame: prepare done, download in flight (drives the file-count +
// progress bar), later steps still pending.
const getDownloadingProgressItems = (intl: Intl): Array<MithrilProgressItem> =>
  getActiveProgressItems(intl).map((item) => {
    if (item.id === 'prepare') {
      return { ...item, state: 'completed' as const };
    }
    if (item.id === 'download') {
      return { ...item, state: 'active' as const };
    }
    return { ...item, state: 'pending' as const };
  });

// Single selector so each story's waterfall matches its status; all variants
// build their labels from getActiveProgressItems(intl), keeping the intl seam at
// render time (labels are never captured at module scope).
const getProgressItemsForStory = (
  intl: Intl,
  status: MithrilPartialSyncStatus,
  completed?: boolean
): Array<MithrilProgressItem> => {
  if (completed || status === 'completed') {
    return getCompletedProgressItems(intl);
  }
  if (status === 'stopping-node') {
    return getStoppingProgressItems(intl);
  }
  if (status === 'downloading') {
    return getDownloadingProgressItems(intl);
  }
  return getActiveProgressItems(intl);
};

const cancelledError: MithrilPartialSyncError = {
  stage: 'preparing',
  code: 'MITHRIL_PARTIAL_SYNC_CANCELLED',
  message:
    'Partial sync stopped before live chain data was replaced. Your existing database is still available for the selected recovery actions.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const restartAllowedError: MithrilPartialSyncError = {
  stage: 'verifying',
  code: 'MITHRIL_PARTIAL_SYNC_VERIFY_FAILED',
  message:
    'Verification failed before cutover completed, so Daedalus can safely retry partial sync or restart Cardano node normally on the current database.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const wipeOnlyError: MithrilPartialSyncError = {
  stage: 'starting-node',
  code: 'MITHRIL_PARTIAL_SYNC_NODE_START_FAILED',
  message:
    'The staged database was installed but the first Cardano node start did not succeed, so Daedalus must keep recovery on the wipe-and-full-sync path.',
  logPath: '/home/ada/.local/share/Daedalus/mainnet/Logs/cardano-node.log',
};

// Per-stage failure fixtures. Codes resolve through partialSyncErrorCopy.ts:
// downloading/converting/installing map to bespoke title+hint copy by code;
// finalizing's code is intentionally absent from COPY_BY_CODE and `finalizing`
// is absent from COPY_BY_STAGE, so it exercises the generic FAILED fallthrough.
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
  code: 'PARTIAL_SYNC_FINALIZE_FAILED',
  message:
    'Finalizing the restored database failed, so Daedalus kept your previous chain data in place.',
  logPath:
    '/home/ada/.local/share/Daedalus/mainnet/Logs/mithril-partial-sync.log',
};

const baseProps = {
  filesDownloaded: 7,
  filesTotal: 9,
  elapsedSeconds: 645,
  ancillaryBytesDownloaded: 0,
  ancillaryBytesTotal: 0,
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

interface Context {
  intl: Intl;
}

interface StoryProps {
  status: MithrilPartialSyncStatus;
  error?: MithrilPartialSyncError | null;
  canRetry?: boolean;
  canRestartNormally?: boolean;
  canWipeAndFullSync?: boolean;
  filesDownloaded?: number;
  filesTotal?: number;
  elapsedSeconds?: number;
  completed?: boolean;
}

function MithrilPartialSyncOverlayStory(props: StoryProps, context: Context) {
  const { intl } = context;

  return (
    <MithrilPartialSyncOverlay
      {...baseProps}
      status={props.status}
      error={props.error || null}
      canRetry={props.canRetry || false}
      canRestartNormally={props.canRestartNormally || false}
      canWipeAndFullSync={props.canWipeAndFullSync || false}
      transferProgress={{
        filesDownloaded: props.filesDownloaded || baseProps.filesDownloaded,
        filesTotal: props.filesTotal || baseProps.filesTotal,
        elapsedSeconds: props.elapsedSeconds || baseProps.elapsedSeconds,
        ancillaryBytesDownloaded: baseProps.ancillaryBytesDownloaded,
        ancillaryBytesTotal: baseProps.ancillaryBytesTotal,
      }}
      progressItems={getProgressItemsForStory(
        intl,
        props.status,
        props.completed
      )}
    />
  );
}

MithrilPartialSyncOverlayStory.contextTypes = {
  intl: intlShape.isRequired,
};

storiesOf('Loading / Mithril / Partial Sync Overlay', module)
  .addDecorator((story) => (
    <StoryDecorator>
      <LoadingOverlayStoryFrame>{story()}</LoadingOverlayStoryFrame>
    </StoryDecorator>
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
      canWipeAndFullSync
    />
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
      completed
    />
  ))
  // Unified file-count progress (download in flight): file counter + download
  // progress bar, Cancel enabled.
  .add('Downloading File Count', () => (
    <MithrilPartialSyncOverlayStory
      status="downloading"
      filesDownloaded={4}
      filesTotal={9}
    />
  ))
  // Populated stopping-node frame: spinning completion block + every waterfall
  // step pending, Cancel disabled with its stopping tooltip.
  .add('Stopping Node', () => (
    <MithrilPartialSyncOverlayStory status="stopping-node" />
  ))
  // Failure stages with the full retry + restart-normally + wipe-and-full-sync
  // 3-action recovery layout (cancelled-vs-failed: the Cancelled story above uses
  // the same can* flags but renders the cancelled copy).
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
