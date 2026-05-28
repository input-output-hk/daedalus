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
    label: intl.formatMessage(MithrilBootstrapMessages.progressVerifyingDatabase),
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

function MithrilPartialSyncOverlayStory(
  props: StoryProps,
  context: Context
) {
  const { intl } = context;
  const activeProgressItems = getActiveProgressItems(intl);
  const completedProgressItems = getCompletedProgressItems(intl);

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
      progressItems={
        props.completed ? completedProgressItems : activeProgressItems
      }
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
  ));
