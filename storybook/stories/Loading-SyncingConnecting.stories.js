// @flow
import React from 'react';
import { number, boolean, radios } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';

import SyncingConnecting from '../../source/renderer/app/components/loading/syncing-connecting/SyncingConnecting';
import { CardanoNodeStates } from '../../source/common/types/cardano-node.types';

export const DefaultSyncingConnectingStory = () => (
  <SyncingConnecting
    cardanoNodeState={radios(
      'cardanoNodeState',
      CardanoNodeStates,
      CardanoNodeStates.STARTING
    )}
    hasBeenConnected={boolean('hasBeenConnected', false)}
    isConnected={boolean('isConnected', false)}
    isSynced={boolean('isSynced', false)}
    isConnecting={boolean('isConnecting', true)}
    isSyncing={boolean('isSyncing', false)}
    isNodeStopping={boolean('isNodeStopping', false)}
    isNodeStopped={boolean('isNodeStopped', false)}
    isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
    syncPercentage={number('syncPercentage', 0)}
    hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
    hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
    isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
    isNodeResponding={boolean('isNodeResponding', false)}
    isNodeSubscribed={boolean('isNodeSubscribed', false)}
    isNodeSyncing={boolean('isNodeSyncing', false)}
    isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
    onIssueClick={action('onIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
  />
);

export const ConnectivityIssuesSyncingConnectingStory = () => (
  <SyncingConnecting
    forceConnectivityIssue
    isConnected={false}
    cardanoNodeState={CardanoNodeStates.RUNNING}
    hasBeenConnected
    isSynced={false}
    isConnecting
    isSyncing={false}
    isNodeStopping={false}
    isNodeStopped={false}
    isTlsCertInvalid={false}
    syncPercentage={0}
    hasLoadedCurrentLocale
    hasLoadedCurrentTheme
    isCheckingSystemTime={false}
    isNodeResponding
    isNodeSubscribed={false}
    isNodeSyncing={false}
    isNodeTimeCorrect
    onIssueClick={action('onIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', false)}
  />
);

export const SyncIssuesSyncingConnectingStory = () => (
  <SyncingConnecting
    forceSyncIssue
    cardanoNodeState={CardanoNodeStates.RUNNING}
    hasBeenConnected
    isConnected
    isSynced={false}
    isConnecting={false}
    isSyncing
    isNodeStopping={false}
    isNodeStopped={false}
    isTlsCertInvalid={false}
    syncPercentage={50}
    hasLoadedCurrentLocale
    hasLoadedCurrentTheme
    isCheckingSystemTime={false}
    isNodeResponding
    isNodeSubscribed
    isNodeSyncing
    isNodeTimeCorrect
    onIssueClick={action('onIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', false)}
  />
);
