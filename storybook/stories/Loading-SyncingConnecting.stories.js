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
    hasBeenConnected={boolean('hasBeenConnected', true)}
    isConnected={boolean('isConnected', true)}
    isSynced={boolean('isSynced', true)}
    isConnecting={boolean('isConnecting', true)}
    isSyncing={boolean('isSyncing', true)}
    isNodeStopping={boolean('isNodeStopping', false)}
    isNodeStopped={boolean('isNodeStopped', false)}
    isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
    syncPercentage={number('syncPercentage', 100)}
    hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
    hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
    isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
    isNodeResponding={boolean('isNodeResponding', true)}
    isNodeSubscribed={boolean('isNodeSubscribed', true)}
    isNodeSyncing={boolean('isNodeSyncing', true)}
    isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
    isNewAppVersionAvailable={boolean('isNewAppVersionAvailable', false)}
    isNewAppVersionLoading={boolean('isNewAppVersionLoading', false)}
    isNewAppVersionLoaded={boolean('isNewAppVersionLoaded', false)}
    onIssueClick={action('onIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
  />
);

export const ConnectivityIssuesSyncingConnectingStory = () => (
  <SyncingConnecting
    forceConnectivityIssue
    isConnected={false}
    cardanoNodeState={radios(
      'cardanoNodeState',
      CardanoNodeStates,
      CardanoNodeStates.STARTING
    )}
    hasBeenConnected={boolean('hasBeenConnected', true)}
    isSynced={boolean('isSynced', true)}
    isConnecting={boolean('isConnecting', true)}
    isSyncing={boolean('isSyncing', true)}
    isNodeStopping={boolean('isNodeStopping', false)}
    isNodeStopped={boolean('isNodeStopped', false)}
    isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
    syncPercentage={number('syncPercentage', 100)}
    hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
    hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
    isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
    isNodeResponding={boolean('isNodeResponding', true)}
    isNodeSubscribed={boolean('isNodeSubscribed', true)}
    isNodeSyncing={boolean('isNodeSyncing', true)}
    isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
    isNewAppVersionAvailable={boolean('isNewAppVersionAvailable', false)}
    isNewAppVersionLoading={boolean('isNewAppVersionLoading', false)}
    isNewAppVersionLoaded={boolean('isNewAppVersionLoaded', false)}
    onIssueClick={action('onIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
  />
);

export const SyncIssuesSyncingConnectingStory = () => (
  <SyncingConnecting
    cardanoNodeState={radios(
      'cardanoNodeState',
      CardanoNodeStates,
      CardanoNodeStates.STARTING
    )}
    hasBeenConnected={boolean('hasBeenConnected', true)}
    isConnected
    isSynced={false}
    isConnecting={boolean('isConnecting', true)}
    isSyncing={boolean('isSyncing', true)}
    isNodeStopping={boolean('isNodeStopping', false)}
    isNodeStopped={boolean('isNodeStopped', false)}
    isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
    syncPercentage={number('syncPercentage', 100)}
    hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
    hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
    isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
    isNodeResponding={boolean('isNodeResponding', true)}
    isNodeSubscribed={boolean('isNodeSubscribed', true)}
    isNodeSyncing={boolean('isNodeSyncing', true)}
    isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
    isNewAppVersionAvailable={boolean('isNewAppVersionAvailable', false)}
    isNewAppVersionLoading={boolean('isNewAppVersionLoading', false)}
    isNewAppVersionLoaded={boolean('isNewAppVersionLoaded', false)}
    onIssueClick={action('onIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
  />
);
