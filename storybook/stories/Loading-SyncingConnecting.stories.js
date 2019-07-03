// @flow
import React from 'react';
import { number, boolean, radios } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

import SyncingConnecting from '../../source/renderer/app/components/loading/syncing-connecting/SyncingConnecting';
import { CardanoNodeStates } from '../../source/common/types/cardano-node.types';

export const SyncingConnectingStory = () => (
  <SyncingConnecting
    cardanoNodeState={radios(
      'cardanoNodeState',
      CardanoNodeStates,
      CardanoNodeStates.STARTING
    )}
    hasBeenConnected={boolean('hasBeenConnected', true)}
    isConnected={boolean('isConnected', true)}
    isSynced={boolean('isSynced', true)}
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
    onReportIssueClick={action('onReportIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
  />
);
