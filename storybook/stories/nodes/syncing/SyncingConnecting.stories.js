// @flow
import React from 'react';
import { boolean, radios } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';
import { isIncentivizedTestnetTheme } from '../../_support/utils';

import SyncingConnecting from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnecting';
import { CardanoNodeStates } from '../../../../source/common/types/cardano-node.types';

export const DefaultSyncingConnectingStory = (props: {
  currentTheme: string,
}) => (
  <SyncingConnecting
    hasUnreadAlerts={false}
    hasUnreadAnnouncements={false}
    hasUnreadNews={false}
    onToggleNewsFeedIconClick={action('onToggleNewsFeedIconClick')}
    cardanoNodeState={radios(
      'cardanoNodeState',
      CardanoNodeStates,
      CardanoNodeStates.STARTING
    )}
    hasBeenConnected={boolean('hasBeenConnected', false)}
    isFlight={false}
    isConnected={boolean('isConnected', false)}
    isSynced={boolean('isSynced', false)}
    isConnecting={boolean('isConnecting', true)}
    isSyncing={boolean('isSyncing', false)}
    isSyncProgressStalling={boolean('isSyncProgressStalling', false)}
    isNodeStopping={boolean('isNodeStopping', false)}
    isNodeStopped={boolean('isNodeStopped', false)}
    isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
    hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
    hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
    isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
    isNodeResponding={boolean('isNodeResponding', false)}
    isNodeSubscribed={boolean('isNodeSubscribed', false)}
    isNodeSyncing={boolean('isNodeSyncing', false)}
    isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
    isNewAppVersionAvailable={boolean('isNewAppVersionAvailable', false)}
    isNewAppVersionLoading={boolean('isNewAppVersionLoading', false)}
    isNewAppVersionLoaded={boolean('isNewAppVersionLoaded', false)}
    onIssueClick={action('onIssueClick')}
    onOpenExternalLink={action('onOpenExternalLink')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
    showNewsFeedIcon
    isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
  />
);

export const ConnectivityIssuesSyncingConnectingStory = (props: {
  currentTheme: string,
}) => (
  <SyncingConnecting
    hasUnreadAlerts={false}
    hasUnreadAnnouncements={false}
    hasUnreadNews={false}
    onToggleNewsFeedIconClick={action('onToggleNewsFeedIconClick')}
    forceConnectivityIssue
    isConnected={false}
    cardanoNodeState={CardanoNodeStates.RUNNING}
    hasBeenConnected
    isFlight={false}
    isSynced={false}
    isConnecting
    isSyncing={false}
    isSyncProgressStalling={false}
    isNodeStopping={false}
    isNodeStopped={false}
    isTlsCertInvalid={false}
    hasLoadedCurrentLocale
    hasLoadedCurrentTheme
    isCheckingSystemTime={false}
    isNodeResponding
    isNodeSubscribed={false}
    isNodeSyncing={false}
    isNodeTimeCorrect
    isNewAppVersionAvailable={false}
    isNewAppVersionLoading={false}
    isNewAppVersionLoaded
    onIssueClick={action('onIssueClick')}
    onOpenExternalLink={action('onOpenExternalLink')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', false)}
    showNewsFeedIcon
    isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
  />
);

export const LoadingWalletDataSyncingConnectingStory = (props: {
  currentTheme: string,
}) => (
  <SyncingConnecting
    hasUnreadAlerts={false}
    hasUnreadAnnouncements={false}
    hasUnreadNews={false}
    onToggleNewsFeedIconClick={action('onToggleNewsFeedIconClick')}
    isConnected
    cardanoNodeState={CardanoNodeStates.RUNNING}
    hasBeenConnected
    isFlight={false}
    isSynced={false}
    isConnecting={false}
    isSyncing
    isSyncProgressStalling={false}
    isNodeStopping={false}
    isNodeStopped={false}
    isTlsCertInvalid={false}
    hasLoadedCurrentLocale
    hasLoadedCurrentTheme
    isCheckingSystemTime={false}
    isNodeResponding
    isNodeSubscribed
    isNodeSyncing
    isNodeTimeCorrect
    isNewAppVersionAvailable={false}
    isNewAppVersionLoading={false}
    isNewAppVersionLoaded
    onIssueClick={action('onIssueClick')}
    onOpenExternalLink={action('onOpenExternalLink')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={false}
    showNewsFeedIcon
    isIncentivizedTestnet={isIncentivizedTestnetTheme(props.currentTheme)}
  />
);
