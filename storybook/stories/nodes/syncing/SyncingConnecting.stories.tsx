import React from 'react';
import { boolean, radios } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';
import SyncingConnecting from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnecting';
import { CardanoNodeStates } from '../../../../source/common/types/cardano-node.types';

export const DefaultSyncingConnectingStory = () => (
  <SyncingConnecting
    hasNotification={false}
    hasUpdate={false}
    isVerifyingBlockchain={false}
    verificationProgress={0}
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
    isNewAppVersionLoaded={boolean('isNewAppVersionLoaded', false)}
    onIssueClick={action('onIssueClick')}
    onOpenExternalLink={action('onOpenExternalLink')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
    showNewsFeedIcon
  />
);
export const ConnectivityIssuesSyncingConnectingStory = () => (
  <SyncingConnecting
    hasNotification={false}
    hasUpdate={false}
    isVerifyingBlockchain={false}
    verificationProgress={0}
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    hasUnreadAlerts={false}
    hasUnreadAnnouncements={false}
    hasUnreadNews={false}
    onToggleNewsFeedIconClick={action('onToggleNewsFeedIconClick')}
    forceConnectivityIssue
    isConnected={false}
    cardanoNodeState={CardanoNodeStates.RUNNING}
    hasBeenConnected
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
    isNewAppVersionLoaded
    onIssueClick={action('onIssueClick')}
    onOpenExternalLink={action('onOpenExternalLink')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={boolean('disableDownloadLogs', false)}
    showNewsFeedIcon
  />
);
export const LoadingWalletDataSyncingConnectingStory = () => (
  <SyncingConnecting
    hasNotification={false}
    hasUpdate={false}
    isVerifyingBlockchain={false}
    verificationProgress={0}
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    hasUnreadAlerts={false}
    hasUnreadAnnouncements={false}
    hasUnreadNews={false}
    onToggleNewsFeedIconClick={action('onToggleNewsFeedIconClick')}
    isConnected
    cardanoNodeState={CardanoNodeStates.RUNNING}
    hasBeenConnected
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
    isNewAppVersionLoaded
    onIssueClick={action('onIssueClick')}
    onOpenExternalLink={action('onOpenExternalLink')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    onStatusIconClick={linkTo('Diagnostics', () => 'default')}
    disableDownloadLogs={false}
    showNewsFeedIcon
  />
);
