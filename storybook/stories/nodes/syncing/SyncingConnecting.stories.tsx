import React from 'react';
import { boolean, radios, number } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import { action } from '@storybook/addon-actions';
import SyncingConnecting from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnecting';
import {
  BlockSyncType,
  CardanoNodeStates,
} from '../../../../source/common/types/cardano-node.types';

const makeProgressValueKnob = ({ name, value }) =>
  number(name, value, {
    range: true,
    min: 0,
    max: 100,
    step: 1,
  });

const makeBlockSyncProgress = () => ({
  [BlockSyncType.validatingChunk]: makeProgressValueKnob({
    name: 'Verifying on-disk blockchain state',
    value: 100,
  }),
  [BlockSyncType.replayedBlock]: makeProgressValueKnob({
    name: 'Replaying ledger from on-disk blockchain',
    value: 99,
  }),
  [BlockSyncType.pushingLedger]: makeProgressValueKnob({
    name: 'Syncing blockchain',
    value: 0,
  }),
});

export function DefaultSyncingConnectingStory() {
  return (
    <SyncingConnecting
      hasNotification={false}
      hasUpdate={false}
      isVerifyingBlockchain={boolean('isVerifyingBlockchain', false)}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
      blockSyncProgress={makeBlockSyncProgress()}
    />
  );
}
export function ConnectivityIssuesSyncingConnectingStory() {
  return (
    <SyncingConnecting
      hasNotification={false}
      hasUpdate={false}
      isVerifyingBlockchain={false}
      blockSyncProgress={makeBlockSyncProgress()}
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
}
export function LoadingWalletDataSyncingConnectingStory() {
  return (
    <SyncingConnecting
      hasNotification={false}
      hasUpdate={false}
      isVerifyingBlockchain={false}
      blockSyncProgress={makeBlockSyncProgress()}
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
}
