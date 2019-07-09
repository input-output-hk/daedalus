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
    onReportIssueClick={action('onReportIssueClick')}
    onDownloadLogs={action('onDownloadLogs')}
    onGetAvailableVersions={action('onGetAvailableVersions')}
    disableDownloadLogs={boolean('disableDownloadLogs', true)}
  />
);

// .add('Default', () => (
//   <Loading
//     onStatusIconClick={linkTo('Diagnostics', () => 'default')}
//     currencyIcon={currencyIcon}
//     apiIcon={apiIcon}
//     cardanoNodeState={select(
//       'cardanoNodeState',
//       CardanoNodeStates,
//       CardanoNodeStates.STARTING
//     )}
//     availableAppVersion={null}
//     hasBeenConnected={boolean('hasBeenConnected', true)}
//     isConnected={boolean('isConnected', true)}
//     isSynced={boolean('isSynced', true)}
//     isNodeStopping={boolean('isNodeStopping', false)}
//     isNodeStopped={boolean('isNodeStopped', false)}
//     isNotEnoughDiskSpace={boolean('isNotEnoughDiskSpace', false)}
//     isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
//     diskSpaceRequired={`${text('diskSpaceRequired (GB)', 4)} GB`}
//     diskSpaceMissing={`${text('diskSpaceRequired (GB)', 1)} GB`}
//     diskSpaceRecommended={`${text('diskSpaceRequired (GB)', 8)} GB`}
//     syncPercentage={number('syncPercentage', 100)}
//     loadingDataForNextScreenMessage={{
//       id: 'loading.screen.loadingWalletData',
//       defaultMessage: '!!!Loading wallet data',
//       description: 'Message "Loading wallet data" on the loading screen.',
//     }}
//     hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
//     hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
//     localTimeDifference={number('localTimeDifference', 0)}
//     isSystemTimeCorrect={boolean('isSystemTimeCorrect', true)}
//     isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
//     isNodeResponding={boolean('isNodeResponding', true)}
//     isNodeSubscribed={boolean('isNodeSubscribed', true)}
//     isNodeSyncing={boolean('isNodeSyncing', true)}
//     isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
//     currentLocale="en-US"
//     currentAppVersion="1.1.1"
//     isNewAppVersionAvailable={boolean('isNewAppVersionAvailable', false)}
//     isNewAppVersionLoading={boolean('isNewAppVersionLoading', false)}
//     isNewAppVersionLoaded={boolean('isNewAppVersionLoaded', false)}
//     onExternalLinkClick={action('onExternalLinkClick')}
//     onReportIssueClick={action('onReportIssueClick')}
//     onReadSyncIssueArticleClick={action('onReadSyncIssueArticleClick')}
//     onReadConnectivityIssueArticleClick={action(
//       'onReadConnectivityIssueArticleClick'
//     )}
//     onCheckTheTimeAgain={action('onCheckTheTimeAgain')}
//     onContinueWithoutClockSyncCheck={action(
//       'onContinueWithoutClockSyncCheck'
//     )}
//     onDownloadLogs={action('onDownloadLogs')}
//     onGetAvailableVersions={action('onGetAvailableVersions')}
//     disableDownloadLogs={boolean('disableDownloadLogs', true)}
//   />
// ))
// .add('Connectivity Issues', () => (
//   <Loading
//     onStatusIconClick={linkTo('Diagnostics', () => 'default')}
//     currencyIcon={currencyIcon}
//     apiIcon={apiIcon}
//     cardanoNodeState={CardanoNodeStates.UNRECOVERABLE}
//     availableAppVersion={null}
//     hasBeenConnected={boolean('hasBeenConnected', true)}
//     forceConnectivityIssue
//     isConnected={false}
//     isSynced={boolean('isSynced', true)}
//     isNodeStopping={boolean('isNodeStopping', false)}
//     isNodeStopped={boolean('isNodeStopped', false)}
//     isNotEnoughDiskSpace={boolean('isNotEnoughDiskSpace', false)}
//     isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
//     diskSpaceRequired={`${text('diskSpaceRequired (GB)', 4)} GB`}
//     diskSpaceMissing={`${text('diskSpaceRequired (GB)', 1)} GB`}
//     diskSpaceRecommended={`${text('diskSpaceRequired (GB)', 8)} GB`}
//     syncPercentage={number('syncPercentage', 100)}
//     loadingDataForNextScreenMessage={{
//       id: 'loading.screen.loadingWalletData',
//       defaultMessage: '!!!Loading wallet data',
//       description: 'Message "Loading wallet data" on the loading screen.',
//     }}
//     hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
//     hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
//     localTimeDifference={number('localTimeDifference', 0)}
//     isSystemTimeCorrect={boolean('isSystemTimeCorrect', true)}
//     isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
//     isNodeResponding={boolean('isNodeResponding', true)}
//     isNodeSubscribed={boolean('isNodeSubscribed', true)}
//     isNodeSyncing={boolean('isNodeSyncing', true)}
//     isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
//     currentLocale="en-US"
//     currentAppVersion="1.1.1"
//     isNewAppVersionAvailable={false}
//     isNewAppVersionLoading={false}
//     isNewAppVersionLoaded
//     onExternalLinkClick={action('onExternalLinkClick')}
//     onReportIssueClick={action('onReportIssueClick')}
//     onReadSyncIssueArticleClick={action('onReadSyncIssueArticleClick')}
//     onReadConnectivityIssueArticleClick={action(
//       'onReadConnectivityIssueArticleClick'
//     )}
//     onCheckTheTimeAgain={action('onCheckTheTimeAgain')}
//     onContinueWithoutClockSyncCheck={action(
//       'onContinueWithoutClockSyncCheck'
//     )}
//     onDownloadLogs={action('onDownloadLogs')}
//     onGetAvailableVersions={action('onGetAvailableVersions')}
//     disableDownloadLogs={boolean('disableDownloadLogs', true)}
//   />
// ))
// .add('Sync Issues', () => (
//   <Loading
//     onStatusIconClick={linkTo('Diagnostics', () => 'default')}
//     currencyIcon={currencyIcon}
//     apiIcon={apiIcon}
//     cardanoNodeState={CardanoNodeStates.UNRECOVERABLE}
//     availableAppVersion={null}
//     hasBeenConnected={boolean('hasBeenConnected', true)}
//     forceSyncIssue
//     isConnected
//     isSynced={false}
//     isNodeStopping={boolean('isNodeStopping', false)}
//     isNodeStopped={boolean('isNodeStopped', false)}
//     isNotEnoughDiskSpace={boolean('isNotEnoughDiskSpace', false)}
//     isTlsCertInvalid={boolean('isTlsCertInvalid', false)}
//     diskSpaceRequired={`${text('diskSpaceRequired (GB)', 4)} GB`}
//     diskSpaceMissing={`${text('diskSpaceRequired (GB)', 1)} GB`}
//     diskSpaceRecommended={`${text('diskSpaceRequired (GB)', 8)} GB`}
//     syncPercentage={number('syncPercentage', 100)}
//     loadingDataForNextScreenMessage={{
//       id: 'loading.screen.loadingWalletData',
//       defaultMessage: '!!!Loading wallet data',
//       description: 'Message "Loading wallet data" on the loading screen.',
//     }}
//     hasLoadedCurrentLocale={boolean('hasLoadedCurrentLocale', true)}
//     hasLoadedCurrentTheme={boolean('hasLoadedCurrentTheme', true)}
//     localTimeDifference={number('localTimeDifference', 0)}
//     isSystemTimeCorrect={boolean('isSystemTimeCorrect', true)}
//     isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
//     isNodeResponding={boolean('isNodeResponding', true)}
//     isNodeSubscribed={boolean('isNodeSubscribed', true)}
//     isNodeSyncing={boolean('isNodeSyncing', true)}
//     isNodeTimeCorrect={boolean('isNodeTimeCorrect', true)}
//     currentLocale="en-US"
//     currentAppVersion="1.1.1"
//     isNewAppVersionAvailable={false}
//     isNewAppVersionLoading={false}
//     isNewAppVersionLoaded
//     onExternalLinkClick={action('onExternalLinkClick')}
//     onReportIssueClick={action('onReportIssueClick')}
//     onReadSyncIssueArticleClick={action('onReadSyncIssueArticleClick')}
//     onReadConnectivityIssueArticleClick={action(
//       'onReadConnectivityIssueArticleClick'
//     )}
//     onCheckTheTimeAgain={action('onCheckTheTimeAgain')}
//     onContinueWithoutClockSyncCheck={action(
//       'onContinueWithoutClockSyncCheck'
//     )}
//     onDownloadLogs={action('onDownloadLogs')}
//     onGetAvailableVersions={action('onGetAvailableVersions')}
//     disableDownloadLogs={boolean('disableDownloadLogs', true)}
//   />
// ));
