import React from 'react';
import { observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SyncingConnecting from '../../components/loading/syncing-connecting/SyncingConnecting';
import { generateSupportRequestLink } from '../../../../common/utils/reporting';

type Props = InjectedProps;

const SyncingConnectingPage: React.FC<Props> = ({ stores, actions }) => {
  const { newsFeed, appUpdate, networkStatus, profile, app } = stores;
  const {
    cardanoNodeState,
    isNodeResponding,
    isNodeSyncing,
    isNodeTimeCorrect,
    isConnected,
    isSynced,
    isSyncProgressStalling,
    hasBeenConnected,
    getNetworkClockRequest,
    isNodeStopping,
    isNodeStopped,
    isNotEnoughDiskSpace,
    isTlsCertInvalid,
    isVerifyingBlockchain,
    blockSyncProgress,
  } = networkStatus;

  const handleIssueClick = async (issueButtonUrl: string) => {
    const locale = stores.profile.currentLocale;
    const { environment } = stores.app;
    const supportUrl = generateSupportRequestLink(
      issueButtonUrl,
      environment,
      locale
    );
    stores.app.openExternalLink(supportUrl);
  };

  const handleOpenExternalLink = (articleUrl: string) => {
    stores.app.openExternalLink(articleUrl);
  };

  const handleDownloadLogs = () => {
    actions.downloadLogs.trigger();
    actions.setIsDownloadingLogs.trigger(true);
  };

  const openDaedalusDiagnosticsDialog = () => {
    actions.app.openDaedalusDiagnosticsDialog.trigger();
  };

  const { displayAppUpdateNewsItem } = appUpdate;
  const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = profile;
  const { toggleNewsFeed } = actions.app;
  const { unread } = newsFeed.newsFeedData;
  const hasNotification = unread.length > 0;

  return (
    stores.networkStatus.cardanoNodeState !== 'unknown' && (
      <SyncingConnecting
        cardanoNodeState={cardanoNodeState}
        hasBeenConnected={hasBeenConnected}
        isConnected={isConnected}
        isSynced={isSynced}
        isConnecting={!isConnected}
        isSyncing={isConnected && !isSynced}
        isSyncProgressStalling={isSyncProgressStalling}
        isNodeStopping={isNodeStopping}
        isNodeStopped={isNodeStopped}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        isNotEnoughDiskSpace={isNotEnoughDiskSpace}
        isTlsCertInvalid={isTlsCertInvalid}
        hasNotification={hasNotification}
        hasUpdate={displayAppUpdateNewsItem}
        hasLoadedCurrentLocale={hasLoadedCurrentLocale}
        hasLoadedCurrentTheme={hasLoadedCurrentTheme}
        isCheckingSystemTime={
          !getNetworkClockRequest.result || getNetworkClockRequest.isExecuting
        }
        isNodeResponding={isNodeResponding}
        isNodeSyncing={isNodeSyncing}
        isNodeTimeCorrect={isNodeTimeCorrect}
        onIssueClick={handleIssueClick}
        onOpenExternalLink={handleOpenExternalLink}
        onStatusIconClick={openDaedalusDiagnosticsDialog}
        onDownloadLogs={handleDownloadLogs}
        onToggleNewsFeedIconClick={toggleNewsFeed.trigger}
        disableDownloadLogs={app.isDownloadNotificationVisible}
        showNewsFeedIcon={!isNodeStopping && !isNodeStopped}
        isVerifyingBlockchain={isVerifyingBlockchain}
        blockSyncProgress={blockSyncProgress}
      />
    )
  );
};

export default observer(SyncingConnectingPage);
