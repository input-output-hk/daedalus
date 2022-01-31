import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SyncingConnecting from '../../components/loading/syncing-connecting/SyncingConnecting';
import { generateSupportRequestLink } from '../../../../common/utils/reporting';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class LoadingSyncingConnectingPage extends Component<Props> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  render() {
    const {
      newsFeed,
      appUpdate,
      networkStatus,
      profile,
      app,
    } = this.props.stores;
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
      verificationProgress,
    } = networkStatus;
    const { displayAppUpdateNewsItem } = appUpdate;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = profile;
    const { toggleNewsFeed } = this.props.actions.app;
    const { unread } = newsFeed.newsFeedData;
    const hasNotification = unread.length > 0;
    return (
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
        onIssueClick={this.handleIssueClick}
        onOpenExternalLink={this.handleOpenExternalLink}
        onStatusIconClick={this.openDaedalusDiagnosticsDialog}
        onDownloadLogs={this.handleDownloadLogs}
        onToggleNewsFeedIconClick={toggleNewsFeed.trigger}
        disableDownloadLogs={app.isDownloadNotificationVisible}
        showNewsFeedIcon={!isNodeStopping && !isNodeStopped}
        isVerifyingBlockchain={isVerifyingBlockchain}
        verificationProgress={verificationProgress}
      />
    );
  }

  handleIssueClick = async (issueButtonUrl: string) => {
    const locale = this.props.stores.profile.currentLocale;
    const { environment } = this.props.stores.app;
    const supportUrl = generateSupportRequestLink(
      issueButtonUrl,
      environment,
      locale
    );
    this.props.stores.app.openExternalLink(supportUrl);
  };
  handleOpenExternalLink = (articleUrl: string) => {
    this.props.stores.app.openExternalLink(articleUrl);
  };
  handleDownloadLogs = () => {
    const { app } = this.props.actions;
    app.downloadLogs.trigger();
    app.setIsDownloadingLogs.trigger(true);
  };
  openDaedalusDiagnosticsDialog = () => {
    const {
      actions: { app },
    } = this.props;
    app.openDaedalusDiagnosticsDialog.trigger();
  };
}

export default LoadingSyncingConnectingPage;
