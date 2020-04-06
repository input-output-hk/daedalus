// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SyncingConnecting from '../../components/loading/syncing-connecting/SyncingConnecting';
import { generateSupportRequestLink } from '../../../../common/utils/reporting';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class LoadingSyncingConnectingPage extends Component<Props> {
  static defaultProps = { stores: null, actions: null };

  render() {
    const { isIncentivizedTestnet, isFlight } = global;
    const { stores } = this.props;
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
    } = stores.networkStatus;
    const {
      isNewAppVersionAvailable,
      isNewAppVersionLoading,
      isNewAppVersionLoaded,
    } = stores.nodeUpdate;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = stores.profile;
    const { toggleNewsFeed } = this.props.actions.app;
    const { unread } = stores.newsFeed.newsFeedData;
    const hasUnreadNews = unread.length > 0;

    return (
      <SyncingConnecting
        cardanoNodeState={cardanoNodeState}
        hasBeenConnected={hasBeenConnected}
        isFlight={isFlight}
        isConnected={isConnected}
        isSynced={isSynced}
        isConnecting={!isConnected}
        isSyncing={isConnected && !isSynced}
        isSyncProgressStalling={isSyncProgressStalling}
        isNodeStopping={isNodeStopping}
        isNodeStopped={isNodeStopped}
        isNotEnoughDiskSpace={isNotEnoughDiskSpace}
        isTlsCertInvalid={isTlsCertInvalid}
        hasUnreadNews={hasUnreadNews}
        hasLoadedCurrentLocale={hasLoadedCurrentLocale}
        hasLoadedCurrentTheme={hasLoadedCurrentTheme}
        isCheckingSystemTime={
          !getNetworkClockRequest.result || getNetworkClockRequest.isExecuting
        }
        isNodeResponding={isNodeResponding}
        isNodeSyncing={isNodeSyncing}
        isNodeTimeCorrect={isNodeTimeCorrect}
        isNewAppVersionAvailable={isNewAppVersionAvailable}
        isNewAppVersionLoading={isNewAppVersionLoading}
        isNewAppVersionLoaded={isNewAppVersionLoaded}
        isIncentivizedTestnet={isIncentivizedTestnet}
        onIssueClick={this.handleIssueClick}
        onOpenExternalLink={this.handleOpenExternalLink}
        onGetAvailableVersions={this.handleGetAvailableVersions}
        onStatusIconClick={this.openDaedalusDiagnosticsDialog}
        onDownloadLogs={this.handleDownloadLogs}
        onToggleNewsFeedIconClick={toggleNewsFeed.trigger}
        disableDownloadLogs={stores.app.isDownloadNotificationVisible}
        showNewsFeedIcon={!isNodeStopping && !isNodeStopped}
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

  handleGetAvailableVersions = () => {
    const { nodeUpdate } = this.props.actions;
    nodeUpdate.getLatestAvailableAppVersion.trigger();
  };

  openDaedalusDiagnosticsDialog = () => {
    const {
      actions: { app },
    } = this.props;
    app.openDaedalusDiagnosticsDialog.trigger();
  };
}
