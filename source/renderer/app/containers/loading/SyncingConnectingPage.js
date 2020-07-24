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
    const { isIncentivizedTestnet, isShelleyTestnet, isFlight } = global;
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
    } = networkStatus;
    const {
      isNewAppVersionAvailable,
      isNewAppVersionLoaded,
      displayAppUpdateNewsItem,
    } = appUpdate;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = profile;
    const { toggleNewsFeed } = this.props.actions.app;
    const { unread } = newsFeed.newsFeedData;
    const hasNotification = unread.length > 0;

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
        isNewAppVersionAvailable={isNewAppVersionAvailable}
        isNewAppVersionLoaded={isNewAppVersionLoaded}
        isIncentivizedTestnet={isIncentivizedTestnet}
        isShelleyTestnet={isShelleyTestnet}
        onIssueClick={this.handleIssueClick}
        onOpenExternalLink={this.handleOpenExternalLink}
        onGetAvailableVersions={this.handleGetAvailableVersions}
        onStatusIconClick={this.openDaedalusDiagnosticsDialog}
        onDownloadLogs={this.handleDownloadLogs}
        onToggleNewsFeedIconClick={toggleNewsFeed.trigger}
        disableDownloadLogs={app.isDownloadNotificationVisible}
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
    const { appUpdate } = this.props.actions;
    appUpdate.getLatestAvailableAppVersion.trigger();
  };

  openDaedalusDiagnosticsDialog = () => {
    const {
      actions: { app },
    } = this.props;
    app.openDaedalusDiagnosticsDialog.trigger();
  };
}
