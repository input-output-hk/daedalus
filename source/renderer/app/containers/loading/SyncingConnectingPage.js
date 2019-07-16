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
    const { stores } = this.props;
    const {
      cardanoNodeState,
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeTimeCorrect,
      isConnected,
      isSynced,
      syncPercentage,
      hasBeenConnected,
      forceCheckTimeDifferenceRequest,
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

    return (
      <SyncingConnecting
        cardanoNodeState={cardanoNodeState}
        hasBeenConnected={hasBeenConnected}
        isConnected={isConnected}
        isSynced={isSynced}
        isConnecting={!isConnected}
        isSyncing={isConnected && !isSynced}
        isNodeStopping={isNodeStopping}
        isNodeStopped={isNodeStopped}
        isNotEnoughDiskSpace={isNotEnoughDiskSpace}
        isTlsCertInvalid={isTlsCertInvalid}
        syncPercentage={syncPercentage}
        hasLoadedCurrentLocale={hasLoadedCurrentLocale}
        hasLoadedCurrentTheme={hasLoadedCurrentTheme}
        isCheckingSystemTime={forceCheckTimeDifferenceRequest.isExecuting}
        isNodeResponding={isNodeResponding}
        isNodeSubscribed={isNodeSubscribed}
        isNodeSyncing={isNodeSyncing}
        isNodeTimeCorrect={isNodeTimeCorrect}
        isNewAppVersionAvailable={isNewAppVersionAvailable}
        isNewAppVersionLoading={isNewAppVersionLoading}
        isNewAppVersionLoaded={isNewAppVersionLoaded}
        onIssueClick={this.handleIssueClick}
        onGetAvailableVersions={this.handleGetAvailableVersions}
        onStatusIconClick={this.openDaedalusDiagnosticsDialog}
        onDownloadLogs={this.handleDownloadLogs}
        disableDownloadLogs={stores.app.isDownloadNotificationVisible}
      />
    );
  }

  handleIssueClick = async (issueButtonUrl: string) => {
    const supportUrl = generateSupportRequestLink(issueButtonUrl);
    this.props.stores.app.openExternalLink(supportUrl);
  };

  handleDownloadLogs = () => {
    const { app } = this.props.actions;
    app.downloadLogs.trigger();
    app.setNotificationVisibility.trigger(true);
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
