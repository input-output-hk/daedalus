// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SyncingConnecting from '../../components/loading/syncing-connecting/SyncingConnecting';

type Props = InjectedProps;

@inject('stores')
@observer
export default class LoadingSyncingConnectingPage extends Component<Props> {
  static defaultProps = { stores: null };

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
        onReportIssueClick={this.handleReportIssueClick}
        onGetAvailableVersions={this.handleGetAvailableVersions}
        onDownloadLogs={this.handleDownloadLogs}
        disableDownloadLogs={stores.app.isDownloadNotificationVisible}
      />
    );
  }
}
