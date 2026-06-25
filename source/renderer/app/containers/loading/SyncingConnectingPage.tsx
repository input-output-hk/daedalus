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
      mithrilPartialSync,
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
      blockSyncProgress,
      networkTip,
      localTip,
    } = networkStatus;
    // The proactive Mithril prompt yields to the partial-sync overlay once
    // "Start now" flips status to `stopping-node`: this gate stays true, but the
    // loading routing swaps the overlay in and the prompt unmounts. "Start now"
    // is disabled via the prompt's own `isStarting` flag to prevent a
    // double-start during any brief lingering mount.
    const networkEpoch =
      networkTip && Number.isFinite(networkTip.epoch) ? networkTip.epoch : null;
    const localEpoch =
      localTip && Number.isFinite(localTip.epoch) ? localTip.epoch : null;
    const behindByEpochs =
      networkEpoch !== null && localEpoch !== null
        ? Math.max(1, networkEpoch - localEpoch)
        : undefined;
    const showMithrilPrompt =
      mithrilPartialSync.isPartialSyncEnabled &&
      mithrilPartialSync.isSignificantlyBehind &&
      !mithrilPartialSync.proactivePromptDismissedThisSession;
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
        blockSyncProgress={blockSyncProgress}
        showMithrilPrompt={showMithrilPrompt}
        mithrilBehindByEpochs={behindByEpochs}
        onStartMithrilSync={mithrilPartialSync.startPartialSync}
        onDismissMithrilPrompt={mithrilPartialSync.dismissProactivePrompt}
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
