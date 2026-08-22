import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SyncingConnecting from '../../components/loading/syncing-connecting/SyncingConnecting';
import SyncingConnectingMithrilPrompt from '../../components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';
import { computeBehindByEpochs } from '../../utils/mithrilBehindness';
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
    const { newsFeed, appUpdate, networkStatus, profile, app, backend } =
      this.props.stores;
    const {
      isNodeResponding,
      isNodeSyncing,
      isNodeTimeCorrect,
      isConnected,
      isSynced,
      isSyncProgressStalling,
      hasBeenConnected,
      getNetworkClockRequest,
      isNotEnoughDiskSpace,
      isTlsCertInvalid,
    } = networkStatus;
    const {
      loadingPhase,
      nodeStartupPhase,
      blockSyncProgress,
      mithrilSignificantlyBehind,
      mithrilPromptDismissed,
      startMithrilForce,
      dismissMithrilPrompt,
    } = backend;
    // Map loadingPhase to the cardanoNodeState shape the component expects
    const cardanoNodeState = loadingPhase;
    const isNodeStopping = backend.isStopping;
    const isNodeStopped = false;
    // Node is verifying blockchain when it has started but wallet isn't ready yet
    const isVerifyingBlockchain =
      loadingPhase === 'node-starting' && nodeStartupPhase !== null;
    const { displayAppUpdateNewsItem } = appUpdate;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = profile;
    const { toggleNewsFeed } = this.props.actions.app;
    const { unread } = newsFeed.newsFeedData;
    const hasNotification = unread.length > 0;
    const isInLongReplay =
      (blockSyncProgress.replayedBlock > 0 &&
        blockSyncProgress.replayedBlock < 99) ||
      (blockSyncProgress.validatingChunk > 0 &&
        blockSyncProgress.validatingChunk < 99);
    const showMithrilPrompt =
      !mithrilPromptDismissed &&
      (mithrilSignificantlyBehind !== null ||
        (loadingPhase === 'node-starting' && isInLongReplay));
    const behindByEpochs = mithrilSignificantlyBehind
      ? computeBehindByEpochs(
          mithrilSignificantlyBehind.localImmutableCount,
          mithrilSignificantlyBehind.latestCertifiedImmutable
        )
      : undefined;
    return (
      <>
        {showMithrilPrompt && (
          <SyncingConnectingMithrilPrompt
            behindByEpochs={behindByEpochs}
            onStart={async () => {
              startMithrilForce();
            }}
            onDismiss={dismissMithrilPrompt}
          />
        )}
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
          isNodeResponding={
            isNodeResponding || loadingPhase === 'node-starting'
          }
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
          nodeStartupPhase={nodeStartupPhase}
          blockSyncProgress={blockSyncProgress}
        />
      </>
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
