import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import {
  isMithrilSyncBlockingNodeStart,
  isMithrilSyncOverlayStatus,
} from '../../../../common/types/mithril-sync.types';
import type { MithrilSyncStatus } from '../../../../common/types/mithril-sync.types';
import DaedalusDiagnostics from '../../components/status/DaedalusDiagnostics';
import styles from './DaedalusDiagnosticsDialog.scss';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';
import { buildSystemInfo } from '../../utils/buildSystemInfo';
import { formatUptime } from '../../utils/formatUptime';

type Props = InjectedDialogContainerProps;

export const shouldCloseDiagnosticsForPartialSyncOverlay = (
  previousStatus: MithrilSyncStatus,
  nextStatus: MithrilSyncStatus
) =>
  !isMithrilSyncOverlayStatus(previousStatus) &&
  isMithrilSyncOverlayStatus(nextStatus);

@inject('stores', 'actions')
@observer
export class DaedalusDiagnosticsDialog extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  handleForceCheckNetworkClock = () =>
    this.props.actions.networkStatus.forceCheckNetworkClock.trigger();
  handleCopyStateDirectoryPath = () =>
    this.props.actions.networkStatus.copyStateDirectoryPath.trigger();

  componentDidUpdate(prevProps: Props) {
    const { actions, stores } = this.props;

    if (
      shouldCloseDiagnosticsForPartialSyncOverlay(
        prevProps.stores.mithrilSync.status,
        stores.mithrilSync.status
      )
    ) {
      actions.app.closeDaedalusDiagnosticsDialog.trigger();
    }
  }

  render() {
    const { actions, stores } = this.props;
    const { closeDaedalusDiagnosticsDialog } = actions.app;
    const { restartNode } = actions.networkStatus;
    const { app, mithrilSync, networkStatus } = stores;
    const { openExternalLink } = app;
    const {
      // Node state
      cardanoNodeState,
      isNodeResponding,
      isNodeSyncing,
      isNodeInSync,
      isNodeTimeCorrect,
      // Application state
      isConnected,
      isSynced,
      syncPercentage,
      hasBeenConnected,
      localTimeDifference,
      isSystemTimeCorrect,
      isSystemTimeIgnored,
      openStateDirectory,
      getNetworkInfoRequest,
      networkTip,
      localTip,
      environment,
      tlsConfig,
      cardanoNodePID,
      cardanoWalletPID,
      cardanoNodeStartedAt,
      cardanoWalletStartedAt,
      cardanoWalletRestartCount,
      watchdogPid,
      nodeForceKilled,
      lastWalletExitCode,
      nodeSocketWaitMs,
      walletReadyWaitMs,
      stateDirectoryPath,
      getNetworkClockRequest,
    } = networkStatus;
    const systemInfo = buildSystemInfo(environment, networkStatus);
    const {
      network,
      version,
      rendererProcessID,
      mainProcessID,
      isBlankScreenFixActive,
      nodeVersion,
      apiVersion,
      build,
    } = environment;
    const coreInfo = {
      daedalusVersion: version,
      daedalusBuildNumber: build,
      daedalusProcessID: rendererProcessID,
      daedalusMainProcessID: mainProcessID,
      daedalusStateDirectoryPath: stateDirectoryPath,
      isBlankScreenFixActive,
      cardanoNodeVersion: nodeVersion,
      cardanoNodePID,
      cardanoNodeUptime: formatUptime(cardanoNodeStartedAt),
      cardanoWalletVersion: apiVersion,
      cardanoWalletPID,
      cardanoWalletUptime: formatUptime(cardanoWalletStartedAt),
      cardanoWalletRestartCount,
      cardanoWalletApiPort: tlsConfig ? tlsConfig.port : 0,
      cardanoNetwork: network,
      watchdogPid,
      nodeForceKilled,
      lastWalletExitCode,
      nodeSocketWaitMs,
      walletReadyWaitMs,
    };
    return (
      <ReactModal
        isOpen
        closeOnOverlayClick
        onRequestClose={closeDaedalusDiagnosticsDialog.trigger}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <DaedalusDiagnostics
          systemInfo={systemInfo}
          coreInfo={coreInfo}
          cardanoNodeState={cardanoNodeState}
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ systemInfo: SystemInfo; coreInfo: { daedal... Remove this comment to see the full error message
          isDev={environment.isDev}
          isMainnet={environment.isMainnet}
          isStaging={environment.isStaging}
          isTestnet={environment.isTestnet}
          isNodeResponding={isNodeResponding}
          isNodeSyncing={isNodeSyncing}
          isNodeInSync={isNodeInSync}
          isNodeTimeCorrect={isNodeTimeCorrect}
          isConnected={isConnected}
          isSynced={isSynced}
          syncPercentage={syncPercentage}
          hasBeenConnected={hasBeenConnected}
          localTimeDifference={localTimeDifference}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isSystemTimeIgnored={isSystemTimeIgnored}
          isMithrilPartialSyncWorking={mithrilSync.isWorking}
          isMithrilPartialSyncEnabled={mithrilSync.isPartialSyncEnabled}
          isMithrilPartialSyncSignificantlyBehind={
            mithrilSync.isSignificantlyBehind
          }
          isMithrilPartialSyncProbeFailed={mithrilSync.isProbeFailed}
          isMithrilPartialSyncAtOrPastSnapshot={mithrilSync.isAtOrPastSnapshot}
          isMithrilBootstrapActive={
            mithrilSync.flowType === 'bootstrap' &&
            isMithrilSyncBlockingNodeStart(mithrilSync.status)
          }
          onStartMithrilPartialSync={async () => {
            closeDaedalusDiagnosticsDialog.trigger();
            await mithrilSync.startPartialSync();
          }}
          nodeConnectionError={getNetworkInfoRequest.error}
          localTip={localTip}
          networkTip={networkTip}
          certifiedEpoch={mithrilSync.certifiedEpoch}
          isCheckingSystemTime={
            !getNetworkClockRequest.result || getNetworkClockRequest.isExecuting
          }
          isForceCheckingSystemTime={getNetworkClockRequest.isExecutingWithArgs(
            {
              isForceCheck: true,
            }
          )}
          onOpenStateDirectory={openStateDirectory}
          onOpenExternalLink={openExternalLink}
          onRestartNode={restartNode}
          onClose={closeDaedalusDiagnosticsDialog.trigger}
          onCopyStateDirectoryPath={this.handleCopyStateDirectoryPath}
          onForceCheckNetworkClock={this.handleForceCheckNetworkClock}
        />
      </ReactModal>
    );
  }
}

export default DaedalusDiagnosticsDialog;
