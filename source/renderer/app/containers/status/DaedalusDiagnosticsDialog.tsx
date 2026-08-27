import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import DaedalusDiagnostics from '../../components/status/DaedalusDiagnostics';
import styles from './DaedalusDiagnosticsDialog.scss';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';
import { buildSystemInfo } from '../../utils/buildSystemInfo';
import { formatUptime } from '../../utils/formatUptime';

type Props = InjectedDialogContainerProps;

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
  handleLaunchDapp = async (
    url: string,
    walletId: string,
    localName: string
  ): Promise<void> => {
    await this.props.stores.dapp.launchDiagnostics(url, walletId, localName);
    this.props.actions.app.closeDaedalusDiagnosticsDialog.trigger();
  };

  render() {
    const { actions, stores } = this.props;
    const { closeDaedalusDiagnosticsDialog } = actions.app;
    const { app, dapp, networkStatus, backend, wallets } = stores;
    const { openExternalLink } = app;
    const {
      isNodeResponding,
      isNodeSyncing,
      isNodeInSync,
      isNodeTimeCorrect,
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
      stateDirectoryPath,
      getNetworkClockRequest,
    } = networkStatus;
    const {
      nodePid: cardanoNodePID,
      walletPid: cardanoWalletPID,
      watchdogPid,
      nodeStartedAt: cardanoNodeStartedAt,
      walletStartedAt: cardanoWalletStartedAt,
      walletRestartCount: cardanoWalletRestartCount,
      walletPort,
      nodeForceKilled,
      lastWalletExitCode,
      nodeSocketWaitMs,
      walletReadyWaitMs,
    } = backend;
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
      cardanoWalletApiPort: walletPort ?? 0,
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
          cardanoNodeState={backend.loadingPhase}
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
          nodeConnectionError={getNetworkInfoRequest.error}
          localTip={localTip}
          networkTip={networkTip}
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
          onClose={closeDaedalusDiagnosticsDialog.trigger}
          onForceCheckNetworkClock={this.handleForceCheckNetworkClock}
          onRestartNode={actions.networkStatus.restartNode}
          onRestartWallet={actions.networkStatus.restartWallet}
          onCopyStateDirectoryPath={this.handleCopyStateDirectoryPath}
          diagnosticsWallets={wallets.eligibleDappWallets.map(
            ({ id, name }) => ({ id, name })
          )}
          defaultDiagnosticsWalletId={
            wallets.activeDappWallet?.id ||
            wallets.eligibleDappWallets[0]?.id ||
            ''
          }
          diagnosticsAvailable={dapp.diagnosticsAvailable}
          diagnosticsReady={dapp.diagnosticsReady}
          isDappLaunching={dapp.isLaunching}
          onLaunchDapp={this.handleLaunchDapp}
        />
      </ReactModal>
    );
  }
}

export default DaedalusDiagnosticsDialog;
