// @ts-nocheck
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import DaedalusDiagnostics from '../../components/status/DaedalusDiagnostics';
import styles from './DaedalusDiagnosticsDialog.scss';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';
import { buildSystemInfo } from '../../utils/buildSystemInfo';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class DaedalusDiagnosticsDialog extends Component<Props> {
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

  render() {
    const { actions, stores } = this.props;
    const { closeDaedalusDiagnosticsDialog } = actions.app;
    const { restartNode } = actions.networkStatus;
    const { app, networkStatus } = stores;
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
      cardanoWalletVersion: apiVersion,
      cardanoWalletPID,
      cardanoWalletApiPort: tlsConfig ? tlsConfig.port : 0,
      cardanoNetwork: network,
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
