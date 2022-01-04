import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import DaedalusDiagnostics from '../../components/status/DaedalusDiagnostics';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DaedalusDiagnosticsDialog.sc... Remove this comment to see the full error message
import styles from './DaedalusDiagnosticsDialog.scss';
import { formattedBytesToSize } from '../../utils/formatters';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';

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
      diskSpaceAvailable,
      tlsConfig,
      cardanoNodePID,
      cardanoWalletPID,
      stateDirectoryPath,
      getNetworkClockRequest,
    } = networkStatus;
    const systemInfo = {
      platform: environment.os,
      platformVersion: environment.platformVersion,
      cpu: Array.isArray(environment.cpu) ? environment.cpu[0].model : '',
      ram: formattedBytesToSize(environment.ram),
      availableDiskSpace: diskSpaceAvailable,
    };
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
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ systemInfo: { platform: any; platformVersi... Remove this comment to see the full error message
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
