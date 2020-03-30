// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import BigNumber from 'bignumber.js';
import DaedalusDiagnostics from '../../components/status/DaedalusDiagnostics';
import styles from './DaedalusDiagnosticsDialog.scss';
import { formattedBytesToSize } from '../../utils/formatters';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class DaedalusDiagnosticsDialog extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

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
      cardanoNodeID,
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
      rawNetwork,
      version,
      rendererProcessID,
      mainProcessID,
      isBlankScreenFixActive,
      buildNumber,
    } = environment;

    const coreInfo = {
      daedalusVersion: version,
      daedalusProcessID: rendererProcessID,
      daedalusMainProcessID: mainProcessID,
      daedalusStateDirectoryPath: stateDirectoryPath,
      isBlankScreenFixActive,
      cardanoVersion: buildNumber,
      cardanoProcessID: cardanoNodeID,
      cardanoAPIPort: tlsConfig ? tlsConfig.port : 0,
      cardanoNetwork: network,
      cardanoRawNetwork: rawNetwork,
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
          localTimeDifference={
            localTimeDifference
              ? new BigNumber(localTimeDifference).toFormat()
              : localTimeDifference
          }
          isSystemTimeCorrect={isSystemTimeCorrect}
          isSystemTimeIgnored={isSystemTimeIgnored}
          nodeConnectionError={getNetworkInfoRequest.error}
          localTip={localTip}
          networkTip={networkTip}
          isCheckingSystemTime={
            !getNetworkClockRequest.result || getNetworkClockRequest.isExecuting
          }
          onOpenStateDirectory={openStateDirectory}
          onOpenExternalLink={openExternalLink}
          onRestartNode={restartNode}
          onClose={closeDaedalusDiagnosticsDialog.trigger}
          onCopyStateDirectoryPath={this.handleCopyStateDirectoryPath}
        />
      </ReactModal>
    );
  }
}
