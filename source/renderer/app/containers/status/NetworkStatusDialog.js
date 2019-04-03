// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import NetworkStatus from '../../components/status/NetworkStatus';
import styles from './NetworkStatusDialog.scss';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class NetworkStatusDialog extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { closeNetworkStatusDialog } = actions.app;
    const { app, networkStatus } = stores;
    const { openExternalLink } = app;
    const {
      // Node state
      cardanoNodeState,
      isNodeResponding,
      isNodeSubscribed,
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
      forceCheckTimeDifferenceRequest,
      forceCheckLocalTimeDifference,
      getNetworkStatusRequest,
      localBlockHeight,
      networkBlockHeight,
      latestLocalBlockTimestamp,
      latestNetworkBlockTimestamp,
      restartNode,
      isSystemTimeIgnored,
      environment,
    } = networkStatus;

    return (
      <ReactModal
        isOpen
        closeOnOverlayClick
        onRequestClose={closeNetworkStatusDialog.trigger}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <NetworkStatus
          environment={environment}
          cardanoNodeState={cardanoNodeState}
          isDev={environment.isDev}
          isMainnet={environment.isMainnet}
          isStaging={environment.isStaging}
          isTestnet={environment.isTestnet}
          isNodeResponding={isNodeResponding}
          isNodeSubscribed={isNodeSubscribed}
          isNodeSyncing={isNodeSyncing}
          isNodeInSync={isNodeInSync}
          isNodeTimeCorrect={isNodeTimeCorrect}
          isConnected={isConnected}
          isSynced={isSynced}
          syncPercentage={syncPercentage}
          hasBeenConnected={hasBeenConnected}
          localTimeDifference={localTimeDifference}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isForceCheckingNodeTime={forceCheckTimeDifferenceRequest.isExecuting}
          isSystemTimeIgnored={isSystemTimeIgnored}
          latestLocalBlockTimestamp={latestLocalBlockTimestamp}
          latestNetworkBlockTimestamp={latestNetworkBlockTimestamp}
          nodeConnectionError={
            getNetworkStatusRequest.error ||
            forceCheckTimeDifferenceRequest.error
          }
          localBlockHeight={localBlockHeight}
          networkBlockHeight={networkBlockHeight}
          onForceCheckLocalTimeDifference={forceCheckLocalTimeDifference}
          onOpenExternalLink={openExternalLink}
          onRestartNode={restartNode}
          onClose={closeNetworkStatusDialog.trigger}
        />
      </ReactModal>
    );
  }
}
