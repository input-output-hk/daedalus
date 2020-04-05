// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import styles from './SyncingConnectingStatus.scss';
import { CardanoNodeStates } from '../../../../../common/types/cardano-node.types';
import type { CardanoNodeState } from '../../../../../common/types/cardano-node.types';

const messages = defineMessages({
  starting: {
    id: 'loading.screen.startingCardanoMessage',
    defaultMessage: '!!!Starting Cardano node',
    description: 'Message "Starting Cardano node" on the loading screen.',
  },
  stopping: {
    id: 'loading.screen.stoppingCardanoMessage',
    defaultMessage: '!!!Stopping Cardano node',
    description: 'Message "Stopping Cardano node" on the loading screen.',
  },
  stopped: {
    id: 'loading.screen.stoppedCardanoMessage',
    defaultMessage: '!!!Cardano node stopped',
    description: 'Message "Cardano node stopped" on the loading screen.',
  },
  updating: {
    id: 'loading.screen.updatingCardanoMessage',
    defaultMessage: '!!!Updating Cardano node',
    description: 'Message "Updating Cardano node" on the loading screen.',
  },
  updated: {
    id: 'loading.screen.updatedCardanoMessage',
    defaultMessage: '!!!Cardano node updated',
    description: 'Message "Cardano node updated" on the loading screen.',
  },
  crashed: {
    id: 'loading.screen.crashedCardanoMessage',
    defaultMessage: '!!!Cardano node crashed',
    description: 'Message "Cardano node crashed" on the loading screen.',
  },
  unrecoverable: {
    id: 'loading.screen.unrecoverableCardanoMessage',
    defaultMessage:
      '!!!Unable to start Cardano node. Please submit a support request.',
    description:
      'Message "Unable to start Cardano node. Please submit a support request." on the loading screen.',
  },
  connecting: {
    id: 'loading.screen.connectingToNetworkMessage',
    defaultMessage: '!!!Connecting to network',
    description: 'Message "Connecting to network" on the loading screen.',
  },
  reconnecting: {
    id: 'loading.screen.reconnectingToNetworkMessage',
    defaultMessage: '!!!Network connection lost - reconnecting',
    description:
      'Message "Network connection lost - reconnecting" on the loading screen.',
  },
  syncing: {
    id: 'loading.screen.syncingBlocksMessage',
    defaultMessage: '!!!Syncing blocks',
    description: 'Message "Syncing blocks" on the loading screen.',
  },
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.',
  },
  tlsCertificateNotValidError: {
    id: 'loading.screen.errors.tlsCertificateNotValidPleaseRestartError',
    defaultMessage: '!!!TLS certificate is not valid, please restart Daedalus.',
    description: 'The TLS cert is not valid and Daedalus should be restarted',
  },
});

type Props = {
  cardanoNodeState: ?CardanoNodeState,
  hasLoadedCurrentLocale: boolean,
  hasBeenConnected: boolean,
  isTlsCertInvalid: boolean,
  isConnected: boolean,
  isSynced: boolean,
  isLoadingWallets: boolean,
  isNodeStopping: boolean,
  isNodeStopped: boolean,
  syncPercentage: number,
};

export default class SyncingConnectingStatus extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  _getConnectingMessage = () => {
    const {
      cardanoNodeState,
      hasBeenConnected,
      isTlsCertInvalid,
      isConnected,
      isSynced,
      isLoadingWallets,
    } = this.props;
    if (isConnected) {
      if (isLoadingWallets || isSynced) return messages.loadingWalletData;
      return messages.syncing;
    }
    let connectingMessage;
    switch (cardanoNodeState) {
      case null:
      case CardanoNodeStates.STARTING:
        connectingMessage = messages.starting;
        break;
      case CardanoNodeStates.STOPPING:
      case CardanoNodeStates.EXITING:
        connectingMessage = messages.stopping;
        break;
      case CardanoNodeStates.STOPPED:
        connectingMessage = messages.stopped;
        break;
      case CardanoNodeStates.UPDATING:
        connectingMessage = messages.updating;
        break;
      case CardanoNodeStates.UPDATED:
        connectingMessage = messages.updated;
        break;
      case CardanoNodeStates.CRASHED:
      case CardanoNodeStates.ERRORED:
        connectingMessage = messages.crashed;
        break;
      case CardanoNodeStates.UNRECOVERABLE:
        connectingMessage = messages.unrecoverable;
        break;
      default:
        // also covers CardanoNodeStates.RUNNING state
        connectingMessage = hasBeenConnected
          ? messages.reconnecting
          : messages.connecting;
    }
    const isConnectingMessage =
      connectingMessage === messages.connecting ||
      connectingMessage === messages.reconnecting;
    if (isTlsCertInvalid && isConnectingMessage) {
      return messages.tlsCertificateNotValidError;
    }
    return connectingMessage;
  };

  render() {
    const { intl } = this.context;
    const {
      isConnected,
      isSynced,
      isLoadingWallets,
      isNodeStopping,
      isNodeStopped,
      isTlsCertInvalid,
      syncPercentage,
      hasLoadedCurrentLocale,
    } = this.props;

    if (!hasLoadedCurrentLocale) return null;

    const showEllipsis =
      !isConnected && (isNodeStopped || (isTlsCertInvalid && !isNodeStopping));

    const componentStyles = classNames([
      styles.component,
      isConnected ? styles.syncing : styles.connecting,
    ]);

    const headlineStyles = classNames([
      styles.headline,
      showEllipsis ? styles.withoutAnimation : null,
    ]);

    const syncPercentageDisplay =
      isConnected && !isSynced && !isLoadingWallets
        ? `${syncPercentage.toFixed(2)}%`
        : null;

    return (
      <div className={componentStyles}>
        <h1 className={headlineStyles}>
          {intl.formatMessage(this._getConnectingMessage())}{' '}
          {syncPercentageDisplay}
        </h1>
      </div>
    );
  }
}
