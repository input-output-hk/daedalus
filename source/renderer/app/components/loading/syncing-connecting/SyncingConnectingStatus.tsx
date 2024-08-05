import classNames from 'classnames';
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import {
  BlockSyncType,
  CardanoNodeState,
  CardanoNodeStates,
} from '../../../../../common/types/cardano-node.types';
import styles from './SyncingConnectingStatus.scss';
import SyncingProgress from './SyncingProgress';

const messages = defineMessages({
  starting: {
    id: 'loading.screen.startingCardanoMessage',
    defaultMessage: '!!!Starting Cardano node',
    description: 'Message "Starting Cardano node" on the loading screen.',
  },
  startingDescription: {
    id: 'loading.screen.startingCardanoDescription',
    defaultMessage:
      '!!!This process validates the integrity of local blockchain data.',
    description: 'Message "Starting Cardano node" on the loading screen.',
  },
  stopping: {
    id: 'loading.screen.stoppingCardanoMessage',
    defaultMessage: '!!!Stopping Cardano node',
    description: 'Message "Stopping Cardano node" on the loading screen.',
  },
  stoppingDescription: {
    id: 'loading.screen.stoppingCardanoDescription',
    defaultMessage:
      '!!!This process updates the databases and could take several minutes.<br />To preserve data integrity, please wait until this process is complete.',
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

interface Props {
  cardanoNodeState: CardanoNodeState | null | undefined;
  blockSyncProgress: Record<BlockSyncType, number>;
  hasLoadedCurrentLocale: boolean;
  hasBeenConnected: boolean;
  isTlsCertInvalid: boolean;
  isConnected: boolean;
  isNodeStopping: boolean;
  isNodeStopped: boolean;
  isVerifyingBlockchain: boolean;
}

export default class SyncingConnectingStatus extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  _getConnectingMessage = (): {
    connectingMessage: string;
    connectingDescription?: string;
  } => {
    const {
      cardanoNodeState,
      hasBeenConnected,
      isTlsCertInvalid,
      isConnected,
    } = this.props;
    let connectingMessage;

    if (isConnected) {
      connectingMessage = messages.loadingWalletData;
      return {
        connectingMessage,
      };
    }

    let connectingDescription;

    switch (cardanoNodeState) {
      case null:
      case CardanoNodeStates.STARTING:
        connectingMessage = messages.starting;
        connectingDescription = messages.startingDescription;
        break;

      case CardanoNodeStates.STOPPING:
      case CardanoNodeStates.EXITING:
        connectingMessage = messages.stopping;
        connectingDescription = messages.stoppingDescription;
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

      case CardanoNodeStates.RUNNING:
      default:
        connectingMessage = hasBeenConnected
          ? messages.reconnecting
          : messages.connecting;
    }

    const isConnectingMessage =
      connectingMessage === messages.connecting ||
      connectingMessage === messages.reconnecting;

    if (isTlsCertInvalid && isConnectingMessage) {
      connectingMessage = messages.tlsCertificateNotValidError;
    }

    return {
      connectingMessage,
      connectingDescription,
    };
  };

  render() {
    const { intl } = this.context;
    const {
      isConnected,
      isNodeStopping,
      isNodeStopped,
      isTlsCertInvalid,
      isVerifyingBlockchain,
      hasLoadedCurrentLocale,
      blockSyncProgress,
      cardanoNodeState,
    } = this.props;
    if (!hasLoadedCurrentLocale) return null;

    const {
      connectingMessage,
      connectingDescription,
    } = this._getConnectingMessage();

    if (
      cardanoNodeState === CardanoNodeStates.RUNNING &&
      isVerifyingBlockchain
    ) {
      return (
        <div className={styles.component}>
          <SyncingProgress {...blockSyncProgress} />
        </div>
      );
    }

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

    return (
      <div className={componentStyles}>
        <h1 className={headlineStyles}>
          {intl.formatMessage(connectingMessage)}
        </h1>
        <div className={styles.description}>
          {connectingDescription && (
            // @ts-ignore
            <FormattedHTMLMessage {...connectingDescription} />
          )}
        </div>
      </div>
    );
  }
}
