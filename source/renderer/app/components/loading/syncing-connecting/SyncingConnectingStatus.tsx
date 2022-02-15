import classNames from 'classnames';
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import SVGInline from 'react-svg-inline';
import {
  BlockSyncType,
  CardanoNodeState,
  CardanoNodeStates,
} from '../../../../../common/types/cardano-node.types';
import checkMarkIcon from '../../../assets/images/check-w.inline.svg';
import questionMarkIcon from '../../../assets/images/question-mark.inline.svg';
import spinnerIcon from '../../../assets/images/spinner-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SyncingConnectingStatus.scss... Remove this comment to see the full error message
import styles from './SyncingConnectingStatus.scss';

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
  verifyingBlockchain: {
    id: 'loading.screen.verifyingBlockchainMessage',
    defaultMessage: '!!!Verifying the blockchain',
    description: 'Message "Verifying the blockchain" on the loading screen.',
  },
  verifyingBlockchainDescription: {
    id: 'loading.screen.verifyingBlockchainDescription',
    defaultMessage:
      '!!!This process replays the blockchain from the last saved ledger',
    description:
      'Description of "Verifying the blockchain" message on the loading screen.',
  },
  validatingChunk: {
    id: 'loading.screen.validatingChunk',
    defaultMessage: '!!!Validating blocks',
    description: 'Message "Validating blocks" on the loading screen.',
  },
  validatingChunkDescription: {
    id: 'loading.screen.validatingChunkDescription',
    defaultMessage:
      '!!!This process verifies the integrity of locally stored blockchain',
    description:
      'Description of "Validating blocks" message on the loading screen.',
  },
  pushingLedgerState: {
    id: 'loading.screen.pushingLedgerState',
    defaultMessage: '!!!Applying block',
    description: 'Message "Applying a block to ledger" on the loading screen.',
  },
  pushingLedgerStateDescription: {
    id: 'loading.screen.pushingLedgerStateDescription',
    defaultMessage: '!!!This process applies a block to the ledger',
    description:
      'Description of "Applying block" message on the loading screen.',
  },
});

interface Props {
  cardanoNodeState: CardanoNodeState | null | undefined;
  blockSync: Record<BlockSyncType, number>;
  hasLoadedCurrentLocale: boolean;
  hasBeenConnected: boolean;
  isTlsCertInvalid: boolean;
  isConnected: boolean;
  isNodeStopping: boolean;
  isNodeStopped: boolean;
  isVerifyingBlockchain: boolean;
}

const blockSyncTypesOrdered: Array<BlockSyncType> = [
  BlockSyncType.pushingLedger,
  BlockSyncType.validatingChunk,
  BlockSyncType.replayedBlock,
];

interface MakeBlockSyncIterationCallbackArgs {
  type: BlockSyncType;
  value: number;
}

const makeBlockSyncIteration = (blockSync: Record<BlockSyncType, number>) => (
  cb: (args: MakeBlockSyncIterationCallbackArgs) => any
) => blockSyncTypesOrdered.map((type) => cb({ type, value: blockSync[type] }));

const getDescriptionOfBlockSyncType = (type: BlockSyncType) =>
  (({
    [BlockSyncType.replayedBlock]: messages.verifyingBlockchainDescription,
    [BlockSyncType.validatingChunk]: messages.validatingChunkDescription,
    [BlockSyncType.pushingLedger]: messages.pushingLedgerStateDescription,
  } as Record<BlockSyncType, any>)[type]);

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
      isVerifyingBlockchain,
      isTlsCertInvalid,
      isConnected,
    } = this.props;
    let connectingMessage;

    // if (!isConnected) {
    if (isConnected) {
      connectingMessage = messages.loadingWalletData;
      return {
        connectingMessage,
      };
    }

    let connectingDescription;
    // const cardanoNodeState = CardanoNodeStates.RUNNING;
    // const isVerifyingBlockchain = true;

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

  getBlockSyncMessage = (type: BlockSyncType) => {
    // getBlockSyncMessage = () => {
    switch (type) {
      // switch (this.props.blockSync.type) {
      case 'replayedBlock':
        return messages.verifyingBlockchain;

      case 'pushingLedger':
        return messages.pushingLedgerState;

      case 'validatingChunk':
      default:
        return messages.validatingChunk;
    }
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
      blockSync,
    } = this.props;
    if (!hasLoadedCurrentLocale) return null;

    const {
      connectingMessage,
      connectingDescription,
    } = this._getConnectingMessage();

    const isConnectingMessage =
      connectingMessage === messages.connecting ||
      connectingMessage === messages.reconnecting;

    if (isVerifyingBlockchain && isConnectingMessage) {
      const iterateOverBlockSync = makeBlockSyncIteration(blockSync);

      const componentStyles = classNames(
        styles.component,
        styles.syncingProgresses
      );
      const iconsColumnStyles = classNames(
        styles.syncingProgresses_column,
        styles.syncingProgresses_iconsColumn
      );
      const messagesColumnStyles = classNames(
        styles.syncingProgresses_column,
        styles.syncingProgresses_messagesColumn
      );
      const questionMarkIconStyles = classNames(
        styles.syncingProgresses_icon,
        styles.syncingProgresses_descriptionIcon
      );

      return (
        <div className={componentStyles}>
          <div className={iconsColumnStyles}>
            {iterateOverBlockSync(({ type, value }) => (
              <div key={type} className={styles.syncingProgresses_cell}>
                <SVGInline
                  svg={value < 100 ? spinnerIcon : checkMarkIcon}
                  className={classNames(
                    styles.syncingProgresses_icon,
                    styles.syncingProgresses_faded,
                    {
                      [styles.syncingProgresses_iconRotating]: value < 100,
                    }
                  )}
                />
              </div>
            ))}
          </div>
          <div className={messagesColumnStyles}>
            {iterateOverBlockSync(({ type, value }) => (
              <div key={type} className={styles.syncingProgresses_cell}>
                <span
                  className={classNames({
                    [styles.syncingProgresses_faded]: value === 100,
                  })}
                >
                  {intl.formatMessage(this.getBlockSyncMessage(type))}
                </span>
                <PopOver
                  content={intl.formatMessage(
                    getDescriptionOfBlockSyncType(type)
                  )}
                >
                  <SVGInline
                    svg={questionMarkIcon}
                    className={questionMarkIconStyles}
                  />
                </PopOver>
              </div>
            ))}
          </div>
          <div className={styles.syncingProgresses_column}>
            {iterateOverBlockSync(({ type, value }) => (
              <div
                key={type}
                className={classNames(
                  styles.syncingProgresses_cell,
                  styles.syncingProgresses_cellTextRight,
                  {
                    [styles.syncingProgresses_faded]: value === 100,
                  }
                )}
              >
                {Math.floor(value)}%
              </div>
            ))}
          </div>
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
            <FormattedHTMLMessage {...connectingDescription} />
          )}
        </div>
      </div>
    );
  }
}
