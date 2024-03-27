'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const classnames_1 = __importDefault(require('classnames'));
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const cardano_node_types_1 = require('../../../../../common/types/cardano-node.types');
const SyncingConnectingStatus_scss_1 = __importDefault(
  require('./SyncingConnectingStatus.scss')
);
const SyncingProgress_1 = __importDefault(require('./SyncingProgress'));
const messages = (0, react_intl_1.defineMessages)({
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
class SyncingConnectingStatus extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  _getConnectingMessage = () => {
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
      case cardano_node_types_1.CardanoNodeStates.STARTING:
        connectingMessage = messages.starting;
        connectingDescription = messages.startingDescription;
        break;
      case cardano_node_types_1.CardanoNodeStates.STOPPING:
      case cardano_node_types_1.CardanoNodeStates.EXITING:
        connectingMessage = messages.stopping;
        connectingDescription = messages.stoppingDescription;
        break;
      case cardano_node_types_1.CardanoNodeStates.STOPPED:
        connectingMessage = messages.stopped;
        break;
      case cardano_node_types_1.CardanoNodeStates.UPDATING:
        connectingMessage = messages.updating;
        break;
      case cardano_node_types_1.CardanoNodeStates.UPDATED:
        connectingMessage = messages.updated;
        break;
      case cardano_node_types_1.CardanoNodeStates.CRASHED:
      case cardano_node_types_1.CardanoNodeStates.ERRORED:
        connectingMessage = messages.crashed;
        break;
      case cardano_node_types_1.CardanoNodeStates.UNRECOVERABLE:
        connectingMessage = messages.unrecoverable;
        break;
      case cardano_node_types_1.CardanoNodeStates.RUNNING:
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
      cardanoNodeState === cardano_node_types_1.CardanoNodeStates.RUNNING &&
      isVerifyingBlockchain
    ) {
      return react_1.default.createElement(
        'div',
        { className: SyncingConnectingStatus_scss_1.default.component },
        react_1.default.createElement(SyncingProgress_1.default, {
          ...blockSyncProgress,
        })
      );
    }
    const showEllipsis =
      !isConnected && (isNodeStopped || (isTlsCertInvalid && !isNodeStopping));
    const componentStyles = (0, classnames_1.default)([
      SyncingConnectingStatus_scss_1.default.component,
      isConnected
        ? SyncingConnectingStatus_scss_1.default.syncing
        : SyncingConnectingStatus_scss_1.default.connecting,
    ]);
    const headlineStyles = (0, classnames_1.default)([
      SyncingConnectingStatus_scss_1.default.headline,
      showEllipsis
        ? SyncingConnectingStatus_scss_1.default.withoutAnimation
        : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentStyles },
      react_1.default.createElement(
        'h1',
        { className: headlineStyles },
        intl.formatMessage(connectingMessage)
      ),
      react_1.default.createElement(
        'div',
        { className: SyncingConnectingStatus_scss_1.default.description },
        connectingDescription &&
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...{ connectingDescription },
          })
      )
    );
  }
}
exports.default = SyncingConnectingStatus;
//# sourceMappingURL=SyncingConnectingStatus.js.map
