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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const Notification_1 = __importDefault(
  require('../../components/notifications/Notification')
);
const success_small_inline_svg_1 = __importDefault(
  require('../../assets/images/success-small.inline.svg')
);
const spinner_dark_inline_svg_1 = __importDefault(
  require('../../assets/images/spinner-dark.inline.svg')
);
const ICONS = {
  successIcon: success_small_inline_svg_1.default,
  spinnerIcon: spinner_dark_inline_svg_1.default,
};
const messages = (0, react_intl_1.defineMessages)({
  downloadLogsProgress: {
    id: 'notification.downloadLogsProgress',
    defaultMessage: '!!!Preparing logs for download',
    description:
      'Notification for download logs in progress in the Loading and Settings pages.',
  },
  downloadLogsSuccess: {
    id: 'notification.downloadLogsSuccess',
    defaultMessage: '!!!Logs successfully downloaded',
    description:
      'Notification for download logs in the Loading and Settings pages.',
  },
  downloadRewardsCSVSuccess: {
    id: 'notification.downloadRewardsCSVSuccess',
    defaultMessage: '!!!CSV file successfully downloaded',
    description: 'Notification for download Rewards CSV file.',
  },
  downloadTransactionsCSVSuccess: {
    id: 'notification.downloadTransactionsCSVSuccess',
    defaultMessage: '!!!CSV file successfully downloaded',
    description: 'Notification for download Transactions CSV file.',
  },
  copyWalletPublicKey: {
    id: 'notification.copyWalletPublicKey',
    defaultMessage:
      '!!!Public key: <strong>{publicKey}</strong> copied to clipboard',
    description:
      'Notification for the wallet public key copy success in the Wallet Settings page.',
  },
  copyICOPublicKey: {
    id: 'notification.copyICOPublicKey',
    defaultMessage:
      '!!!ICO Public key: <strong>{publicKey}</strong> copied to clipboard',
    description:
      'Notification for the ICO public key copy success in the Wallet Settings page.',
  },
  copyAddress: {
    id: 'notification.copyAddress',
    defaultMessage:
      '!!!Address: <strong>{address}</strong> copied to clipboard',
    description:
      'Notification for the wallet address copy success in the Wallet Receive page.',
  },
  copyAssetParam: {
    id: 'notification.copyAssetParam',
    defaultMessage:
      '!!!{param}: <strong>{shortValue}</strong> copied to clipboard',
    description:
      'Notification for the wallet assetItem copy success in the Wallet Receive page.',
  },
  downloadAddressPDFSuccess: {
    id: 'notification.downloadAddressPDFSuccess',
    defaultMessage:
      '!!!Address: <strong>{walletAddress}</strong> PDF successfully downloaded',
    description:
      'Notification for the wallet address PDF download success in the Wallet Receive page.',
  },
  downloadVotingPDFSuccess: {
    id: 'notification.downloadVotingPDFSuccess',
    defaultMessage: '!!!PDF successfully downloaded',
    description:
      'Notification for the wallet voting PDF download success in the Voting Registration dialog.',
  },
  downloadQRCodeImageSuccess: {
    id: 'notification.downloadQRCodeImageSuccess',
    defaultMessage:
      '!!!Address: <strong>{walletAddress}</strong> QR code image successfully downloaded',
    description:
      'Notification for the wallet address PDF download success in the Wallet Receive page.',
  },
  copyStateDirectoryPath: {
    id: 'notification.copyStateDirectoryPath',
    defaultMessage: '!!!Daedalus state directory copied to clipboard',
    description:
      'Notification for the state directory copy success in the Diagnostics page.',
  },
});
let NotificationsContainer = class NotificationsContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  constructor(props) {
    super(props);
    this.registerNotifications();
  }
  notificationsConfig = [
    {
      id: 'downloadLogsProgress',
      actionToListenAndOpen: this.props.actions.profile.downloadLogs,
      actionToListenAndClose: this.props.actions.profile.downloadLogsSuccess,
    },
    {
      id: 'downloadLogsSuccess',
      actionToListenAndOpen: this.props.actions.profile.downloadLogsSuccess,
      actionToListenAndClose: this.props.actions.profile.downloadLogs,
    },
    {
      id: 'downloadRewardsCSVSuccess',
      actionToListenAndOpen: this.props.actions.staking.requestCSVFileSuccess,
      actionToListenAndClose: this.props.actions.staking.requestCSVFile,
    },
    {
      id: 'downloadTransactionsCSVSuccess',
      actionToListenAndOpen: this.props.actions.transactions
        .requestCSVFileSuccess,
      actionToListenAndClose: this.props.actions.transactions.requestCSVFile,
    },
    {
      id: 'copyWalletPublicKey',
      actionToListenAndOpen: this.props.actions.wallets.copyWalletPublicKey,
    },
    {
      id: 'copyICOPublicKey',
      actionToListenAndOpen: this.props.actions.wallets.copyICOPublicKey,
    },
    {
      id: 'copyAddress',
      actionToListenAndOpen: this.props.actions.wallets.copyAddress,
    },
    {
      id: 'downloadAddressPDFSuccess',
      actionToListenAndOpen: this.props.actions.wallets
        .generateAddressPDFSuccess,
      actionToListenAndClose: this.props.actions.wallets.generateAddressPDF,
    },
    {
      id: 'downloadVotingPDFSuccess',
      actionToListenAndOpen: this.props.actions.voting.saveAsPDFSuccess,
      actionToListenAndClose: this.props.actions.voting.saveAsPDF,
    },
    {
      id: 'downloadQRCodeImageSuccess',
      actionToListenAndOpen: this.props.actions.wallets.saveQRCodeImageSuccess,
      actionToListenAndClose: this.props.actions.wallets.saveQRCodeImage,
    },
    {
      id: 'copyStateDirectoryPath',
      actionToListenAndOpen: this.props.actions.networkStatus
        .copyStateDirectoryPath,
    },
    {
      id: 'copyAssetParam',
      actionToListenAndOpen: this.props.actions.assets
        .copyAssetParamNotification,
    },
  ];
  // @ts-ignore ts-migrate(2740) FIXME: Type '{ downloadLogsProgress: { icon: string; hasE... Remove this comment to see the full error message
  notificationsData = {
    downloadLogsProgress: {
      icon: 'spinner',
      hasEllipsis: true,
      clickToClose: false,
    },
  };
  registerNotifications = () => {
    const { registerNotification } = this.props.actions.notifications;
    this.notificationsConfig.forEach((notificationConfig) =>
      registerNotification.trigger(notificationConfig)
    );
  };
  getIcon = (icon = 'success') => (icon ? ICONS[`${icon}Icon`] : icon);
  getLabel = (id, labelValues) => {
    const values = typeof labelValues === 'object' ? labelValues : {};
    return react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
      ...messages[id],
      values: values,
    });
  };
  render() {
    const { stores, actions } = this.props;
    const { closeNotification } = actions.notifications;
    const { activeNotifications } = stores.uiNotifications;
    return react_1.default.createElement(
      'div',
      null,
      this.notificationsConfig.map(({ id }) => {
        const isVisible = id in activeNotifications;
        const data = this.notificationsData[id] || {};
        // @ts-ignore ts-migrate(2525) FIXME: Initializer provides no value for this binding ele... Remove this comment to see the full error message
        const { labelValues, index } = isVisible ? activeNotifications[id] : {};
        const { icon } = data || {};
        const hasSpinner = icon === 'spinner';
        return react_1.default.createElement(
          Notification_1.default,
          {
            key: id,
            ...data,
            onClose: () =>
              closeNotification.trigger({
                id,
              }),
            icon: this.getIcon(icon),
            isVisible: isVisible,
            hasSpinner: hasSpinner,
            index: index || 0,
          },
          isVisible ? this.getLabel(id, labelValues) : null
        );
      })
    );
  }
};
NotificationsContainer = __decorate(
  [
    (0, mobx_react_1.inject)('stores', 'actions'),
    mobx_react_1.observer,
    __metadata('design:paramtypes', [Object]),
  ],
  NotificationsContainer
);
exports.default = NotificationsContainer;
//# sourceMappingURL=NotificationsContainer.js.map
