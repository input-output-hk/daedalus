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
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const WalletAdd_scss_1 = __importDefault(require('./WalletAdd.scss'));
const BigButtonForDialogs_1 = __importDefault(
  require('../widgets/BigButtonForDialogs')
);
const create_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/create-ic.inline.svg')
);
const import_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/import-ic.inline.svg')
);
const connect_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/connect-ic.inline.svg')
);
const restore_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/restore-ic.inline.svg')
);
const numbersConfig_1 = require('../../config/numbersConfig');
const hardwareWalletsConfig_1 = require('../../config/hardwareWalletsConfig');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.add.dialog.title.label',
    defaultMessage: '!!!Add wallet',
    description: 'Label for the "Add wallet" title on the wallet add dialog.',
  },
  createLabel: {
    id: 'wallet.add.dialog.create.label',
    defaultMessage: '!!!Create',
    description: 'Label for the "Create" button on the wallet add dialog.',
  },
  createDescription: {
    id: 'wallet.add.dialog.create.description',
    defaultMessage: '!!!Create a new wallet',
    description:
      'Description for the "Create a new wallet" button on the wallet add dialog.',
  },
  joinLabel: {
    id: 'wallet.add.dialog.join.label',
    defaultMessage: '!!!Join',
    description: 'Label for the "Join" button on the wallet add dialog.',
  },
  joinDescription: {
    id: 'wallet.add.dialog.join.description',
    defaultMessage: '!!!Join a shared wallet with up to 5 people',
    description: 'Description for the "Join" button on the wallet add dialog.',
  },
  connectLabel: {
    id: 'wallet.add.dialog.connect.label',
    defaultMessage: '!!!Pair',
    description: 'Label for the "Connect" button on the wallet add dialog.',
  },
  connectDescription: {
    id: 'wallet.add.dialog.connect.description',
    defaultMessage: '!!!Pair a hardware wallet device',
    description:
      'Description for the "Connect" button on the wallet add dialog.',
  },
  restoreLabel: {
    id: 'wallet.add.dialog.restore.label',
    defaultMessage: '!!!Restore',
    description: 'Label for the "Restore" button on the wallet add dialog.',
  },
  restoreWithCertificateDescription: {
    id: 'wallet.add.dialog.restore.withCertificate.description',
    defaultMessage:
      '!!!Restore a wallet or paper wallet using wallet recovery phrase',
    description:
      'Description for the "Restore" button with paper wallet certificate on the wallet add dialog.',
  },
  restoreWithoutCertificateDescription: {
    id: 'wallet.add.dialog.restore.withoutCertificate.description',
    defaultMessage: '!!!Restore wallet from backup',
    description:
      'Description for the "Restore" button without paper wallet certificate on the wallet add dialog.',
  },
  importLabel: {
    id: 'wallet.add.dialog.import.label',
    defaultMessage: '!!!Import',
    description: 'Label for the "Import" button on the wallet add dialog.',
  },
  importDescription: {
    id: 'wallet.add.dialog.import.description',
    defaultMessage:
      '!!!Import wallets from an earlier version of Daedalus or the Daedalus state directory',
    description:
      'Description for the "Import" button on the wallet add dialog.',
  },
  restoreNotificationMessage: {
    id: 'wallet.add.dialog.restoreNotificationMessage',
    defaultMessage:
      '!!!Wallet restoration is currently in progress. Until it completes, it is not possible to restore or import new wallets.',
    description:
      'Restore notification message shown during async wallet restore on the wallet add screen.',
  },
  maxNumberOfWalletsNotificationMessage: {
    id: 'wallet.add.dialog.maxNumberOfWalletsNotificationMessage',
    defaultMessage:
      '!!!You have reached the maximum of 50 wallets.<br>No more wallets can be added.',
    description:
      '"Maximum number of wallets reached" notification message shown on the wallet add screen if user has 50 wallets.',
  },
});
let WalletAdd = class WalletAdd extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    isMainnet: false,
    isTestnet: false,
  };
  render() {
    const { intl } = this.context;
    const {
      onCreate,
      onRestore,
      onImport,
      onConnect,
      isMaxNumberOfWalletsReached,
      isMainnet,
      isTestnet,
      isProduction,
    } = this.props;
    const componentClasses = (0, classnames_1.default)([
      WalletAdd_scss_1.default.component,
      'WalletAdd',
    ]);
    let activeNotification = null;
    if (isMaxNumberOfWalletsReached) {
      activeNotification = 'maxNumberOfWalletsNotificationMessage';
    }
    return react_1.default.createElement(
      'div',
      { className: componentClasses },
      react_1.default.createElement(
        'div',
        { className: WalletAdd_scss_1.default.buttonsContainer },
        react_1.default.createElement(
          'div',
          { className: WalletAdd_scss_1.default.firstRow },
          react_1.default.createElement(BigButtonForDialogs_1.default, {
            className: 'createWalletButton',
            onClick: onCreate,
            icon: create_ic_inline_svg_1.default,
            label: intl.formatMessage(messages.createLabel),
            description: intl.formatMessage(messages.createDescription),
            isDisabled: isMaxNumberOfWalletsReached,
          }),
          react_1.default.createElement(BigButtonForDialogs_1.default, {
            className: 'connectWalletButton',
            onClick: onConnect,
            icon: connect_ic_inline_svg_1.default,
            label: intl.formatMessage(messages.connectLabel),
            description: intl.formatMessage(messages.connectDescription),
            isDisabled:
              isMaxNumberOfWalletsReached ||
              !hardwareWalletsConfig_1.isHardwareWalletSupportEnabled,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: WalletAdd_scss_1.default.secondRow },
          react_1.default.createElement(BigButtonForDialogs_1.default, {
            className: 'restoreWalletButton',
            onClick: onRestore,
            icon: restore_ic_inline_svg_1.default,
            label: intl.formatMessage(messages.restoreLabel),
            description: intl.formatMessage(
              messages.restoreWithCertificateDescription
            ),
            isDisabled: isMaxNumberOfWalletsReached,
          }),
          react_1.default.createElement(BigButtonForDialogs_1.default, {
            className: 'importWalletButton',
            onClick: onImport,
            icon: import_ic_inline_svg_1.default,
            label: intl.formatMessage(messages.importLabel),
            description: intl.formatMessage(messages.importDescription),
            isDisabled:
              isMaxNumberOfWalletsReached ||
              (isProduction && !(isMainnet || isTestnet)),
          })
        ),
        activeNotification
          ? react_1.default.createElement(
              'div',
              { className: WalletAdd_scss_1.default.notification },
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...messages[activeNotification],
                values: {
                  maxWalletsCount: numbersConfig_1.MAX_ADA_WALLETS_COUNT,
                },
              })
            )
          : null
      )
    );
  }
};
WalletAdd = __decorate([mobx_react_1.observer], WalletAdd);
exports.default = WalletAdd;
//# sourceMappingURL=WalletAdd.js.map
