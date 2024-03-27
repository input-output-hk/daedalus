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
const react_intl_1 = require('react-intl');
const WalletPublicKeyQRCodeDialog_1 = __importDefault(
  require('../../../../components/wallet/settings/WalletPublicKeyQRCodeDialog')
);
const ICOPublicKeyQRCodeDialog_1 = __importDefault(
  require('../../../../components/wallet/settings/ICOPublicKeyQRCodeDialog')
);
const strings_1 = require('../../../../utils/strings');
const walletsConfig_1 = require('../../../../config/walletsConfig');
const walletMessages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.settings.walletPublicKey',
    defaultMessage: '!!!Wallet Public Key',
    description: 'Title for the "Wallet Public Key QR Code" dialog.',
  },
  copyPublicKeyLabel: {
    id: 'wallet.settings.copyWalletPublicKey',
    defaultMessage: '!!!Copy public key',
    description: 'Copy public key label.',
  },
  derivationPathTooltip: {
    id: 'wallet.settings.dialog.derivationPathTooltip',
    defaultMessage: '!!!Derivation path',
    description: 'Tooltip for the derivation path',
  },
});
let PublicKeyQRCodeDialogContainer = class PublicKeyQRCodeDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleCopyWalletPublicKey = (isICO = false) => {
    const { actions, stores } = this.props;
    const { wallets: walletsAction } = actions;
    const { wallets: walletsStore } = stores;
    const { activePublicKey, icoPublicKey } = walletsStore;
    if ((!activePublicKey && !isICO) || (!icoPublicKey && isICO))
      throw new Error(
        'Active wallet public key required for PublicKeyQRCodeDialogContainer.'
      );
    const publicKey = (0, strings_1.ellipsis)(
      // @ts-ignore Flow cannot detect the previous condition. Hopefully this is solved using Typescript
      isICO ? icoPublicKey : activePublicKey,
      walletsConfig_1.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      walletsConfig_1.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );
    if (isICO)
      walletsAction.copyICOPublicKey.trigger({
        publicKey,
      });
    else
      walletsAction.copyWalletPublicKey.trigger({
        publicKey,
      });
  };
  render() {
    const { actions, stores, isICO = false } = this.props;
    const { wallets } = stores;
    const { active: activeWallet, activePublicKey, icoPublicKey } = wallets;
    if (!activeWallet)
      throw new Error(
        'Active wallet required for PublicKeyQRCodeDialogContainer.'
      );
    if (!icoPublicKey && !activePublicKey) {
      throw new Error(
        'Active wallet public key or ICO public key required for PublicKeyQRCodeDialogContainer.'
      );
    }
    if (isICO && !!icoPublicKey) {
      return react_1.default.createElement(ICOPublicKeyQRCodeDialog_1.default, {
        walletName: activeWallet.name,
        walletPublicKey: icoPublicKey,
        onCopyWalletPublicKey: () => this.handleCopyWalletPublicKey(true),
        onClose: () => {
          actions.dialogs.closeActiveDialog.trigger();
        },
        derivationPath: walletsConfig_1.ICO_PUBLIC_KEY_DERIVATION_PATH,
      });
    }
    if (!isICO && !!activePublicKey) {
      return react_1.default.createElement(
        WalletPublicKeyQRCodeDialog_1.default,
        {
          walletName: activeWallet.name,
          walletPublicKey: activePublicKey,
          onCopyWalletPublicKey: () => this.handleCopyWalletPublicKey(),
          onClose: () => {
            actions.dialogs.closeActiveDialog.trigger();
          },
          messages: walletMessages,
          derivationPath: walletsConfig_1.WALLET_PUBLIC_KEY_DERIVATION_PATH,
        }
      );
    }
    return null;
  }
};
PublicKeyQRCodeDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('actions', 'stores'), mobx_react_1.observer],
  PublicKeyQRCodeDialogContainer
);
exports.default = PublicKeyQRCodeDialogContainer;
//# sourceMappingURL=PublicKeyQRCodeDialogContainer.js.map
