'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.messages = void 0;
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const WalletSettings_scss_1 = __importDefault(require('./WalletSettings.scss'));
const WalletPublicKeyQRCodeDialog_1 = __importDefault(
  require('./WalletPublicKeyQRCodeDialog')
);
const WalletPublicKeyDialog_1 = __importDefault(
  require('./WalletPublicKeyDialog')
);
const PublicKeyField_1 = __importDefault(require('./PublicKeyField'));
exports.messages = (0, react_intl_1.defineMessages)({
  publicKey: {
    id: 'wallet.settings.walletPublicKey',
    defaultMessage: '!!!Wallet public key',
    description: 'Wallet public key label.',
  },
  publicKeyShowInstruction: {
    id: 'wallet.settings.walletPublicKeyShowInstruction',
    defaultMessage:
      '!!!Click Reveal on the right-hand side to display the public key of the wallet.',
    description: 'Wallet public key show instruction.',
  },
  showQRCode: {
    id: 'wallet.settings.showQRCode',
    defaultMessage: '!!!Show QR code',
    description: 'Show QR code tooltip.',
  },
});
function WalletPublicKeyBox(props) {
  const { publicKey, locale, onCopyWalletPublicKey, openDialogAction } = props;
  return react_1.default.createElement(
    BorderedBox_1.default,
    { className: WalletSettings_scss_1.default.walletPublicKeyBox },
    react_1.default.createElement(PublicKeyField_1.default, {
      publicKey: publicKey || '',
      locale: locale,
      onCopyPublicKey: onCopyWalletPublicKey,
      onShowQRCode: () =>
        openDialogAction({
          dialog: WalletPublicKeyQRCodeDialog_1.default,
        }),
      onOpenWalletKeyDialog: () =>
        openDialogAction({
          dialog: WalletPublicKeyDialog_1.default,
        }),
      messages: exports.messages,
    })
  );
}
exports.default = (0, react_intl_1.injectIntl)(WalletPublicKeyBox);
//# sourceMappingURL=WalletPublicKeyBox.js.map
