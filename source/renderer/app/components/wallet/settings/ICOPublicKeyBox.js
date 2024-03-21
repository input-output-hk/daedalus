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
const ICOPublicKeyQRCodeDialog_1 = __importDefault(
  require('./ICOPublicKeyQRCodeDialog')
);
const ICOPublicKeyDialog_1 = __importDefault(require('./ICOPublicKeyDialog'));
const PublicKeyField_1 = __importDefault(require('./PublicKeyField'));
exports.messages = (0, react_intl_1.defineMessages)({
  publicKey: {
    id: 'wallet.settings.icoPublicKey',
    defaultMessage: '!!!ICO public key',
    description: 'Wallet public key label.',
  },
  publicKeyDescription: {
    id: 'wallet.settings.icoPublicKey.description',
    defaultMessage:
      "!!!Your wallet's ICO public key enables participation in the initial coin offering presales.",
    description: 'ICO public key header on the wallet settings page.',
  },
  publicKeyShowInstruction: {
    id: 'wallet.settings.icoPublicKeyShowInstruction',
    defaultMessage:
      '!!!Click the icon on the right to view your ICO public key.',
    description: 'ICO public key show instruction.',
  },
  showQRCode: {
    id: 'wallet.settings.showQRCode',
    defaultMessage: '!!!Show QR code',
    description: 'Show QR code tooltip.',
  },
});
function ICOPublicKeyBox(props) {
  const {
    publicKey,
    locale,
    onCopyICOPublicKey,
    openDialogAction,
    intl,
  } = props;
  return react_1.default.createElement(
    BorderedBox_1.default,
    { className: WalletSettings_scss_1.default.walletPublicKeyBox },
    react_1.default.createElement(PublicKeyField_1.default, {
      publicKey: publicKey || '',
      description: intl.formatMessage(exports.messages.publicKeyDescription),
      locale: locale,
      onCopyPublicKey: onCopyICOPublicKey,
      onShowQRCode: () =>
        openDialogAction({
          dialog: ICOPublicKeyQRCodeDialog_1.default,
        }),
      onOpenWalletKeyDialog: () =>
        openDialogAction({
          dialog: ICOPublicKeyDialog_1.default,
        }),
      messages: exports.messages,
    })
  );
}
exports.default = (0, react_intl_1.injectIntl)(ICOPublicKeyBox);
//# sourceMappingURL=ICOPublicKeyBox.js.map
