'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const qrcode_react_1 = __importDefault(require('qrcode.react'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
const clipboard_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/clipboard-ic.inline.svg')
);
const PublicKeyQRCodeDialog_scss_1 = __importDefault(
  require('./PublicKeyQRCodeDialog.scss')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const ICOPublicKeyQRCodeDialog_messages_1 = require('./ICOPublicKeyQRCodeDialog.messages');
const ICOPublicKeyQRCodeDialog = (0, mobx_react_1.observer)((props) => {
  const {
    walletName,
    walletPublicKey,
    onCopyWalletPublicKey,
    onClose,
    derivationPath,
    intl,
  } = props;
  const actions = [
    {
      label: intl.formatMessage(global_messages_1.default.close),
      onClick: onClose,
    },
  ];
  // Get QRCode color value from active theme's CSS variable
  const qrCodeBackgroundColor = document.documentElement
    ? document.documentElement.style.getPropertyValue(
        '--theme-receive-qr-code-background-color'
      )
    : 'transparent';
  const qrCodeForegroundColor = document.documentElement
    ? document.documentElement.style.getPropertyValue(
        '--theme-receive-qr-code-foreground-color'
      )
    : '#000';
  return react_1.default.createElement(
    Dialog_1.default,
    {
      title: intl.formatMessage(
        ICOPublicKeyQRCodeDialog_messages_1.messages.dialogTitle
      ),
      subtitle: walletName,
      actions: actions,
      closeOnOverlayClick: true,
      onClose: onClose,
      className: PublicKeyQRCodeDialog_scss_1.default.dialog,
      closeButton: react_1.default.createElement(DialogCloseButton_1.default, {
        onClose: onClose,
      }),
    },
    react_1.default.createElement(
      'div',
      { className: PublicKeyQRCodeDialog_scss_1.default.walletPublicKeyQrCode },
      react_1.default.createElement(qrcode_react_1.default, {
        value: walletPublicKey,
        bgColor: qrCodeBackgroundColor,
        fgColor: qrCodeForegroundColor,
        size: 192,
      })
    ),
    react_1.default.createElement(
      'div',
      { className: PublicKeyQRCodeDialog_scss_1.default.addressPathsWrapper },
      react_1.default.createElement(
        PopOver_1.PopOver,
        {
          content: intl.formatMessage(
            ICOPublicKeyQRCodeDialog_messages_1.messages.derivationPathTooltip
          ),
        },
        react_1.default.createElement(
          'div',
          { className: PublicKeyQRCodeDialog_scss_1.default.spendingPath },
          derivationPath
        )
      )
    ),
    react_1.default.createElement(
      'div',
      { className: PublicKeyQRCodeDialog_scss_1.default.walletPublicKey },
      walletPublicKey
    ),
    react_1.default.createElement(
      react_copy_to_clipboard_1.default,
      { text: walletPublicKey, onCopy: onCopyWalletPublicKey },
      react_1.default.createElement(
        'span',
        { className: PublicKeyQRCodeDialog_scss_1.default.copyPublicKey },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: clipboard_ic_inline_svg_1.default,
          className: PublicKeyQRCodeDialog_scss_1.default.copyIcon,
        }),
        react_1.default.createElement(
          'span',
          {
            className: PublicKeyQRCodeDialog_scss_1.default.copyPublicKeyLabel,
          },
          intl.formatMessage(
            ICOPublicKeyQRCodeDialog_messages_1.messages.copyPublicKeyLabel
          )
        )
      )
    )
  );
});
exports.default = (0, react_intl_1.injectIntl)(ICOPublicKeyQRCodeDialog);
//# sourceMappingURL=ICOPublicKeyQRCodeDialog.js.map
