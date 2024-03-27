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
const qrcode_react_1 = __importDefault(require('qrcode.react'));
const react_intl_1 = require('react-intl');
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const network_1 = require('../../../utils/network');
const CompletionDialog_scss_1 = __importDefault(
  require('./CompletionDialog.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
const clipboard_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/clipboard-ic.inline.svg')
);
const InlineNotification_1 = __importDefault(
  require('../../notifications/InlineNotification')
);
const environment_types_1 = require('../../../../../common/types/environment.types');
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'paper.wallet.create.certificate.completion.dialog.headline',
    defaultMessage: '!!!Paper wallet certificate',
    description:
      'Headline for the "Paper wallet create certificate completion dialog" headline.',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.completion.dialog.subtitle',
    defaultMessage:
      '!!!You may wish to fold your paper wallet certificate and glue together the edges to store it securely. Please keep your certificate safe.',
    description:
      'Headline for the "Paper wallet create certificate completion dialog" subtitle.',
  },
  linkInstructions: {
    id: 'paper.wallet.create.certificate.completion.dialog.linkInstructions',
    defaultMessage: `!!!When you wish to import your wallet back into Daedalus crop any glued edges of the certificate to open it.
      To check your balance on the paper wallet at any time, you may use the link below. Copy or save the URL to your browser bookmarks to do this easily`,
    description:
      'Headline for the "Paper wallet create certificate completion dialog" link instructions.',
  },
  addressInstructions: {
    id: 'paper.wallet.create.certificate.completion.dialog.addressInstructions',
    defaultMessage:
      '!!!To receive funds to your paper wallet simply share your wallet address with others.',
    description:
      'Headline for the "Paper wallet create certificate completion dialog" address instructions.',
  },
  cardanoLinkLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.cardanoLinkLabel',
    defaultMessage: '!!!Cardano explorer link',
    description:
      '"Paper wallet create certificate completion dialog" cardano link label.',
  },
  addressCopiedLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.addressCopiedLabel',
    defaultMessage: '!!!copied',
    description:
      '"Paper wallet create certificate completion dialog" address copied.',
  },
  addressLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.addressLabel',
    defaultMessage: '!!!Wallet address',
    description:
      '"Paper wallet create certificate completion dialog" wallet address label.',
  },
  finishButtonLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.finishButtonLabel',
    defaultMessage: '!!!Finish',
    description:
      '"Paper wallet create certificate completion dialog" finish button label.',
  },
});
let CompletionDialog = class CompletionDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    network: environment_types_1.DEVELOPMENT,
  };
  state = {
    showCopyNotification: false,
  };
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  copyNotificationTimeout;
  onShowCopyNotification = () => {
    const { copyAddressNotificationDuration } = this.props;
    const timeInSeconds = copyAddressNotificationDuration * 1000;
    clearTimeout(this.copyNotificationTimeout);
    this.setState({
      showCopyNotification: true,
    });
    this.copyNotificationTimeout = setTimeout(
      () =>
        this.setState({
          showCopyNotification: false,
        }),
      timeInSeconds
    );
  };
  render() {
    const { intl } = this.context;
    const {
      onClose,
      walletCertificateAddress,
      onOpenExternalLink,
      network,
    } = this.props;
    const { showCopyNotification } = this.state;
    const actions = [
      {
        className: 'finishButton',
        label: intl.formatMessage(messages.finishButtonLabel),
        primary: true,
        onClick: onClose,
      },
    ];
    const cardanoExplorerLink = `${(0, network_1.getNetworkExplorerUrl)(
      network
    )}/address/${walletCertificateAddress}`;
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
        className: 'completionDialog',
        title: intl.formatMessage(messages.headline),
        actions: actions,
      },
      react_1.default.createElement(
        'div',
        { className: CompletionDialog_scss_1.default.completionContentWrapper },
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(messages.subtitle)
        ),
        react_1.default.createElement(
          'div',
          {
            className: CompletionDialog_scss_1.default.linkInstructionsWrapper,
          },
          react_1.default.createElement(
            'p',
            null,
            intl.formatMessage(messages.linkInstructions)
          ),
          react_1.default.createElement(
            'p',
            { className: CompletionDialog_scss_1.default.infoBoxLabel },
            intl.formatMessage(messages.cardanoLinkLabel)
          ),
          react_1.default.createElement(
            'div',
            { className: CompletionDialog_scss_1.default.infoBox },
            react_1.default.createElement(Link_1.Link, {
              className: CompletionDialog_scss_1.default.link,
              onClick: () => onOpenExternalLink(cardanoExplorerLink),
              label: cardanoExplorerLink,
              skin: LinkSkin_1.LinkSkin,
            })
          )
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              CompletionDialog_scss_1.default.addressInstructionsWrapper,
          },
          react_1.default.createElement(
            'p',
            null,
            intl.formatMessage(messages.addressInstructions)
          ),
          react_1.default.createElement(
            'p',
            { className: CompletionDialog_scss_1.default.infoBoxLabel },
            intl.formatMessage(messages.addressLabel)
          ),
          react_1.default.createElement(
            InlineNotification_1.default,
            { show: showCopyNotification },
            intl.formatMessage(messages.addressCopiedLabel)
          ),
          react_1.default.createElement(
            'div',
            { className: CompletionDialog_scss_1.default.infoBox },
            walletCertificateAddress,
            react_1.default.createElement(
              react_copy_to_clipboard_1.default,
              {
                text: walletCertificateAddress,
                onCopy: this.onShowCopyNotification,
              },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: clipboard_ic_inline_svg_1.default,
                className: CompletionDialog_scss_1.default.copyIconBig,
              })
            )
          )
        ),
        react_1.default.createElement(
          'div',
          { className: CompletionDialog_scss_1.default.qrCode },
          react_1.default.createElement(qrcode_react_1.default, {
            value: walletCertificateAddress,
            bgColor: qrCodeBackgroundColor,
            fgColor: qrCodeForegroundColor,
            size: 152,
          })
        )
      )
    );
  }
};
CompletionDialog = __decorate([mobx_react_1.observer], CompletionDialog);
exports.default = CompletionDialog;
//# sourceMappingURL=CompletionDialog.js.map
