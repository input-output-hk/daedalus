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
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const AddressActions_scss_1 = __importDefault(require('./AddressActions.scss'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/qr-code... Remove this comment to see the full error message
const qr_code_inline_svg_1 = __importDefault(
  require('../../../assets/images/qr-code.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboard... Remove this comment to see the full error message
const clipboard_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/clipboard-ic.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  instructionsTitle: {
    id: 'wallet.receive.page.instructions.instructionsTitle',
    defaultMessage: '!!!Available wallet addresses',
    description: 'Instructions Title on the wallet "Receive page"',
  },
  instructionsDescription: {
    id: 'wallet.receive.page.instructions.instructionsDescription',
    defaultMessage:
      '!!!Share this wallet address to receive payments. To protect your privacy, new addresses are generated automatically once you use them.',
    description: 'Instructions Description on the wallet "Receive page"',
  },
  addressesTitle: {
    id: 'wallet.receive.page.addresses.addressesTitle',
    defaultMessage: '!!!Addresses',
    description: 'Addresses Title on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!Show used',
    description:
      'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
  shareAddressLabel: {
    id: 'wallet.receive.page.shareAddressLabel',
    defaultMessage: '!!!Share',
    description: 'Label for "Share" link on the wallet "Receive page"',
  },
  copyAddressLabel: {
    id: 'wallet.receive.page.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});
let AddressActions = class AddressActions extends react_1.Component {
  static defaultProps = {
    type: 'copy',
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  addressElement;
  addressContainerElement;
  render() {
    const { intl } = this.context;
    const {
      address,
      onShareAddress,
      onCopyAddress,
      type = 'copy',
    } = this.props;
    const { id: addressId, used: isUsed } = address;
    const componentClasses = (0, classnames_1.default)(
      AddressActions_scss_1.default[`${type}Actions`],
      {
        [AddressActions_scss_1.default.isUsed]: isUsed,
      }
    );
    return react_1.default.createElement(
      'div',
      { className: componentClasses },
      type === 'copy'
        ? react_1.default.createElement(
            react_copy_to_clipboard_1.default,
            { text: addressId, onCopy: () => onCopyAddress(addressId) },
            react_1.default.createElement(
              'span',
              { className: AddressActions_scss_1.default.copyAddress },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: clipboard_ic_inline_svg_1.default,
                className: AddressActions_scss_1.default.copyIcon,
              }),
              react_1.default.createElement(
                'span',
                { className: AddressActions_scss_1.default.copyAddressLabel },
                intl.formatMessage(messages.copyAddressLabel)
              )
            )
          )
        : react_1.default.createElement(
            'button',
            {
              className: AddressActions_scss_1.default.shareAddressButton,
              onClick: () => onShareAddress(address),
            },
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: qr_code_inline_svg_1.default,
              className: AddressActions_scss_1.default.shareIcon,
            }),
            react_1.default.createElement(
              'span',
              { className: AddressActions_scss_1.default.shareAddressLabel },
              intl.formatMessage(messages.shareAddressLabel)
            )
          )
    );
  }
};
AddressActions = __decorate([mobx_react_1.observer], AddressActions);
exports.default = AddressActions;
//# sourceMappingURL=AddressActions.js.map
