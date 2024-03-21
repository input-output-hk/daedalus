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
const lodash_1 = require('lodash');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const TinySwitch_1 = __importDefault(require('../../widgets/forms/TinySwitch'));
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const VirtualAddressesList_1 = require('./VirtualAddressesList');
const WalletReceiveSequential_scss_1 = __importDefault(
  require('./WalletReceiveSequential.scss')
);
const AddressSequential_1 = __importDefault(require('./AddressSequential'));
const messages = (0, react_intl_1.defineMessages)({
  instructionsTitle: {
    id: 'wallet.receive.page.instructions.instructionsTitle',
    defaultMessage: '!!!Available wallet addresses',
    description: 'Instructions Title on the wallet "Receive page"',
  },
  instructionsDescription: {
    id: 'wallet.receive.page.instructions.instructionsDescription',
    defaultMessage:
      '!!!Share any of these wallet addresses <b>to receive payments in ada or a native Cardano token</b>.',
    description: 'Instructions Description on the wallet "Receive page"',
  },
  privacyWarning: {
    id: 'wallet.receive.page.instructions.privacyWarning',
    defaultMessage:
      '!!!Privacy warning: Please note that all of your receiving addresses include your stake key. When you share a receiving address, the recipient can search the blockchain using your stake key to locate all addresses associated with your wallet, and also discover your wallet balance and transaction history.',
    description: 'Privacy warning on the wallet "Receive page"',
  },
  addressesTitle: {
    id: 'wallet.receive.page.addresses.addressesTitle',
    defaultMessage: '!!!Receiving addresses',
    description: 'Addresses Title on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!Show used',
    description:
      'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
});
messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
let WalletReceiveSequential = class WalletReceiveSequential extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  containerElement;
  state = {
    addressSlice: 0,
    addressWidth: 0,
    charWidth: 0,
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;
  componentDidMount() {
    this._isMounted = true;
    window.addEventListener('resize', this.debounceAddressCalculation);
    this.props.onToggleSubMenus.listen(this.debounceAddressCalculation);
  }
  componentWillUnmount() {
    this._isMounted = false;
    window.removeEventListener('resize', this.debounceAddressCalculation);
    this.props.onToggleSubMenus.remove(this.debounceAddressCalculation);
  }
  debounceAddressCalculation = (0, lodash_1.debounce)(
    () => this.calculateAddressSlice(),
    300
  );
  get addressLength() {
    const [address] = this.props.walletAddresses;
    return address.id.length;
  }
  handleRegisterHTMLElements = (addressElement, containerElement) => {
    setTimeout(() => {
      if (this._isMounted) {
        this.containerElement = containerElement;
        const addressWidth = addressElement.offsetWidth;
        const charWidth = addressWidth / this.addressLength;
        this.setState(
          {
            charWidth,
            addressWidth,
          },
          this.calculateAddressSlice
        );
      }
    }, 500);
  };
  calculateAddressSlice = () => {
    if (this._isMounted) {
      const { charWidth, addressWidth } = this.state;
      const { addressLength, containerElement } = this;
      if (!containerElement || !charWidth || !addressLength) return;
      const containerWidth = containerElement.offsetWidth;
      const addressSlice =
        containerWidth < addressWidth
          ? Math.floor(containerWidth / charWidth / 2) - 1
          : 0;
      this.setState({
        addressSlice,
      });
    }
  };
  toggleUsedAddresses = () => {
    const { onToggleUsedAddresses } = this.props;
    onToggleUsedAddresses();
  };
  renderRow = (address, index) => {
    const { onShareAddress, onCopyAddress } = this.props;
    const { addressSlice } = this.state;
    return react_1.default.createElement(AddressSequential_1.default, {
      address: address,
      onShareAddress: onShareAddress,
      onCopyAddress: onCopyAddress,
      shouldRegisterAddressElement: index === 0,
      onRegisterHTMLElements: this.handleRegisterHTMLElements,
      addressSlice: addressSlice,
    });
  };
  getFilteredAddresses = (walletAddresses) =>
    walletAddresses.filter((address) => !address.used || this.props.showUsed);
  render() {
    const { walletAddresses, showUsed } = this.props;
    const { intl } = this.context;
    return react_1.default.createElement(
      'div',
      { className: WalletReceiveSequential_scss_1.default.component },
      react_1.default.createElement(
        BorderedBox_1.default,
        { fullHeight: true },
        react_1.default.createElement(
          'div',
          { className: WalletReceiveSequential_scss_1.default.container },
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'h2',
              {
                className:
                  WalletReceiveSequential_scss_1.default.instructionsTitle,
              },
              intl.formatMessage(messages.instructionsTitle)
            ),
            react_1.default.createElement(
              'p',
              {
                className:
                  WalletReceiveSequential_scss_1.default
                    .instructionsDescription,
              },
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...messages.instructionsDescription,
              })
            ),
            react_1.default.createElement(
              'p',
              {
                className:
                  WalletReceiveSequential_scss_1.default.privacyWarning,
              },
              intl.formatMessage(messages.privacyWarning)
            )
          ),
          react_1.default.createElement(
            'div',
            { className: WalletReceiveSequential_scss_1.default.addresses },
            react_1.default.createElement(
              'h3',
              {
                className:
                  WalletReceiveSequential_scss_1.default.addressesTitle,
              },
              intl.formatMessage(messages.addressesTitle),
              react_1.default.createElement(
                'div',
                { className: WalletReceiveSequential_scss_1.default.hideUsed },
                react_1.default.createElement(TinySwitch_1.default, {
                  label: intl.formatMessage(messages.showUsedLabel),
                  onChange: this.toggleUsedAddresses,
                  checked: showUsed,
                })
              )
            ),
            react_1.default.createElement(
              VirtualAddressesList_1.VirtualAddressesList,
              {
                rows: this.getFilteredAddresses(walletAddresses),
                renderRow: this.renderRow,
              }
            )
          )
        )
      )
    );
  }
};
WalletReceiveSequential = __decorate(
  [mobx_react_1.observer],
  WalletReceiveSequential
);
exports.default = WalletReceiveSequential;
//# sourceMappingURL=WalletReceiveSequential.js.map
