'use strict';
// @ts-nocheck
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
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const qrcode_react_1 = __importDefault(require('qrcode.react'));
const Button_1 = require('@react-polymorph/components/Button');
const Input_1 = require('@react-polymorph/components/Input');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const form_1 = require('../../../utils/form');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const TinySwitch_1 = __importDefault(require('../../widgets/forms/TinySwitch'));
const clipboard_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/clipboard-ic.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const VirtualAddressesList_1 = require('./VirtualAddressesList');
const WalletReceiveRandom_scss_1 = __importDefault(
  require('./WalletReceiveRandom.scss')
);
const AddressRandom_1 = __importDefault(require('./AddressRandom'));
const messages = (0, react_intl_1.defineMessages)({
  walletAddressLabel: {
    id: 'wallet.receive.page.walletAddressLabel',
    defaultMessage: '!!!Your wallet address',
    description: 'Label for wallet address on the wallet "Receive page"',
  },
  walletReceiveInstructions: {
    id: 'wallet.receive.page.walletReceiveInstructions',
    defaultMessage:
      '!!!Share this wallet address to receive payments. To protect your privacy, always use a new address when requesting funds. To generate a new address please enter your spending password and press the ‘Generate a new address’ button.',
    description:
      'Wallet receive payments instructions on the wallet "Receive page"',
  },
  generateNewAddressButtonLabel: {
    id: 'wallet.receive.page.generateNewAddressButtonLabel',
    defaultMessage: '!!!Generate a new address',
    description:
      'Label for "Generate new address" button on the wallet "Receive page"',
  },
  generatedAddressesSectionTitle: {
    id: 'wallet.receive.page.receivingAddressesSectionTitle',
    defaultMessage: '!!!Receiving addresses',
    description:
      '"Generated addresses" section title on the wallet "Receive page"',
  },
  showUsedLabel: {
    id: 'wallet.receive.page.showUsedLabel',
    defaultMessage: '!!!Show used',
    description:
      'Label for "show used" wallet addresses link on the wallet "Receive page"',
  },
  spendingPasswordPlaceholder: {
    id: 'wallet.receive.page.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for "spending password" on the wallet "Receive page"',
  },
  copyAddressLabel: {
    id: 'wallet.receive.page.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});
messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
let WalletReceiveRandom = class WalletReceiveRandom extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  passwordField;
  toggleUsedAddresses = () => {
    const { onToggleUsedAddresses } = this.props;
    onToggleUsedAddresses();
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: ' ',
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (this.props.walletHasPassword && field.value === '') {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validationDebounceWait: 0,
        // Disable debounce to avoid error state after clearing
        validateOnChange: true,
        showErrorsOnBlur: false,
        showErrorsOnClear: false,
      },
    }
  );
  renderRow = (address, index) =>
    react_1.default.createElement(AddressRandom_1.default, {
      index: index,
      address: address,
      onCopyAddress: this.props.onCopyAddress,
      onShareAddress: this.props.onShareAddress,
    });
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { spendingPassword } = form.values();
        this.props.onGenerateAddress(spendingPassword || '');
        form.clear();
      },
      onError: () => {},
    });
    // eslint-disable-next-line no-unused-expressions
    this.passwordField && this.passwordField.focus();
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  getFilteredAddresses = (walletAddresses) =>
    walletAddresses.filter((address) => !address.used || this.props.showUsed);
  render() {
    const { form } = this;
    const {
      walletAddress,
      walletAddresses,
      onCopyAddress,
      isSidebarExpanded,
      walletHasPassword,
      isSubmitting,
      error,
      isWalletAddressUsed,
      showUsed,
    } = this.props;
    const { intl } = this.context;
    const walletAddressClasses = (0, classnames_1.default)([
      WalletReceiveRandom_scss_1.default.hash,
      isWalletAddressUsed ? WalletReceiveRandom_scss_1.default.usedHash : null,
    ]);
    const generateAddressWrapperClasses = (0, classnames_1.default)([
      WalletReceiveRandom_scss_1.default.generateAddressWrapper,
      isSidebarExpanded
        ? WalletReceiveRandom_scss_1.default.fullWidthOnSmallScreen
        : null,
    ]);
    const generateAddressButtonClasses = (0, classnames_1.default)([
      'primary',
      'generateAddressButton',
      walletHasPassword
        ? WalletReceiveRandom_scss_1.default.submitWithPasswordButton
        : WalletReceiveRandom_scss_1.default.submitButton,
      isSubmitting ? WalletReceiveRandom_scss_1.default.spinning : null,
    ]);
    const passwordField = form.$('spendingPassword');
    const canSubmit = !isSubmitting && passwordField.value;
    const generateAddressForm = react_1.default.createElement(
      'div',
      { className: generateAddressWrapperClasses },
      walletHasPassword &&
        react_1.default.createElement(Input_1.Input, {
          className: WalletReceiveRandom_scss_1.default.spendingPassword,
          ...passwordField.bind(),
          ref: (input) => {
            this.passwordField = input;
          },
          error: passwordField.error,
          skin: InputSkin_1.InputSkin,
          onKeyPress: this.handleSubmitOnEnter,
        }),
      react_1.default.createElement(Button_1.Button, {
        className: generateAddressButtonClasses,
        disabled: !canSubmit,
        label: intl.formatMessage(messages.generateNewAddressButtonLabel),
        skin: ButtonSkin_1.ButtonSkin,
        onClick: this.submit,
      })
    );
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
      'div',
      { className: WalletReceiveRandom_scss_1.default.component },
      react_1.default.createElement(
        BorderedBox_1.default,
        { fullHeight: true },
        react_1.default.createElement(
          'div',
          { className: WalletReceiveRandom_scss_1.default.container },
          react_1.default.createElement(
            'div',
            {
              className:
                WalletReceiveRandom_scss_1.default.qrCodeAndInstructions,
            },
            walletAddress &&
              react_1.default.createElement(
                'div',
                { className: WalletReceiveRandom_scss_1.default.qrCode },
                react_1.default.createElement(qrcode_react_1.default, {
                  value: walletAddress,
                  bgColor: qrCodeBackgroundColor,
                  fgColor: qrCodeForegroundColor,
                  size: 152,
                })
              ),
            react_1.default.createElement(
              'div',
              { className: WalletReceiveRandom_scss_1.default.instructions },
              walletAddress &&
                react_1.default.createElement(
                  'div',
                  { className: walletAddressClasses },
                  walletAddress,
                  react_1.default.createElement(
                    react_copy_to_clipboard_1.default,
                    {
                      text: walletAddress,
                      onCopy: onCopyAddress.bind(this, walletAddress),
                    },
                    react_1.default.createElement(react_svg_inline_1.default, {
                      svg: clipboard_ic_inline_svg_1.default,
                      className: WalletReceiveRandom_scss_1.default.copyIconBig,
                    })
                  )
                ),
              react_1.default.createElement(
                'div',
                { className: WalletReceiveRandom_scss_1.default.hashLabel },
                intl.formatMessage(messages.walletAddressLabel)
              ),
              react_1.default.createElement(
                'div',
                {
                  className:
                    WalletReceiveRandom_scss_1.default.instructionsText,
                },
                react_1.default.createElement(
                  react_intl_1.FormattedHTMLMessage,
                  { ...messages.walletReceiveInstructions }
                )
              ),
              error
                ? react_1.default.createElement(
                    'p',
                    { className: WalletReceiveRandom_scss_1.default.error },
                    intl.formatMessage(error)
                  )
                : null,
              generateAddressForm
            )
          ),
          walletAddresses.length
            ? react_1.default.createElement(
                'div',
                {
                  className:
                    WalletReceiveRandom_scss_1.default.generatedAddresses,
                },
                react_1.default.createElement(
                  'h2',
                  null,
                  intl.formatMessage(messages.generatedAddressesSectionTitle),
                  react_1.default.createElement(
                    'div',
                    { className: WalletReceiveRandom_scss_1.default.hideUsed },
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
            : null
        )
      )
    );
  }
};
WalletReceiveRandom = __decorate([mobx_react_1.observer], WalletReceiveRandom);
exports.default = WalletReceiveRandom;
//# sourceMappingURL=WalletReceiveRandom.js.map
