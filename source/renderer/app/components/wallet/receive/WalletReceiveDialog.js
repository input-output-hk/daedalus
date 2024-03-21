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
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const TextArea_1 = require('@react-polymorph/components/TextArea');
const TextAreaSkin_1 = require('@react-polymorph/skins/simple/TextAreaSkin');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const qrcode_react_1 = __importDefault(require('qrcode.react'));
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const address_1 = require('@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address');
const RadioSet_1 = __importDefault(require('../../widgets/RadioSet'));
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const WalletReceiveDialog_scss_1 = __importDefault(
  require('./WalletReceiveDialog.scss')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const HardwareWalletStatus_1 = __importDefault(
  require('../../hardware-wallet/HardwareWalletStatus')
);
const clipboard_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/clipboard-ic.inline.svg')
);
const hardwareWalletsConfig_1 = require('../../../config/hardwareWalletsConfig');
const hardwareWalletUtils_1 = require('../../../utils/hardwareWalletUtils');
const HardwareWalletsStore_1 = require('../../../stores/HardwareWalletsStore');
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const messages = (0, react_intl_1.defineMessages)({
  inputLabel: {
    id: 'wallet.receive.dialog.inputLabel',
    defaultMessage: '!!!PDF note',
    description: 'placeholder on the wallet "Share Address" dialog',
  },
  inputPlaceholder: {
    id: 'wallet.receive.dialog.inputPlaceholder',
    defaultMessage: '!!!Add a note to the sender',
    description: 'inputPlaceholder on the wallet "Share Address" dialog',
  },
  saveQRCodeImage: {
    id: 'wallet.receive.dialog.saveQRCodeImage',
    defaultMessage: '!!!Save QR code image',
    description: 'saveQRCodeImage on the wallet "Share Address" dialog',
  },
  downloadPDFButton: {
    id: 'wallet.receive.dialog.downloadPDFButton',
    defaultMessage: '!!!Download as PDF',
    description: 'downloadPDFButton on the wallet "Share Address" dialog',
  },
  dialogTitle: {
    id: 'wallet.receive.dialog.dialogTitle',
    defaultMessage: '!!!Share wallet address',
    description: 'dialogTitle on the wallet "Share Address" dialog',
  },
  copyAddressLabel: {
    id: 'wallet.receive.dialog.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
  spendingPathTooltip: {
    id: 'wallet.receive.dialog.spendingPathTooltip',
    defaultMessage: '!!!Receiving address path',
    description: 'Tooltip for the receiving address path',
  },
  stakingPathTooltip: {
    id: 'wallet.receive.dialog.stakingPathTooltip',
    defaultMessage: '!!!Rewards address path',
    description: 'Tooltip for the rewards address path',
  },
  supportRequestButtonLabel: {
    id: 'wallet.receive.dialog.supportRequestButtonLabel',
    defaultMessage: '!!!Submit a request to IOHK Support',
    description: 'Support request button label',
  },
  supportRequestLinkUrl: {
    id: 'wallet.receive.dialog.supportRequestLinkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Support request link URL',
  },
  invalidAddressConfirmationLabel: {
    id: 'wallet.receive.dialog.invalidAddressConfirmationLabel',
    defaultMessage:
      '!!!Yes, I am sure I have compared the address displayed in Daedalus with the address displayed on the {deviceType} device by comparing the beginning and ending of the address.',
    description: 'Invalid address confirmation checkbox label',
  },
  verificationCheckOptionsLabel: {
    id: 'wallet.receive.dialog.verificationCheckOptionsLabel',
    defaultMessage: '!!!Is the address you have verified correct?',
    description: 'Verification options section label',
  },
  verificationCheckOptionValid: {
    id: 'wallet.receive.dialog.verificationCheckOptionValid',
    defaultMessage: '!!!Yes',
    description: 'Verification option "Valid" label',
  },
  verificationCheckOptionInvalid: {
    id: 'wallet.receive.dialog.verificationCheckOptionInvalid',
    defaultMessage:
      '!!!No, I am sure that address displayed in Daedalus is different from the address displayed on my {deviceType} device',
    description: 'Verification option "Invalid" label',
  },
  verificationCheckOptionReverify: {
    id: 'wallet.receive.dialog.verificationCheckOptionReverify',
    defaultMessage: '!!!No, reverify',
    description: 'Verification option "Reverify" label',
  },
  softwareCheckLabel: {
    id: 'wallet.receive.dialog.softwareCheckLabel',
    defaultMessage: '!!!Daedalus verified the address',
    description: 'Daedalus verification status check label',
  },
  confirmationCheckLabel: {
    id: 'wallet.receive.dialog.confirmationCheckLabel',
    defaultMessage: '!!!You have verified the address',
    description: 'User verification status check label',
  },
  addressVerificationInstructions: {
    id: 'wallet.receive.dialog.addressVerificationInstructions',
    defaultMessage:
      '!!!Please compare the address displayed here on the screen with the address displayed on the {deviceType} device by comparing <b>at least the first 5 characters at the start</b> of the address after the "addr" part and <b>at least 5 characters at the end</b> of the address.',
    description: 'Address verification instructions',
  },
  invalidAddressWarningTitle: {
    id: 'wallet.receive.dialog.invalidAddressWarningTitle',
    defaultMessage: '!!!Warning, your copy of Daedalus may be hacked!',
    description: 'Invalid address "Warning" title',
  },
  invalidAddressWarningDescription: {
    id: 'wallet.receive.dialog.invalidAddressWarningDescription',
    defaultMessage:
      '!!!You have manually compared the address shown in Daedalus with the address shown on the hardware wallet device and reported that they are different. If this is the case, please contact support and make sure you download and attach logs.',
    description: 'Invalid address "Warning" description',
  },
});
messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
let WalletReceiveDialog = class WalletReceiveDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    selectedVerificationStatus: null,
    isInvalidAddressConfirmed: false,
    isReverifying: false,
  };
  form = new ReactToolboxMobxForm_1.default({
    fields: {
      noteInput: {
        value: '',
        label: this.context.intl.formatMessage(messages.inputLabel),
        placeholder: this.context.intl.formatMessage(messages.inputPlaceholder),
      },
    },
  });
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { noteInput } = form.values();
        const { onDownloadPDF } = this.props;
        onDownloadPDF(noteInput);
      },
      onError: (err) => {
        // @ts-ignore Argument of type 'MobxReactForm<Fields>' is not assignable to parameter of type 'string'.
        throw new Error(err);
      },
    });
  };
  handleChange = (field) => {
    field.value = field.value.replace(/\n/g, '');
  };
  constructPaths = (address) => {
    const hardenedSpendingPath = (0, address_1.str_to_path)(
      address.spendingPath
    );
    const derivationSpendingPath = (0,
    hardwareWalletUtils_1.hardenedPathToDerivationPath)(hardenedSpendingPath);
    const spendingPath = (0, lodash_1.map)(
      derivationSpendingPath.constructed,
      (constructeSpendingPathChunk, index) => {
        const isChangeablePart =
          index >= derivationSpendingPath.constructed.length - 2;
        if (isChangeablePart) {
          return react_1.default.createElement(
            'b',
            { key: `chunk-${index}` },
            '/',
            constructeSpendingPathChunk
          );
        }
        return index === 0
          ? constructeSpendingPathChunk
          : `/${constructeSpendingPathChunk}`;
      }
    );
    const derivationStakingPath = (0,
    hardwareWalletUtils_1.hardenedPathToDerivationPath)(
      hardwareWalletsConfig_1.HW_SHELLEY_CONFIG.DEFAULT_DERIVATION_PATH
    );
    const stakingPath = (0, lodash_1.map)(
      derivationStakingPath.constructed,
      (constructeStakingPathChunk, index) => {
        const isLastIndex =
          index === derivationStakingPath.constructed.length - 1;
        if (isLastIndex) {
          return react_1.default.createElement(
            'b',
            { key: `chunk-${index}` },
            '/',
            constructeStakingPathChunk
          );
        }
        return index === 0
          ? constructeStakingPathChunk
          : `/${constructeStakingPathChunk}`;
      }
    );
    return {
      stakingPath,
      spendingPath,
    };
  };
  onChangeVerificationStatus = (status) => {
    this.setState({
      selectedVerificationStatus:
        status ===
        HardwareWalletsStore_1.AddressVerificationCheckStatuses.REVERIFY
          ? null
          : status,
      isInvalidAddressConfirmed: false,
      isReverifying:
        status ===
        HardwareWalletsStore_1.AddressVerificationCheckStatuses.REVERIFY,
    });
    this.props.onChangeVerificationStatus(status);
  };
  handleConfirmInvalidAddress = (isConfirmed) => {
    this.setState({
      isInvalidAddressConfirmed: isConfirmed,
    });
  };
  render() {
    const {
      address,
      onCopyAddress,
      onSaveQRCodeImage,
      onClose,
      walletName,
      hwDeviceStatus,
      isHardwareWallet,
      isAddressDerived,
      isAddressChecked,
      onSupportRequestClick,
      isTrezor,
    } = this.props;
    const {
      selectedVerificationStatus,
      isInvalidAddressConfirmed,
      isReverifying,
    } = this.state;
    const { intl } = this.context;
    const noteInputField = this.form.$('noteInput');
    const deviceType = isHardwareWallet && isTrezor ? 'Trezor' : 'Ledger';
    const isSubmitting = false;
    const supportButtonLabel = !isSubmitting
      ? intl.formatMessage(messages.supportRequestButtonLabel)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const isSupportRequestButton =
      selectedVerificationStatus ===
      HardwareWalletsStore_1.AddressVerificationCheckStatuses.INVALID;
    let actions;
    if (isSupportRequestButton) {
      const supportRequestLinkUrl = intl.formatMessage(
        messages.supportRequestLinkUrl
      );
      actions = [
        {
          className: 'attention',
          label: supportButtonLabel,
          onClick: onSupportRequestClick.bind(this, supportRequestLinkUrl),
          disabled: !isInvalidAddressConfirmed,
          primary: true,
        },
      ];
    } else {
      actions = [
        {
          label: intl.formatMessage(messages.saveQRCodeImage),
          onClick: () => onSaveQRCodeImage(),
        },
        {
          className: 'downloadPDFButton',
          label: intl.formatMessage(messages.downloadPDFButton),
          onClick: this.submit,
          primary: true,
        },
      ];
    }
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
    const constructedPaths = this.constructPaths(address);
    const verificationOptions = [
      {
        status: HardwareWalletsStore_1.AddressVerificationCheckStatuses.VALID,
        label: intl.formatMessage(messages.verificationCheckOptionValid),
      },
      {
        status:
          HardwareWalletsStore_1.AddressVerificationCheckStatuses.REVERIFY,
        label: intl.formatMessage(messages.verificationCheckOptionReverify),
      },
      {
        status: HardwareWalletsStore_1.AddressVerificationCheckStatuses.INVALID,
        label: intl.formatMessage(messages.verificationCheckOptionInvalid, {
          deviceType,
        }),
      },
    ];
    const filteredVerificationOptions = (0, lodash_1.filter)(
      verificationOptions,
      (option) => {
        const isInvalidOption =
          option.status ===
          HardwareWalletsStore_1.AddressVerificationCheckStatuses.INVALID;
        if (
          (!selectedVerificationStatus &&
            (!isInvalidOption || (isInvalidOption && isReverifying))) ||
          (selectedVerificationStatus &&
            selectedVerificationStatus === option.status)
        ) {
          return option;
        }
        return null;
      }
    );
    const showActions =
      !isHardwareWallet ||
      (isHardwareWallet &&
        (selectedVerificationStatus ===
          HardwareWalletsStore_1.AddressVerificationCheckStatuses.INVALID ||
          selectedVerificationStatus ===
            HardwareWalletsStore_1.AddressVerificationCheckStatuses.VALID));
    const isAddressConfirmed =
      isAddressChecked &&
      isAddressDerived &&
      selectedVerificationStatus !== null;
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.dialogTitle),
        subtitle: walletName,
        actions: showActions ? actions : [],
        closeOnOverlayClick: false,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
      },
      react_1.default.createElement(
        'div',
        { className: WalletReceiveDialog_scss_1.default.container },
        react_1.default.createElement(
          'div',
          { className: WalletReceiveDialog_scss_1.default.qrCode },
          react_1.default.createElement(qrcode_react_1.default, {
            value: address.id,
            bgColor: qrCodeBackgroundColor,
            fgColor: qrCodeForegroundColor,
            size: 192,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: WalletReceiveDialog_scss_1.default.addressPathsWrapper },
          react_1.default.createElement(
            PopOver_1.PopOver,
            { content: intl.formatMessage(messages.spendingPathTooltip) },
            react_1.default.createElement(
              'div',
              { className: WalletReceiveDialog_scss_1.default.spendingPath },
              constructedPaths.spendingPath
            )
          ),
          react_1.default.createElement(
            PopOver_1.PopOver,
            { content: intl.formatMessage(messages.stakingPathTooltip) },
            react_1.default.createElement(
              'div',
              { className: WalletReceiveDialog_scss_1.default.stakingPath },
              constructedPaths.stakingPath
            )
          )
        ),
        react_1.default.createElement(
          'div',
          { className: WalletReceiveDialog_scss_1.default.address },
          address.id
        ),
        react_1.default.createElement(
          react_copy_to_clipboard_1.default,
          { text: address.id, onCopy: () => onCopyAddress(address.id) },
          react_1.default.createElement(
            'span',
            { className: WalletReceiveDialog_scss_1.default.copyAddress },
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: clipboard_ic_inline_svg_1.default,
              className: WalletReceiveDialog_scss_1.default.copyIcon,
            }),
            react_1.default.createElement(
              'span',
              {
                className: WalletReceiveDialog_scss_1.default.copyAddressLabel,
              },
              intl.formatMessage(messages.copyAddressLabel)
            )
          )
        ),
        isHardwareWallet &&
          react_1.default.createElement(
            'div',
            {
              className:
                WalletReceiveDialog_scss_1.default.hardwareWalletStatusWrapper,
            },
            react_1.default.createElement(HardwareWalletStatus_1.default, {
              hwDeviceStatus: hwDeviceStatus,
              walletName: walletName,
              isTrezor: isTrezor,
            }),
            react_1.default.createElement(
              'div',
              {
                className:
                  WalletReceiveDialog_scss_1.default.verificationCheckboxes,
              },
              react_1.default.createElement(Checkbox_1.Checkbox, {
                checked: isAddressDerived,
                label: intl.formatMessage(messages.softwareCheckLabel),
                disabled: true,
                skin: CheckboxSkin_1.CheckboxSkin,
              }),
              react_1.default.createElement(Checkbox_1.Checkbox, {
                checked: isAddressConfirmed,
                label: intl.formatMessage(messages.confirmationCheckLabel),
                disabled: true,
                skin: CheckboxSkin_1.CheckboxSkin,
              })
            ),
            react_1.default.createElement(
              'p',
              {
                className:
                  WalletReceiveDialog_scss_1.default.verificationInstructions,
              },
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...messages.addressVerificationInstructions,
                values: {
                  deviceType,
                },
              })
            ),
            isAddressDerived &&
              isAddressChecked &&
              react_1.default.createElement(RadioSet_1.default, {
                label: react_1.default.createElement(
                  'p',
                  null,
                  intl.formatMessage(messages.verificationCheckOptionsLabel)
                ),
                items: (0, lodash_1.map)(
                  filteredVerificationOptions,
                  (option) => ({
                    // @ts-ignore ts-migrate(2339) FIXME: Property 'status' does not exist on type 'number |... Remove this comment to see the full error message
                    key: option.status,
                    disabled: false,
                    // @ts-ignore ts-migrate(2339) FIXME: Property 'label' does not exist on type 'number | ... Remove this comment to see the full error message
                    label: option.label,
                    // @ts-ignore ts-migrate(2339) FIXME: Property 'status' does not exist on type 'number |... Remove this comment to see the full error message
                    selected: option.status === selectedVerificationStatus,
                    onChange: () =>
                      // @ts-ignore ts-migrate(2339) FIXME: Property 'status' does not exist on type 'number |... Remove this comment to see the full error message
                      this.onChangeVerificationStatus(option.status),
                  })
                ),
                verticallyAligned: true,
              })
          ),
        selectedVerificationStatus ===
          HardwareWalletsStore_1.AddressVerificationCheckStatuses.INVALID &&
          react_1.default.createElement(
            'div',
            { className: WalletReceiveDialog_scss_1.default.warningWrapper },
            react_1.default.createElement(Checkbox_1.Checkbox, {
              label: intl.formatMessage(
                messages.invalidAddressConfirmationLabel,
                {
                  deviceType,
                }
              ),
              onChange: this.handleConfirmInvalidAddress,
              checked: isInvalidAddressConfirmed,
              skin: CheckboxSkin_1.CheckboxSkin,
            }),
            react_1.default.createElement(
              'div',
              {
                className:
                  WalletReceiveDialog_scss_1.default.warningDescriptionWrapper,
              },
              react_1.default.createElement(
                'p',
                { className: WalletReceiveDialog_scss_1.default.warningTitle },
                intl.formatMessage(messages.invalidAddressWarningTitle)
              ),
              react_1.default.createElement(
                'p',
                null,
                intl.formatMessage(messages.invalidAddressWarningDescription, {
                  deviceType,
                })
              )
            )
          ),
        (!isHardwareWallet ||
          selectedVerificationStatus ===
            HardwareWalletsStore_1.AddressVerificationCheckStatuses.VALID) &&
          react_1.default.createElement(TextArea_1.TextArea, {
            className: WalletReceiveDialog_scss_1.default.noteInput,
            skin: TextAreaSkin_1.TextAreaSkin,
            autoResize: false,
            rows: 3,
            maxLength: 201,
            ...noteInputField.bind({
              onChange: this.handleChange(noteInputField),
            }),
          })
      )
    );
  }
};
WalletReceiveDialog = __decorate([mobx_react_1.observer], WalletReceiveDialog);
exports.default = WalletReceiveDialog;
//# sourceMappingURL=WalletReceiveDialog.js.map
