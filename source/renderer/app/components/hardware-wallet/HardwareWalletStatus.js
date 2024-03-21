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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const check_inline_svg_1 = __importDefault(
  require('../../assets/images/hardware-wallet/check.inline.svg')
);
const close_cross_red_inline_svg_1 = __importDefault(
  require('../../assets/images/hardware-wallet/close-cross-red.inline.svg')
);
const LoadingSpinner_1 = __importDefault(require('../widgets/LoadingSpinner'));
const Wallet_1 = require('../../domains/Wallet');
const HardwareWalletStatus_scss_1 = __importDefault(
  require('./HardwareWalletStatus.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  connecting: {
    id: 'wallet.hardware.deviceStatus.connecting',
    defaultMessage: '!!!Connect your device and enter your PIN to unlock it',
    description:
      '"Connect your device and enter your PIN to unlock it" device state',
  },
  connecting_failed: {
    id: 'wallet.hardware.deviceStatus.connecting.failed',
    defaultMessage:
      '!!!Disconnect and reconnect your device to restart the process.',
    description: '"Connect failed" device state',
  },
  connecting_known: {
    id: 'wallet.hardware.deviceStatus.connecting.known',
    defaultMessage: '!!!Connect the "{walletName}" device',
    description: '"Connect the IOHK Trezor 1 device" device state',
  },
  launching_cardano_app: {
    id: 'wallet.hardware.deviceStatus.launching_cardano_app',
    defaultMessage: '!!!Launch Cardano application on your device',
    description: '"Launch Cardano application on your device" device state',
  },
  exporting_public_key: {
    id: 'wallet.hardware.deviceStatus.exporting_public_key',
    defaultMessage: '!!!Export the public key on your device',
    description:
      '"Confirm exporting your public key on your device" device state',
  },
  exporting_public_key_failed: {
    id: 'wallet.hardware.deviceStatus.exporting_public_key_failed',
    defaultMessage: '!!!Exporting the public key failed',
    description: '"Exporting public key failed" device state',
  },
  unrecognized_wallet: {
    id: 'wallet.hardware.deviceStatus.unrecognized_wallet',
    defaultMessage:
      '!!!We do not recognize this wallet on your device. Please ensure that you are using the same device that you selected for pairing {walletName} and that you have entered the correct passphrase.',
    description: '"Unrecognized wallet" device state',
  },
  exportingPublicKeyError: {
    id: 'wallet.hardware.deviceStatus.exportingPublicKeyError',
    defaultMessage:
      '!!!Disconnect and reconnect your device to restart the process.',
    description:
      '"Disconnect and reconnect your device to start the process again" device state',
  },
  enterPassphrase: {
    id: 'wallet.hardware.deviceStatus.enterPassphrase',
    defaultMessage: '!!!Enter passphrase if needed',
    description: '"Enter passphrase if needed" device sub-state',
  },
  ready: {
    id: 'wallet.hardware.deviceStatus.ready',
    defaultMessage: '!!!Device ready',
    description: '"Device ready" device state',
  },
  verifying_transaction: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction',
    defaultMessage:
      '!!!Confirm the transaction using the "{walletName}" device',
    description:
      '"Confirm the transaction using the IOHK Trezor 1 device" device state',
  },
  verifying_transaction_failed: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction_failed',
    defaultMessage: '!!!Transaction verification and signing failed',
    description: '"Transaction verification and signing failed" device state',
  },
  verifying_transaction_succeeded: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction_succeeded',
    defaultMessage: '!!!Transaction confirmed',
    description: '"Transaction verified and signed" device state',
  },
  trezor_bridge_failure: {
    id: 'wallet.hardware.deviceStatus.trezor_bridge_failure',
    defaultMessage: '!!!Trezor Bridge not installed!',
    description:
      '"Trezor Bridge not installed! {instructionsLink}" device state',
  },
  trezor_bridge_failure_link_label: {
    id: 'wallet.hardware.deviceStatus.trezor_bridge_failure.link.label',
    defaultMessage: '!!!Installation instructions',
    description: 'Trezor Bridge installation instructions link label',
  },
  trezor_bridge_failure_link_url: {
    id: 'wallet.hardware.deviceStatus.trezor_bridge_failure.link.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360011451693',
    description: 'URL for the "Trezor Bridge" update',
  },
  wrong_firmware: {
    id: 'wallet.hardware.deviceStatus.wrong_firmware',
    defaultMessage: '!!!Unsupported firmware! {instructionsLink}',
    description: '"Unsupported firmware!" device state',
  },
  wrong_firmware_link_label: {
    id: 'wallet.hardware.deviceStatus.wrong_firmware.link.label',
    defaultMessage: '!!!Firmware update instructions',
    description: 'Firmware update installation instructions link label',
  },
  wrong_firmware_link_url: {
    id: 'wallet.hardware.deviceStatus.wrong_firmware.link.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360011451693',
    description: 'URL for the "Firmware Update"',
  },
  unsupported_device: {
    id: 'wallet.hardware.deviceStatus.unsupported_device',
    defaultMessage: '!!!The device is not supported!',
    description: '"The device is not supported!" device state',
  },
  wrong_cardano_app_version: {
    id: 'wallet.hardware.deviceStatus.wrong_cardano_app_version',
    defaultMessage: '!!!Outdated Ledger software!! {instructionsLink}',
    description: '"Unsupported firmware!" device state',
  },
  wrong_cardano_app_version_link_label: {
    id: 'wallet.hardware.deviceStatus.wrong_cardano_app_version.link.label',
    defaultMessage: '!!!Software update instructions',
    description: 'Firmware update installation instructions link label',
  },
  wrong_cardano_app_version_link_url: {
    id: 'wallet.hardware.deviceStatus.wrong_cardano_app_version.link.url',
    defaultMessage:
      '!!!https://support.ledger.com/hc/en-us/articles/360020095874-Cardano-ADA-',
    description: 'URL for the "Firmware Update"',
  },
  verifying_address: {
    id: 'wallet.hardware.deviceStatus.verifying_address',
    defaultMessage: '!!!Verify address on your "{walletName}" device',
    description: '"Verify receiving address on your Hardware Wallet device',
  },
  verifying_address_confirmation: {
    id: 'wallet.hardware.deviceStatus.verifying_address_confirmation',
    defaultMessage: '!!!Please answer the question below',
    description: '"Confirm receiving address on your Hardware Wallet device',
  },
  verifying_address_failed: {
    id: 'wallet.hardware.deviceStatus.verifying_address_failed',
    defaultMessage: '!!!Address verification failed',
    description: '"Address verification failed" device state',
  },
  verifying_address_aborted: {
    id: 'wallet.hardware.deviceStatus.verifying_address_aborted',
    defaultMessage: '!!!Verification was aborted by the user',
    description: '"Address verification aborted" device state',
  },
  verifying_address_succeeded: {
    id: 'wallet.hardware.deviceStatus.verifying_address_succeeded',
    defaultMessage: '!!!Address verified',
    description: '"Address verified" device state',
  },
});
const hwDeviceLoadingStatuses = [
  Wallet_1.HwDeviceStatuses.CONNECTING,
  Wallet_1.HwDeviceStatuses.LAUNCHING_CARDANO_APP,
  Wallet_1.HwDeviceStatuses.EXPORTING_PUBLIC_KEY,
  Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION,
  Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS,
  Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION,
];
const hwDeviceReadyStatuses = [
  Wallet_1.HwDeviceStatuses.READY,
  Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
  Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_SUCCEEDED,
];
const hwDeviceErrorStatuses = [
  Wallet_1.HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED,
  Wallet_1.HwDeviceStatuses.CONNECTING_FAILED,
  Wallet_1.HwDeviceStatuses.TREZOR_BRIDGE_FAILURE,
  Wallet_1.HwDeviceStatuses.WRONG_FIRMWARE,
  Wallet_1.HwDeviceStatuses.WRONG_CARDANO_APP_VERSION,
  Wallet_1.HwDeviceStatuses.UNSUPPORTED_DEVICE,
  Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED,
  Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_FAILED,
  Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_ABORTED,
  Wallet_1.HwDeviceStatuses.UNRECOGNIZED_WALLET,
];
const hwDevicePassphraseRelatedStatuses = [
  Wallet_1.HwDeviceStatuses.EXPORTING_PUBLIC_KEY,
  Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION,
  Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS,
];
const hwDeviceInstructionsLinkRelatedStatuses = [
  Wallet_1.HwDeviceStatuses.TREZOR_BRIDGE_FAILURE,
  Wallet_1.HwDeviceStatuses.WRONG_CARDANO_APP_VERSION,
  Wallet_1.HwDeviceStatuses.WRONG_FIRMWARE,
];
let HardwareWalletStatus = class HardwareWalletStatus extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    hwDeviceStatus: this.props.hwDeviceStatus,
  };
  // eslint-disable-next-line
  UNSAFE_componentWillReceiveProps(nextProps) {
    if (nextProps.hwDeviceStatus !== this.props.hwDeviceStatus) {
      // Set with delay
      if (
        nextProps.hwDeviceStatus === Wallet_1.HwDeviceStatuses.CONNECTING &&
        this.props.hwDeviceStatus ===
          Wallet_1.HwDeviceStatuses.LAUNCHING_CARDANO_APP
      ) {
        setTimeout(() => {
          // Status remains unchanged in 1.5s - set as new status
          if (nextProps.hwDeviceStatus === this.props.hwDeviceStatus) {
            this.setState({
              hwDeviceStatus: nextProps.hwDeviceStatus,
            });
          }
        }, 4000);
      } else {
        this.setState({
          hwDeviceStatus: nextProps.hwDeviceStatus,
        });
      }
    }
  }
  render() {
    const { intl } = this.context;
    const { onExternalLinkClick, walletName, isTrezor } = this.props;
    const { hwDeviceStatus } = this.state;
    const isLoading = hwDeviceLoadingStatuses.includes(hwDeviceStatus);
    const isReady = hwDeviceReadyStatuses.includes(hwDeviceStatus);
    const isError = hwDeviceErrorStatuses.includes(hwDeviceStatus);
    const hasInstructionsLink = hwDeviceInstructionsLinkRelatedStatuses.includes(
      hwDeviceStatus
    );
    const passphraseLabelVisible =
      isTrezor && hwDevicePassphraseRelatedStatuses.includes(hwDeviceStatus);
    let secondaryMessage = null;
    if (passphraseLabelVisible) {
      secondaryMessage = messages.enterPassphrase;
    } else if (
      hwDeviceStatus === Wallet_1.HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED
    ) {
      secondaryMessage = messages.exportingPublicKeyError;
    }
    const componentClasses = (0, classnames_1.default)(
      HardwareWalletStatus_scss_1.default.component,
      {
        [HardwareWalletStatus_scss_1.default.isReady]: isReady,
        [HardwareWalletStatus_scss_1.default.isError]: isError,
      }
    );
    let instructionsLink;
    let label;
    if (hasInstructionsLink && onExternalLinkClick) {
      // @TODO - add Ledger firmware update support article links
      instructionsLink = react_1.default.createElement(Link_1.Link, {
        className: HardwareWalletStatus_scss_1.default.externalLink,
        onClick: (event) =>
          onExternalLinkClick(
            intl.formatMessage(messages[`${hwDeviceStatus}_link_url`]),
            event
          ),
        label: intl.formatMessage(messages[`${hwDeviceStatus}_link_label`]),
        skin: LinkSkin_1.LinkSkin,
      });
    } else if (
      walletName &&
      (hwDeviceStatus === Wallet_1.HwDeviceStatuses.CONNECTING ||
        hwDeviceStatus === Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION ||
        hwDeviceStatus === Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS ||
        hwDeviceStatus === Wallet_1.HwDeviceStatuses.UNRECOGNIZED_WALLET ||
        hwDeviceStatus ===
          Wallet_1.HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION)
    ) {
      const message =
        hwDeviceStatus === Wallet_1.HwDeviceStatuses.CONNECTING
          ? `${hwDeviceStatus}_known`
          : hwDeviceStatus;
      label = react_1.default.createElement(react_intl_1.FormattedMessage, {
        ...messages[message],
        values: {
          walletName,
        },
      });
    } else {
      label = intl.formatMessage(messages[hwDeviceStatus]);
    }
    return react_1.default.createElement(
      'div',
      { className: componentClasses },
      react_1.default.createElement(
        'div',
        { className: HardwareWalletStatus_scss_1.default.messageWrapper },
        react_1.default.createElement(
          'div',
          { className: HardwareWalletStatus_scss_1.default.message },
          hasInstructionsLink && instructionsLink
            ? react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...messages[hwDeviceStatus],
                values: {
                  instructionsLink,
                },
              })
            : label
        ),
        secondaryMessage &&
          react_1.default.createElement(
            'div',
            { className: HardwareWalletStatus_scss_1.default.secondaryMessage },
            intl.formatMessage(secondaryMessage)
          )
      ),
      isLoading &&
        react_1.default.createElement(LoadingSpinner_1.default, {
          className: 'hardwareWalletProcessProgress',
        }),
      isReady &&
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: check_inline_svg_1.default,
          className: HardwareWalletStatus_scss_1.default.checkIcon,
        }),
      isError &&
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: close_cross_red_inline_svg_1.default,
          className: HardwareWalletStatus_scss_1.default.clearIcon,
        })
    );
  }
};
HardwareWalletStatus = __decorate(
  [mobx_react_1.observer],
  HardwareWalletStatus
);
exports.default = HardwareWalletStatus;
//# sourceMappingURL=HardwareWalletStatus.js.map
