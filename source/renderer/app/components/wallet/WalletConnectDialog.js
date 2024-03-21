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
// TODO: Remove once the new wallet creation process is ready
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const lodash_1 = require('lodash');
const ledger_cropped_inline_svg_1 = __importDefault(
  require('../../assets/images/hardware-wallet/ledger-cropped.inline.svg')
);
const ledgerSP_cropped_inline_svg_1 = __importDefault(
  require('../../assets/images/hardware-wallet/ledgerSP-cropped.inline.svg')
);
const ledger_x_cropped_inline_svg_1 = __importDefault(
  require('../../assets/images/hardware-wallet/ledger-x-cropped.inline.svg')
);
const trezor_inline_svg_1 = __importDefault(
  require('../../assets/images/hardware-wallet/trezor.inline.svg')
);
const trezor_ledger_inline_svg_1 = __importDefault(
  require('../../assets/images/hardware-wallet/trezor-ledger.inline.svg')
);
const DialogCloseButton_1 = __importDefault(
  require('../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../widgets/Dialog'));
const WalletConnectDialog_scss_1 = __importDefault(
  require('./WalletConnectDialog.scss')
);
const LoadingSpinner_1 = __importDefault(require('../widgets/LoadingSpinner'));
const HardwareWalletStatus_1 = __importDefault(
  require('../hardware-wallet/HardwareWalletStatus')
);
const hardwareWalletsConfig_1 = require('../../config/hardwareWalletsConfig');
const hardware_wallets_types_1 = require('../../../../common/types/hardware-wallets.types');
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.connect.dialog.title',
    defaultMessage: '!!!Pair a hardware wallet device',
    description:
      'Title "Connect a hardware wallet device" in the connect wallet dialog.',
  },
  cancelButton: {
    id: 'wallet.connect.dialog.button.cancel',
    defaultMessage: '!!!Cancel',
    description: 'Label for the "Cancel" button in the connect wallet dialog',
  },
  instructions: {
    id: 'wallet.connect.dialog.instructions',
    defaultMessage:
      '!!!<p>Daedalus currently supports Ledger Nano S, Ledger Nano X, and Trezor Model T hardware wallet devices.</p><p>If you are <b>pairing your device with Daedalus for the first time</b>, please follow the instructions below.</p><p>If you have <b>already paired your device with Daedalus</b>, you don’t need to repeat this step. Just connect your device when you need to confirm a transaction.</p>',
    description: 'Follow instructions label',
  },
  instructionsTrezorOnly: {
    id: 'wallet.connect.dialog.instructionsTrezorOnly',
    defaultMessage:
      '!!!<p><b>Daedalus currently supports only Trezor Model T hardware wallet devices.</b></p><p>If you are <b>pairing your device with Daedalus for the first time</b>, please follow the instructions below.</p><p>If you have <b>already paired your device with Daedalus</b>, you don’t need to repeat this step. Just connect your device when you need to confirm a transaction.</p>',
    description: 'Follow instructions label',
  },
  connectingIssueSupportLabel: {
    id: 'wallet.connect.dialog.connectingIssueSupportLabel',
    defaultMessage:
      '!!!If you are experiencing issues pairing your hardware wallet device, please {supportLink}',
    description: 'Connecting issue support description',
  },
  connectingIssueSupportLink: {
    id: 'wallet.connect.dialog.connectingIssueSupportLink',
    defaultMessage: '!!!read the instructions.',
    description: 'Connecting issue support link',
  },
  connectingIssueSupportLinkUrl: {
    id: 'wallet.connect.dialog.connectingIssueSupportLinkUrl',
    defaultMessage: 'https://support.ledger.com/hc/en-us/articles/115005165269',
    description: 'Link to support article',
  },
});
let WalletConnectDialog = class WalletConnectDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      onClose,
      isSubmitting,
      hwDeviceStatus,
      transportDevice,
      onExternalLinkClick,
      error,
    } = this.props;
    const deviceType = (0, lodash_1.get)(transportDevice, 'deviceType');
    const deviceModel = (0, lodash_1.get)(transportDevice, 'deviceModel');
    const isLedger = deviceType === hardware_wallets_types_1.DeviceTypes.LEDGER;
    const isTrezor = deviceType === hardware_wallets_types_1.DeviceTypes.TREZOR;
    const dialogClasses = (0, classnames_1.default)([
      WalletConnectDialog_scss_1.default.component,
      'WalletConnectDialog',
    ]);
    const buttonLabel = !isSubmitting
      ? this.context.intl.formatMessage(messages.cancelButton)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const actions = [
      {
        disabled: isSubmitting,
        label: buttonLabel,
        primary: false,
        onClick: onClose,
      },
    ];
    const renderUnknownDevice = () => {
      let unknownDeviceElement;
      if (
        hardwareWalletsConfig_1.isTrezorEnabled &&
        !hardwareWalletsConfig_1.isLedgerEnabled
      ) {
        unknownDeviceElement = react_1.default.createElement(
          'div',
          {
            className: WalletConnectDialog_scss_1.default.hardwareWalletTrezor,
          },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: trezor_inline_svg_1.default,
            className: WalletConnectDialog_scss_1.default.trezorIcon,
          })
        );
      } else if (
        hardwareWalletsConfig_1.isLedgerEnabled &&
        !hardwareWalletsConfig_1.isTrezorEnabled
      ) {
        unknownDeviceElement = react_1.default.createElement(
          'div',
          {
            className: WalletConnectDialog_scss_1.default.hardwareWalletLedger,
          },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: ledger_cropped_inline_svg_1.default,
            className: WalletConnectDialog_scss_1.default.ledgerIcon,
          })
        );
      } else {
        unknownDeviceElement = react_1.default.createElement(
          'div',
          {
            className: WalletConnectDialog_scss_1.default.hardwareWalletUnknown,
          },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: trezor_ledger_inline_svg_1.default,
            className: WalletConnectDialog_scss_1.default.unknownDeviceIcon,
          })
        );
      }
      return unknownDeviceElement;
    };
    const instructions = hardwareWalletsConfig_1.isLedgerEnabled
      ? messages.instructions
      : messages.instructionsTrezorOnly;
    const supportLink = react_1.default.createElement(Link_1.Link, {
      className: WalletConnectDialog_scss_1.default.externalLink,
      onClick: () =>
        onExternalLinkClick(
          intl.formatMessage(messages.connectingIssueSupportLinkUrl)
        ),
      label: intl.formatMessage(messages.connectingIssueSupportLink),
      skin: LinkSkin_1.LinkSkin,
    });
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(messages.dialogTitle),
        actions: actions,
        closeOnOverlayClick: false,
        onClose: !isSubmitting ? onClose : () => {},
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'div',
        { className: WalletConnectDialog_scss_1.default.hardwareWalletWrapper },
        (!transportDevice || (!isTrezor && !isLedger)) && renderUnknownDevice(),
        isLedger &&
          hardwareWalletsConfig_1.isLedgerEnabled &&
          react_1.default.createElement(
            'div',
            {
              className:
                WalletConnectDialog_scss_1.default.hardwareWalletLedger,
            },
            deviceModel ===
              hardware_wallets_types_1.DeviceModels.LEDGER_NANO_X &&
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: ledger_x_cropped_inline_svg_1.default,
                className: WalletConnectDialog_scss_1.default.ledgerXIcon,
              }),
            deviceModel ===
              hardware_wallets_types_1.DeviceModels.LEDGER_NANO_S &&
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: ledger_cropped_inline_svg_1.default,
                className: WalletConnectDialog_scss_1.default.ledgerIcon,
              }),
            deviceModel ===
              hardware_wallets_types_1.DeviceModels.LEDGER_NANO_S_PLUS &&
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: ledgerSP_cropped_inline_svg_1.default,
                className: WalletConnectDialog_scss_1.default.ledgerSpIcon,
              })
          ),
        isTrezor &&
          hardwareWalletsConfig_1.isTrezorEnabled &&
          react_1.default.createElement(
            'div',
            {
              className:
                WalletConnectDialog_scss_1.default.hardwareWalletTrezor,
            },
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: trezor_inline_svg_1.default,
              className: WalletConnectDialog_scss_1.default.trezorIcon,
            })
          ),
        error
          ? react_1.default.createElement(
              'p',
              { className: WalletConnectDialog_scss_1.default.error },
              intl.formatMessage(error)
            )
          : react_1.default.createElement(
              'div',
              null,
              react_1.default.createElement(
                'p',
                {
                  className:
                    WalletConnectDialog_scss_1.default.hardwareWalletMessage,
                },
                react_1.default.createElement(
                  react_intl_1.FormattedHTMLMessage,
                  { ...instructions }
                )
              ),
              react_1.default.createElement(
                'div',
                {
                  className:
                    WalletConnectDialog_scss_1.default
                      .hardwareWalletStatusWrapper,
                },
                react_1.default.createElement(HardwareWalletStatus_1.default, {
                  hwDeviceStatus: hwDeviceStatus,
                  onExternalLinkClick: onExternalLinkClick,
                  // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                  isTransactionStatus: false,
                  isTrezor: isTrezor,
                })
              ),
              react_1.default.createElement(
                'div',
                {
                  className:
                    WalletConnectDialog_scss_1.default
                      .hardwareWalletIssueArticleWrapper,
                },
                react_1.default.createElement(
                  'p',
                  null,
                  react_1.default.createElement(react_intl_1.FormattedMessage, {
                    ...messages.connectingIssueSupportLabel,
                    values: {
                      supportLink,
                    },
                  })
                )
              )
            )
      )
    );
  }
};
WalletConnectDialog = __decorate([mobx_react_1.observer], WalletConnectDialog);
exports.default = WalletConnectDialog;
//# sourceMappingURL=WalletConnectDialog.js.map
