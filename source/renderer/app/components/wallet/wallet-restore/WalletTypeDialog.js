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
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const RadioSet_1 = __importDefault(require('../../widgets/RadioSet'));
const WalletRestoreDialog_1 = __importDefault(
  require('./widgets/WalletRestoreDialog')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const WalletTypeDialog_scss_1 = __importDefault(
  require('./WalletTypeDialog.scss')
);
const walletRestoreConfig_1 = require('../../../config/walletRestoreConfig');
const messages = (0, react_intl_1.defineMessages)({
  labelWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKind',
    defaultMessage: '!!!What kind of wallet would you like to restore?',
    description: 'Label for the "labelwalletKind" checkbox.',
  },
  labelWalletKindDaedalus: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKindDaedalus',
    defaultMessage: '!!!Daedalus wallet',
    description: 'Label for the "labelWalletKindDaedalus" checkbox.',
  },
  labelWalletKindYoroi: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKindYoroi',
    defaultMessage: '!!!Yoroi wallet',
    description: 'Label for the "labelWalletKindYoroi" checkbox.',
  },
  labelWalletKindHardware: {
    id: 'wallet.restore.dialog.step.walletKind.label.walletKindHardware',
    defaultMessage: '!!!Hardware wallet',
    description: 'Label for the "labelWalletKindHardware" checkbox.',
  },
  labelDaedalusWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind',
    defaultMessage:
      '!!!What kind of Daedalus wallet would you like to restore?',
    description: 'Label for the "labelDaedalusWalletKind" checkbox.',
  },
  labelDaedalusWalletKind12WordByron: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind12WordByron',
    defaultMessage: '!!!12 words <em>(Byron legacy wallet)</em>',
    description: 'Label for the "labelDaedalusWalletKind12WordByron" checkbox.',
  },
  labelDaedalusWalletKind15WordShelley: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind15WordShelley',
    defaultMessage:
      '!!!15 words <em>(Incentivized Testnet Rewards wallet)</em>',
    description:
      'Label for the "labelDaedalusWalletKind15WordShelley" checkbox.',
  },
  labelDaedalusWalletKind24WordShelley: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind24WordShelley',
    defaultMessage: '!!!24 words <em>(Shelley wallet)</em>',
    description:
      'Label for the "labelDaedalusWalletKind24WordShelley" checkbox.',
  },
  labelDaedalusWalletKind27WordPaper: {
    id:
      'wallet.restore.dialog.step.walletKind.label.daedalusWalletKind27WordPaper',
    defaultMessage: '!!!27 words - paper wallet (Byron legacy wallet)</em>',
    description: 'Label for the "labelDaedalusWalletKind27WordPaper" checkbox.',
  },
  labelYoroiWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.yoroiWalletKind',
    defaultMessage: '!!!What kind of Yoroi wallet would you like to restore?',
    description: 'Label for the "labelYoroiWalletKind" checkbox.',
  },
  labelYoroiWalletKind15WordByron: {
    id:
      'wallet.restore.dialog.step.walletKind.label.yoroiWalletKindByronLegacy15Word',
    defaultMessage: '!!!15 words <em>(Byron legacy wallet)</em>',
    description: 'Label for the "labelDaedalusWalletKind15WordByron" checkbox.',
  },
  labelYoroiWalletKind15WordShelley: {
    id:
      'wallet.restore.dialog.step.walletKind.label.yoroiWalletKindShelley15Word',
    defaultMessage: '!!!15 words <em>(Shelley wallet)</em>',
    description:
      'Label for the "labelDaedalusWalletKind15WordShelley" checkbox.',
  },
  labelHardwareWalletKind: {
    id: 'wallet.restore.dialog.step.walletKind.label.hardwareWalletKind',
    defaultMessage:
      '!!!What kind of hardware wallet would you like to restore?',
    description: 'Label for the "labelHardwareWalletKind" checkbox.',
  },
  labelHardwareWalletKindLedger: {
    id: 'wallet.restore.dialog.step.walletKind.label.hardwareWalletKindLedger',
    defaultMessage: '!!!24 words - Ledger (Byron legacy wallet)',
    description: 'Label for the "labelHardwareWalletKindLedger" checkbox.',
  },
  labelHardwareWalletKindTrezor: {
    id: 'wallet.restore.dialog.step.walletKind.label.hardwareWalletKindTrezor',
    defaultMessage: '!!!24 words - Trezor (Byron legacy wallet)',
    description: 'Label for the "labelHardwareWalletKindTrezor" checkbox.',
  },
  hardwareWalletDisclaimer1: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer1',
    defaultMessage:
      '!!!Hardware wallets store your private keys securely on a physical device so they are immune to common computer threats such as viruses and software bugs. Recovery phrases for hardware wallets should always be kept offline. By entering your hardware wallet recovery phrase in Daedalus, you expose your hardware wallet private keys to the security risks associated with computers and software.',
    description: 'Label for the "hardwareWalletDisclaimer1" disclaimer.',
  },
  hardwareWalletDisclaimer2: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer2',
    defaultMessage:
      '!!!All of your assets held on your hardware wallet device are associated with the same wallet recovery phrase and its corresponding private key. If you hold assets other than ada on your hardware wallet device, you expose all of those assets to security risks.',
    description: 'Label for the "hardwareWalletDisclaimer2" disclaimer.',
  },
  hardwareWalletDisclaimer3: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletDisclaimer3',
    defaultMessage:
      '!!!We strongly recommend that you delete the Byron legacy wallet that was restored from your hardware wallet once you have moved funds into a Shelley wallet.',
    description: 'Label for the "hardwareWalletDisclaimer3" disclaimer.',
  },
  hardwareWalletCheckbox1: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletCheckbox1',
    defaultMessage:
      '!!!I understand and accept responsibility for the security concerns of restoring a hardware wallet on a computer.',
    description: 'Label for the "hardwareWalletCheckbox1" disclaimer.',
  },
  hardwareWalletCheckbox2: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletCheckbox2',
    defaultMessage:
      '!!!I understand that I should delete the Byron legacy wallet I am restoring from a hardware wallet after moving funds to a Shelley wallet.',
    description: 'Label for the "hardwareWalletCheckbox2" disclaimer.',
  },
  hardwareWalletCheckbox3: {
    id: 'wallet.restore.dialog.step.walletKind.hardwareWalletCheckbox3',
    defaultMessage:
      '!!!I understand that I am exposing all of the assets that are stored on my hardware wallet device, and not just ada, to security risks.',
    description: 'Label for the "hardwareWalletCheckbox2" disclaimer.',
  },
});
class WalletTypeDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    hardwareWalletAcceptance1: false,
    hardwareWalletAcceptance2: false,
    hardwareWalletAcceptance3: false,
  };
  toggleAcceptance = (param) =>
    this.setState((currentState) =>
      (0, lodash_1.set)({}, param, !currentState[param])
    );
  getWalletKind = (kinds, message, value, kindParam) =>
    react_1.default.createElement(RadioSet_1.default, {
      label: this.context.intl.formatMessage(message),
      items: Object.keys(kinds).map((key) => {
        const kind = kinds[key];
        const messageParam = `label${kindParam || ''}WalletKind${kind}`;
        const msg = messages[messageParam];
        if (!msg) {
          throw new Error(`Missing ${messageParam} message`);
        }
        return {
          key: kind,
          disabled: false,
          label: react_1.default.createElement(
            react_intl_1.FormattedHTMLMessage,
            { ...msg }
          ),
          selected: value === kind,
          onChange: () => this.props.onSetWalletKind(kind, kindParam),
        };
      }),
      verticallyAligned: true,
    });
  get isDisabled() {
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props;
    const {
      hardwareWalletAcceptance1,
      hardwareWalletAcceptance2,
      hardwareWalletAcceptance3,
    } = this.state;
    if (!walletKind) return true;
    if (
      walletKind === walletRestoreConfig_1.WALLET_KINDS.DAEDALUS &&
      !walletKindDaedalus
    )
      return true;
    if (
      walletKind === walletRestoreConfig_1.WALLET_KINDS.YOROI &&
      !walletKindYoroi
    )
      return true;
    return (
      walletKind === walletRestoreConfig_1.WALLET_KINDS.HARDWARE &&
      (!walletKindHardware ||
        !hardwareWalletAcceptance1 ||
        !hardwareWalletAcceptance2 ||
        !hardwareWalletAcceptance3)
    );
  }
  render() {
    const { intl } = this.context;
    const {
      onClose,
      onContinue,
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props;
    const {
      hardwareWalletAcceptance1,
      hardwareWalletAcceptance2,
      hardwareWalletAcceptance3,
    } = this.state;
    return react_1.default.createElement(
      WalletRestoreDialog_1.default,
      {
        stepNumber: 0,
        actions: [
          {
            primary: true,
            label: intl.formatMessage(
              global_messages_1.default.dialogButtonContinueLabel
            ),
            onClick: onContinue,
            disabled: this.isDisabled,
          },
        ],
        onClose: onClose,
      },
      this.getWalletKind(
        walletRestoreConfig_1.WALLET_KINDS,
        messages.labelWalletKind,
        walletKind
      ),
      walletKind === walletRestoreConfig_1.WALLET_KINDS.DAEDALUS &&
        this.getWalletKind(
          walletRestoreConfig_1.WALLET_DAEDALUS_KINDS,
          messages.labelDaedalusWalletKind,
          walletKindDaedalus,
          walletRestoreConfig_1.WALLET_KINDS.DAEDALUS
        ),
      walletKind === walletRestoreConfig_1.WALLET_KINDS.YOROI &&
        this.getWalletKind(
          walletRestoreConfig_1.WALLET_YOROI_KINDS,
          messages.labelYoroiWalletKind,
          walletKindYoroi,
          walletRestoreConfig_1.WALLET_KINDS.YOROI
        ),
      walletKind === walletRestoreConfig_1.WALLET_KINDS.HARDWARE &&
        react_1.default.createElement(
          react_1.Fragment,
          null,
          this.getWalletKind(
            walletRestoreConfig_1.WALLET_HARDWARE_KINDS,
            messages.labelHardwareWalletKind,
            walletKindHardware,
            walletRestoreConfig_1.WALLET_KINDS.HARDWARE
          ),
          react_1.default.createElement(
            'p',
            {
              className:
                WalletTypeDialog_scss_1.default.hardwareWalletAcceptance,
            },
            intl.formatMessage(messages.hardwareWalletDisclaimer1)
          ),
          react_1.default.createElement(
            'p',
            {
              className:
                WalletTypeDialog_scss_1.default.hardwareWalletAcceptance,
            },
            intl.formatMessage(messages.hardwareWalletDisclaimer2)
          ),
          react_1.default.createElement(
            'p',
            {
              className:
                WalletTypeDialog_scss_1.default.hardwareWalletAcceptance,
            },
            react_1.default.createElement(
              'b',
              null,
              intl.formatMessage(messages.hardwareWalletDisclaimer3)
            )
          ),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: 'walletSecurityRisk',
            label: intl.formatMessage(messages.hardwareWalletCheckbox3),
            onChange: () => this.toggleAcceptance('hardwareWalletAcceptance3'),
            checked: hardwareWalletAcceptance3,
            skin: CheckboxSkin_1.CheckboxSkin,
          }),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: 'restoreSecurityNote',
            label: intl.formatMessage(messages.hardwareWalletCheckbox1),
            onChange: () => this.toggleAcceptance('hardwareWalletAcceptance1'),
            checked: hardwareWalletAcceptance1,
            skin: CheckboxSkin_1.CheckboxSkin,
          }),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: 'walletDeleteNote',
            label: intl.formatMessage(messages.hardwareWalletCheckbox2),
            onChange: () => this.toggleAcceptance('hardwareWalletAcceptance2'),
            checked: hardwareWalletAcceptance2,
            skin: CheckboxSkin_1.CheckboxSkin,
          })
        )
    );
  }
}
exports.default = WalletTypeDialog;
//# sourceMappingURL=WalletTypeDialog.js.map
