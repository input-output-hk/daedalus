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
const MnemonicsDialog_1 = __importDefault(
  require('../../../../components/wallet/wallet-restore/MnemonicsDialog')
);
const injectedPropsType_1 = require('../../../../types/injectedPropsType');
const decrypt_1 = require('../../../../../../common/config/crypto/decrypt');
const crypto_1 = require('../../../../utils/crypto');
const walletRestoreConfig_1 = require('../../../../config/walletRestoreConfig');
const cryptoConfig_1 = require('../../../../config/cryptoConfig');
const DefaultProps =
  injectedPropsType_1.InjectedDialogContainerStepDefaultProps;
let MnemonicsDialogContainer = class MnemonicsDialogContainer extends react_1.Component {
  static defaultProps = DefaultProps;
  handleSetWalletMnemonics = (mnemonics) =>
    this.props.actions.wallets.restoreWalletSetMnemonics.trigger({
      mnemonics,
    });
  handleValidateMnemonics = (mnemonics) => {
    let enteredWords = mnemonics;
    let numberOfWords = mnemonics.length;
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props.stores.wallets;
    const expectedWordCount = this.getExpectedWordCount(
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware
    );
    if (
      expectedWordCount ===
      cryptoConfig_1.PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT
    ) {
      numberOfWords = cryptoConfig_1.LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT;
      const { passphrase, scrambledInput } = (0, crypto_1.getScrambledInput)(
        mnemonics
      );
      try {
        enteredWords = (0, crypto_1.unscramblePaperWalletMnemonic)(
          passphrase,
          scrambledInput
        );
      } catch (e) {
        return false;
      }
    }
    return (0, decrypt_1.isValidMnemonic)(
      enteredWords.join(' '),
      numberOfWords
    );
  };
  getExpectedWordCount = (
    walletKind,
    walletKindDaedalus,
    walletKindYoroi,
    walletKindHardware
  ) => {
    let expectedWordCount = 0;
    if (
      walletKindDaedalus &&
      walletKind === walletRestoreConfig_1.WALLET_KINDS.DAEDALUS
    ) {
      expectedWordCount =
        walletRestoreConfig_1.WALLET_DAEDALUS_WORD_COUNT[walletKindDaedalus];
    } else if (
      walletKindYoroi &&
      walletKind === walletRestoreConfig_1.WALLET_KINDS.YOROI
    ) {
      expectedWordCount =
        walletRestoreConfig_1.WALLET_YOROI_WORD_COUNT[walletKindYoroi];
    } else if (walletKindHardware) {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'number[]' is not assignable to type 'number'... Remove this comment to see the full error message
      expectedWordCount =
        walletRestoreConfig_1.WALLET_HARDWARE_WORD_COUNT[walletKindHardware];
    }
    return expectedWordCount;
  };
  getMaxWordCount = (expectedWordCount) =>
    Array.isArray(expectedWordCount)
      ? Math.max(...expectedWordCount)
      : expectedWordCount;
  render() {
    const { onContinue, onClose, onBack, stores } = this.props;
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
      mnemonics,
    } = stores.wallets;
    const expectedWordCount = this.getExpectedWordCount(
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware
    );
    const maxWordCount = this.getMaxWordCount(expectedWordCount);
    return react_1.default.createElement(MnemonicsDialog_1.default, {
      onClose: onClose,
      onContinue: onContinue,
      onBack: onBack,
      onValidateMnemonics: this.handleValidateMnemonics,
      walletKind: walletKind,
      walletKindDaedalus: walletKindDaedalus,
      walletKindYoroi: walletKindYoroi,
      walletKindHardware: walletKindHardware,
      onSetWalletMnemonics: this.handleSetWalletMnemonics,
      mnemonics: mnemonics,
      expectedWordCount: expectedWordCount,
      maxWordCount: maxWordCount,
    });
  }
};
MnemonicsDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  MnemonicsDialogContainer
);
exports.default = MnemonicsDialogContainer;
//# sourceMappingURL=StepMnemonicsContainer.js.map
