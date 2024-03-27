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
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const Step1ConfigurationDialog_1 = __importDefault(
  require('../../../../components/staking/redeem-itn-rewards/Step1ConfigurationDialog')
);
const injectedPropsType_1 = require('../../../../types/injectedPropsType');
const valid_words_en_1 = __importDefault(
  require('../../../../../../common/config/crypto/valid-words.en')
);
const decrypt_1 = require('../../../../../../common/config/crypto/decrypt');
const stakingConfig_1 = require('../../../../config/stakingConfig');
const DefaultProps =
  injectedPropsType_1.InjectedDialogContainerStepDefaultProps;
const messages = (0, react_intl_1.defineMessages)({
  errorMinRewardFunds: {
    id: 'staking.redeemItnRewards.step1.errorMessage',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {calculatedMinRewardsReceiverBalance} ADA which is required to cover the necessary transaction fees. Please select a wallet with <span>a minimum amount of {calculatedMinRewardsReceiverBalance} ADA</span> and click continue.',
    description:
      'errorMinRewardFunds Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorRestoringWallet: {
    id: 'staking.redeemItnRewards.step1.errorRestoringWallet',
    defaultMessage:
      '!!!This wallet can’t be used for rewards redemption while it’s being synced.',
    description:
      'RestoringWallet Error Label on the rewards redemption setup "choose wallet" step dialog.',
  },
});
let Step1ConfigurationContainer = class Step1ConfigurationContainer extends react_1.Component {
  static defaultProps = DefaultProps;
  hasEnoughAdaToCoverFees = (walletAmount) => {
    const minRewardsFunds = new bignumber_js_1.default(
      stakingConfig_1.MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE
    );
    return walletAmount && walletAmount.gte(minRewardsFunds);
  };
  getErrorMessage = (wallet) => {
    if (!wallet) {
      return null;
    }
    const { amount, isRestoring } = wallet;
    if (isRestoring) {
      return messages.errorRestoringWallet;
    }
    if (!this.hasEnoughAdaToCoverFees(amount)) {
      return messages.errorMinRewardFunds;
    }
    return null;
  };
  render() {
    const { actions, stores, onClose } = this.props;
    const { app, staking, wallets } = stores;
    const { allWallets } = wallets;
    const {
      redeemWallet,
      isCalculatingReedemFees,
      redeemRecoveryPhrase,
    } = staking;
    const { openExternalLink } = app;
    const {
      onConfigurationContinue,
      onCalculateRedeemWalletFees,
    } = actions.staking;
    const selectedWalletId = (0, lodash_1.get)(redeemWallet, 'id', null);
    const selectedWallet = allWallets.find(
      (current) => current && current.id === selectedWalletId
    );
    const errorMessage = this.getErrorMessage(selectedWallet);
    return react_1.default.createElement(Step1ConfigurationDialog_1.default, {
      error: errorMessage,
      isCalculatingReedemFees: isCalculatingReedemFees,
      mnemonicValidator: decrypt_1.isValidMnemonic,
      onClose: onClose,
      onContinue: onConfigurationContinue.trigger,
      onSelectWallet: (walletId, recoveryPhrase) =>
        onCalculateRedeemWalletFees.trigger({
          walletId,
          recoveryPhrase,
        }),
      suggestedMnemonics: valid_words_en_1.default,
      wallet: redeemWallet,
      wallets: allWallets,
      openExternalLink: openExternalLink,
      recoveryPhrase: redeemRecoveryPhrase,
    });
  }
};
Step1ConfigurationContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  Step1ConfigurationContainer
);
exports.default = Step1ConfigurationContainer;
//# sourceMappingURL=Step1ConfigurationContainer.js.map
