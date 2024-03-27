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
const lodash_1 = require('lodash');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const react_intl_1 = require('react-intl');
const bignumber_js_1 = require('bignumber.js');
const validations_1 = require('../../../utils/validations');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const WalletsDropdown_1 = __importDefault(
  require('../../widgets/forms/WalletsDropdown')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const Step1ConfigurationDialog_scss_1 = __importDefault(
  require('./Step1ConfigurationDialog.scss')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const timingConfig_1 = require('../../../config/timingConfig');
const stakingConfig_1 = require('../../../config/stakingConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross-thin.inline.svg')
);
const mnemonic_input_1 = require('../../wallet/mnemonic-input');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.redeemItnRewards.step1.title',
    defaultMessage: '!!!Redeem Incentivized Testnet rewards',
    description: 'Title for Redeem Incentivized Testnet - Step 1',
  },
  description1: {
    id: 'staking.redeemItnRewards.step1.description1',
    defaultMessage:
      '!!!If you participated in the { itnLink } and earned rewards by running a stake pool or delegating your stake, you can use this feature to redeem your rewards as ada on the Cardano mainnet.',
    description: 'description for Redeem Incentivized Testnet - Step 1',
  },
  description2: {
    id: 'staking.redeemItnRewards.step1.description2',
    defaultMessage:
      '!!!You will need the <b>wallet recovery phrase for the Incentivized Testnet wallet</b> used to earn rewards and an existing mainnet wallet in Daedalus to which the rewards will be transferred. This wallet will also be used to pay any applicable transaction fees.',
    description: 'description for Redeem Incentivized Testnet - Step 1',
  },
  descriptionItnLinkLabel: {
    id: 'staking.redeemItnRewards.step1.descriptionItnLinkLabel',
    defaultMessage: '!!!Incentivized Testnet',
    description:
      'descriptionItnLinkLabel for Redeem Incentivized Testnet - Step 1',
  },
  descriptionItnLinkUrl: {
    id: 'staking.redeemItnRewards.step1.descriptionItnLinkUrl',
    defaultMessage: '!!!https://staking.cardano.org/',
    description:
      'descriptionItnLinkUrl for Redeem Incentivized Testnet - Step 1',
  },
  recoveryPhraseLabel: {
    id: 'staking.redeemItnRewards.step1.recoveryPhraseLabel',
    defaultMessage: '!!!Wallet recovery phrase:',
    description: 'recoveryPhraseLabel for Redeem Incentivized Testnet - Step 1',
  },
  walletsDropdownLabel: {
    id: 'staking.redeemItnRewards.step1.walletsDropdownLabel',
    defaultMessage: '!!!Redeem rewards to:',
    description:
      'walletsDropdownLabel for Redeem Incentivized Testnet - Step 1',
  },
  checkbox1Label: {
    id: 'staking.redeemItnRewards.step1.checkbox1Label',
    defaultMessage:
      '!!!I understand that redeeming rewards from the Incentivized Testnet requires paying transaction fees.',
    description: 'checkbox1Label for Redeem Incentivized Testnet - Step 1',
  },
  checkbox2Label: {
    id: 'staking.redeemItnRewards.step1.checkbox2Label',
    defaultMessage:
      '!!!I understand that fees will be paid from the wallet I am redeeming my rewards to.',
    description: 'checkbox2Label for Redeem Incentivized Testnet - Step 1',
  },
  continueButtonLabel: {
    id: 'staking.redeemItnRewards.step1.continueButton.label',
    defaultMessage: '!!!Continue',
    description: 'continueButtonLabel for Redeem Incentivized Testnet - Step 1',
  },
  learnMoreLinkLabel: {
    id: 'staking.redeemItnRewards.step1.learnMoreLink.label',
    defaultMessage: '!!!Learn More',
    description: 'learnMoreLinkLabel for Redeem Incentivized Testnet - Step 1',
  },
  learnMoreLinkUrl: {
    id: 'staking.redeemItnRewards.step1.learnMoreLink.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900001656586',
    description: 'learnMoreLinkUrl for Redeem Incentivized Testnet - Step 1',
  },
  recoveryPhraseInputHint: {
    id: 'staking.redeemItnRewards.step1.recoveryPhraseInputHint',
    defaultMessage: '!!!Enter recovery phrase',
    description:
      'Hint "Enter recovery phrase" for the recovery phrase input on the wallet restore dialog.',
  },
  selectWalletInputPlaceholder: {
    id: 'staking.redeemItnRewards.step1.selectWalletInputPlaceholder',
    defaultMessage: '!!!Select Wallet',
    description:
      'Placeholder "Select Wallet" for select input on the delegation setup "choose wallet" step dialog.',
  },
  noResults: {
    id: 'staking.redeemItnRewards.step1.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the recovery phrase input search results.',
  },
  invalidRecoveryPhrase: {
    id: 'staking.redeemItnRewards.step1.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
});
let Step1ConfigurationDialog = class Step1ConfigurationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    error: null,
    recoveryPhrase: [],
  };
  state = {
    wasRecoveryPhraseValidAtLeastOnce: false,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        recoveryPhrase: {
          value: [...(this.props.recoveryPhrase || [])],
          label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
          validators: ({ field }) =>
            (0, validations_1.validateMnemonics)({
              requiredWords:
                cryptoConfig_1.ITN_WALLET_RECOVERY_PHRASE_WORD_COUNT,
              providedWords: field.value,
              validator: (providedWords) => [
                this.props.mnemonicValidator(
                  providedWords.join(' '),
                  providedWords.length
                ),
                this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
              ],
            }),
          hooks: {
            onChange: (field) => {
              if (
                this.state.wasRecoveryPhraseValidAtLeastOnce === false &&
                field.isValid
              ) {
                this.setState({ wasRecoveryPhraseValidAtLeastOnce: true });
              }
            },
          },
        },
        walletsDropdown: {
          type: 'select',
          label: this.context.intl.formatMessage(messages.walletsDropdownLabel),
        },
        checkboxAcceptance1: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(messages.checkbox1Label),
        },
        checkboxAcceptance2: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(messages.checkbox2Label),
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: true,
        showErrorsOnChange: false,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.form.submit({
      onSuccess: () => this.props.onContinue(),
    });
  };
  get canSubmit() {
    const { isCalculatingReedemFees, wallet, error } = this.props;
    const { form } = this;
    const { checked: checkboxAcceptance1isChecked } = form.$(
      'checkboxAcceptance1'
    );
    const { checked: checkboxAcceptance2isChecked } = form.$(
      'checkboxAcceptance2'
    );
    return (
      !isCalculatingReedemFees &&
      wallet &&
      !error &&
      checkboxAcceptance1isChecked &&
      checkboxAcceptance2isChecked &&
      form.isValid
    );
  }
  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      isCalculatingReedemFees,
      onClose,
      onContinue,
      onSelectWallet,
      wallet,
      suggestedMnemonics,
      openExternalLink,
      wallets,
      error,
    } = this.props;
    const calculatedMinRewardsReceiverBalance = new bignumber_js_1.BigNumber(
      stakingConfig_1.MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE
    );
    let errorMessage;
    if (
      !isCalculatingReedemFees &&
      error &&
      // @ts-ignore ts-migrate(2339) FIXME: Property 'id' does not exist on type 'LocalizableE... Remove this comment to see the full error message
      error.id === 'staking.redeemItnRewards.step1.errorRestoringWallet'
    )
      errorMessage = react_1.default.createElement(
        'p',
        { className: Step1ConfigurationDialog_scss_1.default.error },
        intl.formatMessage(error)
      );
    if (
      !isCalculatingReedemFees &&
      error &&
      // @ts-ignore ts-migrate(2339) FIXME: Property 'id' does not exist on type 'LocalizableE... Remove this comment to see the full error message
      error.id === 'staking.redeemItnRewards.step1.errorMessage'
    )
      errorMessage = react_1.default.createElement(
        'p',
        { className: Step1ConfigurationDialog_scss_1.default.errorMessage },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...error,
          values: {
            calculatedMinRewardsReceiverBalance,
          },
        })
      );
    const recoveryPhraseField = form.$('recoveryPhrase');
    const walletsDropdownField = form.$('walletsDropdown');
    const checkboxAcceptance1Field = form.$('checkboxAcceptance1');
    const checkboxAcceptance2Field = form.$('checkboxAcceptance2');
    const walletId = (0, lodash_1.get)(wallet, 'id', null);
    const walletsDropdownDisabled = !(
      recoveryPhraseField.isValid ||
      this.state.wasRecoveryPhraseValidAtLeastOnce
    );
    const actions = {
      direction: 'column',
      items: [
        {
          className: 'primary',
          disabled: !this.canSubmit,
          primary: true,
          label: intl.formatMessage(messages.continueButtonLabel),
          onClick: this.submit,
        },
        {
          onClick: (event) =>
            openExternalLink(
              intl.formatMessage(messages.learnMoreLinkUrl, event)
            ),
          label: intl.formatMessage(messages.learnMoreLinkLabel),
          isLink: true,
        },
      ],
    };
    const itnLink = react_1.default.createElement(Link_1.Link, {
      className: Step1ConfigurationDialog_scss_1.default.itnLink,
      onClick: (event) =>
        openExternalLink(
          intl.formatMessage(messages.descriptionItnLinkUrl, event)
        ),
      label: intl.formatMessage(messages.descriptionItnLinkLabel),
      skin: LinkSkin_1.LinkSkin,
    });
    const closeButton = react_1.default.createElement(
      DialogCloseButton_1.default,
      {
        icon: close_cross_thin_inline_svg_1.default,
        className: Step1ConfigurationDialog_scss_1.default.closeButton,
        onClose: onClose,
      }
    );
    const { ...mnemonicInputProps } = recoveryPhraseField.bind();
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        actions: actions,
        onContinue: onContinue,
        onClose: onClose,
        closeButton: closeButton,
        closeOnOverlayClick: false,
        fullSize: true,
      },
      react_1.default.createElement(
        'div',
        { className: Step1ConfigurationDialog_scss_1.default.component },
        react_1.default.createElement(
          'p',
          { className: Step1ConfigurationDialog_scss_1.default.description },
          react_1.default.createElement(react_intl_1.FormattedMessage, {
            ...messages.description1,
            values: {
              itnLink,
            },
          }),
          ' ',
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description2,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: Step1ConfigurationDialog_scss_1.default.recoveryPhrase },
          react_1.default.createElement(mnemonic_input_1.MnemonicInput, {
            ...mnemonicInputProps,
            label: intl.formatMessage(messages.recoveryPhraseInputHint),
            availableWords: suggestedMnemonics,
            wordCount: cryptoConfig_1.ITN_WALLET_RECOVERY_PHRASE_WORD_COUNT,
            error: recoveryPhraseField.error,
            reset: form.resetting,
          })
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              Step1ConfigurationDialog_scss_1.default.walletsDropdownWrapper,
          },
          react_1.default.createElement(WalletsDropdown_1.default, {
            ...walletsDropdownField.bind(),
            numberOfStakePools: 4,
            wallets: wallets,
            onChange: (id) => onSelectWallet(id, recoveryPhraseField.value),
            placeholder: intl.formatMessage(
              messages.selectWalletInputPlaceholder
            ),
            value: walletId,
            getStakePoolById: () => {},
            errorPosition: 'bottom',
            disabled: walletsDropdownDisabled,
          })
        ),
        react_1.default.createElement(Checkbox_1.Checkbox, {
          ...checkboxAcceptance1Field.bind(),
          className: Step1ConfigurationDialog_scss_1.default.checkbox,
          skin: CheckboxSkin_1.CheckboxSkin,
          error: checkboxAcceptance1Field.error,
        }),
        react_1.default.createElement(Checkbox_1.Checkbox, {
          ...checkboxAcceptance2Field.bind(),
          className: Step1ConfigurationDialog_scss_1.default.checkbox,
          skin: CheckboxSkin_1.CheckboxSkin,
          error: checkboxAcceptance2Field.error,
        }),
        errorMessage
      )
    );
  }
};
Step1ConfigurationDialog = __decorate(
  [mobx_react_1.observer],
  Step1ConfigurationDialog
);
exports.default = Step1ConfigurationDialog;
//# sourceMappingURL=Step1ConfigurationDialog.js.map
