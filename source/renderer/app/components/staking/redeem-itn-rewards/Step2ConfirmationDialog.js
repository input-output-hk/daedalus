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
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const Step2ConfirmationDialog_scss_1 = __importDefault(
  require('./Step2ConfirmationDialog.scss')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const formatters_1 = require('../../../utils/formatters');
const validations_1 = require('../../../utils/validations');
const form_1 = require('../../../utils/form');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const timingConfig_1 = require('../../../config/timingConfig');
const stakingConfig_1 = require('../../../config/stakingConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross-thin.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.redeemItnRewards.step2.title',
    defaultMessage: '!!!Confirm rewards redemption',
    description: 'title for Redeem Incentivized Testnet - Step 2',
  },
  walletToLabel: {
    id: 'staking.redeemItnRewards.step2.walletToLabel',
    defaultMessage: '!!!To',
    description: 'walletToLabel for Redeem Incentivized Testnet - Step 2',
  },
  walletToName: {
    id: 'staking.redeemItnRewards.step2.walletToName',
    defaultMessage: '!!!{walletName} <span>wallet</span>',
    description: 'walletToName for Redeem Incentivized Testnet - Step 2',
  },
  transactionFees: {
    id: 'staking.redeemItnRewards.step2.transactionFees',
    defaultMessage: '!!!Transaction fees',
    description: 'transactionFees for Redeem Incentivized Testnet - Step 2',
  },
  spendingPasswordLabel: {
    id: 'staking.redeemItnRewards.step2.spendingPasswordLabel',
    defaultMessage:
      '!!!Wallet spending password <em>(<b>{walletName}</b> wallet)</em>',
    description:
      'spendingPasswordLabel for Redeem Incentivized Testnet - Step 2',
  },
  spendingPasswordPlaceholder: {
    id: 'staking.redeemItnRewards.step2.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'spendingPasswordPlaceholder for Redeem Incentivized Testnet - Step 2',
  },
  continueButtonLabel: {
    id: 'staking.redeemItnRewards.step2.continueButtonLabel',
    defaultMessage: '!!!Confirm rewards redemption',
    description: 'continueButtonLabel for Redeem Incentivized Testnet - Step 2',
  },
  backButtonLabel: {
    id: 'staking.redeemItnRewards.step2.backButtonLabel',
    defaultMessage: '!!!Back',
    description:
      'Label for the back button in the wallet send confirmation dialog.',
  },
});
let Step2ConfirmationDialog = class Step2ConfirmationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    error: null,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: react_1.default.createElement(
            react_intl_1.FormattedHTMLMessage,
            {
              ...messages.spendingPasswordLabel,
              values: {
                walletName: this.props.wallet.name,
              },
            }
          ),
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              return [
                (0, validations_1.isValidSpendingPassword)(field.value),
                this.context.intl.formatMessage(
                  global_messages_1.default.invalidSpendingPassword
                ),
              ];
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
        validateOnChange: true,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { spendingPassword } = form.values();
        const { onContinue } = this.props;
        onContinue({
          spendingPassword,
        });
      },
    });
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      wallet,
      transactionFees,
      onContinue,
      onClose,
      onBack,
      error,
    } = this.props;
    const { amount } = wallet || {};
    const minRewardsReceiverBalance = new bignumber_js_1.default(
      stakingConfig_1.MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE
    );
    const differenceBetweenAmountAndFee = amount.minus(transactionFees);
    const calculatedTransactionFees = differenceBetweenAmountAndFee.isLessThan(
      minRewardsReceiverBalance
    )
      ? amount
      : transactionFees;
    const { name: walletName } = wallet;
    const spendingPasswordField = form.$('spendingPassword');
    const actions = {
      direction: 'column',
      items: [
        {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'isValid' does not exist on type 'ReactTo... Remove this comment to see the full error message
          disabled: !form.isValid,
          primary: true,
          label: intl.formatMessage(messages.continueButtonLabel),
          onClick: this.submit,
        },
        {
          onClick: onBack,
          label: intl.formatMessage(messages.backButtonLabel),
          isLink: true,
          hasIconAfter: false,
        },
      ],
    };
    const closeButton = react_1.default.createElement(
      DialogCloseButton_1.default,
      {
        icon: close_cross_thin_inline_svg_1.default,
        className: Step2ConfirmationDialog_scss_1.default.closeButton,
        onClose: onClose,
      }
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        actions: actions,
        onContinue: onContinue,
        onClose: onClose,
        closeButton: closeButton,
        onBack: onBack,
        closeOnOverlayClick: false,
        fullSize: true,
      },
      react_1.default.createElement(
        'div',
        { className: Step2ConfirmationDialog_scss_1.default.to },
        react_1.default.createElement(
          'div',
          null,
          intl.formatMessage(messages.walletToLabel)
        ),
        react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.walletToName,
            values: {
              walletName,
            },
          })
        )
      ),
      react_1.default.createElement(
        'div',
        { className: Step2ConfirmationDialog_scss_1.default.transactionFees },
        react_1.default.createElement(
          'div',
          null,
          intl.formatMessage(messages.transactionFees)
        ),
        react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(
            'b',
            null,
            (0, formatters_1.formattedWalletAmount)(
              calculatedTransactionFees,
              false
            ),
            '\u00A0'
          ),
          react_1.default.createElement(
            'em',
            null,
            intl.formatMessage(global_messages_1.default.adaUnit)
          )
        )
      ),
      react_1.default.createElement(Input_1.Input, {
        className: Step2ConfirmationDialog_scss_1.default.spendingPassword,
        ...spendingPasswordField.bind(),
        skin: InputSkin_1.InputSkin,
        error: spendingPasswordField.error,
        onKeyPress: this.handleSubmitOnEnter,
      }),
      error &&
        react_1.default.createElement(
          'p',
          { className: Step2ConfirmationDialog_scss_1.default.error },
          intl.formatMessage(error)
        )
    );
  }
};
Step2ConfirmationDialog = __decorate(
  [mobx_react_1.observer],
  Step2ConfirmationDialog
);
exports.default = Step2ConfirmationDialog;
//# sourceMappingURL=Step2ConfirmationDialog.js.map
