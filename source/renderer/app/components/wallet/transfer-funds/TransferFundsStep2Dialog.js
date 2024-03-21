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
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const DialogBackButton_1 = __importDefault(
  require('../../widgets/DialogBackButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const TransferFundsStep2Dialog_scss_1 = __importDefault(
  require('./TransferFundsStep2Dialog.scss')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const timingConfig_1 = require('../../../config/timingConfig');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const form_1 = require('../../../utils/form');
const formatters_1 = require('../../../utils/formatters');
const numbersConfig_1 = require('../../../config/numbersConfig');
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.transferFunds.dialog2.title',
    defaultMessage: '!!!Transfer funds from the legacy wallet',
    description: 'Title in the transfer funds form.',
  },
  description: {
    id: 'wallet.transferFunds.dialog2.description.label',
    defaultMessage:
      '!!!Confirm transfer from {sourceWalletName}wallet to the {targetWalletName} wallet.',
    description: 'description in the transfer funds form.',
  },
  sourceWalletAmountLabel: {
    id: 'wallet.transferFunds.dialog2.sourceWalletAmount.label',
    defaultMessage: '!!!{sourceWalletName} amount',
    description: 'Label Source wallet Amount in the transfer funds form',
  },
  feesLabel: {
    id: 'wallet.transferFunds.dialog2.fees.label',
    defaultMessage: '!!!Fees',
    description: 'Label Fees in the transfer funds form',
  },
  totalLabel: {
    id: 'wallet.transferFunds.dialog2.total.label',
    defaultMessage: '!!!Total',
    description: 'Total Fees in the transfer funds form',
  },
  leftoversLabel: {
    id: 'wallet.transferFunds.dialog2.leftovers.label',
    defaultMessage: '!!!Leftovers',
    description: 'Label Leftovers in the transfer funds form',
  },
  leftoversLearnMoreLabel: {
    id: 'wallet.transferFunds.dialog2.leftovers.LearnMore.label',
    defaultMessage: '!!!Learn more',
    description: 'Label Leftovers in the transfer funds form',
  },
  leftoversLearnMoreUrl: {
    id: 'wallet.transferFunds.dialog2.leftovers.LearnMore.url',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/articles/',
    description: 'Label Leftovers in the transfer funds form',
  },
  buttonLabel: {
    id: 'wallet.transferFunds.dialog2.label.buttonLabel',
    defaultMessage: '!!!Transfer funds',
    description: 'buttonLabel in the transfer funds form.',
  },
  passphraseFieldPlaceholder: {
    id: 'wallet.transferFunds.dialog2.passphraseFieldPlaceholder',
    defaultMessage: '!!!Spending password',
    description: 'passphraseFieldPlaceholder in the transfer funds form.',
  },
  passphraseLabel: {
    id: 'wallet.transferFunds.dialog2.passphraseLabel',
    defaultMessage: '!!!Spending password',
    description: 'passphraseLabel in the transfer funds form.',
  },
});
messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
let TransferFundsStep2Dialog = class TransferFundsStep2Dialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.passphraseLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passphraseFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (field.value === '') {
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
        validateOnChange: true,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { spendingPassword } = form.values();
        this.props.onFinish(spendingPassword);
      },
      onError: () => {},
    });
  };
  handleSubmitOnEnter = (event) =>
    this.form.$('spendingPassword').isValid &&
    (0, form_1.submitOnEnter)(this.submit, event);
  render() {
    const { intl } = this.context;
    const {
      onClose,
      onBack,
      feesAmount,
      leftoversAmount,
      sourceWalletName,
      sourceWalletAmount,
      targetWalletName,
      onOpenExternalLink,
      isSubmitting,
      error,
    } = this.props;
    const fees = feesAmount.toFormat(numbersConfig_1.DECIMAL_PLACES_IN_ADA);
    const leftovers =
      leftoversAmount && !leftoversAmount.isZero()
        ? leftoversAmount.toFormat(numbersConfig_1.DECIMAL_PLACES_IN_ADA)
        : null;
    const totalAmount = sourceWalletAmount
      .minus(feesAmount)
      .minus(leftoversAmount);
    const totalToBeReceived = (0, formatters_1.formattedWalletAmount)(
      totalAmount,
      false
    );
    const sourceWalletBalance = (0, formatters_1.formattedWalletAmount)(
      sourceWalletAmount,
      false
    );
    const spendingPasswordField = this.form.$('spendingPassword');
    const buttonClasses = (0, classnames_1.default)([
      'confirmButton',
      isSubmitting
        ? TransferFundsStep2Dialog_scss_1.default.submitButtonSpinning
        : null,
    ]);
    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabel),
        onClick: this.submit,
        primary: true,
        className: buttonClasses,
        disabled: isSubmitting || !spendingPasswordField.isValid,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: TransferFundsStep2Dialog_scss_1.default.dialog,
        title: intl.formatMessage(messages.dialogTitle),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
        backButton: react_1.default.createElement(DialogBackButton_1.default, {
          onBack: onBack,
        }),
      },
      react_1.default.createElement(
        react_intl_1.FormattedMessage,
        {
          ...messages.description,
          values: {
            sourceWalletName: react_1.default.createElement(
              'b',
              { key: 'source' },
              sourceWalletName
            ),
            targetWalletName: react_1.default.createElement(
              'b',
              { key: 'target' },
              targetWalletName
            ),
          },
        },
        (...content) =>
          react_1.default.createElement(
            'div',
            { className: TransferFundsStep2Dialog_scss_1.default.description },
            content
          )
      ),
      react_1.default.createElement(
        'div',
        { className: TransferFundsStep2Dialog_scss_1.default.amountGroupFull },
        react_1.default.createElement(
          'p',
          { className: TransferFundsStep2Dialog_scss_1.default.label },
          react_1.default.createElement(react_intl_1.FormattedMessage, {
            ...messages.sourceWalletAmountLabel,
            values: {
              sourceWalletName: react_1.default.createElement(
                'b',
                { key: 'source' },
                sourceWalletName
              ),
            },
          })
        ),
        react_1.default.createElement(
          'div',
          { className: TransferFundsStep2Dialog_scss_1.default.amount },
          sourceWalletBalance
        )
      ),
      react_1.default.createElement(
        'div',
        { className: TransferFundsStep2Dialog_scss_1.default.amountGroup },
        react_1.default.createElement(
          'p',
          { className: TransferFundsStep2Dialog_scss_1.default.label },
          intl.formatMessage(messages.feesLabel)
        ),
        react_1.default.createElement(
          'div',
          { className: TransferFundsStep2Dialog_scss_1.default.amountOpacity },
          fees
        )
      ),
      leftovers &&
        react_1.default.createElement(
          'div',
          { className: TransferFundsStep2Dialog_scss_1.default.amountGroup },
          react_1.default.createElement(
            'p',
            { className: TransferFundsStep2Dialog_scss_1.default.label },
            intl.formatMessage(messages.leftoversLabel),
            react_1.default.createElement(Link_1.Link, {
              className:
                TransferFundsStep2Dialog_scss_1.default.leftoversLearnMoreLink,
              onClick: (event) =>
                onOpenExternalLink(
                  intl.formatMessage(messages.leftoversLearnMoreUrl, event)
                ),
              label: intl.formatMessage(messages.leftoversLearnMoreLabel),
              skin: LinkSkin_1.LinkSkin,
            })
          ),
          react_1.default.createElement(
            'div',
            {
              className: TransferFundsStep2Dialog_scss_1.default.amountOpacity,
            },
            leftovers
          )
        ),
      react_1.default.createElement(
        'div',
        { className: TransferFundsStep2Dialog_scss_1.default.amountGroupFull },
        react_1.default.createElement(
          'p',
          { className: TransferFundsStep2Dialog_scss_1.default.label },
          intl.formatMessage(messages.totalLabel)
        ),
        react_1.default.createElement(
          'div',
          { className: TransferFundsStep2Dialog_scss_1.default.amount },
          totalToBeReceived
        )
      ),
      react_1.default.createElement(Input_1.Input, {
        type: 'password',
        ...spendingPasswordField.bind(),
        error: spendingPasswordField.error,
        skin: InputSkin_1.InputSkin,
        onKeyPress: this.handleSubmitOnEnter,
        autoFocus: true,
      }),
      error
        ? react_1.default.createElement(
            'p',
            { className: TransferFundsStep2Dialog_scss_1.default.error },
            this.context.intl.formatMessage(error)
          )
        : null
    );
  }
};
TransferFundsStep2Dialog = __decorate(
  [mobx_react_1.observer],
  TransferFundsStep2Dialog
);
exports.default = TransferFundsStep2Dialog;
//# sourceMappingURL=TransferFundsStep2Dialog.js.map
