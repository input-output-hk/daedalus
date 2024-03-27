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
// @ts-nocheck
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const classnames_1 = __importDefault(require('classnames'));
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const Input_1 = require('@react-polymorph/components/Input');
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const timingConfig_1 = require('../../../config/timingConfig');
const formatters_1 = require('../../../utils/formatters');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const FormattedHTMLMessageWithLink_1 = require('../../widgets/FormattedHTMLMessageWithLink');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const Wallet_1 = require('../../../domains/Wallet');
const HardwareWalletStatus_1 = __importDefault(
  require('../../hardware-wallet/HardwareWalletStatus')
);
const UndelegateWalletConfirmationDialog_scss_1 = __importDefault(
  require('./UndelegateWalletConfirmationDialog.scss')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const form_1 = require('../../../utils/form');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.settings.undelegate.dialog.title',
    defaultMessage: '!!!Undelegate',
    description: 'Title for the "Undelegate wallet" dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.undelegate.dialog.confirmButtonLabel',
    defaultMessage: '!!!Undelegate',
    description:
      'Label for the "Undelegate" button in the undelegate wallet dialog.',
  },
  descriptionWithTicker: {
    id: 'wallet.settings.undelegate.dialog.descriptionWithTicker',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>[{stakePoolTicker}] {stakePoolName}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
    description:
      'Description of current delegation of wallet in the "Undelegate wallet" dialog.',
  },
  descriptionWithUnknownTicker: {
    id: 'wallet.settings.undelegate.dialog.descriptionWithUnknownTicker',
    defaultMessage:
      '!!!<p>The stake from your wallet <strong>{walletName}</strong> is currently delegated to the <strong>{stakePoolTicker}</strong> stake pool.</p><p>Do you want to undelegate your stake and stop earning rewards?</p>',
    description:
      'Description of current delegation of wallet in the "Undelegate wallet" dialog.',
  },
  unknownStakePoolLabel: {
    id: 'wallet.settings.undelegate.dialog.unknownStakePoolLabel',
    defaultMessage: '!!!unknown',
    description: 'unknown stake pool label in the "Undelegate wallet" dialog.',
  },
  confirmUnsupportNotice: {
    id: 'wallet.settings.undelegate.dialog.confirmUnsupportNotice',
    defaultMessage:
      '!!!I understand that I am not supporting the Cardano network when my stake is undelegated.',
    description:
      'Notice to confirm if the user understands unsupporting Cardano network after undelegation',
  },
  confirmIneligibleNotice: {
    id: 'wallet.settings.undelegate.dialog.confirmIneligibleNotice',
    defaultMessage:
      '!!!I understand that I will not be eligible to earn rewards when my stake is undelegated.',
    description:
      'Notice to confirm if the user understands non-earning rewards after undelegation',
  },
  feesLabel: {
    id: 'wallet.settings.undelegate.dialog.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label in the "Undelegate wallet" dialog.',
  },
  depositLabel: {
    id: 'wallet.settings.undelegate.dialog.depositLabel',
    defaultMessage: '!!!Deposits reclaimed',
    description: 'Deposits reclaimed label in the "Undelegate wallet" dialog.',
  },
  spendingPasswordLabel: {
    id: 'wallet.settings.undelegate.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Spending password label in the "Undelegate wallet" dialog.',
  },
  spendingPasswordPlaceholder: {
    id: 'wallet.settings.undelegate.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Type your spending password here',
    description:
      'Spending password placeholder in the "Undelegate wallet" dialog.',
  },
  passwordErrorMessage: {
    id: 'wallet.settings.undelegate.dialog.passwordError',
    defaultMessage: '!!!Incorrect spending password.',
    description: 'Label for password error in the "Undelegate wallet" dialog.',
  },
  calculatingFees: {
    id: 'wallet.settings.undelegate.dialog.calculatingFees',
    defaultMessage: '!!!Calculating fees',
    description:
      '"Calculating fees" message in the "Undelegate wallet" dialog.',
  },
});
messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
let UndelegateWalletConfirmationDialog = class UndelegateWalletConfirmationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        confirmUnsupportChecked: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(
            messages.confirmUnsupportNotice
          ),
          value: false,
          validators: [
            ({ field }) => {
              if (field.value === false) {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
        confirmIneligibleChecked: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(
            messages.confirmIneligibleNotice
          ),
          value: false,
          validators: [
            ({ field }) => {
              if (field.value === false) {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
        passphrase: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              const isHardwareWallet = (0, lodash_1.get)(
                this.props.selectedWallet,
                'isHardwareWallet'
              );
              if (isHardwareWallet) return [true];
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
  confirmationDisabled = () => {
    const { form } = this;
    const { fees, isSubmitting, hwDeviceStatus, selectedWallet } = this.props;
    const { isValid: unsupportCheckboxIsValid } = form.$(
      'confirmUnsupportChecked'
    );
    const { isValid: ineligibleCheckboxIsValid } = form.$(
      'confirmIneligibleChecked'
    );
    const { isValid: passphraseIsValid } = form.$('passphrase');
    const isHardwareWallet = (0, lodash_1.get)(
      selectedWallet,
      'isHardwareWallet'
    );
    if (isHardwareWallet) {
      return (
        hwDeviceStatus !==
        Wallet_1.HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED
      );
    }
    return (
      isSubmitting ||
      !fees ||
      !unsupportCheckboxIsValid ||
      !ineligibleCheckboxIsValid ||
      !passphraseIsValid
    );
  };
  handleSubmit = () => {
    if (this.confirmationDisabled()) {
      return false;
    }
    return this.form.submit({
      onSuccess: (form) => {
        const { selectedWallet, onConfirm } = this.props;
        const isHardwareWallet = (0, lodash_1.get)(
          selectedWallet,
          'isHardwareWallet'
        );
        const { passphrase } = form.values();
        onConfirm(passphrase, isHardwareWallet);
      },
      onError: () => null,
    });
  };
  handleSubmitOnEnter = (event) =>
    (0, form_1.submitOnEnter)(this.handleSubmit, event);
  generateErrorElement = () => {
    const { error, onExternalLinkClick } = this.props;
    if (!error) {
      return null;
    }
    const errorHasLink = !!(0, lodash_1.get)(error, 'values.linkLabel', false);
    const result = errorHasLink
      ? react_1.default.createElement(
          FormattedHTMLMessageWithLink_1.FormattedHTMLMessageWithLink,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          {
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            message: error,
            onExternalLinkClick: onExternalLinkClick,
          }
        )
      : this.context.intl.formatMessage(error);
    return result;
  };
  render() {
    const { form } = this;
    const { intl } = this.context;
    const unsupportCheckboxField = form.$('confirmUnsupportChecked');
    const ineligibleCheckboxField = form.$('confirmIneligibleChecked');
    const passphraseField = form.$('passphrase');
    const {
      selectedWallet,
      stakePoolName,
      stakePoolTicker,
      onCancel,
      isSubmitting,
      fees,
      hwDeviceStatus,
      onExternalLinkClick,
      isTrezor,
    } = this.props;
    const walletName = (0, lodash_1.get)(selectedWallet, 'name');
    const isHardwareWallet = (0, lodash_1.get)(
      selectedWallet,
      'isHardwareWallet'
    );
    const confirmationDisabled = this.confirmationDisabled();
    const buttonClasses = (0, classnames_1.default)([
      'attention',
      isSubmitting
        ? UndelegateWalletConfirmationDialog_scss_1.default.isSubmitting
        : null,
    ]);
    const actions = [
      {
        label: intl.formatMessage(global_messages_1.default.cancel),
        onClick: !isSubmitting ? onCancel : () => null,
      },
      {
        className: buttonClasses,
        label: intl.formatMessage(messages.confirmButtonLabel),
        onClick: this.handleSubmit,
        disabled: confirmationDisabled,
        primary: true,
      },
    ];
    const errorElement = this.generateErrorElement();
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        subtitle: walletName,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: !isSubmitting ? onCancel : () => null,
        className: UndelegateWalletConfirmationDialog_scss_1.default.dialog,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: !isSubmitting ? onCancel : () => null }
        ),
      },
      react_1.default.createElement(
        'div',
        {
          className:
            UndelegateWalletConfirmationDialog_scss_1.default.description,
        },
        stakePoolTicker
          ? react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...messages.descriptionWithTicker,
              values: {
                walletName,
                stakePoolName,
                stakePoolTicker,
              },
            })
          : react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...messages.descriptionWithUnknownTicker,
              values: {
                walletName,
                stakePoolTicker: intl.formatMessage(
                  messages.unknownStakePoolLabel
                ),
              },
            })
      ),
      react_1.default.createElement(Checkbox_1.Checkbox, {
        ...unsupportCheckboxField.bind(),
        error: unsupportCheckboxField.error,
      }),
      react_1.default.createElement(Checkbox_1.Checkbox, {
        ...ineligibleCheckboxField.bind(),
        error: ineligibleCheckboxField.error,
      }),
      react_1.default.createElement('div', {
        className: UndelegateWalletConfirmationDialog_scss_1.default.divider,
      }),
      react_1.default.createElement(
        'div',
        {
          className: UndelegateWalletConfirmationDialog_scss_1.default.feesRow,
        },
        react_1.default.createElement(
          'div',
          {
            className:
              UndelegateWalletConfirmationDialog_scss_1.default.feesWrapper,
          },
          react_1.default.createElement(
            'p',
            {
              className:
                UndelegateWalletConfirmationDialog_scss_1.default.feesLabel,
            },
            intl.formatMessage(messages.feesLabel)
          ),
          react_1.default.createElement(
            'p',
            {
              className:
                UndelegateWalletConfirmationDialog_scss_1.default.feesAmount,
            },
            !fees || !fees.fee
              ? react_1.default.createElement(
                  'span',
                  {
                    className:
                      UndelegateWalletConfirmationDialog_scss_1.default
                        .calculatingFeesLabel,
                  },
                  intl.formatMessage(messages.calculatingFees)
                )
              : react_1.default.createElement(
                  react_1.default.Fragment,
                  null,
                  react_1.default.createElement(
                    'span',
                    null,
                    (0, formatters_1.formattedWalletAmount)(fees.fee, false)
                  ),
                  react_1.default.createElement(
                    'span',
                    {
                      className:
                        UndelegateWalletConfirmationDialog_scss_1.default
                          .feesAmountLabel,
                    },
                    ` `,
                    intl.formatMessage(global_messages_1.default.adaUnit)
                  )
                )
          )
        ),
        fees &&
          !fees.depositsReclaimed.isZero() &&
          react_1.default.createElement(
            'div',
            {
              className:
                UndelegateWalletConfirmationDialog_scss_1.default
                  .depositWrapper,
            },
            react_1.default.createElement(
              'p',
              {
                className:
                  UndelegateWalletConfirmationDialog_scss_1.default
                    .depositLabel,
              },
              intl.formatMessage(messages.depositLabel)
            ),
            react_1.default.createElement(
              'p',
              {
                className:
                  UndelegateWalletConfirmationDialog_scss_1.default
                    .depositAmount,
              },
              react_1.default.createElement(
                'span',
                null,
                (0, formatters_1.formattedWalletAmount)(
                  fees.depositsReclaimed,
                  false
                )
              ),
              react_1.default.createElement(
                'span',
                {
                  className:
                    UndelegateWalletConfirmationDialog_scss_1.default
                      .depositAmountLabel,
                },
                ` `,
                intl.formatMessage(global_messages_1.default.adaUnit)
              )
            )
          )
      ),
      isHardwareWallet
        ? react_1.default.createElement(
            'div',
            {
              className:
                UndelegateWalletConfirmationDialog_scss_1.default
                  .hardwareWalletStatusWrapper,
            },
            react_1.default.createElement(HardwareWalletStatus_1.default, {
              hwDeviceStatus: hwDeviceStatus,
              walletName: walletName,
              isTrezor: isTrezor,
              onExternalLinkClick: onExternalLinkClick,
            })
          )
        : react_1.default.createElement(Input_1.Input, {
            type: 'password',
            ...passphraseField.bind(),
            error: passphraseField.error,
            onKeyPress: this.handleSubmitOnEnter,
          }),
      errorElement &&
        react_1.default.createElement(
          'p',
          {
            className: UndelegateWalletConfirmationDialog_scss_1.default.error,
          },
          errorElement
        )
    );
  }
};
UndelegateWalletConfirmationDialog = __decorate(
  [mobx_react_1.observer],
  UndelegateWalletConfirmationDialog
);
exports.default = UndelegateWalletConfirmationDialog;
//# sourceMappingURL=UndelegateWalletConfirmationDialog.js.map
