// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import Dialog from '../widgets/Dialog';
import DialogCloseButton from '../widgets/DialogCloseButton';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletSendConfirmationDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { submitOnEnter } from '../../utils/form';

export const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.send.confirmationDialog.title',
    defaultMessage: '!!!Confirm transaction',
    description: 'Title for the "Confirm transaction" dialog.'
  },
  spendingPasswordLabel: {
    id: 'wallet.send.confirmationDialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for the "Spending password" input in the wallet send confirmation dialog.',
  },
  addressToLabel: {
    id: 'wallet.send.confirmationDialog.addressToLabel',
    defaultMessage: '!!!To',
    description: 'Label for the "To" in the wallet send confirmation dialog.',
  },
  amountLabel: {
    id: 'wallet.send.confirmationDialog.amountLabel',
    defaultMessage: '!!!Amount',
    description: 'Label for the "Amount" in the wallet send confirmation dialog.',
  },
  feesLabel: {
    id: 'wallet.send.confirmationDialog.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Label for the "Fees" in the wallet send confirmation dialog.',
  },
  totalLabel: {
    id: 'wallet.send.confirmationDialog.totalLabel',
    defaultMessage: '!!!Total',
    description: 'Label for the "Total" in the wallet send confirmation dialog.',
  },
  spendingPasswordFieldPlaceholder: {
    id: 'wallet.send.confirmationDialog.spendingPasswordFieldPlaceholder',
    defaultMessage: '!!!Type your spending password',
    description: 'Placeholder for the "Spending password" inputs in the wallet send confirmation dialog.',
  },
  sendButtonLabel: {
    id: 'wallet.send.confirmationDialog.submit',
    defaultMessage: '!!!Send',
    description: 'Label for the send button in the wallet send confirmation dialog.'
  },
  backButtonLabel: {
    id: 'wallet.send.confirmationDialog.back',
    defaultMessage: '!!!Back',
    description: 'Label for the back button in the wallet send confirmation dialog.'
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  isSpendingPasswordSet: boolean,
  amount: string,
  receiver: string,
  totalAmount: ?string,
  transactionFee: ?string,
  onSubmit: Function,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  onCancel: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
  currencyUnit: string,
};

@observer
export default class WalletSendConfirmationDialog extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      spendingPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.spendingPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.spendingPasswordFieldPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (this.props.isSpendingPasswordSet && field.value === '') {
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          return [true];
        }],
      },
    }
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { isSpendingPasswordSet, receiver, amount, amountToNaturalUnits } = this.props;
        const { spendingPassword } = form.values();
        const transactionData = {
          receiver,
          amount: amountToNaturalUnits(amount),
          password: isSpendingPasswordSet ? spendingPassword : null,
        };
        this.props.onSubmit(transactionData);
      },
      onError: () => {}
    });
  };

  submitOnEnter = (event: {}) => this.form.$('spendingPassword').isValid && submitOnEnter(this.submit, event);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const spendingPasswordField = form.$('spendingPassword');
    const {
      onCancel,
      isSpendingPasswordSet,
      amount,
      receiver,
      totalAmount,
      transactionFee,
      isSubmitting,
      error,
      currencyUnit
    } = this.props;

    const confirmButtonClasses = classnames([
      'confirmButton',
      isSubmitting ? styles.submitButtonSpinning : null,
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.backButtonLabel),
        onClick: !isSubmitting && onCancel,
      },
      {
        label: intl.formatMessage(messages.sendButtonLabel),
        onClick: this.submit,
        primary: true,
        className: confirmButtonClasses,
        disabled: !spendingPasswordField.isValid,
      },
    ];

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        primaryButtonAutoFocus
        onClose={!isSubmitting ? onCancel : null}
        className={styles.dialog}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.spendingPasswordFields}>
          <div className={styles.addressToLabelWrapper}>
            <div className={styles.addressToLabel}>
              {intl.formatMessage(messages.addressToLabel)}
            </div>
            <div className={styles.addressTo}>{receiver}</div>
          </div>

          <div className={styles.amountFeesWrapper}>
            <div className={styles.amountWrapper}>
              <div className={styles.amountLabel}>{intl.formatMessage(messages.amountLabel)}</div>
              <div className={styles.amount}>{amount}
                <span className={styles.currencySymbol}>&nbsp;{currencyUnit}</span>
              </div>
            </div>

            <div className={styles.feesWrapper}>
              <div className={styles.feesLabel}>{intl.formatMessage(messages.feesLabel)}</div>
              <div className={styles.fees}>+{transactionFee}
                <span className={styles.currencySymbol}>&nbsp;{currencyUnit}</span>
              </div>
            </div>
          </div>

          <div className={styles.totalAmountWrapper}>
            <div className={styles.totalAmountLabel}>{intl.formatMessage(messages.totalLabel)}</div>
            <div className={styles.totalAmount}>{totalAmount}
              <span className={styles.currencySymbol}>&nbsp;{currencyUnit}</span>
            </div>
          </div>

          {isSpendingPasswordSet ? (
            <Input
              type="password"
              className={styles.spendingPassword}
              {...spendingPasswordField.bind()}
              error={spendingPasswordField.error}
              skin={InputSkin}
              onKeyPress={this.submitOnEnter}
              autoFocus
            />
          ) : null}
        </div>

        {error ? <p className={styles.error}>{intl.formatMessage(error)}</p> : null}

      </Dialog>
    );
  }

}
