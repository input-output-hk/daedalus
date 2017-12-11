// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import { intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from '../WalletSendConfirmationDialog.scss';
import { formattedAmountWithoutTrailingZeros } from '../../../utils/formatters';
import { messages } from '../WalletSendConfirmationDialog';

type Props = {
  isWalletPasswordSet: boolean,
  amount: string,
  receiver: string,
  totalAmount: string,
  transactionFee: string,
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
      walletPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.walletPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.walletPasswordFieldPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (this.props.isWalletPasswordSet && field.value === '') {
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          return [true];
        }],
      },
    }
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  submit() {
    this.form.submit({
      onSuccess: (form) => {
        const { isWalletPasswordSet, receiver, amount, amountToNaturalUnits } = this.props;
        const { walletPassword } = form.values();
        const transactionData = {
          receiver,
          amount: amountToNaturalUnits(amount),
          password: isWalletPasswordSet ? walletPassword : null,
        };
        this.props.onSubmit(transactionData);
      },
      onError: () => {}
    });
  }

  render() {
    const { form } = this;
    const { intl } = this.context;
    const walletPasswordField = form.$('walletPassword');
    const {
      onCancel,
      isWalletPasswordSet,
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
        onClick: this.submit.bind(this),
        primary: true,
        className: confirmButtonClasses,
        disabled: !walletPasswordField.isValid,
      },
    ];

    const formattedAmount = formattedAmountWithoutTrailingZeros(amount);
    const formattedTransactionFee = formattedAmountWithoutTrailingZeros(transactionFee);
    const formattedTotalAmount = formattedAmountWithoutTrailingZeros(totalAmount);

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : null}
        className={styles.dialog}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.walletPasswordFields}>
          <div className={styles.addressToLabelWrapper}>
            <div className={styles.addressToLabel}>
              {intl.formatMessage(messages.addressToLabel)}
            </div>
            <div className={styles.addressTo}>{receiver}</div>
          </div>

          <div className={styles.amountFeesWrapper}>
            <div className={styles.amountWrapper}>
              <div className={styles.amountLabel}>{intl.formatMessage(messages.amountLabel)}</div>
              <div className={styles.amount}>{formattedAmount}
                <span className={styles.currencySymbol}>&nbsp;{currencyUnit}</span>
              </div>
            </div>

            <div className={styles.feesWrapper}>
              <div className={styles.feesLabel}>{intl.formatMessage(messages.feesLabel)}</div>
              <div className={styles.fees}>+{formattedTransactionFee}
                <span className={styles.currencySymbol}>&nbsp;{currencyUnit}</span>
              </div>
            </div>
          </div>

          <div className={styles.totalAmountWrapper}>
            <div className={styles.totalAmountLabel}>{intl.formatMessage(messages.totalLabel)}</div>
            <div className={styles.totalAmount}>{formattedTotalAmount}
              <span className={styles.currencySymbol}>&nbsp;{currencyUnit}</span>
            </div>
          </div>

          {isWalletPasswordSet ? (
            <Input
              type="password"
              className={styles.walletPassword}
              {...walletPasswordField.bind()}
              error={walletPasswordField.error}
              skin={<SimpleInputSkin />}
            />
          ) : null}
        </div>

        {error ? <p className={styles.error}>{intl.formatMessage(error)}</p> : null}

      </Dialog>
    );
  }

}
