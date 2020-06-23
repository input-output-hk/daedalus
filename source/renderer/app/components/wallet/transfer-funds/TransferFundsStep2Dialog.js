// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import styles from './TransferFundsStep2Dialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { formattedWalletAmount } from '../../../utils/formatters';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import Wallet from '../../../domains/Wallet';
import { submitOnEnter } from '../../../utils/form';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.transferFunds.dialog2.title',
    defaultMessage: '!!!Transfer funds from the legacy wallet',
    description: 'Title in the transfer funds form.',
  },
  description: {
    id: 'wallet.transferFunds.dialog2.label.description',
    defaultMessage:
      '!!!Confirm transfer from {sourceWalletName}wallet to the {targetWalletName} wallet.',
    description: 'description in the transfer funds form.',
  },
  labelAmount: {
    id: 'wallet.transferFunds.dialog2.label.amount',
    defaultMessage: '!!!Amount',
    description: 'Label Amount in the transfer funds form',
  },
  labelFees: {
    id: 'wallet.transferFunds.dialog2.label.fees',
    defaultMessage: '!!!Fees',
    description: 'Label Fees in the transfer funds form',
  },
  labelTotal: {
    id: 'wallet.transferFunds.dialog2.label.total',
    defaultMessage: '!!!Total',
    description: 'Total Fees in the transfer funds form',
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

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onFinish: Function,
  onClose: Function,
  onBack: Function,
  // addresses: Array<any>,
  sourceWallet: $Shape<Wallet>,
  targetWallet: $Shape<Wallet>,
  transferFundsFee: ?BigNumber,
  isSubmitting?: boolean,
  error?: ?LocalizableError,
};

type State = {
  total: string,
  fees: ?number,
  amount: ?string,
};

@observer
export default class TransferFundsStep2Dialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    total: formattedWalletAmount(this.props.sourceWallet.amount, false),
    fees: null,
    amount: null,
  };

  componentDidUpdate() {
    const { transferFundsFee, sourceWallet } = this.props;
    // "freezes" the current amounts in the component state
    if (transferFundsFee && !this.state.fees && !this.state.amount) {
      const fees = transferFundsFee.toFormat(DECIMAL_PLACES_IN_ADA);
      const amount = formattedWalletAmount(
        sourceWallet.amount.minus(transferFundsFee),
        false
      );
      this.setState({ fees, amount }); // eslint-disable-line
    }
  }

  form = new ReactToolboxMobxForm(
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
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { spendingPassword } = form.values();
        this.props.onFinish(spendingPassword);
      },
      onError: () => {},
    });
  };

  handleSubmitOnEnter = (event: {}) =>
    this.form.$('spendingPassword').isValid &&
    submitOnEnter(this.submit, event);

  render() {
    const { intl } = this.context;
    const { total, fees, amount } = this.state;
    const {
      onClose,
      onBack,
      // addresses,
      sourceWallet,
      targetWallet,
      isSubmitting,
      error,
    } = this.props;

    const spendingPasswordField = this.form.$('spendingPassword');

    const buttonClasses = classnames([
      'confirmButton',
      isSubmitting ? styles.submitButtonSpinning : null,
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

    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <FormattedMessage
          {...messages.description}
          values={{
            sourceWalletName: <b key="source">{sourceWallet.name}</b>,
            targetWalletName: <b key="target">{targetWallet.name}</b>,
          }}
        >
          {(...content) => <div className={styles.description}>{content}</div>}
        </FormattedMessage>
        <div className={styles.amountGroup}>
          <p className={styles.label}>
            {intl.formatMessage(messages.labelAmount)}
          </p>
          <div className={styles.amount}>{amount}</div>
        </div>
        <div className={styles.amountGroup}>
          <p className={styles.label}>
            {intl.formatMessage(messages.labelFees)}
          </p>
          <div className={styles.amount}>+ {fees}</div>
        </div>
        <div className={styles.amountGroup}>
          <p className={styles.label}>
            {intl.formatMessage(messages.labelTotal)}
          </p>
          <div className={styles.amount}>{total}</div>
        </div>
        <Input
          type="password"
          className={styles.currentPassword}
          {...spendingPasswordField.bind()}
          error={spendingPasswordField.error}
          skin={InputSkin}
          onKeyPress={this.handleSubmitOnEnter}
          autoFocus
        />
        {error ? (
          <p className={styles.error}>
            {this.context.intl.formatMessage(error)}
          </p>
        ) : null}
      </Dialog>
    );
  }
}
