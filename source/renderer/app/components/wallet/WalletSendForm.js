// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/raw/ButtonSkin';
import Input from 'react-polymorph/lib/components/Input';
import NumericInput from 'react-polymorph/lib/components/NumericInput';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import { defineMessages, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import AmountInputSkin from './skins/AmountInputSkin';
import BorderedBox from '../widgets/BorderedBox';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './WalletSendForm.scss';
import globalMessages from '../../i18n/global-messages';
import WalletSendConfirmationDialog from './WalletSendConfirmationDialog';
import WalletSendConfirmationDialogContainer from '../../containers/wallet/dialogs/WalletSendConfirmationDialogContainer';
import { formattedAmountToBigNumber, formattedAmountToNaturalUnits } from '../../utils/formatters';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';

export const messages = defineMessages({
  titleLabel: {
    id: 'wallet.send.form.title.label',
    defaultMessage: '!!!Title',
    description: 'Label for the "Title" text input in the wallet send form.'
  },
  titleHint: {
    id: 'wallet.send.form.title.hint',
    defaultMessage: '!!!E.g: Money for Frank',
    description: 'Hint inside the "Receiver" text input in the wallet send form.'
  },
  receiverLabel: {
    id: 'wallet.send.form.receiver.label',
    defaultMessage: '!!!Receiver',
    description: 'Label for the "Receiver" text input in the wallet send form.'
  },
  receiverHint: {
    id: 'wallet.send.form.receiver.hint',
    defaultMessage: '!!!Wallet Address',
    description: 'Hint inside the "Receiver" text input in the wallet send form.'
  },
  amountLabel: {
    id: 'wallet.send.form.amount.label',
    defaultMessage: '!!!Amount',
    description: 'Label for the "Amount" number input in the wallet send form.'
  },
  equalsAdaHint: {
    id: 'wallet.send.form.amount.equalsAda',
    defaultMessage: '!!!equals {amount} ADA',
    description: 'Convertion hint for the "Amount" number input in the wallet send form.'
  },
  descriptionLabel: {
    id: 'wallet.send.form.description.label',
    defaultMessage: '!!!Description',
    description: 'Label for the "description" text area in the wallet send form.'
  },
  descriptionHint: {
    id: 'wallet.send.form.description.hint',
    defaultMessage: '!!!You can add a message if you want',
    description: 'Hint in the "description" text area in the wallet send form.'
  },
  nextButtonLabel: {
    id: 'wallet.send.form.next',
    defaultMessage: '!!!Next',
    description: 'Label for the next button on the wallet send form.'
  },
  invalidAddress: {
    id: 'wallet.send.form.errors.invalidAddress',
    defaultMessage: '!!!Please enter a valid address.',
    description: 'Error message shown when invalid address was entered.'
  },
  invalidAmount: {
    id: 'wallet.send.form.errors.invalidAmount',
    defaultMessage: '!!!Please enter a valid amount.',
    description: 'Error message shown when invalid amount was entered.',
  },
  invalidTitle: {
    id: 'wallet.send.form.errors.invalidTitle',
    defaultMessage: '!!!Please enter a title with at least 3 characters.',
    description: 'Error message shown when invalid transaction title was entered.',
  },
  transactionFeeError: {
    id: 'wallet.send.form.transactionFeeError',
    defaultMessage: '!!!Not enough Ada for fees. Try sending a smaller amount.',
    description: '"Not enough Ada for fees. Try sending a smaller amount." error message',
  },
  syncingTransactionsMessage: {
    id: 'wallet.send.form.syncingTransactionsMessage',
    defaultMessage: '!!!This wallet is currently being synced with the blockchain. While synchronisation is in progress transacting is not possible and transaction history is not complete.',
    description: 'Syncing transactions message shown during async wallet restore in the wallet send form.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  currencyUnit: string,
  currencyMaxIntegerDigits?: number,
  currencyMaxFractionalDigits: number,
  validateAmount: (amountInNaturalUnits: string) => Promise<boolean>,
  calculateTransactionFee: (receiver: string, amount: string) => Promise<BigNumber>,
  addressValidator: Function,
  openDialogAction: Function,
  isDialogOpen: Function,
  isRestoreActive: boolean,
};

type State = {
  isTransactionFeeCalculated: boolean,
  transactionFee: BigNumber,
  transactionFeeError: ?string,
};

@observer
export default class WalletSendForm extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isTransactionFeeCalculated: false,
    transactionFee: new BigNumber(0),
    transactionFeeError: null,
  };

  // We need to track the fee calculation state in order to disable
  // the "Submit" button as soon as either receiver or amount field changes.
  // This is required as we are using debounced validation and we need to
  // disable the "Submit" button as soon as the value changes and then wait for
  // the validation to end in order to see if the button should be enabled or not.
  _isCalculatingFee = false;

  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  // FORM VALIDATION
  form = new ReactToolboxMobxForm({
    fields: {
      receiver: {
        label: this.context.intl.formatMessage(messages.receiverLabel),
        placeholder: this.context.intl.formatMessage(messages.receiverHint),
        value: '',
        validators: [async ({ field, form }) => {
          const value = field.value;
          if (value === '') {
            this._resetTransactionFee();
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          const isValid = await this.props.addressValidator(value);
          const amountField = form.$('amount');
          const amountValue = amountField.value;
          const isAmountValid = amountField.isValid;
          if (isValid && isAmountValid) {
            await this._calculateTransactionFee(value, amountValue);
          } else {
            this._resetTransactionFee();
          }
          return [isValid, this.context.intl.formatMessage(messages.invalidAddress)];
        }],
      },
      amount: {
        label: this.context.intl.formatMessage(messages.amountLabel),
        placeholder: `0.${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
        value: '',
        validators: [async ({ field, form }) => {
          const amountValue = field.value;
          if (amountValue === '') {
            this._resetTransactionFee();
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          const isValid = await this.props.validateAmount(
            formattedAmountToNaturalUnits(amountValue)
          );
          const receiverField = form.$('receiver');
          const receiverValue = receiverField.value;
          const isReceiverValid = receiverField.isValid;
          if (isValid && isReceiverValid) {
            await this._calculateTransactionFee(receiverValue, amountValue);
          } else {
            this._resetTransactionFee();
          }
          return [isValid, this.context.intl.formatMessage(messages.invalidAmount)];
        }],
      },
    },
  }, {
    options: {
      validateOnBlur: false,
      validateOnChange: true,
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      currencyUnit, currencyMaxIntegerDigits, currencyMaxFractionalDigits,
      openDialogAction, isDialogOpen, isRestoreActive,
    } = this.props;
    const { isTransactionFeeCalculated, transactionFee, transactionFeeError } = this.state;
    const amountField = form.$('amount');
    const receiverField = form.$('receiver');
    const receiverFieldProps = receiverField.bind();
    const amountFieldProps = amountField.bind();
    const amount = formattedAmountToBigNumber(amountFieldProps.value);

    let fees = null;
    let total = null;
    if (isTransactionFeeCalculated) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
      total = amount.add(transactionFee).toFormat(currencyMaxFractionalDigits);
    }

    const buttonClasses = classnames([
      'primary',
      styles.nextButton,
    ]);

    return (
      <div className={styles.component}>

        {isRestoreActive ? (
          <div className={styles.syncingTransactionsWrapper}>
            <LoadingSpinner big />
            <p className={styles.syncingTransactionsText}>
              {intl.formatMessage(messages.syncingTransactionsMessage)}
            </p>
          </div>
        ) : (
          <BorderedBox>
            <div className="WalletSendForm">
              <div className={styles.receiverInput}>
                <Input
                  className="receiver"
                  {...receiverField.bind()}
                  error={receiverField.error}
                  onChange={(value) => {
                    this._isCalculatingFee = true;
                    receiverField.onChange(value || '');
                  }}
                  skin={<SimpleInputSkin />}
                />
              </div>

              <div className={styles.amountInput}>
                <NumericInput
                  {...amountFieldProps}
                  className="amount"
                  label={intl.formatMessage(messages.amountLabel)}
                  maxBeforeDot={currencyMaxIntegerDigits}
                  maxAfterDot={currencyMaxFractionalDigits}
                  error={transactionFeeError || amountField.error}
                  onChange={(value) => {
                    this._isCalculatingFee = true;
                    amountField.onChange(value || '');
                  }}
                  // AmountInputSkin props
                  currency={currencyUnit}
                  fees={fees}
                  total={total}
                  skin={<AmountInputSkin />}
                />
              </div>

              <Button
                className={buttonClasses}
                label={intl.formatMessage(messages.nextButtonLabel)}
                onMouseUp={() => openDialogAction({
                  dialog: WalletSendConfirmationDialog,
                })}
                disabled={this._isCalculatingFee || !isTransactionFeeCalculated}
                skin={<SimpleButtonSkin />}
              />
            </div>
          </BorderedBox>
        )}

        {isDialogOpen(WalletSendConfirmationDialog) ? (
          <WalletSendConfirmationDialogContainer
            amount={amountFieldProps.value}
            receiver={receiverFieldProps.value}
            totalAmount={total}
            transactionFee={fees}
            amountToNaturalUnits={formattedAmountToNaturalUnits}
            currencyUnit={currencyUnit}
          />
        ) : null}

      </div>
    );
  }

  _resetTransactionFee() {
    if (this._isMounted) {
      this.setState({
        isTransactionFeeCalculated: false,
        transactionFee: new BigNumber(0),
        transactionFeeError: null,
      });
    }
  }

  async _calculateTransactionFee(receiver: string, amountValue: string) {
    const amount = formattedAmountToNaturalUnits(amountValue);
    try {
      const fee = await this.props.calculateTransactionFee(receiver, amount);
      if (this._isMounted) {
        this._isCalculatingFee = false;
        this.setState({
          isTransactionFeeCalculated: true,
          transactionFee: fee,
          transactionFeeError: null,
        });
      }
    } catch (error) {
      if (this._isMounted) {
        this._isCalculatingFee = false;
        this.setState({
          isTransactionFeeCalculated: false,
          transactionFee: new BigNumber(0),
          transactionFeeError: this.context.intl.formatMessage(error)
        });
      }
    }
  }
}
