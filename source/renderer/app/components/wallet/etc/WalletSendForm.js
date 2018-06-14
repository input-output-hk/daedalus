// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/raw/ButtonSkin';
import Input from 'react-polymorph/lib/components/Input';
import NumericInput from 'react-polymorph/lib/components/NumericInput';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import AmountInputSkin from '../skins/etc/AmountInputSkin';
import BorderedBox from '../../widgets/BorderedBox';
import styles from '../WalletSendForm.scss';
import WalletSendConfirmationDialog from './WalletSendConfirmationDialog';
import WalletSendConfirmationDialogContainer from '../../../containers/wallet/dialogs/WalletSendConfirmationDialogContainer';
import { formattedAmountToBigNumber, formattedAmountToNaturalUnits } from '../../../utils/formatters';
import { messages } from '../WalletSendForm';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

type Props = {
  currencyUnit: string,
  currencyMaxIntegerDigits?: number,
  currencyMaxFractionalDigits: number,
  validateAmount: (amountInNaturalUnits: string) => Promise<boolean>,
  calculateTransactionFee: (receiver: string, amount: string) => Promise<BigNumber>,
  addressValidator: Function,
  openDialogAction: Function,
  isDialogOpen: Function,
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
        validators: [({ field, form }) => {
          const value = field.value;
          if (value === '') {
            this._resetTransactionFee();
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          return this.props.addressValidator(value)
            .then(isValid => {
              const amountField = form.$('amount');
              const amountValue = amountField.value;
              const isAmountValid = amountField.isValid;
              if (isValid && isAmountValid) {
                this._calculateTransactionFee(value, amountValue);
              } else {
                this._resetTransactionFee();
              }
              return [isValid, this.context.intl.formatMessage(messages.invalidAddress)];
            });
        }],
      },
      amount: {
        label: this.context.intl.formatMessage(messages.amountLabel),
        placeholder: `0.${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
        value: '',
        validators: [({ field, form }) => {
          const amountValue = field.value;
          if (amountValue === '') {
            this._resetTransactionFee();
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          const isValid = this.props.validateAmount(formattedAmountToNaturalUnits(amountValue));
          const receiverField = form.$('receiver');
          const receiverValue = receiverField.value;
          const isReceiverValid = receiverField.isValid;
          if (isValid && isReceiverValid) {
            this._calculateTransactionFee(receiverValue, amountValue);
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
      openDialogAction, isDialogOpen
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

        <BorderedBox>

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
            // Form can't be submitted in case transaction fees are not calculated
            disabled={this._isCalculatingFee || !isTransactionFeeCalculated}
            skin={<SimpleButtonSkin />}
          />

        </BorderedBox>

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
