// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Button from 'react-toolbox/lib/button/Button';
import Input from 'react-polymorph/lib/components/Input';
import NumericInput from 'react-polymorph/lib/components/NumericInput';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import { defineMessages, intlShape } from 'react-intl';
import { isValidAmountInLovelaces } from '../../lib/validations';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
import BorderedBox from '../widgets/BorderedBox';
import styles from './WalletSendForm.scss';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';

const messages = defineMessages({
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
  sendButtonLabel: {
    id: 'wallet.send.form.submit',
    defaultMessage: '!!!Send',
    description: 'Label for the send button on the wallet send form.'
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
  walletPasswordLabel: {
    id: 'wallet.send.form.walletPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description: 'Label for the "Wallet password" input in the wallet send form.'
  },
  passwordFieldPlaceholder: {
    id: 'wallet.send.form.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the wallet send form.'
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class WalletSendForm extends Component {

  props: {
    onSubmit: Function,
    isSubmitting: boolean,
    addressValidator: Function,
    isWalletPasswordSet: boolean,
    error?: ?LocalizableError,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  adaToLovelaces = (adaAmount: string) => (
    adaAmount.replace('.', '').replace(/,/g, '').replace(/^0+/, '')
  );

  // FORM VALIDATION
  form = new ReactToolboxMobxForm({
    fields: {
      receiver: {
        label: this.context.intl.formatMessage(messages.receiverLabel),
        placeholder: this.context.intl.formatMessage(messages.receiverHint),
        value: '',
        validators: ({ field }) => {
          const value = field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return this.props.addressValidator(field.value)
            .then(isValid => [isValid, this.context.intl.formatMessage(messages.invalidAddress)]);
        },
      },
      amount: {
        label: this.context.intl.formatMessage(messages.amountLabel),
        placeholder: '0.000000',
        value: '',
        validators: ({ field }) => {
          if (field.value === '') {
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          const amountInLovelaces = this.adaToLovelaces(field.value);
          const isValid = isValidAmountInLovelaces(amountInLovelaces);
          return [isValid, this.context.intl.formatMessage(messages.invalidAmount)];
        },
      },
      walletPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.walletPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (this.props.isWalletPasswordSet && field.value === '') {
            return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          }
          return [true];
        }],
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  submit() {
    this.form.submit({
      onSuccess: (form) => {
        const { isWalletPasswordSet } = this.props;
        const { receiver, amount, walletPassword } = form.values();
        const transactionData = {
          receiver,
          amount: this.adaToLovelaces(amount),
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
    const { isWalletPasswordSet, isSubmitting, error } = this.props;
    const amountField = form.$('amount');
    const receiverField = form.$('receiver');
    const passwordField = form.$('walletPassword');

    return (
      <div className={styles.component}>

        <BorderedBox>

          <div className={styles.receiverInput}>
            <Input
              className="receiver"
              {...receiverField.bind()}
              error={receiverField.error}
              skin={<SimpleInputSkin />}
            />
          </div>

          <div className={styles.amountInput}>
            <NumericInput
              {...amountField.bind()}
              className="amount"
              label={intl.formatMessage(messages.amountLabel)}
              minValue={0.000001}
              maxValue={45000000000}
              maxAfterDot={6}
              maxBeforeDot={11}
              error={amountField.error}
              skin={<SimpleInputSkin />}
            />
            <span className={styles.adaLabel}>
              {intl.formatMessage(globalMessages.unitAda)}
            </span>
          </div>

          {isWalletPasswordSet ? (
            <div className={styles.passwordInput}>
              <Input
                className="walletPassword"
                {...passwordField.bind()}
                error={passwordField.error}
                skin={<SimpleInputSkin />}
              />
            </div>
          ) : null}

          {error ? <p className={styles.error}>{intl.formatMessage(error)}</p> : null}

          <Button
            className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
            label={intl.formatMessage(messages.sendButtonLabel)}
            onMouseUp={this.submit.bind(this)}
            primary
          />
        </BorderedBox>

      </div>
    );
  }
}

