// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Input from 'react-toolbox/lib/input/Input';
import Button from 'react-toolbox/lib/button/Button';
import { defineMessages, intlShape } from 'react-intl';
import isInt from 'validator/lib/isInt';
import BigNumber from 'bignumber.js';
import { LOVELACES_PER_ADA, DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
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
  amountHint: {
    id: 'wallet.send.form.amount.hint',
    defaultMessage: '!!!Amount in Lovelaces',
    description: 'Hint inside the "Amount" number input in the wallet send form.'
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
    defaultMessage: '!!!Please enter a valid amount (positive integer).',
    description: 'Error message shown when invalid amount was entered.',
  },
  invalidTitle: {
    id: 'wallet.send.form.errors.invalidTitle',
    defaultMessage: '!!!Please enter a title with at least 3 characters.',
    description: 'Error message shown when invalid transaction title was entered.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class WalletSendForm extends Component {

  static propTypes = {
    onSubmit: PropTypes.func.isRequired,
    isSubmitting: PropTypes.bool.isRequired,
    addressValidator: PropTypes.func.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  // FORM VALIDATION
  form = new ReactToolboxMobxForm({
    fields: {
      receiver: {
        label: this.context.intl.formatMessage(messages.receiverLabel),
        placeholder: this.context.intl.formatMessage(messages.receiverHint),
        value: '',
        validate: ({ field }) => {
          const value = field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return this.props.addressValidator(field.value)
            .then(isValid => [isValid, this.context.intl.formatMessage(messages.invalidAddress)]);
        },
        bindings: 'ReactToolbox',
      },
      amount: {
        label: this.context.intl.formatMessage(messages.amountLabel),
        placeholder: this.context.intl.formatMessage(messages.amountHint),
        value: '',
        validate: ({ field }) => {
          const isValid = isInt(field.value, {
            allow_leading_zeroes: false,
            min: 1,
            max: 45000000000000000,
          });
          return [isValid, this.context.intl.formatMessage(messages.invalidAmount)];
        },
        bindings: 'ReactToolbox',
      },
      currency: {
        value: 'ada' // TODO: Remove hardcoded currency
      },
    },
  }, {
    options: {
      validateOnChange: true,
    },
  });

  submit() {
    this.form.submit({
      onSuccess: (form) => {
        this.props.onSubmit(form.values());
        form.reset();
      },
      onError: () => {}
    });
  }

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { isSubmitting, error } = this.props;

    let adaAmount = '0';
    if (form.$('amount').value !== '' && form.$('amount').isValid) {
      const lovelaces = new BigNumber(form.$('amount').value);
      adaAmount = lovelaces.dividedBy(LOVELACES_PER_ADA).toFormat(DECIMAL_PLACES_IN_ADA);
    }

    return (
      <div className={styles.component}>

        <BorderedBox>

          <Input className="receiver" {...form.$('receiver').bind()} />

          <div className={styles.amountInput}>

            {!form.$('amount').error ? (
              <span className={styles.adaAmount}>
                {intl.formatMessage(messages.equalsAdaHint, { amount: adaAmount })}
              </span>
            ) : null}

            <Input className="amount" {...form.$('amount').bind()} />
          </div>

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

