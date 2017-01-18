// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Input from 'react-toolbox/lib/input/Input';
import Button from 'react-toolbox/lib/button/Button';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import isInt from 'validator/lib/isInt';
import styles from './WalletSendForm.scss';
import globalMessages from '../../i18n/global-messages';

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
  amountHint: {
    id: 'wallet.send.form.amount.hint',
    defaultMessage: '!!!Amount in $',
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
    defaultMessage: '!!!Please enter a valid amount.',
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
    addressValidator: PropTypes.func.isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isSubmitting: false
  };

  // FORM VALIDATION
  validator = new MobxReactForm({
    options: {
      validateOnChange: false,
    },
    fields: {
      title: {
        value: '',
        validate: ({ field }) => {
          const isValid = field.value.length >= 3;
          return [isValid, 'invalidTitle'];
        }
      },
      receiver: {
        value: '',
        validate: ({ field }) => {
          const value = field.value;
          if (value === '') return [false, 'fieldIsRequired'];
          return this.props.addressValidator(field.value).then(isValid => [isValid, 'invalidAddress']);
        }
      },
      amount: {
        value: '',
        validate: ({ field }) => {
          const isValid = isInt(field.value, {
            allow_leading_zeroes: false,
            min: 1,
          });
          return [isValid, 'invalidAmount'];
        }
      },
      currency: {
        value: 'ada' // TODO: Remove hardcoded currency
      },
      description: {},
    }
  }, {});

  submit() {
    this.validator.submit({
      onSuccess: (form) => {
        this.setState({ isSubmitting: true });
        this.props.onSubmit(form.values());
        form.reset();
      },
      onError: () => {
        this.setState({ isSubmitting: false });
      }
    });
  }

  render() {
    const { validator } = this;
    const { intl } = this.context;
    const title = validator.$('title');
    const receiver = validator.$('receiver');
    const amount = validator.$('amount');
    const description = validator.$('description');
    const errors = {
      title: title.error && messages[title.error] ? intl.formatMessage(messages[title.error]) : null,
      receiver: receiver.error && messages[receiver.error] ? intl.formatMessage(messages[receiver.error]) : null,
      amount: amount.error ? intl.formatMessage(messages[amount.error]) : null,
    };
    return (
      <div className={styles.component}>

        <div className={styles.fields}>

          <Input
            className="title"
            label={intl.formatMessage(messages.titleLabel)}
            hint={intl.formatMessage(messages.titleHint)}
            value={title.value}
            error={errors.title}
            onChange={title.onChange}
            onFocus={title.onFocus}
            onBlur={title.onBlur}
          />

          <Input
            className="receiver"
            label={intl.formatMessage(messages.receiverLabel)}
            hint={intl.formatMessage(messages.receiverHint)}
            value={receiver.value}
            error={errors.receiver}
            onChange={receiver.onChange}
            onFocus={receiver.onFocus}
            onBlur={receiver.onBlur}
          />

          <Input
            className="amount"
            label={intl.formatMessage(messages.amountLabel)}
            hint={intl.formatMessage(messages.amountHint)}
            value={amount.value}
            error={errors.amount}
            onChange={amount.onChange}
            onFocus={amount.onFocus}
            onBlur={amount.onBlur}
          />

          <Input
            className="description"
            label={intl.formatMessage(messages.descriptionLabel)}
            hint={intl.formatMessage(messages.descriptionHint)}
            value={description.value}
            onChange={description.onChange}
            onFocus={description.onFocus}
            onBlur={description.onBlur}
            multiline
          />

        </div>

        <Button
          className={this.state.isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
          label={intl.formatMessage(messages.sendButtonLabel)}
          onMouseUp={this.submit.bind(this)}
          primary
        />

      </div>
    );
  }
}

