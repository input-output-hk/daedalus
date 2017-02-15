// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Input from 'react-toolbox/lib/input/Input';
import Button from 'react-toolbox/lib/button/Button';
import { defineMessages, intlShape } from 'react-intl';
import isInt from 'validator/lib/isInt';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
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
  form = new ReactToolboxMobxForm({
    fields: {
      // title: {
      //   label: this.context.intl.formatMessage(messages.titleLabel),
      //   placeholder: this.context.intl.formatMessage(messages.titleHint),
      //   value: '',
      //   validate: ({ field }) => {
      //     const isValid = field.value.length >= 3;
      //     return [isValid, this.context.intl.formatMessage(messages.invalidTitle)];
      //   },
      //   bindings: 'ReactToolbox',
      // },
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
          });
          return [isValid, this.context.intl.formatMessage(messages.invalidAmount)];
        },
        bindings: 'ReactToolbox',
      },
      currency: {
        value: 'ada' // TODO: Remove hardcoded currency
      },
      // description: {
      //   label: this.context.intl.formatMessage(messages.descriptionLabel),
      //   placeholder: this.context.intl.formatMessage(messages.descriptionHint),
      //   value: '',
      //   bindings: 'ReactToolbox',
      // },
    },
  }, {
    options: {
      validateOnChange: false,
    },
  });

  submit() {
    this.form.submit({
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
    const { form } = this;
    const { intl } = this.context;
    return (
      <div className={styles.component}>

        <div className={styles.fields}>
          {/*<Input className="title" {...form.$('title').bind()} />*/}
          <Input className="receiver" {...form.$('receiver').bind()} />
          <Input className="amount" {...form.$('amount').bind()} />
          {/*<Input className="description" multiline {...form.$('description').bind()} />*/}
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

