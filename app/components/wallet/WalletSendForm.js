// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Input from 'react-toolbox/lib/input/Input';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import FullWidthButton from '../widgets/FullWidthButton';
import styles from './WalletSendForm.scss';

const messages = defineMessages({
  receiverLabel: {
    id: 'wallet.send.form.receiver.label',
    defaultMessage: '!!!Receiver',
    description: 'Label for the "Receiver" text input in the wallet send form.'
  },
  receiverHint: {
    id: 'wallet.send.form.receiver.hint',
    defaultMessage: '!!!Bitcoin address',
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
  }
});

@observer
export default class WalletSendForm extends Component {
  render() {
    const { validator } = this.props;
    const { intl } = this.context;
    const receiver = validator.$('receiver');
    const amount = validator.$('amount');
    const description = validator.$('description');
    return (
      <div className={styles.component}>

        <div className={styles.fields}>

          <Input
            label={intl.formatMessage(messages.receiverLabel)}
            hint={intl.formatMessage(messages.receiverHint)}
            value={receiver.value}
            error={receiver.error ? intl.formatMessage(receiver.error) : null}
            onChange={receiver.onChange}
            onFocus={receiver.onFocus}
            onBlur={receiver.onBlur}
          />

          <Input
            label={intl.formatMessage(messages.amountLabel)}
            hint={intl.formatMessage(messages.amountHint)}
            value={validator.$('amount').value}
            error={amount.error ? intl.formatMessage(amount.error) : null}
            onChange={amount.onChange}
            onFocus={amount.onFocus}
            onBlur={amount.onBlur}
          />

          <Input
            label={intl.formatMessage(messages.descriptionLabel)}
            hint={intl.formatMessage(messages.descriptionHint)}
            value={description.value}
            onChange={description.onChange}
            onFocus={description.onFocus}
            onBlur={description.onBlur}
            multiline
          />

        </div>

        <FullWidthButton
          label={intl.formatMessage(messages.sendButtonLabel)}
          onClick={validator.onSubmit}
        />

      </div>
    );
  }
}

WalletSendForm.propTypes = {
  validator: PropTypes.instanceOf(MobxReactForm),
};

WalletSendForm.contextTypes = {
  intl: intlShape.isRequired,
};
