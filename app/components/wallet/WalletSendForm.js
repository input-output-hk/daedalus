// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import TextField from 'material-ui/TextField';
import RaisedButton from 'material-ui/RaisedButton';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
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
    return (
      <div className={styles.component}>

        <div className={styles.fields}>

          <TextField
            className={styles.textField}
            floatingLabelText={intl.formatMessage(messages.receiverLabel)}
            hintText={intl.formatMessage(messages.receiverHint)}
            value={validator.$('receiver').value}
            errorText={validator.$('receiver').error}
            onChange={validator.$('receiver').onChange}
            onFocus={validator.$('receiver').onFocus}
            onBlur={validator.$('receiver').onBlur}
            floatingLabelFixed
            fullWidth
          />

          <TextField
            className={styles.textField}
            floatingLabelText={intl.formatMessage(messages.amountLabel)}
            hintText={intl.formatMessage(messages.amountHint)}
            value={validator.$('amount').value}
            errorText={validator.$('amount').error}
            onChange={validator.$('amount').onChange}
            onFocus={validator.$('amount').onFocus}
            onBlur={validator.$('amount').onBlur}
            floatingLabelFixed
            fullWidth
          />

          <TextField
            className={styles.textField}
            floatingLabelText={intl.formatMessage(messages.descriptionLabel)}
            hintText={intl.formatMessage(messages.descriptionHint)}
            value={validator.$('description').value}
            errorText={validator.$('description').error}
            onChange={validator.$('description').onChange}
            onFocus={validator.$('description').onFocus}
            onBlur={validator.$('description').onBlur}
            floatingLabelFixed
            multiLine
            fullWidth
          />

        </div>

        <RaisedButton
          className={styles.sendButton}
          label={intl.formatMessage(messages.sendButtonLabel)}
          onClick={validator.onSubmit}
          primary
          fullWidth
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
