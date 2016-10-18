// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import TextField from 'material-ui/TextField';
import RaisedButton from 'material-ui/RaisedButton';
import MobxReactForm from 'mobx-react-form';
import styles from './WalletSendForm.scss';

@observer
export default class WalletSendForm extends Component {
  render() {
    const { validator } = this.props;
    return (
      <div className={styles.component}>

        <div className={styles.fields}>

          <TextField
            className={styles.textField}
            floatingLabelText="Receiver"
            hintText="Bitcoin address"
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
            floatingLabelText="Amount"
            hintText="Amount in $"
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
            floatingLabelText="Description"
            hintText="You can add a message if you want"
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
          label="Send"
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
