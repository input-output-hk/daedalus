// @flow
import React, { Component } from 'react';
import TextField from 'material-ui/TextField';
import RaisedButton from 'material-ui/RaisedButton';
import styles from './WalletSendForm.scss';

export default class WalletSendForm extends Component {
  render() {
    return (
      <div>

        <TextField
          className={styles.textField}
          hintText="Bitcoin address"
          floatingLabelText="Receiver"
          floatingLabelFixed
          fullWidth
        />

        <TextField
          className={styles.textField}
          hintText="Amount in $"
          floatingLabelText="Amount"
          floatingLabelFixed
          fullWidth
        />

        <TextField
          className={styles.textField}
          hintText="You can add message if you want"
          floatingLabelText="Description"
          floatingLabelFixed
          multiLine
          fullWidth
        />

        <RaisedButton label="Send" primary fullWidth />

      </div>
    );
  }
}
