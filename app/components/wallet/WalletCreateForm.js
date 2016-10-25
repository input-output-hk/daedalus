// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import TextField from 'material-ui/TextField';
import Paper from 'material-ui/Paper';
import SelectField from 'material-ui/SelectField';
import MenuItem from 'material-ui/MenuItem';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletCreateForm.scss';

const messages = defineMessages({
  walletName: {
    id: 'wallet.create.form.name.label',
    defaultMessage: '!!!Wallet Name',
    description: 'Label for the "Wallet Name" text input in the wallet create form.'
  },
});

@observer
export default class WalletCreate extends Component {

  static propTypes = {
    validator: PropTypes.instanceOf(MobxReactForm),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { validator } = this.props;
    const errors = {
      walletName: validator.$('walletName').error,
    };
    return (
      <div className={styles.component}>
        <Paper className={styles.dialog} zDepth={3} rounded>

          <div className={styles.header}>
            Create Wallet
          </div>

          <div className={styles.form}>

            <TextField
              className={styles.textField}
              floatingLabelText={intl.formatMessage(messages.walletName)}
              value={validator.$('walletName').value}
              errorText={errors.walletName ? intl.formatMessage(errors.walletName) : null}
              onChange={validator.$('walletName').onChange}
              onFocus={validator.$('walletName').onFocus}
              onBlur={validator.$('walletName').onBlur}
              floatingLabelFixed
              fullWidth
            />

            <div>
              <SelectField
                floatingLabelText="Currency"
                value={validator.$('currency').value || 'ada'}
                onChange={validator.$('currency').onChange}
                floatingLabelFixed
              >
                <MenuItem value="ada" primaryText="ADA" />
                <MenuItem value="btc" primaryText="BTC" />
                <MenuItem value="eth" primaryText="ETH" />
                <MenuItem value="etc" primaryText="ETC" />
              </SelectField>
            </div>

          </div>
        </Paper>
      </div>
    );
  }

}
