// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletCreateForm.scss';

const messages = defineMessages({
  walletName: {
    id: 'wallet.create.form.name.label',
    defaultMessage: '!!!Wallet Name',
    description: 'Label for the "Wallet Name" text input in the wallet create form.'
  },
  currency: {
    id: 'wallet.create.form.currency.label',
    defaultMessage: '!!!Currency',
    description: 'Label for the "Currency" dropdown in the wallet create form.'
  },
});

const currencies = [
  { value: 'ada', label: 'ADA' },
  { value: 'btc', label: 'BTC' },
  { value: 'eth', label: 'ETH' },
];

@observer
export default class WalletCreateForm extends Component {

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

        <Dialog
          title="Create Wallet"
          actions={[{ label: 'Create personal wallet', onClick: () => {} }]}
          active
        >

          <Input
            type="text"
            label={intl.formatMessage(messages.walletName)}
            hint="e.g: Shopping Wallet"
            value={validator.$('walletName').value}
            error={errors.walletName ? intl.formatMessage(errors.walletName) : null}
            onChange={validator.$('walletName').onChange}
            onFocus={validator.$('walletName').onFocus}
            onBlur={validator.$('walletName').onBlur}
          />

          <Dropdown
            label={intl.formatMessage(messages.currency)}
            value={validator.$('currency').value || 'ada'}
            onChange={validator.$('currency').onChange}
            source={currencies}
          />

        </Dialog>
      </div>
    );
  }

}
