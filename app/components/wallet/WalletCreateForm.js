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
];

@observer
export default class WalletCreateForm extends Component {

  static propTypes = {
    validator: PropTypes.instanceOf(MobxReactForm),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state: {
    isSubmitting: boolean
  };

  state = {
    isSubmitting: false
  };

  actions = [
    {
      label: 'Create personal wallet',
      onClick: () => {
        this.props.validator.submit({
          onSuccess: (form) => {
            this.setState({ isSubmitting: true });
            form.onSuccess(form);
          },
          onError: (form) => {
            this.setState({ isSubmitting: false });
            form.onError(form);
          }
        });
      }
    }
  ];

  render() {
    const { intl } = this.context;
    const { validator } = this.props;
    const walletName = validator.$('walletName');
    const currency = validator.$('currency');
    return (
      <div className={styles.component}>

        <Dialog
          className={this.state.isSubmitting ? styles.isSubmitting : null}
          title="Create Wallet"
          actions={this.actions}
          active
        >

          <Input
            type="text"
            label={intl.formatMessage(messages.walletName)}
            hint="e.g: Shopping Wallet"
            value={walletName.value}
            error={walletName.error}
            onChange={walletName.onChange}
            onFocus={walletName.onFocus}
            onBlur={walletName.onBlur}
          />

          <Dropdown
            label={intl.formatMessage(messages.currency)}
            value={currency.value}
            onChange={currency.onChange}
            onFocus={currency.onFocus}
            onBlur={currency.onBlur}
            error={currency.error}
            source={currencies}
          />

        </Dialog>
      </div>
    );
  }

}
