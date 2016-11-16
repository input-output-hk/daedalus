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
  invalidWalletName: {
    id: 'wallet.create.form.errors.invalidWalletName',
    defaultMessage: '!!!The wallet name must have at least 3 letters.',
    description: 'Error message shown when invalid wallet name was entered in create wallet dialog.'
  },
  invalidCurrency: {
    id: 'wallet.create.form.errors.invalidCurrency',
    defaultMessage: '!!!This currency is not yet supported.',
    description: 'Error message shown when invalid currency was selected in create wallet dialog.'
  },
});

const currencies = [
  { value: 'ada', label: 'ADA' },
];

const isValidWalletName = ({ field }) => {
  const isValid = field.value.length >= 3;
  return [isValid, 'invalidWalletName'];
};

const isValidCurrency = ({ field }) => {
  const isValid = field.value === 'ada';
  return [isValid, 'invalidCurrency'];
};

const fields = {
  walletName: {
    value: '',
    validate: [isValidWalletName]
  },
  currency: {
    value: 'ada',
    validate: [isValidCurrency]
  },
};

const options = {
  validateOnChange: false
};

@observer
export default class WalletCreateForm extends Component {

  static propTypes = {
    onSubmit: PropTypes.func.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isSubmitting: false
  };

  actions = [
    {
      label: 'Create personal wallet',
      onClick: this.submit.bind(this)
    }
  ];

  validator = new MobxReactForm({ options, fields });

  submit() {
    this.validator.submit({
      onSuccess: (form) => {
        this.setState({ isSubmitting: true });
        this.props.onSubmit(form.values());
      },
      onError: () => {
        this.setState({ isSubmitting: false });
      }
    });
  }

  checkForEnterKey(event: KeyboardEvent) {
    if (event.key === 'Enter') {
      this.submit();
    }
  }

  render() {
    const { intl } = this.context;
    const { validator } = this;
    const walletName = validator.$('walletName');
    const currency = validator.$('currency');
    const errors = {
      walletName: walletName.error ? intl.formatMessage(messages[walletName.error]) : null,
      currency: currency.error ? intl.formatMessage(messages[currency.error]) : null,
    };
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
            error={errors.walletName}
            onChange={walletName.onChange}
            onFocus={walletName.onFocus}
            onBlur={walletName.onBlur}
            onKeyPress={this.checkForEnterKey.bind(this)}
          />

          <Dropdown
            label={intl.formatMessage(messages.currency)}
            value={currency.value}
            onChange={currency.onChange}
            onFocus={currency.onFocus}
            onBlur={currency.onBlur}
            error={errors.currency}
            source={currencies}
          />

        </Dialog>
      </div>
    );
  }

}
