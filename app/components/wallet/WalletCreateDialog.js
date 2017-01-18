// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import Dropup from '../widgets/forms/Dropup';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../widgets/DialogCloseButton';
import styles from './WalletCreateDialog.scss';

const messages = defineMessages({
  walletName: {
    id: 'wallet.create.dialog.name.label',
    defaultMessage: '!!!Wallet Name',
    description: 'Label for the "Wallet Name" text input in the wallet create form.'
  },
  currency: {
    id: 'wallet.create.dialog.currency.label',
    defaultMessage: '!!!Currency',
    description: 'Label for the "Currency" dropdown in the wallet create form.'
  },
  invalidWalletName: {
    id: 'wallet.create.dialog.errors.invalidWalletName',
    defaultMessage: '!!!The wallet name must have at least 3 letters.',
    description: 'Error message shown when invalid wallet name was entered in create wallet dialog.'
  },
  invalidCurrency: {
    id: 'wallet.create.dialog.errors.invalidCurrency',
    defaultMessage: '!!!This currency is not yet supported.',
    description: 'Error message shown when invalid currency was selected in create wallet dialog.'
  },
  createPersonalWallet: {
    id: 'wallet.create.dialog.create.personal.wallet.button.label',
    defaultMessage: '!!!Create personal wallet',
    description: 'Label for the "Create personal wallet" button on create wallet dialog.'
  }
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
export default class WalletCreateDialog extends Component {

  static propTypes = {
    onSubmit: PropTypes.func.isRequired,
    onCancel: PropTypes.func.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isSubmitting: false
  };

  componentDidMount() {
    this.walletNameInput.getWrappedInstance().focus();
  }

  walletNameInput: Input;
  validator = new MobxReactForm({ options, fields }, {});
  actions = [
    {
      label: this.context.intl.formatMessage(messages.createPersonalWallet),
      primary: true,
      onClick: () => this.submit()
    }
  ];

  submit = () => {
    this.validator.submit({
      onSuccess: (form) => {
        this.setState({ isSubmitting: true });
        this.props.onSubmit(form.values());
      },
      onError: () => {
        this.setState({ isSubmitting: false });
      }
    });
  };

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
    const dialogClasses = classnames([
      'WalletCreateDialog',
      this.state.isSubmitting ? styles.isSubmitting : null
    ]);
    return (
      <Dialog
        className={dialogClasses}
        title="Create Wallet"
        actions={this.actions}
        onOverlayClick={this.props.onCancel}
        active
      >

        <Input
          type="text"
          className="walletName"
          label={intl.formatMessage(messages.walletName)}
          hint="e.g: Shopping Wallet"
          value={walletName.value}
          error={errors.walletName}
          onChange={walletName.onChange}
          onFocus={walletName.onFocus}
          onBlur={walletName.onBlur}
          onKeyPress={this.checkForEnterKey.bind(this)}
          ref={(input) => { this.walletNameInput = input; }}
        />

        <Dropup
          className="currency"
          label={intl.formatMessage(messages.currency)}
          value={currency.value}
          onChange={currency.onChange}
          onFocus={currency.onFocus}
          onBlur={currency.onBlur}
          error={errors.currency}
          source={currencies}
        />

        <DialogCloseButton onClose={this.props.onCancel} />

      </Dialog>
    );
  }

}
