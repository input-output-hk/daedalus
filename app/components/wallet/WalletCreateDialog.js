// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import Dropup from '../widgets/forms/Dropup';
import DialogCloseButton from '../widgets/DialogCloseButton';
import { isValidWalletName, isValidCurrency } from '../../lib/validations';
import globalMessages from '../../i18n/global-messages';
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

  validator = new MobxReactForm({
    options: {
      validateOnChange: false
    },
    fields: {
      walletName: {
        value: '',
        validate: [({ field }) => (
          [
            isValidWalletName(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletName)
          ]
        )]
      },
      currency: {
        value: 'ada',
        validate: [({ field }) => (
          [
            isValidCurrency(field.value),
            this.context.intl.formatMessage(messages.invalidCurrency)
          ]
        )]
      },
    }
  });

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
    const dialogClasses = classnames([
      'WalletCreateDialog',
      this.state.isSubmitting ? styles.isSubmitting : null
    ]);
    return (
      <Dialog
        className={dialogClasses}
        title="Create Wallet" // TODO: Missing translation
        actions={this.actions}
        onOverlayClick={this.props.onCancel}
        active
      >

        <Input
          type="text"
          className="walletName"
          label={intl.formatMessage(messages.walletName)}
          hint="e.g: Shopping Wallet" // TODO: Missing translation
          value={walletName.value}
          error={walletName.error}
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
          error={currency.error}
          source={currencies}
        />

        <DialogCloseButton onClose={this.props.onCancel} />

      </Dialog>
    );
  }

}
