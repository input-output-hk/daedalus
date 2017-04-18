// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
import Dropup from '../widgets/forms/Dropup';
import DialogCloseButton from '../widgets/DialogCloseButton';
import { isValidWalletName, isValidCurrency } from '../../lib/validations';
import globalMessages from '../../i18n/global-messages';
import styles from './WalletCreateDialog.scss';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.'
  },
  walletName: {
    id: 'wallet.create.dialog.name.label',
    defaultMessage: '!!!Wallet Name',
    description: 'Label for the "Wallet Name" text input in the wallet create form.'
  },
  walletNameHint: {
    id: 'wallet.create.dialog.walletNameHint',
    defaultMessage: '!!!e.g: Shopping Wallet',
    description: 'Hint for the "Wallet Name" text input in the wallet create form.'
  },
  currencyLabel: {
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

  form = new ReactToolboxMobxForm({
    fields: {
      walletName: {
        label: this.context.intl.formatMessage(messages.walletName),
        placeholder: this.context.intl.formatMessage(messages.walletNameHint),
        value: '',
        validate: [({ field }) => (
          [
            isValidWalletName(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletName)
          ]
        )],
        bindings: 'ReactToolbox',
      },
      currency: {
        label: this.context.intl.formatMessage(messages.currencyLabel),
        value: 'ada',
        validate: [({ field }) => (
          [
            isValidCurrency(field.value),
            this.context.intl.formatMessage(messages.invalidCurrency)
          ]
        )],
        bindings: 'ReactToolbox',
      },
    }
  }, {
    options: {
      validateOnChange: false
    },
  });

  actions = [
    {
      label: this.context.intl.formatMessage(messages.createPersonalWallet),
      primary: true,
      onClick: () => this.submit()
    }
  ];

  submit = () => {
    this.form.submit({
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
    const { form } = this;
    const { intl } = this.context;
    const dialogClasses = classnames([
      styles.component,
      'WalletCreateDialog',
      this.state.isSubmitting ? styles.isSubmitting : null
    ]);
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={this.actions}
        onOverlayClick={this.props.onCancel}
        active
      >

        <Input
          className="walletName"
          onKeyPress={this.checkForEnterKey.bind(this)}
          ref={(input) => { this.walletNameInput = input; }}
          {...form.$('walletName').bind()}
        />

        <Dropup
          className="currency"
          {...form.$('currency').bind()}
          source={currencies}
        />

        <DialogCloseButton onClose={this.props.onCancel} />

      </Dialog>
    );
  }

}
