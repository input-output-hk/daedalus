// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
import Dropup from '../widgets/forms/Dropup';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Switch from '../widgets/Switch';
import { isValidWalletName, isValidCurrency, isValidWalletPassword, isValidRepeatPassword } from '../../lib/validations';
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
  },
  passwordSwitchLabel: {
    id: 'wallet.create.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Password',
    description: 'Label for the "Activate to create password" switch in the create wallet dialog.',
  },
  passwordSwitchPlaceholder: {
    id: 'wallet.create.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Activate to create password',
    description: 'Text for the "Activate to create password" switch in the create wallet dialog.',
  },
  walletPasswordLabel: {
    id: 'wallet.create.dialog.walletPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description: 'Label for the "Wallet password" input in the create wallet dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.create.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description: 'Label for the "Repeat password" input in the create wallet dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.create.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the create wallet dialog.',
  },
});

const currencies = [
  { value: 'ada', label: 'ADA' },
];

@observer
export default class WalletCreateDialog extends Component {

  props: {
    onSubmit: Function,
    onCancel: Function,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isSubmitting: false,
    createPassword: false,
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
        validators: [({ field }) => (
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
        validators: [({ field }) => (
          [
            isValidCurrency(field.value),
            this.context.intl.formatMessage(messages.invalidCurrency)
          ]
        )],
        bindings: 'ReactToolbox',
      },
      walletPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.walletPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (!this.state.createPassword) return [true];
          return [
            isValidWalletPassword(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
          ];
        }],
        bindings: 'ReactToolbox',
      },
      repeatPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (!this.state.createPassword) return [true];
          const walletPassword = this.form.$('walletPassword').value;
          if (walletPassword.length === 0) return [true];
          return [
            isValidRepeatPassword(walletPassword, field.value),
            this.context.intl.formatMessage(globalMessages.invalidRepeatPassword)
          ];
        }],
        bindings: 'ReactToolbox',
      },
    }
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  actions = [
    {
      label: this.context.intl.formatMessage(messages.createPersonalWallet),
      primary: true,
      onClick: () => this.submit(),
    },
  ];

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        this.setState({ isSubmitting: true });
        const { createPassword } = this.state;
        const { walletName, currency, walletPassword } = form.values();
        const walletData = {
          name: walletName,
          currency,
          password: createPassword ? walletPassword : null,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {
        this.setState({ isSubmitting: false });
      },
    });
  };

  checkForEnterKey(event: KeyboardEvent) {
    if (event.key === 'Enter') {
      this.submit();
    }
  }

  handlePasswordSwitchToggle = (value: boolean) => {
    this.setState({ createPassword: value });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onCancel } = this.props;
    const { createPassword, isSubmitting } = this.state;
    const dialogClasses = classnames([
      styles.component,
      'WalletCreateDialog',
      isSubmitting ? styles.isSubmitting : null
    ]);
    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      createPassword ? styles.show : null
    ]);

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={this.actions}
        onOverlayClick={onCancel}
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

        <div className={styles.walletPassword}>
          <div className={styles.walletPasswordSwitch}>
            <Switch
              label={intl.formatMessage(messages.passwordSwitchLabel)}
              placeholder={intl.formatMessage(messages.passwordSwitchPlaceholder)}
              active={createPassword}
              onChange={this.handlePasswordSwitchToggle}
            />
          </div>

          <div className={walletPasswordFieldsClasses}>
            <Input {...form.$('walletPassword').bind()} />
            <Input {...form.$('repeatPassword').bind()} />
          </div>
        </div>

        <DialogCloseButton onClose={onCancel} />

      </Dialog>
    );
  }

}
