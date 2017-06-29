// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Switch from '../widgets/Switch';
import { isValidWalletName, isValidWalletPassword, isValidRepeatPassword } from '../../lib/validations';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletRestoreDialog.scss';

const messages = defineMessages({
  title: {
    id: 'wallet.restore.dialog.title.label',
    defaultMessage: '!!!Restore wallet',
    description: 'Label "Restore wallet" on the wallet restore dialog.'
  },
  walletNameInputLabel: {
    id: 'wallet.restore.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description: 'Label for the wallet name input on the wallet restore dialog.'
  },
  walletNameInputHint: {
    id: 'wallet.restore.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Enter wallet name',
    description: 'Hint "Enter wallet name" for the wallet name input on the wallet restore dialog.'
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.input.label',
    defaultMessage: '!!!Recovery phrase',
    description: 'Label for the recovery phrase input on the wallet restore dialog.'
  },
  recoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.recovery.phrase.input.hint',
    defaultMessage: '!!!Enter recovery phrase',
    description: 'Hint "Enter recovery phrase" for the recovery phrase input on the wallet restore dialog.'
  },
  importButtonLabel: {
    id: 'wallet.restore.dialog.restore.wallet.button.label',
    defaultMessage: '!!!Restore wallet',
    description: 'Label for the "Restore wallet" button on the wallet restore dialog.'
  },
  invalidRecoveryPhrase: {
    id: 'wallet.restore.dialog.form.errors.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description: 'Error message shown when invalid recovery phrase was entered.'
  },
  passwordSwitchLabel: {
    id: 'wallet.restore.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Password',
    description: 'Label for the "Activate to create password" switch in the wallet restore dialog.',
  },
  passwordSwitchPlaceholder: {
    id: 'wallet.restore.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Activate to create password',
    description: 'Text for the "Activate to create password" switch in the wallet restore dialog.',
  },
  walletPasswordLabel: {
    id: 'wallet.restore.dialog.walletPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description: 'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.restore.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description: 'Label for the "Repeat password" input in the wallet restore dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.restore.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the wallet restore dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class WalletRestoreDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  props: {
    onSubmit: Function,
    onCancel: Function,
    isSubmitting: boolean,
    mnemonicValidator: Function,
    error?: ?LocalizableError,
  };

  state = {
    createPassword: false,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      walletName: {
        label: this.context.intl.formatMessage(messages.walletNameInputLabel),
        placeholder: this.context.intl.formatMessage(messages.walletNameInputHint),
        value: '',
        validators: [({ field }) => (
          [
            isValidWalletName(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletName)
          ]
        )],
        bindings: 'ReactToolbox',
      },
      recoveryPhrase: {
        label: this.context.intl.formatMessage(messages.recoveryPhraseInputLabel),
        placeholder: this.context.intl.formatMessage(messages.recoveryPhraseInputHint),
        value: '',
        validators: ({ field }) => {
          const value = field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.mnemonicValidator(field.value),
            this.context.intl.formatMessage(messages.invalidRecoveryPhrase)
          ];
        },
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
        validators: [({ field, form }) => {
          if (!this.state.createPassword) return [true];
          const walletPassword = form.$('walletPassword').value;
          if (walletPassword.length === 0) return [true];
          return [
            isValidRepeatPassword(walletPassword, field.value),
            this.context.intl.formatMessage(globalMessages.invalidRepeatPassword)
          ];
        }],
        bindings: 'ReactToolbox',
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  actions = [
    {
      label: this.context.intl.formatMessage(messages.importButtonLabel),
      primary: true,
      onClick: () => this.submit(),
    },
  ];

  handlePasswordSwitchToggle = (value: boolean) => {
    this.setState({ createPassword: value });
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { createPassword } = this.state;
        const { recoveryPhrase, walletName, walletPassword } = form.values();
        const walletData = {
          recoveryPhrase,
          walletName,
          walletPassword: createPassword ? walletPassword : null,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {}
    });
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { isSubmitting, error, onCancel } = this.props;
    const { createPassword } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'WalletRestoreDialog',
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      createPassword ? styles.show : null,
    ]);

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.title)}
        actions={this.actions}
        onOverlayClick={onCancel}
        active
      >
        <Input className="walletName" {...form.$('walletName').bind()} />

        <Input
          className="recoveryPhrase"
          multiline
          rows={3}
          {...form.$('recoveryPhrase').bind()}
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
            <Input
              className="walletPassword"
              {...form.$('walletPassword').bind()}
            />
            <Input
              className="repeatedPassword"
              {...form.$('repeatPassword').bind()}
            />
            <p className={styles.passwordInstructions}>
              {intl.formatMessage(globalMessages.passwordInstructions)}
            </p>
          </div>
        </div>

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        <DialogCloseButton onClose={onCancel} />

      </Dialog>
    );
  }

}
