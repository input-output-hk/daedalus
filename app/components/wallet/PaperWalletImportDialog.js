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
import styles from './PaperWalletImportDialog.scss';

const messages = defineMessages({
  title: {
    id: 'paper.wallet.import.dialog.title.label',
    defaultMessage: '!!!Paper wallet import',
    description: 'Label "Paper wallet import" on the paper wallet import dialog.'
  },
  privateKeyInputLabel: {
    id: 'paper.wallet.import.dialog.wallet.privateKey.input.label',
    defaultMessage: '!!!Private key',
    description: 'Label for the wallet private key input on the paper wallet import dialog.'
  },
  privateKeyInputHint: {
    id: 'paper.wallet.import.dialog.wallet.privateKey.input.hint',
    defaultMessage: '!!!Enter private key',
    description: 'Hint "Enter private key" for the wallet private key input on the paper wallet import dialog.'
  },
  mnemonicPhraseInputLabel: {
    id: 'paper.wallet.import.dialog.mnemonic.phrase.input.label',
    defaultMessage: '!!!Mnemonic phrase',
    description: 'Label for the mnemonic phrase input on the paper wallet import dialog.'
  },
  mnemonicPhraseInputHint: {
    id: 'paper.wallet.import.dialog.mnemonic.phrase.input.hint',
    defaultMessage: '!!!Enter mnemonic phrase',
    description: 'Hint "Enter mnemonic phrase" for the mnemonic phrase input on the paper wallet import dialog.'
  },
  walletNameInputLabel: {
    id: 'paper.wallet.import.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description: 'Label for the wallet name input on the paper wallet import dialog.'
  },
  walletNameInputHint: {
    id: 'paper.wallet.import.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Enter wallet name',
    description: 'Hint "Enter wallet name" for the wallet name input on the paper wallet import dialog.'
  },
  importButtonLabel: {
    id: 'paper.wallet.import.dialog.import.wallet.button.label',
    defaultMessage: '!!!Import wallet',
    description: 'Label for the "Import wallet" button on the paper wallet import dialog.'
  },
  invalidMnemonicPhrase: {
    id: 'paper.wallet.import.dialog.form.errors.invalidMnemonicPhrase',
    defaultMessage: '!!!Invalid mnemonic phrase',
    description: 'Error message shown when invalid mnemonic phrase was entered.'
  },
  invalidPrivateKey: {
    id: 'paper.wallet.import.dialog.form.errors.invalidPrivateKey',
    defaultMessage: '!!!Invalid private key',
    description: 'Error message shown when invalid private key was entered.'
  },
  passwordSwitchLabel: {
    id: 'paper.wallet.import.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Password',
    description: 'Label for the "Activate to create password" switch in the paper wallet import dialog.',
  },
  passwordSwitchPlaceholder: {
    id: 'paper.wallet.import.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Activate to create password',
    description: 'Text for the "Activate to create password" switch in the paper wallet import dialog.',
  },
  walletPasswordLabel: {
    id: 'paper.wallet.import.dialog.walletPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description: 'Label for the "Wallet password" input in the paper wallet import dialog.',
  },
  repeatPasswordLabel: {
    id: 'paper.wallet.import.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description: 'Label for the "Repeat password" input in the paper wallet import dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'paper.wallet.import.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the paper wallet import dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class PaperWalletImportDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  props: {
    onSubmit: Function,
    onCancel: Function,
    isSubmitting: boolean,
    mnemonicValidator: Function,
    privateKeyValidator: Function,
    error?: ?LocalizableError,
  };

  state = {
    createPassword: false,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      privateKey: {
        label: this.context.intl.formatMessage(messages.privateKeyInputLabel),
        placeholder: this.context.intl.formatMessage(messages.privateKeyInputHint),
        value: '',
        validators: ({ field }) => {
          const value = field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.privateKeyValidator(field.value),
            this.context.intl.formatMessage(messages.invalidPrivateKey)
          ];
        },
        bindings: 'ReactToolbox',
      },
      mnemonicPhrase: {
        label: this.context.intl.formatMessage(messages.mnemonicPhraseInputLabel),
        placeholder: this.context.intl.formatMessage(messages.mnemonicPhraseInputHint),
        value: '',
        validators: ({ field }) => {
          const value = field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.mnemonicValidator(field.value),
            this.context.intl.formatMessage(messages.invalidMnemonicPhrase)
          ];
        },
        bindings: 'ReactToolbox',
      },
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
        const { privateKey, mnemonicPhrase, walletName, walletPassword } = form.values();
        const walletData = {
          privateKey,
          mnemonicPhrase,
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
      'PaperWalletImportDialog',
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

        <Input className="privateKey" {...form.$('privateKey').bind()} />

        <Input
          className="mnemonicPhrase"
          multiline
          rows={3}
          {...form.$('mnemonicPhrase').bind()}
        />

        <Input className="walletName" {...form.$('walletName').bind()} />

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
