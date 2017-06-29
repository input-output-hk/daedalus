// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import Input from 'react-toolbox/lib/input/Input';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import Switch from '../../widgets/Switch';
import { isValidWalletPassword, isValidRepeatPassword } from '../../../lib/validations';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './WalletKeyImportDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.key.import.dialog.headline',
    defaultMessage: '!!!Import Wallet',
    description: 'headline for "Import wallet from file with the key" dialog.'
  },
  keyFileLabel: {
    id: 'wallet.key.import.dialog.keyFileLabel',
    defaultMessage: '!!!Upload your key',
    description: 'Label "Upload your key" on the dialog for importing a wallet from the key.'
  },
  keyFileHint: {
    id: 'wallet.key.import.dialog.keyFileHint',
    defaultMessage: '!!!Drop file here or click to choose',
    description: 'Hint for the key file upload on the dialog for importing a wallet from the key.'
  },
  submitLabel: {
    id: 'wallet.key.import.dialog.submitLabel',
    defaultMessage: '!!!Import wallet',
    description: 'Label "Import you key" submit button on the dialog for importing a wallet from the key.'
  },
  passwordSwitchLabel: {
    id: 'wallet.key.import.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Password',
    description: 'Label for the "Activate to create password" switch in the wallet key import dialog.',
  },
  passwordSwitchPlaceholder: {
    id: 'wallet.key.import.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Activate to create password',
    description: 'Text for the "Activate to create password" switch in the wallet key import dialog.',
  },
  walletPasswordLabel: {
    id: 'wallet.key.import.dialog.walletPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description: 'Label for the "Wallet password" input in the wallet key import dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.key.import.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description: 'Label for the "Repeat password" input in the wallet key import dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.key.import.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the wallet key import dialog.',
  },
});

@observer
export default class WalletKeyImportDialog extends Component {

  props: {
    onSubmit: Function,
    onClose: Function,
    isSubmitting: boolean,
    error: ?LocalizableError,
  };

  state = {
    createPassword: false,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handlePasswordSwitchToggle = (value: boolean) => {
    this.setState({ createPassword: value });
  };


  form = new ReactToolboxMobxForm({
    fields: {
      keyFile: {
        type: 'file',
        label: this.context.intl.formatMessage(messages.keyFileLabel),
        placeholder: this.context.intl.formatMessage(messages.keyFileHint),
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

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { createPassword } = this.state;
        const { keyFile, walletPassword } = form.values();
        const walletData = {
          filePath: keyFile.path,
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
    const { isSubmitting, error, onClose } = this.props;
    const { createPassword } = this.state;

    const keyFile = form.$('keyFile');
    const dialogClasses = classnames([
      styles.component,
      'WalletKeyImportDialog',
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      createPassword ? styles.show : null,
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.submitLabel),
        primary: true,
        disabled: !(keyFile.value instanceof File),
        onClick: () => this.submit()
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        onOverlayClick={onClose}
        active
      >

        <div className={styles.keyUpload}>
          <FileUploadWidget
            {...keyFile.bind()}
            selectedFile={keyFile.value}
            onFileSelected={keyFile.onChange}
          />
        </div>

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

        <DialogCloseButton onClose={onClose} />

      </Dialog>
    );
  }

}
