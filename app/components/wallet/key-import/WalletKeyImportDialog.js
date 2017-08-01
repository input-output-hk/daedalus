// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/SwitchSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
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
  passwordSwitchPlaceholder: {
    id: 'wallet.key.import.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Activate to create password',
    description: 'Text for the "Activate to create password" switch in the wallet key import dialog.',
  },
  passwordSwitchLabel: {
    id: 'wallet.key.import.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Password',
    description: 'Label for the "Activate to create password" switch in the wallet key import dialog.',
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
      },
      walletPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.walletPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (!this.state.createPassword) return [true];
          const repeatPasswordField = form.$('repeatPassword');
          if (repeatPasswordField.value.length > 0) repeatPasswordField.validate(form);
          return [
            isValidWalletPassword(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
          ];
        }],
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
    ]);

    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      createPassword ? styles.show : null,
    ]);

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: intl.formatMessage(messages.submitLabel),
        primary: true,
        disabled: !(keyFile.value instanceof File),
        onClick: this.submit,
      }
    ];

    const walletPasswordField = form.$('walletPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onClose : null}
        closeButton={<DialogCloseButton />}
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
            <div className={styles.passwordLabel}>
              {intl.formatMessage(messages.passwordSwitchLabel)}
            </div>
            <Checkbox
              onChange={this.handlePasswordSwitchToggle}
              label={intl.formatMessage(messages.passwordSwitchPlaceholder)}
              checked={createPassword}
              skin={<SimpleSwitchSkin />}
            />
          </div>

          <div className={walletPasswordFieldsClasses}>
            <Input
              className="walletPassword"
              {...walletPasswordField.bind()}
              error={walletPasswordField.error}
              skin={<SimpleInputSkin />}
            />
            <Input
              className="repeatedPassword"
              {...repeatedPasswordField.bind()}
              error={repeatedPasswordField.error}
              skin={<SimpleInputSkin />}
            />
            <p className={styles.passwordInstructions}>
              {intl.formatMessage(globalMessages.passwordInstructions)}
            </p>
          </div>
        </div>

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

      </Dialog>
    );
  }

}
