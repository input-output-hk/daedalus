// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/SwitchSkin';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import TextArea from 'react-polymorph/lib/components/TextArea';
import SimpleTextAreaSkin from 'react-polymorph/lib/skins/simple/TextAreaSkin';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import { isValidWalletName, isValidWalletPassword, isValidRepeatPassword } from '../../utils/validations';
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

type Props = {
  onSubmit: Function,
  onCancel: Function,
  isSubmitting: boolean,
  mnemonicValidator: Function,
  privateKeyValidator: Function,
  error?: ?LocalizableError,
};

type State = {
  createPassword: boolean,
};

@observer
export default class PaperWalletImportDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired
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
    ]);

    const mnemonicPhraseFieldClasses = classnames([
      'mnemonicPhrase',
      styles.mnemonicPhrase,
    ]);

    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      createPassword ? styles.show : null,
    ]);

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: this.context.intl.formatMessage(messages.importButtonLabel),
        primary: true,
        onClick: this.submit,
      },
    ];

    const privateKeyField = form.$('privateKey');
    const walletNameField = form.$('walletName');
    const mnemonicPhraseField = form.$('mnemonicPhrase');
    const walletPasswordField = form.$('walletPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : null}
        closeButton={<DialogCloseButton />}
      >

        <Input
          className="privateKey"
          {...privateKeyField.bind()}
          error={privateKeyField.error}
          skin={<SimpleInputSkin />}
        />

        <TextArea
          className={mnemonicPhraseFieldClasses}
          autoResize={false}
          rows={3}
          {...mnemonicPhraseField.bind()}
          error={mnemonicPhraseField.error}
          skin={<SimpleTextAreaSkin />}
        />

        <Input
          className="walletName"
          {...walletNameField.bind()}
          error={walletNameField.error}
          skin={<SimpleInputSkin />}
        />

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
