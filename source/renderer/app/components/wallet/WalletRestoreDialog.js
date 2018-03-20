// @flow
import React, { Component } from 'react';
import { join } from 'lodash';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import Autocomplete from 'react-polymorph/lib/components/Autocomplete';
import SimpleAutocompleteSkin from 'react-polymorph/lib/skins/simple/raw/AutocompleteSkin';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/raw/SwitchSkin';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import { isValidWalletName, isValidWalletPassword, isValidRepeatPassword } from '../../utils/validations';
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
  recoveryPhraseNoResults: {
    id: 'wallet.restore.dialog.recovery.phrase.input.noResults',
    defaultMessage: '!!!No results',
    description: '"No results" message for the recovery phrase input search results.'
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
  passwordSwitchPlaceholder: {
    id: 'wallet.restore.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Activate to create password',
    description: 'Text for the "Activate to create password" switch in the wallet restore dialog.',
  },
  passwordSwitchLabel: {
    id: 'wallet.restore.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Password',
    description: 'Label for the "Activate to create password" switch in the wallet restore dialog.',
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
  recoveryPhraseTabTitle: {
    id: 'wallet.restore.dialog.tab.title.recoveryPhrase',
    defaultMessage: '!!!Backup recovery phrase',
    description: 'Tab title "Backup recovery phrase" in the wallet restore dialog.',
  },
  certificateTabTitle: {
    id: 'wallet.restore.dialog.tab.title.certificate',
    defaultMessage: '!!!Paper wallet certificate',
    description: 'Tab title "Paper wallet certificate" in the wallet restore dialog.',
  },
  shieldedRecoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.label',
    defaultMessage: '!!!Shielded recovery phrase',
    description: 'Label for the shielded recovery phrase input on the wallet restore dialog.'
  },
  shieldedRecoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.hint',
    defaultMessage: '!!!Enter shielded recovery phrase',
    description: 'Hint "Enter shielded recovery phrase" for the recovery phrase input on the wallet restore dialog.'
  },
  certificatePasswordLabel: {
    id: 'wallet.restore.dialog.wallet.certificate.password.label',
    defaultMessage: '!!!Paper wallet certificate password',
    description: 'Label for the paper wallet certificate password on the wallet restore dialog.'
  },
  certificatePasswordHint: {
    id: 'wallet.restore.dialog.wallet.certificate.password.hint',
    defaultMessage: '!!!Enter paper wallet certificate password',
    description: 'Hint for the paper wallet certificate password field on the wallet restore dialog.'
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onSubmit: Function,
  onCancel: Function,
  isSubmitting: boolean,
  mnemonicValidator: Function,
  certificateMnemonicValidator: Function,
  error?: ?LocalizableError,
  suggestedMnemonics: Array<string>,
};

type State = {
  createPassword: boolean,
  activeChoice: string,
};

@observer
export default class WalletRestoreDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  state = {
    createPassword: false,
    activeChoice: 'regular', // regular | certificate
  };

  recoveryPhraseAutocomplete: Autocomplete;

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
      },
      recoveryPhrase: {
        value: '',
        validators: ({ field }) => {
          const value = join(field.value, ' ');
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.state.activeChoice === 'regular'
              ? this.props.mnemonicValidator(value)
              : this.props.certificateMnemonicValidator(value),
            this.context.intl.formatMessage(messages.invalidRecoveryPhrase)
          ];
        },
      },
      certificatePassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.certificatePasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.certificatePasswordHint),
        value: '',
        validators: ({ field }) => {
          if (this.state.activeChoice === 'certificate') {
            if (field.value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
            return [
              isValidWalletPassword(field.value),
              this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
            ];
          }
          return [true];
        }
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
        const {
          recoveryPhrase,
          walletName,
          walletPassword,
          certificatePassword,
        } = form.values();

        const walletData = {
          recoveryPhrase: join(recoveryPhrase, ' '),
          walletName,
          walletPassword: createPassword ? walletPassword : null,
          type: this.state.activeChoice,
          certificatePassword,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {}
    });
  };

  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each((field) => { field.debouncedValidation.cancel(); });
    form.reset();
    form.showErrors(false);

    // Autocomplete has to be reset manually
    this.recoveryPhraseAutocomplete.clear();
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      suggestedMnemonics,
      showCertificateRestore,
      isSubmitting,
      error,
      onCancel,
    } = this.props;
    const { createPassword, activeChoice } = this.state;

    const dialogClasses = classnames([
      styles.component,
      showCertificateRestore ? styles.dialogWithCertificateRestore : null,
      'WalletRestoreDialog',
    ]);

    const walletNameFieldClasses = classnames([
      'walletName',
      styles.walletName,
    ]);

    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      createPassword ? styles.show : null,
    ]);

    const certificatePasswordFieldClasses = classnames([
      'certificatePassword',
      styles.certificatePassword,
    ]);

    const walletNameField = form.$('walletName');
    const certificatePasswordField = form.$('certificatePassword');
    const recoveryPhraseField = form.$('recoveryPhrase');
    const walletPasswordField = form.$('walletPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: this.context.intl.formatMessage(messages.importButtonLabel),
        primary: true,
        disabled: isSubmitting,
        onClick: this.submit,
      },
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancel}
        closeButton={<DialogCloseButton />}
      >
        {showCertificateRestore &&
          <div className={styles.restoreTypeChoice}>
            <button
              className={activeChoice === 'regular' ? styles.activeButton : ''}
              onClick={this.onSelectChoice.bind(this, 'regular')}
            >
              {intl.formatMessage(messages.recoveryPhraseTabTitle)}
            </button>
            <button
              className={activeChoice === 'certificate' ? styles.activeButton : ''}
              onClick={this.onSelectChoice.bind(this, 'certificate')}
            >
              {intl.formatMessage(messages.certificateTabTitle)}
            </button>
          </div>
        }

        <Input
          className={walletNameFieldClasses}
          {...walletNameField.bind()}
          error={walletNameField.error}
          skin={<SimpleInputSkin />}
        />

        <Autocomplete
          {...recoveryPhraseField.bind()}
          ref={(autocomplete) => { this.recoveryPhraseAutocomplete = autocomplete; }}
          label={activeChoice === 'regular' ? intl.formatMessage(messages.recoveryPhraseInputLabel) : intl.formatMessage(messages.shieldedRecoveryPhraseInputLabel)}
          placeholder={(activeChoice === 'regular') ? intl.formatMessage(messages.recoveryPhraseInputHint) : intl.formatMessage(messages.shieldedRecoveryPhraseInputHint)}
          options={suggestedMnemonics}
          maxSelections={activeChoice === 'certificate' ? 15 : 12}
          error={recoveryPhraseField.error}
          maxVisibleOptions={5}
          noResultsMessage={intl.formatMessage(messages.recoveryPhraseNoResults)}
          skin={<SimpleAutocompleteSkin />}
        />

        {(activeChoice === 'certificate') &&
          <Input
            className={certificatePasswordFieldClasses}
            {...certificatePasswordField.bind()}
            error={certificatePasswordField.error}
            skin={<SimpleInputSkin />}
          />
        }

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

  onSelectChoice = (choice: string) => {
    this.setState({
      activeChoice: choice,
      createPassword: false,
    });
    this.resetForm();
  };
}
