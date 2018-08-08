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
import { PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT, WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../config/cryptoConfig';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import styles from './WalletRestoreDialog.scss';

const RESTORE_TYPES = {
  REGULAR: 'regular',
  CERTIFICATE: 'certificate'
};

const messages = defineMessages({
  title: {
    id: 'wallet.restore.dialog.title.label',
    defaultMessage: '!!!Restore a wallet',
    description: 'Label "Restore wallet" on the wallet restore dialog.'
  },
  walletNameInputLabel: {
    id: 'wallet.restore.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description: 'Label for the wallet name input on the wallet restore dialog.'
  },
  walletNameInputHint: {
    id: 'wallet.restore.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Name the wallet you are restoring',
    description: 'Hint "Name the wallet you are restoring" for the wallet name input on the wallet restore dialog.'
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
    defaultMessage: '!!!Keep your private keys safely encrypted by setting the spending password',
    description: 'Text for the "Spending password" switch in the wallet restore dialog.',
  },
  passwordSwitchLabel: {
    id: 'wallet.restore.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for the "Spending password" switch in the wallet restore dialog.',
  },
  walletPasswordLabel: {
    id: 'wallet.restore.dialog.walletPasswordLabel',
    defaultMessage: '!!!Enter password',
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
    defaultMessage: '!!!Paper wallet recovery phrase',
    description: 'Label for the shielded recovery phrase input on the wallet restore dialog.'
  },
  shieldedRecoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.hint',
    defaultMessage: '!!!Enter the recovery phrase from your paper wallet certificate',
    description: 'Hint "Enter shielded recovery phrase" for the recovery phrase input on the wallet restore dialog.'
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onSubmit: Function,
  onCancel: Function,
  isSubmitting: boolean,
  mnemonicValidator: Function,
  error?: ?LocalizableError,
  suggestedMnemonics: Array<string>,
  showCertificateRestore: boolean,
  onChoiceChange: ?Function,
};

type State = {
  createPassword: boolean,
  activeChoice: string,
};

@observer
export default class WalletRestoreDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    createPassword: true,
    activeChoice: RESTORE_TYPES.REGULAR, // regular | certificate
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
        value: [],
        validators: ({ field }) => {
          const { intl } = this.context;
          const enteredWords = field.value;
          const wordCount = enteredWords.length;
          const expectedWordCount = (this.isRegular() ?
            WALLET_RECOVERY_PHRASE_WORD_COUNT : PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT
          );
          const value = join(enteredWords, ' ');
          // Regular mnemonics have 12 and paper wallet recovery needs 27 words
          const isPhraseComplete = wordCount === expectedWordCount;
          if (!isPhraseComplete) {
            return [
              false,
              intl.formatMessage(globalMessages.incompleteMnemonic, { expected: expectedWordCount })
            ];
          }
          return [
            // TODO: we should also validate paper wallets mnemonics here!
            this.isRegular() ? this.props.mnemonicValidator(value) : true,
            this.context.intl.formatMessage(messages.invalidRecoveryPhrase)
          ];
        },
      },
      walletPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.walletPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (!this.state.createPassword) return [true];
          const repeatPasswordField = form.$('repeatPassword');
          if (repeatPasswordField.value.length > 0) {
            repeatPasswordField.validate({ showErrors: true });
          }
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
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  handlePasswordSwitchToggle = (value: boolean) => {
    this.setState({ createPassword: value });
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { createPassword } = this.state;
        const { showCertificateRestore, onSubmit } = this.props;
        const {
          recoveryPhrase,
          walletName,
          walletPassword,
        } = form.values();

        const walletData: Object = {
          recoveryPhrase: join(recoveryPhrase, ' '),
          walletName,
          walletPassword: createPassword ? walletPassword : null,
        };

        if (showCertificateRestore) {
          walletData.type = this.state.activeChoice;
        }

        onSubmit(walletData);
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
    const { createPassword } = this.state;

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

    const walletNameField = form.$('walletName');
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


    const regularTabClasses = classnames([
      'regularTab',
      this.isRegular() ? styles.activeButton : '',
    ]);

    const certificateTabClasses = classnames([
      'certificateTab',
      this.isCertificate() ? styles.activeButton : '',
    ]);

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
              className={regularTabClasses}
              onClick={this.onSelectChoice.bind(this, RESTORE_TYPES.REGULAR)}
            >
              {intl.formatMessage(messages.recoveryPhraseTabTitle)}
            </button>
            <button
              className={certificateTabClasses}
              onClick={this.onSelectChoice.bind(this, RESTORE_TYPES.CERTIFICATE)}
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
          label={this.isRegular()
            ? intl.formatMessage(messages.recoveryPhraseInputLabel)
            : intl.formatMessage(messages.shieldedRecoveryPhraseInputLabel)
          }
          placeholder={this.isRegular()
            ? intl.formatMessage(messages.recoveryPhraseInputHint)
            : intl.formatMessage(messages.shieldedRecoveryPhraseInputHint)
          }
          options={suggestedMnemonics}
          maxSelections={(this.isCertificate() ?
            PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT : WALLET_RECOVERY_PHRASE_WORD_COUNT
          )}
          error={recoveryPhraseField.error}
          maxVisibleOptions={5}
          noResultsMessage={intl.formatMessage(messages.recoveryPhraseNoResults)}
          skin={<SimpleAutocompleteSkin />}
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

  isRegular() {
    return this.state.activeChoice === RESTORE_TYPES.REGULAR;
  }

  isCertificate() {
    return this.state.activeChoice === RESTORE_TYPES.CERTIFICATE;
  }

  onSelectChoice = (choice: string) => {
    const { isSubmitting, onChoiceChange } = this.props;
    if (!isSubmitting) {
      this.setState({
        activeChoice: choice,
        createPassword: true,
      });
      this.resetForm();
      if (onChoiceChange) onChoiceChange();
    }
  };
}
