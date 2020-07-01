// @flow
import React, { Component, Fragment } from 'react';
import { join } from 'lodash';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { Input } from 'react-polymorph/lib/components/Input';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { PasswordInput } from '../widgets/forms/PasswordInput';
import RadioSet from '../widgets/RadioSet';
import ReactToolboxMobxForm, {
  handleFormErrors,
} from '../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import {
  isValidWalletName,
  isValidSpendingPassword,
  isValidRepeatPassword,
} from '../../utils/validations';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import styles from './WalletRestoreDialog.scss';
import { submitOnEnter } from '../../utils/form';
import {
  WALLET_RESTORE_TYPES,
  RECOVERY_PHRASE_WORD_COUNT_OPTIONS,
} from '../../config/walletsConfig';
import {
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
  YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../config/cryptoConfig';

const messages = defineMessages({
  title: {
    id: 'wallet.restore.dialog.title.label',
    defaultMessage: '!!!Restore a wallet',
    description: 'Label "Restore wallet" on the wallet restore dialog.',
  },
  walletNameInputLabel: {
    id: 'wallet.restore.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description:
      'Label for the wallet name input on the wallet restore dialog.',
  },
  walletNameInputHint: {
    id: 'wallet.restore.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Name the wallet you are restoring',
    description:
      'Hint "Name the wallet you are restoring" for the wallet name input on the wallet restore dialog.',
  },
  recoveryPhraseTypeLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.type.options.label',
    defaultMessage: '!!!Number of words in the recovery phrase',
    description:
      'Label for the recovery phrase type options on the wallet restore dialog.',
  },
  recoveryPhraseTypeOptionWord: {
    id: 'wallet.restore.dialog.recovery.phrase.type.word',
    defaultMessage: '!!! words',
    description:
      'Word for the recovery phrase type on the wallet restore dialog.',
  },
  recoveryPhraseType15WordOption: {
    id: 'wallet.restore.dialog.recovery.phrase.type.15word.option',
    defaultMessage: '!!!Rewards wallet',
    description:
      'Label for the recovery phrase type 15-word option on the wallet restore dialog.',
  },
  recoveryPhraseType12WordOption: {
    id: 'wallet.restore.dialog.recovery.phrase.type.12word.option',
    defaultMessage: '!!!Balance wallet',
    description:
      'Label for the recovery phrase type 12-word option on the wallet restore dialog.',
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.input.label',
    defaultMessage: '!!!Recovery phrase',
    description:
      'Label for the recovery phrase input on the wallet restore dialog.',
  },
  recoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.recovery.phrase.input.hint',
    defaultMessage: '!!!Enter recovery phrase',
    description:
      'Hint "Enter recovery phrase" for the recovery phrase input on the wallet restore dialog.',
  },
  newLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.newLabel',
    defaultMessage: '!!!New',
    description: 'Label "new" on the wallet restore dialog.',
  },
  recoveryPhraseNoResults: {
    id: 'wallet.restore.dialog.recovery.phrase.input.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the recovery phrase input search results.',
  },
  importButtonLabel: {
    id: 'wallet.restore.dialog.restore.wallet.button.label',
    defaultMessage: '!!!Restore wallet',
    description:
      'Label for the "Restore wallet" button on the wallet restore dialog.',
  },
  invalidRecoveryPhrase: {
    id: 'wallet.restore.dialog.form.errors.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
  passwordSectionLabel: {
    id: 'wallet.restore.dialog.passwordSectionLabel',
    defaultMessage: '!!!Spending password',
    description: 'Password creation label.',
  },
  passwordSectionDescription: {
    id: 'wallet.restore.dialog.passwordSectionDescription',
    defaultMessage:
      '!!!Keep your wallet secure by setting the spending password',
    description: 'Password creation description.',
  },
  spendingPasswordLabel: {
    id: 'wallet.restore.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Enter password',
    description:
      'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.restore.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the wallet restore dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.restore.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the wallet restore dialog.',
  },
  recoveryPhraseTabTitle: {
    id: 'wallet.restore.dialog.tab.title.recoveryPhrase',
    defaultMessage: '!!!Daedalus wallet',
    description: 'Tab title "Daedalus wallet" in the wallet restore dialog.',
  },
  certificateTabTitle: {
    id: 'wallet.restore.dialog.tab.title.certificate',
    defaultMessage: '!!!Daedalus paper wallet',
    description:
      'Tab title "Daedalus paper wallet" in the wallet restore dialog.',
  },
  yoroiTabTitle: {
    id: 'wallet.restore.dialog.tab.title.yoroi',
    defaultMessage: '!!!Yoroi wallet',
    description: 'Tab title "Yoroi wallet" in the wallet restore dialog.',
  },
  shieldedRecoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.label',
    defaultMessage: '!!!27-word paper wallet recovery phrase',
    description:
      'Label for the shielded recovery phrase input on the wallet restore dialog.',
  },
  shieldedRecoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.hint',
    defaultMessage:
      '!!!Enter your {numberOfWords}-word paper wallet recovery phrase',
    description:
      'Hint "Enter your 27-word paper wallet recovery phrase." for the recovery phrase input on the wallet restore dialog.',
  },
  restorePaperWalletButtonLabel: {
    id: 'wallet.restore.dialog.paper.wallet.button.label',
    defaultMessage: '!!!Restore paper wallet',
    description:
      'Label for the "Restore paper wallet" button on the wallet restore dialog.',
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
  onChoiceChange: ?Function,
};

type State = {
  walletType: string,
};

@observer
export default class WalletRestoreDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    error: null,
  };

  state = {
    walletType: WALLET_RESTORE_TYPES.LEGACY, // regular | certificate | legacy | yoroi
  };

  recoveryPhraseAutocomplete: Autocomplete;

  componentDidUpdate() {
    if (this.props.error) {
      handleFormErrors('.WalletRestoreDialog_error');
    }
  }

  form = new ReactToolboxMobxForm(
    {
      fields: {
        walletName: {
          label: this.context.intl.formatMessage(messages.walletNameInputLabel),
          placeholder: this.context.intl.formatMessage(
            messages.walletNameInputHint
          ),
          value: '',
          validators: [
            ({ field }) => [
              isValidWalletName(field.value),
              this.context.intl.formatMessage(globalMessages.invalidWalletName),
            ],
          ],
        },
        recoveryPhrase: {
          value: [],
          validators: ({ field }) => {
            const { intl } = this.context;
            const { walletType } = this.state;
            const enteredWords = field.value;
            const wordCount = enteredWords.length;
            const expectedWordCount =
              RECOVERY_PHRASE_WORD_COUNT_OPTIONS[walletType];
            const value = join(enteredWords, ' ');
            // Regular mnemonics have 12 and paper wallet recovery needs 27 words
            const isPhraseComplete = wordCount === expectedWordCount;
            if (!isPhraseComplete) {
              return [
                false,
                intl.formatMessage(globalMessages.incompleteMnemonic, {
                  expected: expectedWordCount,
                }),
              ];
            }
            return [
              // TODO: we should also validate paper wallets mnemonics here!
              !this.isCertificate()
                ? this.props.mnemonicValidator(value, expectedWordCount)
                : true,
              this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
            ];
          },
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              const repeatPasswordField = form.$('repeatPassword');
              if (repeatPasswordField.value.length > 0) {
                repeatPasswordField.validate({ showErrors: true });
              }
              return [
                isValidSpendingPassword(field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidSpendingPassword
                ),
              ];
            },
          ],
        },
        repeatPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              const spendingPassword = form.$('spendingPassword').value;
              if (spendingPassword.length === 0) return [true];
              return [
                isValidRepeatPassword(spendingPassword, field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidRepeatPassword
                ),
              ];
            },
          ],
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { onSubmit } = this.props;
        const { recoveryPhrase, walletName, spendingPassword } = form.values();
        const walletData: Object = {
          recoveryPhrase: join(recoveryPhrase, ' '),
          walletName,
          spendingPassword,
        };

        walletData.type = this.state.walletType;

        onSubmit(walletData);
      },
      onError: () =>
        handleFormErrors('.SimpleFormField_error', { focusElement: true }),
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each(field => {
      field.debouncedValidation.cancel();
    });
    form.reset();
    form.showErrors(false);
  };

  resetMnemonics = () => {
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    recoveryPhraseField.debouncedValidation.cancel();
    recoveryPhraseField.reset();
    recoveryPhraseField.showErrors(false);

    // Autocomplete has to be reset manually
    this.recoveryPhraseAutocomplete.clear();
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { walletType } = this.state;
    const { suggestedMnemonics, isSubmitting, error, onCancel } = this.props;

    const dialogClasses = classnames([
      styles.component,
      styles.dialogWithCertificateRestore,
      'WalletRestoreDialog',
    ]);

    const walletNameFieldClasses = classnames([
      'walletName',
      styles.walletName,
    ]);

    const walletNameField = form.$('walletName');
    const recoveryPhraseField = form.$('recoveryPhrase');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: this.isCertificate()
          ? this.context.intl.formatMessage(
              messages.restorePaperWalletButtonLabel
            )
          : this.context.intl.formatMessage(messages.importButtonLabel),
        primary: true,
        disabled: isSubmitting,
        onClick: this.submit,
      },
    ];

    const regularTabClasses = classnames([
      'regularTab',
      this.isRegular() || this.isLegacy() ? styles.activeButton : '',
    ]);

    const certificateTabClasses = classnames([
      'certificateTab',
      this.isCertificate() ? styles.activeButton : '',
    ]);

    const yoroiTabClasses = classnames([
      'yoroiTab',
      this.isYoroi() ? styles.activeButton : '',
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
        <div className={styles.restoreTypeChoice}>
          <button
            className={regularTabClasses}
            onClick={() =>
              this.onSelectWalletType(WALLET_RESTORE_TYPES.LEGACY, true)
            }
          >
            {intl.formatMessage(messages.recoveryPhraseTabTitle)}
          </button>
          <button
            className={certificateTabClasses}
            onClick={() =>
              this.onSelectWalletType(WALLET_RESTORE_TYPES.CERTIFICATE, true)
            }
          >
            {intl.formatMessage(messages.certificateTabTitle)}
          </button>
          <button
            className={yoroiTabClasses}
            onClick={() =>
              this.onSelectWalletType(WALLET_RESTORE_TYPES.YOROI_LEGACY, true)
            }
          >
            {intl.formatMessage(messages.yoroiTabTitle)}
          </button>
        </div>

        <Input
          className={walletNameFieldClasses}
          onKeyPress={this.handleSubmitOnEnter}
          {...walletNameField.bind()}
          error={walletNameField.error}
        />

        {(this.isRegular() || this.isLegacy()) && (
          <RadioSet
            label={intl.formatMessage(messages.recoveryPhraseTypeLabel)}
            items={[
              {
                key: WALLET_RESTORE_TYPES.LEGACY,
                label: (
                  <Fragment>
                    {LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT}
                    {intl.formatMessage(
                      messages.recoveryPhraseTypeOptionWord
                    )}{' '}
                    <span>
                      (
                      {intl.formatMessage(
                        messages.recoveryPhraseType12WordOption
                      )}
                      )
                    </span>
                  </Fragment>
                ),
                selected: this.isLegacy(),
                onChange: () =>
                  this.onSelectWalletType(WALLET_RESTORE_TYPES.LEGACY),
              },
              {
                key: WALLET_RESTORE_TYPES.REGULAR,
                label: (
                  <Fragment>
                    {WALLET_RECOVERY_PHRASE_WORD_COUNT}
                    {intl.formatMessage(
                      messages.recoveryPhraseTypeOptionWord
                    )}{' '}
                    <span>
                      (
                      {intl.formatMessage(
                        messages.recoveryPhraseType15WordOption
                      )}
                      )
                    </span>
                    <span className={styles.newLabel}>
                      {intl.formatMessage(messages.newLabel)}
                    </span>
                  </Fragment>
                ),
                selected: !this.isLegacy(),
                onChange: () =>
                  this.onSelectWalletType(WALLET_RESTORE_TYPES.REGULAR),
              },
            ]}
          />
        )}

        {this.isYoroi() && (
          <RadioSet
            label={intl.formatMessage(messages.recoveryPhraseTypeLabel)}
            items={[
              {
                key: WALLET_RESTORE_TYPES.YOROI_LEGACY,
                label: (
                  <Fragment>
                    {YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT}
                    {intl.formatMessage(
                      messages.recoveryPhraseTypeOptionWord
                    )}{' '}
                    <span>
                      (
                      {intl.formatMessage(
                        messages.recoveryPhraseType12WordOption
                      )}
                      )
                    </span>
                  </Fragment>
                ),
                selected: this.isYoroiLegacy(),
                onChange: () =>
                  this.onSelectWalletType(WALLET_RESTORE_TYPES.YOROI_LEGACY),
              },
              {
                key: WALLET_RESTORE_TYPES.YOROI_REGULAR,
                label: (
                  <Fragment>
                    {YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT}
                    {intl.formatMessage(
                      messages.recoveryPhraseTypeOptionWord
                    )}{' '}
                    <span>
                      (
                      {intl.formatMessage(
                        messages.recoveryPhraseType15WordOption
                      )}
                      )
                    </span>
                    <span className={styles.newLabel}>
                      {intl.formatMessage(messages.newLabel)}
                    </span>
                  </Fragment>
                ),
                selected: this.isYoroiRegular(),
                onChange: () =>
                  this.onSelectWalletType(WALLET_RESTORE_TYPES.YOROI_REGULAR),
              },
            ]}
          />
        )}

        <Autocomplete
          {...recoveryPhraseField.bind()}
          ref={autocomplete => {
            this.recoveryPhraseAutocomplete = autocomplete;
          }}
          label={
            !this.isCertificate()
              ? intl.formatMessage(messages.recoveryPhraseInputLabel)
              : intl.formatMessage(messages.shieldedRecoveryPhraseInputLabel)
          }
          placeholder={
            !this.isCertificate()
              ? intl.formatMessage(messages.recoveryPhraseInputHint)
              : intl.formatMessage(messages.shieldedRecoveryPhraseInputHint, {
                  numberOfWords: PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                })
          }
          options={suggestedMnemonics}
          maxSelections={RECOVERY_PHRASE_WORD_COUNT_OPTIONS[walletType]}
          error={recoveryPhraseField.error}
          maxVisibleOptions={5}
          noResultsMessage={intl.formatMessage(
            messages.recoveryPhraseNoResults
          )}
          optionHeight={50}
        />

        <div className={styles.spendingPasswordWrapper}>
          <div className={styles.passwordSectionLabel}>
            {intl.formatMessage(messages.passwordSectionLabel)}
          </div>

          <div className={styles.passwordSectionDescription}>
            {intl.formatMessage(messages.passwordSectionDescription)}
          </div>

          <div className={styles.spendingPasswordFields}>
            <PasswordInput
              className="spendingPassword"
              onKeyPress={this.handleSubmitOnEnter}
              {...spendingPasswordField.bind()}
            />
            <PasswordInput
              className="repeatedPassword"
              onKeyPress={this.handleSubmitOnEnter}
              {...repeatedPasswordField.bind()}
              repeatPassword={spendingPasswordField.value}
              isPasswordRepeat
            />
          </div>
          <p className={styles.passwordInstructions}>
            <FormattedHTMLMessage {...globalMessages.passwordInstructions} />
          </p>
        </div>

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
      </Dialog>
    );
  }

  isRegular() {
    return this.state.walletType === WALLET_RESTORE_TYPES.REGULAR;
  }

  isCertificate() {
    return this.state.walletType === WALLET_RESTORE_TYPES.CERTIFICATE;
  }

  isLegacy() {
    return this.state.walletType === WALLET_RESTORE_TYPES.LEGACY;
  }

  isYoroiLegacy() {
    return this.state.walletType === WALLET_RESTORE_TYPES.YOROI_LEGACY;
  }

  isYoroiRegular() {
    return this.state.walletType === WALLET_RESTORE_TYPES.YOROI_REGULAR;
  }

  isYoroi() {
    return this.isYoroiLegacy() || this.isYoroiRegular();
  }

  onSelectWalletType = (walletType: string, shouldResetForm?: boolean) => {
    const { onChoiceChange, isSubmitting } = this.props;
    if (isSubmitting) return;
    this.setState({ walletType });
    if (shouldResetForm) this.resetForm();
    this.resetMnemonics();
    if (onChoiceChange) onChoiceChange();
  };
}
