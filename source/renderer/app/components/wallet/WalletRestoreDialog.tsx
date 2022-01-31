import React, { Component, Fragment } from 'react';
import { join } from 'lodash';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { Input } from 'react-polymorph/lib/components/Input';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import SVGInline from 'react-svg-inline';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
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
  errorOrIncompleteMarker,
  validateMnemonics,
} from '../../utils/validations';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRestoreDialog.scss' or... Remove this comment to see the full error message
import styles from './WalletRestoreDialog.scss';
import { submitOnEnter } from '../../utils/form';
import {
  WALLET_RESTORE_TYPES,
  RECOVERY_PHRASE_WORD_COUNT_OPTIONS,
} from '../../config/walletsConfig';
import {
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
  YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../config/cryptoConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/info-icon.... Remove this comment to see the full error message
import infoIconInline from '../../assets/images/info-icon.inline.svg';
import LoadingSpinner from '../widgets/LoadingSpinner';

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
  shieldedRecoveryPhraseInputPlaceholder: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.placeholder',
    defaultMessage: '!!!Enter word #{wordNumber}',
    description:
      'Placeholder "Enter word #" for the recovery phrase input on the wallet restore dialog.',
  },
  restorePaperWalletButtonLabel: {
    id: 'wallet.restore.dialog.paper.wallet.button.label',
    defaultMessage: '!!!Restore paper wallet',
    description:
      'Label for the "Restore paper wallet" button on the wallet restore dialog.',
  },
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      'We recommend using a password manager app to manage and store your spending password. Generate a unique password using a password manager and paste it here. Passwords should never be reused.',
    description: 'Tooltip for the password input in the wallet dialog.',
  },
});
messages.fieldIsRequired = globalMessages.fieldIsRequired;
type Props = {
  onSubmit: (...args: Array<any>) => any;
  onCancel: (...args: Array<any>) => any;
  isSubmitting: boolean;
  mnemonicValidator: (...args: Array<any>) => any;
  error?: LocalizableError | null | undefined;
  suggestedMnemonics: Array<string>;
  onChoiceChange: ((...args: Array<any>) => any) | null | undefined;
};
type State = {
  walletType: string;
};

@observer
class WalletRestoreDialog extends Component<Props, State> {
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
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
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
            const expectedWordCount =
              RECOVERY_PHRASE_WORD_COUNT_OPTIONS[this.state.walletType];
            return validateMnemonics({
              requiredWords: expectedWordCount,
              providedWords: field.value,
              validator: (providedWords) => [
                // TODO: we should also validate paper wallets mnemonics here!
                !this.isCertificate()
                  ? this.props.mnemonicValidator(
                      providedWords,
                      expectedWordCount
                    )
                  : true,
                this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
              ],
            });
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
                repeatPasswordField.validate({
                  showErrors: true,
                });
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
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { onSubmit } = this.props;
        const { recoveryPhrase, walletName, spendingPassword } = form.values();
        const walletData: Record<string, any> = {
          recoveryPhrase: join(recoveryPhrase, ' '),
          walletName,
          spendingPassword,
        };
        walletData.type = this.state.walletType;
        onSubmit(walletData);
      },
      onError: () =>
        handleFormErrors('.SimpleFormField_error', {
          focusElement: true,
        }),
    });
  };
  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);
  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    // @ts-ignore ts-migrate(2339) FIXME: Property 'each' does not exist on type 'ReactToolb... Remove this comment to see the full error message
    form.each((field) => {
      field.debouncedValidation.cancel();
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'reset' does not exist on type 'ReactTool... Remove this comment to see the full error message
    form.reset();
    // @ts-ignore ts-migrate(2339) FIXME: Property 'showErrors' does not exist on type 'Reac... Remove this comment to see the full error message
    form.showErrors(false);
  };
  resetMnemonics = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
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
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const walletNameField = form.$('walletName');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const recoveryPhraseField = form.$('recoveryPhrase');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const spendingPasswordField = form.$('spendingPassword');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const repeatedPasswordField = form.$('repeatPassword');
    const label = this.isCertificate()
      ? this.context.intl.formatMessage(messages.restorePaperWalletButtonLabel)
      : this.context.intl.formatMessage(messages.importButtonLabel);
    const buttonLabel = !isSubmitting ? label : <LoadingSpinner />;
    const actions = [
      {
        label: buttonLabel,
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
          ref={(autocomplete) => {
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
              : intl.formatMessage(
                  messages.shieldedRecoveryPhraseInputPlaceholder,
                  {
                    wordNumber: recoveryPhraseField.value.length + 1,
                  }
                )
          }
          options={suggestedMnemonics}
          requiredSelections={[RECOVERY_PHRASE_WORD_COUNT_OPTIONS[walletType]]}
          requiredSelectionsInfo={(required, actual) =>
            intl.formatMessage(globalMessages.knownMnemonicWordCount, {
              actual,
              required,
            })
          }
          maxSelections={RECOVERY_PHRASE_WORD_COUNT_OPTIONS[walletType]}
          error={errorOrIncompleteMarker(recoveryPhraseField.error)}
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
            <div className={styles.spendingPasswordField}>
              <PasswordInput
                className="spendingPassword"
                onKeyPress={this.handleSubmitOnEnter}
                {...spendingPasswordField.bind()}
              />
              <PopOver
                content={<FormattedHTMLMessage {...messages.passwordTooltip} />}
                key="tooltip"
              >
                <SVGInline svg={infoIconInline} className={styles.infoIcon} />
              </PopOver>
            </div>
            <div className={styles.spendingPasswordField}>
              <PasswordInput
                className="repeatedPassword"
                onKeyPress={this.handleSubmitOnEnter}
                {...repeatedPasswordField.bind()}
                repeatPassword={spendingPasswordField.value}
                isPasswordRepeat
              />
            </div>
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
    this.setState({
      walletType,
    });
    if (shouldResetForm) this.resetForm();
    this.resetMnemonics();
    if (onChoiceChange) onChoiceChange();
  };
}

export default WalletRestoreDialog;
