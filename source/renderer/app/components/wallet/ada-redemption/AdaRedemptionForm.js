// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { join } from 'lodash';
import { isEmail, isEmpty } from 'validator';
import classnames from 'classnames';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { Select } from 'react-polymorph/lib/components/Select';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import AdaCertificateUploadWidget from '../../widgets/forms/AdaCertificateUploadWidget';
import AdaRedemptionChoices from './AdaRedemptionChoices';
import AdaRedemptionDisclaimer from './AdaRedemptionDisclaimer';
import BorderedBox from '../../widgets/BorderedBox';
import LocalizableError from '../../../i18n/LocalizableError';
import {
  InvalidMnemonicError,
  InvalidEmailError,
  FieldRequiredError,
} from '../../../i18n/errors';
import globalMessages from '../../../i18n/global-messages';
import styles from './AdaRedemptionForm.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { ADA_REDEMPTION_PASSPHRASE_LENGTH } from '../../../config/cryptoConfig';
import { ADA_REDEMPTION_TYPES } from '../../../types/redemptionTypes';
import type { RedemptionTypeChoices } from '../../../types/redemptionTypes';
import { submitOnEnter } from '../../../utils/form';

const messages = defineMessages({
  headline: {
    id: 'wallet.redeem.dialog.headline',
    defaultMessage: '!!!Ada Redemption',
    description: 'Headline "Ada redemption" dialog.',
  },
  instructionsRegular: {
    id: 'wallet.redeem.dialog.instructions.regular',
    defaultMessage:
      '!!!<p>To redeem your ada, upload your certificate or copy and paste your redemption code from the certificate. Below is an example of a redemption key. Your key will look similar:</p><p><strong>B_GQOAffMBeRIn6vh1hJmeOT3ViS_TmaT4XAHAfDVH0=</strong></p><p>If you upload a PDF file with your certificate, a redemption code will be automatically extracted.</p><p>If you upload an <strong>encrypted certificate</strong>, you will need to provide a <strong>{adaRedemptionPassphraseLength} word mnemonic passphrase</strong> to decrypt your certificate and your redemption code will be automatically extracted.</p>',
    description:
      'Detailed instructions for redeeming ada from the regular vending',
  },
  instructionsForceVended: {
    id: 'wallet.redeem.dialog.instructions.forceVended',
    defaultMessage:
      '!!!<p>To redeem your ada, upload your certificate or copy and paste your redemption code from the certificate. Below is an example of a redemption key. Your key will look similar:</p><p><strong>B_GQOAffMBeRIn6vh1hJmeOT3ViS_TmaT4XAHAfDVH0=</strong></p><p>If you upload a PDF file with your certificate, the redemption code will be automatically extracted.</p><p>If you upload an <strong>encrypted certificate</strong>, you will need to provide <strong>your email address, ada passcode and ada amount<strong> to decrypt your certificate and your redemption code will be automatically extracted.</p>',
    description:
      'Detailed instructions for redeeming ada from the force vending',
  },
  instructionsRecoveryRegular: {
    id: 'wallet.redeem.dialog.instructions.recoveryRegular',
    defaultMessage:
      '!!!<p>To redeem your ada using the regularly vended certificate from the recovery service, please upload your encrypted certificate and enter a {adaRedemptionPassphraseLength}-word mnemonic passphrase.</p><p>After you upload your <strong>encrypted certificate</strong> and enter your <strong>{adaRedemptionPassphraseLength}-word mnemonic passphrase</strong>, your redemption key will be automatically extracted and you will be able to redeem your ada to the selected wallet.</p>',
    description:
      'Detailed instructions for redeeming ada from the regular vending via Recovery service',
  },
  instructionsRecoveryForceVended: {
    id: 'wallet.redeem.dialog.instructions.recoveryForceVended',
    defaultMessage:
      '!!!<p>To redeem your ada using the force vended certificate from the recovery service, please upload your encrypted certificate and enter the decryption key. Your decryption key should look like this:</p><p><strong>qXQWDxI3JrlFRtC4SeQjeGzLbVXWBomYPbNO1Vfm1T4=</strong></p><p>After you upload your <strong>encrypted certificate</strong> and enter your <strong>decryption key</strong>, your redemption key will be automatically extracted and you will be able to redeem your ada to the selected wallet.</p>',
    description:
      'Detailed instructions for redeeming ada from the force vending via Recovery service',
  },
  instructionsPaperVended: {
    id: 'wallet.redeem.dialog.instructions.paperVended',
    defaultMessage:
      '!!!<p>To redeem your ada, enter your shielded vending key from the certificate, choose a wallet where ada should be redeemed and enter {adaRedemptionPassphraseLength} word mnemonic passphrase.</p>',
    description:
      'Detailed instructions for redeeming ada from the paper vending',
  },
  certificateLabel: {
    id: 'wallet.redeem.dialog.certificateLabel',
    defaultMessage: '!!!Certificate',
    description: 'Label for the certificate file upload',
  },
  certificateHint: {
    id: 'wallet.redeem.dialog.certificateHint',
    defaultMessage:
      '!!!Drop the file with your certificate here or click to find on your computer',
    description: 'Hint for the certificate file upload',
  },
  walletSelectLabel: {
    id: 'wallet.redeem.dialog.walletSelectLabel',
    defaultMessage: '!!!Choose Wallet',
    description: 'Label for the wallet select field on ada redemption form',
  },
  passphraseLabel: {
    id: 'wallet.redeem.dialog.passphraseLabel',
    defaultMessage: '!!!Passphrase to Decrypt the ada Voucher Certificate',
    description:
      'Label for the passphrase to decrypt ada voucher certificate input',
  },
  passphraseHint: {
    id: 'wallet.redeem.dialog.passphraseHint',
    defaultMessage: '!!!Enter your {length} word mnemonic here',
    description: 'Hint for the mnemonic passphrase input',
  },
  passphraseNoResults: {
    id: 'wallet.redeem.dialog.passphrase.input.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the passphrase input search results.',
  },
  redemptionKeyLabel: {
    id: 'wallet.redeem.dialog.redemptionKeyLabel',
    defaultMessage: '!!!Redemption key',
    description: 'Label for ada redemption key input',
  },
  shieldedRedemptionKeyLabel: {
    id: 'wallet.redeem.dialog.shieldedRedemptionKeyLabel',
    defaultMessage: '!!!Shielded redemption key',
    description: 'Label for shielded redemption key input',
  },
  decryptionKeyLabel: {
    id: 'wallet.redeem.dialog.decryptionKeyLabel',
    defaultMessage: '!!!Decryption key',
    description: 'Label for decryption key input',
  },
  redemptionKeyError: {
    id: 'wallet.redeem.dialog.redemptionCodeError',
    defaultMessage: '!!!Invalid redemption key',
    description: 'Error "Invalid redemption key" for ada redemption code input',
  },
  shieldedRedemptionKeyError: {
    id: 'wallet.redeem.dialog.shieldedRedemptionCodeError',
    defaultMessage: '!!!Invalid shielded vending key',
    description:
      'Error "Invalid shielded vending key" for ada redemption code input',
  },
  redemptionKeyHint: {
    id: 'wallet.redeem.dialog.redemptionCodeHint',
    defaultMessage: '!!!Enter your redemption key or upload a certificate',
    description: 'Hint for ada redemption key input',
  },
  recoveryRedemptionKeyHint: {
    id: 'wallet.redeem.dialog.recoveryRedemptionKeyHint',
    defaultMessage: '!!!Upload your certificate',
    description: 'Hint for ada redemption key input shown on Recovery tabs',
  },
  shieldedRedemptionKeyHint: {
    id: 'wallet.redeem.dialog.shieldedRedemptionKeyHint',
    defaultMessage: '!!!Enter your shielded vending key',
    description: 'Hint for shielded vending key input',
  },
  decryptionKeyHint: {
    id: 'wallet.redeem.dialog.decryptionKeyHint',
    defaultMessage: '!!!Enter your decryption key',
    description: 'Hint for decryption key input',
  },
  submitLabel: {
    id: 'wallet.redeem.dialog.submitLabel',
    defaultMessage: '!!!Redeem your money',
    description: 'Label for the "Redeem your money" dialog submit button.',
  },
  emailLabel: {
    id: 'wallet.redeem.dialog.emailLabel',
    defaultMessage: '!!!Email',
    description: 'Label for the email input field.',
  },
  emailHint: {
    id: 'wallet.redeem.dialog.emailHint',
    defaultMessage: '!!!Enter your email address',
    description: 'Hint for the email input field.',
  },
  adaPasscodeLabel: {
    id: 'wallet.redeem.dialog.adaPasscodeLabel',
    defaultMessage: '!!!Ada passcode',
    description: 'Label for the ada passcode input field.',
  },
  adaPasscodeHint: {
    id: 'wallet.redeem.dialog.adaPasscodeHint',
    defaultMessage: '!!!Enter your ada passcode',
    description: 'Hint for the ada passcode input field.',
  },
  adaAmountLabel: {
    id: 'wallet.redeem.dialog.adaAmountLabel',
    defaultMessage: '!!!Ada amount',
    description: 'Label for the ada amount input field.',
  },
  adaAmountHint: {
    id: 'wallet.redeem.dialog.adaAmountHint',
    defaultMessage: '!!!Enter your ada amount',
    description: 'Hint for the ada amount input field.',
  },
  spendingPasswordPlaceholder: {
    id: 'wallet.redeem.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id: 'wallet.redeem.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Password',
    description: 'Label for "spending password"',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  wallets: Array<{ value: string, label: string }>,
  onAcceptRedemptionDisclaimer: Function,
  onChooseRedemptionType: Function,
  onCertificateSelected: Function,
  onRemoveCertificate: Function,
  onPassPhraseChanged: Function,
  onEmailChanged: Function,
  onAdaPasscodeChanged: Function,
  onAdaAmountChanged: Function,
  onRedemptionCodeChanged: Function,
  onDecryptionKeyChanged: Function,
  onSubmit: Function,
  redemptionType: RedemptionTypeChoices,
  postVendRedemptionCodeValidator: Function,
  redemptionCodeValidator: Function,
  mnemonicValidator: Function,
  getSelectedWallet: Function,
  isRedemptionDisclaimerAccepted: boolean,
  isSubmitting: boolean,
  isCertificateSelected: boolean,
  isCertificateEncrypted: boolean,
  showInputsForDecryptingForceVendedCertificate: boolean,
  showInputForDecryptionKey: boolean,
  showPassPhraseWidget: boolean,
  isCertificateInvalid: boolean,
  redemptionCode: ?string,
  error: ?LocalizableError,
  suggestedMnemonics: Array<string>,
};

@observer
export default class AdaRedemptionForm extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        certificate: {
          label: this.context.intl.formatMessage(messages.certificateLabel),
          placeholder: this.context.intl.formatMessage(
            messages.certificateHint
          ),
          type: 'file',
        },
        passPhrase: {
          label: this.context.intl.formatMessage(messages.passphraseLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passphraseHint,
            {
              length: ADA_REDEMPTION_PASSPHRASE_LENGTH,
            }
          ),
          value: [],
          validators: [
            ({ field }) => {
              // Don't validate No pass phrase needed when certificate is not encrypted
              if (!this.props.showPassPhraseWidget) return [true];
              // Otherwise check mnemonic
              const passPhrase = join(field.value, ' ');
              if (!isEmpty(passPhrase))
                this.props.onPassPhraseChanged(passPhrase);
              return [
                this.props.mnemonicValidator(passPhrase),
                this.context.intl.formatMessage(new InvalidMnemonicError()),
              ];
            },
          ],
        },
        redemptionKey: {
          label: this.context.intl.formatMessage(messages.redemptionKeyLabel),
          value: '',
          validators: ({ field }) => {
            if (this.props.redemptionType === ADA_REDEMPTION_TYPES.PAPER_VENDED)
              return [true];
            const value = this.props.redemptionCode || field.value;
            if (value === '')
              return [
                false,
                this.context.intl.formatMessage(messages.fieldIsRequired),
              ];
            return [
              this.props.redemptionCodeValidator(value),
              this.context.intl.formatMessage(messages.redemptionKeyError),
            ];
          },
        },
        shieldedRedemptionKey: {
          label: this.context.intl.formatMessage(
            messages.shieldedRedemptionKeyLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.shieldedRedemptionKeyHint
          ),
          value: '',
          validators: ({ field }) => {
            if (this.props.redemptionType !== ADA_REDEMPTION_TYPES.PAPER_VENDED)
              return [true];
            const value = field.value;
            if (value === '')
              return [
                false,
                this.context.intl.formatMessage(messages.fieldIsRequired),
              ];
            return [
              this.props.postVendRedemptionCodeValidator(value),
              this.context.intl.formatMessage(
                messages.shieldedRedemptionKeyError
              ),
            ];
          },
        },
        walletId: {
          label: this.context.intl.formatMessage(messages.walletSelectLabel),
          value: this.props.wallets[0].value,
        },
        email: {
          label: this.context.intl.formatMessage(messages.emailLabel),
          placeholder: this.context.intl.formatMessage(messages.emailHint),
          value: '',
          validators: [
            ({ field }) => {
              if (!this.props.showInputsForDecryptingForceVendedCertificate)
                return [true];
              const email = field.value;
              if (isEmail(email)) this.props.onEmailChanged(email);
              return [
                isEmail(email),
                this.context.intl.formatMessage(new InvalidEmailError()),
              ];
            },
          ],
        },
        adaPasscode: {
          label: this.context.intl.formatMessage(messages.adaPasscodeLabel),
          placeholder: this.context.intl.formatMessage(
            messages.adaPasscodeHint
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (!this.props.showInputsForDecryptingForceVendedCertificate)
                return [true];
              const adaPasscode = field.value;
              if (!isEmpty(adaPasscode))
                this.props.onAdaPasscodeChanged(adaPasscode);
              return [
                !isEmpty(adaPasscode),
                this.context.intl.formatMessage(new FieldRequiredError()),
              ];
            },
          ],
        },
        adaAmount: {
          label: this.context.intl.formatMessage(messages.adaAmountLabel),
          placeholder: this.context.intl.formatMessage(messages.adaAmountHint),
          value: '',
          validators: [
            ({ field }) => {
              if (!this.props.showInputsForDecryptingForceVendedCertificate)
                return [true];
              const adaAmount = field.value;
              if (!isEmpty(adaAmount)) this.props.onAdaAmountChanged(adaAmount);
              return [
                !isEmpty(adaAmount),
                this.context.intl.formatMessage(new FieldRequiredError()),
              ];
            },
          ],
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              const password = field.value;
              const walletId = form.$('walletId').value;
              const wallet = this.props.getSelectedWallet(walletId);
              if (wallet && wallet.hasPassword && password === '') {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
        decryptionKey: {
          label: this.context.intl.formatMessage(messages.decryptionKeyLabel),
          placeholder: this.context.intl.formatMessage(
            messages.decryptionKeyHint
          ),
          value: '',
          validators: ({ field }) => {
            if (!this.props.showInputForDecryptionKey) return [true];
            const decryptionKey = field.value;
            if (!isEmpty(decryptionKey))
              this.props.onDecryptionKeyChanged(decryptionKey);
            return [
              !isEmpty(decryptionKey),
              this.context.intl.formatMessage(new FieldRequiredError()),
            ];
          },
        },
      },
    },
    {
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const {
          walletId,
          shieldedRedemptionKey,
          spendingPassword,
        } = form.values();
        this.props.onSubmit({
          walletId,
          shieldedRedemptionKey,
          spendingPassword: spendingPassword || null,
        });
      },
      onError: () => {},
    });
  };

  resetForm = () => {
    const { form } = this;

    // Cancel all debounced field validations
    form.each(field => {
      field.debouncedValidation.cancel();
    });

    // We can not user form.reset() call here as it would reset selected walletId
    // which is a bad UX since we are calling resetForm on certificate add/remove
    form.$('spendingPassword').reset();
    form.$('adaAmount').reset();
    form.$('adaPasscode').reset();
    form.$('certificate').reset();
    form.$('email').reset();
    form.$('passPhrase').reset();
    form.$('redemptionKey').reset();
    form.$('shieldedRedemptionKey').reset();
    form.$('decryptionKey').reset();

    form.showErrors(false);
  };

  onWalletChange = (walletId: string) => {
    const { form } = this;
    form.$('walletId').value = walletId;
    form.$('spendingPassword').value = '';
  };

  render() {
    const { intl } = this.context;
    const { form, resetForm, submit } = this;
    const {
      wallets,
      isCertificateSelected,
      isCertificateEncrypted,
      isSubmitting,
      onCertificateSelected,
      redemptionCode,
      onRedemptionCodeChanged,
      onRemoveCertificate,
      onChooseRedemptionType,
      isCertificateInvalid,
      redemptionType,
      showInputsForDecryptingForceVendedCertificate,
      showPassPhraseWidget,
      isRedemptionDisclaimerAccepted,
      onAcceptRedemptionDisclaimer,
      error,
      getSelectedWallet,
      suggestedMnemonics,
      showInputForDecryptionKey,
    } = this.props;
    const certificateField = form.$('certificate');
    const passPhraseField = form.$('passPhrase');
    const redemptionKeyField = form.$('redemptionKey');
    const shieldedRedemptionKeyField = form.$('shieldedRedemptionKey');
    const walletId = form.$('walletId');
    const emailField = form.$('email');
    const adaPasscodeField = form.$('adaPasscode');
    const adaAmountField = form.$('adaAmount');
    const spendingPasswordField = form.$('spendingPassword');
    const decryptionKeyField = form.$('decryptionKey');
    const componentClasses = classnames([
      styles.component,
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const selectedWallet = getSelectedWallet(walletId.value);
    const walletHasPassword = selectedWallet.hasPassword;

    const showUploadWidget =
      redemptionType !== ADA_REDEMPTION_TYPES.PAPER_VENDED;
    const isRecovery =
      redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_REGULAR ||
      redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED;

    const passwordSubmittable =
      !walletHasPassword || spendingPasswordField.value !== '';

    let canSubmit = false;
    if (
      (redemptionType === ADA_REDEMPTION_TYPES.REGULAR ||
        redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_REGULAR) &&
      redemptionCode !== '' &&
      passwordSubmittable
    )
      canSubmit = true;
    if (
      (redemptionType === ADA_REDEMPTION_TYPES.FORCE_VENDED ||
        redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED) &&
      redemptionCode !== '' &&
      passwordSubmittable
    )
      canSubmit = true;
    if (
      redemptionType === ADA_REDEMPTION_TYPES.PAPER_VENDED &&
      shieldedRedemptionKeyField.isDirty &&
      passPhraseField.isDirty &&
      passwordSubmittable
    )
      canSubmit = true;

    let instructionMessage = '';
    let instructionValues = {};
    switch (redemptionType) {
      case ADA_REDEMPTION_TYPES.REGULAR:
        instructionMessage = messages.instructionsRegular;
        instructionValues = {
          adaRedemptionPassphraseLength: ADA_REDEMPTION_PASSPHRASE_LENGTH,
        };
        break;
      case ADA_REDEMPTION_TYPES.FORCE_VENDED:
        instructionMessage = messages.instructionsForceVended;
        break;
      case ADA_REDEMPTION_TYPES.PAPER_VENDED:
        instructionMessage = messages.instructionsPaperVended;
        instructionValues = {
          adaRedemptionPassphraseLength: ADA_REDEMPTION_PASSPHRASE_LENGTH,
        };
        break;
      case ADA_REDEMPTION_TYPES.RECOVERY_REGULAR:
        instructionMessage = messages.instructionsRecoveryRegular;
        instructionValues = {
          adaRedemptionPassphraseLength: ADA_REDEMPTION_PASSPHRASE_LENGTH,
        };
        break;
      case ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED:
        instructionMessage = messages.instructionsRecoveryForceVended;
        break;
      default:
        instructionMessage = messages.instructionsRegular;
    }

    const submitButtonClasses = classnames([
      'primary',
      isSubmitting ? styles.submitButtonSpinning : styles.submitButton,
    ]);

    return (
      <div className={componentClasses}>
        <div className={styles.scrollableContent}>
          <BorderedBox>
            <h1 className={styles.headline}>
              {intl.formatMessage(messages.headline)}
            </h1>

            <AdaRedemptionChoices
              activeChoice={redemptionType}
              onSelectChoice={(choice: string) => {
                const isRedemptionTypeChanged = redemptionType !== choice;
                if (isRedemptionTypeChanged) resetForm();
                onChooseRedemptionType(choice);
              }}
            />

            <div className={styles.instructions}>
              <FormattedHTMLMessage
                {...instructionMessage}
                values={instructionValues}
              />
            </div>

            <div className={styles.redemption}>
              <div className={styles.inputs}>
                {redemptionType !== ADA_REDEMPTION_TYPES.PAPER_VENDED ? (
                  <Input
                    onKeyPress={submitOnEnter.bind(this, submit)}
                    className="redemption-key"
                    {...redemptionKeyField.bind()}
                    placeholder={intl.formatMessage(
                      messages[
                        isRecovery
                          ? 'recoveryRedemptionKeyHint'
                          : 'redemptionKeyHint'
                      ]
                    )}
                    value={redemptionCode}
                    onChange={value => {
                      onRedemptionCodeChanged(value);
                      redemptionKeyField.onChange(value);
                    }}
                    disabled={isRecovery || isCertificateSelected}
                    error={redemptionKeyField.error}
                    skin={InputSkin}
                  />
                ) : (
                  <Input
                    onKeyPress={submitOnEnter.bind(this, submit)}
                    className="shielded-redemption-key"
                    {...shieldedRedemptionKeyField.bind()}
                    disabled={isCertificateSelected}
                    error={shieldedRedemptionKeyField.error}
                    skin={InputSkin}
                  />
                )}

                <Select
                  className={styles.walletSelect}
                  options={wallets}
                  {...walletId.bind()}
                  onChange={this.onWalletChange}
                  isOpeningUpward
                  skin={SelectSkin}
                />
              </div>

              {showUploadWidget ? (
                <div className={styles.certificate}>
                  <AdaCertificateUploadWidget
                    {...certificateField.bind()}
                    selectedFile={certificateField.value}
                    onFileSelected={file => {
                      resetForm();
                      onCertificateSelected(file);
                      certificateField.set(file);
                    }}
                    isCertificateEncrypted={isCertificateEncrypted}
                    isCertificateSelected={isCertificateSelected}
                    isCertificateInvalid={isCertificateInvalid}
                    onRemoveCertificate={() => {
                      resetForm();
                      onRemoveCertificate();
                    }}
                  />
                </div>
              ) : null}
            </div>

            {walletHasPassword ? (
              <div className={styles.passwordInput}>
                <Input
                  onKeyPress={submitOnEnter.bind(this, submit)}
                  className="spendingPassword"
                  {...spendingPasswordField.bind()}
                  error={spendingPasswordField.error}
                  skin={InputSkin}
                />
              </div>
            ) : null}

            {showPassPhraseWidget ? (
              <div className={styles.passPhrase}>
                <Autocomplete
                  className="pass-phrase"
                  options={suggestedMnemonics}
                  maxSelections={ADA_REDEMPTION_PASSPHRASE_LENGTH}
                  {...passPhraseField.bind()}
                  error={passPhraseField.error}
                  maxVisibleOptions={5}
                  noResultsMessage={intl.formatMessage(
                    messages.passphraseNoResults
                  )}
                  isOpeningUpward
                  skin={AutocompleteSkin}
                />
              </div>
            ) : null}

            {showInputForDecryptionKey ? (
              <div className={styles.decryptionKey}>
                <Input
                  onKeyPress={submitOnEnter.bind(this, submit)}
                  className="decryption-key"
                  {...decryptionKeyField.bind()}
                  error={decryptionKeyField.error}
                  skin={InputSkin}
                />
              </div>
            ) : null}

            {showInputsForDecryptingForceVendedCertificate ? (
              <div className={styles.email}>
                <Input
                  onKeyPress={submitOnEnter.bind(this, submit)}
                  className="email"
                  {...emailField.bind()}
                  error={emailField.error}
                  skin={InputSkin}
                />
              </div>
            ) : null}

            {showInputsForDecryptingForceVendedCertificate ? (
              <div className={styles.adaPasscode}>
                <Input
                  onKeyPress={submitOnEnter.bind(this, submit)}
                  className="ada-passcode"
                  {...adaPasscodeField.bind()}
                  error={adaPasscodeField.error}
                  skin={InputSkin}
                />
              </div>
            ) : null}

            {showInputsForDecryptingForceVendedCertificate ? (
              <div className={styles.adaAmount}>
                <Input
                  onKeyPress={submitOnEnter.bind(this, submit)}
                  className="ada-amount"
                  {...adaAmountField.bind()}
                  error={adaAmountField.error}
                  skin={InputSkin}
                />
              </div>
            ) : null}

            {error ? (
              <p className={styles.error}>{intl.formatMessage(error)}</p>
            ) : null}

            <Button
              className={submitButtonClasses}
              label={intl.formatMessage(messages.submitLabel)}
              onClick={submit}
              disabled={!canSubmit}
              skin={ButtonSkin}
            />
          </BorderedBox>
        </div>

        {!isRedemptionDisclaimerAccepted ? (
          <AdaRedemptionDisclaimer onSubmit={onAcceptRedemptionDisclaimer} />
        ) : null}
      </div>
    );
  }
}
