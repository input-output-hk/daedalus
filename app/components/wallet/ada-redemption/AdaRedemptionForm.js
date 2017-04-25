// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { isEmail, isEmpty } from 'validator';
import classnames from 'classnames';
import Button from 'react-toolbox/lib/button/Button';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Dropup from '../../widgets/forms/Dropup';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import AdaCertificateUploadWidget from '../../widgets/forms/AdaCertificateUploadWidget';
import AdaRedemptionChoices from './AdaRedemptionChoices';
import BorderedBox from '../../widgets/BorderedBox';
import LocalizableError from '../../../i18n/LocalizableError';
import { InvalidMnemonicError, InvalidEmailError, FieldRequiredError } from '../../../i18n/errors';
import globalMessages from '../../../i18n/global-messages';
import styles from './AdaRedemptionForm.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.redeem.dialog.headline',
    defaultMessage: '!!!Ada Redemption',
    description: 'headline "Ada redemption" dialog.'
  },
  instructionsRegular: {
    id: 'wallet.redeem.dialog.instructions.regular',
    defaultMessage: `<p>To redeem your Ada upload your certificate or copy and paste your redemption
code from the certificate. Here is an example redemption key, yours will look similar:</p>
<p><strong>B_GQOAffMBeRIn6vh1hJmeOT3ViS_TmaT4XAHAfDVH0=</strong></p>
<p>If you upload a PDF file with your certificate redemption code will be automatically extracted.</p>
<p>If you upload <strong>encrypted certificate</strong> you will need to provide a <strong>9 word mnemonic
passphrase</strong> to decrypt your certificate and your redemption code will be automatically extracted.</p>`,
    description: '!!!Detailed instructions for redeeming Ada from the regular vending',
  },
  instructionsForceVended: {
    id: 'wallet.redeem.dialog.instructions.forceVended',
    defaultMessage: `<p>To redeem your Ada upload your certificate or copy and paste your redemption code from the certificate.
Here is an example redemption key, yours will look similar:</p><p><strong>B_GQOAffMBeRIn6vh1hJmeOT3ViS_TmaT4XAHAfDVH0=</strong></p>
<p>If you upload a PDF file with your certificate redemption code will be automatically extracted.</p>
<p>If you upload <strong>encrypted certificate</strong> you will need to provide a <strong>your email address, Ada passcode and Ada amount</strong>
to decrypt your certificate and your redemption code will be automatically extracted.</p>`,
    description: '!!!Detailed instructions.regular',
  },
  instructionsPaperVended: {
    id: 'wallet.redeem.dialog.instructions.paperVended',
    defaultMessage: `<p>To redeem your Ada enter your Shielded vending key from the certificate, choose a wallet
where Ada should be redeemed and enter 9 word mnemonic passphrase.</p>`,
    description: '!!!Detailed instructions.regular',
  },
  certificateLabel: {
    id: 'wallet.redeem.dialog.certificateLabel',
    defaultMessage: '!!!Certificate',
    description: 'Label for the certificate file upload'
  },
  certificateHint: {
    id: 'wallet.redeem.dialog.certificateHint',
    defaultMessage: '!!!Drop the file with your certificate here or click to find on your computer',
    description: 'Hint for the certificate file upload'
  },
  walletSelectLabel: {
    id: 'wallet.redeem.dialog.walletSelectLabel',
    defaultMessage: '!!!Choose Wallet',
    description: 'Label for the wallet select field on Ada redemption form'
  },
  passphraseLabel: {
    id: 'wallet.redeem.dialog.passphraseLabel',
    defaultMessage: '!!!Passphrase to Decrypt the Ada Voucher Certificate',
    description: 'Label for the passphrase to decrypt Ada voucher certificate input'
  },
  passphraseHint: {
    id: 'wallet.redeem.dialog.passphraseHint',
    defaultMessage: '!!!Enter your 9 word mnemonic here',
    description: 'Hint for the mnemonic passphrase input'
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
  redemptionKeyError: {
    id: 'wallet.redeem.dialog.redemptionCodeError',
    defaultMessage: '!!!Invalid redemption key',
    description: 'Error "Invalid redemption key" for ada redemption code input',
  },
  shieldedRedemptionKeyError: {
    id: 'wallet.redeem.dialog.shieldedRedemptionCodeError',
    defaultMessage: '!!!Invalid Shielded Redemption Key',
    description: 'Error "Invalid Redemption Code" for ada redemption code input',
  },
  redemptionKeyHint: {
    id: 'wallet.redeem.dialog.redemptionCodeHint',
    defaultMessage: '!!!Enter your redemption key or upload a certificate',
    description: 'Hint for ada redemption key input',
  },
  shieldedRedemptionKeyHint: {
    id: 'wallet.redeem.dialog.shieldedRedemptionKeyHint',
    defaultMessage: '!!!Enter your shielded redemption key',
    description: 'Hint for shielded redemption key input',
  },
  submitLabel: {
    id: 'wallet.redeem.dialog.submitLabel',
    defaultMessage: '!!!Redeem your money',
    description: 'Label for the "Ada redemption" dialog submit button.'
  },
  emailLabel: {
    id: 'wallet.redeem.dialog.emailLabel',
    defaultMessage: '!!!Email',
    description: 'Label for the email input field.'
  },
  emailHint: {
    id: 'wallet.redeem.dialog.emailHint',
    defaultMessage: '!!!Enter your email address',
    description: 'Hint for the email input field.'
  },
  adaPasscodeLabel: {
    id: 'wallet.redeem.dialog.adaPasscodeLabel',
    defaultMessage: '!!!Ada passcode',
    description: 'Label for the ada passcode input field.'
  },
  adaPasscodeHint: {
    id: 'wallet.redeem.dialog.adaPasscodeHint',
    defaultMessage: '!!!Enter your Ada passcode',
    description: 'Hint for the Ada passcode input field.'
  },
  adaAmountLabel: {
    id: 'wallet.redeem.dialog.adaAmountLabel',
    defaultMessage: '!!!Ada amount',
    description: 'Label for the ada amount input field.'
  },
  adaAmountHint: {
    id: 'wallet.redeem.dialog.adaAmountHint',
    defaultMessage: '!!!Enter your Ada passcode',
    description: 'Hint for the Ada amount input field.'
  },
  disclaimerTitle: {
    id: 'wallet.redeem.dialog.disclaimerTitle',
    defaultMessage: '!!!Daedalus Redemption Disclaimer',
    description: 'Testnet Ada redemption disclaimer title.'
  },
  disclaimer: {
    id: 'wallet.redeem.dialog.disclaimer',
    defaultMessage: '!!!Disclaimer',
    description: 'Testnet Ada redemption disclaimer.'
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class AdaRedemptionForm extends Component {

  props: {
    wallets: Array<{ value: string, label: string }>,
    onChooseRedemptionType: Function,
    onCertificateSelected: Function,
    onRemoveCertificate: Function,
    onPassPhraseChanged: Function,
    onEmailChanged: Function,
    onAdaPasscodeChanged: Function,
    onAdaAmountChanged: Function,
    onRedemptionCodeChanged: Function,
    onSubmit: Function,
    redemptionType: string,
    postVendRedemptionCodeValidator: Function,
    redemptionCodeValidator: Function,
    mnemonicValidator: Function,
    isSubmitting: boolean,
    isCertificateSelected: boolean,
    isCertificateEncrypted: boolean,
    showInputsForDecryptingForceVendedCertificate: boolean,
    showPassPhraseWidget: boolean,
    isCertificateInvalid: boolean,
    redemptionCode: ?string,
    error: ?LocalizableError,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { walletId, shieldedRedemptionKey } = form.values();
        this.props.onSubmit({ walletId, shieldedRedemptionKey });
      },
      onError: (/* form */) => {
        // console.log('FORM ERRORS', form.errors());
      }
    });
  };

  form = new ReactToolboxMobxForm({
    fields: {
      certificate: {
        label: this.context.intl.formatMessage(messages.certificateLabel),
        placeholder: this.context.intl.formatMessage(messages.certificateHint),
        type: 'file',
        bindings: 'ReactToolbox',
      },
      passPhrase: {
        label: this.context.intl.formatMessage(messages.passphraseLabel),
        placeholder: this.context.intl.formatMessage(messages.passphraseHint),
        value: '',
        bindings: 'ReactToolbox',
        validators: [({ field }) => {
          // Don't validate No pass phrase needed when certificate is not encrypted
          if (!this.props.showPassPhraseWidget) return [true];
          // Otherwise check mnemonic
          const passPhrase = field.value;
          if (this.props.mnemonicValidator(passPhrase)) this.props.onPassPhraseChanged(passPhrase);
          return [
            this.props.mnemonicValidator(passPhrase),
            this.context.intl.formatMessage(new InvalidMnemonicError())
          ];
        }]
      },
      redemptionKey: {
        label: this.context.intl.formatMessage(messages.redemptionKeyLabel),
        placeholder: this.context.intl.formatMessage(messages.redemptionKeyHint),
        value: '',
        bindings: 'ReactToolbox',
        validators: ({ field }) => {
          if (this.props.redemptionType === 'paperVended') return [true];
          const value = this.props.redemptionCode ? this.props.redemptionCode : field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.redemptionCodeValidator(value),
            this.context.intl.formatMessage(messages.redemptionKeyError)
          ];
        },
      },
      shieldedRedemptionKey: {
        label: this.context.intl.formatMessage(messages.shieldedRedemptionKeyLabel),
        placeholder: this.context.intl.formatMessage(messages.shieldedRedemptionKeyHint),
        value: '',
        bindings: 'ReactToolbox',
        validators: ({ field }) => {
          if (this.props.redemptionType !== 'paperVended') return [true];
          const value = field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.postVendRedemptionCodeValidator(value),
            this.context.intl.formatMessage(messages.shieldedRedemptionKeyError)
          ];
        },
      },
      walletId: {
        label: this.context.intl.formatMessage(messages.walletSelectLabel),
        value: this.props.wallets[0].value,
        bindings: 'ReactToolbox',
      },
      email: {
        label: this.context.intl.formatMessage(messages.emailLabel),
        placeholder: this.context.intl.formatMessage(messages.emailHint),
        value: '',
        bindings: 'ReactToolbox',
        validators: [({ field }) => {
          if (!this.props.showInputsForDecryptingForceVendedCertificate) return [true];
          const email = field.value;
          if (isEmail(email)) this.props.onEmailChanged(email);
          return [
            isEmail(email),
            this.context.intl.formatMessage(new InvalidEmailError())
          ];
        }]
      },
      adaPasscode: {
        label: this.context.intl.formatMessage(messages.adaPasscodeLabel),
        placeholder: this.context.intl.formatMessage(messages.adaPasscodeHint),
        value: '',
        bindings: 'ReactToolbox',
        validators: [({ field }) => {
          if (!this.props.showInputsForDecryptingForceVendedCertificate) return [true];
          const adaPasscode = field.value;
          if (!isEmpty(adaPasscode)) this.props.onAdaPasscodeChanged(adaPasscode);
          return [
            !isEmpty(adaPasscode),
            this.context.intl.formatMessage(new FieldRequiredError())
          ];
        }],
      },
      adaAmount: {
        label: this.context.intl.formatMessage(messages.adaAmountLabel),
        placeholder: this.context.intl.formatMessage(messages.adaAmountHint),
        value: '',
        bindings: 'ReactToolbox',
        validators: [({ field }) => {
          if (!this.props.showInputsForDecryptingForceVendedCertificate) return [true];
          const adaAmount = field.value;
          if (!isEmpty(adaAmount)) this.props.onAdaAmountChanged(adaAmount);
          return [
            !isEmpty(adaAmount),
            this.context.intl.formatMessage(new FieldRequiredError())
          ];
        }],
      },
    }
  }, {
    options: {
      showErrorsOnChange: true,
      validateOnChange: true,
      validationDebounceWait: 250,
      validationDebounceOptions: { trailing: true, },
    },
  });

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      wallets, isCertificateSelected, isCertificateEncrypted,
      isSubmitting, onCertificateSelected, redemptionCode,
      onRedemptionCodeChanged, onRemoveCertificate, onChooseRedemptionType,
      isCertificateInvalid, redemptionType, showInputsForDecryptingForceVendedCertificate,
      showPassPhraseWidget, error
    } = this.props;
    const certificate = form.$('certificate');
    const passPhrase = form.$('passPhrase');
    const redemptionKeyField = form.$('redemptionKey');
    const shieldedRedemptionKeyField = form.$('shieldedRedemptionKey');
    const walletId = form.$('walletId');
    const emailField = form.$('email');
    const adaPasscodeField = form.$('adaPasscode');
    const adaAmountField = form.$('adaAmount');
    const componentClasses = classnames([
      styles.component,
      isSubmitting ? styles.isSubmitting : null
    ]);

    const showUploadWidget = redemptionType !== 'paperVended';
    let canSubmit = false;
    if (redemptionType === 'regular' && redemptionCode !== '') canSubmit = true;
    if (redemptionType === 'forceVended' && redemptionCode !== '') canSubmit = true;
    if (
      redemptionType === 'paperVended' &&
      shieldedRedemptionKeyField.isValid && shieldedRedemptionKeyField.isDirty &&
      passPhrase.isValid && passPhrase.isDirty
    ) canSubmit = true;

    let instructionMessage = '';
    switch (redemptionType) {
      case 'regular':
        instructionMessage = messages.instructionsRegular;
        break;
      case 'forceVended':
        instructionMessage = messages.instructionsForceVended;
        break;
      case 'paperVended':
        instructionMessage = messages.instructionsPaperVended;
        break;
      default:
        instructionMessage = messages.instructionsRegular;
    }

    return (
      <div className={componentClasses}>

        <BorderedBox>

          <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

          <h2 className={styles.disclaimer}>{intl.formatMessage(messages.disclaimerTitle)}</h2>
          <div className={styles.disclaimer}>{intl.formatMessage(messages.disclaimer)}</div>

          <AdaRedemptionChoices
            activeChoice={redemptionType}
            onSelectChoice={(choice: string) => {
              redemptionKeyField.resetValidation();
              onChooseRedemptionType(choice);
            }}
          />

          <div className={styles.instructions}>
            <FormattedHTMLMessage {...instructionMessage} />
          </div>

          <div className={styles.redemption}>
            <div className={styles.inputs}>

              {redemptionType !== 'paperVended' ? (
                <Input
                  className="redemption-key"
                  {...redemptionKeyField.bind()}
                  value={redemptionCode}
                  onChange={(value) => {
                    onRedemptionCodeChanged(value);
                    redemptionKeyField.onChange(value);
                  }}
                  disabled={isCertificateSelected}
                />
              ) : (
                <Input
                  className="shielded-redemption-key"
                  {...shieldedRedemptionKeyField.bind()}
                  disabled={isCertificateSelected}
                />
              )}

              <Dropup
                className="wallet"
                source={wallets}
                {...walletId.bind()}
              />

            </div>

            {showUploadWidget ? (
              <div className={styles.certificate}>
                <div className={styles.certificate}>
                  <AdaCertificateUploadWidget
                    {...certificate.bind()}
                    selectedFile={certificate.value}
                    onFileSelected={(file) => {
                      onCertificateSelected(file);
                      certificate.onChange(file);
                    }}
                    isCertificateEncrypted={isCertificateEncrypted}
                    isCertificateSelected={isCertificateSelected}
                    isCertificateInvalid={isCertificateInvalid}
                    onRemoveCertificate={onRemoveCertificate}
                  />
                </div>
              </div>
            ) : null}
          </div>

          {showPassPhraseWidget ? (
            <div className={styles.passPhrase}>
              <Input
                className="pass-phrase"
                {...passPhrase.bind()}
              />
            </div>
          ) : null}

          {showInputsForDecryptingForceVendedCertificate ? (
            <div className={styles.email}>
              <Input
                className="email"
                {...emailField.bind()}
              />
            </div>
          ) : null}

          {showInputsForDecryptingForceVendedCertificate ? (
            <div className={styles.adaPasscode}>
              <Input
                className="ada-passcode"
                {...adaPasscodeField.bind()}
              />
            </div>
          ) : null}

          {showInputsForDecryptingForceVendedCertificate ? (
            <div className={styles.adaAmount}>
              <Input
                className="ada-amount"
                {...adaAmountField.bind()}
              />
            </div>
          ) : null}

          {error ? <p className={styles.error}>{intl.formatMessage(error)}</p> : null}

          <Button
            className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
            label={intl.formatMessage(messages.submitLabel)}
            onMouseUp={this.submit}
            primary
            disabled={!canSubmit}
          />

        </BorderedBox>

      </div>
    );
  }

}
