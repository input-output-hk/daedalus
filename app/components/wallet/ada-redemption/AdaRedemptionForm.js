// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { isEmail, isEmpty } from 'validator';
import classnames from 'classnames';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import Button from 'react-toolbox/lib/button/Button';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import AdaCertificateUploadWidget from '../../widgets/forms/AdaCertificateUploadWidget';
import AdaRedemptionChoices from './AdaRedemptionChoices';
import BorderedBox from '../../widgets/BorderedBox';
import LocalizableError from '../../../i18n/LocalizableError';
import { InvalidMnemonicError, InvalidEmailError } from '../../../i18n/errors';
import { isValidMnemonic } from '../../../../lib/decrypt';
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
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class AdaRedemptionForm extends Component {

  static propTypes = {
    wallets: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.string.isRequired,
      label: PropTypes.string.isRequired,
    })).isRequired,
    onChooseRedemptionType: PropTypes.func.isRequired,
    onCertificateSelected: PropTypes.func.isRequired,
    onRemoveCertificate: PropTypes.func.isRequired,
    onPassPhraseChanged: PropTypes.func.isRequired,
    onRedemptionCodeChanged: PropTypes.func.isRequired,
    onSubmit: PropTypes.func.isRequired,
    redemptionType: PropTypes.string.isRequired,
    redemptionCodeValidator: PropTypes.func.isRequired,
    isSubmitting: PropTypes.bool.isRequired,
    isCertificateSelected: PropTypes.bool.isRequired,
    isCertificateEncrypted: PropTypes.bool.isRequired,
    isCertificateInvalid: PropTypes.bool,
    redemptionCode: PropTypes.string,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { walletId } = form.values();
        this.props.onSubmit({ walletId });
      },
      onError: () => {}
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
        validate: [({ field }) => {
          // Don't validate No pass phrase needed when certificate is not encrypted
          if (!this.props.isCertificateEncrypted) return [true];
          // Otherwise check mnemonic
          const passPhrase = field.value;
          if (isValidMnemonic(passPhrase)) this.props.onPassPhraseChanged(passPhrase);
          return [
            isValidMnemonic(passPhrase),
            this.context.intl.formatMessage(new InvalidMnemonicError())
          ];
        }]
      },
      redemptionKey: {
        label: this.context.intl.formatMessage(messages.redemptionKeyLabel),
        placeholder: this.context.intl.formatMessage(messages.redemptionKeyHint),
        value: '',
        bindings: 'ReactToolbox',
        validate: ({ field }) => {
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
        validate: ({ field }) => {
          const value = this.props.redemptionCode ? this.props.redemptionCode : field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.redemptionCodeValidator(value),
            this.context.intl.formatMessage(messages.shie)
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
        validate: [({ field }) => {
          return [
            isEmail(field.value),
            this.context.intl.formatMessage(new InvalidEmailError())
          ];
        }]
      },
      adaPasscode: {
        label: this.context.intl.formatMessage(messages.adaPasscodeLabel),
        placeholder: this.context.intl.formatMessage(messages.adaPasscodeHint),
        value: '',
        bindings: 'ReactToolbox',
        validate: [({ field }) => {
          if (field.value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
        }]
      },
      adaAmount: {
        label: this.context.intl.formatMessage(messages.adaAmountLabel),
        placeholder: this.context.intl.formatMessage(messages.adaAmountHint),
        value: '',
        bindings: 'ReactToolbox',
        validate: [({ field }) => {
          if (field.value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
        }]
      },
    }
  }, {
    options: {
      validateOnChange: false,
    }
  });

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      wallets, isCertificateSelected, isCertificateEncrypted,
      isSubmitting, onCertificateSelected, redemptionCode,
      onRedemptionCodeChanged, onRemoveCertificate, onChooseRedemptionType,
      isCertificateInvalid, redemptionType, error
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
    const showPassPhraseWidget = isCertificateSelected && isCertificateEncrypted && redemptionType === 'regular' ||
      redemptionType === 'paperVended';
    const showInputsForDecryptingForceVendedCertificate = isCertificateSelected && isCertificateEncrypted && redemptionType === 'forceVended';
    const showUploadWidget = redemptionType !== 'paperVended';
    const canSubmit = redemptionCode !== '';
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

          <AdaRedemptionChoices
            activeChoice={redemptionType}
            onSelectChoice={onChooseRedemptionType}
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
                  value={redemptionCode}
                  onChange={(value) => {
                    onRedemptionCodeChanged(value);
                    redemptionKeyField.onChange(value);
                  }}
                  disabled={isCertificateSelected}
                />
              )}

              <Dropdown
                className="wallet"
                source={wallets}
                {...walletId.bind()}
              />

            </div>
            {showUploadWidget && (
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
            )}
          </div>

          {showPassPhraseWidget && (
            <div className={styles.passPhrase}>
              <Input
                className="pass-phrase"
                {...passPhrase.bind({
                  onBlur: event => {
                    event.preventDefault();
                    passPhrase.onBlur();
                    passPhrase.validate();
                  }
                })}
              />
            </div>
          )}

          {showInputsForDecryptingForceVendedCertificate && (
            <div className={styles.email}>
              <Input
                className="email"
                {...emailField.bind({
                  onBlur: event => {
                    event.preventDefault();
                    passPhrase.onBlur();
                    passPhrase.validate();
                  }
                })}
              />
            </div>
          )}

          {showInputsForDecryptingForceVendedCertificate && (
            <div className={styles.email}>
              <Input
                className="email"
                {...adaPasscodeField.bind({
                  onBlur: event => {
                    event.preventDefault();
                    passPhrase.onBlur();
                    passPhrase.validate();
                  }
                })}
              />
            </div>
          )}

          {showInputsForDecryptingForceVendedCertificate && (
            <div className={styles.email}>
              <Input
                className="email"
                {...adaAmountField.bind({
                  onBlur: event => {
                    event.preventDefault();
                    passPhrase.onBlur();
                    passPhrase.validate();
                  }
                })}
              />
            </div>
          )}

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

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
