// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classnames from 'classnames';
import Button from 'react-toolbox/lib/button/Button';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import DropUp from '../../widgets/forms/Dropup';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import AdaCertificateUploadWidget from '../../widgets/forms/AdaCertificateUploadWidget';
import LocalizableError from '../../../i18n/LocalizableError';
import { InvalidMnemonicError } from '../../../i18n/global-errors';
import { isValidMnemonic } from '../../../../lib/decrypt';
import globalMessages from '../../../i18n/global-messages';
import styles from './AdaRedemptionForm.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.redeem.dialog.headline',
    defaultMessage: '!!!Ada Redemption',
    description: 'headline "Ada redemption" dialog.'
  },
  instructions: {
    id: 'wallet.redeem.dialog.instructions',
    defaultMessage: '!!!Detailed instructions',
    description: 'headline "Ada redemption" dialog.'
  },
  certificateLabel: {
    id: 'wallet.redeem.dialog.certificateLabel',
    defaultMessage: '!!!Certificate',
    description: 'Label for the certificate file upload'
  },
  certificateHint: {
    id: 'wallet.redeem.dialog.certificateHint',
    defaultMessage: '!!!Drop file here or click to choose',
    description: 'Hint for the certificate file upload'
  },
  walletSelectLabel: {
    id: 'wallet.redeem.dialog.walletSelectLabel',
    defaultMessage: '!!!Choose Wallet',
    description: 'Label for the walletId select'
  },
  passphraseLabel: {
    id: 'wallet.redeem.dialog.passphraseLabel',
    defaultMessage: '!!!Redeem token',
    description: 'Label for the token input'
  },
  passphraseHint: {
    id: 'wallet.redeem.dialog.passphraseHint',
    defaultMessage: '!!!Enter your 9 word mnemonic here',
    description: 'Hint for the token input'
  },
  redemptionCodeLabel: {
    id: 'wallet.redeem.dialog.redemptionCodeLabel',
    defaultMessage: '!!!Redemption Code',
    description: 'Label for ada redemption code input',
  },
  redemptionCodeError: {
    id: 'wallet.redeem.dialog.redemptionCodeError',
    defaultMessage: '!!!Invalid Redemption Code',
    description: 'Error "Invalid Redemption Code" for ada redemption code input',
  },
  redemptionCodeHint: {
    id: 'wallet.redeem.dialog.redemptionCodeHint',
    defaultMessage: '!!!Enter your code or upload a certificate',
    description: 'Hint for ada redemption code input',
  },
  submitLabel: {
    id: 'wallet.redeem.dialog.submitLabel',
    defaultMessage: '!!!Redeem your money',
    description: 'Label for the "Ada redemption" dialog submit button.'
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
    onCertificateSelected: PropTypes.func.isRequired,
    onRemoveCertificate: PropTypes.func.isRequired,
    onPassPhraseChanged: PropTypes.func.isRequired,
    onRedemptionCodeChanged: PropTypes.func.isRequired,
    onSubmit: PropTypes.func.isRequired,
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
      redemptionCode: {
        label: this.context.intl.formatMessage(messages.redemptionCodeLabel),
        placeholder: this.context.intl.formatMessage(messages.redemptionCodeHint),
        value: '',
        bindings: 'ReactToolbox',
        validate: ({ field }) => {
          const value = this.props.redemptionCode ? this.props.redemptionCode : field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.redemptionCodeValidator(value),
            this.context.intl.formatMessage(messages.redemptionCodeError)
          ];
        },
      },
      walletId: {
        label: this.context.intl.formatMessage(messages.walletSelectLabel),
        value: this.props.wallets[0].value,
        bindings: 'ReactToolbox',
      }
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
      onRedemptionCodeChanged, onRemoveCertificate,
      isCertificateInvalid, error
    } = this.props;
    const certificate = form.$('certificate');
    const passPhrase = form.$('passPhrase');
    const redemptionCodeField = form.$('redemptionCode');
    const walletId = form.$('walletId');
    const componentClasses = classnames([
      styles.component,
      isSubmitting ? styles.isSubmitting : null
    ]);
    const showPassPhraseWidget = isCertificateSelected && isCertificateEncrypted;
    const canSubmit = redemptionCode !== '';

    return (
      <div className={componentClasses}>

        <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

        <div className={styles.instructions}>
          <FormattedHTMLMessage {...messages.instructions} />
        </div>

        <div className={styles.redemption}>
          <div className={styles.inputs}>

            <Input
              className="redemption-code"
              {...redemptionCodeField.bind()}
              value={redemptionCode}
              onChange={(value) => {
                onRedemptionCodeChanged(value);
                redemptionCodeField.onChange(value);
              }}
              disabled={isCertificateSelected}
            />

            <DropUp
              className="wallet"
              source={wallets}
              {...walletId.bind()}
            />

          </div>
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

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        <Button
          className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
          label={intl.formatMessage(messages.submitLabel)}
          onMouseUp={this.submit}
          primary
          disabled={!canSubmit}
        />

      </div>
    );
  }

}
