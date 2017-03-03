// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classnames from 'classnames';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import Button from 'react-toolbox/lib/button/Button';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape } from 'react-intl';
import CustomMobxReactForm from '../../../lib/CustomMobxReactForm';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import LocalizableError from '../../../i18n/LocalizableError';
import { InvalidMnemonicError } from '../../../i18n/global-errors';
import { isValidMnemonic } from '../../../../lib/decrypt';
import styles from './AdaRedemptionForm.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.redeem.dialog.headline',
    defaultMessage: '!!!Ada Redemption',
    description: 'headline "Ada redemption" dialog.'
  },
  certificateLabel: {
    id: 'wallet.redeem.dialog.certificateLabel',
    defaultMessage: '!!!Upload your certificate',
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
    defaultMessage: '!!!Enter your 9 word mnemonic passPhraseTokens here',
    description: 'Hint for the token input'
  },
  redemptionCodeLabel: {
    id: 'wallet.redeem.dialog.redemptionCodeLabel',
    defaultMessage: '!!!Your Redemption Code',
    description: 'Label for ada redemption code input',
  },
  redemptionCodeHint: {
    id: 'wallet.redeem.dialog.redemptionCodeHint',
    defaultMessage: '!!!Enter your code manually here or upload a certificate above',
    description: 'Hint for ada redemption code input',
  },
  submitLabel: {
    id: 'wallet.redeem.dialog.submitLabel',
    defaultMessage: '!!!Redeem your money',
    description: 'Label for the "Ada redemption" dialog submit button.'
  },
});

@observer
export default class AdaRedemptionForm extends Component {

  static propTypes = {
    wallets: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.string.isRequired,
      label: PropTypes.string.isRequired,
    })).isRequired,
    onCertificateSelected: PropTypes.func.isRequired,
    onPassPhraseChanged: PropTypes.func.isRequired,
    onRedemptionCodeChanged: PropTypes.func.isRequired,
    onSubmit: PropTypes.func.isRequired,
    isSubmitting: PropTypes.bool.isRequired,
    isCertificateSelected: PropTypes.bool.isRequired,
    isCertificateEncrypted: PropTypes.bool.isRequired,
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

  form = new CustomMobxReactForm({
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
      onRedemptionCodeChanged, error
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

        <div className={styles.certificate}>
          <FileUploadWidget
            {...certificate.bind()}
            selectedFile={certificate.value}
            onFileSelected={(file) => {
              onCertificateSelected(file);
              certificate.onChange(file);
            }}
          />
        </div>

        {showPassPhraseWidget && (
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
        )}

        <Input
          className="redemption-code"
          {...redemptionCodeField.bind()}
          value={redemptionCode}
          onChange={(value) => {
            onRedemptionCodeChanged(value);
            redemptionCodeField.onChange(value);
          }}
        />

        <Dropdown
          className="wallet"
          source={wallets}
          {...walletId.bind()}
        />

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
