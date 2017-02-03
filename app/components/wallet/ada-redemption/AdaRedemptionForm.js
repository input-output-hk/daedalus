// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classnames from 'classnames';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import Button from 'react-toolbox/lib/button/Button';
import Input from 'react-toolbox/lib/input/Input';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
// import MnemonicInputWidget from '../../widgets/forms/MnemonicInputWidget';
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

  state = {
    passPhrase: ''
  };

  onPassPhraseChanged = (passPhrase: string) => {
    this.setState({ passPhrase });
    if (isValidMnemonic(passPhrase)) this.props.onPassPhraseChanged(passPhrase);
  };

  submit = () => {
    this.validator.submit({
      onSuccess: (form) => {
        const { redemptionCode, walletId } = form.values();
        this.props.onSubmit({ redemptionCode, walletId });
      },
      onError: () => {}
    });
  };

  validator = new MobxReactForm({
    fields: {
      certificate: {
        value: null,
      },
      passPhrase: {
        value: '',
        validate: [({ field }) => {
          // Don't validate No pass phrase needed when certificate is not encrypted
          if (!this.props.isCertificateEncrypted) return [true];
          // Otherwise check mnemonic
          return [isValidMnemonic(field.value), this.context.intl.formatMessage(new InvalidMnemonicError())]
        }]
      },
      redemptionCode: {
        value: '',
      },
      walletId: {
        value: this.props.wallets[0].value,
      }
    }
  });

  render() {
    const { intl } = this.context;
    const { validator } = this;
    const {
      wallets, isCertificateSelected, isCertificateEncrypted,
      isSubmitting, onCertificateSelected, redemptionCode,
      onRedemptionCodeChanged, error
    } = this.props;
    const certificate = validator.$('certificate');
    const passPhrase = validator.$('passPhrase');
    const redemptionCodeField = validator.$('redemptionCode');
    const walletId = validator.$('walletId');
    const componentClasses = classnames([
      styles.component,
      isSubmitting ? styles.isSubmitting : null
    ]);
    const showPassPhraseWidget = isCertificateSelected && isCertificateEncrypted;
    const canSubmit = redemptionCode != '';

    return (
      <div className={componentClasses}>

        <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

        <div className={styles.certificate}>
          <FileUploadWidget
            label={intl.formatMessage(messages.certificateLabel)}
            hint={intl.formatMessage(messages.certificateHint)}
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
            label={intl.formatMessage(messages.passphraseLabel)}
            hint={intl.formatMessage(messages.passphraseHint)}
            value={this.state.passPhrase}
            error={passPhrase.error}
            onChange={this.onPassPhraseChanged}
            onFocus={passPhrase.onFocus}
            onBlur={() => {
              passPhrase.onChange(this.state.passPhrase);
              passPhrase.onBlur();
            }}
          />
        )}

        <Input
          className="redemption-code"
          label={intl.formatMessage(messages.redemptionCodeLabel)}
          hint={intl.formatMessage(messages.redemptionCodeHint)}
          value={redemptionCode}
          error={redemptionCodeField.error}
          onChange={(value) => {
            onRedemptionCodeChanged(value);
            redemptionCodeField.onChange(value);
          }}
          onFocus={redemptionCodeField.onFocus}
          onBlur={redemptionCodeField.onBlur}
        />

        <Dropdown
          className="wallet"
          label={intl.formatMessage(messages.walletSelectLabel)}
          value={walletId.value}
          onChange={walletId.onChange}
          onFocus={walletId.onFocus}
          onBlur={walletId.onBlur}
          error={walletId.error}
          source={wallets}
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
