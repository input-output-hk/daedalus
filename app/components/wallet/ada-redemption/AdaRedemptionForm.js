// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classnames from 'classnames';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import Button from 'react-toolbox/lib/button/Button';
import MnemonicInputWidget from '../../widgets/forms/MnemonicInputWidget';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import LocalizableError from '../../../i18n/LocalizableError';
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
    onSubmit: PropTypes.func.isRequired,
    isSubmitting: PropTypes.bool.isRequired,
    isCertificateSelected: PropTypes.bool.isRequired,
    isCertificateEncrypted: PropTypes.bool.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  validator = new MobxReactForm({
    options: {
      validateOnChange: false
    },
    fields: {
      certificate: {
        value: null,
      },
      passPhraseTokens: {
        value: ['', '', '', '', '', '', '', '', ''] // 9 tokens
      },
      walletId: {
        value: this.props.wallets[0].value,
      }
    }
  });

  submit = () => {
    this.validator.submit({
      onSuccess: (form) => {
        this.props.onSubmit(form.values());
      },
      onError: () => {}
    });
  };

  onPassphraseTokenChanged = (index, value) => {
    const passPhraseTokens = this.validator.$('passPhraseTokens');
    const newValues = passPhraseTokens.value.slice();
    newValues[index] = value;
    passPhraseTokens.onChange(newValues);
  };

  render() {
    const { intl } = this.context;
    const { validator } = this;
    const {
      wallets, isCertificateSelected, isCertificateEncrypted, isSubmitting, onCertificateSelected, error
    } = this.props;
    const certificate = validator.$('certificate');
    const passPhraseTokens = validator.$('passPhraseTokens');
    const walletId = validator.$('walletId');
    const componentClasses = classnames([
      styles.component,
      isSubmitting ? styles.isSubmitting : null
    ]);
    const showPassPhraseWidget = isCertificateSelected && isCertificateEncrypted;
    const canSubmit = isCertificateSelected && walletId.value &&
      (isCertificateEncrypted && passPhraseTokens.value || !isCertificateEncrypted);

    return (
      <div className={componentClasses}>

        <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

        <div className={styles.certificate}>
          <FileUploadWidget
            label={intl.formatMessage(messages.certificateLabel)}
            hint={intl.formatMessage(messages.certificateHint)}
            value={certificate.value}
            error={certificate.error}
            onFileSelected={(file) => {
              onCertificateSelected(file);
              certificate.onChange(file);
            }}
          />
        </div>

        {showPassPhraseWidget && (
          <MnemonicInputWidget
            label={intl.formatMessage(messages.passphraseLabel)}
            tokens={passPhraseTokens.value}
            onTokenChanged={this.onPassphraseTokenChanged}
          />
        )}

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
