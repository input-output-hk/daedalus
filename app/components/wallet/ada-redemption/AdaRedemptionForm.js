// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classnames from 'classnames';
import Input from 'react-toolbox/lib/input/Input';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import Button from 'react-toolbox/lib/button/Button';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
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
    defaultMessage: '!!!Enter your 9 word mnemonic passphrase here',
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
    onSubmit: PropTypes.func.isRequired,
    isCertificateUploaded: PropTypes.bool.isRequired,
    isCertificateEncrypted: PropTypes.bool.isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isSubmitting: false
  };

  validator = new MobxReactForm({
    options: {
      validateOnChange: false
    },
    fields: {
      certificate: {
        value: null,
      },
      passphrase: {
        value: '',
      },
      walletId: {
        value: this.props.wallets[0].value,
      }
    }
  });

  submit = () => {
    this.validator.submit({
      onSuccess: (form) => {
        this.setState({ isSubmitting: true });
        this.props.onSubmit(form.values());
      },
      onError: () => {
        this.setState({ isSubmitting: false });
      }
    });
  };

  render() {
    const { intl } = this.context;
    const { validator } = this;
    const { wallets, isCertificateUploaded, isCertificateEncrypted } = this.props;
    const certificate = validator.$('certificate');
    const passphrase = validator.$('passphrase');
    const walletId = validator.$('walletId');
    const componentClasses = classnames([
      styles.component,
      this.state.isSubmitting ? styles.isSubmitting : null
    ]);
    const showTokenInput = isCertificateUploaded && isCertificateEncrypted;
    const canSubmit = isCertificateUploaded && walletId.value &&
      (isCertificateEncrypted && passphrase.value || !isCertificateEncrypted);

    return (
      <div className={componentClasses}>

        <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

        <div className={styles.certificate}>
          <FileUploadWidget
            label={intl.formatMessage(messages.certificateLabel)}
            hint={intl.formatMessage(messages.certificateHint)}
            value={certificate.value}
            error={certificate.error}
            onFileSelected={certificate.onChange}
          />
        </div>

        {showTokenInput && (
          <Input
            className="passphrase"
            label={intl.formatMessage(messages.passphraseLabel)}
            hint={intl.formatMessage(messages.passphraseHint)}
            value={passphrase.value}
            onChange={passphrase.onChange}
            onFocus={passphrase.onFocus}
            onBlur={passphrase.onBlur}
            multiline
            rows={3}
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

        <Button
          className={this.state.isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
          label={intl.formatMessage(messages.submitLabel)}
          onMouseUp={this.submit}
          primary
          disabled={!canSubmit}
        />

      </div>
    );
  }

}
