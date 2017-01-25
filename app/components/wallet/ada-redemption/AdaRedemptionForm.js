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
  tokenLabel: {
    id: 'wallet.redeem.dialog.tokenLabel',
    defaultMessage: '!!!Redeem token',
    description: 'Label for the token input'
  },
  tokenHint: {
    id: 'wallet.redeem.dialog.tokenHint',
    defaultMessage: '!!!Type the phrase here',
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
      token: {
        value: '',
      },
      walletId: {
        value: this.props.wallets[0].value,
      }
    }
  });

  actions = [
    {
      label: this.context.intl.formatMessage(messages.submitLabel),
      primary: true,
      onClick: () => this.submit()
    }
  ];

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
    const { wallets } = this.props;
    const certificate = validator.$('certificate');
    const token = validator.$('token');
    const walletId = validator.$('walletId');
    const componentClasses = classnames([
      styles.component,
      this.state.isSubmitting ? styles.isSubmitting : null
    ]);
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
            acceptedFileTypes="application/pdf"
          />
        </div>

        <Input
          className="token"
          label={intl.formatMessage(messages.tokenLabel)}
          hint={intl.formatMessage(messages.tokenHint)}
          value={token.value}
          onChange={token.onChange}
          onFocus={token.onFocus}
          onBlur={token.onBlur}
          multiline
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

        <Button
          className={this.state.isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
          label={intl.formatMessage(messages.submitLabel)}
          onMouseUp={this.submit}
          primary
        />

      </div>
    );
  }

}
