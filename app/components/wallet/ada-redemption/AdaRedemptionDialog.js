// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import Dropup from '../../widgets/forms/Dropup';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import MobxReactForm from 'mobx-react-form';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import styles from './AdaRedemptionDialog.scss';

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
    description: 'Label for the wallet select'
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

const wallets = [
  { value: 'wallet-1', label: 'First Wallet' },
];

@observer
export default class AdaRedemptionDialog extends Component {

  static propTypes = {
    onSubmit: PropTypes.func.isRequired,
    onCancel: PropTypes.func.isRequired,
    onCertificateSelected: PropTypes.func.isRequired,
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
        value: '',
      },
      token: {
        value: '',
      },
      wallet: {
        value: wallets[0].value,
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
    const { onCancel, onCertificateSelected } = this.props;
    const certificate = validator.$('certificate');
    const token = validator.$('token');
    const wallet = validator.$('wallet');
    const dialogClasses = classnames([
      styles.component,
      this.state.isSubmitting ? styles.isSubmitting : null
    ]);
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={this.actions}
        onOverlayClick={onCancel}
        active
      >

        <div className={styles.certificate}>
          <FileUploadWidget
            label={intl.formatMessage(messages.certificateLabel)}
            hint={intl.formatMessage(messages.certificateHint)}
            onFileSelected={onCertificateSelected}
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

        <Dropup
          className="wallet"
          label={intl.formatMessage(messages.walletSelectLabel)}
          value={wallet.value}
          onChange={wallet.onChange}
          onFocus={wallet.onFocus}
          onBlur={wallet.onBlur}
          error={wallet.error}
          source={wallets}
        />

        <DialogCloseButton onClose={onCancel} />

      </Dialog>
    );
  }

}
