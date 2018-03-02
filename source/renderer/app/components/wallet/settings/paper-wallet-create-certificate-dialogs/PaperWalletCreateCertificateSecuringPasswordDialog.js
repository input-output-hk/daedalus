// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import paperWalletImage from '../../../../assets/images/paper-wallet.png';
import globalMessages from '../../../../i18n/global-messages';

import styles from './PaperWalletCreateCertificateSecuringPasswordDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.headline',
    defaultMessage: '!!!Certificate password',
    description: 'Headline for the "Paper wallet create certificate securyng password dialog".'
  },
  infoLabel1: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.infoLabel1',
    defaultMessage: '!!!To restore your wallet you will need shielded recovery phrase from the certificate and the password.',
    description: '"Paper wallet create certificate securyng password dialog" first info label.'
  },
  infoLabel2: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.infoLabel2',
    defaultMessage: '!!!The password can optionally be written on the certificate or kept securely in other location. Here is the placeholder on the certificate intended for your password.',
    description: '"Paper wallet create certificate securyng password dialog" second info label.'
  },
  securingPasswordConfirmation: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.securingPasswordConfirmation',
    defaultMessage: '!!!I understand that I can not use my certificate without the password and I have stored it safely.',
    description: '"Paper wallet create certificate securyng password dialog" secure password confirmation.'
  }
});

type State = {
  securePasswordConfirmed: boolean,
};

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
};

@observer
// eslint-disable-next-line
export default class PaperWalletCreateCertificateSecuringPasswordDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    securePasswordConfirmed: false
  }

  render() {
    const { intl } = this.context;
    const { onClose, onBack, onContinue } = this.props;
    const { securePasswordConfirmed } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'PaperWalletCreateCertificateSecuringPasswordDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        disabled: !securePasswordConfirmed,
        onClick: onContinue,
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >

        <div className={styles.securingPasswordContentWrapper}>
          <div className={styles.subtitle}>
            <p>{intl.formatMessage(messages.infoLabel1)}</p>
            <p>{intl.formatMessage(messages.infoLabel2)}</p>
          </div>
          <div className={styles.content}>

            <div className={styles.recoveryPhrase}>
              flugenheimer
            </div>

            <div className={styles.paperWalletImageWrapper}>
              <img src={paperWalletImage} role="presentation" />
            </div>

            <Checkbox
              className={styles.securingPasswordConfirmation}
              label={intl.formatMessage(messages.securingPasswordConfirmation)}
              onChange={this.onSecurePasswordConfirmationChange.bind(this)}
              checked={securePasswordConfirmed}
              skin={<SimpleCheckboxSkin />}
            />
          </div>
        </div>

      </Dialog>
    );
  }

  onSecurePasswordConfirmationChange = () => {
    this.setState({ securePasswordConfirmed: !this.state.securePasswordConfirmed });
  }
}
