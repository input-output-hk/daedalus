// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import paperWalletImage from '../../../assets/images/paper-wallet-certificate/certificate.png';
import globalMessages from '../../../i18n/global-messages';
import styles from './SecuringPasswordDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.headline',
    defaultMessage: '!!!Complete your certificate',
    description: 'Headline for the "Paper wallet create certificate securyng password dialog".'
  },
  infoLabel1: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.infoLabel1',
    defaultMessage: '!!!To complete your paper wallet certificate you will need to write the remaining 9 words of your paper wallet recovery phrase on your certificate.',
    description: '"Paper wallet create certificate securyng password dialog" first info label.'
  },
  infoLabel2: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.infoLabel2',
    defaultMessage: '!!!The password can optionally be written on the certificate or kept securely in other location. Here is the placeholder on the certificate intended for your password.',
    description: 'You may write the remaining words here:'
  },
  securingPasswordConfirmation: {
    id: 'paper.wallet.create.certificate.securyng password.dialog.securingPasswordConfirmation',
    defaultMessage: '!!!I have written the remaining 9 words on the certificate.',
    description: '"Paper wallet create certificate securyng password dialog" secure password confirmation.'
  }
});

type State = {
  securePasswordConfirmed: boolean,
};

type Props = {
  additionalMnemonics: string,
  onContinue: Function,
};

@observer
export default class SecuringPasswordDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    securePasswordConfirmed: false
  };

  render() {
    const { intl } = this.context;
    const { securePasswordConfirmed } = this.state;
    const { additionalMnemonics, onContinue } = this.props;

    const dialogClasses = classnames([
      styles.component,
      'SecuringPasswordDialog',
    ]);

    const actions = [
      {
        className: 'continueButton',
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
      >

        <div className={styles.securingPasswordContentWrapper}>
          <div className={styles.content}>
            <p className={styles.infoLabel}>{intl.formatMessage(messages.infoLabel1)}</p>

            <div className={styles.recoveryPhrase}>
              { additionalMnemonics }
            </div>

            <p className={styles.infoLabel}>{intl.formatMessage(messages.infoLabel2)}</p>

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
  };
}
