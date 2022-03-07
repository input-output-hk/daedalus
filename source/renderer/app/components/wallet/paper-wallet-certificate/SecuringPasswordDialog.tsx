import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/paper-w... Remove this comment to see the full error message
import paperWalletImage from '../../../assets/images/paper-wallet-certificate/certificate.png';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SecuringPasswordDialog.scss'... Remove this comment to see the full error message
import styles from './SecuringPasswordDialog.scss';
import { PAPER_WALLET_WRITTEN_WORDS_COUNT } from '../../../config/cryptoConfig';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.securingPassword.dialog.headline',
    defaultMessage: '!!!Complete your certificate',
    description:
      'Headline for the "Paper wallet create certificate securing password dialog".',
  },
  infoLabel1: {
    id: 'paper.wallet.create.certificate.securingPassword.dialog.infoLabel1',
    defaultMessage: `!!!To complete your paper wallet certificate you will need to
      write the remaining {paperWalletWrittenWordsCount} words of your paper wallet recovery
      phrase on your certificate.`,
    description:
      '"Paper wallet create certificate securing password dialog" first info label.',
  },
  infoLabel2: {
    id: 'paper.wallet.create.certificate.securingPassword.dialog.infoLabel2',
    defaultMessage:
      '!!!The password can optionally be written on the certificate or kept securely in other location. Here is the placeholder on the certificate intended for your password.',
    description: 'You may write the remaining words here:',
  },
  securingPasswordConfirmation: {
    id:
      'paper.wallet.create.certificate.securingPassword.dialog.securingPasswordConfirmation',
    defaultMessage:
      '!!!I have written the remaining {paperWalletWrittenWordsCount} words on the certificate.',
    description:
      '"Paper wallet create certificate securing password dialog" secure password confirmation.',
  },
});
type State = {
  securePasswordConfirmed: boolean;
};
type Props = {
  additionalMnemonics: string;
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
};

@observer
class SecuringPasswordDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    securePasswordConfirmed: false,
  };
  onSecurePasswordConfirmation = () => {
    this.setState((prevState) => ({
      securePasswordConfirmed: !prevState.securePasswordConfirmed,
    }));
  };

  render() {
    const { intl } = this.context;
    const { securePasswordConfirmed } = this.state;
    const { additionalMnemonics, onContinue, onClose } = this.props;
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
      },
    ];
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.securingPasswordContentWrapper}>
          <div className={styles.content}>
            <p className={styles.infoLabel}>
              {intl.formatMessage(messages.infoLabel1, {
                paperWalletWrittenWordsCount: PAPER_WALLET_WRITTEN_WORDS_COUNT,
              })}
            </p>

            <div className={styles.recoveryPhrase}>{additionalMnemonics}</div>

            <p className={styles.infoLabel}>
              {intl.formatMessage(messages.infoLabel2)}
            </p>

            <div className={styles.paperWalletImageWrapper}>
              <img src={paperWalletImage} role="presentation" />
            </div>

            <Checkbox
              className={styles.securingPasswordConfirmation}
              label={intl.formatMessage(messages.securingPasswordConfirmation, {
                paperWalletWrittenWordsCount: PAPER_WALLET_WRITTEN_WORDS_COUNT,
              })}
              onChange={this.onSecurePasswordConfirmation}
              checked={securePasswordConfirmed}
              skin={CheckboxSkin}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}

export default SecuringPasswordDialog;
