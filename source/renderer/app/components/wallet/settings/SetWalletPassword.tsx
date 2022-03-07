import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/insecur... Remove this comment to see the full error message
import insecureWalletIcon from '../../../assets/images/insecure-wallet.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SetWalletPassword.scss' or i... Remove this comment to see the full error message
import styles from './SetWalletPassword.scss';
import ChangeSpendingPasswordDialogContainer from '../../../containers/wallet/dialogs/settings/ChangeSpendingPasswordDialogContainer';

const messages = defineMessages({
  setPasswordButton: {
    id: 'wallet.settings.setWalletPassword.dialog.setPasswordButton',
    defaultMessage: '!!!Set a password',
    description:
      'Label for the "Set a password" button in the set wallet password dialog.',
  },
  setPasswordMessage: {
    id: 'wallet.settings.setWalletPassword.dialog.setPasswordMessage',
    defaultMessage:
      '!!!To keep your wallet secure and start using it in Daedalus, you need to set a spending password.',
    description:
      'Message for the "Set a password" button in the set wallet password dialog.',
  },
  setPasswordTitle: {
    id: 'wallet.settings.setWalletPassword.dialog.setPasswordTitle',
    defaultMessage: '!!!Your wallet is not protected with a password',
    description:
      'Title for the "Set wallet password" dialog when there is not password set.',
  },
});
type Props = {
  isSetWalletPasswordDialogOpen: boolean;
  onSetWalletPassword: (...args: Array<any>) => any;
};

@observer
class SetWalletPassword extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isSetWalletPasswordDialogOpen, onSetWalletPassword } = this.props;
    return (
      <>
        <div className={styles.component}>
          <div className={styles.setPasswordDialog}>
            <div className={styles.setPasswordWrapper}>
              <SVGInline
                svg={insecureWalletIcon}
                className={styles.insecureWalletIcon}
              />
              <h2 className={styles.setPasswordTitle}>
                {intl.formatMessage(messages.setPasswordTitle)}
              </h2>
              <p className={styles.setPasswordMessage}>
                {intl.formatMessage(messages.setPasswordMessage)}
              </p>
              <button
                className={styles.setPasswordButton}
                onClick={onSetWalletPassword}
              >
                {intl.formatMessage(messages.setPasswordButton)}
              </button>
            </div>
          </div>
        </div>

        {isSetWalletPasswordDialogOpen && (
          <ChangeSpendingPasswordDialogContainer />
        )}
      </>
    );
  }
}

export default SetWalletPassword;
