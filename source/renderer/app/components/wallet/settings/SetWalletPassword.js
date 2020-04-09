// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import insecureWalletIcon from '../../../assets/images/insecure-wallet.inline.svg';
import styles from './SetWalletPassword.scss';
import ChangeSpendingPasswordDialog from "./ChangeSpendingPasswordDialog";
import ChangeSpendingPasswordDialogContainer
  from "../../../containers/wallet/dialogs/settings/ChangeSpendingPasswordDialogContainer";

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
      '!!!Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.',
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
  onConfirm: Function,
  isDialogOpen: Function,
};

@observer
export default class SetWalletPassword extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isDialogOpen, onConfirm } = this.props;

    return (
      <>
        {isDialogOpen(ChangeSpendingPasswordDialog) ? (
          <ChangeSpendingPasswordDialogContainer forceSetPassword />
        ) : (
          false
        )}
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
              <button className={styles.setPasswordButton} onClick={onConfirm}>
                {intl.formatMessage(messages.setPasswordButton)}
              </button>
            </div>
          </div>
        </div>
      </>
    );
  }
}
