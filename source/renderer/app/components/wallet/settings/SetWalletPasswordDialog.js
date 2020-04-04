// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../widgets/Dialog';
import insecureWalletIcon from '../../../assets/images/insecure-wallet.png';
import styles from './SetWalletPasswordDialog.scss';

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
};

@observer
export default class SetWalletPasswordDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    const actions = [
      {
        label: intl.formatMessage(messages.setPasswordButton),
        onClick: this.props.onConfirm(),
        primary: true,
        className: styles.setPasswordButton,
      },
    ];

    return (
      <Dialog actions={actions} className={styles.setPasswordDialog}>
        <div className={styles.setPasswordWrapper}>
          <img
            src={insecureWalletIcon}
            className={styles.insecureWalletIcon}
            role="presentation"
            draggable="false"
          />
          <h2 className={styles.setPasswordTitle}>
            {intl.formatMessage(messages.setPasswordTitle)}
          </h2>
          <p className={styles.setPasswordMessage}>
            {intl.formatMessage(messages.setPasswordMessage)}
          </p>
        </div>
      </Dialog>
    );
  }
}
