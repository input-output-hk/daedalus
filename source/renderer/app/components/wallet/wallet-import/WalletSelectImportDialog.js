// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import ReactModal from 'react-modal';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import styles from './WalletSelectImportDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';

const messages = defineMessages({
  title: {
    id: 'wallet.select.import.dialog.title',
    defaultMessage: '!!!Found wallets',
    description: 'Select import wallets dialog title',
  },
  description: {
    id: 'wallet.select.import.dialog.description',
    defaultMessage: '!!!These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
    description: 'These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
  },
  passwordProtected: {
    id: 'wallet.select.import.dialog.passwordProtected',
    defaultMessage: '!!!Password protected',
    description: 'Password protected',
  },
  walletExists: {
    id: 'wallet.select.import.dialog.walletExists',
    defaultMessage: '!!!Wallet already exists',
    description: 'Wallet already exists',
  },
  noPassword: {
    id: 'wallet.select.import.dialog.noPassword',
    defaultMessage: '!!!No password',
    description: 'No password',
  },
  importingWallet: {
    id: 'wallet.select.import.dialog.importingWallet',
    defaultMessage: '!!!Importing wallet...',
    description: 'Importing wallet...',
  },
  walletName: {
    id: 'wallet.select.import.dialog.walletName',
    defaultMessage: '!!!Enter wallet name',
    description: 'Enter wallet name',
  },
  notFound: {
    id: 'wallet.select.import.dialog.notFound',
    defaultMessage: '!!!Name not found',
    description: 'Name not found',
  },
  walletImported: {
    id: 'wallet.select.import.dialog.walletImported',
    defaultMessage: '!!!Wallet imported',
    description: 'Wallet imported',
  },
  buttonLabel: {
    id: 'wallet.select.import.dialog.buttonLabel',
    defaultMessage: '!!!Import selected wallets',
    description: 'Import selected wallets',
  },
});

type Props = {
  onConfirm: Function,
  onClose: Function,
};

export default class WalletSelectImportDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  confirm = () => {
    this.props.onConfirm();
  };

  render() {
    const { intl } = this.context;
    const { onConfirm, onClose } = this.props;

    const buttonLabel = intl.formatMessage(messages.buttonLabel);

    return (
      <ReactModal
        isOpen
        onRequestClose={onClose}
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={onClose}
          />
          <div className={styles.backgroundContainer} />
          <div className={styles.content}>
            <div className={styles.action}>
              <Button
                className={styles.actionButton}
                label={buttonLabel}
                onClick={onConfirm}
                skin={ButtonSkin}
              />
            </div>
          </div>
        </div>
      </ReactModal>
    );
  }
}
