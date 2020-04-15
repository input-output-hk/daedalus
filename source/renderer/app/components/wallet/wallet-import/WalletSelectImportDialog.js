// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import ReactModal from 'react-modal';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import styles from './WalletSelectImportDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import penIcon from '../../../assets/images/pen.inline.svg';
import crossIcon from '../../../assets/images/close-cross.inline.svg';

const messages = defineMessages({
  title: {
    id: 'wallet.select.import.dialog.title',
    defaultMessage: '!!!Found wallets',
    description: 'Select import wallets dialog title',
  },
  description: {
    id: 'wallet.select.import.dialog.description',
    defaultMessage:
      '!!!These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
    description:
      'These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
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
  onSelectStateDirectory: Function,
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
    const { onConfirm, onClose, onSelectStateDirectory } = this.props;

    const wallets = [
      {
        id: 1,
        name: 'Alex Wallet',
        status: 'importing',
      },
      {
        id: 2,
        name: 'Nikola Wallet',
        status: 'alreadyExists',
      },
      {
        id: 3,
        name: 'Sasha Wallet',
        status: 'noPassword',
      },
      {
        id: 4,
        name: 'Yakov Wallet',
        status: 'hasPassword',
      },
    ];

    const title = intl.formatMessage(messages.title);
    const description = intl.formatMessage(messages.description);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const importingStatus = intl.formatMessage(messages.importingWallet);
    const noPasswordStatus = intl.formatMessage(messages.noPassword);
    const hasPasswordStatus = intl.formatMessage(messages.passwordProtected);
    const alreadyExistsStatus = intl.formatMessage(messages.walletExists);
    // const walletImportedStatus = intl.formatMessage(messages.walletImported);
    // const walletNotFoundStatus = intl.formatMessage(messages.notFound);

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
            <div className={styles.title}>{title}</div>
            <div className={styles.description}>{description}</div>
            <hr className={styles.separator} />
            <div className={styles.walletsContainer}>
              {wallets.map(wallet => (
                <>
                  {wallet.status === 'noPassword' && (
                    <hr className={styles.separator} />
                  )}
                  <div className={styles.walletsRow} key={wallet.id}>
                    <div
                      className={styles.walletsCounter}
                    >{`${wallet.id}.`}</div>
                    <div className={styles.walletsInputField}>
                      <Input
                        type="text"
                        className={classNames([
                          styles.walletsInput,
                          wallet.status === 'alreadyExists' ||
                          wallet.status === 'importing'
                            ? styles.walletUnavailable
                            : null,
                          wallet.status === 'noPassword'
                            ? styles.walletNoPassword
                            : null,
                        ])}
                        skin={InputSkin}
                      />
                      {wallet.status === 'noPassword' && (
                        <button
                          className={styles.selectStateDirectoryButton}
                          onClick={onSelectStateDirectory}
                        >
                          <SVGInline svg={penIcon} className={styles.penIcon} />
                        </button>
                      )}
                      {wallet.status === 'hasPassword' && (
                        <button
                          className={styles.selectStateDirectoryButton}
                          onClick={onSelectStateDirectory}
                        >
                          <SVGInline
                            svg={crossIcon}
                            className={styles.crossIcon}
                          />
                        </button>
                      )}
                    </div>
                    <div className={styles.walletsStatus}>
                      {wallet.status === 'importing' && importingStatus}
                      {wallet.status === 'alreadyExists' && alreadyExistsStatus}
                      {wallet.status === 'noPassword' && noPasswordStatus}
                      {wallet.status === 'hasPassword' && hasPasswordStatus}
                    </div>
                    <div className={styles.walletsStatusIcon} />
                  </div>
                </>
              ))}
            </div>
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
