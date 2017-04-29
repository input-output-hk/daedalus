// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../widgets/DialogCloseButton';
import BigButtonForDialogs from '../widgets/BigButtonForDialogs';
import styles from './WalletAddDialog.scss';
import createIcon from '../../assets/images/create-ic.svg';
import importIcon from '../../assets/images/import-ic.svg';
import joinSharedIcon from '../../assets/images/join-shared-ic.svg';
import restoreIcon from '../../assets/images/restore-ic.svg';

const messages = defineMessages({
  title: {
    id: 'wallet.add.dialog.title.label',
    defaultMessage: '!!!Add wallet',
    description: 'Label for the "Add wallet" title on the wallet add dialog.'
  },
  createLabel: {
    id: 'wallet.add.dialog.create.label',
    defaultMessage: '!!!Create',
    description: 'Label for the "Create" button on the wallet add dialog.'
  },
  createDescription: {
    id: 'wallet.add.dialog.create.description',
    defaultMessage: '!!!Create a new wallet',
    description: 'Description for the "Create" button on the wallet add dialog.'
  },
  joinLabel: {
    id: 'wallet.add.dialog.join.label',
    defaultMessage: '!!!Join',
    description: 'Label for the "Join" button on the wallet add dialog.'
  },
  joinDescription: {
    id: 'wallet.add.dialog.join.description',
    defaultMessage: '!!!Join a shared wallet with up to 5 people',
    description: 'Description for the "Join" button on the wallet add dialog.'
  },
  restoreLabel: {
    id: 'wallet.add.dialog.restore.label',
    defaultMessage: '!!!Restore',
    description: 'Label for the "Restore" button on the wallet add dialog.'
  },
  restoreDescription: {
    id: 'wallet.add.dialog.restore.description',
    defaultMessage: '!!!Restore wallet from backup',
    description: 'Description for the "Restore" button on the wallet add dialog.'
  },
  importLabel: {
    id: 'wallet.add.dialog.import.label',
    defaultMessage: '!!!Import',
    description: 'Label for the "Import" button on the wallet add dialog.'
  },
  importDescription: {
    id: 'wallet.add.dialog.import.description',
    defaultMessage: '!!!Import wallet from a key',
    description: 'Description for the "Import" button on the wallet add dialog.'
  }
});

@observer
export default class WalletAddDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  props: {
    onCreate: Function,
    onRestore: Function,
    onCancel: Function,
    onImportKey: Function,
    canClose: boolean,
  };

  render() {
    const { intl } = this.context;
    const { onCreate, onRestore, onCancel, canClose, onImportKey } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'WalletAddDialog',
    ]);
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.title)}
        onOverlayClick={canClose && onCancel}
        active
      >
        <div className={styles.buttonsContainer}>
          <div className={styles.firstRow}>
            <BigButtonForDialogs
              className="createWalletButton"
              onClick={onCreate}
              icon={createIcon}
              label={intl.formatMessage(messages.createLabel)}
              description={intl.formatMessage(messages.createDescription)}
            />
            <BigButtonForDialogs
              className="joinWalletButton"
              icon={joinSharedIcon}
              label={intl.formatMessage(messages.joinLabel)}
              description={intl.formatMessage(messages.joinDescription)}
              isDisabled
            />
          </div>
          <div className={styles.secondRow}>
            <BigButtonForDialogs
              className="restoreWalletButton"
              onClick={onRestore}
              icon={restoreIcon}
              label={intl.formatMessage(messages.restoreLabel)}
              description={intl.formatMessage(messages.restoreDescription)}
            />
            <BigButtonForDialogs
              className="importWalletButton"
              onClick={onImportKey}
              icon={importIcon}
              label={intl.formatMessage(messages.importLabel)}
              description={intl.formatMessage(messages.importDescription)}
            />
          </div>
        </div>
        {canClose && (
          <DialogCloseButton onClose={onCancel} />
        )}
      </Dialog>
    );
  }

}
