// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../widgets/DialogCloseButton';
import BigButtonForDialogs from '../widgets/BigButtonForDialogs';
import styles from './WalletAddDialog.scss';
import createIcon from '../../assets/images/create-ic.svg';
import importIcon from '../../assets/images/import-ic.svg';
import joinSharedIcon from '../../assets/images/join-shared-ic.svg';

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
  importLabel: {
    id: 'wallet.add.dialog.import.label',
    defaultMessage: '!!!Import',
    description: 'Label for the "Import" button on the wallet add dialog.'
  },
  importDescription: {
    id: 'wallet.add.dialog.import.description',
    defaultMessage: '!!!Import existing wallet',
    description: 'Description for the "Import" button on the wallet add dialog.'
  }
});

@observer
export default class WalletAddDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  static propTypes = {
    onCreate: PropTypes.func.isRequired,
    onImport: PropTypes.func.isRequired,
  };


  render() {
    const { intl } = this.context;
    const { onCreate, onImport } = this.props;
    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.title)}
        active
      >
        <div className={styles.buttonsContainer}>
          <div className={styles.firstRow}>
            <BigButtonForDialogs
              onClick={onCreate}
              icon={createIcon}
              label={intl.formatMessage(messages.createLabel)}
              description={intl.formatMessage(messages.createDescription)}
            />
          </div>
          <div className={styles.secondRow}>
            <BigButtonForDialogs
              icon={joinSharedIcon}
              label={intl.formatMessage(messages.joinLabel)}
              description={intl.formatMessage(messages.joinDescription)}
              isDisabled
            />
            <BigButtonForDialogs
              onClick={onImport}
              icon={importIcon}
              label={intl.formatMessage(messages.importLabel)}
              description={intl.formatMessage(messages.importDescription)}
            />
          </div>
        </div>
        <DialogCloseButton onClose={this.props.onCancel} />

      </Dialog>
    );
  }

}
