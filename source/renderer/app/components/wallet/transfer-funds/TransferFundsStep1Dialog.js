// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './TransferFundsStep1Dialog.scss';
import type { DialogAction } from '../../widgets/Dialog';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.transferFunds.dialog.title',
    defaultMessage: '!!!Redeem funds...',
    description: 'Title  in the redeem funds form.',
  },
});

type Props = {
  actions?: Array<DialogAction>,
  onClose?: Function,
  wallets: Array<any>,
};

export default class TransferFundsStep1Dialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { actions, wallets, onClose } = this.props;

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p>Choose a wallet you would like to transfer your funds.</p>
        <pre>{JSON.stringify(wallets, null, 2)}</pre>
      </Dialog>
    );
  }
}
