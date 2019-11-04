// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './TransferFundsStep2Dialog.scss';
import type { DialogAction } from '../../widgets/Dialog';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
});

type Props = {
  actions?: Array<DialogAction>,
  onClose?: Function,
  children: Node,
};

export default class TransferFundsStep2Dialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { actions, children, onClose } = this.props;

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.content}>{children}</div>
      </Dialog>
    );
  }
}
