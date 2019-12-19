// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import WalletRestoreSteps from './WalletRestoreSteps';
import styles from './WalletRestoreDialog.scss';
import type { DialogAction } from '../../widgets/Dialog';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.restore.dialog.title',
    defaultMessage: '!!!Restore a wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
  dialogTitleSuccess: {
    id: 'wallet.restore.dialog.titleSuccess',
    defaultMessage: '!!!Restore a wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
});

type Props = {
  stepNumber?: number,
  actions?: Array<DialogAction>,
  onClose?: Function,
  children: Node,
};

export default class WalletRestoreDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { actions, children, stepNumber, onClose } = this.props;
    const hasStep = stepNumber !== undefined;
    const title = hasStep
      ? intl.formatMessage(messages.dialogTitle)
      : intl.formatMessage(messages.dialogTitleSuccess);

    return (
      <Dialog
        className={styles.component}
        title={title}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        {hasStep && <WalletRestoreSteps stepNumber={stepNumber || 0} />}
        {children}
      </Dialog>
    );
  }
}
