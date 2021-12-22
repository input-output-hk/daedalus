import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import WalletCreateSteps from './WalletCreateSteps';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletCreateDialog.scss' or ... Remove this comment to see the full error message
import styles from './WalletCreateDialog.scss';
import type { DialogActionItems } from '../../widgets/Dialog';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
});
type Props = {
  stepNumber: number;
  actions?: DialogActionItems;
  onClose?: (...args: Array<any>) => any;
  children: Node;
};
export default class WalletCreateDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { actions, children, stepNumber, onClose } = this.props;
    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <WalletCreateSteps stepNumber={stepNumber} />
        <div className={styles.content}>{children}</div>
      </Dialog>
    );
  }
}
