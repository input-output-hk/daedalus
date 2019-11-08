// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../widgets/Dialog';
import styles from './CancelTransactionConfirmationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'cancel.transaction.confirmation.dialog.headline',
    defaultMessage: '!!!Confirm transaction cancellation?',
    description:
      'Headline for the pending transaction cancellation confirmation dialog.',
  },
  content: {
    id: 'cancel.transaction.confirmation.dialog.content',
    defaultMessage:
      '!!!This transaction was submitted to the Cardano network. However, this transaction may still be processed successfully, and if that happens, it will appear in the list of transactions in this wallet.',
    description:
      'Content for the pending transaction cancellation confirmation dialog.',
  },
  cancelButtonLabel: {
    id: 'cancel.transaction.confirmation.dialog.button.backLabel',
    defaultMessage: '!!!No, keep the transaction pending',
    description:
      '"Cancel" button label for the pending transaction cancellation confirmation dialog.',
  },
  confirmButtonLabel: {
    id: 'cancel.transaction.confirmation.dialog.button.confirmLabel',
    defaultMessage: '!!!Yes, cancel the transaction',
    description:
      '"Confirm" button label for the pending transaction cancellation confirmation dialog.',
  },
});

type Props = {
  onConfirm: Function,
  onCancel: Function,
};

@observer
export default class CancelTransactionConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onConfirm, onCancel } = this.props;

    const dialogClasses = classnames([styles.component, 'ConfirmDialog']);

    const confirmButtonClasses = classnames([
      'confirmButton',
      'attention',
      styles.confirmButton,
    ]);

    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: onCancel,
      },
      {
        className: confirmButtonClasses,
        label: intl.formatMessage(messages.confirmButtonLabel),
        primary: true,
        onClick: onConfirm,
      },
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick={false}
        onClose={onCancel}
      >
        <p>{intl.formatMessage(messages.content)}</p>
      </Dialog>
    );
  }
}
