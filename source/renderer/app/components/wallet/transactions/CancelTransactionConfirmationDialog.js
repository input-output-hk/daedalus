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
  content1: {
    id: 'cancel.transaction.confirmation.dialog.content1',
    defaultMessage:
      '!!!This transaction was submitted to the Cardano network some time ago, but hasn’t been finalized yet. You can try to cancel the transaction now to release the pending funds, but there is a chance that the transaction will be finalized regardless. In this case, the transaction will reappear in your wallet as a completed transaction.',
    description:
      'Content for the pending transaction cancellation confirmation dialog.',
  },
  content2: {
    id: 'cancel.transaction.confirmation.dialog.content2',
    defaultMessage:
      '!!!To ensure that this transfer of funds is processed as soon as possible, we recommend that you cancel this transaction and submit a new one to the network.',
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
        <p>{intl.formatMessage(messages.content1)}</p>
        <p>
          <strong>{intl.formatMessage(messages.content2)}</strong>
        </p>
      </Dialog>
    );
  }
}
