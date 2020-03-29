// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../../widgets/Dialog';
import styles from './ConfirmationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.restore.dialog.confirmation.headline',
    defaultMessage: '!!!Are you sure?',
    description:
      'Headline for the wallet restoration cancellation confirmation dialog.',
  },
  content: {
    id: 'wallet.restore.dialog.confirmation.content',
    defaultMessage:
      '!!!You haven’t submitted this information yet. If you close the window now, you will lose your progress and have to start again.',
    description:
      'Content for the wallet restoration cancellation confirmation dialog.',
  },
  cancelButtonLabel: {
    id: 'wallet.restore.dialog.confirmation.button.cancelButtonLabel',
    defaultMessage: '!!!Back to wallet restoration',
    description:
      '"Cancel" button label for the wallet restoration cancellation confirmation dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.restore.dialog.confirmation.button.confirmButtonLabel',
    defaultMessage: '!!!Close window',
    description:
      '"Abort" button label for the wallet restoration cancellation confirmation dialog.',
  },
});

type Props = {
  onConfirm: Function,
  onCancel: Function,
};

@observer
export default class ConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onConfirm, onCancel } = this.props;

    const dialogClasses = classnames([styles.component, 'ConfirmDialog']);

    const confirmButtonClasses = classnames([
      'confirmButton',
      // 'attention',
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
