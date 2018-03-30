// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../widgets/Dialog';
import styles from './ConfirmationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.headline',
    defaultMessage: '!!!Are you sure?',
    description: 'Headline for the "Confirmation dialog".'
  },
  content: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.content',
    defaultMessage: '!!!Your certificate should not be used without passing verification steps.',
    description: 'Content for the "Confirmation dialog".'
  },
  cancelButtonLabel: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.button.canceLabel',
    defaultMessage: '!!!Cancel',
    description: '"Confirmation dialog" button cancel label.'
  },
  confirmButtonLabel: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.button.confirmLabel',
    defaultMessage: '!!!Confirm',
    description: '"Confirmation dialog" button confirm label.'
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

    const dialogClasses = classnames([
      styles.component,
      'ConfirmDialog',
    ]);

    const confirmButtonClasses = classnames([
      'confirmButton',
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
