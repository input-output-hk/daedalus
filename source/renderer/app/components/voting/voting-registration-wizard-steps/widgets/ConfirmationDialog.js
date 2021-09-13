// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../../widgets/Dialog';
import styles from './ConfirmationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'voting.votingRegistration.dialog.confirmation.headline',
    defaultMessage: '!!!Cancel Fund6 voting registration?',
    description:
      'Headline for the voting registration cancellation confirmation dialog.',
  },
  content: {
    id: 'voting.votingRegistration.dialog.confirmation.content',
    defaultMessage:
      '!!!Are you sure that you want to cancel Fund6 voting registration? The transaction fee you paid for the voting registration transaction will be lost and you will need to repeat the registration from the beginning.',
    description:
      'Content for the voting registration cancellation confirmation dialog.',
  },
  cancelButtonLabel: {
    id:
      'voting.votingRegistration.dialog.confirmation.button.cancelButtonLabel',
    defaultMessage: '!!!Cancel registration',
    description:
      '"Cancel registration" button label for the voting registration cancellation confirmation dialog.',
  },
  confirmButtonLabel: {
    id:
      'voting.votingRegistration.dialog.confirmation.button.confirmButtonLabel',
    defaultMessage: '!!!Continue registration',
    description:
      '"Continue registration" button label for the voting registration cancellation confirmation dialog.',
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
        onClose={onConfirm}
      >
        <p>{intl.formatMessage(messages.content)}</p>
      </Dialog>
    );
  }
}
