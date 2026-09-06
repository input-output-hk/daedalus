import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ConfirmationDialog.scss' or ... Remove this comment to see the full error message
import styles from './ConfirmationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'voting.votingRegistration.dialog.confirmation.headline',
    defaultMessage: '!!!Cancel voting registration?',
    description:
      'Headline for the voting registration cancellation confirmation dialog.',
  },
  content: {
    id: 'voting.votingRegistration.dialog.confirmation.content',
    defaultMessage:
      '!!!Are you sure you want to cancel your voting registration? You will lose the registration fee, and you will need to restart the process.',
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
  onConfirm: (...args: Array<any>) => any;
  onCancel: (...args: Array<any>) => any;
};

@observer
class ConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onConfirm, onCancel } = this.props;
    const dialogClasses = classnames([styles.component, 'ConfirmDialog']);
    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: onCancel,
      },
      {
        className: 'confirmButton',
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

export default ConfirmationDialog;
