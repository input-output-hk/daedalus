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
    defaultMessage: '!!!Abort Fund3 Voting Registration?',
    description:
      'Headline for the voting registration cancellation confirmation dialog.',
  },
  content: {
    id: 'voting.votingRegistration.dialog.confirmation.content',
    defaultMessage:
      '!!!Are you sure that you want to abort Fund3 voting registration? The transaction fee you paid for the voting registration will go to waste and you will need to start the registration from the beginning if you decide to register for Fund3 voting.',
    description:
      'Content for the voting registration cancellation confirmation dialog.',
  },
  cancelButtonLabel: {
    id:
      'voting.votingRegistration.dialog.confirmation.button.cancelButtonLabel',
    defaultMessage: '!!!Close window',
    description:
      '"Close window" button label for the voting registration cancellation confirmation dialog.',
  },
  cancelButtonLabelVariation: {
    id:
      'voting.votingRegistration.dialog.confirmation.button.cancelButtonLabelVariation',
    defaultMessage: '!!!Close Daedalus',
    description:
      '"Close Daedalus" button label for the voting registration cancellation confirmation dialog.',
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
  isDaedalusClosing: boolean,
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
    const { isDaedalusClosing, onConfirm, onCancel } = this.props;

    const dialogClasses = classnames([styles.component, 'ConfirmDialog']);

    const confirmButtonClasses = classnames([
      'confirmButton',
      // 'attention',
      styles.confirmButton,
    ]);

    const cancelButtonLabel = isDaedalusClosing
      ? intl.formatMessage(messages.cancelButtonLabelVariation)
      : intl.formatMessage(messages.cancelButtonLabel);

    const actions = [
      {
        className: 'cancelButton',
        label: cancelButtonLabel,
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
