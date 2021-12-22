import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ConfirmationDialog.scss' or ... Remove this comment to see the full error message
import styles from './ConfirmationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.headline',
    defaultMessage: '!!!Abort paper wallet certificate creation?',
    description:
      'Headline for the paper wallet certificate cancellation confirmation dialog.',
  },
  content1: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.contentPart1',
    defaultMessage:
      '!!!At this point, your paper wallet certificate is not complete and verifications steps are not yet done.',
    description:
      'Content for the paper wallet certificate cancellation confirmation dialog.',
  },
  content2: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.contentPart2',
    defaultMessage:
      '!!!At this point, your paper wallet certificate is not complete and verifications steps are not yet done.',
    description:
      'Content for the paper wallet certificate cancellation confirmation dialog.',
  },
  cancelButtonLabel: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.button.backLabel',
    defaultMessage: '!!!Back',
    description:
      '"Cancel" button label for the paper wallet certificate cancellation confirmation dialog.',
  },
  confirmButtonLabel: {
    id: 'paper.wallet.create.certificate.confirmation.dialog.button.abortLabel',
    defaultMessage: '!!!Abort',
    description:
      '"Abort" button label for the paper wallet certificate cancellation confirmation dialog.',
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

export default ConfirmationDialog;
