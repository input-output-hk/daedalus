// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Input } from 'react-polymorph/lib/components/Input';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './DeleteWalletConfirmationDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import { DELETE_WALLET_COUNTDOWN } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';
import LoadingSpinner from '../../widgets/LoadingSpinner';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.delete.dialog.title',
    defaultMessage: '!!!Delete Wallet',
    description: 'Title for the "Delete wallet" dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.delete.dialog.confirmButtonLabel',
    defaultMessage: '!!!Delete',
    description:
      'Label for the "Delete (x)" button in the delete wallet dialog.',
  },
  wantToDeleteWalletQuestion: {
    id: 'wallet.settings.delete.dialog.wantToDeleteWalletQuestion',
    defaultMessage:
      '!!!Do you really want to delete <strong>{walletName}</strong> wallet?',
    description: 'Question if the user really wants to delete the wallet.',
  },
  confirmBackupNotice: {
    id: 'wallet.settings.delete.dialog.confirmBackupNotice',
    defaultMessage:
      '!!!Make sure you have access to backup before continuing. Otherwise, you will lose all your funds connected to this wallet.',
    description:
      'Notice to confirm if the user has made a backup of his wallet',
  },
  enterRecoveryWordLabel: {
    id: 'wallet.settings.delete.dialog.enterRecoveryWordLabel',
    defaultMessage: '!!!Enter the name of the wallet to confirm deletion:',
    description: 'Instruction for recovery word on delete wallet dialog',
  },
});

type Props = {
  walletName: string,
  countdownFn: Function,
  isBackupNoticeAccepted: boolean,
  confirmationValue: string,
  onAcceptBackupNotice: Function,
  onContinue: Function,
  onCancel: Function,
  onConfirmationValueChange: Function,
  isSubmitting: boolean,
  isTest: boolean,
};

@observer
export default class DeleteWalletConfirmationDialog extends Component<Props> {
  static defaultProps = {
    isBackupNoticeAccepted: false,
    confirmationValue: '',
    isTest: false,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      countdownFn,
      isBackupNoticeAccepted,
      onAcceptBackupNotice,
      onCancel,
      onContinue,
      walletName,
      confirmationValue,
      onConfirmationValueChange,
      isSubmitting,
      isTest,
    } = this.props;

    const countdownRemaining = countdownFn(
      isTest ? 0 : DELETE_WALLET_COUNTDOWN
    );
    const countdownDisplay =
      countdownRemaining > 0 ? ` (${countdownRemaining})` : '';
    const isCountdownFinished = countdownRemaining <= 0;
    const isWalletNameConfirmationCorrect =
      confirmationValue.normalize('NFKC') === walletName.normalize('NFKC'); // Always normalize non-breaking space into regular space.
    const isDisabled =
      !isCountdownFinished ||
      !isBackupNoticeAccepted ||
      !isWalletNameConfirmationCorrect;
    const handleSubmit = () => !isDisabled && onContinue();

    const buttonClasses = classnames([
      'attention',
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.confirmButtonLabel) + countdownDisplay
    ) : (
      <LoadingSpinner />
    );

    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onCancel,
      },
      {
        className: buttonClasses,
        label: buttonLabel,
        onClick: onContinue,
        disabled: isDisabled,
        primary: true,
      },
    ];

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancel}
        className={styles.dialog}
        closeButton={<DialogCloseButton onClose={onCancel} />}
      >
        <FormattedHTMLMessage
          tagName="p"
          {...messages.wantToDeleteWalletQuestion}
          values={{ walletName }}
        />
        <Checkbox
          label={intl.formatMessage(messages.confirmBackupNotice)}
          onChange={onAcceptBackupNotice}
          checked={isBackupNoticeAccepted}
          skin={CheckboxSkin}
        />
        {isBackupNoticeAccepted ? (
          <Input
            className={styles.confirmationInput}
            label={intl.formatMessage(messages.enterRecoveryWordLabel)}
            value={confirmationValue}
            // eslint-disable-next-line react/jsx-no-bind
            onKeyPress={submitOnEnter.bind(this, handleSubmit)}
            onChange={onConfirmationValueChange}
            skin={InputSkin}
          />
        ) : null}
      </Dialog>
    );
  }
}
