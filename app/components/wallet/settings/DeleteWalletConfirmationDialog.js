// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import CheckboxWithLongLabel from '../../widgets/forms/CheckboxWithLongLabel';
import styles from './DeleteWalletConfirmationDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import environment from '../../../environment';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.delete.dialog.title',
    defaultMessage: '!!!Delete Wallet',
    description: 'Title for the "Delete wallet" dialog.'
  },
  confirmButtonLabel: {
    id: 'wallet.settings.delete.dialog.confirmButtonLabel',
    defaultMessage: '!!!Delete',
    description: 'Label for the "Delete (x)" button in the delete wallet dialog.',
  },
  wantToDeleteWalletQuestion: {
    id: 'wallet.settings.delete.dialog.wantToDeleteWalletQuestion',
    defaultMessage: '!!!Do you really want to delete <strong>{walletName}</strong> wallet?',
    description: 'Question if the user really wants to delete the wallet.',
  },
  confirmBackupNotice: {
    id: 'wallet.settings.delete.dialog.confirmBackupNotice',
    defaultMessage: '!!!Make sure you have access to backup before continuing. Otherwise, you will lose all your funds connected to this wallet.',
    description: 'Notice to confirm if the user has made a backup of his wallet',
  },
  enterRecoveryWordLabel: {
    id: 'wallet.settings.delete.dialog.enterRecoveryWordLabel',
    defaultMessage: '!!!Enter the name of the wallet to confirm deletion:',
    description: 'Instruction for recovery word on delete wallet dialog',
  }
});

@observer
export default class DeleteWalletConfirmationDialog extends Component {

  props: {
    walletName: string,
    countdownFn: Function,
    isBackupNoticeAccepted: boolean,
    confirmationValue: string,
    onAcceptBackupNotice: Function,
    onContinue: Function,
    onCancel: Function,
    onConfirmationValueChange: Function,
  };

  static defaultProps = {
    isBackupNoticeAccepted: false,
    confirmationValue: '',
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
      onConfirmationValueChange
    } = this.props;

    const countdownRemaining = countdownFn(environment.isTest() ? 0 : 10);
    const countdownDisplay = countdownRemaining > 0 ? ` (${countdownRemaining})` : '';
    const isCountdownFinished = countdownRemaining <= 0;
    const isWalletNameConfirmationCorrect = confirmationValue === walletName;

    const actions = [
      {
        label: intl.formatMessage(globalMessages.cancel),
        onClick: onCancel,
      },
      {
        label: intl.formatMessage(messages.confirmButtonLabel) + countdownDisplay,
        onClick: onContinue,
        disabled: (
          !isCountdownFinished || !isBackupNoticeAccepted || !isWalletNameConfirmationCorrect
        ),
        primary: true
      },
    ];

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        active
        className={styles.dialog}
      >
        <FormattedHTMLMessage
          {...messages.wantToDeleteWalletQuestion}
          values={{ walletName }}
        />
        <CheckboxWithLongLabel
          label={intl.formatMessage(messages.confirmBackupNotice)}
          onChange={onAcceptBackupNotice}
          checked={isBackupNoticeAccepted}
        />
        {isBackupNoticeAccepted ? (
          <Input
            className={styles.confirmationInput}
            label={intl.formatMessage(messages.enterRecoveryWordLabel)}
            value={confirmationValue}
            onChange={onConfirmationValueChange}
          />
        ) : null}
        <DialogCloseButton onClose={onCancel} />
      </Dialog>
    );
  }

}
