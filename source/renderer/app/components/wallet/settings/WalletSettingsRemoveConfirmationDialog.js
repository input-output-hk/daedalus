// @flow
import React from 'react';
import classnames from 'classnames';
import { FormattedHTMLMessage, injectIntl, intlShape } from 'react-intl';
import { observer } from 'mobx-react';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './DeleteWalletConfirmationDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import { DELETE_WALLET_COUNTDOWN } from '../../../config/timingConfig';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import DeleteWalletConfirmation from './DeleteWalletConfirmation';

export type WalletSettingRemoveMessages = {
  dialogTitle: Message,
  confirmButtonLabel: Message,
  confirmationQuestion: $Exact<Message>,
  confirmBackupNotice: Message,
  enterRecoveryWordLabel: Message,
};

type Message = {
  id: string,
  defaultMessage: string,
  description: string,
};

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
  isUnpair: boolean,
  messages: WalletSettingRemoveMessages,
  intl: intlShape.isRequired,
};

const WalletSettingsRemoveConfirmationDialog = observer((props: Props) => {
  const {
    countdownFn,
    isBackupNoticeAccepted = false,
    onAcceptBackupNotice,
    onCancel,
    onContinue,
    walletName,
    confirmationValue = '',
    onConfirmationValueChange,
    isSubmitting,
    isTest = false,
    isUnpair = false,
    messages,
    intl,
  } = props;

  const countdownRemaining = countdownFn(isTest ? 0 : DELETE_WALLET_COUNTDOWN);
  const countdownDisplay =
    !isUnpair && countdownRemaining > 0 ? ` (${countdownRemaining})` : '';
  const isCountdownFinished = countdownRemaining <= 0;
  const isWalletNameConfirmationCorrect =
    confirmationValue.normalize('NFKC') === walletName.normalize('NFKC'); // Always normalize non-breaking space into regular space.
  const isDisabled =
    !isUnpair &&
    (!isCountdownFinished ||
      !isBackupNoticeAccepted ||
      !isWalletNameConfirmationCorrect);
  const handleSubmit = React.useCallback(() => !isDisabled && onContinue());

  const buttonClasses = classnames([
    'attention',
    isSubmitting ? styles.isSubmitting : null,
  ]);

  const buttonLabel = !isSubmitting ? (
    `${intl.formatMessage(messages.confirmButtonLabel)} ${countdownDisplay}`
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
        {...messages.confirmationQuestion}
        values={{ walletName }}
      />

      {!isUnpair && (
        <DeleteWalletConfirmation
          isBackupNoticeAccepted={isBackupNoticeAccepted}
          confirmationValue={confirmationValue}
          onAcceptBackupNotice={onAcceptBackupNotice}
          handleSubmit={handleSubmit}
          onConfirmationValueChange={onConfirmationValueChange}
          checkboxLabel={intl.formatMessage(messages.confirmBackupNotice)}
          inputLabel={intl.formatMessage(messages.enterRecoveryWordLabel)}
        />
      )}
    </Dialog>
  );
});

export default injectIntl(WalletSettingsRemoveConfirmationDialog);
