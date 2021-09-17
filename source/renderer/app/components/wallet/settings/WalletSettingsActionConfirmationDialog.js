// @flow
import React from 'react';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Input } from 'react-polymorph/lib/components/Input';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { FormattedHTMLMessage, injectIntl, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './DeleteWalletConfirmationDialog.scss';
import globalMessages from '../../../i18n/global-messages';
import { DELETE_WALLET_COUNTDOWN } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';
import LoadingSpinner from '../../widgets/LoadingSpinner';
// import type {ReactIntlMessage} from "../../../types/i18nTypes";

export type WalletSettingActionMessages = {
  dialogTitle: Message,
  confirmButtonLabel: Message,
  wantToWalletQuestion: $Exact<Message>,
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
  messages: WalletSettingActionMessages,
  intl: intlShape.isRequired,
};

const WalletSettingsActionConfirmationDialog = (props: Props) => {
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
    messages,
    intl,
  } = props;

  const countdownRemaining = countdownFn(isTest ? 0 : DELETE_WALLET_COUNTDOWN);
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
        {...messages.wantToWalletQuestion}
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
};

export default injectIntl(WalletSettingsActionConfirmationDialog);
