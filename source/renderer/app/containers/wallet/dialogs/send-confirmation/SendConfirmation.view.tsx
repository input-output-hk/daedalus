import React from 'react';
import compose from 'lodash/fp/compose';
import { observer } from 'mobx-react';
import { FormattedHTMLMessage, injectIntl } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { DialogContentWithAssets } from './DialogContentWithAssets';
import { DialogContentWithoutAssets } from './DialogContentWithoutAssets';
import { ConfirmationError } from './Error';
import { PasswordInput } from './PasswordInput';
import { messages } from './messages';
import Dialog from '../../../../components/widgets/Dialog';
import DialogCloseButton from '../../../../components/widgets/DialogCloseButton';
import { shouldShowEmptyWalletWarning } from '../../../../utils/walletUtils';
import { hasTokensLeftAfterTransaction } from '../../../../utils/assets';
import LoadingSpinner from '../../../../components/widgets/LoadingSpinner';
import { ViewProps as Props } from './types';
import { useForm } from './hooks';
import {
  didHWTxVerificationFailed,
  doTermsNeedAcceptance,
  isNotEnoughFundsForTokenError,
  isPasswordValid,
} from './helpers';
import styles from './styles.scss';

function View({
  intl,
  amount,
  areTermsAccepted,
  selectedAssets,
  assetsAmounts,
  receiver,
  totalAmount,
  transactionFee,
  hwDeviceStatus,
  formattedTotalAmount,
  wallet,
  assetTokens,
  error,
  isFlight,
  isTrezor,
  isSubmitting,
  isHardwareWallet,
  onExternalLinkClick,
  onCancel,
  onSubmitCb,
  onTermsCheckboxClick,
  onCopyAssetParam,
}: Props) {
  const {
    passphraseField,
    flightCandidateCheckboxField,
    onSubmit,
    handleSubmitOnEnter,
  } = useForm({
    intl,
    error,
    amount,
    assetTokens,
    assetsAmounts,
    receiver,
    selectedAssets,
    isHardwareWallet,
    onSubmitCb,
  });
  const isSendingAssets = !!selectedAssets.length;

  const buttonLabel = !isSubmitting ? (
    intl.formatMessage(messages.sendButtonLabel)
  ) : (
    <LoadingSpinner />
  );

  const dialogActions = [
    {
      label: intl.formatMessage(messages.backButtonLabel),
      onClick: !isSubmitting ? onCancel : () => {},
    },
    {
      label: buttonLabel,
      onClick: onSubmit,
      primary: true,
      className: 'confirmButton',
      disabled:
        isNotEnoughFundsForTokenError(error?.id) ||
        didHWTxVerificationFailed({ isHardwareWallet, hwDeviceStatus }) ||
        doTermsNeedAcceptance({ areTermsAccepted, isFlight }) ||
        !isPasswordValid({
          isHardwareWallet,
          isValid: passphraseField.isValid,
        }),
    },
  ];

  return (
    <Dialog
      title={intl.formatMessage(messages.dialogTitle)}
      subtitle={wallet.name}
      actions={dialogActions}
      closeOnOverlayClick
      primaryButtonAutoFocus
      onClose={!isSubmitting ? onCancel : () => {}}
      className={styles.root}
      closeButton={<DialogCloseButton />}
    >
      {shouldShowEmptyWalletWarning(
        totalAmount,
        wallet,
        !!assetTokens?.length &&
          assetTokens.length > 0 &&
          hasTokensLeftAfterTransaction(
            assetTokens,
            selectedAssets,
            assetsAmounts
          )
      ) && (
        <div className={styles.warning}>
          <FormattedHTMLMessage {...messages.emptyingWarning} tagName="p" />
        </div>
      )}
      {isSendingAssets ? (
        <DialogContentWithAssets
          amount={amount}
          formattedTotalAmount={formattedTotalAmount}
          receiver={receiver}
          transactionFee={transactionFee}
          selectedAssets={selectedAssets}
          assetsAmounts={assetsAmounts}
          isHardwareWallet={isHardwareWallet}
          onCopyAssetParam={onCopyAssetParam}
        />
      ) : (
        <DialogContentWithoutAssets
          amount={amount}
          receiver={receiver}
          transactionFee={transactionFee}
          formattedTotalAmount={formattedTotalAmount}
        />
      )}
      {isFlight && (
        <div className={styles.flightCandidateWarning}>
          <FormattedHTMLMessage
            {...messages.flightCandidateWarning}
            tagName="p"
          />
          <Checkbox
            {...flightCandidateCheckboxField.bind()}
            error={flightCandidateCheckboxField.error}
            skin={CheckboxSkin}
            disabled={areTermsAccepted}
            onChange={onTermsCheckboxClick}
            checked={areTermsAccepted}
          />
        </div>
      )}
      <PasswordInput
        isFlight={isFlight}
        isTrezor={isTrezor}
        isHardwareWallet={isHardwareWallet}
        areTermsAccepted={areTermsAccepted}
        hwDeviceStatus={hwDeviceStatus}
        handleSubmitOnEnter={handleSubmitOnEnter}
        onExternalLinkClick={onExternalLinkClick}
        walletName={wallet.name}
        passphraseField={passphraseField}
      />
      <ConfirmationError
        error={error}
        onExternalLinkClick={onExternalLinkClick}
      />
    </Dialog>
  );
}

export const WalletSendConfirmationDialogView = compose(
  injectIntl,
  observer
)(View) as React.ComponentType<Omit<Props, 'intl'>>;
