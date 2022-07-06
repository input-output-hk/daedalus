import React, { useMemo, useCallback, useState } from 'react';
import { inject, observer } from 'mobx-react';
import compose from 'lodash/fp/compose';
import { getNonZeroAssetTokens } from '../../../../utils/assets';
import { ContainerProps as Props, SubmitPayload } from './types';
import { WalletSendConfirmationDialogView } from './SendConfirmation.view';

export function Containter({
  actions,
  stores,
  amount,
  selectedAssets,
  assetsAmounts,
  receiver,
  totalAmount,
  onExternalLinkClick,
  transactionFee,
  hwDeviceStatus,
  isHardwareWallet,
  formattedTotalAmount,
}: Props) {
  const { isFlight } = global;
  const {
    assets: { getAsset },
    wallets: { sendMoneyRequest, active: activeWallet },
    hardwareWallets: {
      _resetTransaction: resetHardwareWalletTransaction,
      sendMoneyRequest: sendMoneyExternalRequest,
      isTransactionPending,
      checkIsTrezorByWalletId,
      initiateTransaction,
    },
  } = stores;
  const {
    assets: { onCopyAssetParam },
    dialogs: { closeActiveDialog },
    hardwareWallets: { sendMoney: hardwareWalletsSendMoney },
    wallets: { sendMoney: walletsSendMoney },
  } = actions;

  if (!activeWallet) {
    throw new Error('Active wallet required for WalletSendPage.');
  }

  const [areTermsAccepted, setAreTermsAccepted] = useState(false);
  const assetTokens = useMemo(
    () => getNonZeroAssetTokens(activeWallet.assets.total, getAsset),
    [activeWallet.assets.total, getAsset]
  );
  const isSubmitting =
    (!isHardwareWallet && sendMoneyRequest.isExecuting) ||
    (isHardwareWallet &&
      (sendMoneyExternalRequest.isExecuting || isTransactionPending));

  const error = isHardwareWallet
    ? sendMoneyExternalRequest.error
    : sendMoneyRequest.error;

  const onSubmitCb = useCallback(
    (values: SubmitPayload) => {
      if (values.isHardwareWallet) {
        hardwareWalletsSendMoney.trigger();
      } else {
        walletsSendMoney.trigger(values);
      }
    },
    [hardwareWalletsSendMoney, walletsSendMoney]
  );

  const onTermsCheckboxClick = useCallback(
    (areTermsAccepted: boolean) => {
      setAreTermsAccepted(areTermsAccepted);

      if (isHardwareWallet) {
        initiateTransaction({
          walletId: activeWallet.id,
        });
      }
    },
    [isHardwareWallet, initiateTransaction, setAreTermsAccepted]
  );

  const onCancel = useCallback(() => {
    closeActiveDialog.trigger();
    sendMoneyRequest.reset();
    resetHardwareWalletTransaction({
      cancelDeviceAction: true,
    });
  }, [sendMoneyRequest, closeActiveDialog, resetHardwareWalletTransaction]);

  return (
    <WalletSendConfirmationDialogView
      amount={amount}
      assetTokens={assetTokens}
      assetsAmounts={assetsAmounts}
      selectedAssets={selectedAssets}
      areTermsAccepted={areTermsAccepted}
      receiver={receiver}
      hwDeviceStatus={hwDeviceStatus}
      error={error}
      onCopyAssetParam={onCopyAssetParam.trigger}
      formattedTotalAmount={formattedTotalAmount}
      totalAmount={totalAmount}
      transactionFee={transactionFee}
      wallet={activeWallet}
      isFlight={isFlight}
      isSubmitting={isSubmitting}
      isHardwareWallet={isHardwareWallet}
      isTrezor={checkIsTrezorByWalletId(activeWallet.id)}
      onCancel={onCancel}
      onSubmitCb={onSubmitCb}
      onTermsCheckboxClick={onTermsCheckboxClick}
      onExternalLinkClick={onExternalLinkClick}
    />
  );
}

export const WalletSendConfirmationDialogContainer = compose(
  inject('actions', 'stores'),
  observer
)(Containter) as React.ComponentType<Omit<Props, 'actions' | 'stores'>>;
