// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type BigNumber from 'bignumber.js';
import { ellipsis } from '../../../utils/strings';
import WalletSendConfirmationDialog from '../../../components/wallet/send-form/WalletSendConfirmationDialog';
import WalletSendAssetsConfirmationDialog from '../../../components/wallet/send-form/WalletSendAssetsConfirmationDialog';
import type { StoresMap } from '../../../stores/index';
import type { ActionsMap } from '../../../actions/index';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import type { AssetToken } from '../../../api/assets/types';
import { getAssetTokens } from '../../../utils/assets';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  amount: string,
  receiver: string,
  selectedAssets: Array<AssetToken>,
  assetsAmounts: Array<string>,
  totalAmount: BigNumber,
  transactionFee: ?string,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  onExternalLinkClick: Function,
  hwDeviceStatus: HwDeviceStatus,
  isHardwareWallet: boolean,
  formattedTotalAmount: string,
};

@inject('actions', 'stores')
@observer
export default class WalletSendConfirmationDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleWalletSendFormSubmit = (values: Object) => {
    if (values.isHardwareWallet) {
      this.props.actions.hardwareWallets.sendMoney.trigger({});
    } else {
      this.props.actions.wallets.sendMoney.trigger(values);
    }
  };

  handleInitiateTransaction = () => {
    const { stores } = this.props;
    const { wallets, hardwareWallets } = stores;
    const { active: activeWallet } = wallets;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');
    hardwareWallets.initiateTransaction({ walletId: activeWallet.id });
  };

  handleOnCopyAssetItem = (assetItem: string, fullValue: string) => {
    const value = ellipsis(fullValue, 15, 15);
    this.props.actions.wallets.copyAssetItem.trigger({
      assetItem,
      value,
    });
  };

  render() {
    const {
      actions,
      amount,
      selectedAssets,
      assetsAmounts,
      receiver,
      totalAmount,
      onExternalLinkClick,
      transactionFee,
      amountToNaturalUnits,
      hwDeviceStatus,
      isHardwareWallet,
      formattedTotalAmount,
      stores,
    } = this.props;

    const {
      assets: assetsStore,
      wallets: { sendMoneyRequest, active: activeWallet },
      hardwareWallets: {
        _resetTransaction: resetHardwareWalletTransaction,
        sendMoneyRequest: sendMoneyExternalRequest,
        isTransactionPending,
        checkIsTrezorByWalletId,
      },
    } = stores;

    const { getAsset } = assetsStore;
    const { isFlight } = global;

    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');

    const isSubmitting =
      (!isHardwareWallet && sendMoneyRequest.isExecuting) ||
      (isHardwareWallet &&
        (sendMoneyExternalRequest.isExecuting || isTransactionPending));

    const error = isHardwareWallet
      ? sendMoneyExternalRequest.error
      : sendMoneyRequest.error;

    const isTrezor = checkIsTrezorByWalletId(activeWallet.id);

    const walletTokens = activeWallet.assets.total;
    const assetTokens = getAssetTokens(walletTokens, getAsset);

    return (
      <>
        {selectedAssets.length ? (
          <WalletSendAssetsConfirmationDialog
            amount={amount}
            sender={activeWallet.id}
            receiver={receiver}
            wallet={activeWallet}
            totalAmount={totalAmount}
            selectedAssets={selectedAssets}
            allAvailableTokens={assetTokens}
            assetsAmounts={assetsAmounts}
            transactionFee={transactionFee}
            amountToNaturalUnits={amountToNaturalUnits}
            onSubmit={this.handleWalletSendFormSubmit}
            isSubmitting={isSubmitting}
            isFlight={isFlight}
            onCancel={() => {
              actions.dialogs.closeActiveDialog.trigger();
              sendMoneyRequest.reset();
              resetHardwareWalletTransaction({ cancelDeviceAction: true });
            }}
            error={error}
            onExternalLinkClick={onExternalLinkClick}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            onInitiateTransaction={this.handleInitiateTransaction}
            onCopyAssetItem={this.handleOnCopyAssetItem}
            isTrezor={isTrezor}
            formattedTotalAmount={formattedTotalAmount}
          />
        ) : (
          <WalletSendConfirmationDialog
            amount={amount}
            receiver={receiver}
            wallet={activeWallet}
            totalAmount={totalAmount}
            allAvailableTokens={assetTokens}
            transactionFee={transactionFee}
            amountToNaturalUnits={amountToNaturalUnits}
            onSubmit={this.handleWalletSendFormSubmit}
            isSubmitting={isSubmitting}
            isFlight={isFlight}
            onCancel={() => {
              actions.dialogs.closeActiveDialog.trigger();
              sendMoneyRequest.reset();
              resetHardwareWalletTransaction({ cancelDeviceAction: true });
            }}
            error={error}
            onExternalLinkClick={onExternalLinkClick}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            onInitiateTransaction={this.handleInitiateTransaction}
            isTrezor={isTrezor}
            formattedTotalAmount={formattedTotalAmount}
          />
        )}
      </>
    );
  }
}
