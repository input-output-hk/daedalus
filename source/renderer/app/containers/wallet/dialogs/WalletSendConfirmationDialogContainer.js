// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { ellipsis } from '../../../utils/strings';
import type { StoresMap } from '../../../stores/index';
import type { ActionsMap } from '../../../actions/index';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import WalletSendConfirmationDialog from '../../../components/wallet/WalletSendConfirmationDialog';
import WalletAssetsSendConfirmationDialog from '../../../components/wallet/WalletAssetsSendConfirmationDialog';
import type { WalletSummaryAsset } from '../../../api/assets/types';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  amount: string,
  receiver: string,
  assets: Array<WalletSummaryAsset>,
  assetsAmounts: Array<string>,
  totalAmount: ?string,
  transactionFee: ?string,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  currencyUnit: string,
  onExternalLinkClick: Function,
  hwDeviceStatus: HwDeviceStatus,
  isHardwareWallet: boolean,
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
      assets,
      assetsAmounts,
      receiver,
      totalAmount,
      onExternalLinkClick,
      transactionFee,
      amountToNaturalUnits,
      currencyUnit,
      hwDeviceStatus,
      isHardwareWallet,
    } = this.props;
    const { stores } = this.props;
    const { sendMoneyRequest, active: activeWallet } = stores.wallets;
    const {
      _resetTransaction: resetHardwareWalletTransaction,
      sendMoneyRequest: sendMoneyExternalRequest,
      isTransactionPending,
    } = stores.hardwareWallets;
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

    return (
      <>
        {assets.length ? (
          <WalletAssetsSendConfirmationDialog
            amount={amount}
            sender={activeWallet.id}
            receiver={receiver}
            totalAmount={totalAmount}
            assets={assets}
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
            currencyUnit={currencyUnit}
            onExternalLinkClick={onExternalLinkClick}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            onInitiateTransaction={this.handleInitiateTransaction}
            walletName={activeWallet.name}
            onCopyAssetItem={this.handleOnCopyAssetItem}
          />
        ) : (
          <WalletSendConfirmationDialog
            amount={amount}
            receiver={receiver}
            totalAmount={totalAmount}
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
            currencyUnit={currencyUnit}
            onExternalLinkClick={onExternalLinkClick}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            onInitiateTransaction={this.handleInitiateTransaction}
            walletName={activeWallet.name}
          />
        )}
      </>
    );
  }
}
