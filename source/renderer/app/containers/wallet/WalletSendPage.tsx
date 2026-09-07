import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import {
  DECIMAL_PLACES_IN_ADA,
  MAX_INTEGER_PLACES_IN_ADA,
} from '../../config/numbersConfig';
import WalletSendForm, {
  FormData,
} from '../../components/wallet/WalletSendForm';
import { WalletSendConfirmationDialogContainer } from './dialogs/send-confirmation/SendConfirmation.container';
import WalletTokenPicker from '../../components/wallet/tokens/wallet-token-picker/WalletTokenPicker';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';
import Asset from '../../domains/Asset';
import type { ApiTokens } from '../../api/assets/types';
import { getNonZeroAssetTokens } from '../../utils/assets';
import {
  withAnalytics,
  WithAnalyticsTrackerProps,
} from '../../components/analytics/withAnalytics';
import { CoinSelectionsResponse } from '../../api/transactions/types';
import { LOVELACES_PER_ADA } from '../../config/numbersConfig';

type Props = InjectedProps & WithAnalyticsTrackerProps;

@inject('stores', 'actions')
@observer
class WalletSendPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  calculateTransactionFee = async (params: {
    walletId: string;
    address: string;
    amount: number;
    isHardwareWallet: boolean;
    selectedAssets?: ApiTokens;
  }) => {
    const {
      walletId,
      address,
      amount,
      isHardwareWallet,
      selectedAssets,
    } = params;

    if (isHardwareWallet) {
      const coinSelection: CoinSelectionsResponse = await this.props.stores.hardwareWallets.selectCoins(
        {
          walletId,
          address,
          amount,
          assets: selectedAssets,
        }
      );
      return {
        fee: coinSelection.fee,
        coinSelection,
      };
    }

    const {
      fee,
      minimumAda,
    } = await this.props.stores.transactions.calculateTransactionFee({
      walletId,
      address,
      amount,
      assets: selectedAssets,
    });

    return {
      fee,
      minimumAda,
    };
  };

  submit = (
    isHardwareWallet: boolean,
    walletId: string,
    { coinSelection, ...data }: FormData
  ) => {
    const wallet = this.props.stores.wallets.getWalletById(walletId);
    if (!wallet) throw new Error('Wallet required before sending.');
    if (wallet.isLegacy) {
      this.props.actions.dialogs.open.trigger({
        dialog: WalletSendConfirmationDialogContainer,
        props: {
          amount: data.amount.toFixed(),
          selectedAssets: data.selectedAssets,
          assetsAmounts: data.assetsAmounts,
          receiver: data.receiver,
          totalAmount: data.totalAmount,
          transactionFee: data.transactionFee.toFixed(),
          hwDeviceStatus: this.props.stores.hardwareWallets.hwDeviceStatus,
          isHardwareWallet,
          formattedTotalAmount: data.totalAmount.toFixed(),
          onExternalLinkClick: this.props.stores.app.openExternalLink,
        },
      });
      return;
    }
    const amount = data.amount.times(LOVELACES_PER_ADA).toFixed(0);
    const hasAssetsRemainingAfterTransaction = data.selectedAssets.length
      ? !(
          data.selectedAssets.length === wallet.assets.total.length &&
          data.selectedAssets.every(({ quantity }, index) =>
            quantity.isEqualTo(data.assetsAmounts[index])
          )
        )
      : wallet.assets.total.length > 0;
    if (isHardwareWallet) {
      this.props.stores.hardwareWallets.updateTxSignRequest(
        coinSelection,
        this.props.stores.collateral.preparationFormActive
      );
      this.props.actions.hardwareWallets.sendMoney.trigger();
      return;
    }
    this.props.actions.wallets.sendMoney.trigger({
      receiver: data.receiver,
      amount,
      assets: data.selectedAssets,
      assetsAmounts: data.assetsAmounts,
      hasAssetsRemainingAfterTransaction,
      isCollateralPreparation: data.isCollateralPreparation,
    });
  };

  openTokenPickerDialog = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletTokenPicker,
    });
  };

  closeTokenPickerDialog = () => {
    const { actions, stores } = this.props;
    if (!stores.uiDialogs.isOpen(WalletTokenPicker)) return;
    actions.dialogs.closeActiveDialog.trigger();
  };

  getAssetByUniqueId = (uniqueId: string, allAssets: Array<Asset>) => {
    return allAssets.find((asset) => asset.uniqueId === uniqueId);
  };

  render() {
    const { stores, actions } = this.props;
    const {
      uiDialogs,
      wallets,
      transactions,
      app,
      profile,
      hardwareWallets,
      assets: assetsStore,
      addresses,
      collateral,
    } = stores;
    const { isValidAddress, isAddressFromSameWallet } = wallets;
    const { validateAmount, validateAssetAmount } = transactions;
    const { hwDeviceStatus } = hardwareWallets;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    const { all: allAssets, activeAsset, getAsset, favorites } = assetsStore;
    const { unsetActiveAsset } = actions.wallets;
    const selectedAsset = activeAsset
      ? this.getAssetByUniqueId(activeAsset, allAssets)
      : null;
    // Guard against potential null values
    const wallet = wallets.active;
    if (!wallet) throw new Error('Active wallet required for WalletSendPage.');
    const { isHardwareWallet, name: walletName } = wallet;
    const walletTokens = wallet.assets.total;
    const assetTokens = getNonZeroAssetTokens(walletTokens, getAsset);
    const totalRawAssets = wallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = wallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;
    const isCollateralPreparation = collateral.preparationFormActive;
    const preparationAddress = addresses.active?.id ?? addresses.all[0]?.id;
    return (
      <WalletSendForm
        currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
        currentNumberFormat={profile.currentNumberFormat}
        calculateTransactionFee={(address, amount, selectedAssets) =>
          this.calculateTransactionFee({
            walletId: wallet.id,
            address,
            amount,
            isHardwareWallet,
            selectedAssets,
          })
        }
        walletAmount={wallet.amount}
        validateAmount={validateAmount}
        validateAssetAmount={validateAssetAmount}
        addressValidator={isValidAddress}
        assets={isCollateralPreparation ? [] : assetTokens}
        hasAssets={!isCollateralPreparation && hasAssetsEnabled && hasRawAssets}
        selectedAsset={isCollateralPreparation ? null : selectedAsset}
        isLoadingAssets={isLoadingAssets}
        isDialogOpen={uiDialogs.isOpen}
        isRestoreActive={wallet.isRestoring}
        onSubmit={(data: FormData) =>
          this.submit(isHardwareWallet, wallet.id, data)
        }
        onUnsetActiveAsset={unsetActiveAsset.trigger}
        onExternalLinkClick={app.openExternalLink}
        isAddressFromSameWallet={isAddressFromSameWallet}
        tokenFavorites={favorites}
        walletName={walletName}
        onTokenPickerDialogOpen={this.openTokenPickerDialog}
        onTokenPickerDialogClose={this.closeTokenPickerDialog}
        analyticsTracker={this.props.analyticsTracker}
        initialReceiver={
          isCollateralPreparation ? preparationAddress : undefined
        }
        initialAmount={isCollateralPreparation ? '5' : undefined}
        isCollateralPreparation={isCollateralPreparation}
      />
    );
  }
}

export default withAnalytics(WalletSendPage);
