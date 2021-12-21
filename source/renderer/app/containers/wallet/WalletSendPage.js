// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import {
  DECIMAL_PLACES_IN_ADA,
  MAX_INTEGER_PLACES_IN_ADA,
} from '../../config/numbersConfig';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';
import Asset from '../../domains/Asset';
import type { ApiTokens } from '../../api/assets/types';
import { getNonZeroAssetTokens } from '../../utils/assets';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSendPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  calculateTransactionFee = async (params: {
    walletId: string,
    address: string,
    amount: number,
    isHardwareWallet: boolean,
    selectedAssets?: ApiTokens,
  }) => {
    const {
      walletId,
      address,
      amount,
      isHardwareWallet,
      selectedAssets,
    } = params;
    let fee;
    let minimumAda;
    if (isHardwareWallet) {
      const coinsSelection = await this.props.stores.hardwareWallets.selectCoins(
        {
          walletId,
          address,
          amount,
          assets: selectedAssets,
        }
      );
      fee = coinsSelection.fee;
    } else {
      ({
        fee,
        minimumAda,
      } = await this.props.stores.transactions.calculateTransactionFee({
        walletId,
        address,
        amount,
        assets: selectedAssets,
      }));
    }
    return { fee, minimumAda };
  };

  openDialog = (
    dialog: Function,
    isHardwareWallet: boolean,
    walletId: string
  ) => {
    const { isFlight } = global;
    this.props.actions.dialogs.open.trigger({
      dialog,
    });
    if (isHardwareWallet && !isFlight) {
      this.props.stores.hardwareWallets.initiateTransaction({ walletId });
    }
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

    const { isHardwareWallet } = wallet;

    const walletTokens = wallet.assets.total;
    const assetTokens = getNonZeroAssetTokens(walletTokens, getAsset);
    const totalRawAssets = wallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = wallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;

    return (
      <WalletSendForm
        currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
        currentNumberFormat={profile.currentNumberFormat}
        calculateTransactionFee={(
          address: string,
          amount: number,
          selectedAssets: ApiTokens
        ) =>
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
        assets={assetTokens}
        hasAssets={hasAssetsEnabled && hasRawAssets}
        selectedAsset={selectedAsset}
        isLoadingAssets={isLoadingAssets}
        isDialogOpen={uiDialogs.isOpen}
        isRestoreActive={wallet.isRestoring}
        isHardwareWallet={isHardwareWallet}
        hwDeviceStatus={hwDeviceStatus}
        onOpenDialogAction={(params) =>
          this.openDialog(params.dialog, isHardwareWallet, wallet.id)
        }
        onUnsetActiveAsset={unsetActiveAsset.trigger}
        onExternalLinkClick={app.openExternalLink}
        isAddressFromSameWallet={isAddressFromSameWallet}
        tokenFavorites={favorites}
      />
    );
  }
}
