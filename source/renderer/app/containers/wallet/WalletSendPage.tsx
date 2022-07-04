import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import {
  DECIMAL_PLACES_IN_ADA,
  MAX_INTEGER_PLACES_IN_ADA,
} from '../../config/numbersConfig';
import WalletSendForm, {
  FormData,
  ConfirmationDialogData,
} from '../../components/wallet/WalletSendForm';
import { WalletSendConfirmationDialogView } from './dialogs/send-confirmation/SendConfirmation.view';
import WalletTokenPicker from '../../components/wallet/tokens/wallet-token-picker/WalletTokenPicker';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';
import Asset from '../../domains/Asset';
import type { ApiTokens } from '../../api/assets/types';
import { getNonZeroAssetTokens } from '../../utils/assets';
import { CoinSelectionsResponse } from '../../api/transactions/types';

type Props = InjectedProps;
type State = {
  confirmationDialogData: ConfirmationDialogData;
};

@inject('stores', 'actions')
@observer
class WalletSendPage extends Component<Props, State> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  state: State = {
    confirmationDialogData: null,
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
    const { isFlight } = global;

    if (isHardwareWallet) {
      this.props.stores.hardwareWallets.updateTxSignRequest(coinSelection);
    }

    this.props.actions.dialogs.open.trigger({
      dialog: WalletSendConfirmationDialogView,
    });

    if (isHardwareWallet && !isFlight) {
      this.props.stores.hardwareWallets.initiateTransaction({
        walletId,
      });
    }

    this.setState({ confirmationDialogData: { ...data } });
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
        assets={assetTokens}
        hasAssets={hasAssetsEnabled && hasRawAssets}
        selectedAsset={selectedAsset}
        isLoadingAssets={isLoadingAssets}
        isDialogOpen={uiDialogs.isOpen}
        isRestoreActive={wallet.isRestoring}
        isHardwareWallet={isHardwareWallet}
        hwDeviceStatus={hwDeviceStatus}
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
        confirmationDialogData={this.state.confirmationDialogData}
      />
    );
  }
}

export default WalletSendPage;
