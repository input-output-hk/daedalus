// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import globalMessages from '../../i18n/global-messages';
import {
  DECIMAL_PLACES_IN_ADA,
  MAX_INTEGER_PLACES_IN_ADA,
} from '../../config/numbersConfig';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';
import Asset from '../../domains/Asset';
import type {
  AssetItems,
  WalletTransactionAsset,
} from '../../api/assets/types';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSendPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  calculateTransactionFee = async (params: {
    walletId: string,
    address: string,
    amount: number,
    isHardwareWallet: boolean,
    selectedAssets?: AssetItems,
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

  getAssetByFingerprint = (fingerprint: string, allAssets: Array<Asset>) => {
    return allAssets.find((asset) => asset.fingerprint === fingerprint);
  };

  handleUnsetActiveAssetFingerprint = () => {
    const { wallets: walletActions } = this.props.actions;
    walletActions.setActiveAssetFingerprint.trigger({
      fingerprint: null,
    });
  };

  render() {
    const { intl } = this.context;
    const {
      uiDialogs,
      wallets,
      transactions,
      app,
      profile,
      hardwareWallets,
      assets: assetsStore,
    } = this.props.stores;
    const { isValidAddress } = wallets;
    const { validateAmount } = transactions;
    const { hwDeviceStatus } = hardwareWallets;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    const {
      all: allAssets,
      activeAssetFingerprint,
      getAssetDetails,
    } = assetsStore;

    const selectedAsset = activeAssetFingerprint
      ? this.getAssetByFingerprint(activeAssetFingerprint, allAssets)
      : null;

    // Guard against potential null values
    const wallet = wallets.active;
    if (!wallet) throw new Error('Active wallet required for WalletSendPage.');

    const { isHardwareWallet } = wallet;

    // $FlowFixMe
    const walletAssets: Array<WalletTransactionAsset> = wallet.assets.total
      .map((rawAsset) => {
        const { policyId, assetName } = rawAsset;
        const assetDetails = getAssetDetails(policyId, assetName);
        return assetDetails ? Object.assign({}, rawAsset, assetDetails) : null;
      })
      .filter((asset) => asset != null)
      .sort((asset1, asset2) => {
        if (asset1 && asset2) {
          if (asset1.fingerprint < asset2.fingerprint) {
            return -1;
          }
          if (asset1.fingerprint > asset2.fingerprint) {
            return 1;
          }
        }
        return 0;
      });
    const totalRawAssets = wallet.assets.total.length;
    const totalAssets = walletAssets.length;
    const hasRawAssets = wallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;

    return (
      <WalletSendForm
        currencyUnit={intl.formatMessage(globalMessages.unitAda)}
        currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
        currentNumberFormat={profile.currentNumberFormat}
        calculateTransactionFee={(
          address: string,
          amount: number,
          selectedAssets: AssetItems
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
        addressValidator={isValidAddress}
        assets={walletAssets}
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
        onUnsetActiveAssetFingerprint={this.handleUnsetActiveAssetFingerprint}
        onExternalLinkClick={app.openExternalLink}
      />
    );
  }
}
