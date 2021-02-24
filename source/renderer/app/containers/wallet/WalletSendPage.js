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
import WalletAssetsSendForm from '../../components/wallet/WalletAssetsSendForm';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';
import Asset from '../../domains/Asset';
import type { AssetItems } from '../../api/assets/types';

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

  getTokenByFingerprintId = (fingerprint: string, allAssets: Array<Asset>) => {
    return allAssets.find((asset) => asset.fingerprint === fingerprint);
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
    // $FlowFixMe
    const locationPath = this.props.location;
    const { pathname } = locationPath;
    const splicedPath = pathname.split('/send/');
    let tokenFingerprint = '';
    if (splicedPath && splicedPath.length) {
      tokenFingerprint = splicedPath[splicedPath.length - 1];
    }
    const { isValidAddress } = wallets;
    const { validateAmount } = transactions;
    const { hwDeviceStatus } = hardwareWallets;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    const { all: allAssets } = assetsStore;

    const selectedToken = this.getTokenByFingerprintId(
      tokenFingerprint,
      allAssets
    );

    // Guard against potential null values
    const activeWallet = wallets.active;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');

    const { isHardwareWallet } = activeWallet;

    let { hasAssets } = activeWallet;

    const assets = activeWallet.assets.total.map((walletAsset) => {
      const assetData = allAssets.find(
        (item) => item.assetName === walletAsset.assetName
      );
      let fingerprint;
      if (!assetData || !assetData.fingerprint) {
        hasAssets = !!assetData;
        fingerprint = `token${walletAsset.policyId}${walletAsset.assetName}`.substr(
          0,
          44
        );
      } else {
        fingerprint = assetData.fingerprint;
      }

      return {
        policyId: walletAsset.policyId,
        assetName: walletAsset.assetName,
        fingerprint,
        quantity: walletAsset.quantity,
        metadata: assetData
          ? assetData.metadata
          : {
              name: '',
              acronym: '',
              description: '',
            },
      };
    });

    return (
      <WalletAssetsSendForm
        currencyUnit={intl.formatMessage(globalMessages.unitAda)}
        currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
        currentNumberFormat={profile.currentNumberFormat}
        validateAmount={validateAmount}
        calculateTransactionFee={(
          address: string,
          amount: number,
          selectedAssets?: AssetItems
        ) =>
          this.calculateTransactionFee({
            walletId: activeWallet.id,
            address,
            amount,
            isHardwareWallet,
            selectedAssets,
          })
        }
        walletAmount={activeWallet.amount}
        assets={assets}
        addressValidator={isValidAddress}
        isDialogOpen={uiDialogs.isOpen}
        openDialogAction={(params) =>
          this.openDialog(params.dialog, isHardwareWallet, activeWallet.id)
        }
        isRestoreActive={activeWallet.isRestoring}
        onExternalLinkClick={app.openExternalLink}
        hwDeviceStatus={hwDeviceStatus}
        isHardwareWallet={isHardwareWallet}
        hasAssets={hasAssetsEnabled && hasAssets}
        selectedToken={selectedToken}
      />
    );
  }
}
