// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Wallet from '../../../domains/Wallet';
import type { Currency } from '../../../types/currencyTypes';
import WalletSummaryHeader from './WalletSummaryHeader';
import WalletSummaryAssets from './WalletSummaryAssets';
import WalletSummaryCurrency from './WalletSummaryCurrency';
import type { AssetTokenProps } from '../../../api/assets/types';
import WalletSummaryNoTokens from './WalletSummaryNoTokens';

type Props = {
  wallet: Wallet,
  numberOfRecentTransactions: number,
  numberOfTransactions?: number,
  numberOfPendingTransactions: number,
  isLoadingTransactions: boolean,
  currencyIsFetchingRate: boolean,
  currencyIsActive: boolean,
  currencySelected: ?Currency,
  currencyRate: ?number,
  currencyLastFetched: ?Date,
  onCurrencySettingClick: Function,
  assets: Array<AssetTokenProps>,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  onAssetSettings: Function,
  isLoadingAssets: boolean,
  assetSettingsDialogWasOpened: boolean,
  onExternalLinkClick: Function,
};

@observer
export default class WalletSummary extends Component<Props> {
  render() {
    const {
      wallet,
      numberOfPendingTransactions,
      numberOfRecentTransactions,
      numberOfTransactions,
      isLoadingTransactions,
      currencyIsActive,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
      assets,
      onOpenAssetSend,
      onCopyAssetItem,
      onAssetSettings,
      assetSettingsDialogWasOpened,
      isLoadingAssets,
      onExternalLinkClick,
    } = this.props;

    const { isRestoring } = wallet;
    const hasAssets = assets.length;

    return (
      <>
        <WalletSummaryHeader
          wallet={wallet}
          numberOfRecentTransactions={numberOfRecentTransactions}
          numberOfTransactions={numberOfTransactions}
          numberOfPendingTransactions={numberOfPendingTransactions}
          isLoadingTransactions={isLoadingTransactions}
          currency={
            currencyIsActive && (
              <WalletSummaryCurrency
                wallet={wallet}
                currencyIsFetchingRate={currencyIsFetchingRate}
                currencyIsActive={currencyIsActive}
                currencySelected={currencySelected}
                currencyRate={currencyRate}
                currencyLastFetched={currencyLastFetched}
                onCurrencySettingClick={onCurrencySettingClick}
              />
            )
          }
        />
        {!isRestoring && (
          <>
            {hasAssets ? (
              <WalletSummaryAssets
                wallet={wallet}
                assets={assets}
                onOpenAssetSend={onOpenAssetSend}
                isLoadingAssets={isLoadingAssets}
                onCopyAssetItem={onCopyAssetItem}
                onAssetSettings={onAssetSettings}
                assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
              />
            ) : (
              <WalletSummaryNoTokens
                numberOfAssets={assets.length}
                isLoadingAssets={isLoadingAssets}
                onExternalLinkClick={onExternalLinkClick}
              />
            )}
          </>
        )}
      </>
    );
  }
}
