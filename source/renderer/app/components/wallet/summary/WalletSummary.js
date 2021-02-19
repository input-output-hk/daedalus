// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Wallet from '../../../domains/Wallet';
import type { Currency } from '../../../types/currencyTypes';
import WalletSummaryHeader from './WalletSummaryHeader';
import WalletSummaryAssets from './WalletSummaryAssets';
import WalletSummaryCurrency from './WalletSummaryCurrency';
import type { WalletSummaryAsset } from '../../../api/assets/types';

type Props = {
  wallet: Wallet,
  numberOfRecentTransactions: number,
  numberOfTransactions?: number,
  numberOfPendingTransactions: number,
  isLoadingTransactions: boolean,
  currencyIsFetchingRate: boolean,
  currencyIsAvailable: boolean,
  currencyIsActive: boolean,
  currencySelected: ?Currency,
  currencyRate: ?number,
  currencyLastFetched: ?Date,
  onCurrencySettingClick: Function,
  hasAssetsEnabled?: boolean,
  assets: Array<WalletSummaryAsset>,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  isLoading?: boolean,
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
      currencyIsAvailable,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
      hasAssetsEnabled,
      assets,
      onOpenAssetSend,
      onCopyAssetItem,
      isLoading,
    } = this.props;

    const hasCurrency =
      currencyIsActive &&
      currencyIsAvailable &&
      !!currencySelected &&
      (!!currencyRate || currencyIsFetchingRate);

    return (
      <>
        <WalletSummaryHeader
          wallet={wallet}
          numberOfRecentTransactions={numberOfRecentTransactions}
          numberOfTransactions={numberOfTransactions}
          numberOfPendingTransactions={numberOfPendingTransactions}
          isLoadingTransactions={isLoadingTransactions}
          currency={
            hasCurrency && (
              <WalletSummaryCurrency
                wallet={wallet}
                currencyIsFetchingRate={currencyIsFetchingRate}
                currencyIsAvailable={currencyIsAvailable}
                currencyIsActive={currencyIsActive}
                currencySelected={currencySelected}
                currencyRate={currencyRate}
                currencyLastFetched={currencyLastFetched}
                onCurrencySettingClick={onCurrencySettingClick}
              />
            )
          }
        />
        {hasAssetsEnabled && (
          <WalletSummaryAssets
            wallet={wallet}
            assets={assets}
            onOpenAssetSend={onOpenAssetSend}
            isLoading={isLoading}
            onCopyAssetItem={onCopyAssetItem}
          />
        )}
      </>
    );
  }
}
