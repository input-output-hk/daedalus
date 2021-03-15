// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Wallet from '../../../domains/Wallet';
import type { Currency } from '../../../types/currencyTypes';
import WalletSummaryHeader from './WalletSummaryHeader';
import WalletSummaryAssets from './WalletSummaryAssets';
import WalletSummaryCurrency from './WalletSummaryCurrency';
import type { WalletSummaryAsset } from '../../../api/assets/types';
import WalletSummaryNoTokens from './WalletSummaryNoTokens';

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
  assets: Array<WalletSummaryAsset>,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  isLoadingAssets: boolean,
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
      currencyIsAvailable,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
      assets,
      onOpenAssetSend,
      onCopyAssetItem,
      isLoadingAssets,
      onExternalLinkClick,
    } = this.props;

    const { isRestoring } = wallet;

    const hasCurrency =
      currencyIsActive &&
      currencyIsAvailable &&
      !!currencySelected &&
      (!!currencyRate || currencyIsFetchingRate);

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
        {!isRestoring && (
          <>
            {hasAssets ? (
              <WalletSummaryAssets
                wallet={wallet}
                assets={assets}
                onOpenAssetSend={onOpenAssetSend}
                isLoadingAssets={isLoadingAssets}
                onCopyAssetItem={onCopyAssetItem}
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
