import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../../domains/Wallet';
import type { Currency } from '../../../types/currencyTypes';
import WalletSummaryHeader from './WalletSummaryHeader';
import WalletSummaryCurrency from './WalletSummaryCurrency';
import type { AssetToken } from '../../../api/assets/types';
import WalletTokensList from '../tokens/WalletTokensList';
import { MAX_TOKENS_ON_SUMMARY_PAGE } from '../../../config/numbersConfig';

const messages = defineMessages({
  tokensListTitle: {
    id: 'wallet.summary.assets.tokensTitle',
    defaultMessage: '!!!Tokens',
    description: 'Tokens title in the wallet summary',
  },
});
type Props = {
  wallet: Wallet;
  numberOfRecentTransactions: number;
  numberOfTransactions?: number;
  numberOfPendingTransactions: number;
  isLoadingTransactions: boolean;
  currentLocale: string;
  currencyIsFetchingRate: boolean;
  currencyIsActive: boolean;
  currencySelected: Currency | null | undefined;
  currencyRate: number | null | undefined;
  currencyLastFetched: Date | null | undefined;
  onCurrencySettingClick: (...args: Array<any>) => any;
  assets: Array<AssetToken>;
  onOpenAssetSend: (...args: Array<any>) => any;
  onCopyAssetParam: (...args: Array<any>) => any;
  onAssetSettings: (...args: Array<any>) => any;
  isLoadingAssets: boolean;
  assetSettingsDialogWasOpened: boolean;
  onExternalLinkClick: (...args: Array<any>) => any;
  onViewAllButtonClick: (...args: Array<any>) => any;
  onToggleFavorite: (...args: Array<any>) => any;
  tokenFavorites: Record<string, any>;
};

@observer
class WalletSummary extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      wallet,
      numberOfPendingTransactions,
      numberOfRecentTransactions,
      numberOfTransactions,
      isLoadingTransactions,
      currentLocale,
      currencyIsActive,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
      assets,
      onOpenAssetSend,
      onCopyAssetParam,
      onAssetSettings,
      assetSettingsDialogWasOpened,
      isLoadingAssets,
      onExternalLinkClick,
      onViewAllButtonClick,
      onToggleFavorite,
      tokenFavorites,
    } = this.props;
    const { intl } = this.context;
    const { isRestoring } = wallet;
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
          <WalletTokensList
            assets={assets.slice(0, MAX_TOKENS_ON_SUMMARY_PAGE)}
            assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
            currentLocale={currentLocale}
            isLoadingAssets={isLoadingAssets}
            onAssetSettings={onAssetSettings}
            onCopyAssetParam={onCopyAssetParam}
            onExternalLinkClick={onExternalLinkClick}
            onOpenAssetSend={onOpenAssetSend}
            onToggleFavorite={onToggleFavorite}
            onViewAllButtonClick={onViewAllButtonClick}
            title={intl.formatMessage(messages.tokensListTitle)}
            tokenFavorites={tokenFavorites}
            wallet={wallet}
          />
        )}
      </>
    );
  }
}

export default WalletSummary;
