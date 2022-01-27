import React, { Component } from 'react';
import { take } from 'lodash';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { MAX_TRANSACTIONS_ON_SUMMARY_PAGE } from '../../config/numbersConfig';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletSummary from '../../components/wallet/summary/WalletSummary';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import { ROUTES } from '../../routes-config';
import { formattedWalletAmount } from '../../utils/formatters';
import { getNetworkExplorerUrlByType } from '../../utils/network';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';
import { getAssetTokens, sortAssets } from '../../utils/assets';
import type { InjectedProps } from '../../types/injectedPropsType';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.summary.page.no.transactions',
    defaultMessage: '!!!No recent transactions',
    description:
      'Message shown when wallet has no transactions on wallet summary page.',
  },
});
type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class WalletSummaryPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  handleShowMoreTransaction = (walletId: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: {
        id: walletId,
        page: 'transactions',
      },
    });
  };
  handleCurrencySettingsClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.SETTINGS.WALLETS,
    });
  };
  handleViewAllButtonClick = (walletId: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: {
        id: walletId,
        page: 'tokens',
      },
    });
  };

  render() {
    const { intl } = this.context;
    const { stores, actions } = this.props;
    const {
      app,
      wallets,
      addresses,
      transactions,
      profile,
      assets,
      currency,
    } = stores;
    const { all, getAsset, assetSettingsDialogWasOpened, favorites } = assets;
    const { isInternalAddress } = addresses;
    const {
      onAssetSettingsOpen,
      onOpenAssetSend,
      onCopyAssetParam,
      onToggleFavorite,
    } = actions.assets;
    const {
      openExternalLink,
      environment: { network },
    } = app;
    const {
      hasAny,
      totalAvailable,
      recent,
      recentTransactionsRequest,
      deletePendingTransaction,
      deleteTransactionRequest,
      pendingTransactionsCount,
    } = transactions;
    const { active: wallet } = wallets;
    const { isActive, isFetchingRate, lastFetched, rate, selected } = currency;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    // Guard against potential null values
    if (!wallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const walletTokens = wallet.assets.total;
    const assetTokens = getAssetTokens(all, walletTokens).sort(
      sortAssets('token', 'asc')
    );
    const totalRawAssets = wallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = wallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;

    const onViewAllButtonClick = () => this.handleViewAllButtonClick(wallet.id);

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(type, param, network, currentLocale);

    if (
      recentTransactionsRequest.isExecutingFirstTime ||
      hasAny ||
      wallet.isRestoring
    ) {
      walletTransactions = (
        <WalletTransactionsList
          key={`WalletTransactionsList_${wallet.id}`}
          transactions={take(recent, MAX_TRANSACTIONS_ON_SUMMARY_PAGE)}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
          hasMoreToLoad={false}
          deletePendingTransaction={deletePendingTransaction}
          walletId={wallet.id}
          isDeletingTransaction={deleteTransactionRequest.isExecuting}
          isRestoreActive={wallet.isRestoring}
          formattedWalletAmount={formattedWalletAmount}
          showMoreTransactionsButton={
            recent.length > MAX_TRANSACTIONS_ON_SUMMARY_PAGE
          }
          onOpenExternalLink={openExternalLink}
          getUrlByType={getUrlByType}
          onShowMoreTransactions={this.handleShowMoreTransaction}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          totalAvailable={totalAvailable}
          currentTimeFormat={currentTimeFormat}
          currentDateFormat={currentDateFormat}
          isInternalAddress={isInternalAddress}
          hasAssetsEnabled={hasAssetsEnabled}
          getAsset={getAsset}
          onCopyAssetParam={onCopyAssetParam.trigger}
        />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <VerticalFlexContainer>
        <WalletSummary
          wallet={wallet}
          numberOfRecentTransactions={recent.length}
          numberOfTransactions={totalAvailable}
          numberOfPendingTransactions={pendingTransactionsCount}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
          isLoadingAssets={isLoadingAssets}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          hasAssetsEnabled={hasAssetsEnabled && hasRawAssets}
          currentLocale={currentLocale}
          currencyIsActive={isActive}
          currencyIsFetchingRate={isFetchingRate}
          currencyLastFetched={lastFetched}
          currencyRate={rate}
          currencySelected={selected}
          onCurrencySettingClick={this.handleCurrencySettingsClick}
          assets={assetTokens}
          assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
          onOpenAssetSend={onOpenAssetSend.trigger}
          onCopyAssetParam={onCopyAssetParam.trigger}
          onAssetSettings={onAssetSettingsOpen.trigger}
          onExternalLinkClick={app.openExternalLink}
          onViewAllButtonClick={onViewAllButtonClick}
          tokenFavorites={favorites}
          onToggleFavorite={onToggleFavorite.trigger}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }
}

export default WalletSummaryPage;
