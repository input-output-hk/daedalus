// @flow
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
import { ellipsis } from '../../utils/strings';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { WalletSummaryAsset } from '../../api/assets/types';

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
export default class WalletSummaryPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleShowMoreTransaction = (walletId: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: { id: walletId, page: 'transactions' },
    });
  };

  handleCurrencySettingsClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.SETTINGS.WALLETS,
    });
  };

  handleOpenAssetSend = ({ fingerprint }: WalletSummaryAsset) => {
    const { stores } = this.props;
    const { wallets } = stores;
    const { active } = wallets;
    if (active) {
      const { id } = active;
      const { wallets: walletActions, router } = this.props.actions;
      walletActions.setActiveAssetFingerprint.trigger({
        fingerprint,
      });
      router.goToRoute.trigger({
        route: ROUTES.WALLETS.PAGE,
        params: { id, page: 'send' },
      });
    }
  };

  handleOnCopyAssetItem = (assetItem: string, fullValue: string) => {
    const value = ellipsis(fullValue, 15, 15);
    this.props.actions.wallets.copyAssetItem.trigger({
      assetItem,
      value,
    });
  };

  render() {
    const { intl } = this.context;
    const { stores } = this.props;
    const { app, wallets, addresses, transactions, profile, assets } = stores;
    const { getAssetDetails } = assets;
    const { isInternalAddress } = addresses;
    const {
      openExternalLink,
      environment: { network, rawNetwork },
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
    const {
      active: wallet,
      currencyIsActive,
      currencyIsAvailable,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
    } = wallets;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;

    // Guard against potential null values
    if (!wallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    // $FlowFixMe
    const walletAssets: Array<WalletSummaryAsset> = wallet.assets.total
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

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(
        type,
        param,
        network,
        rawNetwork,
        currentLocale
      );

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
          totalAvailable={totalAvailable}
          currentTimeFormat={currentTimeFormat}
          currentDateFormat={currentDateFormat}
          isInternalAddress={isInternalAddress}
          hasAssetsEnabled={hasAssetsEnabled}
          getAssetDetails={getAssetDetails}
          onCopyAssetItem={this.handleOnCopyAssetItem}
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
          hasAssetsEnabled={hasAssetsEnabled && hasRawAssets}
          currencyIsActive={currencyIsActive}
          currencyIsAvailable={currencyIsAvailable}
          currencyIsFetchingRate={currencyIsFetchingRate}
          currencyLastFetched={currencyLastFetched}
          currencyRate={currencyRate}
          currencySelected={currencySelected}
          onCurrencySettingClick={this.handleCurrencySettingsClick}
          assets={walletAssets}
          onOpenAssetSend={this.handleOpenAssetSend}
          onCopyAssetItem={this.handleOnCopyAssetItem}
          onExternalLinkClick={app.openExternalLink}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }
}
