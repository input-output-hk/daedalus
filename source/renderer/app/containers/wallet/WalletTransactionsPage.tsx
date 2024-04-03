import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletTransactions from '../../components/wallet/transactions/WalletTransactions';
import { getNetworkExplorerUrlByType } from '../../utils/network';
import type { InjectedProps } from '../../types/injectedPropsType';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class WalletTransactionsPage extends Component<Props> {
  render() {
    const { actions, stores } = this.props;
    const { app, wallets, addresses, profile, assets } = stores;
    const {
      openExternalLink,
      environment: { network },
    } = app;
    const { isInternalAddress } = addresses;
    const activeWallet = wallets.active;
    const {
      allFiltered,
      filterOptions,
      searchRequest,
      totalAvailable,
      deletePendingTransaction,
      deleteTransactionRequest,
      defaultFilterOptions,
      populatedFilterOptions,
    } = this.props.stores.transactions;
    const {
      currentTimeFormat,
      currentDateFormat,
      currentLocale,
      currentNumberFormat,
    } = profile;
    const { searchLimit = 0 } = filterOptions || {};
    const { transactions: transactionActions } = this.props.actions;
    const { filterTransactions, requestCSVFile } = transactionActions;
    const { onCopyAssetParam } = actions.assets;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    const { getAsset } = assets;

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(type, param, network, currentLocale);

    const hasMoreToLoad = () =>
      searchLimit !== null &&
      searchLimit !== undefined &&
      totalAvailable > searchLimit;

    return (
      <WalletTransactions
        activeWallet={activeWallet}
        transactions={allFiltered}
        filterOptions={filterOptions || {}}
        defaultFilterOptions={defaultFilterOptions}
        populatedFilterOptions={populatedFilterOptions}
        deletePendingTransaction={deletePendingTransaction}
        isLoadingTransactions={searchRequest.isExecutingFirstTime}
        hasMoreToLoad={hasMoreToLoad()}
        onLoadMore={actions.transactions.loadMoreTransactions.trigger}
        isDeletingTransaction={deleteTransactionRequest.isExecuting}
        onOpenExternalLink={openExternalLink}
        getUrlByType={getUrlByType}
        totalAvailable={totalAvailable}
        currentLocale={currentLocale}
        currentTimeFormat={currentTimeFormat}
        currentNumberFormat={currentNumberFormat}
        currentDateFormat={currentDateFormat}
        onFilter={filterTransactions.trigger}
        onRequestCSVFile={requestCSVFile.trigger}
        hasAssetsEnabled={hasAssetsEnabled}
        isInternalAddress={isInternalAddress}
        getAsset={getAsset}
        onCopyAssetParam={onCopyAssetParam.trigger}
      />
    );
  }
}

export default WalletTransactionsPage;
