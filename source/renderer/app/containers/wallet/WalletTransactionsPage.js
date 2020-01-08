// @flow
import React, { Component } from 'react';
import { get } from 'lodash';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import FilterDialogContainer from './dialogs/FilterDialogContainer';
import FilterDialog from '../../components/wallet/transactions/FilterDialog';
import type { InjectedProps } from '../../types/injectedPropsType';
import { formattedWalletAmount } from '../../utils/formatters';
import { WalletSyncStateStatuses } from '../../domains/Wallet';
import { getNetworkExplorerUrlByType } from '../../utils/network';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message shown when wallet has no transactions yet.',
  },
  noTransactionsFound: {
    id: 'wallet.transactions.no.transactions.found',
    defaultMessage: '!!!No transactions found',
    description:
      'Message shown when wallet transaction search returns zero results.',
  },
});

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletTransactionsPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  openFilterDialog = () => {
    const { dialogs } = this.props.actions;
    const { filterEdges } = this.props.stores.transactions;

    dialogs.open.trigger({ dialog: FilterDialog });
    dialogs.updateDataForActiveDialog.trigger({
      data: filterEdges,
    });
  };

  onFilter = (filterProps: {
    fromDate: string,
    toDate: string,
    fromAmount: number,
    toAmount: number,
    incomingChecked: boolean,
    outgoingChecked: boolean,
  }) => {
    const { transactions: actions } = this.props.actions;
    actions.filterTransactions.trigger(filterProps);
  };

  render() {
    const { intl } = this.context;
    const { actions, stores } = this.props;
    const { app, uiDialogs, wallets, profile } = stores;
    const {
      openExternalLink,
      environment: { network, rawNetwork },
    } = app;
    const activeWallet = wallets.active;
    const {
      searchOptions,
      searchRequest,
      hasAny,
      totalAvailable,
      filtered,
      recent,
      deletePendingTransaction,
      deleteTransactionRequest,
    } = stores.transactions;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;

    // Guard against potential null values
    if (!searchOptions || !activeWallet) return null;

    let walletTransactions = null;
    const { searchLimit, searchTerm } = searchOptions;
    const wasSearched = searchTerm !== '';
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const noTransactionsFoundLabel = intl.formatMessage(
      messages.noTransactionsFound
    );
    const hasMoreToLoad = () =>
      searchLimit !== null &&
      searchLimit !== undefined &&
      totalAvailable > searchLimit;

    const isRestoreActive =
      get(activeWallet, ['syncState', 'status']) ===
      WalletSyncStateStatuses.RESTORING;

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(
        type,
        param,
        network,
        rawNetwork,
        currentLocale
      );

    // Straight away show recent transactions if filtered ones are not loaded yet
    const transactions = recent.length && !filtered.length ? recent : filtered;

    if (searchRequest.isExecutingFirstTime || hasAny || isRestoreActive) {
      walletTransactions = (
        <WalletTransactionsList
          openFilterDialog={this.openFilterDialog}
          transactions={transactions}
          deletePendingTransaction={deletePendingTransaction}
          isLoadingTransactions={searchRequest.isExecutingFirstTime}
          isRestoreActive={isRestoreActive}
          hasMoreToLoad={hasMoreToLoad()}
          onLoadMore={actions.transactions.loadMoreTransactions.trigger}
          walletId={activeWallet.id}
          isDeletingTransaction={deleteTransactionRequest.isExecuting}
          formattedWalletAmount={formattedWalletAmount}
          onOpenExternalLink={openExternalLink}
          getUrlByType={getUrlByType}
          currentTimeFormat={currentTimeFormat}
          currentDateFormat={currentDateFormat}
          isRenderingAsVirtualList
        />
      );
    } else if (wasSearched && !hasAny) {
      walletTransactions = (
        <WalletNoTransactions label={noTransactionsFoundLabel} />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <VerticalFlexContainer>
        {uiDialogs.isOpen(FilterDialog) && (
          <FilterDialogContainer onFilter={this.onFilter} />
        )}
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }
}
