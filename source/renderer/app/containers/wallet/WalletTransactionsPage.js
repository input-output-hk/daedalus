// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactions from '../../components/wallet/transactions/WalletTransactions';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { TransactionFilterOptionsType } from '../../stores/TransactionsStore';
import { getNetworkExplorerUrlByType } from '../../utils/network';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message shown when wallet has no transactions yet.',
  },
});

type Props = InjectedProps;
type State = {
  isFilterButtonFaded: boolean,
};

@inject('stores', 'actions')
@observer
export default class WalletTransactionsPage extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isFilterButtonFaded: false,
  };

  componentDidMount() {
    const { dialogs } = this.props.actions;
    dialogs.closeActiveDialog.trigger();
  }

  openFilterDialog = () => {
    const { dialogs } = this.props.actions;
    const {
      defaultFilterOptions,
      populatedFilterOptions,
    } = this.props.stores.transactions;
    const { currentNumberFormat: numberFormat } = this.props.stores.profile;

    this.setState({ isFilterButtonFaded: false });
    // dialogs.open.trigger({ dialog: FilterDialog });
    dialogs.updateDataForActiveDialog.trigger({
      data: {
        defaultFilterOptions,
        populatedFilterOptions,
        numberFormat,
      },
    });
  };

  onFilter = (filterProps: TransactionFilterOptionsType) => {
    const {
      transactions: transactionActions,
      dialogs: dialogActions,
    } = this.props.actions;
    transactionActions.filterTransactions.trigger(filterProps);
    dialogActions.closeActiveDialog.trigger();
  };

  setFilterButtonFaded = (isFilterButtonFaded: boolean) =>
    this.setState({ isFilterButtonFaded });

  render() {
    const { actions, stores } = this.props;
    const { isFilterButtonFaded } = this.state;
    const { app, wallets, profile } = stores;
    const {
      openExternalLink,
      environment: { network, rawNetwork },
    } = app;
    const activeWallet = wallets.active;
    const {
      filterOptions,
      searchRequest,
      hasAny,
      totalAvailable,
      allFiltered,
      recentFiltered,
      deletePendingTransaction,
      deleteTransactionRequest,
    } = stores.transactions;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    const { dataForActiveDialog } = stores.uiDialogs;
    const { closeActiveDialog } = actions.dialogs;
    const { searchLimit = 0 } = filterOptions;

    let transactions = [];
    const shouldDisplayTransactions =
      hasAny || !activeWallet || activeWallet.isRestoring;

    // Straight away show recent filtered transactions if all filtered ones are not loaded yet
    if (shouldDisplayTransactions) {
      transactions =
        recentFiltered.length && !allFiltered.length
          ? recentFiltered
          : allFiltered;
    }

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(
        type,
        param,
        network,
        rawNetwork,
        currentLocale
      );

    const hasMoreToLoad = () =>
      searchLimit !== null &&
      searchLimit !== undefined &&
      totalAvailable > searchLimit;

    return (
      <WalletTransactions
        activeWallet={activeWallet}
        transactions={transactions}
        shouldDisplayTransactions={shouldDisplayTransactions}
        filterOptions={filterOptions}
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
        currentDateFormat={currentDateFormat}
        onFilter={this.onFilter}
        onClose={() => closeActiveDialog.trigger()}
        {...dataForActiveDialog}
        isRenderingAsVirtualList
      />
    );
  }
}
