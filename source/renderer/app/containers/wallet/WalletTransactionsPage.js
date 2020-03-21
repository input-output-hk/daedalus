// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList, {
  WalletTransactionsListScrollContext,
} from '../../components/wallet/transactions/WalletTransactionsList';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import FilterDialogContainer from './dialogs/FilterDialogContainer';
import FilterDialog from '../../components/wallet/transactions/FilterDialog';
import FilterButton from '../../components/wallet/transactions/FilterButton';
import FilterResultInfo from '../../components/wallet/transactions/FilterResultInfo';
import type { InjectedProps } from '../../types/injectedPropsType';
import { formattedWalletAmount } from '../../utils/formatters';
import { getNumberOfFilterDimensionsApplied } from '../../utils/transaction';
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
    dialogs.open.trigger({ dialog: FilterDialog });
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
    const { intl } = this.context;
    const { actions, stores } = this.props;
    const { isFilterButtonFaded } = this.state;
    const { app, uiDialogs, wallets, profile } = stores;
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

    // Guard against potential null values
    if (!filterOptions || !activeWallet) return null;

    let walletTransactions = null;
    const { searchLimit } = filterOptions;
    const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
      filterOptions
    );
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const hasMoreToLoad = () =>
      searchLimit !== null &&
      searchLimit !== undefined &&
      totalAvailable > searchLimit;

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(
        type,
        param,
        network,
        rawNetwork,
        currentLocale
      );

    // Straight away show recent filtered transactions if all filtered ones are not loaded yet
    const transactions =
      recentFiltered.length && !allFiltered.length
        ? recentFiltered
        : allFiltered;

    if (!hasAny && !activeWallet.isRestoring) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    } else if (numberOfFilterDimensionsApplied > 0 && !transactions.length) {
      walletTransactions = (
        <FilterResultInfo filtered={0} total={totalAvailable} />
      );
    } else if (
      searchRequest.isExecutingFirstTime ||
      hasAny ||
      activeWallet.isRestoring
    ) {
      walletTransactions = (
        <WalletTransactionsList
          transactions={transactions}
          deletePendingTransaction={deletePendingTransaction}
          isLoadingTransactions={searchRequest.isExecutingFirstTime}
          isRestoreActive={activeWallet.isRestoring}
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
    }

    return (
      <WalletTransactionsListScrollContext.Provider
        value={{ setFilterButtonFaded: this.setFilterButtonFaded }}
      >
        <VerticalFlexContainer>
          {uiDialogs.isOpen(FilterDialog) && (
            <FilterDialogContainer onFilter={this.onFilter} />
          )}
          {walletTransactions}
        </VerticalFlexContainer>
        {hasAny && (
          <FilterButton
            numberOfFilterDimensionsApplied={numberOfFilterDimensionsApplied}
            faded={isFilterButtonFaded}
            onClick={this.openFilterDialog}
          />
        )}
      </WalletTransactionsListScrollContext.Provider>
    );
  }
}
