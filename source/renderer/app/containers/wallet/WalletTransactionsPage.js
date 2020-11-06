// @flow
import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { showSaveDialogChannel } from '../../ipc/show-file-dialog-channels';
import WalletTransactions from '../../components/wallet/transactions/WalletTransactions';
import { getNetworkExplorerUrlByType } from '../../utils/network';
import transactionsCsvGenerator from '../../utils/transactionsCsvGenerator';
import { WalletTransaction } from '../../domains/WalletTransaction';
import type { CsvFileContent } from '../../../../common/types/csv-request.types';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletTransactionsPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get transactions() {
    let transactions = [];
    const { stores } = this.props;
    const { wallets } = stores;
    const activeWallet = wallets.active;
    const { hasAny, allFiltered, recentFiltered } = stores.transactions;

    // Straight away show recent filtered transactions if all filtered ones are not loaded yet
    if (hasAny && activeWallet && !activeWallet.isRestoring) {
      transactions =
        recentFiltered.length && !allFiltered.length
          ? recentFiltered
          : allFiltered;
    }
    return transactions;
  }

  render() {
    const { intl } = this.context;
    const { actions, stores } = this.props;
    const { app, wallets, profile } = stores;
    const {
      openExternalLink,
      environment: { network, rawNetwork },
    } = app;
    const activeWallet = wallets.active;
    const {
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
      desktopDirectoryPath,
    } = profile;
    const { searchLimit = 0 } = filterOptions || {};
    const { transactions: transactionActions } = this.props.actions;
    const { transactions } = this;

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

    const onRequestCSVFile = () =>
      transactionsCsvGenerator({
        desktopDirectoryPath,
        intl,
        transactions,
      });

    return (
      <WalletTransactions
        activeWallet={activeWallet}
        transactions={transactions}
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
        currentDateFormat={currentDateFormat}
        onFilter={transactionActions.filterTransactions.trigger}
        onRequestCSVFile={onRequestCSVFile}
        isRenderingAsVirtualList
      />
    );
  }
}
