// @flow
import React, { Component } from 'react';
import path from 'path';
import { observer, inject } from 'mobx-react';
import { showSaveDialogChannel } from '../../ipc/show-file-dialog-channels';
import WalletTransactions from '../../components/wallet/transactions/WalletTransactions';
import { getNetworkExplorerUrlByType } from '../../utils/network';
import { downloadCsv } from '../../utils/csvGenerator';
import { generateFileNameWithTimestamp } from '../../../../common/utils/files';
import type { CsvFileContent } from '../../../../common/types/csv-request.types';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletTransactionsPage extends Component<Props> {
  onExportCsv = async (fileContent: CsvFileContent) => {
    const fileName = generateFileNameWithTimestamp({
      prefix: 'transactions',
      extension: 'csv',
      isUTC: true,
    });
    const { desktopDirectoryPath } = this.props.stores.profile;
    const defaultPath = path.join(desktopDirectoryPath, fileName);
    const params = {
      defaultPath,
      filters: [
        {
          extensions: ['csv'],
        },
      ],
    };
    const { filePath } = await showSaveDialogChannel.send(params);

    // if cancel button is clicked or path is empty
    if (!filePath) return;

    downloadCsv({ filePath, fileContent });
  };

  render() {
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
      hasAny,
      totalAvailable,
      allFiltered,
      recentFiltered,
      deletePendingTransaction,
      deleteTransactionRequest,
      defaultFilterOptions,
      populatedFilterOptions,
    } = this.props.stores.transactions;
    const {
      currentTimeFormat,
      currentDateFormat,
      currentNumberFormat,
      currentLocale,
    } = profile;
    const { searchLimit = 0 } = filterOptions || {};
    const { transactions: transactionActions } = this.props.actions;

    let transactions = [];

    // Straight away show recent filtered transactions if all filtered ones are not loaded yet
    if (hasAny && activeWallet && !activeWallet.isRestoring) {
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
        currentNumberFormat={currentNumberFormat}
        onFilter={transactionActions.filterTransactions.trigger}
        onRequestCSVFile={this.onExportCsv}
        isRenderingAsVirtualList
      />
    );
  }
}
