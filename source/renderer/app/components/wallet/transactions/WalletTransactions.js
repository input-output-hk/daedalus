// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList, {
  WalletTransactionsListScrollContext,
} from './WalletTransactionsList';
import WalletTransactionsHeader from './WalletTransactionsHeader';
import FilterResultInfo from './FilterResultInfo';
import WalletNoTransactions from './WalletNoTransactions';
import VerticalFlexContainer from '../../layout/VerticalFlexContainer';
import { formattedWalletAmount } from '../../../utils/formatters';
import { getNumberOfFilterDimensionsApplied } from '../../../utils/transaction';
import { WalletTransaction } from '../../../domains/WalletTransaction';
import Wallet from '../../../domains/Wallet';
import type { TransactionFilterOptionsType } from '../../../stores/TransactionsStore';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message shown when wallet has no transactions yet.',
  },
});

type Props = {
  activeWallet: ?Wallet,
  transactions: Array<WalletTransaction>,
  filterOptions: TransactionFilterOptionsType,
  defaultFilterOptions: TransactionFilterOptionsType,
  populatedFilterOptions: TransactionFilterOptionsType,
  deletePendingTransaction: Function,
  onLoadMore: Function,
  hasMoreToLoad: boolean,
  isLoadingTransactions: boolean,
  onOpenExternalLink: Function,
  getUrlByType: Function,
  isDeletingTransaction: boolean,
  totalAvailable: number,
  currentLocale: string,
  currentDateFormat: string,
  currentNumberFormat: string,
  onFilter: Function,
  onClose: Function,
  onRequestCSVFile: Function,
};

type State = {
  isScrolling: boolean,
  isFilterButtonFaded: boolean,
  isFilterDialogOpen: boolean,
  filterOptions: TransactionFilterOptionsType,
};

@observer
export default class WalletTransactions extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isFilterButtonFaded: false,
    isFilterDialogOpen: false,
  };

  setFilterButtonFaded = (isFilterButtonFaded) => {
    this.setState(() => ({
      isFilterButtonFaded,
    }));
  };

  onFilterDialogOpen = () => {
    this.setState(() => ({
      isFilterDialogOpen: true,
    }));
  };

  onFilterDialogClose = () => {
    this.setState(() => ({
      isFilterDialogOpen: false,
    }));
  };

  render() {
    const { intl } = this.context;
    const { isFilterButtonFaded, isFilterDialogOpen } = this.state;
    const {
      activeWallet,
      transactions,
      filterOptions,
      deletePendingTransaction,
      onLoadMore,
      hasMoreToLoad,
      isLoadingTransactions,
      onOpenExternalLink,
      getUrlByType,
      isDeletingTransaction,
      totalAvailable,
      currentDateFormat,
      currentTimeFormat,
      currentNumberFormat,
      currentLocale,
      onRequestCSVFile,
      defaultFilterOptions,
      populatedFilterOptions,
    } = this.props;

    // Guard against potential null values
    // @TX TODO - display NoWallets?
    if (!filterOptions || !activeWallet) return null;

    let walletTransactions = null;
    // const { searchLimit } = filterOptions;
    const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
      filterOptions
    );
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    const isRestoreActive = activeWallet && activeWallet.isRestoring;

    if (!transactions.length) {
      walletTransactions = numberOfFilterDimensionsApplied ? (
        <FilterResultInfo filtered={0} total={totalAvailable} />
      ) : (
        <WalletNoTransactions label={noTransactionsLabel} />
      );
    } else {
      walletTransactions = (
        <WalletTransactionsList
          transactions={transactions}
          deletePendingTransaction={deletePendingTransaction}
          isLoadingTransactions={isLoadingTransactions}
          isRestoreActive={isRestoreActive}
          onLoadMore={onLoadMore}
          hasMoreToLoad={hasMoreToLoad}
          walletId={activeWallet.id}
          isDeletingTransaction={isDeletingTransaction}
          formattedWalletAmount={formattedWalletAmount}
          onOpenExternalLink={onOpenExternalLink}
          getUrlByType={getUrlByType}
          currentLocale={currentLocale}
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
        <WalletTransactionsHeader
          transactions={transactions}
          onRequestCSVFile={onRequestCSVFile}
          onFilterDialogOpen={this.onFilterDialogOpen}
          onFilterDialogClose={this.onFilterDialogClose}
          isFilterButtonFaded={isFilterButtonFaded}
          isFilterDialogOpen={isFilterDialogOpen}
          defaultFilterOptions={defaultFilterOptions}
          populatedFilterOptions={populatedFilterOptions}
          currentLocale={currentLocale}
          currentTimeFormat={currentTimeFormat}
          currentNumberFormat={currentNumberFormat}
          currentDateFormat={currentDateFormat}
        />
        <VerticalFlexContainer>{walletTransactions}</VerticalFlexContainer>
      </WalletTransactionsListScrollContext.Provider>
    );
  }
}
