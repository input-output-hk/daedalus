// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList, {
  WalletTransactionsListScrollContext,
} from './WalletTransactionsList';
import WalletTransactionsHeader from './WalletTransactionsHeader';
import FilterDialog from './FilterDialog';
import FilterResultInfo from './FilterResultInfo';
import WalletNoTransactions from './WalletNoTransactions';
import VerticalFlexContainer from '../../layout/VerticalFlexContainer';
import { formattedWalletAmount } from '../../../utils/formatters';
import { getNumberOfFilterDimensionsApplied } from '../../../utils/transaction';
import { WalletTransaction } from '../../../domains/WalletTransaction';
import Wallet from '../../../domains/Wallet';
import styles from './WalletTransactions.scss';
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
  currentDateFormat: string,
  currentLocale: string,
  currentTimeFormat: string,
  defaultFilterOptions: TransactionFilterOptionsType,
  deletePendingTransaction: Function,
  filterOptions: TransactionFilterOptionsType,
  getUrlByType: Function,
  hasMoreToLoad: boolean,
  isDeletingTransaction: boolean,
  isLoadingTransactions: boolean,
  onFilter: Function,
  onLoadMore: Function,
  onOpenExternalLink: Function,
  onRequestCSVFile: Function,
  populatedFilterOptions: TransactionFilterOptionsType,
  totalAvailable: number,
  transactions: Array<WalletTransaction>,
};

type State = {
  isScrolling: boolean,
  isFilterDialogOpen: boolean,
};

@observer
export default class WalletTransactions extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isScrolling: false,
    isFilterDialogOpen: false,
  };

  setIsScrolling = (isScrolling: boolean) => this.setState({ isScrolling });

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

  onFilter = (filterOptions: TransactionFilterOptionsType) => {
    this.props.onFilter(filterOptions);
    this.onFilterDialogClose();
  };

  render() {
    const { intl } = this.context;
    const { isFilterDialogOpen, isScrolling } = this.state;
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
        value={{ setIsScrolling: this.setIsScrolling }}
      >
        <div className={styles.component}>
          <WalletTransactionsHeader
            numberOfFilterDimensionsApplied={numberOfFilterDimensionsApplied}
            numberOfTransactions={transactions.length}
            onRequestCSVFile={onRequestCSVFile}
            onFilterDialogOpen={this.onFilterDialogOpen}
            isScrolling={isScrolling}
          >
            {isFilterDialogOpen && (
              <FilterDialog
                locale={currentLocale}
                dateFormat={currentDateFormat}
                defaultFilterOptions={defaultFilterOptions}
                populatedFilterOptions={populatedFilterOptions}
                onFilter={this.onFilter}
                onClose={this.onFilterDialogClose}
              />
            )}
          </WalletTransactionsHeader>
          <VerticalFlexContainer>{walletTransactions}</VerticalFlexContainer>
        </div>
      </WalletTransactionsListScrollContext.Provider>
    );
  }
}
