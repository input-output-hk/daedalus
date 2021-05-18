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
  currentNumberFormat: string,
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
  hasAssetsEnabled: boolean,
  getAsset: Function,
  isInternalAddress: Function,
  onCopyAssetItem: Function,
};

type State = {
  isScrolling: boolean,
};

@observer
export default class WalletTransactions extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isScrolling: false,
  };

  setIsScrolling = (isScrolling: boolean) => this.setState({ isScrolling });

  onFilter = (filterOptions: TransactionFilterOptionsType) => {
    this.props.onFilter(filterOptions);
  };

  render() {
    const { intl } = this.context;
    const { isScrolling } = this.state;
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
      hasAssetsEnabled,
      getAsset,
      isInternalAddress,
      onCopyAssetItem,
    } = this.props;

    // Guard against potential null values
    if (!filterOptions || !activeWallet) return null;

    let walletTransactions = null;
    // const { searchLimit } = filterOptions;
    const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
      filterOptions
    );
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    const isRestoreActive = activeWallet && activeWallet.isRestoring;

    const isFilterDisabled =
      !transactions.length && !numberOfFilterDimensionsApplied;

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
          hasAssetsEnabled={hasAssetsEnabled}
          getAsset={getAsset}
          isRenderingAsVirtualList
          isInternalAddress={isInternalAddress}
          onCopyAssetItem={onCopyAssetItem}
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
            isScrolling={isScrolling}
            isFilterDisabled={isFilterDisabled}
            filterDialogProps={{
              defaultFilterOptions,
              populatedFilterOptions,
              locale: currentLocale,
              dateFormat: currentDateFormat,
              onFilter: this.onFilter,
              numberFormat: currentNumberFormat,
            }}
          />
          <VerticalFlexContainer>{walletTransactions}</VerticalFlexContainer>
        </div>
      </WalletTransactionsListScrollContext.Provider>
    );
  }
}
