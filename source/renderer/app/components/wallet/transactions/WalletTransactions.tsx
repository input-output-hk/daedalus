import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList, {
  WalletTransactionsListScrollContext,
} from './WalletTransactionsList';
import WalletTransactionsHeader from './WalletTransactionsHeader';
import FilterResultInfo from './FilterResultInfo';
import WalletNoTransactions from './WalletNoTransactions';
import { formattedWalletAmount } from '../../../utils/formatters';
import { getNumberOfFilterDimensionsApplied } from '../../../utils/transaction';
import { WalletTransaction } from '../../../domains/WalletTransaction';
import Wallet from '../../../domains/Wallet';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTransactions.scss' or ... Remove this comment to see the full error message
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
  activeWallet: Wallet | null | undefined;
  currentDateFormat: string;
  currentLocale: string;
  currentTimeFormat: string;
  currentNumberFormat: string;
  defaultFilterOptions: TransactionFilterOptionsType;
  deletePendingTransaction: (...args: Array<any>) => any;
  filterOptions: TransactionFilterOptionsType;
  getUrlByType: (...args: Array<any>) => any;
  hasMoreToLoad: boolean;
  isDeletingTransaction: boolean;
  isLoadingTransactions: boolean;
  onFilter: (...args: Array<any>) => any;
  onLoadMore: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  onRequestCSVFile: (...args: Array<any>) => any;
  populatedFilterOptions: TransactionFilterOptionsType;
  totalAvailable: number;
  transactions: Array<WalletTransaction>;
  hasAssetsEnabled: boolean;
  getAsset: (...args: Array<any>) => any;
  isInternalAddress: (...args: Array<any>) => any;
  onCopyAssetParam: (...args: Array<any>) => any;
};
type State = {
  isScrolling: boolean;
};

@observer
class WalletTransactions extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    isScrolling: false,
  };
  setIsScrolling = (isScrolling: boolean) =>
    this.setState({
      isScrolling,
    });
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
      onCopyAssetParam,
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
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
          onCopyAssetParam={onCopyAssetParam}
        />
      );
    }

    return (
      <WalletTransactionsListScrollContext.Provider
        value={{
          setIsScrolling: this.setIsScrolling,
        }}
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
              isDisabled: isFilterDisabled,
            }}
          />
          {walletTransactions}
        </div>
      </WalletTransactionsListScrollContext.Provider>
    );
  }
}

export default WalletTransactions;
