// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList, {
  WalletTransactionsListScrollContext,
} from './WalletTransactionsList';
import FilterResultInfo from './FilterResultInfo';
import WalletNoTransactions from './WalletNoTransactions';
import VerticalFlexContainer from '../../layout/VerticalFlexContainer';
import { formattedWalletAmount } from '../../../utils/formatters';
import { getNumberOfFilterDimensionsApplied } from '../../../utils/transaction';
import { getNetworkExplorerUrlByType } from '../../../utils/network';
import { WalletTransaction } from '../../../domains/WalletTransaction';
import Wallet from '../../../domains/Wallet';

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
  shouldDisplayTransactions: boolean,
  filterOptions: any,
  deletePendingTransaction: Function,
  onLoadMore: Function,
  hasMoreToLoad: boolean,
  isLoadingTransactions: boolean,
  isRenderingAsVirtualList: boolean,
  onShowMoreTransactions?: Function,
  onOpenExternalLink: Function,
  getUrlByType: Function,
  showMoreTransactionsButton?: boolean,
  isDeletingTransaction: boolean,
  totalAvailable: number,
  currentDateFormat: string,
  currentTimeFormat: string,
};
type State = {
  isFilterButtonFaded: boolean,
};

@observer
export default class WalletTransactions extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isFilterButtonFaded: false,
  };

  setFilterButtonFaded = (isFilterButtonFaded: boolean) =>
    this.setState({ isFilterButtonFaded });

  render() {
    const { intl } = this.context;
    const { isFilterButtonFaded } = this.state;
    const {
      activeWallet,
      transactions,
      shouldDisplayTransactions,
      filterOptions,
      deletePendingTransaction,
      onLoadMore,
      hasMoreToLoad,
      isLoadingTransactions,
      isRenderingAsVirtualList,
      onShowMoreTransactions,
      onOpenExternalLink,
      getUrlByType,
      showMoreTransactionsButton,
      isDeletingTransaction,
      totalAvailable,
      currentDateFormat,
      currentTimeFormat,
    } = this.props;

    // Guard against potential null values
    // @TX TODO - display NoWallets?
    if (!filterOptions || !activeWallet) return null;

    let walletTransactions = null;
    const { searchLimit } = filterOptions;
    const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
      filterOptions
    );
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    const isRestoreActive = activeWallet && activeWallet.isRestoring;
    const walletId = activeWallet && activeWallet.id;

    if (!shouldDisplayTransactions) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    } else if (numberOfFilterDimensionsApplied > 0 && !transactions.length) {
      walletTransactions = (
        <FilterResultInfo filtered={0} total={totalAvailable} />
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
        <VerticalFlexContainer>{walletTransactions}</VerticalFlexContainer>
      </WalletTransactionsListScrollContext.Provider>
    );
  }
}
