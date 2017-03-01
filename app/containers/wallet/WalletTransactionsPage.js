// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
// import WalletTransactionsSearch from '../../components/wallet/home/WalletTransactionsSearch';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import CachedRequest from '../../stores/lib/CachedRequest';

const messages = defineMessages({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message shown when wallet has no transactions yet.'
  },
  noTransactionsFound: {
    id: 'wallet.transactions.no.transactions.found',
    defaultMessage: '!!!No transactions found',
    description: 'Message shown when wallet transaction search returns zero results.'
  }
});

@inject('stores', 'actions') @observer
export default class WalletTransactionsPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      transactions: PropTypes.shape({
        searchOptions: PropTypes.shape({
          searchTerm: PropTypes.string.isRequired,
          searchLimit: PropTypes.number.isRequired,
        }),
        allTransactionsRequest: PropTypes.instanceOf(CachedRequest),
        filtered: MobxPropTypes.arrayOrObservableArray.isRequired,
        hasAnyFiltered: PropTypes.bool.isRequired,
        totalFilteredAvailable: PropTypes.number.isRequired,
      })
    }).isRequired,
    actions: PropTypes.shape({
      transactions: PropTypes.shape({
        filterTransactions: PropTypes.func.isRequired,
      }),
    }).isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  // _handleSearchInputChange = (value: string, event: Object) => {
  //   this.props.actions.transactions.filterTransactions({ searchTerm: event.target.value });
  // };

  render() {
    const { intl } = this.context;
    const actions = this.props.actions;
    const { transactions } = this.props.stores;
    const {
      searchOptions,
      searchRequest,
      hasAny,
      totalAvailable,
      filtered,
    } = transactions;
    const { searchLimit, searchTerm } = searchOptions;
    const wasSearched = searchTerm !== '';
    let walletTransactions = null;
    // let transactionSearch = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const noTransactionsFoundLabel = intl.formatMessage(messages.noTransactionsFound);

    // if (wasSearched || hasAny) {
    //   transactionSearch = (
    //     <div style={{ flexShrink: 0 }}>
    //       <WalletTransactionsSearch
    //         searchTerm={searchTerm}
    //         onChange={this._handleSearchInputChange}
    //       />
    //     </div>
    //   );
    // }

    if (searchRequest.isExecutingFirstTime || hasAny) {
      walletTransactions = (
        <WalletTransactionsList
          transactions={filtered}
          isLoadingTransactions={searchRequest.isExecutingFirstTime}
          hasMoreToLoad={totalAvailable > searchLimit}
          onLoadMore={actions.transactions.loadMoreTransactions}
        />
      );
    } else if (wasSearched && !hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsFoundLabel} />;
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <div style={{ height: '100%' }}>
        {/* transactionSearch */}
        {walletTransactions}
      </div>
    );
  }

}
