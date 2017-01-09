// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList from '../../components/wallet/home/WalletTransactionsList';
import WalletTransactionsSearch from '../../components/wallet/home/WalletTransactionsSearch';
import WalletNoTransactions from '../../components/wallet/home/WalletNoTransactions';
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
export default class WalletHomePage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      transactions: PropTypes.shape({
        searchOptions: PropTypes.shape({
          searchTerm: PropTypes.string.isRequired,
          searchLimit: PropTypes.number.isRequired,
        }),
        searchRequest: PropTypes.instanceOf(CachedRequest),
        filtered: MobxPropTypes.arrayOrObservableArray.isRequired,
        hasAny: PropTypes.bool.isRequired,
        totalAvailable: PropTypes.number.isRequired,
      }),
    }).isRequired,
    actions: PropTypes.shape({
      filterTransactions: PropTypes.func.isRequired
    }).isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  _handleSearchInputChange = (value: string, event: Object) => {
    this.props.actions.filterTransactions({ searchTerm: event.target.value });
  };

  render() {
    const { intl } = this.context;
    const actions = this.props.actions;
    const {
      searchOptions,
      searchRequest,
      hasAny,
      totalAvailable,
      filtered,
    } = this.props.stores.transactions;
    const { searchLimit, searchTerm } = searchOptions;

    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const noTransactionsFoundLabel = intl.formatMessage(messages.noTransactionsFound);

    if (searchRequest.isExecuting || hasAny) {
      walletTransactions = (
        <WalletTransactionsList
          transactions={filtered}
          isLoadingTransactions={searchRequest.isExecuting}
          hasMoreToLoad={totalAvailable > searchLimit}
          onLoadMore={actions.loadMoreTransactions}
        />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    } else {
      walletTransactions = <WalletNoTransactions label={noTransactionsFoundLabel} />;
    }

    return (
      <div style={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
        {(searchRequest.isExecuting || hasAny) && (
          <div style={{ flexShrink: 0 }}>
            <WalletTransactionsSearch
              searchTerm={searchTerm}
              onChange={this._handleSearchInputChange}
            />
          </div>
        )}
        {walletTransactions}
      </div>
    );
  }

}
