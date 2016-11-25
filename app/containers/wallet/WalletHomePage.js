// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList from '../../components/wallet/home/WalletTransactionsList';
import WalletTransactionsSearch from '../../components/wallet/home/WalletTransactionsSearch';
import WalletNoTransactions from '../../components/wallet/home/WalletNoTransactions';

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

@observer(['state', 'controller'])
export default class WalletHomePage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      activeWallet: PropTypes.shape({
        wallet: MobxPropTypes.observableObject.isRequired,
        isLoadingTransactions: PropTypes.bool.isRequired,
        transactionsSearchTerm: PropTypes.string.isRequired,
        transactionsSearchLimit: PropTypes.number.isRequired,
        totalAvailableTransactions: PropTypes.number.isRequired,
        hasAnyTransactions: PropTypes.bool.isRequired,
      }).isRequired
    }).isRequired,
    controller: PropTypes.shape({
      wallets: PropTypes.shape({
        filterTransactions: PropTypes.func.isRequired
      }).isRequired
    }).isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleSearchInputChange(value: string, event: Object) {
    this.props.controller.wallets.filterTransactions(event.target.value);
  }

  render() {
    const { controller } = this.props;
    const { intl } = this.context;
    const {
      wallet,
      transactionsSearchTerm,
      isLoadingTransactions,
      transactionsSearchLimit,
      totalAvailableTransactions,
      hasAnyTransactions,
    } = this.props.state.activeWallet;

    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const noTransactionsFoundLabel = intl.formatMessage(messages.noTransactionsFound);

    if (isLoadingTransactions || totalAvailableTransactions) {
      walletTransactions = (
        <WalletTransactionsList
          transactions={wallet.transactions}
          isLoadingTransactions={isLoadingTransactions}
          hasMoreToLoad={totalAvailableTransactions > transactionsSearchLimit}
          onLoadMore={() => controller.wallets.loadMoreTransactions()}
        />
      );
    } else if (!hasAnyTransactions) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    } else {
      walletTransactions = <WalletNoTransactions label={noTransactionsFoundLabel} />;
    }

    return (
      <div style={{ height: '100%', display: 'flex', flexDirection: 'column' }}>
        {(wallet.transactions.length || hasAnyTransactions) && (
          <div style={{ flexShrink: 0 }}>
            <WalletTransactionsSearch
              searchTerm={transactionsSearchTerm}
              onChange={this.handleSearchInputChange.bind(this)}
            />
          </div>
        )}
        {walletTransactions}
      </div>
    );
  }

}
