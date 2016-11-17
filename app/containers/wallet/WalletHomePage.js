// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletTransactionsList from '../../components/wallet/home/WalletTransactionsList';
import WalletTransactionsSearch from '../../components/wallet/home/WalletTransactionsSearch';

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
      }).isRequired
    }).isRequired,
    controller: PropTypes.shape({
      wallets: PropTypes.shape({
        filterTransactions: PropTypes.func.isRequired
      }).isRequired
    }).isRequired
  };

  handleSearchInputChange(value: string, event: Object) {
    this.props.controller.wallets.filterTransactions(event.target.value);
  }

  render() {
    const { controller } = this.props;
    const {
      wallet,
      transactionsSearchTerm,
      isLoadingTransactions,
      transactionsSearchLimit,
      totalAvailableTransactions
    } = this.props.state.activeWallet;
    return (
      <div style={{ height: '100%', padding: '20px' }}>
        <WalletTransactionsSearch
          searchTerm={transactionsSearchTerm}
          onChange={this.handleSearchInputChange.bind(this)}
        />
        <WalletTransactionsList
          transactions={wallet.transactions}
          isLoadingTransactions={isLoadingTransactions}
          hasMoreToLoad={totalAvailableTransactions > transactionsSearchLimit}
          onLoadMore={() => controller.wallets.loadMoreTransactions()}
        />
      </div>
    );
  }

}
