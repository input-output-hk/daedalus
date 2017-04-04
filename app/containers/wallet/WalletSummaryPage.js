// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletSummary from '../../components/wallet/summary/WalletSummary';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import WalletsStore from '../../stores/WalletsStore';
import TransactionsStore from '../../stores/TransactionsStore';
import SettingsStore from '../../stores/SettingsStore';

const messages = defineMessages({
  noTransactions: {
    id: 'wallet.summary.no.transactions',
    defaultMessage: '!!!No recent transactions',
    description: 'Message shown when wallet has no transactions on wallet summary page.'
  },
});

@inject('stores', 'actions') @observer
export default class WalletSummaryPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.instanceOf(WalletsStore),
      transactions: PropTypes.instanceOf(TransactionsStore),
      settings: PropTypes.instanceOf(SettingsStore),
    }).isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { wallets, transactions } = this.props.stores;
    const {
      hasAny,
      totalAvailable,
      recent,
      recentTransactionsRequest,
      totalUnconfirmedAmount
    } = transactions;
    const wallet = wallets.active;
    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    if (recentTransactionsRequest.isExecutingFirstTime || hasAny) {
      walletTransactions = (
        <WalletTransactionsList
          transactions={recent}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
          hasMoreToLoad={false}
          onLoadMore={() => {}}
          assuranceMode={wallets.active.assuranceMode}
        />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <VerticalFlexContainer>
        <WalletSummary
          walletName={wallet.name}
          amount={wallet.amount.toFormat()}
          numberOfTransactions={totalAvailable}
          pendingAmount={totalUnconfirmedAmount.toFormat()}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }

}
