// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../domain/Wallet';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletSummary from '../../components/wallet/summary/WalletSummary';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import Request from '../../stores/lib/Request';

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
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet),
      }),
      transactions: PropTypes.shape({
        recent: MobxPropTypes.arrayOrObservableArray.isRequired,
        hasAny: PropTypes.bool.isRequired,
        totalAvailable: PropTypes.number.isRequired,
        totalUnconfirmedAmount: PropTypes.number.isRequired,
        recentTransactionsRequest: PropTypes.instanceOf(Request),
      }),
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
        />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <VerticalFlexContainer>
        <WalletSummary
          walletName={wallet.name}
          amount={wallet.amount}
          numberOfTransactions={totalAvailable}
          pendingAmount={totalUnconfirmedAmount}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }

}
