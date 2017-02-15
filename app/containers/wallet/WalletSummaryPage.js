// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../domain/Wallet';
import WalletTransactionsList from '../../components/wallet/home/WalletTransactionsList';
import WalletSummary from '../../components/wallet/summary/WalletSummary';
import WalletNoTransactions from '../../components/wallet/home/WalletNoTransactions';
import Request from '../../stores/lib/Request';
import AdaRedemptionSuccessOverlay from '../../components/wallet/ada-redemption/AdaRedemptionSuccessOverlay';

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
      adaRedemption: PropTypes.shape({
        showAdaRedemptionSuccessMessage: PropTypes.bool.isRequired,
        amountRedeemed: PropTypes.number.isRequired,
      }),
    }).isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const actions = this.props.actions;
    const { wallets, transactions, adaRedemption } = this.props.stores;
    const {
      hasAny,
      totalAvailable,
      recent,
      recentTransactionsRequest,
      totalUnconfirmedAmount
    } = transactions;
    const wallet = wallets.active;
    const { showAdaRedemptionSuccessMessage, amountRedeemed } = adaRedemption;
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
      <div style={{ height: '100%', display: 'flex', flexDirection: 'column', position: 'relative' }}>
        <WalletSummary
          walletName={wallet.name}
          amount={wallet.amount}
          numberOfTransactions={totalAvailable}
          pendingAmount={totalUnconfirmedAmount}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
        />
        {walletTransactions}
        {showAdaRedemptionSuccessMessage && (
          <AdaRedemptionSuccessOverlay
            amount={amountRedeemed}
            onClose={actions.closeAdaRedemptionSuccessOverlay}
          />
        )}
      </div>
    );
  }

}
