// @flow
import React, { Component } from 'react';
import { take, get } from 'lodash';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { MAX_TRANSACTIONS_ON_SUMMARY_PAGE } from '../../config/numbersConfig';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletSummary from '../../components/wallet/summary/WalletSummary';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import { formattedWalletAmount } from '../../utils/formatters';
import { WalletSyncStateStatuses } from '../../domains/Wallet';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.summary.no.transactions',
    defaultMessage: '!!!No recent transactions',
    description:
      'Message shown when wallet has no transactions on wallet summary page.',
  },
});

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSummaryPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleShowMoreTransaction = (walletId: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: { id: walletId, page: 'transactions' },
    });
  };

  render() {
    const { intl } = this.context;
    const { app, wallets, transactions, profile } = this.props.stores;
    const {
      openExternalLink,
      environment: { network },
    } = app;
    const {
      hasAny,
      totalAvailable,
      recent,
      recentTransactionsRequest,
      unconfirmedAmount,
      deletePendingTransaction,
      deleteTransactionRequest,
    } = transactions;
    const wallet = wallets.active;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    // Guard against potential null values
    if (!wallet)
      throw new Error('Active wallet required for WalletSummaryPage.');

    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    const isRestoreActive =
      get(wallet, ['syncState', 'status'], '') ===
      WalletSyncStateStatuses.RESTORING;

    if (
      recentTransactionsRequest.isExecutingFirstTime ||
      hasAny ||
      isRestoreActive
    ) {
      walletTransactions = (
        <WalletTransactionsList
          key={`WalletTransactionsList_${wallet.id}`}
          transactions={take(recent, MAX_TRANSACTIONS_ON_SUMMARY_PAGE)}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
          hasMoreToLoad={false}
          deletePendingTransaction={deletePendingTransaction}
          walletId={wallet.id}
          isDeletingTransaction={deleteTransactionRequest.isExecuting}
          isRestoreActive={isRestoreActive}
          formattedWalletAmount={formattedWalletAmount}
          showMoreTransactionsButton={
            recent.length > MAX_TRANSACTIONS_ON_SUMMARY_PAGE
          }
          network={network}
          onOpenExternalLink={openExternalLink}
          onShowMoreTransactions={this.handleShowMoreTransaction}
          totalAvailable={totalAvailable}
          currentTimeFormat={currentTimeFormat}
          currentDateFormat={currentDateFormat}
          currentLocale={currentLocale}
        />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <VerticalFlexContainer>
        <WalletSummary
          wallet={wallet}
          numberOfRecentTransactions={recent.length}
          numberOfTransactions={totalAvailable}
          pendingAmount={unconfirmedAmount}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
          isRestoreActive={isRestoreActive}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }
}
