// @flow
import React, { Component } from 'react';
import { get } from 'lodash';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletSummary from '../../components/wallet/summary/WalletSummary';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import { formattedWalletAmount } from '../../utils/formatters';
import { WalletSyncStateTags } from '../../domains/Wallet';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.summary.no.transactions',
    defaultMessage: '!!!No recent transactions',
    description: 'Message shown when wallet has no transactions on wallet summary page.'
  },
});

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class WalletSummaryPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { ada, app } = this.props.stores;
    const { wallets, transactions } = ada;
    const { openExternalLink } = app;
    const {
      hasAny,
      totalAvailable,
      recent,
      recentTransactionsRequest,
      unconfirmedAmount,
    } = transactions;
    const wallet = wallets.active;
    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletSummaryPage.');

    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    const isRestoreActive = get(wallet, 'syncState.tag') === WalletSyncStateTags.RESTORING;

    if (recentTransactionsRequest.isExecutingFirstTime || hasAny || isRestoreActive) {
      walletTransactions = (
        <WalletTransactionsList
          key={`WalletTransactionsList_${wallet.id}`}
          transactions={recent}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
          hasMoreToLoad={false}
          assuranceMode={wallet.assuranceMode}
          walletId={wallet.id}
          isRestoreActive={isRestoreActive}
          formattedWalletAmount={formattedWalletAmount}
          showMoreTransactionsButton={totalAvailable > 5}
          onOpenExternalLink={openExternalLink}
          onShowMoreTransactions={this.handleShowMoreTransaction}
        />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <VerticalFlexContainer>
        <WalletSummary
          walletName={wallet.name}
          amount={wallet.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
          numberOfTransactions={totalAvailable}
          pendingAmount={unconfirmedAmount}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }

  handleShowMoreTransaction = (walletId: string) => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: { id: walletId, page: 'transactions' },
    });
  };
}
