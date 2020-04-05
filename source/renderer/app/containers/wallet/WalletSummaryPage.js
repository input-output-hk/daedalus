// @flow
import React, { Component } from 'react';
import { take } from 'lodash';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { MAX_TRANSACTIONS_ON_SUMMARY_PAGE } from '../../config/numbersConfig';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletSummary from '../../components/wallet/summary/WalletSummary';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import ChangeSpendingPasswordDialog from '../../components/wallet/settings/ChangeSpendingPasswordDialog';
import ChangeSpendingPasswordDialogContainer from './dialogs/settings/ChangeSpendingPasswordDialogContainer';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import { formattedWalletAmount } from '../../utils/formatters';
import { getNetworkExplorerUrlByType } from '../../utils/network';

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
    const { stores } = this.props;
    const { app, wallets, transactions, profile, uiDialogs } = stores;
    const {
      openExternalLink,
      environment: { network, rawNetwork },
    } = app;
    const {
      hasAny,
      totalAvailable,
      recent,
      recentTransactionsRequest,
      deletePendingTransaction,
      deleteTransactionRequest,
      pendingTransactionsCount,
    } = transactions;
    const { isOpen: isDialogOpen } = uiDialogs;
    const wallet = wallets.active;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    // Guard against potential null values
    if (!wallet)
      throw new Error('Active wallet required for WalletSummaryPage.');

    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(
        type,
        param,
        network,
        rawNetwork,
        currentLocale
      );

    if (
      recentTransactionsRequest.isExecutingFirstTime ||
      hasAny ||
      wallet.isRestoring
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
          isRestoreActive={wallet.isRestoring}
          formattedWalletAmount={formattedWalletAmount}
          showMoreTransactionsButton={
            recent.length > MAX_TRANSACTIONS_ON_SUMMARY_PAGE
          }
          onOpenExternalLink={openExternalLink}
          getUrlByType={getUrlByType}
          onShowMoreTransactions={this.handleShowMoreTransaction}
          totalAvailable={totalAvailable}
          currentTimeFormat={currentTimeFormat}
          currentDateFormat={currentDateFormat}
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
          numberOfPendingTransactions={pendingTransactionsCount}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
        />
        {walletTransactions}
        {isDialogOpen(ChangeSpendingPasswordDialog) ? (
          <ChangeSpendingPasswordDialogContainer forceSetPassword />
        ) : (
          false
        )}
      </VerticalFlexContainer>
    );
  }
}
