// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import WalletSummary from '../../../components/wallet/summary/etc/WalletSummary';
import WalletNoTransactions from '../../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../../components/layout/VerticalFlexContainer';
import { DECIMAL_PLACES_IN_ETC } from '../../../config/numbersConfig';
import type { InjectedProps } from '../../../types/injectedPropsType';
import WalletTransactionsList from '../../../components/wallet/transactions/WalletTransactionsList';
import { messages } from '../WalletSummaryPage';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class WalletSummaryPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { wallets, transactions } = this.props.stores.etc;
    const {
      hasAny,
      recent,
      recentTransactionsRequest,
    } = transactions;
    const wallet = wallets.active;

    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletSummaryPage.');

    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    let walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;

    if (recentTransactionsRequest.isExecutingFirstTime || hasAny) {
      walletTransactions = (
        <WalletTransactionsList
          key={`WalletTransactionsList_${wallet.id}`}
          transactions={recent}
          isLoadingTransactions={recentTransactionsRequest.isExecutingFirstTime}
          hasMoreToLoad={false}
          onLoadMore={() => {}}
          assuranceMode={wallet.assuranceMode}
          walletId={wallet.id}
        />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    // Format wallet amount into Integer and Decimal part
    const amount = wallet.amount.toFormat(DECIMAL_PLACES_IN_ETC);
    const amountParts = amount.split('.');
    const amountIntegerPart = amountParts[0];
    const amountDecimalPart = amountParts[1];

    return (
      <VerticalFlexContainer>
        <WalletSummary
          walletName={wallet.name}
          amountInteger={amountIntegerPart}
          amountDecimal={amountDecimalPart}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }

}
