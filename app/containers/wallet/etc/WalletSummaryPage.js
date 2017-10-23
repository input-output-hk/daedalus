// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import WalletSummary from '../../../components/wallet/summary/etc/WalletSummary';
import WalletNoTransactions from '../../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../../components/layout/VerticalFlexContainer';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import type { InjectedProps } from '../../../types/injectedPropsType';

const messages = defineMessages({
  noTransactions: {
    id: 'wallet.summary.no.transactions',
    defaultMessage: '!!!No recent transactions',
    description: 'Message shown when wallet has no transactions on wallet summary page.'
  },
});

@inject('stores', 'actions') @observer
export default class WalletSummaryPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { wallets } = this.props.stores.etc;
    const wallet = wallets.active;

    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletSummaryPage.');

    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;

    return (
      <VerticalFlexContainer>
        <WalletSummary
          walletName={wallet.name}
          amount={wallet.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
        />
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }

}
