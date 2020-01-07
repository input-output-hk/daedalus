// @flow
import React, { Component } from 'react';
import { get } from 'lodash';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import SVGInline from 'react-svg-inline';
import WalletTransactionsList from '../../components/wallet/transactions/WalletTransactionsList';
import WalletNoTransactions from '../../components/wallet/transactions/WalletNoTransactions';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import FilterDialogContainer from './dialogs/FilterDialogContainer';
import FilterDialog from '../../components/wallet/transactions/FilterDialog';
import filterIcon from '../../assets/images/filter-dis-ic.inline.svg';
import type { InjectedProps } from '../../types/injectedPropsType';
import { formattedWalletAmount } from '../../utils/formatters';
import { WalletSyncStateStatuses } from '../../domains/Wallet';
import { getNetworkExplorerUrlByType } from '../../utils/network';
import globalMessages from '../../i18n/global-messages';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message shown when wallet has no transactions yet.',
  },
  noTransactionsFound: {
    id: 'wallet.transactions.no.transactions.found',
    defaultMessage: '!!!No transactions found',
    description:
      'Message shown when wallet transaction search returns zero results.',
  },
});

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletTransactionsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  openFilterDialog = () => {
    const { dialogs } = this.props.actions;

    dialogs.open.trigger({ dialog: FilterDialog });
    dialogs.updateDataForActiveDialog.trigger({
      data: {
        // minDate: new Date('2019-09-23').getTime(),
        // maxDate: new Date('2019-12-19').getTime(),
        // minAmount: new BigNumber(200),
        // maxAmount: new BigNumber(1200),
        isFiltering: true,
        onFilter: payload => console.log('---here--', payload),
      },
    });
  };

  render() {
    const { intl } = this.context;
    const { actions, stores } = this.props;
    const { app, uiDialogs, wallets, profile } = stores;
    const {
      openExternalLink,
      environment: { network, rawNetwork },
    } = app;
    const activeWallet = wallets.active;
    const {
      searchOptions,
      searchRequest,
      hasAny,
      totalAvailable,
      filtered,
      recent,
      deletePendingTransaction,
      deleteTransactionRequest,
    } = stores.transactions;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;

    // Guard against potential null values
    if (!searchOptions || !activeWallet) return null;

    const { searchLimit, searchTerm } = searchOptions;
    const wasSearched = searchTerm !== '';
    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const noTransactionsFoundLabel = intl.formatMessage(
      messages.noTransactionsFound
    );
    const hasMoreToLoad = () =>
      searchLimit !== null && totalAvailable > searchLimit;

    const isRestoreActive =
      get(activeWallet, ['syncState', 'status']) ===
      WalletSyncStateStatuses.RESTORING;

    const getUrlByType = (type: 'tx' | 'address', param: string) =>
      getNetworkExplorerUrlByType(
        type,
        param,
        network,
        rawNetwork,
        currentLocale
      );

    // Straight away show recent transactions if filtered ones are not loaded yet
    const transactions = recent.length && !filtered.length ? recent : filtered;

    if (searchRequest.isExecutingFirstTime || hasAny || isRestoreActive) {
      walletTransactions = (
        <WalletTransactionsList
          transactions={transactions}
          deletePendingTransaction={deletePendingTransaction}
          isLoadingTransactions={searchRequest.isExecutingFirstTime}
          isRestoreActive={isRestoreActive}
          hasMoreToLoad={hasMoreToLoad()}
          onLoadMore={actions.transactions.loadMoreTransactions.trigger}
          walletId={activeWallet.id}
          isDeletingTransaction={deleteTransactionRequest.isExecuting}
          formattedWalletAmount={formattedWalletAmount}
          onOpenExternalLink={openExternalLink}
          getUrlByType={getUrlByType}
          currentTimeFormat={currentTimeFormat}
          currentDateFormat={currentDateFormat}
          isRenderingAsVirtualList
        />
      );
    } else if (wasSearched && !hasAny) {
      walletTransactions = (
        <WalletNoTransactions label={noTransactionsFoundLabel} />
      );
    } else if (!hasAny) {
      walletTransactions = <WalletNoTransactions label={noTransactionsLabel} />;
    }

    return (
      <VerticalFlexContainer>
        <button onClick={() => this.openFilterDialog()}>Filter</button>
        {uiDialogs.isOpen(FilterDialog) && <FilterDialogContainer />}
        {walletTransactions}
      </VerticalFlexContainer>
    );
  }
}
