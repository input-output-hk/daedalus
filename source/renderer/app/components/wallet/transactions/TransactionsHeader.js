// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
// import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import FilterDialogContainer from '../dialogs/FilterDialogContainer';
// /Users/danilo/iohk/daedalus/source/renderer/app/containers/wallet/dialogs/FilterDialogContainer.js
// /Users/danilo/iohk/daedalus/source/renderer/app/components/wallet/transactions/TransactionsHeader.js
import FilterDialog from './FilterDialog';
import FilterButton from '../../components/wallet/transactions/FilterButton';
import FilterResultInfo from '../../components/wallet/transactions/FilterResultInfo';
import TinyButton from '../../components/widgets/forms/TinyButton';
import type { InjectedProps } from '../../types/injectedPropsType';
import { formattedWalletAmount } from '../../utils/formatters';
import { getNumberOfFilterDimensionsApplied } from '../../utils/transaction';
import type { TransactionFilterOptionsType } from '../../stores/TransactionsStore';
import { getNetworkExplorerUrlByType } from '../../utils/network';

export const messages = defineMessages({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message shown when wallet has no transactions yet.',
  },
});

type Props = {};
type State = {
  isFilterButtonFaded: boolean,
};

@observer
export default class TransactionsHeader extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isFilterButtonFaded: false,
  };

  componentDidMount() {
    const { dialogs } = this.props.actions;
    dialogs.closeActiveDialog.trigger();
  }

  openFilterDialog = () => {
    const { dialogs } = this.props.actions;
    const {
      defaultFilterOptions,
      populatedFilterOptions,
    } = this.props.stores.transactions;
    const { currentNumberFormat: numberFormat } = this.props.stores.profile;

    this.setState({ isFilterButtonFaded: false });
    dialogs.open.trigger({ dialog: FilterDialog });
    dialogs.updateDataForActiveDialog.trigger({
      data: {
        defaultFilterOptions,
        populatedFilterOptions,
        numberFormat,
      },
    });
  };

  onFilter = (filterProps: TransactionFilterOptionsType) => {
    const {
      transactions: transactionActions,
      dialogs: dialogActions,
    } = this.props.actions;
    transactionActions.filterTransactions.trigger(filterProps);
    dialogActions.closeActiveDialog.trigger();
  };

  setFilterButtonFaded = (isFilterButtonFaded: boolean) =>
    this.setState({ isFilterButtonFaded });

  render() {
    const { intl } = this.context;
    const { actions, stores } = this.props;
    const { isFilterButtonFaded } = this.state;
    const { app, uiDialogs, wallets, profile } = stores;
    const {
      openExternalLink,
      environment: { network, rawNetwork },
    } = app;
    const activeWallet = wallets.active;
    const {
      filterOptions,
      searchRequest,
      hasAny,
      totalAvailable,
      allFiltered,
      recentFiltered,
      deletePendingTransaction,
      deleteTransactionRequest,
    } = stores.transactions;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;

    // Guard against potential null values
    if (!filterOptions || !activeWallet) return null;

    let walletTransactions = null;
    const { searchLimit } = filterOptions;
    const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
      filterOptions
    );
    const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    const hasMoreToLoad = () =>
      searchLimit !== null &&
      searchLimit !== undefined &&
      totalAvailable > searchLimit;

    return (
      <div>
        <FilterDialogContainer />
        {hasAny && (
          <FilterButton
            numberOfFilterDimensionsApplied={numberOfFilterDimensionsApplied}
            faded={isFilterButtonFaded}
            onClick={this.openFilterDialog}
          />
        )}
      </div>
    );
  }
}
