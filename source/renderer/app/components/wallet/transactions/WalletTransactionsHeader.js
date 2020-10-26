// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import FilterDialog from './FilterDialog';
import FilterButton from './FilterButton';
import FilterResultInfo from './FilterResultInfo';
import styles from './WalletTransactionsHeader.scss';
import TinyButton from '../widgets/forms/TinyButton';
import type { TransactionFilterOptionsType } from '../../../stores/TransactionsStore';

export const messages = defineMessages({
  transactions: {
    id: 'wallet.transactions.header.transactions',
    defaultMessage: '!!!Transactions',
    description: 'Label for the "Transactions" header.',
  },
});

type Props = {
  transactions: Array<WalletTransaction>,
  filterOptions: TransactionFilterOptionsType,
  currentLocale: string,
  currentDateFormat: string,
  currentNumberFormat: string,
  defaultFilterOptions: TransactionFilterOptionsType,
  populatedFilterOptions: TransactionFilterOptionsType,
  onFilter: Function,
  onClose: Function,
  isScrolling: boolean,
};

type State = {
  isFilterButtonFaded: boolean,
};

@observer
export default class WalletTransactionsHeader extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isFilterButtonFaded: false,
  };

  setFilterButtonFaded = (isFilterButtonFaded: boolean) =>
    this.setState({ isFilterButtonFaded });

  render() {
    const { intl } = this.context;
    const { isFilterButtonFaded } = this.state;
    const {
      transactions,
      filterOptions,
      isScrolling /* , hasAny*/,
    } = this.props;
    // console.log('this.props.hasAny', this.props.hasAny);
    const hasAny = true;

    const componentClassnames = classnames([
      styles.component,
      isScrolling ? styles.isScrolling : null,
    ]);

    return (
      <div className={componentClassnames}>
        <div className={styles.numberOfTransactions}>
          {intl.formatMessage(messages.transactions)} ({transactions.length})
        </div>
        {hasAny && (
          <div className={styles.actions}>
            <TinyButton className={styles.csvButton} loading={false} />
            <FilterButton
              numberOfFilterDimensionsApplied={0}
              faded={isFilterButtonFaded}
              onClick={this.openFilterDialog}
            />
          </div>
        )}
      </div>
    );

    // const {
    //   filterOptions,
    //   searchRequest,
    //   hasAny,
    //   totalAvailable,
    //   allFiltered,
    //   recentFiltered,
    //   deletePendingTransaction,
    //   deleteTransactionRequest,
    // } = stores.transactions;
    // const { currentTimeFormat, currentDateFormat, currentLocale } = profile;

    // // Guard against potential null values
    // if (!filterOptions || !activeWallet) return null;

    // let walletTransactions = null;
    // const { searchLimit } = filterOptions;
    // const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
    //   filterOptions
    // );
    // const noTransactionsLabel = intl.formatMessage(messages.noTransactions);
    // const hasMoreToLoad = () =>
    //   searchLimit !== null &&
    //   searchLimit !== undefined &&
    //   totalAvailable > searchLimit;

    // return (
    //   <div>
    //     <FilterDialogContainer />
    //     {hasAny && (
    //       <FilterButton
    //         numberOfFilterDimensionsApplied={numberOfFilterDimensionsApplied}
    //         faded={isFilterButtonFaded}
    //         onClick={this.openFilterDialog}
    //       />
    //     )}
    //   </div>
    // );
  }
}
