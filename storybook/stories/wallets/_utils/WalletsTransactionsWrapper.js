// @flow
import React, { Component, Children } from 'react';
import { computed } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import { linkTo } from '@storybook/addon-links';
import { get } from 'lodash';
import WalletWithNavigation from '../../../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

// Assets and helpers
import {
  generateWallet,
  generateTransaction,
  generateMultipleTransactions,
} from '../../_support/utils';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import {
  generateFilterOptions,
  isTransactionInFilterRange,
} from '../../../../source/renderer/app/utils/transaction.js';
import {
  TransactionStates,
  TransactionTypes,
} from '../../../../source/renderer/app/domains/WalletTransaction';
import {
  DATE_ENGLISH_OPTIONS,
  // LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';

import { emptyTransactionFilterOptions } from '../../../../source/renderer/app/stores/TransactionsStore';
import type { TransactionFilterOptionsType } from '../../../../source/renderer/app/stores/TransactionsStore';

type Props = {
  children: Node,
  getStory: Function,
  locale: string,
  transactionsOption: string,
};

type State = {
  defaultFilterOptions: TransactionFilterOptionsType,
  filterOptions: TransactionFilterOptionsType,
  populatedFilterOptions: TransactionFilterOptionsType,
};

export default class WalletsTransactionsWrapper extends Component<
  Props,
  State
> {
  state = {
    filterOptions: generateFilterOptions(this.transactions),
  };

  get transactionsOptions() {
    return {
      groupedByDays: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          moment().subtract(1, 'days').toDate(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          moment().subtract(2, 'days').toDate(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          moment().subtract(1, 'days').toDate(),
          new BigNumber(1)
        ),
      ],
      confirmedAndPendingTransactions: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(1),
          TransactionStates.PENDING
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(2019, 10, 8, 20),
          new BigNumber(1),
          TransactionStates.PENDING,
          true
        ),
      ],
      renderingManyTransactions: generateMultipleTransactions(500),
      unresolvedIncomeAddresses: [
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK,
          true
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK,
          true
        ),
      ],
      withoutIncomeAddresses: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          true
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          true
        ),
      ],
      withWithdrawalAddresses: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          false
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          false
        ),
      ],
    };
  }

  get totalAvailable() {
    const { transactionsOption } = this.props;
    return this.transactionsOptions[transactionsOption].length;
  }

  get transactions() {
    const { transactionsOption } = this.props;
    const { filterOptions = emptyTransactionFilterOptions } = this.state || {};
    const transactionsList = this.transactionsOptions[transactionsOption];
    return transactionsList.filter((transaction) =>
      isTransactionInFilterRange(filterOptions, transaction)
    );
  }

  @computed get defaultFilterOptions(): TransactionFilterOptionsType {
    return generateFilterOptions(this.transactions);
  }

  @computed get populatedFilterOptions(): TransactionFilterOptionsType {
    return this.state.filterOptions || emptyTransactionFilterOptions;
  }

  onFilter = (filterOptions: TransactionFilterOptionsType) => {
    this.setState({ filterOptions });
  };

  render() {
    const { getStory, locale } = this.props;
    const { filterOptions } = this.state;
    const {
      defaultFilterOptions,
      onFilter,
      populatedFilterOptions,
      transactions,
      totalAvailable,
    } = this;
    const children = getStory({
      defaultFilterOptions,
      filterOptions,
      locale,
      onFilter,
      populatedFilterOptions,
      transactions,
      totalAvailable,
    });
    return children;
  }
}
