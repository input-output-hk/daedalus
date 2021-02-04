// @flow
import { Component } from 'react';
import { computed } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment';

// Assets and helpers
import {
  generateTransaction,
  generateMultipleTransactions,
  generateNativeTokenTransaction,
} from '../../_support/utils';
import {
  generateFilterOptions,
  isTransactionInFilterRange,
} from '../../../../source/renderer/app/utils/transaction.js';
import {
  TransactionStates,
  TransactionTypes,
} from '../../../../source/renderer/app/domains/WalletTransaction';

import { emptyTransactionFilterOptions } from '../../../../source/renderer/app/stores/TransactionsStore';
import type { TransactionFilterOptionsType } from '../../../../source/renderer/app/stores/TransactionsStore';
import {
  WALLET_ASSETS_ENABLED
} from '../../../../source/renderer/app/config/walletsConfig';

type Props = {
  getStory: Function,
  locale: string,
  transactionsOption: string,
};

type State = {
  filterOptions: TransactionFilterOptionsType,
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
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(13),
          TransactionStates.FAILED
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

  get transactionsNativeTokensOptions() {
    return {
      groupedByDays: [
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          true,
          ['USDT', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.EXPEND,
          moment().subtract(1, 'days').toDate(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          true,
          ['USDC', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          true,
          ['DAI', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.EXPEND,
          moment().subtract(2, 'days').toDate(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          true,
          ['USDT', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          moment().subtract(1, 'days').toDate(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          true,
          ['USDC', 'ADA']
        ),
      ],
      confirmedAndPendingTransactions: [
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          true,
          ['USDT', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(2),
          TransactionStates.PENDING,
          false,
          false,
          true,
          ['DAI', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(2019, 10, 8, 20),
          new BigNumber(2),
          TransactionStates.PENDING,
          true,
          false,
          true,
          ['USDC', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(13),
          TransactionStates.FAILED,
          false,
          false,
          true,
          ['USDT', 'ADA']
        ),
      ],
      renderingManyTransactions: generateMultipleTransactions(500),
      unresolvedIncomeAddresses: [
        generateNativeTokenTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          true,
          false,
          true,
          ['DAI', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          true,
          false,
          true,
          ['USDT', 'ADA']
        ),
      ],
      withoutIncomeAddresses: [
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          true,
          true,
          ['USDC', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          true,
          true,
          ['USDT', 'ADA']
        ),
      ],
      withWithdrawalAddresses: [
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          false,
          ['USDT', 'ADA']
        ),
        generateNativeTokenTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(2),
          TransactionStates.OK,
          false,
          false,
          false,
          ['USDC', 'ADA']
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
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    const transactionsList = hasAssetsEnabled
      ? this.transactionsNativeTokensOptions[transactionsOption]
      : this.transactionsOptions[transactionsOption];
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
