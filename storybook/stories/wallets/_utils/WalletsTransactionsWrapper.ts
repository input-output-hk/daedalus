import { Component } from 'react';
import { computed } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment';
// Assets and helpers
import {
  generateTransaction,
  generateMultipleTransactions,
  generateHash,
} from '../../_support/utils';
import {
  generateFilterOptions,
  isTransactionInFilterRange,
} from '../../../../source/renderer/app/utils/transaction';
import {
  TransactionStates,
  TransactionTypes,
} from '../../../../source/renderer/app/domains/WalletTransaction';
import { emptyTransactionFilterOptions } from '../../../../source/renderer/app/stores/TransactionsStore';
import type { TransactionFilterOptionsType } from '../../../../source/renderer/app/stores/TransactionsStore';
import { WALLET_ASSETS_ENABLED } from '../../../../source/renderer/app/config/walletsConfig';

type Props = {
  getStory: (...args: Array<any>) => any;
  locale: string;
  transactionsOption: string;
};
type State = {
  filterOptions: TransactionFilterOptionsType;
};
const assets = [
  {
    id: generateHash(),
    policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    assetName: '',
    quantity: new BigNumber(100),
    uniqueId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
  },
  {
    id: generateHash(),
    policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    assetName: '',
    quantity: new BigNumber(200),
    uniqueId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
  },
  {
    id: generateHash(),
    policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    assetName: '',
    quantity: new BigNumber(300),
    uniqueId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
  },
  {
    id: generateHash(),
    policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    assetName: '',
    quantity: new BigNumber(400),
    uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
  },
];
export default class WalletsTransactionsWrapper extends Component<
  Props,
  State
> {
  // @ts-ignore ts-migrate(2416) FIXME: Property 'state' in type 'WalletsTransactionsWrapp... Remove this comment to see the full error message
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
          new BigNumber(1),
          TransactionStates.OK
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(1),
          new BigNumber(1),
          TransactionStates.PENDING
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(2019, 10, 8, 20),
          new BigNumber(1),
          new BigNumber(1),
          TransactionStates.PENDING,
          true
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(13),
          new BigNumber(1),
          TransactionStates.FAILED
        ),
      ],
      renderingManyTransactions: generateMultipleTransactions(500),
      unresolvedIncomeAddresses: [
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(1),
          new BigNumber(1),
          TransactionStates.OK,
          true
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
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
          new BigNumber(1),
          TransactionStates.OK,
          false,
          true
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
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
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          false
        ),
      ],
    };
  }

  get transactionsWithAssetsOptions() {
    return {
      groupedByDays: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          moment().subtract(1, 'days').toDate(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          moment().subtract(2, 'days').toDate(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          moment().subtract(1, 'days').toDate(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
      ],
      confirmedAndPendingTransactions: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.PENDING,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(2019, 10, 8, 20),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.PENDING,
          true,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(13),
          new BigNumber(1),
          TransactionStates.FAILED,
          false,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
      ],
      renderingManyTransactions: generateMultipleTransactions(500),
      unresolvedIncomeAddresses: [
        generateTransaction(
          TransactionTypes.EXPEND,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          true,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          true,
          false,
          true,
          new BigNumber(0.012345),
          assets
        ),
      ],
      withoutIncomeAddresses: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          true,
          true,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          true,
          true,
          new BigNumber(0.012345),
          assets
        ),
      ],
      withWithdrawalAddresses: [
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          false,
          new BigNumber(0.012345),
          assets
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(200),
          new BigNumber(1),
          TransactionStates.OK,
          false,
          false,
          false,
          new BigNumber(0.012345),
          assets
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
      ? this.transactionsWithAssetsOptions[transactionsOption]
      : this.transactionsOptions[transactionsOption];
    return transactionsList.filter((transaction) =>
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ dateRange: string; fromDate: s... Remove this comment to see the full error message
      isTransactionInFilterRange(filterOptions, transaction)
    );
  }

  @computed
  get defaultFilterOptions(): TransactionFilterOptionsType {
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ dateRange: string; fromDate: string; toDat... Remove this comment to see the full error message
    return generateFilterOptions(this.transactions);
  }

  @computed
  get populatedFilterOptions(): TransactionFilterOptionsType {
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ dateRange: string; fromDate: string; toDat... Remove this comment to see the full error message
    return this.state.filterOptions || emptyTransactionFilterOptions;
  }

  onFilter = (filterOptions: TransactionFilterOptionsType) => {
    this.setState({
      filterOptions,
    });
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
