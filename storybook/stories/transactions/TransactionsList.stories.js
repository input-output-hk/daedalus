// TODO: Move these stories into transactions and addresses domains

// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import BigNumber from 'bignumber.js';
import moment from 'moment';

// Assets and helpers
import {
  generateTransaction,
  generateMultipleTransactions,
} from '../_support/utils';
import { formattedWalletAmount } from '../../../source/renderer/app/utils/formatters';
import {
  TransactionStates,
  TransactionTypes,
} from '../../../source/renderer/app/domains/WalletTransaction';
import WalletsWrapper from '../wallets/utils/WalletsWrapper';

// Screens
import WalletTransactionsList from '../../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';

/* eslint-disable consistent-return */
storiesOf('Transactions|Transactions', module)
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('transactions grouped by days', () => (
    <WalletTransactionsList
      currentLocale="en-US"
      transactions={[
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          moment()
            .subtract(1, 'days')
            .toDate(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          moment()
            .subtract(2, 'days')
            .toDate(),
          new BigNumber(1)
        ),
        generateTransaction(
          TransactionTypes.INCOME,
          moment()
            .subtract(1, 'days')
            .toDate(),
          new BigNumber(1)
        ),
      ]}
      deletePendingTransaction={() => {}}
      isRestoreActive={false}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      isDeletingTransaction={false}
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={5}
    />
  ))

  .add('confirmed and pending transactions', () => (
    <WalletTransactionsList
      currentLocale="en-US"
      transactions={[
        generateTransaction(
          TransactionTypes.INCOME,
          new Date(),
          new BigNumber(1),
          TransactionStates.OK
        ),
        generateTransaction(
          TransactionTypes.INCOME,
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
      ]}
      deletePendingTransaction={() => {}}
      isRestoreActive={false}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      isDeletingTransaction={false}
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={3}
    />
  ))

  .add('rendering many transactions', () => (
    <WalletTransactionsList
      currentLocale="en-US"
      isRenderingAsVirtualList
      isRestoreActive={false}
      transactions={generateMultipleTransactions(500)}
      deletePendingTransaction={() => {}}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      isDeletingTransaction={false}
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={500}
    />
  ))

  .add('transactions with unresolved income addresses', () => (
    <WalletTransactionsList
      currentLocale="en-US"
      isRenderingAsVirtualList
      deletePendingTransaction={() => {}}
      isRestoreActive={false}
      transactions={[
        generateTransaction(
          TransactionTypes.INCOME,
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
      ]}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      isDeletingTransaction={false}
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={3}
    />
  ))

  .add('transactions without income addresses', () => (
    <WalletTransactionsList
      currentLocale="en-US"
      isRenderingAsVirtualList
      deletePendingTransaction={() => {}}
      isRestoreActive={false}
      transactions={[
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
      ]}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      isDeletingTransaction={false}
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={3}
    />
  ));
