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
import {
  DATE_ENGLISH_OPTIONS,
  // LANGUAGE_OPTIONS,
  // NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';

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
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
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
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
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
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
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
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
    />
  ))

  .add('transactions without income addresses', () => (
    <WalletTransactionsList
      currentLocale="en-US"
      isRenderingAsVirtualList
      deletePendingTransaction={() => {}}
      isRestoreActive={false}
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
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
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
    />
  ));
