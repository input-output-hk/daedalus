// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import StoryDecorator from './support/StoryDecorator';
import {
  generateMultipleTransactions,
  generateTransaction,
} from './support/utils';
import WalletTransactionsList from '../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import {
  TransactionStates,
  TransactionTypes,
} from '../../source/renderer/app/domains/WalletTransaction';
import { formattedWalletAmount } from '../../source/renderer/app/utils/formatters';

storiesOf('WalletTransactionsList', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('transactions grouped by days', () => (
    <WalletTransactionsList
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
      isRestoreActive={false}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={5}
    />
  ))

  .add('failed and pending transactions', () => (
    <WalletTransactionsList
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
          new Date(),
          new BigNumber(1),
          TransactionStates.FAILED
        ),
      ]}
      isRestoreActive={false}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={3}
    />
  ))

  .add('rendering many transactions', () => (
    <WalletTransactionsList
      isRenderingAsVirtualList
      isRestoreActive={false}
      transactions={generateMultipleTransactions(500)}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={500}
    />
  ));
