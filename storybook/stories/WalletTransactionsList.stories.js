// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import faker from 'faker';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import StoryDecorator from './support/StoryDecorator';
import WalletTransactionsList from '../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import WalletTransaction, {
  transactionStates,
  transactionTypes
} from '../../source/renderer/app/domains/WalletTransaction';
import { formattedWalletAmount } from '../../source/renderer/app/utils/ada/formatters';

const generateTransaction = (
  type, date, amount, confirmations = 1, state = transactionStates.OK
) => (
  new WalletTransaction({
    id: faker.random.uuid(),
    title: '',
    type,
    amount,
    date,
    state,
    description: '',
    numberOfConfirmations: confirmations,
    addresses: {
      from: [faker.random.uuid()], to: [faker.random.uuid()]
    },
  })
);

storiesOf('WalletTransactionsList', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('transactions grouped by days', () => (
    <WalletTransactionsList
      transactions={[
        generateTransaction(transactionTypes.INCOME, new Date(), new BigNumber(1)),
        generateTransaction(transactionTypes.INCOME, moment().subtract(1, 'days').toDate(), new BigNumber(1)),
        generateTransaction(transactionTypes.INCOME, new Date(), new BigNumber(1)),
        generateTransaction(transactionTypes.INCOME, moment().subtract(2, 'days').toDate(), new BigNumber(1)),
        generateTransaction(transactionTypes.INCOME, moment().subtract(1, 'days').toDate(), new BigNumber(1)),
      ]}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      assuranceMode={{ low: 1, medium: 2 }}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
    />
  ))

  .add('failed and pending transactions', () => (
    <WalletTransactionsList
      transactions={[
        generateTransaction(
          transactionTypes.INCOME, new Date(), new BigNumber(1), 1, transactionStates.OK
        ),
        generateTransaction(
          transactionTypes.INCOME, new Date(), new BigNumber(1), 0, transactionStates.PENDING
        ),
        generateTransaction(
          transactionTypes.INCOME, new Date(), new BigNumber(1), 0, transactionStates.FAILED
        ),
      ]}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      assuranceMode={{ low: 1, medium: 2 }}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
    />
  ));
