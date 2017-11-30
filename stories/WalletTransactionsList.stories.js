import React from 'react';
import { observable, runInAction } from 'mobx';
import { storiesOf } from '@kadira/storybook';
import faker from 'faker';
import moment from 'moment';
import StoryDecorator from './support/StoryDecorator';
import WalletTransactionsList from '../app/components/wallet/transactions/WalletTransactionsList';
import WalletTransaction from '../app/domain/WalletTransaction';
import TrasactionAddresses from '../app/domain/WalletTransaction';
import BigNumber from 'bignumber.js';

const generateTransaction = (
  type, date, amount, confirmations=1, condition='CPtxInBlocks'
) => {
  return new WalletTransaction({
    id: faker.random.uuid(),
    title: '',
    type,
    amount,
    date,
    condition,
    description: '',
    numberOfConfirmations: confirmations,
    addresses: new TrasactionAddresses({
      from: [faker.random.uuid()], to: [faker.random.uuid()]
    }),
  });
};

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
        generateTransaction('income', new Date(), new BigNumber(1)),
        generateTransaction('income', moment().subtract(1, 'days').toDate(), new BigNumber(1)),
        generateTransaction('income', new Date(), new BigNumber(1)),
        generateTransaction('income', moment().subtract(2, 'days').toDate(), new BigNumber(1)),
        generateTransaction('income', moment().subtract(1, 'days').toDate(), new BigNumber(1)),
      ]}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      assuranceMode={{ low: 1, medium: 2 }}
      walletId="test-wallet"
    />
  ))

  .add('failed and pending transactions', () => (
    <WalletTransactionsList
      transactions={[
        generateTransaction('income', new Date(), new BigNumber(1), 1, 'CPtxInBlocks'),
        generateTransaction('income', new Date(), new BigNumber(1), 0, 'CPtxApplying'),
        generateTransaction('income', new Date(), new BigNumber(1), 0, 'CPtxWontApply'),
      ]}
      isLoadingTransactions={false}
      hasMoreToLoad={false}
      assuranceMode={{ low: 1, medium: 2 }}
      walletId="test-wallet"
    />
  ));
