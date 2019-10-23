// TODO: Move these stories into transactions and addresses domains

// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, number, select } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import faker from 'faker';

// Assets and helpers
import { generateTransaction } from '../_support/utils';
import { formattedWalletAmount } from '../../../source/renderer/app/utils/formatters';
import {
  transactionStates,
  transactionTypes,
} from '../../../source/renderer/app/domains/WalletTransaction';
import WalletsWrapper from '../wallets/utils/WalletsWrapper';
import {
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
  TIME_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';

// Screens
import WalletTransactionsList from '../../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import { UtxoDistributionStory } from './Utxo.stories';

const timeOptions = TIME_OPTIONS.reduce((obj, { label, value }) => {
  obj[label] = value;
  return obj;
}, {});
const dateOptions = [...DATE_ENGLISH_OPTIONS, ...DATE_JAPANESE_OPTIONS].reduce(
  (obj, { label, value }) => {
    obj[label] = value;
    return obj;
  },
  {}
);

/* eslint-disable consistent-return */
storiesOf('Transactions|Transactions', module)
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('Transactions List', () => (
    <WalletTransactionsList
      currentDateFormat={select(
        'currentDateFormat',
        dateOptions,
        DATE_ENGLISH_OPTIONS[0].value
      )}
      currentTimeFormat={select(
        'currentTimeFormat',
        timeOptions,
        TIME_OPTIONS[0].value
      )}
      transactions={[
        ...Array.from(Array(number('Transactions Sent', 1))).map((x, i) =>
          generateTransaction(
            transactionTypes.EXPEND,
            moment()
              .subtract(i, 'days')
              .toDate(),
            new BigNumber(faker.random.number(5))
          )
        ),
        ...Array.from(Array(number('Transactions Received', 1))).map((x, i) =>
          generateTransaction(
            transactionTypes.INCOME,
            moment()
              .subtract(i, 'days')
              .toDate(),
            new BigNumber(faker.random.number(5))
          )
        ),
        ...Array.from(Array(number('Transactions Pending', 1))).map((x, i) =>
          generateTransaction(
            transactionTypes.INCOME,
            moment()
              .subtract(i, 'days')
              .toDate(),
            new BigNumber(1),
            0,
            transactionStates.PENDING
          )
        ),
        ...Array.from(Array(number('Transactions Failed', 1))).map((x, i) =>
          generateTransaction(
            transactionTypes.INCOME,
            moment()
              .subtract(i, 'days')
              .toDate(),
            new BigNumber(1),
            0,
            transactionStates.FAILED
          )
        ),
      ]}
      isLoadingTransactions={boolean('isLoadingTransactions', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
      hasMoreToLoad={false}
      assuranceMode={{ low: 1, medium: 2 }}
      walletId="test-wallet"
      formattedWalletAmount={formattedWalletAmount}
      totalAvailable={
        number('Transactions Sent', 1) + number('Transactions Received', 1)
      }
    />
  ))
  .add('UTXO Distribution', () => <UtxoDistributionStory />);
