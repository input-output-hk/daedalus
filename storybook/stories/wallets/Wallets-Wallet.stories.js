// TODO: Move these stories into transactions and addresses domains

// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { text, boolean, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import faker from 'faker';

// Assets and helpers
import VerticalFlexContainer from '../../../source/renderer/app/components/layout/VerticalFlexContainer';
import {
  generateWallet,
  generateTransaction,
  generateAddress,
  promise,
} from '../_support/utils';
import { formattedWalletAmount } from '../../../source/renderer/app/utils/formatters';
import {
  transactionStates,
  transactionTypes,
} from '../../../source/renderer/app/domains/WalletTransaction';
import WalletsWrapper from './WalletsWrapper';

// Screens
import WalletSummary from '../../../source/renderer/app/components/wallet/summary/WalletSummary';
import WalletSendForm from '../../../source/renderer/app/components/wallet/WalletSendForm';
import WalletReceive from '../../../source/renderer/app/components/wallet/receive/WalletReceive';
import WalletTransactionsList from '../../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import WalletsUtxo from './Wallets-Wallet-Utxo.stories';

// Screens sets
import './Wallets-Wallet-Settings.stories';

/* eslint-disable consistent-return */
storiesOf('WALLETS|Wallet', module)
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('Summary', () => (
    <WalletSummary
      wallet={generateWallet('Wallet name', '45119903750165')}
      pendingAmount={{
        incoming: new BigNumber(number('Incoming', 1)),
        outgoing: new BigNumber(number('Outgoing', 2)),
        total: new BigNumber(3),
      }}
      numberOfTransactions={number('Number of transactions', 100)}
      numberOfRecentTransactions={number('Number of Recent transactions', 100)}
      isLoadingTransactions={boolean('isLoadingTransactions', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
    />
  ))

  .add('Send', () => (
    <WalletSendForm
      currencyUnit="Ada"
      currencyMaxFractionalDigits={6}
      currencyMaxIntegerDigits={11}
      validateAmount={promise(true)}
      calculateTransactionFee={promise(true)}
      addressValidator={action('addressValidator')}
      openDialogAction={action('openDialogAction')}
      isDialogOpen={() => boolean('isDialogOpen', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
    />
  ))

  .add('Receive', () => (
    <VerticalFlexContainer>
      <WalletReceive
        walletAddress={text(
          'Wallet address',
          'DdzFFzCqrhsg9ngNRhHEa49se7qEMKyubT9tcE13Fkvh8QC82trpTDsNvdQV7mg9SCZiuENkf77zrtwPXrTyGMNznUsSinPC1gb2ZCqK'
        )}
        isWalletAddressUsed={boolean('isWalletAddressUsed', false)}
        walletAddresses={[
          ...Array.from(Array(number('Addresses', 10))).map(() =>
            generateAddress()
          ),
          ...Array.from(Array(number('Addresses (used)', 10))).map(() =>
            generateAddress(true)
          ),
        ]}
        onGenerateAddress={action('onGenerateAddress')}
        onCopyAddress={action('onGenerateAddress')}
        isSidebarExpanded={boolean('isSidebarExpanded', true)}
        walletHasPassword={boolean('walletHasPassword', false)}
        isSubmitting={boolean('isSubmitting', false)}
      />
    </VerticalFlexContainer>
  ))

  .add('Transactions', () => (
    <WalletTransactionsList
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

  .add('Wallet UTXO distribution', WalletsUtxo);
