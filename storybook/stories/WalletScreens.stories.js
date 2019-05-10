// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
// import { action } from '@storybook/addon-actions';
import { linkTo } from '@storybook/addon-links';
import { withKnobs, text, boolean, number } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import faker from 'faker';
import startCase from 'lodash/startCase';

// Assets and helpers
import StoryLayout from './support/StoryLayout';
import StoryProvider from './support/StoryProvider';
import StoryDecorator from './support/StoryDecorator';
import {
  generateWallet,
  generateTransaction,
  generateAddress,
  promise,
} from './support/utils';
import { formattedWalletAmount } from '../../source/renderer/app/utils/formatters';
import { transactionTypes } from '../../source/renderer/app/domains/WalletTransaction';
import WalletWithNavigation from '../../source/renderer/app/components/wallet/layouts/WalletWithNavigation';

// Screens
import WalletSummary from '../../source/renderer/app/components/wallet/summary/WalletSummary';
import WalletSendForm from '../../source/renderer/app/components/wallet/WalletSendForm';
import WalletReceive from '../../source/renderer/app/components/wallet/receive/WalletReceive';
import WalletTransactionsList from '../../source/renderer/app/components/wallet/transactions/WalletTransactionsList';
import WalletScreensSettings from './WalletScreens-Settings.stories';
import WalletScreensUtxo from './WalletScreens-Utxo.stories';

/* eslint-disable consistent-return */
storiesOf('WalletScreens', module)
  .addDecorator((story, context) => {
    const storyWithKnobs = withKnobs(story, context);

    const getItemFromContext = () =>
      context.story
        .replace('Wallet UTXO distribution', 'utxo')
        .toLocaleLowerCase();

    return (
      <StoryDecorator>
        <StoryProvider>
          <StoryLayout
            activeSidebarCategory="/wallets"
            storyName={context.story}
          >
            {context.story !== 'Empty' ? (
              <WalletWithNavigation
                isActiveScreen={item => item === getItemFromContext()}
                onWalletNavItemClick={linkTo('WalletScreens', item =>
                  item === 'utxo' ? 'Wallet UTXO distribution' : startCase(item)
                )}
                activeItem={getItemFromContext()}
              >
                {storyWithKnobs}
              </WalletWithNavigation>
            ) : (
              storyWithKnobs
            )}
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })

  // ====== Stories ======

  .add('Empty', () => null)

  .add('Wallet Navigation', () => <div>&nbsp;</div>)

  .add('Summary', () => (
    <WalletSummary
      wallet={generateWallet('Wallet name', '45119903750165')}
      pendingAmount={{
        incoming: new BigNumber(number('Incoming', 1)),
        outgoing: new BigNumber(number('Outgoing', 2)),
        total: new BigNumber(3),
      }}
      numberOfTransactions={number('Number of transactions', 20303585)}
      numberOfRecentTransactions={50}
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
      addressValidator={() => {}}
      openDialogAction={() => {}}
      isDialogOpen={() => boolean('hasDialog', false)}
      isRestoreActive={boolean('isRestoreActive', false)}
    />
  ))

  .add('Receive', () => (
    <WalletReceive
      walletAddress={text(
        'Wallet address',
        '5628aab8ac98c963e4a2e8cfce5aa1cbd4384fe2f9a0f3c5f791bfb83a5e02ds'
      )}
      isWalletAddressUsed={boolean('isWalletAddressUsed', false)}
      walletAddresses={[
        ...Array.from(Array(number('Addresses', 1))).map(() =>
          generateAddress()
        ),
        ...Array.from(Array(number('Addresses (used)', 1))).map(() =>
          generateAddress(true)
        ),
      ]}
      onGenerateAddress={() => {}}
      onCopyAddress={() => {}}
      isSidebarExpanded
      walletHasPassword={boolean('walletHasPassword', false)}
      isSubmitting={boolean('isSubmitting', false)}
    />
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

  .add('Settings', WalletScreensSettings)
  .add('Wallet UTXO distribution', WalletScreensUtxo);
