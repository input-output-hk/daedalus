// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, number } from '@storybook/addon-knobs';

// Assets and helpers
import { action } from '@storybook/addon-actions';
import {
  generateAsset,
  generateWallet,
} from '../../_support/utils';
import WalletsWrapper from '../_utils/WalletsWrapper';

// Screens
import WalletSummary from '../../../../source/renderer/app/components/wallet/summary/WalletSummary';
import TokensWalletSummary from '../../../../source/renderer/app/components/wallet/summary/TokensWalletSummary';
import { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet';

/* eslint-disable consistent-return */
storiesOf('Wallets|Summary', module)
  .addDecorator(WalletsWrapper)
  .add('Wallet Summary', () => (
    <WalletSummary
      wallet={generateWallet('Wallet name', '45119903750165')}
      numberOfTransactions={number('Number of transactions', 100)}
      numberOfRecentTransactions={number('Number of Recent transactions', 100)}
      numberOfPendingTransactions={number('Number of transactions', 3)}
      isLoadingTransactions={boolean('isLoadingTransactions', false)}
    />
  ))
  .add('Tokens Wallet Summary', () => (
    <>
      <WalletSummary
        wallet={generateWallet('Wallet name', '45119903750165')}
        numberOfTransactions={number('Number of transactions', 100)}
        numberOfRecentTransactions={number(
          'Number of Recent transactions',
          100
        )}
        numberOfPendingTransactions={number('Number of transactions', 3)}
        isLoadingTransactions={boolean('isLoadingTransactions', false)}
        hasNativeTokens
      />
      <TokensWalletSummary
        wallet={generateWallet('Wallet name', '45119903750165')}
        nativeTokens={[
          generateAsset(
            'Cardano',
            '55119903750165',
            0,
            null,
            false,
            WalletSyncStateStatuses.READY,
            'ADA'
          ),
          generateAsset(
            'Tether',
            '25119903750165',
            0,
            null,
            false,
            WalletSyncStateStatuses.READY,
            'USDT'
          ),
          generateAsset(
            'TrueUSD',
            '15119903750165',
            0,
            null,
            false,
            WalletSyncStateStatuses.READY,
            'TUSD'
          ),
          generateAsset(
            'USD Coin',
            '0',
            0,
            null,
            false,
            WalletSyncStateStatuses.READY,
            'USDC'
          ),
        ]}
        handleOpenWalletTokenSend={action('onContinue')}
      />
    </>
  ));
