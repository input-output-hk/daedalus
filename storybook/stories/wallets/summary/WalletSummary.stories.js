// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, number } from '@storybook/addon-knobs';

// Assets and helpers
import { generateWallet } from '../../_support/utils';
import WalletsWrapper from '../_utils/WalletsWrapper';

// Screens
import WalletSummary from '../../../../source/renderer/app/components/wallet/summary/WalletSummary';

/* eslint-disable consistent-return */
storiesOf('Wallets|Summary', module)
  .addDecorator(WalletsWrapper)
  .add('Summary', () => (
    <WalletSummary
      wallet={generateWallet('Wallet name', '45119903750165')}
      numberOfTransactions={number('Number of transactions', 100)}
      numberOfRecentTransactions={number('Number of Recent transactions', 100)}
      numberOfPendingTransactions={number('Number of transactions', 3)}
      isLoadingTransactions={boolean('isLoadingTransactions', false)}
    />
  ));
