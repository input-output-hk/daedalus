// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Assets and helpers
import { generateWallet } from '../../_support/utils';
import WalletsWrapper from '../_utils/WalletsWrapper';

// Screens
import WalletNativeTokensSummary from '../../../../source/renderer/app/components/wallet/summary/WalletNativeTokensSummary';

/* eslint-disable consistent-return */
storiesOf('Wallets Native Token|Summary', module)
  .addDecorator(WalletsWrapper)
  .add('Wallet Native Token Summary', () => (
    <WalletNativeTokensSummary
      wallet={generateWallet('Wallet name', '45119903750165')}
      nativeTokens={[
        generateWallet('ADA', '55119903750165'),
        generateWallet('Tether', '25119903750165'),
        generateWallet('TrueUSD', '15119903750165'),
        generateWallet('USD Coin', '0'),
      ]}
    />
  ));
