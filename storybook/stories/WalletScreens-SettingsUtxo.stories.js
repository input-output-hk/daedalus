// @flow
import React from 'react';
import { number } from '@storybook/addon-knobs';
import { linkTo } from '@storybook/addon-links';
import BigNumber from 'bignumber.js';

// Screens
import WalletSettingsLayout from '../../source/renderer/app/components/wallet/settings/WalletSettingsLayout';
import WalletSettingsMenu from '../../source/renderer/app/components/wallet/settings/WalletSettingsMenu';
import WalletUtxoSettings from '../../source/renderer/app/components/wallet/settings-utxo/WalletUtxoSettings';

/* eslint-disable react/display-name  */

export default ({ story }: { story: string }) => (
  <WalletSettingsLayout
    menu={
      <WalletSettingsMenu
        onItemClick={linkTo('WalletScreens', story)}
        isActiveItem={item => item === '/wallets/:id/settings/utxo'}
      />
    }
  >
    <WalletUtxoSettings
      walletAmount={
        new BigNumber(
          number('Amount', 66.998, {
            range: true,
            step: 1,
            min: 0,
            max: 9999,
          })
        )
      }
      walletUtxosAmount={number('UTxOs', 100, {
        range: true,
        step: 1,
        min: 0,
        max: 1000,
      })}
      chartData={[
        {
          walletAmount: 0.00001,
          walletUtxosAmount: number('walletUtxosAmount 0.00001', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: 0.0001,
          walletUtxosAmount: number('walletUtxosAmount 0.0001', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: 0.001,
          walletUtxosAmount: number('walletUtxosAmount 0.001', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: 0.01,
          walletUtxosAmount: number('walletUtxosAmount 0.01', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: 0.1,
          walletUtxosAmount: number('walletUtxosAmount 0.1', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: 1,
          walletUtxosAmount: number('walletUtxosAmount 1', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: 10,
          walletUtxosAmount: number('walletUtxosAmount 10', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: 100,
          walletUtxosAmount: number('walletUtxosAmount 100', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '1K',
          walletUtxosAmount: number('walletUtxosAmount 1K', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '10K',
          walletUtxosAmount: number('walletUtxosAmount 10K', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '100K',
          walletUtxosAmount: number('walletUtxosAmount 100K', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '1M',
          walletUtxosAmount: number('walletUtxosAmount 1M', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '10M',
          walletUtxosAmount: number('walletUtxosAmount 10M', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '100M',
          walletUtxosAmount: number('walletUtxosAmount 100M', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '1B',
          walletUtxosAmount: number('walletUtxosAmount 1B', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '10B',
          walletUtxosAmount: number('walletUtxosAmount 10B', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
        {
          walletAmount: '45B',
          walletUtxosAmount: number('walletUtxosAmount 45B', 0, {
            range: true,
            step: 1,
            min: 0,
            max: 1000,
          }),
        },
      ]}
    />
  </WalletSettingsLayout>
);
