// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import BigNumber from 'bignumber.js';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, number } from '@storybook/addon-knobs';
import { withState } from '@dump247/storybook-state';
import StoryDecorator from '../_support/StoryDecorator';
import DappTransactionRequest from '../../../source/renderer/app/components/dapp/DappTransactionRequest';
import { WALLETS_V2 } from '../_support/StoryProvider';
import { generateAssetToken } from '../_support/utils';

const allAssets = [
  generateAssetToken(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z2345',
    200
  ),
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z4567',
    50,
    {
      name: 'MakerDAO',
      ticker: 'DAI',
      description: 'Test description',
      unit: {
        name: 'DAI',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
];

storiesOf('dApps|TransactionRequest', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add(
    'Request',
    withState({ selectedWallet: null }, (store) => {
      const { selectedWallet } = store.state;
      const wallets = WALLETS_V2.forEach((wallet, index) => ({
        ...wallet,
        assets: index === 0 ? allAssets.slice(0) : allAssets,
        name: index === 0 ? `${wallet.name} - Missing token` : wallet.name,
      }));
      return (
        <DappTransactionRequest
          address="addr1zCqrhsvWEPg886YEtnjN3vLXhFBHsc6j7oZ3pXzuwgZquGUT4fuztk43fHZnBhQKMnojvyxhFBHsc6j7oZ3pXzuwgZq"
          onClose={action('onClose')}
          onSubmit={action('onSubmit')}
          onSelectWallet={(walletId) => {
            const newSelectedWallet = WALLETS_V2.find(
              ({ id }) => id === walletId
            );
            store.set({ selectedWallet: newSelectedWallet });
          }}
          selectedWallet={selectedWallet}
          triggedFrom={select(
            'triggedFrom',
            {
              safari: 'safari',
              chrome: 'chrome',
            },
            'safari'
          )}
          wallets={wallets}
          assets={allAssets}
          feesAmount={new BigNumber(number('feesAmount', 100))}
          additionalData={{
            appName: 'daedalus',
            action: 'convert',
            state: 'true',
          }}
        />
      );
    })
  );
