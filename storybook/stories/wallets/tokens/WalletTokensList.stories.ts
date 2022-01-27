// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean, text } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import { withState } from '@dump247/storybook-state';
import StoryDecorator from '../../_support/StoryDecorator';
import StoryProvider from '../../_support/StoryProvider';

import {
  generateAssetToken,
  generateWallet,
  generateHash,
} from '../../_support/utils';

import type { WalletTokens } from '../../../../source/renderer/app/api/assets/types';

// Screens
import WalletTokensList from '../../../../source/renderer/app/components/wallet/tokens/wallet-tokens-list/WalletTokensList';

const assets = [
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2541',
    100,
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
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
    400
  ),
  generateAssetToken(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2544',
    100,
    {
      name: 'Tether',
      ticker: 'USDT',
      description: 'Test description',
      unit: {
        name: 'USDT',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAssetToken(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2543',
    100,
    {
      name: 'USD Coin',
      ticker: 'USDC',
      description: 'Test description',
      unit: {
        name: 'USDC',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4d7s97d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66aad4e7897d282f9c136cd3513136945c2542',
    500,
    {
      name: 'Little',
      ticker: 'LTTL',
      description: '',
    },
    4
  ),
];

const walletTokens: WalletTokens = {
  available: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
      uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(100),
      uniqueId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
      uniqueId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
      uniqueId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
      uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(100),
      uniqueId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
      uniqueId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
      uniqueId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    },
  ],
};

storiesOf('Wallets|Tokens', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs)

  // ====== Stories ======
  .add(
    'WalletTokensList',
    withState({ favorites: {} }, (store) => (
      <WalletTokensList
        assets={boolean('Has Tokens', true) ? assets : []}
        assetSettingsDialogWasOpened
        currentLocale="en-US"
        isLoadingAssets={boolean('isLoadingAssets', false)}
        onAssetSettings={action('onAssetSettings')}
        onCopyAssetParam={action('onCopyAssetParam')}
        onOpenAssetSend={action('onOpenAssetSend')}
        onViewAllButtonClick={
          boolean('Has View All button', false)
            ? action('onViewAllButtonClick')
            : null
        }
        title={text('Title', 'Tokens')}
        wallet={generateWallet('Wallet name', '45119903750165', walletTokens)}
        onToggleFavorite={({ uniqueId }: { uniqueId: string }) => {
          const { favorites } = store.state;
          const newState = {
            ...favorites,
            [uniqueId]: !favorites[uniqueId],
          };
          store.set({ favorites: newState });
        }}
        tokenFavorites={store.state.favorites}
      />
    ))
  );
