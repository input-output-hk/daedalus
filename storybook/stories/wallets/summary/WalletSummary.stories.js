// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, number } from '@storybook/addon-knobs';

// Assets and helpers
import { action } from '@storybook/addon-actions';
import {
  generateAsset,
  generateHash,
  generateWallet,
} from '../../_support/utils';
import WalletsWrapper from '../_utils/WalletsWrapper';

// Screens
import WalletSummary from '../../../../source/renderer/app/components/wallet/summary/WalletSummary';
import AssetsWalletSummary from '../../../../source/renderer/app/components/wallet/summary/AssetsWalletSummary';
import { WALLET_ASSETS_ENABLED } from '../../../../source/renderer/app/config/walletsConfig';

const allAssets = [
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    {
      name: 'TrueUSD',
      acronym: 'TUSD',
      description: 'Test description',
      unit: {
        name: 'TUSD',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAsset(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    {
      name: 'Tether',
      acronym: 'USDT',
      description: 'Test description',
      unit: {
        name: 'USDT',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAsset(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    {
      name: 'USD Coin',
      acronym: 'USDC',
      description: 'Test description',
      unit: {
        name: 'USDC',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    {
      name: 'MakerDAO',
      acronym: 'DAI',
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

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 100,
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 200,
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: 300,
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: 400,
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 100,
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 200,
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: 300,
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: 400,
    },
  ],
};

const walletAssets = assets.total.map((assetTotal) => {
  const assetData = allAssets.find(
    (item) => item.policyId === assetTotal.policyId
  );
  return {
    id: assetData ? assetData.id : '',
    metadata: assetData
      ? assetData.metadata
      : {
          name: '',
          acronym: '',
          description: '',
        },
    total: assetTotal || {},
  };
});

const hasAssetsEnabled = WALLET_ASSETS_ENABLED;

/* eslint-disable consistent-return */
storiesOf('Wallets|Summary', module)
  .addDecorator(WalletsWrapper)
  .add('Wallet Summary', () => (
    <WalletSummary
      wallet={generateWallet('Wallet name', '45119903750165', assets)}
      numberOfTransactions={number('Number of transactions', 100)}
      numberOfRecentTransactions={number('Number of Recent transactions', 100)}
      numberOfPendingTransactions={number('Number of transactions', 3)}
      isLoadingTransactions={boolean('isLoadingTransactions', false)}
    />
  ))
  .add('Tokens Wallet Summary', () => (
    <>
      <WalletSummary
        wallet={generateWallet('Wallet name', '45119903750165', assets)}
        numberOfTransactions={number('Number of transactions', 100)}
        numberOfRecentTransactions={number(
          'Number of Recent transactions',
          100
        )}
        numberOfPendingTransactions={number('Number of transactions', 3)}
        isLoadingTransactions={boolean('isLoadingTransactions', false)}
        hasAssetsEnabled={
          hasAssetsEnabled && assets && assets.total && assets.total.length > 0
        }
      />
      <AssetsWalletSummary
        wallet={generateWallet('Wallet name', '45119903750165', assets)}
        assets={walletAssets}
        handleOpenAssetSend={action('handleOpenAssetSend')}
      />
    </>
  ));
