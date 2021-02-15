// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, number, select } from '@storybook/addon-knobs';

// Assets and helpers
import { action } from '@storybook/addon-actions';
import {
  generateAsset,
  generateHash,
  generateWallet,
} from '../../_support/utils';
import WalletsWrapper from '../_utils/WalletsWrapper';
import currencyList from '../_utils/currencies.json';

// Screens
import WalletSummary from '../../../../source/renderer/app/components/wallet/summary/WalletSummary';
import AssetsWalletSummary from '../../../../source/renderer/app/components/wallet/summary/AssetsWalletSummary';
import { WALLET_ASSETS_ENABLED } from '../../../../source/renderer/app/config/walletsConfig';

const allAssets = [
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z1234'
  ),
  generateAsset(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z2345'
  ),
  generateAsset(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z3456',
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
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z4567',
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
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 45119903.750165,
    },
    {
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 200,
    },
    {
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: 300,
    },
    {
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: 400,
    },
  ],
  total: [
    {
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 45119903.750165,
    },
    {
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: 200,
    },
    {
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: 300,
    },
    {
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
  const { policyId, assetName, quantity } = assetTotal;
  const { metadata, fingerprint } = assetData || {};
  return {
    policyId,
    assetName,
    fingerprint,
    quantity,
    metadata,
  };
});

const hasAssetsEnabled = WALLET_ASSETS_ENABLED;

/* eslint-disable consistent-return */
storiesOf('Wallets|Summary', module)
  .addDecorator(WalletsWrapper)
  .add('Wallet Summary', () => {
    const currencyState = select(
      'Currency state',
      {
        Fetched: 'fetched',
        'Fetching rate': 'loading',
        'Disabled or unavailable': 'off',
      },
      'fetched'
    );

    let currencyIsFetchingRate = false;
    let currencyIsAvailable = true;
    let currencyIsActive = true;
    let currencyLastFetched = new Date();

    if (currencyState === 'loading') {
      currencyIsFetchingRate = true;
      currencyLastFetched = null;
    } else if (currencyState === 'off') {
      currencyIsAvailable = false;
      currencyIsActive = false;
    }

    const currencySelected = select(
      'currencySelected',
      currencyList.reduce((obj, currency) => {
        obj[`${currency.id} - ${currency.name}`] = currency;
        return obj;
      }, {}),
      {
        id: 'uniswap-state-dollar',
        symbol: 'usd',
        name: 'unified Stable Dollar',
      }
    );

    return (
      <WalletSummary
        wallet={generateWallet('Wallet name', '45119903750165')}
        numberOfTransactions={number('Number of transactions', 100)}
        numberOfRecentTransactions={number(
          'Number of Recent transactions',
          100
        )}
        numberOfPendingTransactions={number('Number of transactions', 3)}
        isLoadingTransactions={boolean('isLoadingTransactions', false)}
        currencyIsFetchingRate={currencyIsFetchingRate}
        currencyIsAvailable={currencyIsAvailable}
        currencyIsActive={currencyIsActive}
        currencySelected={currencySelected}
        currencyRate={0.321}
        currencyLastFetched={currencyLastFetched}
        onCurrencySettingClick={action('onCurrencySettingClick')}
      />
    );
  })
  .add('Wallet Tokens Summary Loading', () => {
    const currencyState = select(
      'Currency state',
      {
        Fetched: 'fetched',
        'Fetching rate': 'loading',
        'Disabled or unavailable': 'off',
      },
      'fetched'
    );

    let currencyIsFetchingRate = false;
    let currencyIsAvailable = true;
    let currencyIsActive = true;
    let currencyLastFetched = new Date();

    if (currencyState === 'loading') {
      currencyIsFetchingRate = true;
      currencyLastFetched = null;
    } else if (currencyState === 'off') {
      currencyIsAvailable = false;
      currencyIsActive = false;
    }

    const currencySelected = select(
      'currencySelected',
      currencyList.reduce((obj, currency) => {
        obj[`${currency.id} - ${currency.name}`] = currency;
        return obj;
      }, {}),
      {
        id: 'uniswap-state-dollar',
        symbol: 'usd',
        name: 'unified Stable Dollar',
      }
    );

    return (
      <>
        <WalletSummary
          wallet={generateWallet('Wallet name', '45119903750165', assets)}
          numberOfTransactions={number('Number of transactions', 100)}
          numberOfRecentTransactions={number(
            'Number of Recent transactions',
            100
          )}
          numberOfPendingTransactions={number('Number of transactions', 3)}
          isLoadingTransactions={boolean('isLoadingTransactions', true)}
          hasAssetsEnabled={
            hasAssetsEnabled &&
            assets &&
            assets.total &&
            assets.total.length > 0
          }
          currencyIsFetchingRate={currencyIsFetchingRate}
          currencyIsAvailable={currencyIsAvailable}
          currencyIsActive={currencyIsActive}
          currencySelected={currencySelected}
          currencyRate={0.321}
          currencyLastFetched={currencyLastFetched}
          onCurrencySettingClick={action('onCurrencySettingClick')}
        />
        <AssetsWalletSummary
          wallet={generateWallet('Wallet name', '45119903750165', assets)}
          assets={walletAssets}
          isLoading={boolean('isLoading', true)}
          handleOpenAssetSend={action('handleOpenAssetSend')}
        />
      </>
    );
  })
  .add('Wallet Tokens Summary', () => {
    const currencyState = select(
      'Currency state',
      {
        Fetched: 'fetched',
        'Fetching rate': 'loading',
        'Disabled or unavailable': 'off',
      },
      'fetched'
    );

    let currencyIsFetchingRate = false;
    let currencyIsAvailable = true;
    let currencyIsActive = true;
    let currencyLastFetched = new Date();

    if (currencyState === 'loading') {
      currencyIsFetchingRate = true;
      currencyLastFetched = null;
    } else if (currencyState === 'off') {
      currencyIsAvailable = false;
      currencyIsActive = false;
    }

    const currencySelected = select(
      'currencySelected',
      currencyList.reduce((obj, currency) => {
        obj[`${currency.id} - ${currency.name}`] = currency;
        return obj;
      }, {}),
      {
        id: 'uniswap-state-dollar',
        symbol: 'usd',
        name: 'unified Stable Dollar',
      }
    );

    return (
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
            hasAssetsEnabled &&
            assets &&
            assets.total &&
            assets.total.length > 0
          }
          currencyIsFetchingRate={currencyIsFetchingRate}
          currencyIsAvailable={currencyIsAvailable}
          currencyIsActive={currencyIsActive}
          currencySelected={currencySelected}
          currencyRate={0.321}
          currencyLastFetched={currencyLastFetched}
          onCurrencySettingClick={action('onCurrencySettingClick')}
        />
        <AssetsWalletSummary
          wallet={generateWallet('Wallet name', '45119903750165', assets)}
          assets={walletAssets}
          handleOpenAssetSend={action('handleOpenAssetSend')}
        />
      </>
    );
  });
