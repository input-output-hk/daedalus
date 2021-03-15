// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
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
  generateAsset(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z4567',
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

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(45119903.750165),
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(45119903.750165),
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
    },
  ],
};

const walletAssets = assets.total.map((assetTotal) => {
  const assetData = allAssets.find(
    (item) => item.policyId === assetTotal.policyId
  );
  let fingerprint;
  if (!assetData || !assetData.fingerprint) {
    fingerprint = `token${assetTotal.policyId}${assetTotal.assetName}`.substr(
      0,
      44
    );
  } else {
    fingerprint = assetData.fingerprint;
  }

  return {
    policyId: assetTotal.policyId,
    assetName: assetTotal.assetName,
    fingerprint,
    quantity: assetTotal.quantity,
    metadata: assetData
      ? assetData.metadata
      : {
          name: '',
          ticker: '',
          description: '',
        },
  };
});

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
        wallet={generateWallet('Wallet name', '45119903750165', assets)}
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
        assets={walletAssets}
        isLoadingAssets={boolean('isLoadingAssets', false)}
        onOpenAssetSend={action('onOpenAssetSend')}
        onCopyAssetItem={action('onCopyAsset')}
        hasAssetsEnabled={boolean('hasAssetsEnabled', true)}
        onExternalLinkClick={action('onExternalLinkClick')}
      />
    );
  });
