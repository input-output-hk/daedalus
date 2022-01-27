import React from 'react';
import BigNumber from 'bignumber.js';
import { storiesOf } from '@storybook/react';
import { boolean, number, select, text } from '@storybook/addon-knobs';
// Assets and helpers
import { action } from '@storybook/addon-actions';
import {
  generateAssetToken,
  generateHash,
  generateWallet,
} from '../../_support/utils';
import WalletsWrapper from '../_utils/WalletsWrapper';
import currenciesList from '../../../../source/renderer/app/config/currenciesList.json';
// Screens
import WalletSummary from '../../../../source/renderer/app/components/wallet/summary/WalletSummary';

const allAssets = [
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 4.
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z1234',
    100
  ),
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 4.
  generateAssetToken(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z2345',
    100
  ),
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  generateAssetToken(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z3456',
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
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'token1rjklcrnsdzqp65wjgrg55sy9723kw09m5z4567',
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
];
const assets = {
  available: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      uniqueId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(45119903.750165),
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      uniqueId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      uniqueId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      assetName: '',
      quantity: new BigNumber(400),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      uniqueId: '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(45119903.750165),
    },
    {
      id: generateHash(),
      policyId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      uniqueId: '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      uniqueId: '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
      assetName: '',
      quantity: new BigNumber(300),
    },
    {
      id: generateHash(),
      policyId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
      uniqueId: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
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
    uniqueId: assetTotal.policyId + assetTotal.assetName,
    fingerprint,
    quantity: assetTotal.quantity,
    decimals: 0,
    recommendedDecimals: null,
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
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
  .add('Wallet Summary', ({ locale }: { locale: string }) => {
    const currencyState = select(
      'Currency state',
      {
        Fetched: 'fetched',
        'Fetching rate': 'loading',
        'Disabled or unavailable': 'off',
      },
      'fetched',
      'Currency'
    );
    let currencyIsFetchingRate = false;
    let currencyIsActive = true;
    let currencyLastFetched = new Date();

    if (currencyState === 'loading') {
      currencyIsFetchingRate = true;
      currencyLastFetched = null;
    } else if (currencyState === 'off') {
      currencyIsActive = false;
    }

    const currencySelected = select(
      'currencySelected',
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ aed: { code: string; decimalDi... Remove this comment to see the full error message
      currenciesList,
      {
        id: 'uniswap-state-dollar',
        symbol: 'usd',
        name: 'unified Stable Dollar',
      },
      'Currency'
    );
    const [firstAsset] = walletAssets;
    return (
      <WalletSummary
        wallet={generateWallet('Wallet name', '45119903750165', assets)}
        numberOfTransactions={number(
          'Number of transactions',
          100,
          {},
          'Header'
        )}
        numberOfRecentTransactions={number(
          'Number of Recent transactions',
          100,
          {},
          'Header'
        )}
        numberOfPendingTransactions={number(
          'Number of pending transactions',
          0,
          // @ts-ignore ts-migrate(2559) FIXME: Type '"Header"' has no properties in common with t... Remove this comment to see the full error message
          'Header'
        )}
        isLoadingTransactions={boolean(
          'isLoadingTransactions',
          false,
          'Header'
        )}
        currentLocale={locale}
        currencyIsFetchingRate={currencyIsFetchingRate}
        currencyIsActive={currencyIsActive}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        currencySelected={currencySelected}
        currencyRate={0.321}
        onToggleFavorite={action('onToggleFavorite')}
        tokenFavorites={{}}
        currencyLastFetched={currencyLastFetched}
        onCurrencySettingClick={action('onCurrencySettingClick')}
        assets={[
          {
            ...firstAsset,
            quantity: new BigNumber(number('quantity', 100, {}, 'First Asset')),
            decimals: number('decimals', 0, {}, 'First Asset'),
            recommendedDecimals: number(
              'recommendedDecimals',
              0,
              {},
              'First Asset'
            ),
            metadata: {
              name: text('Metadata - name', 'FIRST', 'First Asset'),
              ticker: text('Metadata - ticker', '', 'First Asset'),
              description: text('Metadata - description', '', 'First Asset'),
            },
          },
          ...walletAssets.slice(1),
        ]}
        isLoadingAssets={boolean('isLoadingAssets', false)}
        onOpenAssetSend={action('onOpenAssetSend')}
        onCopyAssetParam={action('onCopyAsset')}
        onAssetSettings={action('onAssetSettings')}
        hasAssetsEnabled={boolean('hasAssetsEnabled', true)}
        assetSettingsDialogWasOpened={boolean(
          'assetSettingsDialogWasOpened',
          true
        )}
        onExternalLinkClick={action('onExternalLinkClick')}
        onViewAllButtonClick={action('onViewAllButtonClick')}
      />
    );
  });
