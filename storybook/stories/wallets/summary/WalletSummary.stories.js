// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, number, select } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

// Assets and helpers
import { generateWallet } from '../../_support/utils';
import WalletsWrapper from '../_utils/WalletsWrapper';
import currencyList from '../_utils/currencies.json';

// Screens
import WalletSummary from '../../../../source/renderer/app/components/wallet/summary/WalletSummary';

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
  });

// wallet: Wallet,
// numberOfRecentTransactions: number,
// numberOfTransactions?: number,
// numberOfPendingTransactions: number,
// isLoadingTransactions: boolean,
// currencyIsFetchingRate: boolean,
// currencyIsAvailable: boolean,
// currencyIsActive: boolean,
// currencySelected: ?Currency,
// currencyRate: ?number,
// currencyLastFetched: ?Date,
// onCurrencySettingClick: Function,
