// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
// Assets and helpers
import {
  generateAsset,
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../../_support/utils';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import WalletsWrapper from '../_utils/WalletsWrapper';
import WalletsTransactionsWrapper from '../_utils/WalletsTransactionsWrapper';
import {
  DATE_ENGLISH_OPTIONS,
  // LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';
import { WalletTransaction } from '../../../../source/renderer/app/domains/WalletTransaction';
import type { TransactionFilterOptionsType } from '../../../../source/renderer/app/stores/TransactionsStore';
// Screens
import WalletTransactions from '../../../../source/renderer/app/components/wallet/transactions/WalletTransactions';
import { WALLET_ASSETS_ENABLED } from '../../../../source/renderer/app/config/walletsConfig';
import Asset from '../../../../source/renderer/app/domains/Asset';

type Props = {
  defaultFilterOptions: TransactionFilterOptionsType,
  filterOptions: TransactionFilterOptionsType,
  locale: string,
  onFilter: Function,
  populatedFilterOptions: TransactionFilterOptionsType,
  transactions: Array<WalletTransaction>,
  totalAvailable: number,
};

const assetDetails = {
  '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c': generateAsset(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
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
  '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b': generateAsset(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2342',
    {
      name: 'TrueUSD',
      ticker: 'TUSD',
      description: 'Test description',
      unit: {
        name: 'TUSD',
        decimals: 6,
      },
      url: 'http://example.com',
      logo: '',
    }
  ),
  '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b': generateAsset(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2542',
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
  '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b': generateAsset(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    'tokenb0ca10391caaf66a4d4d2897d281f3c136cd3513136945b2542',
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
};

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};

const getAssetDetails = (policyId: string, assetName: string): ?Asset =>
  assetDetails[policyId + assetName];

/* eslint-disable consistent-return */
storiesOf('Wallets|Transactions', module)
  .addDecorator(withKnobs)
  .addDecorator((getStory, props) => {
    const transactionsOption = select(
      'Transactions',
      {
        'Grouped by days': 'groupedByDays',
        'Confirmed and pending transactions': 'confirmedAndPendingTransactions',
        'Rendering many transactions': 'renderingManyTransactions',
        'Unresolved income addresses': 'unresolvedIncomeAddresses',
        'Without income addresses': 'withoutIncomeAddresses',
        'With withdrawal addresses': 'withWithdrawalAddresses',
      },
      'groupedByDays'
    );
    return (
      <WalletsTransactionsWrapper
        {...props}
        transactionsOption={transactionsOption}
        getStory={getStory}
      />
    );
  })
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('Transactions List', (props: Props) => {
    const {
      defaultFilterOptions,
      filterOptions,
      locale,
      onFilter,
      populatedFilterOptions,
      transactions,
      totalAvailable,
    } = props;
    return (
      <WalletTransactions
        activeWallet={generateWallet('Wallet name', '45119903750165', assets)}
        currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentLocale={locale}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
        defaultFilterOptions={defaultFilterOptions}
        filterOptions={filterOptions}
        deletePendingTransaction={action('deletePendingTransaction')}
        formattedWalletAmount={formattedWalletAmount}
        getUrlByType={action('getUrlByType')}
        hasMoreToLoad={false}
        isInternalAddress={() => {
          return true;
        }}
        isDeletingTransaction={false}
        isLoadingTransactions={false}
        onFilter={onFilter}
        onLoadMore={action('onLoadMore')}
        onOpenExternalLink={action('onOpenExternalLink')}
        onRequestCSVFile={action('onRequestCSVFile')}
        populatedFilterOptions={populatedFilterOptions}
        totalAvailable={totalAvailable}
        transactions={transactions}
        hasAssetsEnabled={false}
        getAssetDetails={getAssetDetails}
        onCopyAssetItem={() => {}}
      />
    );
  })
  .add('Wallet Tokens Transactions List', (props: Props) => {
    const {
      defaultFilterOptions,
      filterOptions,
      locale,
      onFilter,
      populatedFilterOptions,
      transactions,
      totalAvailable,
    } = props;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    return (
      <WalletTransactions
        activeWallet={generateWallet('Wallet name', '45119903750165', assets)}
        currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentLocale={locale}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
        defaultFilterOptions={defaultFilterOptions}
        filterOptions={filterOptions}
        deletePendingTransaction={action('deletePendingTransaction')}
        formattedWalletAmount={formattedWalletAmount}
        getUrlByType={action('getUrlByType')}
        hasMoreToLoad={false}
        isInternalAddress={() => {
          return true;
        }}
        isDeletingTransaction={false}
        isLoadingTransactions={false}
        onFilter={onFilter}
        onLoadMore={action('onLoadMore')}
        onOpenExternalLink={action('onOpenExternalLink')}
        onRequestCSVFile={action('onRequestCSVFile')}
        populatedFilterOptions={populatedFilterOptions}
        totalAvailable={totalAvailable}
        transactions={transactions}
        hasAssetsEnabled={hasAssetsEnabled}
        getAssetDetails={getAssetDetails}
        onCopyAssetItem={() => {}}
      />
    );
  });
