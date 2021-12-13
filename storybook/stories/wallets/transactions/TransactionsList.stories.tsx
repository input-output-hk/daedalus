import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
// Assets and helpers
import {
  generateAssetToken,
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../../_support/utils';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import WalletsWrapper from '../_utils/WalletsWrapper';
import WalletsTransactionsWrapper from '../_utils/WalletsTransactionsWrapper';
import {
  DATE_ENGLISH_OPTIONS, // LANGUAGE_OPTIONS,
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
  defaultFilterOptions: TransactionFilterOptionsType;
  filterOptions: TransactionFilterOptionsType;
  locale: string;
  onFilter: (...args: Array<any>) => any;
  populatedFilterOptions: TransactionFilterOptionsType;
  transactions: Array<WalletTransaction>;
  totalAvailable: number;
};
const assetDetails = {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c': generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    '',
    'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
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
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b': generateAssetToken(
    '65bc72542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2342',
    100,
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
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b': generateAssetToken(
    '65ac82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b',
    '',
    'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2542',
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
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b': generateAssetToken(
    '65cn72542b0ca10391caaf66a4d4d2897d281f3c136cd3513136945b',
    '',
    'tokenb0ca10391caaf66a4d4d2897d281f3c136cd3513136945b2542',
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
};
const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};

const getAsset = (
  policyId: string,
  assetName: string
): Asset | null | undefined => assetDetails[`${policyId}${assetName}`];

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
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      <WalletsTransactionsWrapper
        {...props}
        transactionsOption={transactionsOption}
        getStory={getStory}
      />
    );
  })
  .addDecorator(WalletsWrapper) // ====== Stories ======
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: Props) => JSX.Element' i... Remove this comment to see the full error message
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
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
        getAsset={getAsset}
        onCopyAssetParam={() => {}}
      />
    );
  })
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: Props) => JSX.Element' i... Remove this comment to see the full error message
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
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
        getAsset={getAsset}
        onCopyAssetParam={() => {}}
      />
    );
  });
