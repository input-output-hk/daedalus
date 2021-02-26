// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import {
  withKnobs,
  select,
  number,
  boolean,
  text,
} from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';

// Screens
import Transaction from '../../../../source/renderer/app/components/wallet/transactions/Transaction';

// Assets and helpers
import StoryDecorator from '../../_support/StoryDecorator';
import {
  generateHash,
  generatePolicyIdHash,
  // generat eAsset,
} from '../../_support/utils';
import {
  WalletTransaction,
  TransactionTypes,
  TransactionStates,
} from '../../../../source/renderer/app/domains/WalletTransaction';
import { LOVELACES_PER_ADA } from '../../../../source/renderer/app/config/numbersConfig';

const date = new Date();

const assetsMetadata = [
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
  },
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
  },
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
  },
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
  },
];

const transactionAssets = [
  {
    policyId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
  },
  {
    policyId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2342',
  },
  {
    policyId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2542',
  },
  {
    policyId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca10391caaf66a4d4d2897d281f3c136cd3513136945b2542',
  },
];

/* eslint-disable consistent-return */
storiesOf('Wallets|Transactions', module)
  .addDecorator(withKnobs)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('Transaction', () => {
    const direction = select(
      'direction',
      {
        outgoing: 'Sent',
        incoming: 'Received',
      },
      'incoming'
    );

    const amount = new BigNumber(number('amount', 10, {}, 'Transaction'));
    const assets = [
      {
        ...transactionAssets[0],
        quantity: new BigNumber(number('amount', 10, {}, 'First Asset')),
      },
      ...transactionAssets.slice(1),
    ];

    const decimals = number('decimals', 1, {}, 'First Asset');
    const hasMetadata = boolean('hasMetadata', true, 'First Asset');

    const assetsDetails = assets.map((txAsset, index) => ({
      ...txAsset,
      metadata:
        index === 0
          ? hasMetadata && {
              name: text('md - name', 'MakerDAO', 'First Asset'),
              acronym: text('md - acronym', 'MakerDAO', 'First Asset'),
              description: text(
                'md - description',
                'Test descriptio',
                'First Asset'
              ),
              unit: {
                name: text('md - unit name', 'DAI', 'First Asset'),
                decimals,
              },
            }
          : assetsMetadata[index],
    }));

    const transaction = new WalletTransaction({
      id: generateHash(),
      confirmations: number('confirmations', 10, {}, 'Transaction'),
      slotNumber: number('slotNumber', 10, {}, 'Transaction'),
      epochNumber: number('epochNumber', 10, {}, 'Transaction'),
      title: direction === 'outgoing' ? 'Ada sent' : 'Ada received',
      type:
        direction === 'outgoing'
          ? TransactionTypes.EXPEND
          : TransactionTypes.INCOME,
      amount,
      fee: new BigNumber(number('fee', 1, {}, 'Transaction')).dividedBy(
        LOVELACES_PER_ADA
      ),
      deposit: new BigNumber(number('deposit', 1, {}, 'Transaction')).dividedBy(
        LOVELACES_PER_ADA
      ),
      assets,
      date,
      description: '',
      addresses: {
        from: ['65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c'],
        to: ['65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c'],
        withdrawals: [],
      },
      state: TransactionStates.OK,
      metadata: {},
    });
    return (
      <Transaction
        data={transaction}
        state={TransactionStates.OK}
        isExpanded={boolean('isExpanded', true)}
        isRestoreActive={boolean('isRestoreActive', false)}
        isLastInList={boolean('isLastInList', false)}
        isShowingMetadata={boolean('isShowingMetadata', false)}
        isDeletingTransaction={boolean('isDeletingTransaction', false)}
        hasAssetsEnabled={boolean('hasAssetsEnabled', true)}
        isLoadingAssets={boolean('isLoadingAssets', false)}
        currentTimeFormat="hh:mm:ss A"
        walletId={generateHash()}
        assetsDetails={assetsDetails}
        onShowMetadata={action('onShowMetadata')}
        getUrlByType={action('getUrlByType')}
        deletePendingTransaction={action('deletePendingTransaction')}
        formattedWalletAmount={action('formattedWalletAmount')}
        onDetailsToggled={action('onDetailsToggled')}
        onOpenExternalLink={action('onOpenExternalLink')}
        isInternalAddress={() => direction === 'incoming'}
        onCopyAssetItem={action('onCopyAssetItem')}
      />
    );
  });
