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
import StoryProvider from '../../_support/StoryProvider';
import {
  generateHash,
  generatePolicyIdHash, // generate eAsset,
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
    ticker: 'DAI',
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
    ticker: 'TUSD',
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
    ticker: 'USDT',
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
    ticker: 'USDC',
    description: 'Test description',
    unit: {
      name: 'USDC',
      decimals: 6,
    },
    url: 'http://example.com',
    logo: '',
  },
];
const transactionTokens = [
  {
    policyId: generatePolicyIdHash(),
    uniqueId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca20391caaf66a4d4e7897d282f9c136cd3513136945c2542',
  },
  {
    policyId: generatePolicyIdHash(),
    uniqueId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2342',
  },
  {
    policyId: generatePolicyIdHash(),
    uniqueId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca20391caaf66a4d4d7897d281f9c136cd3513136945b2542',
  },
  {
    policyId: generatePolicyIdHash(),
    uniqueId: generatePolicyIdHash(),
    assetName: '',
    quantity: new BigNumber(200),
    address: '65bc72542b0ca20391caaf66a4d4e7897d282f9c136cd3513136945c',
    fingerprint: 'tokenb0ca10391caaf66a4d4d2897d281f3c136cd3513136945b2542',
  },
];

const feedbackTransactions = [
  {
    id: 'submitted-self-transfer',
    state: TransactionStates.PENDING,
    transferAmount: new BigNumber(12),
    amount: new BigNumber('-0.17'),
    isSelfTransfer: true,
    amountIsKnown: true,
  },
  {
    id: 'confirmed-self-transfer',
    state: TransactionStates.OK,
    transferAmount: new BigNumber(12),
    amount: new BigNumber('-0.17'),
    isSelfTransfer: true,
    amountIsKnown: true,
  },
  {
    id: 'unknown-submission',
    state: TransactionStates.SUBMISSION_UNKNOWN,
    amount: new BigNumber(0),
    isSelfTransfer: false,
    amountIsKnown: false,
  },
].map(
  (feedback) =>
    new WalletTransaction({
      ...feedback,
      confirmations: 0,
      slotNumber: null,
      epochNumber: null,
      title: '',
      type: TransactionTypes.EXPEND,
      fee: new BigNumber('0.17'),
      deposit: new BigNumber(0),
      assets: [],
      date: new Date('2026-09-09T12:00:00.000Z'),
      description: '',
      addresses: {
        from: ['addr_test1selfsender'],
        to: ['addr_test1selfreceiver'],
        withdrawals: [],
      },
      metadata: null,
      hasCertificates: false,
    })
);

/* eslint-disable consistent-return */
storiesOf('Wallets / Transactions', module)
  .addDecorator(withKnobs)
  .addDecorator((story, context) => (
    <StoryProvider>
      <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
    </StoryProvider>
  )) // ====== Stories ======
  .add('Transaction', () => {
    const direction = select(
      'direction',
      {
        outgoing: 'Sent',
        incoming: 'Received',
      },
      'incoming'
    );
    const tokens = [
      {
        ...transactionTokens[0],
        quantity: new BigNumber(number('amount', 10, {}, 'First Asset')),
      },
      ...transactionTokens.slice(1),
    ];
    const decimals = number('decimals', 1, {}, 'First Asset');
    const hasMetadata = boolean('hasMetadata', true, 'First Asset');
    const assetTokens = tokens.map((token, index) => ({
      ...token,
      uniqueId: token.policyId + token.assetName,
      decimals: 0,
      recommendedDecimals: null,
      metadata:
        index === 0
          ? hasMetadata && {
              name: text('md - name', 'MakerDAO', 'First Asset'),
              ticker: text('md - ticker', 'DAO', 'First Asset'),
              description: text(
                'md - description',
                'Test description',
                'First Asset'
              ),
              unit: {
                name: text('md - unit name', 'DAI', 'First Asset'),
                decimals,
              },
            }
          : assetsMetadata[index],
    }));
    const amount = new BigNumber(number('amount', 10, {}, 'Transaction'));
    const transaction = new WalletTransaction({
      id: generateHash(),
      confirmations: number('confirmations', 10, {}, 'Transaction'),
      slotNumber: number('slotNumber', 10, {}, 'Transaction'),
      epochNumber: number('epochNumber', 10, {}, 'Transaction'),
      // @ts-ignore ts-migrate(2367) FIXME: This condition will always return 'false' since th... Remove this comment to see the full error message
      title: direction === 'outgoing' ? 'Ada sent' : 'Ada received',
      type:
        // @ts-ignore ts-migrate(2367) FIXME: This condition will always return 'false' since th... Remove this comment to see the full error message
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
      assets: tokens,
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
        isExpanded={boolean('isExpanded', true, 'Transaction')}
        isRestoreActive={boolean('isRestoreActive', false, 'Transaction')}
        isLastInList={boolean('isLastInList', false)}
        isShowingMetadata={boolean('isShowingMetadata', false)}
        isDeletingTransaction={boolean('isDeletingTransaction', false)}
        hasAssetsEnabled={boolean('hasAssetsEnabled', true)}
        isLoadingAssets={boolean('isLoadingAssets', false)}
        currentTimeFormat="hh:mm:ss A"
        walletId={generateHash()}
        assetTokens={assetTokens}
        onShowMetadata={action('onShowMetadata')}
        getUrlByType={action('getUrlByType')}
        deletePendingTransaction={action('deletePendingTransaction')}
        formattedWalletAmount={action('formattedWalletAmount')}
        onDetailsToggled={action('onDetailsToggled')}
        onOpenExternalLink={action('onOpenExternalLink')}
        isInternalAddress={() => direction === 'incoming'}
        onCopyAssetParam={action('onCopyAssetParam')}
      />
    );
  })
  .add('Submitted, confirmed, and unknown outcomes', () => (
    <div>
      {feedbackTransactions.map((transaction) => (
        <Transaction
          key={transaction.id}
          data={transaction}
          state={transaction.state}
          isExpanded={false}
          isRestoreActive={false}
          isLastInList={false}
          isShowingMetadata={false}
          isDeletingTransaction={false}
          hasAssetsEnabled
          isLoadingAssets={false}
          currentTimeFormat="hh:mm:ss A"
          walletId="feedback-wallet"
          assetTokens={[]}
          onShowMetadata={action('onShowMetadata')}
          getUrlByType={action('getUrlByType')}
          deletePendingTransaction={action('deletePendingTransaction')}
          formattedWalletAmount={action('formattedWalletAmount')}
          onDetailsToggled={action('onDetailsToggled')}
          onOpenExternalLink={action('onOpenExternalLink')}
          isInternalAddress={(address) => address.startsWith('addr_test1self')}
          onCopyAssetParam={action('onCopyAssetParam')}
        />
      ))}
    </div>
  ));
