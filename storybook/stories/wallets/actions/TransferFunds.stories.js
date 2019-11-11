// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import faker from 'faker';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, boolean } from '@storybook/addon-knobs';
import { BigNumber } from 'bignumber.js';
import { set } from 'lodash';
import TransferFundsStep1Dialog from '../../../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep1Dialog';
import TransferFundsStep2Dialog from '../../../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep2Dialog';

// Helpers
import WalletsWrapper from '../utils/WalletsWrapper';


const wallets = [
  {
    id: '1',
    name: 'Wallet 1',
    amount: new BigNumber(faker.finance.amount()),
  },
  {
    id: '2',
    name: 'Wallet 2',
    amount: new BigNumber(faker.finance.amount()),
  },
];

const addresses = [
  [
    'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
    'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
  ],
  [
    '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',
  ],
];

const walletOptions = wallets.reduce(
  (options, wallet) => ({
    ...options,
    ...set({}, wallet.name, wallet),
  }),
  {}
);
const walletIdOptions = wallets.reduce(
  (options, wallet) => ({
    ...options,
    ...set({}, wallet.name, wallet.id),
  }),
  {}
);
storiesOf('Wallets|Actions|Transfer Funds', module)
  .addDecorator(WalletsWrapper)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('TransferFundsStep1Dialog', () => {
    const sourceWalletSelect = select(
      'sourceWallet',
      walletOptions,
      wallets[1]
    );
    const sourceWallet = {
      ...sourceWalletSelect,
      amount: new BigNumber(sourceWalletSelect.amount),
    };
    return (
      <TransferFundsStep1Dialog
        onClose={action('onClose')}
        onContinue={action('onContinue')}
        onSetSourceWallet={action('onSetSourceWallet')}
        sourceWallet={sourceWallet}
        targetWalletId={select(
          'targetWalletId',
          walletIdOptions,
          wallets[0].id
        )}
        wallets={wallets}
      />
    );
  })
  .add('TransferFundsStep2Dialog', () => {
    const sourceWalletSelect = select(
      'sourceWallet',
      walletOptions,
      wallets[1]
    );
    const targetWalletSelect = select(
      'targetWallet',
      walletOptions,
      wallets[0]
    );
    const sourceWallet = {
      ...sourceWalletSelect,
      amount: new BigNumber(sourceWalletSelect.amount),
    };
    const targetWallet = {
      ...targetWalletSelect,
      amount: new BigNumber(targetWalletSelect.amount),
    };
    const step2Addresses = addresses[parseInt(targetWalletSelect.id, 0) - 1];
    return (
      <TransferFundsStep2Dialog
        addresses={step2Addresses}
        transferFundsFee={faker.finance.amount(1, 20)}
        onBack={action('onBack')}
        onClose={action('onClose')}
        onContinue={action('onContinue')}
        onDataChange={action('onDataChange')}
        sourceWallet={sourceWallet}
        targetWallet={targetWallet}
        isSubmitting={boolean('isSubmitting', false)}
        onFinish={action('onFinish')}
      />
    );
  });
