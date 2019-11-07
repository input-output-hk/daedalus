// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import faker from 'faker';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';
import { BigNumber } from 'bignumber.js';
import { set } from 'lodash';
import StoryDecorator from './support/StoryDecorator';
import TransferFundsStep1Dialog from '../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep1Dialog';
import TransferFundsStep2Dialog from '../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep2Dialog';

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

const step1WalletOptions = wallets.reduce(
  (options, wallet) => ({
    ...options,
    ...set({}, wallet.name, wallet),
  }),
  {}
);
const step1WalletIdOptions = wallets.reduce(
  (options, wallet) => ({
    ...options,
    ...set({}, wallet.name, wallet.id),
  }),
  {}
);
storiesOf('WalletTransferFunds', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('TransferFundsStep1Dialog', () => {
    const step1WalletSelect = select(
      'walletFrom',
      step1WalletOptions,
      wallets[1]
    );
    const step1WalletSelectValue = {
      ...step1WalletSelect,
      amount: new BigNumber(step1WalletSelect.amount),
    };
    return (
      <TransferFundsStep1Dialog
        onClose={action('onClose')}
        onContinue={action('onContinue')}
        onSetSourceWallet={action('onSetSourceWallet')}
        walletFrom={step1WalletSelectValue}
        targetWalletId={select(
          'targetWalletId',
          step1WalletIdOptions,
          wallets[0].id
        )}
        wallets={wallets}
      />
    );
  })
  .add('TransferFundsStep2Dialog', () => (
    <TransferFundsStep2Dialog
      onContinue={action('onContinue')}
      onClose={action('onClose')}
      onBack={action('onBack')}
      addresses={[
        'YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM',
        'YbDziZoPjGmJdsgUUJVsagauySKyCqBbpYmQd6yTdA1DUHeFQqfbcEskrDMdVX1h32Fi94Np1gvwMWWKx9gs1ojx9MxMur',
      ]}
      amount="3"
      fees="+ 12.042481"
      total="15.042481"
      sourceWallet={wallets[0]}
      targetWallet={wallets[1]}
    />
  ));
