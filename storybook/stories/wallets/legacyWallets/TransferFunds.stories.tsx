// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, boolean, number } from '@storybook/addon-knobs';
import { set } from 'lodash';
import { BigNumber } from 'bignumber.js';
import TransferFundsStep1Dialog from '../../../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep1Dialog';
import TransferFundsStep2Dialog from '../../../../source/renderer/app/components/wallet/transfer-funds/TransferFundsStep2Dialog';
import { WALLETS_V2 } from '../../_support/StoryProvider';
import STAKE_POOLS from '../../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { LOVELACES_PER_ADA } from '../../../../source/renderer/app/config/numbersConfig';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

storiesOf('Wallets|Legacy Wallets', module)
  .addDecorator(WalletsWrapper)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Transfer Funds - Step1', () => {
    const walletOptions = WALLETS_V2.reduce(
      (options, wallet) => ({
        ...options,
        ...set({}, wallet.name, wallet),
      }),
      {}
    );
    const walletIdOptions = WALLETS_V2.reduce(
      (options, wallet) => ({
        ...options,
        ...set({}, wallet.name, wallet.id),
      }),
      {}
    );
    const sourceWalletSelect = select(
      'sourceWallet',
      walletOptions,
      WALLETS_V2[1]
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
          WALLETS_V2[0].id
        )}
        wallets={WALLETS_V2}
        numberOfStakePools={STAKE_POOLS}
        getStakePoolById={action('getStakePoolById')}
        isSubmitting={false}
        error={null}
      />
    );
  })
  .add('Transfer Funds - Step2', () => {
    const feesNumber = number('fees', 1, {
      range: true,
      min: 1,
      max: 5,
      step: 1,
    });
    const feesAmount = BigNumber(feesNumber);
    const leftoversNumber = number('leftovers (Lovelaces)', 0, {
      range: true,
      min: 0,
      max: 10,
      step: 1,
    });
    const leftoversAmount = new BigNumber(leftoversNumber).dividedBy(
      LOVELACES_PER_ADA
    );
    const sourceWalletNumber = number('sourceWalletBalance', 50, {
      range: true,
      min: 10,
      max: 3000,
      step: 1,
    });
    const sourceWalletAmount = BigNumber(sourceWalletNumber);
    return (
      <TransferFundsStep2Dialog
        feesAmount={feesAmount}
        leftoversAmount={leftoversAmount}
        sourceWalletAmount={sourceWalletAmount}
        sourceWalletName="Source Wallet"
        targetWalletName="Target Wallet"
        onBack={action('onBack')}
        onClose={action('onClose')}
        onContinue={action('onContinue')}
        onDataChange={action('onDataChange')}
        isSubmitting={boolean('isSubmitting', false)}
        onFinish={action('onFinish')}
        onOpenExternalLink={action('onOpenExternalLink')}
      />
    );
  });
