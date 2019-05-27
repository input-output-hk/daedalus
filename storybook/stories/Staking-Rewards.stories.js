// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';

// Screens
import StakingRewards from '../../source/renderer/app/components/staking/rewards/StakingRewards';

// Dummy data initialization
const rewards = [
  {
    date: '15.01.2019',
    wallet: 'Main Wallet',
    amount: 0.2,
    pool: {
      title: 'Help the USA',
      category: 'CATS',
    },
  },

  {
    date: '15.01.2019',
    wallet: 'Spending Money',
    amount: 1.003,
    pool: {
      title: 'Cardano Foundation',
      category: 'CF1',
    },
  },

  {
    date: '10.01.2019',
    wallet: 'Spending Money',
    amount: 2,
    pool: {
      title: 'Micro Mining',
      category: 'MNG',
    },
  },

  {
    date: '10.01.2019',
    wallet: 'Saving',
    amount: 0.9,
    pool: {
      title: 'Blush Pool 1',
      category: 'BLS1',
    },
  },

  {
    date: '10.01.2019',
    wallet: 'Main Wallet',
    amount: 0.2,
    pool: {
      title: 'Blush Pool 2',
      category: 'BLS2',
    },
  },

  {
    date: '05.01.2019',
    wallet: 'Main Wallet',
    amount: 1.6,
    pool: {
      title: 'Spending Money',
      category: 'SPBA',
    },
  },
];

/* eslint-disable react/display-name  */

export default () => (
  <StakingRewards
    rewards={rewards}
    isLoading={false}
    onLearnMoreClick={action('onLearnMoreClick')}
  />
);
