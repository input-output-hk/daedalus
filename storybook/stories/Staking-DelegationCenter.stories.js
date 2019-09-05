// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import { number } from '@storybook/addon-knobs';
import DelegationCenter from '../../source/renderer/app/components/staking/delegation-center/DelegationCenter';
import STAKE_POOLS from '../../source/renderer/app/config/stakingStakePools.dummy.json';
import Wallet from '../../source/renderer/app/domains/Wallet';

const defaultAdaValue = 82650.15;
const defaultPercentage = 33.123456;

const adaValueKnob = (name, defaultValue) => {
  const value = number(name, defaultValue);

  return new BigNumber(value);
};

// Dummy data initialization
const wallets = [
  new Wallet({
    id: 'wallet1',
    name: 'Main wallet',
    amount: new BigNumber(100100),
    assurance: 'normal',
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 24,
    isDelegated: true,
    delegatedStakePool: STAKE_POOLS[0],
  }),
  new Wallet({
    id: 'wallet2',
    name: 'Spending money',
    amount: new BigNumber(10100.2),
    assurance: 'normal',
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 35,
    isDelegated: true,
    delegatedStakePool: STAKE_POOLS[1],
  }),
  new Wallet({
    id: 'wallet3',
    name: 'Savings',
    amount: new BigNumber(5001000),
    assurance: 'normal',
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 0,
    isDelegated: false,
  }),
];

export const StakingDelegationCenterStory = () => (
  <DelegationCenter
    adaValue={adaValueKnob('ADA Value', defaultAdaValue)}
    percentage={number('Percentage', defaultPercentage, {
      min: 0,
      max: 100,
      step: 1,
      range: true,
    })}
    wallets={wallets}
    onDelegate={action('onDelegate')}
  />
);
