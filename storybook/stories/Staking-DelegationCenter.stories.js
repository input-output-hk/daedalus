// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import { number } from '@storybook/addon-knobs';
import DelegationCenter from '../../source/renderer/app/components/staking/delegation-center/DelegationCenter';

const defaultAdaValue = 82650.15;
const defaultPercentage = 10;

const adaValueKnob = (name, defaultValue) => {
  const value = number(name, defaultValue);

  return new BigNumber(value);
};

// Dummy data initialization
const wallets = [
  {
    id: 'wallet1',
    name: 'Main wallet',
    amount: 100100,
    assurance: 'normal',
    hasPassword: true,
    passwordUpdateDate: null,
    isLegacy: false,
    inactiveStakePercentage: 24,
    isDelegated: true,
    delegatedPoolCategory: 'BLSH',
  },
  {
    id: 'wallet2',
    name: 'Spending money',
    amount: 10100.2,
    assurance: 'normal',
    hasPassword: true,
    passwordUpdateDate: null,
    isLegacy: false,
    inactiveStakePercentage: 35,
    isDelegated: true,
    delegatedPoolCategory: 'BLSH',
  },
  {
    id: 'wallet3',
    name: 'Savings',
    amount: 5001000,
    assurance: 'normal',
    hasPassword: true,
    passwordUpdateDate: null,
    isLegacy: false,
    inactiveStakePercentage: 0,
    isDelegated: false,
    delegatedPoolCategory: '',
  },
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
  />
);
