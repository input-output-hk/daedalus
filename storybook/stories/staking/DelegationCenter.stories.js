// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import { number } from '@storybook/addon-knobs';
import DelegationCenter from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenter';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import Wallet from '../../../source/renderer/app/domains/Wallet';
import {
  WalletRecoveryPhraseVerificationStatuses,
  WalletRecoveryPhraseVerificationTypes,
} from '../../../source/renderer/app/stores/WalletsStore';

const defaultAdaValue = 82650.15;
const defaultPercentage = 33.123456;

const adaValueKnob = (name, defaultValue) => {
  const value = number(name, defaultValue);

  return new BigNumber(value);
};

const walletSyncedStateReady = { status: 'ready' };

const walletSyncedStateRestoring = {
  status: 'restoring',
  progress: {
    quantity: 25,
    unit: 'percentage',
  },
};

// Dummy data initialization
const wallets = [
  new Wallet({
    id: 'wallet1',
    addressPoolGap: 20,
    name: 'Main wallet',
    amount: new BigNumber(100100),
    reward: new BigNumber(100),
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 24,
    isDelegated: true,
    syncState: walletSyncedStateReady,
    delegatedStakePool: STAKE_POOLS[0],
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus:
      WalletRecoveryPhraseVerificationStatuses.OK,
    recoveryPhraseVerificationStatusType:
      WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
  }),
  new Wallet({
    id: 'wallet2',
    addressPoolGap: 20,
    name: 'Spending money',
    amount: new BigNumber(10100.2),
    reward: new BigNumber(50),
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 35,
    isDelegated: true,
    syncState: walletSyncedStateReady,
    delegatedStakePool: STAKE_POOLS[1],
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus:
      WalletRecoveryPhraseVerificationStatuses.OK,
    recoveryPhraseVerificationStatusType:
      WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
  }),
  new Wallet({
    id: 'wallet3',
    addressPoolGap: 20,
    name: 'Savings',
    amount: new BigNumber(5001000),
    reward: new BigNumber(30),
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 0,
    isDelegated: false,
    syncState: walletSyncedStateRestoring,
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus:
      WalletRecoveryPhraseVerificationStatuses.OK,
    recoveryPhraseVerificationStatusType:
      WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
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
    numberOfStakePools={STAKE_POOLS.length}
  />
);
