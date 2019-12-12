// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import { find } from 'lodash';
import BigNumber from 'bignumber.js';
import DelegationCenter from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenter';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import Wallet from '../../../source/renderer/app/domains/Wallet';
import {
  WalletRecoveryPhraseVerificationStatuses,
  WalletRecoveryPhraseVerificationTypes,
} from '../../../source/renderer/app/stores/WalletsStore';
import type {
  NextEpoch,
  TipInfo,
} from '../../../source/renderer/app/api/network/types';

const walletSyncedStateReady = { status: 'ready' };

const walletSyncedStateRestoring = {
  status: 'restoring',
  progress: {
    quantity: 25,
    unit: 'percentage',
  },
};

const redirectToStakingInfo: Function = null;

const networkTip: TipInfo = {
  epoch: 12352,
  slot: 123,
};

const nextEpoch: NextEpoch = {
  epochNumber: 1233,
  epochStart: new Date('2019-12-29').toUTCString(),
};

const futureEpoch: NextEpoch = {
  epochNumber: 1234,
  epochStart: new Date('2019-12-30').toUTCString(),
};

// Dummy data initialization
const wallets = [
  new Wallet({
    id: 'wallet1',
    addressPoolGap: 20,
    name: 'Main wallet',
    amount: new BigNumber(100100),
    availableAmount: new BigNumber(100100),
    reward: new BigNumber(100),
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 24,
    syncState: walletSyncedStateReady,
    delegatedStakePoolId: STAKE_POOLS[0].id,
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
    availableAmount: new BigNumber(10100.2),
    reward: new BigNumber(50),
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 35,
    syncState: walletSyncedStateReady,
    delegatedStakePoolId: STAKE_POOLS[1].id,
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
    availableAmount: new BigNumber(5001000),
    reward: new BigNumber(30),
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 0,
    syncState: walletSyncedStateRestoring,
    createdAt: new Date(),
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus:
      WalletRecoveryPhraseVerificationStatuses.OK,
    recoveryPhraseVerificationStatusType:
      WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
  }),
];

export const StakingDelegationCenterStory = ({
  locale,
}: {
  locale: string,
}) => (
  <DelegationCenter
    redirectToStakingInfo={redirectToStakingInfo}
    wallets={wallets}
    onDelegate={action('onDelegate')}
    onUndelegate={action('onUndelegate')}
    getStakePoolById={poolId =>
      find(STAKE_POOLS, stakePool => stakePool.id === poolId)
    }
    numberOfStakePools={STAKE_POOLS.length}
    networkTip={networkTip}
    nextEpoch={nextEpoch}
    futureEpoch={futureEpoch}
    currentLocale={locale}
  />
);
