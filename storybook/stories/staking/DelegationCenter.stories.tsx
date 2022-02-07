import React from 'react';
import { action } from '@storybook/addon-actions';
import { find } from 'lodash';
import BigNumber from 'bignumber.js';
import { number } from '@storybook/addon-knobs';
import DelegationCenter from '../../../source/renderer/app/components/staking/delegation-center/DelegationCenter';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy';
import Wallet, {
  WalletDelegationStatuses,
} from '../../../source/renderer/app/domains/Wallet';
import {
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  RECOVERY_PHRASE_VERIFICATION_TYPES,
} from '../../../source/renderer/app/config/walletRecoveryPhraseVerificationConfig';
import type {
  NextEpoch,
  TipInfo,
} from '../../../source/renderer/app/api/network/types';
import { generateHash, generatePolicyIdHash } from '../_support/utils';

const walletSyncedStateReady = {
  status: 'ready',
};
const walletSyncedStateRestoring = {
  status: 'syncing',
  progress: {
    quantity: 25,
    unit: 'percentage',
  },
};
const networkTip: TipInfo = {
  epoch: 1232,
  slot: 123,
  absoluteSlotNumber: 15000000,
};
const nextEpochDate = new Date();
nextEpochDate.setDate(nextEpochDate.getDate() + 1);
const futureEpochDate = new Date();
futureEpochDate.setDate(futureEpochDate.getDate() + 2);
const nextEpoch: NextEpoch = {
  epochNumber: 1233,
  epochStart: nextEpochDate.toUTCString(),
};
const futureEpoch: NextEpoch = {
  epochNumber: 1234,
  epochStart: futureEpochDate.toUTCString(),
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
    assets: {
      available: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
      total: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
    },
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 24,
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: string; }' is not assignable to ty... Remove this comment to see the full error message
    syncState: walletSyncedStateReady,
    delegatedStakePoolId: null,
    lastDelegationStakePoolId: STAKE_POOLS[250].id,
    pendingDelegations: [
      {
        status: 'delegating',
        target: STAKE_POOLS[250].id,
        changes_at: {
          epoch_number: nextEpoch.epochNumber,
          epoch_start_time: nextEpoch.epochStart,
        },
      },
    ],
    createdAt: new Date(),
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
  }),
  new Wallet({
    id: 'wallet2',
    addressPoolGap: 20,
    name: 'Spending money',
    amount: new BigNumber(100100),
    availableAmount: new BigNumber(100100),
    reward: new BigNumber(100),
    assets: {
      available: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
      total: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
    },
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 24,
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: string; }' is not assignable to ty... Remove this comment to see the full error message
    syncState: walletSyncedStateReady,
    delegatedStakePoolId: STAKE_POOLS[250].id,
    lastDelegationStakePoolId: STAKE_POOLS[0].id,
    pendingDelegations: [
      {
        target: STAKE_POOLS[0].id,
        status: 'delegating',
        changes_at: {
          epoch_number: nextEpoch.epochNumber,
          epoch_start_time: nextEpoch.epochStart,
        },
      },
    ],
    createdAt: new Date(),
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
  }),
  new Wallet({
    id: 'wallet3',
    addressPoolGap: 20,
    name: 'Spending money 2',
    amount: new BigNumber(10100.2),
    availableAmount: new BigNumber(10100.2),
    reward: new BigNumber(50),
    assets: {
      available: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
      total: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
    },
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 35,
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: string; }' is not assignable to ty... Remove this comment to see the full error message
    syncState: walletSyncedStateReady,
    delegatedStakePoolId: STAKE_POOLS[0].id,
    lastDelegationStakePoolId: null,
    pendingDelegations: [
      {
        status: WalletDelegationStatuses.NOT_DELEGATING,
        changes_at: {
          epoch_number: nextEpoch.epochNumber,
          epoch_start_time: nextEpoch.epochStart,
        },
      },
    ],
    createdAt: new Date(),
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
  }),
  new Wallet({
    id: 'wallet4',
    addressPoolGap: 20,
    name: 'Savings',
    amount: new BigNumber(5001000),
    availableAmount: new BigNumber(5001000),
    reward: new BigNumber(30),
    assets: {
      available: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
      total: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
    },
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 0,
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: string; progress: { quantity: numb... Remove this comment to see the full error message
    syncState: walletSyncedStateRestoring,
    delegationStakePoolStatus: WalletDelegationStatuses.NOT_DELEGATING,
    delegatedStakePoolId: null,
    createdAt: new Date(),
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
  }),
  new Wallet({
    id: 'wallet5',
    addressPoolGap: 20,
    name: 'Savings 2',
    amount: new BigNumber(5001000),
    availableAmount: new BigNumber(5001000),
    reward: new BigNumber(30),
    assets: {
      available: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
      total: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
    },
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 0,
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: string; progress: { quantity: numb... Remove this comment to see the full error message
    syncState: walletSyncedStateRestoring,
    delegationStakePoolStatus: WalletDelegationStatuses.DELEGATING,
    delegatedStakePoolId: '90000',
    createdAt: new Date(),
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
  }),
  new Wallet({
    id: 'wallet6',
    addressPoolGap: 20,
    name: 'Spending money 3',
    amount: new BigNumber(10100.2),
    availableAmount: new BigNumber(10100.2),
    reward: new BigNumber(50),
    assets: {
      available: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
      total: [
        {
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ id: string; policyId: string; uniqueId: st... Remove this comment to see the full error message
          id: generateHash(),
          policyId: generatePolicyIdHash(),
          uniqueId: generatePolicyIdHash(),
          assetName: '',
          quantity: new BigNumber(300),
        },
      ],
    },
    hasPassword: true,
    passwordUpdateDate: new Date(),
    isLegacy: false,
    inactiveStakePercentage: 35,
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: string; }' is not assignable to ty... Remove this comment to see the full error message
    syncState: walletSyncedStateReady,
    delegatedStakePoolId: STAKE_POOLS[0].id,
    lastDelegationStakePoolId: '90000',
    pendingDelegations: [
      {
        target: '90000',
        status: WalletDelegationStatuses.DELEGATING,
        changes_at: {
          epoch_number: nextEpoch.epochNumber,
          epoch_start_time: nextEpoch.epochStart,
        },
      },
    ],
    createdAt: new Date(),
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
  }),
];
export const StakingDelegationCenterStory = ({
  locale,
  isLoading,
  isEpochsInfoAvailable,
  currentTheme,
}: {
  locale: string;
  isLoading: boolean;
  isEpochsInfoAvailable: boolean;
  currentTheme: string;
}) => (
  <DelegationCenter
    wallets={wallets}
    onDelegate={action('onDelegate')}
    onUndelegate={action('onUndelegate')}
    getStakePoolById={(poolId) =>
      find(STAKE_POOLS, (stakePool) => stakePool.id === poolId)
    }
    numberOfStakePools={STAKE_POOLS.length}
    networkTip={networkTip}
    nextEpoch={nextEpoch}
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    fetchingStakePoolsFailed={isLoading}
    futureEpoch={futureEpoch}
    currentLocale={locale}
    isLoading={isLoading}
    isEpochsInfoAvailable={isEpochsInfoAvailable}
    slotLength={null}
    epochLength={null}
    containerClassName="StakingWithNavigation_page"
    currentTheme={currentTheme}
    numberOfRankedStakePools={
      STAKE_POOLS.slice(
        0,
        number('Pools', 300, {
          range: true,
          min: 37,
          max: 300,
          step: 1,
        })
      ).length
    }
    onOpenExternalLink={action('onOpenExternalLink')}
  />
);
