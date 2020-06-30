// @flow
import BigNumber from 'bignumber.js';
import StakePool from '../../domains/StakePool';

export type DelegationAction =
  | 'changeDelegation'
  | 'removeDelegation'
  | 'delegate';

export type AdaApiStakePool = {
  id: string,
  metrics: {
    non_myopic_member_rewards: {
      quantity: number,
      unit: 'block',
    },
    produced_blocks: {
      quantity: number,
      unit: 'block',
    },
    relative_stake: {
      quantity: number,
      unit: 'percent',
    },
    saturation: number,
  },
  cost: {
    quantity: number,
    unit: 'lovelace',
  },
  margin: {
    quantity: number,
    unit: 'percent',
  },
  metadata: {
    ticker: string, // [3 .. 5] characters
    name: string, // [1 .. 50] characters
    description?: string, // <= 255 characters
    homepage: string,
  },
  pledge: {
    quantity: number,
    unit: 'lovelace',
  },
};
export type AdaApiStakePools = Array<AdaApiStakePool>;

export type Reward = {
  date: string,
  wallet: string,
  reward: BigNumber,
  pool: StakePool,
};

export type RewardForIncentivizedTestnet = {
  date?: string,
  wallet: string,
  reward: BigNumber,
  pool?: StakePool,
};

export type EpochData = {
  pool: StakePool,
  slotsElected: Array<number>,
  performance?: Array<number>,
  sharedRewards?: Array<number>,
};

export type Epoch = {
  id: number,
  name: string,
  progress?: number,
  endsAt?: string,
  data: Array<EpochData>,
};

export type JoinStakePoolRequest = {
  walletId: string,
  stakePoolId: string,
  passphrase: string,
};

export type GetDelegationFeeRequest = {
  walletId: string,
};

export type QuitStakePoolRequest = {
  walletId: string,
  passphrase: string,
};
