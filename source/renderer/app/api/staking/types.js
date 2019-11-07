// @flow
import BigNumber from 'bignumber.js';

export type StakePool = {
  id: string,
  controlledStake: number,
  description: string,
  slug: string,
  name: string,
  performance: number,
  profitMargin: number,
  ranking: number,
  retiring?: Date,
  created_at: Date,
  isCharity: boolean,
  url: string,
};

export type StakePoolsListType = Array<StakePool>;

export type Reward = {
  date: string,
  wallet: string,
  reward: BigNumber,
  pool: StakePool,
};

export type RewardForIncentivizedTestnet = {
  wallet: string,
  reward: BigNumber,
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
