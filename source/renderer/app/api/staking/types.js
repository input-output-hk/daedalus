// @flow
import BigNumber from 'bignumber.js';
import { WalletUnits } from '../../domains/Wallet';

export const DelegationActions: {
  CHANGE_DELEGATION: DelegationAction,
  REMOVE_DELEGATION: DelegationAction,
  DELEGATE: DelegationAction,
} = {
  CHANGE_DELEGATION: 'changeDelegation',
  REMOVE_DELEGATION: 'removeDelegation',
  DELEGATE: 'delegate',
};

export type DelegationAction =
  | 'changeDelegation'
  | 'removeDelegation'
  | 'delegate';

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

export type JoinStakePoolRequest = {
  walletId: string,
  stakePoolId: string,
  passphrase: string,
};

export type StakePoolJoinFee = {
  amount: {
    quantity: number,
    unit: WalletUnits.LOVELACE,
  },
};

export type EstimateJoinFeeRequest = {
  walletId: string,
  stakePoolId: string,
};
