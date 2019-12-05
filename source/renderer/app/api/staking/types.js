// @flow
import BigNumber from 'bignumber.js';
import { WalletUnits } from '../../domains/Wallet';
import StakePool from '../../domains/StakePool';

export type DelegationAction =
  | 'changeDelegation'
  | 'removeDelegation'
  | 'delegate';

export type AdaApiStakePool = {
  id: string,
  metrics: {
    controlled_stake: {
      quantity: number,
      unit: 'lovelace',
    },
    produced_blocks: {
      quantity: number,
      unit: 'block',
    },
  },
  apparent_performance: number,
  metadata: {
    ticker: string,
    homepage: string,
    pledge_address: string,
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
