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
  cost: {
    quantity: number,
    unit: 'lovelace',
  },
  margin: {
    quantity: number,
    unit: 'percent',
  },
  metadata: {
    owner: string,
    ticker: string, // [3 .. 5] characters
    name: string, // [1 .. 50] characters
    description?: string, // <= 255 characters
    homepage: string,
    pledge_address: string,
  },
  saturation: number,
  desirability: number,
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

export type DelegationFee = {
  amount: {
    quantity: number,
    unit: WalletUnits.LOVELACE,
  },
};

export type QuitStakePoolRequest = {
  walletId: string,
  passphrase: string,
};
