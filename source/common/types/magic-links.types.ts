import BigNumber from 'bignumber.js';

export interface CustomProtocolParameters {
  action: 'stake' | 'transaction';
}

export interface TransactionParameters extends CustomProtocolParameters {
  address: string;
  amount: number;
}

export interface StakingParameters extends CustomProtocolParameters {
  address: string;
  amount: BigNumber;
}
